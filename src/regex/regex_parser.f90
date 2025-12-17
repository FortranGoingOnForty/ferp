module regex_parser
  !> Regex parser - converts token stream to AST
  !> Uses recursive descent parsing with proper precedence:
  !>   alternation < concatenation < quantifier < atom
  use regex_types
  implicit none
  private

  public :: parse, ast_pool_t

  !> AST node pool - stores all nodes with indices
  type :: ast_pool_t
    type(ast_node_t), allocatable :: nodes(:)
    integer :: count = 0
    integer :: capacity = 0
  contains
    procedure :: init => pool_init
    procedure :: alloc => pool_alloc
    procedure :: get => pool_get
    procedure :: cleanup => pool_cleanup
  end type ast_pool_t

  ! Module-level parsing state
  type(token_list_t), pointer :: g_tokens => null()
  integer :: g_pos = 0
  integer :: g_group_num = 0
  type(ast_pool_t), pointer :: g_pool => null()

contains

  !---------------------------------------------------------------------------
  ! AST Pool Methods
  !---------------------------------------------------------------------------
  subroutine pool_init(this, initial_capacity)
    class(ast_pool_t), intent(inout) :: this
    integer, intent(in), optional :: initial_capacity
    integer :: cap

    cap = 64
    if (present(initial_capacity)) cap = initial_capacity

    if (allocated(this%nodes)) deallocate(this%nodes)
    allocate(this%nodes(cap))
    this%count = 0
    this%capacity = cap
  end subroutine pool_init

  function pool_alloc(this) result(idx)
    class(ast_pool_t), intent(inout) :: this
    integer :: idx
    type(ast_node_t), allocatable :: temp(:)

    if (.not. allocated(this%nodes)) call this%init()

    if (this%count >= this%capacity) then
      allocate(temp(this%capacity * 2))
      temp(1:this%count) = this%nodes(1:this%count)
      call move_alloc(temp, this%nodes)
      this%capacity = this%capacity * 2
    end if

    this%count = this%count + 1
    idx = this%count
    this%nodes(idx) = ast_node_t()  ! Initialize to default
  end function pool_alloc

  function pool_get(this, idx) result(node)
    class(ast_pool_t), intent(in) :: this
    integer, intent(in) :: idx
    type(ast_node_t) :: node

    if (idx >= 1 .and. idx <= this%count) then
      node = this%nodes(idx)
    else
      node = ast_node_t()
    end if
  end function pool_get

  subroutine pool_cleanup(this)
    class(ast_pool_t), intent(inout) :: this
    if (allocated(this%nodes)) deallocate(this%nodes)
    this%count = 0
    this%capacity = 0
  end subroutine pool_cleanup

  !---------------------------------------------------------------------------
  ! Main Parse Entry Point
  !---------------------------------------------------------------------------
  subroutine parse(tokens, pool, root_idx, num_groups, ierr)
    !> Parse tokens into an AST stored in pool
    type(token_list_t), target, intent(in) :: tokens
    type(ast_pool_t), target, intent(inout) :: pool
    integer, intent(out) :: root_idx
    integer, intent(out) :: num_groups
    integer, intent(out) :: ierr

    ierr = 0
    root_idx = 0

    ! Set up global state
    g_tokens => tokens
    g_pos = 1
    g_group_num = 0
    g_pool => pool

    call pool%init()

    ! Parse alternation (lowest precedence)
    root_idx = parse_alternation(ierr)
    if (ierr /= 0) return

    num_groups = g_group_num

    ! Verify we consumed all tokens
    if (current_type() /= TOK_END) then
      ierr = 1
    end if

  end subroutine parse

  !---------------------------------------------------------------------------
  ! Token Access Helpers
  !---------------------------------------------------------------------------
  function current_token() result(tok)
    type(token_t) :: tok
    tok = g_tokens%get(g_pos)
  end function current_token

  function current_type() result(ttype)
    integer :: ttype
    type(token_t) :: tok
    tok = g_tokens%get(g_pos)
    ttype = tok%ttype
  end function current_type

  subroutine advance()
    g_pos = g_pos + 1
  end subroutine advance

  !---------------------------------------------------------------------------
  ! Recursive Descent Parser
  !---------------------------------------------------------------------------

  recursive function parse_alternation(ierr) result(idx)
    !> Parse: alternation = concat ('|' concat)*
    integer, intent(out) :: ierr
    integer :: idx

    integer :: left, right, alt_idx

    ierr = 0
    idx = 0
    left = parse_concat(ierr)
    if (ierr /= 0) return

    idx = left

    do while (current_type() == TOK_PIPE)
      call advance()  ! consume |

      right = parse_concat(ierr)
      if (ierr /= 0) return

      ! Create alternation node
      alt_idx = g_pool%alloc()
      g_pool%nodes(alt_idx)%ntype = AST_ALTERNATE
      g_pool%nodes(alt_idx)%left = idx
      g_pool%nodes(alt_idx)%right = right

      idx = alt_idx
    end do

  end function parse_alternation

  recursive function parse_concat(ierr) result(idx)
    !> Parse: concat = quantified+
    integer, intent(out) :: ierr
    integer :: idx

    integer :: left, right, concat_idx, ttype

    ierr = 0
    idx = 0

    ! Check if we have anything to parse
    ttype = current_type()
    if (ttype == TOK_END .or. ttype == TOK_PIPE .or. ttype == TOK_RPAREN) then
      ! Empty - create empty literal (matches empty string)
      idx = g_pool%alloc()
      g_pool%nodes(idx)%ntype = AST_LITERAL
      g_pool%nodes(idx)%char_val = char(0)  ! Special empty marker
      return
    end if

    ! Parse first quantified expression
    left = parse_quantified(ierr)
    if (ierr /= 0) return

    idx = left

    ! Parse remaining quantified expressions
    do
      ttype = current_type()
      if (ttype == TOK_END .or. ttype == TOK_PIPE .or. ttype == TOK_RPAREN) exit

      right = parse_quantified(ierr)
      if (ierr /= 0) return

      ! Create concatenation node
      concat_idx = g_pool%alloc()
      g_pool%nodes(concat_idx)%ntype = AST_CONCAT
      g_pool%nodes(concat_idx)%left = idx
      g_pool%nodes(concat_idx)%right = right

      idx = concat_idx
    end do

  end function parse_concat

  recursive function parse_quantified(ierr) result(idx)
    !> Parse: quantified = atom quantifier?
    integer, intent(out) :: ierr
    integer :: idx

    integer :: atom_idx, quant_idx, ttype
    integer :: min_rep, max_rep

    ierr = 0
    idx = 0

    ! Parse the atom
    atom_idx = parse_atom(ierr)
    if (ierr /= 0) return

    idx = atom_idx

    ! Check for quantifier
    ttype = current_type()
    select case (ttype)
      case (TOK_STAR)
        min_rep = 0
        max_rep = -1  ! unlimited
        call advance()

      case (TOK_PLUS)
        min_rep = 1
        max_rep = -1
        call advance()

      case (TOK_QUESTION)
        min_rep = 0
        max_rep = 1
        call advance()

      case (TOK_LBRACE)
        call parse_brace_quantifier(min_rep, max_rep, ierr)
        if (ierr /= 0) return

      case default
        ! No quantifier
        return
    end select

    ! Create quantifier node
    quant_idx = g_pool%alloc()
    g_pool%nodes(quant_idx)%ntype = AST_QUANTIFIER
    g_pool%nodes(quant_idx)%child = atom_idx
    g_pool%nodes(quant_idx)%min_rep = min_rep
    g_pool%nodes(quant_idx)%max_rep = max_rep

    idx = quant_idx

  end function parse_quantified

  subroutine parse_brace_quantifier(min_rep, max_rep, ierr)
    !> Parse {n}, {n,}, or {n,m}
    integer, intent(out) :: min_rep, max_rep, ierr

    type(token_t) :: tok
    integer :: ttype, num

    ierr = 0
    min_rep = 0
    max_rep = 0

    call advance()  ! consume {

    ! Expect a number or }
    ttype = current_type()
    if (ttype == TOK_RBRACE) then
      ! {} - treat as {0,} (match zero or more)
      call advance()
      max_rep = -1
      return
    end if

    if (ttype /= TOK_LITERAL) then
      ierr = 1
      return
    end if

    ! Parse first number
    tok = current_token()
    if (.not. is_digit(tok%char_val)) then
      ierr = 1
      return
    end if

    num = 0
    do while (current_type() == TOK_LITERAL)
      tok = current_token()
      if (.not. is_digit(tok%char_val)) exit
      num = num * 10 + (ichar(tok%char_val) - ichar('0'))
      call advance()
    end do
    min_rep = num

    ttype = current_type()
    if (ttype == TOK_RBRACE) then
      ! {n} - exact count
      call advance()
      max_rep = min_rep
      return
    end if

    ! Expect comma
    tok = current_token()
    if (ttype /= TOK_LITERAL .or. tok%char_val /= ',') then
      ierr = 1
      return
    end if
    call advance()

    ttype = current_type()
    if (ttype == TOK_RBRACE) then
      ! {n,} - at least n
      call advance()
      max_rep = -1
      return
    end if

    ! Parse second number
    if (ttype /= TOK_LITERAL) then
      ierr = 1
      return
    end if

    num = 0
    do while (current_type() == TOK_LITERAL)
      tok = current_token()
      if (.not. is_digit(tok%char_val)) exit
      num = num * 10 + (ichar(tok%char_val) - ichar('0'))
      call advance()
    end do
    max_rep = num

    ! Expect }
    if (current_type() /= TOK_RBRACE) then
      ierr = 1
      return
    end if
    call advance()

  end subroutine parse_brace_quantifier

  recursive function parse_atom(ierr) result(idx)
    !> Parse: atom = literal | '.' | '[' class ']' | '(' regex ')' | anchor | backref
    integer, intent(out) :: ierr
    integer :: idx

    type(token_t) :: tok
    integer :: ttype, group_idx

    ierr = 0
    idx = 0

    tok = current_token()
    ttype = tok%ttype

    select case (ttype)
      case (TOK_LITERAL)
        idx = g_pool%alloc()
        g_pool%nodes(idx)%ntype = AST_LITERAL
        g_pool%nodes(idx)%char_val = tok%char_val
        call advance()

      case (TOK_DOT)
        idx = g_pool%alloc()
        g_pool%nodes(idx)%ntype = AST_DOT
        call advance()

      case (TOK_LBRACKET)
        ! Character class (already parsed by lexer)
        idx = g_pool%alloc()
        g_pool%nodes(idx)%ntype = AST_CHAR_CLASS
        g_pool%nodes(idx)%char_class = tok%char_class
        g_pool%nodes(idx)%negated = tok%negated
        call advance()

      case (TOK_LPAREN)
        call advance()  ! consume (

        g_group_num = g_group_num + 1
        group_idx = g_group_num

        ! Parse inner expression
        idx = parse_alternation(ierr)
        if (ierr /= 0) return

        ! Expect )
        if (current_type() /= TOK_RPAREN) then
          ierr = 1
          return
        end if
        call advance()

        ! Wrap in group node
        group_idx = g_pool%alloc()
        g_pool%nodes(group_idx)%ntype = AST_GROUP
        g_pool%nodes(group_idx)%child = idx
        g_pool%nodes(group_idx)%group_num = g_group_num

        idx = group_idx

      case (TOK_CARET)
        idx = g_pool%alloc()
        g_pool%nodes(idx)%ntype = AST_ANCHOR
        g_pool%nodes(idx)%anchor_type = 1  ! start
        call advance()

      case (TOK_DOLLAR)
        idx = g_pool%alloc()
        g_pool%nodes(idx)%ntype = AST_ANCHOR
        g_pool%nodes(idx)%anchor_type = 2  ! end
        call advance()

      case (TOK_WORD_BOUNDARY)
        idx = g_pool%alloc()
        g_pool%nodes(idx)%ntype = AST_ANCHOR
        g_pool%nodes(idx)%anchor_type = tok%int_val + 2  ! 3=word_start, 4=word_end, 5=boundary
        call advance()

      case (TOK_BACKREF)
        idx = g_pool%alloc()
        g_pool%nodes(idx)%ntype = AST_BACKREF
        g_pool%nodes(idx)%group_num = tok%int_val
        call advance()

      case default
        ! Unexpected token - error
        ierr = 1
    end select

  end function parse_atom

  !---------------------------------------------------------------------------
  ! Utility Functions
  !---------------------------------------------------------------------------
  pure function is_digit(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    res = (c >= '0' .and. c <= '9')
  end function is_digit

end module regex_parser
