module regex_types
  !> Core data types for the FERP regex engine
  !> Defines tokens, AST nodes, and NFA structures
  use regex_charclass
  implicit none
  private

  ! Token types - public
  public :: TOK_LITERAL, TOK_DOT, TOK_STAR, TOK_PLUS, TOK_QUESTION
  public :: TOK_CARET, TOK_DOLLAR, TOK_PIPE
  public :: TOK_LPAREN, TOK_RPAREN, TOK_LBRACE, TOK_RBRACE
  public :: TOK_LBRACKET, TOK_RBRACKET
  public :: TOK_BACKREF, TOK_WORD_BOUNDARY
  public :: TOK_END

  ! AST node types - public
  public :: AST_LITERAL, AST_DOT, AST_CHAR_CLASS, AST_ANCHOR
  public :: AST_CONCAT, AST_ALTERNATE, AST_QUANTIFIER, AST_GROUP
  public :: AST_BACKREF

  ! NFA transition types - public
  public :: TRANS_EPSILON, TRANS_CHAR, TRANS_CLASS, TRANS_ANY, TRANS_ANCHOR

  ! Data types - public
  public :: token_t, token_list_t
  public :: ast_node_t
  public :: nfa_transition_t, nfa_state_t, nfa_t
  public :: match_result_t

  !---------------------------------------------------------------------------
  ! Token Type Constants
  !---------------------------------------------------------------------------
  integer, parameter :: TOK_LITERAL      = 1   ! Literal character
  integer, parameter :: TOK_DOT          = 2   ! . (any char except newline)
  integer, parameter :: TOK_STAR         = 3   ! * (zero or more)
  integer, parameter :: TOK_PLUS         = 4   ! + (one or more)
  integer, parameter :: TOK_QUESTION     = 5   ! ? (zero or one)
  integer, parameter :: TOK_CARET        = 6   ! ^ (start anchor)
  integer, parameter :: TOK_DOLLAR       = 7   ! $ (end anchor)
  integer, parameter :: TOK_LPAREN       = 8   ! ( or \(
  integer, parameter :: TOK_RPAREN       = 9   ! ) or \)
  integer, parameter :: TOK_LBRACE       = 10  ! { or \{
  integer, parameter :: TOK_RBRACE       = 11  ! } or \}
  integer, parameter :: TOK_PIPE         = 12  ! | (alternation)
  integer, parameter :: TOK_LBRACKET     = 13  ! [ (char class start)
  integer, parameter :: TOK_RBRACKET     = 14  ! ] (char class end)
  integer, parameter :: TOK_BACKREF      = 15  ! \1-\9
  integer, parameter :: TOK_WORD_BOUNDARY = 16 ! \b, \<, \>
  integer, parameter :: TOK_END          = 17  ! End of pattern

  !---------------------------------------------------------------------------
  ! AST Node Type Constants
  !---------------------------------------------------------------------------
  integer, parameter :: AST_LITERAL     = 1   ! Single character
  integer, parameter :: AST_DOT         = 2   ! Any character
  integer, parameter :: AST_CHAR_CLASS  = 3   ! Character class [...]
  integer, parameter :: AST_ANCHOR      = 4   ! ^ or $ or \b
  integer, parameter :: AST_CONCAT      = 5   ! Concatenation (implicit)
  integer, parameter :: AST_ALTERNATE   = 6   ! Alternation |
  integer, parameter :: AST_QUANTIFIER  = 7   ! *, +, ?, {n,m}
  integer, parameter :: AST_GROUP       = 8   ! Capturing group ()
  integer, parameter :: AST_BACKREF     = 9   ! Backreference \1-\9

  !---------------------------------------------------------------------------
  ! NFA Transition Type Constants
  !---------------------------------------------------------------------------
  integer, parameter :: TRANS_EPSILON = 0    ! Epsilon (empty) transition
  integer, parameter :: TRANS_CHAR    = 1    ! Single character match
  integer, parameter :: TRANS_CLASS   = 2    ! Character class match
  integer, parameter :: TRANS_ANY     = 3    ! Any character (dot)
  integer, parameter :: TRANS_ANCHOR  = 4    ! Anchor (zero-width)

  !---------------------------------------------------------------------------
  ! Token Type
  !---------------------------------------------------------------------------
  type :: token_t
    integer :: ttype = 0                    ! Token type (TOK_*)
    character(len=1) :: char_val = ' '      ! Character value for literals
    integer :: int_val = 0                  ! Integer value (backref num, etc.)
    integer :: pos = 0                      ! Position in source pattern
    logical :: char_class(0:255) = .false.  ! For character classes
    logical :: negated = .false.            ! For negated char classes [^...]
  end type token_t

  !---------------------------------------------------------------------------
  ! Token List Type
  !---------------------------------------------------------------------------
  type :: token_list_t
    type(token_t), allocatable :: tokens(:)
    integer :: count = 0
    integer :: capacity = 0
  contains
    procedure :: init => token_list_init
    procedure :: append => token_list_append
    procedure :: get => token_list_get
    procedure :: reset => token_list_reset
  end type token_list_t

  !---------------------------------------------------------------------------
  ! AST Node Type
  !---------------------------------------------------------------------------
  type :: ast_node_t
    integer :: ntype = 0                    ! Node type (AST_*)
    character(len=1) :: char_val = ' '      ! For literals

    ! For character classes
    logical :: char_class(0:255) = .false.  ! Which chars are in class
    logical :: negated = .false.            ! [^...] negation

    ! For quantifiers
    integer :: min_rep = 0                  ! Minimum repetitions
    integer :: max_rep = 0                  ! Maximum (-1 = unlimited)
    logical :: greedy = .true.              ! Greedy matching (always true for POSIX)

    ! For anchors
    integer :: anchor_type = 0              ! 1=start, 2=end, 3=word_start, 4=word_end, 5=word_boundary

    ! For groups and backrefs
    integer :: group_num = 0                ! Capturing group number

    ! Tree structure - using indices instead of pointers for simplicity
    integer :: left = 0                     ! Left child index
    integer :: right = 0                    ! Right child index
    integer :: child = 0                    ! Single child (for unary ops)
  end type ast_node_t

  !---------------------------------------------------------------------------
  ! NFA Transition Type
  !---------------------------------------------------------------------------
  type :: nfa_transition_t
    integer :: trans_type = TRANS_EPSILON   ! Transition type
    character(len=1) :: match_char = ' '    ! For TRANS_CHAR
    logical :: char_class(0:255) = .false.  ! For TRANS_CLASS (legacy)
    type(char_class_bits_t) :: char_bits    ! Bitwise char class (fast)
    logical :: negated = .false.            ! For negated classes
    integer :: target = 0                   ! Target state index

    ! For anchors
    integer :: anchor_type = 0              ! Same as AST anchor_type
  end type nfa_transition_t

  !---------------------------------------------------------------------------
  ! NFA State Type
  !---------------------------------------------------------------------------
  type :: nfa_state_t
    integer :: id = 0                       ! State ID
    logical :: is_accept = .false.          ! Is this an accepting state?
    type(nfa_transition_t), allocatable :: trans(:)  ! Outgoing transitions
    integer :: num_trans = 0                ! Number of transitions

    ! For capturing groups
    integer :: group_start = 0              ! Group number this state starts (0=none)
    integer :: group_end = 0                ! Group number this state ends (0=none)
  contains
    procedure :: add_trans => state_add_transition
  end type nfa_state_t

  !---------------------------------------------------------------------------
  ! NFA Type (the complete automaton)
  !---------------------------------------------------------------------------
  type :: nfa_t
    type(nfa_state_t), allocatable :: states(:)
    integer :: num_states = 0
    integer :: start_state = 0              ! Index of start state
    integer :: accept_state = 0             ! Index of accept state (for simple NFAs)
    integer :: num_groups = 0               ! Number of capturing groups
  contains
    procedure :: add_state => nfa_add_state
    procedure :: init => nfa_init
    procedure :: cleanup => nfa_cleanup
  end type nfa_t

  !---------------------------------------------------------------------------
  ! Match Result Type
  !---------------------------------------------------------------------------
  type :: match_result_t
    logical :: matched = .false.            ! Did the pattern match?
    integer :: match_start = 0              ! Start position of match (1-based)
    integer :: match_end = 0                ! End position of match (1-based)
    integer :: group_starts(9) = 0          ! Start positions of groups 1-9
    integer :: group_ends(9) = 0            ! End positions of groups 1-9
  end type match_result_t

contains

  !---------------------------------------------------------------------------
  ! Token List Methods
  !---------------------------------------------------------------------------
  subroutine token_list_init(this, initial_capacity)
    class(token_list_t), intent(inout) :: this
    integer, intent(in), optional :: initial_capacity
    integer :: cap

    cap = 32
    if (present(initial_capacity)) cap = initial_capacity

    if (allocated(this%tokens)) deallocate(this%tokens)
    allocate(this%tokens(cap))
    this%count = 0
    this%capacity = cap
  end subroutine token_list_init

  subroutine token_list_append(this, tok)
    class(token_list_t), intent(inout) :: this
    type(token_t), intent(in) :: tok
    type(token_t), allocatable :: temp(:)

    ! Initialize if needed
    if (.not. allocated(this%tokens)) call this%init()

    ! Grow if needed
    if (this%count >= this%capacity) then
      allocate(temp(this%capacity * 2))
      temp(1:this%count) = this%tokens(1:this%count)
      call move_alloc(temp, this%tokens)
      this%capacity = this%capacity * 2
    end if

    this%count = this%count + 1
    this%tokens(this%count) = tok
  end subroutine token_list_append

  function token_list_get(this, idx) result(tok)
    class(token_list_t), intent(in) :: this
    integer, intent(in) :: idx
    type(token_t) :: tok

    if (idx >= 1 .and. idx <= this%count) then
      tok = this%tokens(idx)
    else
      tok%ttype = TOK_END
    end if
  end function token_list_get

  subroutine token_list_reset(this)
    class(token_list_t), intent(inout) :: this
    this%count = 0
  end subroutine token_list_reset

  !---------------------------------------------------------------------------
  ! NFA State Methods
  !---------------------------------------------------------------------------
  subroutine state_add_transition(this, trans)
    class(nfa_state_t), intent(inout) :: this
    type(nfa_transition_t), intent(in) :: trans
    type(nfa_transition_t), allocatable :: temp(:)
    integer :: n

    if (.not. allocated(this%trans)) then
      allocate(this%trans(4))
      this%num_trans = 0
    end if

    n = this%num_trans
    if (n >= size(this%trans)) then
      allocate(temp(size(this%trans) * 2))
      temp(1:n) = this%trans(1:n)
      call move_alloc(temp, this%trans)
    end if

    this%num_trans = n + 1
    this%trans(this%num_trans) = trans
  end subroutine state_add_transition

  !---------------------------------------------------------------------------
  ! NFA Methods
  !---------------------------------------------------------------------------
  subroutine nfa_init(this, initial_capacity)
    class(nfa_t), intent(inout) :: this
    integer, intent(in), optional :: initial_capacity
    integer :: cap

    cap = 64
    if (present(initial_capacity)) cap = initial_capacity

    if (allocated(this%states)) deallocate(this%states)
    allocate(this%states(cap))
    this%num_states = 0
    this%start_state = 0
    this%accept_state = 0
    this%num_groups = 0
  end subroutine nfa_init

  function nfa_add_state(this) result(idx)
    class(nfa_t), intent(inout) :: this
    integer :: idx
    type(nfa_state_t), allocatable :: temp(:)
    integer :: n

    if (.not. allocated(this%states)) call this%init()

    n = this%num_states
    if (n >= size(this%states)) then
      allocate(temp(size(this%states) * 2))
      temp(1:n) = this%states(1:n)
      call move_alloc(temp, this%states)
    end if

    this%num_states = n + 1
    idx = this%num_states
    this%states(idx)%id = idx
  end function nfa_add_state

  subroutine nfa_cleanup(this)
    class(nfa_t), intent(inout) :: this
    if (allocated(this%states)) deallocate(this%states)
    this%num_states = 0
  end subroutine nfa_cleanup

end module regex_types
