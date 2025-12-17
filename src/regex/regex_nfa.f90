module regex_nfa
  !> Thompson NFA construction from AST
  !> Implements the classic Thompson construction algorithm
  use regex_types
  use regex_parser, only: ast_pool_t
  implicit none
  private

  public :: build_nfa

  ! Fragment type for Thompson construction
  type :: fragment_t
    integer :: start_state = 0   ! Start state index
    integer :: accept_state = 0  ! Accept state index
  end type fragment_t

contains

  subroutine build_nfa(pool, root_idx, nfa, ierr)
    !> Build NFA from AST
    type(ast_pool_t), intent(in) :: pool
    integer, intent(in) :: root_idx
    type(nfa_t), intent(out) :: nfa
    integer, intent(out) :: ierr

    type(fragment_t) :: frag

    ierr = 0
    call nfa%init()

    if (root_idx == 0) then
      ! Empty pattern - create NFA that matches empty string
      nfa%start_state = nfa%add_state()
      nfa%accept_state = nfa%add_state()
      nfa%states(nfa%accept_state)%is_accept = .true.
      call add_epsilon(nfa, nfa%start_state, nfa%accept_state)
      return
    end if

    ! Build NFA from AST
    frag = build_fragment(pool, root_idx, nfa, ierr)
    if (ierr /= 0) return

    nfa%start_state = frag%start_state
    nfa%accept_state = frag%accept_state
    nfa%states(frag%accept_state)%is_accept = .true.

  end subroutine build_nfa

  recursive function build_fragment(pool, node_idx, nfa, ierr) result(frag)
    !> Build NFA fragment for an AST node
    type(ast_pool_t), intent(in) :: pool
    integer, intent(in) :: node_idx
    type(nfa_t), intent(inout) :: nfa
    integer, intent(out) :: ierr
    type(fragment_t) :: frag

    type(ast_node_t) :: node
    type(fragment_t) :: left_frag, right_frag, child_frag
    type(nfa_transition_t) :: trans
    integer :: s1, s2

    ierr = 0
    frag%start_state = 0
    frag%accept_state = 0

    if (node_idx == 0 .or. node_idx > pool%count) then
      ierr = 1
      return
    end if

    node = pool%nodes(node_idx)

    select case (node%ntype)

      case (AST_LITERAL)
        ! Literal: (s1) --c--> (s2)
        s1 = nfa%add_state()
        s2 = nfa%add_state()

        if (node%char_val == char(0)) then
          ! Empty literal - epsilon transition
          call add_epsilon(nfa, s1, s2)
        else
          trans%trans_type = TRANS_CHAR
          trans%match_char = node%char_val
          trans%target = s2
          call nfa%states(s1)%add_trans(trans)
        end if

        frag%start_state = s1
        frag%accept_state = s2

      case (AST_DOT)
        ! Dot: (s1) --any--> (s2)
        s1 = nfa%add_state()
        s2 = nfa%add_state()

        trans%trans_type = TRANS_ANY
        trans%target = s2
        call nfa%states(s1)%add_trans(trans)

        frag%start_state = s1
        frag%accept_state = s2

      case (AST_CHAR_CLASS)
        ! Character class: (s1) --class--> (s2)
        s1 = nfa%add_state()
        s2 = nfa%add_state()

        trans%trans_type = TRANS_CLASS
        trans%char_class = node%char_class
        trans%negated = node%negated
        trans%target = s2
        call nfa%states(s1)%add_trans(trans)

        frag%start_state = s1
        frag%accept_state = s2

      case (AST_ANCHOR)
        ! Anchor: (s1) --anchor--> (s2) (zero-width)
        s1 = nfa%add_state()
        s2 = nfa%add_state()

        trans%trans_type = TRANS_ANCHOR
        trans%anchor_type = node%anchor_type
        trans%target = s2
        call nfa%states(s1)%add_trans(trans)

        frag%start_state = s1
        frag%accept_state = s2

      case (AST_CONCAT)
        ! Concatenation: connect left accept to right start
        left_frag = build_fragment(pool, node%left, nfa, ierr)
        if (ierr /= 0) return
        right_frag = build_fragment(pool, node%right, nfa, ierr)
        if (ierr /= 0) return

        ! Connect left accept to right start with epsilon
        call add_epsilon(nfa, left_frag%accept_state, right_frag%start_state)

        frag%start_state = left_frag%start_state
        frag%accept_state = right_frag%accept_state

      case (AST_ALTERNATE)
        ! Alternation: new start with epsilon to both, both accept to new accept
        !        e-->(left.s)-->(left.a)--e
        !       /                          \
        ! (s1)-                              ->(s2)
        !       \                          /
        !        e-->(right.s)-->(right.a)--e

        left_frag = build_fragment(pool, node%left, nfa, ierr)
        if (ierr /= 0) return
        right_frag = build_fragment(pool, node%right, nfa, ierr)
        if (ierr /= 0) return

        s1 = nfa%add_state()
        s2 = nfa%add_state()

        call add_epsilon(nfa, s1, left_frag%start_state)
        call add_epsilon(nfa, s1, right_frag%start_state)
        call add_epsilon(nfa, left_frag%accept_state, s2)
        call add_epsilon(nfa, right_frag%accept_state, s2)

        frag%start_state = s1
        frag%accept_state = s2

      case (AST_QUANTIFIER)
        child_frag = build_fragment(pool, node%child, nfa, ierr)
        if (ierr /= 0) return

        if (node%min_rep == 0 .and. node%max_rep == -1) then
          ! Star (*): zero or more
          !      e-------->
          !     /          \
          ! (s1)--e-->(c.s)-->(c.a)-->(s2)
          !               \     /
          !                <-e-

          s1 = nfa%add_state()
          s2 = nfa%add_state()

          call add_epsilon(nfa, s1, child_frag%start_state)
          call add_epsilon(nfa, s1, s2)
          call add_epsilon(nfa, child_frag%accept_state, s2)
          call add_epsilon(nfa, child_frag%accept_state, child_frag%start_state)

          frag%start_state = s1
          frag%accept_state = s2

        else if (node%min_rep == 1 .and. node%max_rep == -1) then
          ! Plus (+): one or more
          ! (c.s)-->(c.a)-->(s2)
          !      \     /
          !       <-e-

          s2 = nfa%add_state()

          call add_epsilon(nfa, child_frag%accept_state, s2)
          call add_epsilon(nfa, child_frag%accept_state, child_frag%start_state)

          frag%start_state = child_frag%start_state
          frag%accept_state = s2

        else if (node%min_rep == 0 .and. node%max_rep == 1) then
          ! Question (?): zero or one
          !      e-------->
          !     /          \
          ! (s1)--e-->(c.s)-->(c.a)-->(s2)

          s1 = nfa%add_state()
          s2 = nfa%add_state()

          call add_epsilon(nfa, s1, child_frag%start_state)
          call add_epsilon(nfa, s1, s2)
          call add_epsilon(nfa, child_frag%accept_state, s2)

          frag%start_state = s1
          frag%accept_state = s2

        else
          ! Bounded quantifier {n,m}
          ! For simplicity, unroll: min copies required, then (max-min) optional
          call build_bounded_quantifier(pool, node, child_frag, nfa, frag, ierr)
          if (ierr /= 0) return
        end if

      case (AST_GROUP)
        ! Group: just build the child, mark states for group capture
        child_frag = build_fragment(pool, node%child, nfa, ierr)
        if (ierr /= 0) return

        ! Mark group boundaries
        nfa%states(child_frag%start_state)%group_start = node%group_num
        nfa%states(child_frag%accept_state)%group_end = node%group_num
        nfa%num_groups = max(nfa%num_groups, node%group_num)

        frag = child_frag

      case (AST_BACKREF)
        ! Backreference - needs special handling in engine
        ! For now, create placeholder states
        s1 = nfa%add_state()
        s2 = nfa%add_state()

        ! Store backref info in transition
        trans%trans_type = TRANS_EPSILON  ! Will be handled specially
        trans%anchor_type = -node%group_num  ! Negative = backref
        trans%target = s2
        call nfa%states(s1)%add_trans(trans)

        frag%start_state = s1
        frag%accept_state = s2

      case default
        ierr = 1

    end select

  end function build_fragment

  subroutine build_bounded_quantifier(pool, node, child_frag, nfa, frag, ierr)
    !> Build NFA for {n,m} quantifier
    type(ast_pool_t), intent(in) :: pool
    type(ast_node_t), intent(in) :: node
    type(fragment_t), intent(in) :: child_frag
    type(nfa_t), intent(inout) :: nfa
    type(fragment_t), intent(out) :: frag
    integer, intent(out) :: ierr

    integer :: i, min_rep, max_rep
    type(fragment_t) :: copy_frag, prev_frag
    integer :: s1, s2

    ierr = 0
    min_rep = node%min_rep
    max_rep = node%max_rep

    if (min_rep == 0 .and. max_rep == 0) then
      ! {0} or {} - match empty
      s1 = nfa%add_state()
      s2 = nfa%add_state()
      call add_epsilon(nfa, s1, s2)
      frag%start_state = s1
      frag%accept_state = s2
      return
    end if

    ! Start with first copy (or empty if min=0)
    if (min_rep == 0) then
      s1 = nfa%add_state()
      frag%start_state = s1
      frag%accept_state = s1
    else
      ! First required copy
      frag = child_frag

      ! Additional required copies
      do i = 2, min_rep
        copy_frag = build_fragment(pool, node%child, nfa, ierr)
        if (ierr /= 0) return
        call add_epsilon(nfa, frag%accept_state, copy_frag%start_state)
        frag%accept_state = copy_frag%accept_state
      end do
    end if

    ! Optional copies
    if (max_rep == -1) then
      ! {n,} - unlimited
      copy_frag = build_fragment(pool, node%child, nfa, ierr)
      if (ierr /= 0) return

      s2 = nfa%add_state()
      call add_epsilon(nfa, frag%accept_state, copy_frag%start_state)
      call add_epsilon(nfa, frag%accept_state, s2)
      call add_epsilon(nfa, copy_frag%accept_state, s2)
      call add_epsilon(nfa, copy_frag%accept_state, copy_frag%start_state)

      frag%accept_state = s2

    else if (max_rep > min_rep) then
      ! {n,m} - limited optional copies
      prev_frag = frag
      s2 = nfa%add_state()

      do i = min_rep + 1, max_rep
        copy_frag = build_fragment(pool, node%child, nfa, ierr)
        if (ierr /= 0) return

        call add_epsilon(nfa, prev_frag%accept_state, copy_frag%start_state)
        call add_epsilon(nfa, prev_frag%accept_state, s2)

        prev_frag = copy_frag
      end do

      call add_epsilon(nfa, prev_frag%accept_state, s2)
      frag%accept_state = s2
    end if

  end subroutine build_bounded_quantifier

  subroutine add_epsilon(nfa, from_state, to_state)
    !> Add epsilon transition from one state to another
    type(nfa_t), intent(inout) :: nfa
    integer, intent(in) :: from_state, to_state

    type(nfa_transition_t) :: trans

    trans%trans_type = TRANS_EPSILON
    trans%target = to_state
    call nfa%states(from_state)%add_trans(trans)

  end subroutine add_epsilon

end module regex_nfa
