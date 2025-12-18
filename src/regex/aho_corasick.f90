module aho_corasick
  !> Aho-Corasick automaton for multi-pattern string matching
  !> Matches all patterns in a single pass O(n + m + z)
  !> where n=text length, m=total pattern length, z=matches
  implicit none
  private

  public :: ac_automaton_t, ac_match_t
  public :: ac_build, ac_search, ac_search_any, ac_free

  integer, parameter :: MAX_CHILDREN = 256  ! ASCII character set
  integer, parameter :: MAX_PATTERNS = 1000
  integer, parameter :: MAX_PATTERN_LEN = 4096

  type :: ac_node_t
    !> Trie node with failure links
    integer :: children(0:255) = 0     ! Child node indices (0 = no child)
    integer :: failure = 0              ! Failure link (fall back on mismatch)
    integer :: output_pattern = 0       ! Pattern index that ends here (0 = none)
    integer :: output_link = 0          ! Link to next output state
    integer :: depth = 0                ! Depth in trie (= prefix length)
  end type ac_node_t

  type :: ac_automaton_t
    !> Aho-Corasick automaton
    type(ac_node_t), allocatable :: nodes(:)
    integer :: num_nodes = 0
    integer :: capacity = 0
    integer :: num_patterns = 0
    integer, allocatable :: pattern_lengths(:)
    logical :: compiled = .false.
    logical :: ignore_case = .false.
  end type ac_automaton_t

  type :: ac_match_t
    !> Match result
    logical :: matched = .false.
    integer :: pattern_idx = 0          ! Which pattern matched (1-based)
    integer :: start_pos = 0            ! Start position in text (1-based)
    integer :: end_pos = 0              ! End position in text (1-based)
  end type ac_match_t

contains

  subroutine ac_build(ac, patterns, num_patterns, ignore_case, ierr)
    !> Build Aho-Corasick automaton from patterns
    type(ac_automaton_t), intent(out) :: ac
    character(len=*), intent(in) :: patterns(:)
    integer, intent(in) :: num_patterns
    logical, intent(in) :: ignore_case
    integer, intent(out) :: ierr

    integer :: i, j, c, state, next_state, child
    integer, allocatable :: queue(:)
    integer :: q_head, q_tail
    integer :: fail_state
    character(len=1) :: ch

    ierr = 0

    ! Allocate BFS queue
    allocate(queue(MAX_PATTERNS * MAX_PATTERN_LEN))
    ac%ignore_case = ignore_case
    ac%num_patterns = num_patterns

    ! Allocate pattern lengths
    allocate(ac%pattern_lengths(num_patterns))
    do i = 1, num_patterns
      ac%pattern_lengths(i) = len_trim(patterns(i))
    end do

    ! Initial capacity - estimate based on total pattern length
    ac%capacity = 1
    do i = 1, num_patterns
      ac%capacity = ac%capacity + len_trim(patterns(i))
    end do
    ac%capacity = max(ac%capacity, 256)
    allocate(ac%nodes(ac%capacity))

    ! Initialize root node (index 1)
    ac%num_nodes = 1
    ac%nodes(1)%depth = 0

    ! Phase 1: Build trie from patterns
    do i = 1, num_patterns
      state = 1  ! Start at root
      do j = 1, len_trim(patterns(i))
        ch = patterns(i)(j:j)
        if (ignore_case) then
          c = to_lower_code(ichar(ch))
        else
          c = ichar(ch)
        end if

        child = ac%nodes(state)%children(c)
        if (child == 0) then
          ! Create new node
          ac%num_nodes = ac%num_nodes + 1
          if (ac%num_nodes > ac%capacity) then
            call grow_nodes(ac)
          end if
          ac%nodes(state)%children(c) = ac%num_nodes
          ac%nodes(ac%num_nodes)%depth = ac%nodes(state)%depth + 1
          child = ac%num_nodes
        end if
        state = child
      end do
      ! Mark this state as accepting for pattern i
      ac%nodes(state)%output_pattern = i
    end do

    ! Phase 2: Compute failure links using BFS
    q_head = 1
    q_tail = 0

    ! Initialize: depth-1 nodes fail to root
    do c = 0, 255
      child = ac%nodes(1)%children(c)
      if (child /= 0) then
        ac%nodes(child)%failure = 1  ! Fail to root
        q_tail = q_tail + 1
        queue(q_tail) = child
      end if
    end do

    ! BFS to compute failure links for deeper nodes
    do while (q_head <= q_tail)
      state = queue(q_head)
      q_head = q_head + 1

      do c = 0, 255
        child = ac%nodes(state)%children(c)
        if (child /= 0) then
          ! Add to queue
          q_tail = q_tail + 1
          queue(q_tail) = child

          ! Compute failure link: follow parent's failure until we find
          ! a state with a transition on c, or reach root
          fail_state = ac%nodes(state)%failure
          do while (fail_state > 1)
            if (ac%nodes(fail_state)%children(c) /= 0) exit
            fail_state = ac%nodes(fail_state)%failure
          end do

          if (fail_state <= 1) then
            ! At or beyond root
            if (ac%nodes(1)%children(c) /= 0 .and. ac%nodes(1)%children(c) /= child) then
              ac%nodes(child)%failure = ac%nodes(1)%children(c)
            else
              ac%nodes(child)%failure = 1  ! Fail to root
            end if
          else if (ac%nodes(fail_state)%children(c) == child) then
            ! Would create self-loop, fail to root
            ac%nodes(child)%failure = 1
          else
            ac%nodes(child)%failure = ac%nodes(fail_state)%children(c)
          end if

          ! Compute output link: chain of accepting states via failure links
          if (ac%nodes(ac%nodes(child)%failure)%output_pattern /= 0) then
            ac%nodes(child)%output_link = ac%nodes(child)%failure
          else
            ac%nodes(child)%output_link = ac%nodes(ac%nodes(child)%failure)%output_link
          end if
        end if
      end do
    end do

    ac%compiled = .true.

    deallocate(queue)

  contains

    function to_lower_code(c) result(lc)
      integer, intent(in) :: c
      integer :: lc
      if (c >= ichar('A') .and. c <= ichar('Z')) then
        lc = c + 32
      else
        lc = c
      end if
    end function to_lower_code

  end subroutine ac_build

  subroutine grow_nodes(ac)
    !> Double the node capacity
    type(ac_automaton_t), intent(inout) :: ac
    type(ac_node_t), allocatable :: temp(:)
    integer :: new_cap

    new_cap = ac%capacity * 2
    allocate(temp(new_cap))
    temp(1:ac%num_nodes) = ac%nodes(1:ac%num_nodes)
    call move_alloc(temp, ac%nodes)
    ac%capacity = new_cap
  end subroutine grow_nodes

  function ac_search_any(ac, text) result(found)
    !> Search for any pattern match (fast path for existence check)
    type(ac_automaton_t), intent(in) :: ac
    character(len=*), intent(in) :: text
    logical :: found

    integer :: i, c, state, next_state, text_len

    found = .false.
    if (.not. ac%compiled) return

    text_len = len(text)
    state = 1  ! Start at root

    do i = 1, text_len
      if (ac%ignore_case) then
        c = to_lower_code(ichar(text(i:i)))
      else
        c = ichar(text(i:i))
      end if

      ! Follow failure links until we find a transition or reach root
      do while (state /= 1 .and. ac%nodes(state)%children(c) == 0)
        state = ac%nodes(state)%failure
      end do

      next_state = ac%nodes(state)%children(c)
      if (next_state /= 0) then
        state = next_state
      else
        state = 1  ! Stay at root if no transition
      end if

      ! Check for match at current state or via output links
      if (ac%nodes(state)%output_pattern /= 0) then
        found = .true.
        return
      end if
      if (ac%nodes(state)%output_link /= 0) then
        found = .true.
        return
      end if
    end do

  contains

    function to_lower_code(c) result(lc)
      integer, intent(in) :: c
      integer :: lc
      if (c >= ichar('A') .and. c <= ichar('Z')) then
        lc = c + 32
      else
        lc = c
      end if
    end function to_lower_code

  end function ac_search_any

  function ac_search(ac, text) result(match)
    !> Search for first pattern match with position info
    type(ac_automaton_t), intent(in) :: ac
    character(len=*), intent(in) :: text
    type(ac_match_t) :: match

    integer :: i, c, state, next_state, text_len, pat_idx, out_state

    match%matched = .false.
    if (.not. ac%compiled) return

    text_len = len(text)
    state = 1  ! Start at root

    do i = 1, text_len
      if (ac%ignore_case) then
        c = to_lower_code(ichar(text(i:i)))
      else
        c = ichar(text(i:i))
      end if

      ! Follow failure links until we find a transition or reach root
      do while (state /= 1 .and. ac%nodes(state)%children(c) == 0)
        state = ac%nodes(state)%failure
      end do

      next_state = ac%nodes(state)%children(c)
      if (next_state /= 0) then
        state = next_state
      else
        state = 1
      end if

      ! Check for match at current state
      pat_idx = ac%nodes(state)%output_pattern
      if (pat_idx /= 0) then
        match%matched = .true.
        match%pattern_idx = pat_idx
        match%end_pos = i
        match%start_pos = i - ac%pattern_lengths(pat_idx) + 1
        return
      end if

      ! Check output links for overlapping patterns
      out_state = ac%nodes(state)%output_link
      if (out_state /= 0) then
        pat_idx = ac%nodes(out_state)%output_pattern
        if (pat_idx /= 0) then
          match%matched = .true.
          match%pattern_idx = pat_idx
          match%end_pos = i
          match%start_pos = i - ac%pattern_lengths(pat_idx) + 1
          return
        end if
      end if
    end do

  contains

    function to_lower_code(c) result(lc)
      integer, intent(in) :: c
      integer :: lc
      if (c >= ichar('A') .and. c <= ichar('Z')) then
        lc = c + 32
      else
        lc = c
      end if
    end function to_lower_code

  end function ac_search

  subroutine ac_free(ac)
    !> Free automaton resources
    type(ac_automaton_t), intent(inout) :: ac

    if (allocated(ac%nodes)) deallocate(ac%nodes)
    if (allocated(ac%pattern_lengths)) deallocate(ac%pattern_lengths)
    ac%num_nodes = 0
    ac%capacity = 0
    ac%num_patterns = 0
    ac%compiled = .false.
  end subroutine ac_free

end module aho_corasick
