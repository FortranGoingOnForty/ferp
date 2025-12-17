module regex_engine
  !> NFA simulation engine using Thompson's algorithm
  !> Tracks set of current states, processes input character by character
  use regex_types
  implicit none
  private

  public :: nfa_match, nfa_search

  integer, parameter :: MAX_STATES = 1024

contains

  function nfa_match(nfa, text, start_pos, ignore_case) result(res)
    !> Try to match NFA starting at start_pos in text
    !> Returns match result with positions if successful
    type(nfa_t), intent(in) :: nfa
    character(len=*), intent(in) :: text
    integer, intent(in) :: start_pos
    logical, intent(in) :: ignore_case
    type(match_result_t) :: res

    integer :: current(MAX_STATES), next_states(MAX_STATES)
    integer :: num_current, num_next
    integer :: i, pos, text_len
    character(len=1) :: c

    res%matched = .false.
    text_len = len_trim(text)

    if (nfa%num_states == 0) return

    ! Initialize with epsilon closure of start state
    num_current = 0
    call epsilon_closure(nfa, nfa%start_state, current, num_current, &
                         text, start_pos, text_len)

    ! Check if already accepting (empty pattern case)
    if (is_accepting(nfa, current, num_current)) then
      res%matched = .true.
      res%match_start = start_pos
      res%match_end = start_pos - 1  ! Empty match
      return
    end if

    ! Process each character
    pos = start_pos
    do while (pos <= text_len .and. num_current > 0)
      c = text(pos:pos)

      ! Compute next states
      num_next = 0
      do i = 1, num_current
        call step(nfa, current(i), c, pos, text, text_len, ignore_case, &
                  next_states, num_next)
      end do

      ! Compute epsilon closure of next states
      num_current = 0
      do i = 1, num_next
        call epsilon_closure(nfa, next_states(i), current, num_current, &
                             text, pos + 1, text_len)
      end do

      pos = pos + 1

      ! Check for acceptance
      if (is_accepting(nfa, current, num_current)) then
        res%matched = .true.
        res%match_start = start_pos
        res%match_end = pos - 1
        ! Don't return yet - try to find longest match (greedy)
      end if
    end do

  end function nfa_match

  function nfa_search(nfa, text, ignore_case) result(res)
    !> Search for pattern anywhere in text
    type(nfa_t), intent(in) :: nfa
    character(len=*), intent(in) :: text
    logical, intent(in) :: ignore_case
    type(match_result_t) :: res

    integer :: i, text_len
    type(match_result_t) :: try_res

    res%matched = .false.
    text_len = len_trim(text)

    ! Try matching at each position
    do i = 1, text_len + 1  ! +1 to handle empty string match at end
      try_res = nfa_match(nfa, text, i, ignore_case)
      if (try_res%matched) then
        res = try_res
        return
      end if
    end do

  end function nfa_search

  subroutine epsilon_closure(nfa, state, states, num_states, text, pos, text_len)
    !> Compute epsilon closure of a state, adding to states array
    type(nfa_t), intent(in) :: nfa
    integer, intent(in) :: state
    integer, intent(inout) :: states(MAX_STATES)
    integer, intent(inout) :: num_states
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos, text_len

    integer :: stack(MAX_STATES), stack_top
    integer :: current, i, target
    type(nfa_transition_t) :: trans
    logical :: visited(MAX_STATES)

    visited = .false.
    stack_top = 1
    stack(1) = state

    do while (stack_top > 0)
      current = stack(stack_top)
      stack_top = stack_top - 1

      if (current < 1 .or. current > nfa%num_states) cycle
      if (visited(current)) cycle
      visited(current) = .true.

      ! Add to result
      if (num_states < MAX_STATES) then
        num_states = num_states + 1
        states(num_states) = current
      end if

      ! Follow epsilon transitions and anchors
      do i = 1, nfa%states(current)%num_trans
        trans = nfa%states(current)%trans(i)

        if (trans%trans_type == TRANS_EPSILON) then
          ! Check for backref marker (negative anchor_type)
          if (trans%anchor_type < 0) then
            ! Backref - skip for now (would need captured text)
            cycle
          end if

          target = trans%target
          if (target >= 1 .and. target <= nfa%num_states .and. .not. visited(target)) then
            stack_top = stack_top + 1
            stack(stack_top) = target
          end if

        else if (trans%trans_type == TRANS_ANCHOR) then
          ! Check if anchor matches
          if (anchor_matches(trans%anchor_type, text, pos, text_len)) then
            target = trans%target
            if (target >= 1 .and. target <= nfa%num_states .and. .not. visited(target)) then
              stack_top = stack_top + 1
              stack(stack_top) = target
            end if
          end if
        end if
      end do
    end do

  end subroutine epsilon_closure

  subroutine step(nfa, state, c, pos, text, text_len, ignore_case, next_states, num_next)
    !> Take one step from state on character c
    type(nfa_t), intent(in) :: nfa
    integer, intent(in) :: state
    character(len=1), intent(in) :: c
    integer, intent(in) :: pos, text_len
    character(len=*), intent(in) :: text
    logical, intent(in) :: ignore_case
    integer, intent(inout) :: next_states(MAX_STATES)
    integer, intent(inout) :: num_next

    integer :: i
    type(nfa_transition_t) :: trans
    character(len=1) :: c_lower, match_lower

    do i = 1, nfa%states(state)%num_trans
      trans = nfa%states(state)%trans(i)

      select case (trans%trans_type)
        case (TRANS_CHAR)
          if (ignore_case) then
            c_lower = to_lower_char(c)
            match_lower = to_lower_char(trans%match_char)
            if (c_lower == match_lower) then
              call add_if_unique(next_states, num_next, trans%target)
            end if
          else
            if (c == trans%match_char) then
              call add_if_unique(next_states, num_next, trans%target)
            end if
          end if

        case (TRANS_CLASS)
          if (char_in_class(c, trans%char_class, trans%negated, ignore_case)) then
            call add_if_unique(next_states, num_next, trans%target)
          end if

        case (TRANS_ANY)
          ! Match any character except newline
          if (c /= char(10)) then
            call add_if_unique(next_states, num_next, trans%target)
          end if

      end select
    end do

  end subroutine step

  function is_accepting(nfa, states, num_states) result(res)
    !> Check if any state in the set is an accepting state
    type(nfa_t), intent(in) :: nfa
    integer, intent(in) :: states(MAX_STATES)
    integer, intent(in) :: num_states
    logical :: res

    integer :: i

    res = .false.
    do i = 1, num_states
      if (states(i) >= 1 .and. states(i) <= nfa%num_states) then
        if (nfa%states(states(i))%is_accept) then
          res = .true.
          return
        end if
      end if
    end do

  end function is_accepting

  function anchor_matches(anchor_type, text, pos, text_len) result(matches)
    !> Check if anchor matches at position
    integer, intent(in) :: anchor_type
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos, text_len
    logical :: matches

    logical :: at_start, at_end, prev_word, curr_word

    matches = .false.

    at_start = (pos == 1) .or. (pos < 1)
    at_end = (pos > text_len)

    select case (anchor_type)
      case (1)  ! ^ start anchor
        if (at_start) then
          matches = .true.
        else if (pos > 1 .and. pos <= text_len + 1) then
          matches = (text(pos-1:pos-1) == char(10))
        end if

      case (2)  ! $ end anchor
        if (at_end) then
          matches = .true.
        else if (pos >= 1 .and. pos <= text_len) then
          matches = (text(pos:pos) == char(10))
        end if

      case (3)  ! \< word start
        prev_word = .false.
        curr_word = .false.
        if (pos > 1 .and. pos <= text_len + 1) prev_word = is_word_char(text(pos-1:pos-1))
        if (pos >= 1 .and. pos <= text_len) curr_word = is_word_char(text(pos:pos))
        matches = (.not. prev_word) .and. curr_word

      case (4)  ! \> word end
        prev_word = .false.
        curr_word = .false.
        if (pos > 1 .and. pos <= text_len + 1) prev_word = is_word_char(text(pos-1:pos-1))
        if (pos >= 1 .and. pos <= text_len) curr_word = is_word_char(text(pos:pos))
        matches = prev_word .and. (.not. curr_word)

      case (5)  ! \b word boundary
        prev_word = .false.
        curr_word = .false.
        if (pos > 1 .and. pos <= text_len + 1) prev_word = is_word_char(text(pos-1:pos-1))
        if (pos >= 1 .and. pos <= text_len) curr_word = is_word_char(text(pos:pos))
        matches = prev_word .neqv. curr_word

      case (6)  ! \B not word boundary
        prev_word = .false.
        curr_word = .false.
        if (pos > 1 .and. pos <= text_len + 1) prev_word = is_word_char(text(pos-1:pos-1))
        if (pos >= 1 .and. pos <= text_len) curr_word = is_word_char(text(pos:pos))
        matches = prev_word .eqv. curr_word
    end select

  end function anchor_matches

  pure function is_word_char(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    integer :: ic

    ic = ichar(c)
    res = (ic >= ichar('a') .and. ic <= ichar('z')) .or. &
          (ic >= ichar('A') .and. ic <= ichar('Z')) .or. &
          (ic >= ichar('0') .and. ic <= ichar('9')) .or. &
          (c == '_')
  end function is_word_char

  pure function to_lower_char(c) result(lower)
    character(len=1), intent(in) :: c
    character(len=1) :: lower
    integer :: ic

    ic = ichar(c)
    if (ic >= ichar('A') .and. ic <= ichar('Z')) then
      lower = char(ic + 32)
    else
      lower = c
    end if
  end function to_lower_char

  function char_in_class(c, char_class, negated, ignore_case) result(res)
    character(len=1), intent(in) :: c
    logical, intent(in) :: char_class(0:255)
    logical, intent(in) :: negated, ignore_case
    logical :: res

    integer :: ic
    character(len=1) :: c_lower, c_upper

    ic = ichar(c)
    res = char_class(ic)

    ! For case-insensitive, check both cases
    if (ignore_case .and. .not. res) then
      c_lower = to_lower_char(c)
      c_upper = to_upper_char(c)
      if (c_lower /= c) res = char_class(ichar(c_lower))
      if (.not. res .and. c_upper /= c) res = char_class(ichar(c_upper))
    end if

    if (negated) res = .not. res

  end function char_in_class

  pure function to_upper_char(c) result(upper)
    character(len=1), intent(in) :: c
    character(len=1) :: upper
    integer :: ic

    ic = ichar(c)
    if (ic >= ichar('a') .and. ic <= ichar('z')) then
      upper = char(ic - 32)
    else
      upper = c
    end if
  end function to_upper_char

  subroutine add_if_unique(arr, num, val)
    integer, intent(inout) :: arr(MAX_STATES)
    integer, intent(inout) :: num
    integer, intent(in) :: val

    integer :: i

    ! Check if already present
    do i = 1, num
      if (arr(i) == val) return
    end do

    ! Add if space available
    if (num < MAX_STATES) then
      num = num + 1
      arr(num) = val
    end if

  end subroutine add_if_unique

end module regex_engine
