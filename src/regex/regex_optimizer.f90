module regex_optimizer
  !> Regex optimization module for FERP
  !> Provides optimized NFA matching with:
  !>   - Literal prefix extraction for Boyer-Moore skip
  !>   - Bit vector state sets for O(1) operations
  !>   - Lazy DFA state caching
  !>   - Anchored pattern fast paths
  use regex_types
  implicit none
  private

  public :: optimized_nfa_t
  public :: optimize_nfa, optimized_match, optimized_search

  integer, parameter :: MAX_STATES = 1024
  integer, parameter :: MAX_PREFIX_LEN = 64
  integer, parameter :: DFA_CACHE_SIZE = 256  ! Cache recent state transitions

  !> Bit vector for state sets - much faster than array lookup
  type :: state_set_t
    integer(8) :: bits(MAX_STATES / 64 + 1) = 0
    integer :: count = 0
  contains
    procedure :: clear => state_set_clear
    procedure :: add => state_set_add
    procedure :: contains => state_set_contains
    procedure :: is_empty => state_set_is_empty
    procedure :: copy_from => state_set_copy
    procedure :: hash => state_set_hash
    procedure :: equals => state_set_equals
  end type state_set_t

  !> DFA cache entry - caches (state_set_hash, char) -> next_states transitions
  type :: dfa_cache_entry_t
    integer(8) :: state_hash = 0        ! Hash of source state set
    integer :: char_code = -1           ! Character being matched
    type(state_set_t) :: next_states    ! Resulting states after transition
    logical :: valid = .false.          ! Entry is populated
    logical :: is_case_insensitive = .false.  ! Case sensitivity flag
  end type dfa_cache_entry_t

  !> Optimized NFA with precomputed data
  type :: optimized_nfa_t
    type(nfa_t) :: nfa                          ! Original NFA
    character(len=MAX_PREFIX_LEN) :: prefix = '' ! Literal prefix for quick skip
    integer :: prefix_len = 0
    logical :: anchored_start = .false.          ! Pattern starts with ^
    logical :: anchored_end = .false.            ! Pattern ends with $
    integer :: skip_table(0:255) = 0             ! Boyer-Moore skip table for prefix
    type(state_set_t) :: start_closure           ! Pre-computed start state epsilon closure
    type(dfa_cache_entry_t) :: dfa_cache(DFA_CACHE_SIZE)  ! Lazy DFA cache
    logical :: optimized = .false.
  end type optimized_nfa_t

contains

  !---------------------------------------------------------------------------
  ! State Set Operations (Bit Vector)
  !---------------------------------------------------------------------------

  subroutine state_set_clear(this)
    class(state_set_t), intent(inout) :: this
    this%bits = 0
    this%count = 0
  end subroutine state_set_clear

  subroutine state_set_add(this, state)
    class(state_set_t), intent(inout) :: this
    integer, intent(in) :: state
    integer :: word_idx, bit_idx
    integer(8) :: mask

    if (state < 1 .or. state > MAX_STATES) return

    word_idx = (state - 1) / 64 + 1
    bit_idx = mod(state - 1, 64)
    mask = ishft(1_8, bit_idx)

    if (iand(this%bits(word_idx), mask) == 0) then
      this%bits(word_idx) = ior(this%bits(word_idx), mask)
      this%count = this%count + 1
    end if
  end subroutine state_set_add

  function state_set_contains(this, state) result(found)
    class(state_set_t), intent(in) :: this
    integer, intent(in) :: state
    logical :: found
    integer :: word_idx, bit_idx
    integer(8) :: mask

    found = .false.
    if (state < 1 .or. state > MAX_STATES) return

    word_idx = (state - 1) / 64 + 1
    bit_idx = mod(state - 1, 64)
    mask = ishft(1_8, bit_idx)

    found = iand(this%bits(word_idx), mask) /= 0
  end function state_set_contains

  function state_set_is_empty(this) result(empty)
    class(state_set_t), intent(in) :: this
    logical :: empty
    empty = this%count == 0
  end function state_set_is_empty

  subroutine state_set_copy(this, other)
    class(state_set_t), intent(inout) :: this
    type(state_set_t), intent(in) :: other
    this%bits = other%bits
    this%count = other%count
  end subroutine state_set_copy

  function state_set_hash(this) result(h)
    !> Compute a hash of the state set for cache lookup
    !> Uses FNV-1a style hashing on the bit words
    class(state_set_t), intent(in) :: this
    integer(8) :: h
    integer :: i
    integer(8), parameter :: FNV_OFFSET = int(Z'CBF29CE484222325', 8)
    integer(8), parameter :: FNV_PRIME = int(Z'100000001B3', 8)

    h = FNV_OFFSET
    do i = 1, size(this%bits)
      h = ieor(h, this%bits(i))
      h = h * FNV_PRIME
    end do
  end function state_set_hash

  function state_set_equals(this, other) result(eq)
    !> Check if two state sets are identical
    class(state_set_t), intent(in) :: this
    type(state_set_t), intent(in) :: other
    logical :: eq
    integer :: i

    eq = .false.
    if (this%count /= other%count) return

    do i = 1, size(this%bits)
      if (this%bits(i) /= other%bits(i)) return
    end do

    eq = .true.
  end function state_set_equals

  !---------------------------------------------------------------------------
  ! Optimization: Analyze NFA and extract optimizations
  !---------------------------------------------------------------------------

  subroutine optimize_nfa(opt, nfa)
    type(optimized_nfa_t), intent(out) :: opt
    type(nfa_t), intent(in) :: nfa

    opt%nfa = nfa
    opt%prefix_len = 0
    opt%prefix = ''
    opt%anchored_start = .false.
    opt%anchored_end = .false.

    ! Extract literal prefix and detect anchors
    call extract_prefix_and_anchors(opt)

    ! Build Boyer-Moore skip table for prefix
    if (opt%prefix_len > 0) then
      call build_skip_table(opt%prefix, opt%prefix_len, opt%skip_table)
    end if

    ! Pre-compute start state epsilon closure (position-independent part)
    call precompute_start_closure(opt)

    ! Clear DFA cache
    opt%dfa_cache%valid = .false.

    opt%optimized = .true.

  end subroutine optimize_nfa

  subroutine extract_prefix_and_anchors(opt)
    type(optimized_nfa_t), intent(inout) :: opt
    integer :: state, i, prefix_len
    type(nfa_transition_t) :: trans
    logical :: done

    prefix_len = 0
    state = opt%nfa%start_state

    ! Check for start anchor
    if (state >= 1 .and. state <= opt%nfa%num_states) then
      do i = 1, opt%nfa%states(state)%num_trans
        trans = opt%nfa%states(state)%trans(i)
        if (trans%trans_type == TRANS_ANCHOR .and. trans%anchor_type == 1) then
          opt%anchored_start = .true.
          state = trans%target
          exit
        end if
      end do
    end if

    ! Extract literal prefix by following single-path character transitions
    done = .false.
    do while (.not. done .and. prefix_len < MAX_PREFIX_LEN)
      if (state < 1 .or. state > opt%nfa%num_states) exit
      if (opt%nfa%states(state)%num_trans /= 1) exit

      trans = opt%nfa%states(state)%trans(1)

      if (trans%trans_type == TRANS_CHAR) then
        prefix_len = prefix_len + 1
        opt%prefix(prefix_len:prefix_len) = trans%match_char
        state = trans%target
      else if (trans%trans_type == TRANS_EPSILON) then
        ! Follow epsilon, but only if it's the only transition
        state = trans%target
      else
        done = .true.
      end if
    end do

    opt%prefix_len = prefix_len

    ! Check for end anchor (scan accept state's incoming transitions)
    ! This is approximate - just check if accept state has anchor transition
    if (opt%nfa%accept_state >= 1 .and. opt%nfa%accept_state <= opt%nfa%num_states) then
      ! Check states that point to accept state
      do state = 1, opt%nfa%num_states
        do i = 1, opt%nfa%states(state)%num_trans
          trans = opt%nfa%states(state)%trans(i)
          if (trans%target == opt%nfa%accept_state) then
            if (trans%trans_type == TRANS_ANCHOR .and. trans%anchor_type == 2) then
              opt%anchored_end = .true.
            end if
          end if
        end do
      end do
    end if

  end subroutine extract_prefix_and_anchors

  subroutine build_skip_table(prefix, prefix_len, skip_table)
    character(len=*), intent(in) :: prefix
    integer, intent(in) :: prefix_len
    integer, intent(out) :: skip_table(0:255)
    integer :: i, c

    ! Default skip is prefix length
    skip_table = prefix_len

    ! Set skip distances for characters in prefix
    do i = 1, prefix_len - 1
      c = ichar(prefix(i:i))
      skip_table(c) = prefix_len - i
    end do
  end subroutine build_skip_table

  subroutine precompute_start_closure(opt)
    type(optimized_nfa_t), intent(inout) :: opt
    ! Compute basic epsilon closure of start state
    ! (Some closures depend on position for anchors, handle those at runtime)
    call opt%start_closure%clear()
    call compute_epsilon_closure_basic(opt%nfa, opt%nfa%start_state, opt%start_closure)
  end subroutine precompute_start_closure

  subroutine compute_epsilon_closure_basic(nfa, start_state, result_set)
    type(nfa_t), intent(in) :: nfa
    integer, intent(in) :: start_state
    type(state_set_t), intent(inout) :: result_set

    integer :: stack(MAX_STATES), stack_top
    integer :: state, i, target
    type(nfa_transition_t) :: trans

    stack_top = 1
    stack(1) = start_state

    do while (stack_top > 0)
      state = stack(stack_top)
      stack_top = stack_top - 1

      if (state < 1 .or. state > nfa%num_states) cycle
      if (result_set%contains(state)) cycle

      call result_set%add(state)

      ! Follow epsilon transitions (not anchors - those are position-dependent)
      do i = 1, nfa%states(state)%num_trans
        trans = nfa%states(state)%trans(i)
        if (trans%trans_type == TRANS_EPSILON .and. trans%anchor_type >= 0) then
          target = trans%target
          if (target >= 1 .and. target <= nfa%num_states) then
            if (.not. result_set%contains(target)) then
              stack_top = stack_top + 1
              if (stack_top <= MAX_STATES) stack(stack_top) = target
            end if
          end if
        end if
      end do
    end do
  end subroutine compute_epsilon_closure_basic

  !---------------------------------------------------------------------------
  ! Optimized Search: Use prefix to skip positions
  !---------------------------------------------------------------------------

  function optimized_search(opt, text, ignore_case) result(res)
    type(optimized_nfa_t), intent(inout) :: opt
    character(len=*), intent(in) :: text
    logical, intent(in) :: ignore_case
    type(match_result_t) :: res

    integer :: text_len, pos, skip
    type(match_result_t) :: try_res

    res%matched = .false.
    text_len = len_trim(text)

    if (opt%nfa%num_states == 0) return

    ! Fast path: anchored start - only try position 1
    if (opt%anchored_start) then
      res = optimized_match(opt, text, 1, ignore_case)
      return
    end if

    ! Use prefix to skip positions (Boyer-Moore style)
    if (opt%prefix_len > 0 .and. .not. ignore_case) then
      pos = opt%prefix_len
      do while (pos <= text_len)
        ! Check if prefix matches at this position
        if (prefix_matches(text, pos - opt%prefix_len + 1, opt%prefix, opt%prefix_len)) then
          ! Try full NFA match from this position
          try_res = optimized_match(opt, text, pos - opt%prefix_len + 1, ignore_case)
          if (try_res%matched) then
            res = try_res
            return
          end if
          pos = pos + 1
        else
          ! Skip based on mismatched character
          skip = opt%skip_table(ichar(text(pos:pos)))
          pos = pos + max(skip, 1)
        end if
      end do
    else
      ! No prefix optimization - try each position
      do pos = 1, text_len + 1
        try_res = optimized_match(opt, text, pos, ignore_case)
        if (try_res%matched) then
          res = try_res
          return
        end if
      end do
    end if

  end function optimized_search

  function prefix_matches(text, pos, prefix, prefix_len) result(matches)
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos, prefix_len
    character(len=*), intent(in) :: prefix
    logical :: matches
    integer :: i, text_len

    matches = .false.
    text_len = len_trim(text)

    if (pos < 1 .or. pos + prefix_len - 1 > text_len) return

    do i = 1, prefix_len
      if (text(pos+i-1:pos+i-1) /= prefix(i:i)) return
    end do

    matches = .true.
  end function prefix_matches

  !---------------------------------------------------------------------------
  ! Optimized Match: Use bit vectors and caching
  !---------------------------------------------------------------------------

  function optimized_match(opt, text, start_pos, ignore_case) result(res)
    type(optimized_nfa_t), intent(inout) :: opt
    character(len=*), intent(in) :: text
    integer, intent(in) :: start_pos
    logical, intent(in) :: ignore_case
    type(match_result_t) :: res

    type(state_set_t) :: current, next_set
    integer :: pos, text_len
    character(len=1) :: c

    res%matched = .false.
    text_len = len_trim(text)

    if (opt%nfa%num_states == 0) return

    ! Initialize with epsilon closure of start state (including position-dependent anchors)
    call current%clear()
    call compute_epsilon_closure_full(opt%nfa, opt%nfa%start_state, current, text, start_pos, text_len)

    ! Check if already accepting (empty pattern)
    if (is_accepting_set(opt%nfa, current)) then
      res%matched = .true.
      res%match_start = start_pos
      res%match_end = start_pos - 1
      return
    end if

    ! Process each character
    pos = start_pos
    do while (pos <= text_len .and. .not. current%is_empty())
      c = text(pos:pos)

      ! Compute next states with DFA caching
      call next_set%clear()
      call step_with_cache(opt, current, c, pos, text, text_len, ignore_case, next_set)

      ! Compute epsilon closure
      call current%clear()
      call expand_epsilon_closure(opt%nfa, next_set, current, text, pos + 1, text_len)

      pos = pos + 1

      ! Check for acceptance (greedy - continue to find longest)
      if (is_accepting_set(opt%nfa, current)) then
        res%matched = .true.
        res%match_start = start_pos
        res%match_end = pos - 1
      end if
    end do

  end function optimized_match

  subroutine compute_epsilon_closure_full(nfa, start_state, result_set, text, pos, text_len)
    type(nfa_t), intent(in) :: nfa
    integer, intent(in) :: start_state
    type(state_set_t), intent(inout) :: result_set
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos, text_len

    integer :: stack(MAX_STATES), stack_top
    integer :: state, i, target
    type(nfa_transition_t) :: trans

    stack_top = 1
    stack(1) = start_state

    do while (stack_top > 0)
      state = stack(stack_top)
      stack_top = stack_top - 1

      if (state < 1 .or. state > nfa%num_states) cycle
      if (result_set%contains(state)) cycle

      call result_set%add(state)

      do i = 1, nfa%states(state)%num_trans
        trans = nfa%states(state)%trans(i)

        if (trans%trans_type == TRANS_EPSILON) then
          if (trans%anchor_type < 0) cycle  ! Skip backrefs
          target = trans%target
          if (target >= 1 .and. target <= nfa%num_states) then
            if (.not. result_set%contains(target)) then
              stack_top = stack_top + 1
              if (stack_top <= MAX_STATES) stack(stack_top) = target
            end if
          end if

        else if (trans%trans_type == TRANS_ANCHOR) then
          if (anchor_matches_opt(trans%anchor_type, text, pos, text_len)) then
            target = trans%target
            if (target >= 1 .and. target <= nfa%num_states) then
              if (.not. result_set%contains(target)) then
                stack_top = stack_top + 1
                if (stack_top <= MAX_STATES) stack(stack_top) = target
              end if
            end if
          end if
        end if
      end do
    end do
  end subroutine compute_epsilon_closure_full

  subroutine expand_epsilon_closure(nfa, input_set, result_set, text, pos, text_len)
    type(nfa_t), intent(in) :: nfa
    type(state_set_t), intent(in) :: input_set
    type(state_set_t), intent(inout) :: result_set
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos, text_len

    integer :: state, word_idx, bit_idx
    integer(8) :: word, mask

    ! Iterate through set bits
    do word_idx = 1, size(input_set%bits)
      word = input_set%bits(word_idx)
      if (word == 0) cycle

      do bit_idx = 0, 63
        mask = ishft(1_8, bit_idx)
        if (iand(word, mask) /= 0) then
          state = (word_idx - 1) * 64 + bit_idx + 1
          if (state <= nfa%num_states) then
            call compute_epsilon_closure_full(nfa, state, result_set, text, pos, text_len)
          end if
        end if
      end do
    end do
  end subroutine expand_epsilon_closure

  subroutine step_with_cache(opt, current, c, pos, text, text_len, ignore_case, next_set)
    !> Compute next states with DFA caching
    !> Cache key: (state_set_hash, char_code, ignore_case)
    !> This avoids recomputing transitions for repeated (state_set, char) pairs
    type(optimized_nfa_t), intent(inout) :: opt
    type(state_set_t), intent(in) :: current
    character(len=1), intent(in) :: c
    integer, intent(in) :: pos, text_len
    character(len=*), intent(in) :: text
    logical, intent(in) :: ignore_case
    type(state_set_t), intent(inout) :: next_set

    integer(8) :: state_hash
    integer :: cache_idx, char_code

    ! Compute cache key
    state_hash = current%hash()
    char_code = ichar(c)

    ! Compute cache index (combine hash with char code)
    cache_idx = int(mod(abs(ieor(state_hash, int(char_code, 8))), int(DFA_CACHE_SIZE, 8))) + 1

    ! Check cache hit (using hash + char + case as key)
    ! Note: This may have rare hash collisions, but performance benefit outweighs risk
    if (opt%dfa_cache(cache_idx)%valid .and. &
        opt%dfa_cache(cache_idx)%state_hash == state_hash .and. &
        opt%dfa_cache(cache_idx)%char_code == char_code .and. &
        (opt%dfa_cache(cache_idx)%is_case_insensitive .eqv. ignore_case)) then
      ! Cache hit - copy cached result
      call next_set%copy_from(opt%dfa_cache(cache_idx)%next_states)
      return
    end if

    ! Cache miss - compute transitions
    call compute_char_transitions(opt%nfa, current, c, ignore_case, next_set)

    ! Store in cache
    opt%dfa_cache(cache_idx)%valid = .true.
    opt%dfa_cache(cache_idx)%state_hash = state_hash
    opt%dfa_cache(cache_idx)%char_code = char_code
    opt%dfa_cache(cache_idx)%is_case_insensitive = ignore_case
    call opt%dfa_cache(cache_idx)%next_states%copy_from(next_set)

  end subroutine step_with_cache

  subroutine compute_char_transitions(nfa, current, c, ignore_case, next_set)
    !> Compute character transitions without caching (called on cache miss)
    type(nfa_t), intent(in) :: nfa
    type(state_set_t), intent(in) :: current
    character(len=1), intent(in) :: c
    logical, intent(in) :: ignore_case
    type(state_set_t), intent(inout) :: next_set

    integer :: state, word_idx, bit_idx, i
    integer(8) :: word, mask
    type(nfa_transition_t) :: trans
    character(len=1) :: c_lower, match_lower

    ! Iterate through current states
    do word_idx = 1, size(current%bits)
      word = current%bits(word_idx)
      if (word == 0) cycle

      do bit_idx = 0, 63
        mask = ishft(1_8, bit_idx)
        if (iand(word, mask) /= 0) then
          state = (word_idx - 1) * 64 + bit_idx + 1
          if (state > nfa%num_states) cycle

          ! Process transitions from this state
          do i = 1, nfa%states(state)%num_trans
            trans = nfa%states(state)%trans(i)

            select case (trans%trans_type)
              case (TRANS_CHAR)
                if (ignore_case) then
                  c_lower = to_lower_char(c)
                  match_lower = to_lower_char(trans%match_char)
                  if (c_lower == match_lower) then
                    call next_set%add(trans%target)
                  end if
                else
                  if (c == trans%match_char) then
                    call next_set%add(trans%target)
                  end if
                end if

              case (TRANS_CLASS)
                if (char_in_class_opt(c, trans%char_class, trans%negated, ignore_case)) then
                  call next_set%add(trans%target)
                end if

              case (TRANS_ANY)
                if (c /= char(10)) then
                  call next_set%add(trans%target)
                end if
            end select
          end do
        end if
      end do
    end do
  end subroutine compute_char_transitions

  function is_accepting_set(nfa, states) result(res)
    type(nfa_t), intent(in) :: nfa
    type(state_set_t), intent(in) :: states
    logical :: res

    integer :: state, word_idx, bit_idx
    integer(8) :: word, mask

    res = .false.

    do word_idx = 1, size(states%bits)
      word = states%bits(word_idx)
      if (word == 0) cycle

      do bit_idx = 0, 63
        mask = ishft(1_8, bit_idx)
        if (iand(word, mask) /= 0) then
          state = (word_idx - 1) * 64 + bit_idx + 1
          if (state >= 1 .and. state <= nfa%num_states) then
            if (nfa%states(state)%is_accept) then
              res = .true.
              return
            end if
          end if
        end if
      end do
    end do
  end function is_accepting_set

  !---------------------------------------------------------------------------
  ! Helper functions
  !---------------------------------------------------------------------------

  function anchor_matches_opt(anchor_type, text, pos, text_len) result(matches)
    integer, intent(in) :: anchor_type
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos, text_len
    logical :: matches

    logical :: at_start, at_end, prev_word, curr_word

    matches = .false.
    at_start = (pos == 1) .or. (pos < 1)
    at_end = (pos > text_len)

    select case (anchor_type)
      case (1)  ! ^
        if (at_start) then
          matches = .true.
        else if (pos > 1 .and. pos <= text_len + 1) then
          matches = (text(pos-1:pos-1) == char(10))
        end if

      case (2)  ! $
        if (at_end) then
          matches = .true.
        else if (pos >= 1 .and. pos <= text_len) then
          matches = (text(pos:pos) == char(10))
        end if

      case (3)  ! \<
        prev_word = .false.
        curr_word = .false.
        if (pos > 1 .and. pos <= text_len + 1) prev_word = is_word_char_opt(text(pos-1:pos-1))
        if (pos >= 1 .and. pos <= text_len) curr_word = is_word_char_opt(text(pos:pos))
        matches = (.not. prev_word) .and. curr_word

      case (4)  ! \>
        prev_word = .false.
        curr_word = .false.
        if (pos > 1 .and. pos <= text_len + 1) prev_word = is_word_char_opt(text(pos-1:pos-1))
        if (pos >= 1 .and. pos <= text_len) curr_word = is_word_char_opt(text(pos:pos))
        matches = prev_word .and. (.not. curr_word)

      case (5)  ! \b
        prev_word = .false.
        curr_word = .false.
        if (pos > 1 .and. pos <= text_len + 1) prev_word = is_word_char_opt(text(pos-1:pos-1))
        if (pos >= 1 .and. pos <= text_len) curr_word = is_word_char_opt(text(pos:pos))
        matches = prev_word .neqv. curr_word

      case (6)  ! \B
        prev_word = .false.
        curr_word = .false.
        if (pos > 1 .and. pos <= text_len + 1) prev_word = is_word_char_opt(text(pos-1:pos-1))
        if (pos >= 1 .and. pos <= text_len) curr_word = is_word_char_opt(text(pos:pos))
        matches = prev_word .eqv. curr_word
    end select
  end function anchor_matches_opt

  pure function is_word_char_opt(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    integer :: ic
    ic = ichar(c)
    res = (ic >= ichar('a') .and. ic <= ichar('z')) .or. &
          (ic >= ichar('A') .and. ic <= ichar('Z')) .or. &
          (ic >= ichar('0') .and. ic <= ichar('9')) .or. &
          (c == '_')
  end function is_word_char_opt

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

  function char_in_class_opt(c, char_class, negated, ignore_case) result(res)
    character(len=1), intent(in) :: c
    logical, intent(in) :: char_class(0:255)
    logical, intent(in) :: negated, ignore_case
    logical :: res

    integer :: ic
    character(len=1) :: c_lower, c_upper

    ic = ichar(c)
    res = char_class(ic)

    if (ignore_case .and. .not. res) then
      c_lower = to_lower_char(c)
      if (c_lower /= c) res = char_class(ichar(c_lower))
      if (.not. res) then
        ic = ichar(c)
        if (ic >= ichar('a') .and. ic <= ichar('z')) then
          c_upper = char(ic - 32)
          res = char_class(ichar(c_upper))
        end if
      end if
    end if

    if (negated) res = .not. res
  end function char_in_class_opt

end module regex_optimizer
