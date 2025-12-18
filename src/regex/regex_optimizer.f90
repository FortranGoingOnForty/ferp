module regex_optimizer
  !> Regex optimization module for FERP
  !> Provides optimized NFA matching with:
  !>   - Literal prefix extraction for Boyer-Moore skip
  !>   - Bit vector state sets for O(1) operations
  !>   - Lazy DFA state caching
  !>   - Anchored pattern fast paths
  !>   - Aho-Corasick for alternation patterns
  use regex_types
  use aho_corasick
  implicit none
  private

  public :: optimized_nfa_t
  public :: optimize_nfa, optimized_match, optimized_search
  public :: try_build_aho_corasick

  integer, parameter :: MAX_STATES = 1024
  integer, parameter :: MAX_PREFIX_LEN = 64
  integer, parameter :: DFA_CACHE_SIZE = 256  ! Cache recent state transitions
  integer, parameter :: MAX_DFA_STATES = 512  ! Max DFA states before fallback to NFA
  integer, parameter :: DFA_DEAD_STATE = 0    ! Special state: no match possible

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

  !> Full DFA state - precomputed transitions for all 256 characters
  type :: dfa_state_t
    integer :: transitions(0:255) = DFA_DEAD_STATE  ! Next state for each byte
    type(state_set_t) :: nfa_states                  ! Corresponding NFA state set
    logical :: is_accept = .false.                   ! Is this an accepting state?
    integer(8) :: state_hash = 0                     ! Hash for lookup
  end type dfa_state_t

  !> Compiled DFA for O(n) matching
  type :: compiled_dfa_t
    type(dfa_state_t), allocatable :: states(:)     ! DFA states
    integer :: num_states = 0                        ! Number of states built
    integer :: start_state = 0                       ! Starting DFA state
    logical :: compiled = .false.                    ! DFA successfully compiled
    logical :: too_large = .false.                   ! DFA exceeded size limit
  end type compiled_dfa_t

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
    type(compiled_dfa_t) :: dfa                  ! Full compiled DFA (if available)
    logical :: use_dfa = .false.                 ! Use DFA instead of NFA
    type(ac_automaton_t) :: ac                   ! Aho-Corasick automaton (for alternation)
    logical :: use_aho_corasick = .false.        ! Use Aho-Corasick for matching
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
    opt%use_dfa = .false.

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

    ! Try to compile full DFA for O(n) matching
    ! Only for patterns without any position-dependent transitions (anchors)
    if (.not. has_anchor_transitions(opt%nfa)) then
      call compile_dfa(opt)
      ! DEBUG: Print DFA compilation result (uncomment for debugging)
      ! write(0,*) 'DFA compiled:', opt%use_dfa, 'states:', opt%dfa%num_states, 'too_large:', opt%dfa%too_large
    end if

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

  function has_anchor_transitions(nfa) result(has_anchors)
    !> Check if NFA has any anchor transitions (position-dependent)
    !> These include ^, $, \<, \>, \b, \B
    type(nfa_t), intent(in) :: nfa
    logical :: has_anchors

    integer :: state, i
    type(nfa_transition_t) :: trans

    has_anchors = .false.

    do state = 1, nfa%num_states
      do i = 1, nfa%states(state)%num_trans
        trans = nfa%states(state)%trans(i)
        if (trans%trans_type == TRANS_ANCHOR) then
          has_anchors = .true.
          return
        end if
      end do
    end do
  end function has_anchor_transitions

  !---------------------------------------------------------------------------
  ! DFA Compilation: Convert NFA to DFA for O(n) matching
  !---------------------------------------------------------------------------

  subroutine compile_dfa(opt)
    !> Compile NFA to DFA using subset construction
    !> Creates DFA states lazily, stopping if too many states
    type(optimized_nfa_t), intent(inout) :: opt

    type(state_set_t) :: start_set, next_set
    integer :: worklist(MAX_DFA_STATES), work_head, work_tail
    integer :: dfa_idx, char_code, next_idx, old_num_states

    ! Allocate DFA states
    if (allocated(opt%dfa%states)) deallocate(opt%dfa%states)
    allocate(opt%dfa%states(MAX_DFA_STATES))
    opt%dfa%num_states = 0
    opt%dfa%compiled = .false.
    opt%dfa%too_large = .false.
    opt%use_dfa = .false.

    ! Compute start state: epsilon closure of NFA start
    call start_set%clear()
    call compute_epsilon_closure_basic(opt%nfa, opt%nfa%start_state, start_set)

    if (start_set%is_empty()) return

    ! Create initial DFA state
    opt%dfa%num_states = 1
    opt%dfa%states(1)%nfa_states = start_set
    opt%dfa%states(1)%state_hash = start_set%hash()
    opt%dfa%states(1)%is_accept = is_accepting_set(opt%nfa, start_set)
    opt%dfa%start_state = 1

    ! Initialize worklist with start state
    work_head = 1
    work_tail = 1
    worklist(1) = 1

    ! Process worklist: for each DFA state, compute transitions
    do while (work_head <= work_tail)
      dfa_idx = worklist(work_head)
      work_head = work_head + 1

      ! Compute transitions for all 256 characters
      ! For case-insensitive matching, we compute transitions for both cases
      ! and union them so 'a' and 'A' go to the same DFA state
      do char_code = 0, 255
        call next_set%clear()

        ! Compute NFA transitions for this character
        call compute_char_transitions_simple(opt%nfa, opt%dfa%states(dfa_idx)%nfa_states, &
                                             char(char_code), next_set)

        ! For alphabetic characters, also compute transitions for opposite case
        if (char_code >= ichar('a') .and. char_code <= ichar('z')) then
          ! Also try uppercase
          call compute_char_transitions_simple(opt%nfa, opt%dfa%states(dfa_idx)%nfa_states, &
                                               char(char_code - 32), next_set)
        else if (char_code >= ichar('A') .and. char_code <= ichar('Z')) then
          ! Also try lowercase
          call compute_char_transitions_simple(opt%nfa, opt%dfa%states(dfa_idx)%nfa_states, &
                                               char(char_code + 32), next_set)
        end if

        ! Compute epsilon closure of result
        if (.not. next_set%is_empty()) then
          call expand_epsilon_closure_simple(opt%nfa, next_set)
        end if

        if (next_set%is_empty()) then
          opt%dfa%states(dfa_idx)%transitions(char_code) = DFA_DEAD_STATE
        else
          ! Find or create DFA state for this NFA state set
          old_num_states = opt%dfa%num_states
          next_idx = find_or_create_dfa_state(opt%dfa, next_set, opt%nfa)

          if (next_idx == -1) then
            ! Too many DFA states - abort
            opt%dfa%too_large = .true.
            opt%dfa%compiled = .false.
            return
          end if

          opt%dfa%states(dfa_idx)%transitions(char_code) = next_idx

          ! Add new state to worklist only if it was just created
          if (opt%dfa%num_states > old_num_states) then
            work_tail = work_tail + 1
            if (work_tail > MAX_DFA_STATES) then
              opt%dfa%too_large = .true.
              opt%dfa%compiled = .false.
              return
            end if
            worklist(work_tail) = next_idx
          end if
        end if
      end do
    end do

    opt%dfa%compiled = .true.
    opt%use_dfa = .true.

  end subroutine compile_dfa

  function find_or_create_dfa_state(dfa, nfa_states, nfa) result(idx)
    !> Find existing DFA state for NFA state set, or create new one
    !> Returns -1 if DFA state limit exceeded
    type(compiled_dfa_t), intent(inout) :: dfa
    type(state_set_t), intent(in) :: nfa_states
    type(nfa_t), intent(in) :: nfa
    integer :: idx

    integer(8) :: h
    integer :: i

    h = nfa_states%hash()

    ! Search existing states
    do i = 1, dfa%num_states
      if (dfa%states(i)%state_hash == h .and. &
          dfa%states(i)%nfa_states%equals(nfa_states)) then
        idx = i
        return
      end if
    end do

    ! Create new state
    if (dfa%num_states >= MAX_DFA_STATES) then
      idx = -1
      return
    end if

    dfa%num_states = dfa%num_states + 1
    idx = dfa%num_states
    dfa%states(idx)%nfa_states = nfa_states
    dfa%states(idx)%state_hash = h
    dfa%states(idx)%is_accept = is_accepting_set(nfa, nfa_states)
    dfa%states(idx)%transitions = DFA_DEAD_STATE

  end function find_or_create_dfa_state

  subroutine compute_char_transitions_simple(nfa, current, c, next_set)
    !> Compute character transitions without case folding (for DFA compilation)
    type(nfa_t), intent(in) :: nfa
    type(state_set_t), intent(in) :: current
    character(len=1), intent(in) :: c
    type(state_set_t), intent(inout) :: next_set

    integer :: state, word_idx, bit_idx, i
    integer(8) :: word, mask
    type(nfa_transition_t) :: trans

    do word_idx = 1, size(current%bits)
      word = current%bits(word_idx)
      if (word == 0) cycle

      do bit_idx = 0, 63
        mask = ishft(1_8, bit_idx)
        if (iand(word, mask) /= 0) then
          state = (word_idx - 1) * 64 + bit_idx + 1
          if (state > nfa%num_states) cycle

          do i = 1, nfa%states(state)%num_trans
            trans = nfa%states(state)%trans(i)

            select case (trans%trans_type)
              case (TRANS_CHAR)
                if (c == trans%match_char) then
                  call next_set%add(trans%target)
                end if

              case (TRANS_CLASS)
                if (trans%char_class(ichar(c)) .neqv. trans%negated) then
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
  end subroutine compute_char_transitions_simple

  subroutine expand_epsilon_closure_simple(nfa, state_set)
    !> Expand state set to include epsilon closure (in-place)
    type(nfa_t), intent(in) :: nfa
    type(state_set_t), intent(inout) :: state_set

    type(state_set_t) :: result
    integer :: word_idx, bit_idx, state
    integer(8) :: word, mask

    call result%clear()

    do word_idx = 1, size(state_set%bits)
      word = state_set%bits(word_idx)
      if (word == 0) cycle

      do bit_idx = 0, 63
        mask = ishft(1_8, bit_idx)
        if (iand(word, mask) /= 0) then
          state = (word_idx - 1) * 64 + bit_idx + 1
          if (state <= nfa%num_states) then
            call compute_epsilon_closure_basic(nfa, state, result)
          end if
        end if
      end do
    end do

    call state_set%copy_from(result)
  end subroutine expand_epsilon_closure_simple

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

    ! Fast path: use Aho-Corasick for alternation patterns
    ! Only use AC if ignore_case setting matches what was compiled
    if (opt%use_aho_corasick) then
      if (ignore_case .eqv. opt%ac%ignore_case) then
        res = ac_optimized_search(opt%ac, text)
        return
      end if
    end if

    if (opt%nfa%num_states == 0) return

    ! Fast path: use DFA if available (O(n) matching)
    ! DFA now supports case-insensitive matching via case-folded transitions
    if (opt%use_dfa) then
      res = dfa_search(opt%dfa, text, text_len)
      return
    end if

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

  function dfa_search(dfa, text, text_len) result(res)
    !> Fast O(n) DFA-based search
    !> Tries each starting position and returns first match
    type(compiled_dfa_t), intent(in) :: dfa
    character(len=*), intent(in) :: text
    integer, intent(in) :: text_len
    type(match_result_t) :: res

    integer :: start_pos
    type(match_result_t) :: try_res

    res%matched = .false.

    if (.not. dfa%compiled .or. dfa%num_states == 0) return

    ! Try each starting position
    do start_pos = 1, text_len + 1
      try_res = dfa_match(dfa, text, text_len, start_pos)
      if (try_res%matched) then
        res = try_res
        return
      end if
    end do

  end function dfa_search

  function dfa_match(dfa, text, text_len, start_pos) result(res)
    !> O(n) DFA matching from a specific position
    !> Just follows transition table - no state set operations
    type(compiled_dfa_t), intent(in) :: dfa
    character(len=*), intent(in) :: text
    integer, intent(in) :: text_len, start_pos
    type(match_result_t) :: res

    integer :: state, pos, char_code

    res%matched = .false.

    if (.not. dfa%compiled) return

    state = dfa%start_state
    pos = start_pos

    ! Check if start state is accepting (empty match)
    if (dfa%states(state)%is_accept) then
      res%matched = .true.
      res%match_start = start_pos
      res%match_end = start_pos - 1
    end if

    ! Process each character
    do while (pos <= text_len)
      char_code = ichar(text(pos:pos))
      state = dfa%states(state)%transitions(char_code)

      if (state == DFA_DEAD_STATE) exit

      pos = pos + 1

      ! Check for acceptance (greedy - find longest)
      if (dfa%states(state)%is_accept) then
        res%matched = .true.
        res%match_start = start_pos
        res%match_end = pos - 1
      end if
    end do

  end function dfa_match

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

  !---------------------------------------------------------------------------
  ! Aho-Corasick Integration for Alternation Patterns
  !---------------------------------------------------------------------------

  subroutine try_build_aho_corasick(opt, pattern, is_ere, ignore_case)
    !> Try to build Aho-Corasick automaton for simple alternation patterns
    !> Pattern like "foo|bar|baz" with only literal characters and | separators
    type(optimized_nfa_t), intent(inout) :: opt
    character(len=*), intent(in) :: pattern
    logical, intent(in) :: is_ere, ignore_case

    character(len=4096), allocatable :: alternatives(:)
    integer :: num_alternatives, ierr
    logical :: is_simple

    allocate(alternatives(1000))

    opt%use_aho_corasick = .false.

    ! Check if pattern is simple alternation of literals
    call parse_simple_alternation(pattern, is_ere, alternatives, num_alternatives, is_simple)

    ! DEBUG (commented out for production)
    ! write(error_unit, '(A,I0,A,L1)') 'DEBUG AC: num_alt=', num_alternatives, ' is_simple=', is_simple

    if (.not. is_simple .or. num_alternatives < 2) return

    ! Build Aho-Corasick automaton
    call ac_build(opt%ac, alternatives, num_alternatives, ignore_case, ierr)

    if (ierr == 0) then
      opt%use_aho_corasick = .true.
    end if

    deallocate(alternatives)

  end subroutine try_build_aho_corasick

  subroutine parse_simple_alternation(pattern, is_ere, alternatives, num_alt, is_simple)
    !> Parse pattern to check if it's simple alternation of literals
    !> Returns the alternatives if so
    character(len=*), intent(in) :: pattern
    logical, intent(in) :: is_ere
    character(len=*), intent(out) :: alternatives(:)
    integer, intent(out) :: num_alt
    logical, intent(out) :: is_simple

    integer :: i, pat_len, alt_start, alt_len
    character(len=1) :: c, next_c
    logical :: in_escape

    is_simple = .true.
    num_alt = 0
    pat_len = len_trim(pattern)

    if (pat_len == 0) then
      is_simple = .false.
      return
    end if

    alt_start = 1
    alt_len = 0
    in_escape = .false.
    i = 1

    do while (i <= pat_len)
      c = pattern(i:i)

      if (in_escape) then
        ! In ERE mode, \| is literal |
        ! In BRE mode, \| is alternation (GNU extension)
        if (c == '|' .and. .not. is_ere) then
          ! BRE alternation
          if (alt_len > 0) then
            num_alt = num_alt + 1
            if (num_alt > size(alternatives)) then
              is_simple = .false.
              return
            end if
            alternatives(num_alt) = pattern(alt_start:alt_start+alt_len-1)
          else
            ! Empty alternative - still valid
            num_alt = num_alt + 1
            alternatives(num_alt) = ''
          end if
          alt_start = i + 1
          alt_len = 0
        else if (c == '(' .or. c == ')' .or. c == '{' .or. c == '}' .or. &
                 c == '<' .or. c == '>' .or. c == 'b' .or. c == 'B' .or. &
                 c == 'd' .or. c == 'D' .or. c == 'w' .or. c == 'W' .or. &
                 c == 's' .or. c == 'S' .or. c == '1' .or. c == '2' .or. &
                 c == '3' .or. c == '4' .or. c == '5' .or. c == '6' .or. &
                 c == '7' .or. c == '8' .or. c == '9') then
          ! Regex metacharacter - not simple
          is_simple = .false.
          return
        else
          ! Escaped literal character (e.g., \., \*, etc.)
          alt_len = alt_len + 1
        end if
        in_escape = .false.
        i = i + 1
        cycle
      end if

      if (c == '\') then
        in_escape = .true.
        i = i + 1
        cycle
      end if

      ! Check for metacharacters
      if (is_ere) then
        ! ERE mode: | is alternation, . * + ? [ ] ^ $ ( ) { } are metacharacters
        if (c == '|') then
          ! Alternation separator
          if (alt_len > 0) then
            num_alt = num_alt + 1
            if (num_alt > size(alternatives)) then
              is_simple = .false.
              return
            end if
            alternatives(num_alt) = pattern(alt_start:alt_start+alt_len-1)
          else
            num_alt = num_alt + 1
            alternatives(num_alt) = ''
          end if
          alt_start = i + 1
          alt_len = 0
          i = i + 1
          cycle
        else if (c == '.' .or. c == '*' .or. c == '+' .or. c == '?' .or. &
                 c == '[' .or. c == ']' .or. c == '^' .or. c == '$' .or. &
                 c == '(' .or. c == ')' .or. c == '{' .or. c == '}') then
          ! Metacharacter - not simple alternation
          is_simple = .false.
          return
        end if
      else
        ! BRE mode: only . * [ ] ^ $ are metacharacters
        ! | is literal, \| is alternation (GNU extension)
        if (c == '.' .or. c == '*' .or. c == '[' .or. c == ']' .or. &
            c == '^' .or. c == '$') then
          is_simple = .false.
          return
        end if
      end if

      ! Regular literal character
      alt_len = alt_len + 1
      i = i + 1
    end do

    ! Handle last alternative
    if (alt_len > 0 .or. num_alt > 0) then
      num_alt = num_alt + 1
      if (num_alt > size(alternatives)) then
        is_simple = .false.
        return
      end if
      if (alt_len > 0) then
        alternatives(num_alt) = pattern(alt_start:alt_start+alt_len-1)
      else
        alternatives(num_alt) = ''
      end if
    end if

    ! Need at least 2 alternatives for Aho-Corasick to be useful
    if (num_alt < 2) then
      is_simple = .false.
    end if

  end subroutine parse_simple_alternation

  function ac_optimized_search(ac, text) result(res)
    !> Search using Aho-Corasick automaton
    type(ac_automaton_t), intent(in) :: ac
    character(len=*), intent(in) :: text
    type(match_result_t) :: res

    type(ac_match_t) :: ac_match

    res%matched = .false.

    ac_match = ac_search(ac, text)
    if (ac_match%matched) then
      res%matched = .true.
      res%match_start = ac_match%start_pos
      res%match_end = ac_match%end_pos
    end if

  end function ac_optimized_search

end module regex_optimizer
