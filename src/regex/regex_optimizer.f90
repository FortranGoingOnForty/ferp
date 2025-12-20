module regex_optimizer
  !> Regex optimization module for FERP
  !> Provides optimized NFA matching with:
  !>   - Literal prefix extraction for Boyer-Moore skip
  !>   - Bit vector state sets for O(1) operations
  !>   - Lazy DFA state caching
  !>   - Anchored pattern fast paths
  !>   - Aho-Corasick for alternation patterns
  !>   - Bitwise character class matching
  use regex_types
  use regex_charclass
  use aho_corasick
  use ferp_kinds, only: pattern_len
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
    ! Character equivalence classes
    integer :: char_to_class(0:255) = 0             ! Maps char code to class index
    integer :: num_classes = 256                     ! Number of equivalence classes
    logical :: use_equiv_classes = .false.           ! Using equivalence classes
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
    type(state_set_t), allocatable :: epsilon_closures(:)  ! Pre-computed epsilon closures per state
    type(dfa_cache_entry_t) :: dfa_cache(DFA_CACHE_SIZE)  ! Lazy DFA cache
    type(compiled_dfa_t) :: dfa                  ! Full compiled DFA (if available)
    logical :: use_dfa = .false.                 ! Use DFA instead of NFA
    type(ac_automaton_t) :: ac                   ! Aho-Corasick automaton (for alternation)
    logical :: use_aho_corasick = .false.        ! Use Aho-Corasick for matching
    logical :: has_backrefs = .false.            ! Pattern contains backreferences
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
    opt%has_backrefs = .false.

    ! Detect backreferences in NFA
    opt%has_backrefs = has_backref_transitions(nfa)

    ! Extract literal prefix and detect anchors
    call extract_prefix_and_anchors(opt)

    ! Build Boyer-Moore skip table for prefix
    if (opt%prefix_len > 0) then
      call build_skip_table(opt%prefix, opt%prefix_len, opt%skip_table)
    end if

    ! Pre-compute start state epsilon closure (position-independent part)
    call precompute_start_closure(opt)

    ! Pre-compute epsilon closures for all states (for fast expansion)
    call precompute_all_epsilon_closures(opt)

    ! Clear DFA cache
    opt%dfa_cache%valid = .false.

    ! Try to compile full DFA for O(n) matching
    ! Only for patterns without any position-dependent transitions (anchors)
    ! and without backreferences (which require backtracking)
    if (.not. has_anchor_transitions(opt%nfa) .and. .not. opt%has_backrefs) then
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

  subroutine precompute_all_epsilon_closures(opt)
    !> Pre-compute epsilon closure for every NFA state
    !> This allows O(1) closure lookup during matching instead of repeated traversal
    type(optimized_nfa_t), intent(inout) :: opt

    integer :: i, n

    n = opt%nfa%num_states
    if (n <= 0) return

    ! Allocate epsilon closures array
    if (allocated(opt%epsilon_closures)) deallocate(opt%epsilon_closures)
    allocate(opt%epsilon_closures(n))

    ! Compute epsilon closure for each state
    do i = 1, n
      call opt%epsilon_closures(i)%clear()
      call compute_epsilon_closure_basic(opt%nfa, i, opt%epsilon_closures(i))
    end do
  end subroutine precompute_all_epsilon_closures

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

  function has_backref_transitions(nfa) result(has_backrefs)
    !> Check if NFA has any backreference transitions
    !> Backrefs are encoded as TRANS_EPSILON with negative anchor_type
    type(nfa_t), intent(in) :: nfa
    logical :: has_backrefs

    integer :: state, i
    type(nfa_transition_t) :: trans

    has_backrefs = .false.

    do state = 1, nfa%num_states
      do i = 1, nfa%states(state)%num_trans
        trans = nfa%states(state)%trans(i)
        if (trans%trans_type == TRANS_EPSILON .and. trans%anchor_type < 0) then
          has_backrefs = .true.
          return
        end if
      end do
    end do
  end function has_backref_transitions

  !---------------------------------------------------------------------------
  ! Character Equivalence Classes
  !---------------------------------------------------------------------------

  subroutine compute_equiv_classes(nfa, char_to_class, num_classes)
    !> Compute character equivalence classes from NFA transitions
    !> Characters with identical behavior across all NFA states belong to same class
    !> This reduces DFA transition table from 256 entries to num_classes entries
    type(nfa_t), intent(in) :: nfa
    integer, intent(out) :: char_to_class(0:255)
    integer, intent(out) :: num_classes

    ! Signature for each character: encodes which transitions it triggers
    ! We use a simple approach: hash the set of (state, target) pairs for each char
    integer(8) :: char_signature(0:255)
    integer :: state, i, c, target
    type(nfa_transition_t) :: trans
    integer(8) :: sig
    integer :: class_map(0:255)  ! signature hash -> class index
    logical :: found

    ! Initialize all characters to have signature 0 (no transitions)
    char_signature = 0_8

    ! Build signature for each character based on NFA transitions
    do state = 1, nfa%num_states
      do i = 1, nfa%states(state)%num_trans
        trans = nfa%states(state)%trans(i)
        target = trans%target

        select case (trans%trans_type)
          case (TRANS_CHAR)
            ! Single character transition
            c = ichar(trans%match_char)
            ! Add (state, target) to signature using FNV-1a-like hash
            char_signature(c) = ieor(char_signature(c), &
                                     int(state * 31 + target, 8) * 1099511628211_8)

          case (TRANS_CLASS)
            ! Character class transition - add to all matching chars
            do c = 0, 255
              if (charclass_test(trans%char_bits, char(c))) then
                char_signature(c) = ieor(char_signature(c), &
                                         int(state * 31 + target, 8) * 1099511628211_8)
              end if
            end do

          case (TRANS_ANY)
            ! Dot matches all except newline
            do c = 0, 255
              if (c /= 10) then  ! Not newline
                char_signature(c) = ieor(char_signature(c), &
                                         int(state * 31 + target, 8) * 1099511628211_8)
              end if
            end do
        end select
      end do
    end do

    ! Force each alphabetic character to have a unique signature
    ! This ensures they get their own equivalence classes, so the case-folding
    ! code in DFA compilation works correctly (it relies on the class representative
    ! being alphabetic to compute transitions for both cases)
    do c = ichar('a'), ichar('z')
      ! Add unique value to each letter's signature to separate them from non-letters
      char_signature(c) = ieor(char_signature(c), int(c * 7919 + 1, 8))
      char_signature(c - 32) = ieor(char_signature(c - 32), int((c - 32) * 7919 + 1, 8))
    end do

    ! Now group characters by signature
    num_classes = 0
    class_map = -1
    char_to_class = 0

    do c = 0, 255
      sig = char_signature(c)

      ! Look for existing class with this signature
      found = .false.
      do i = 0, num_classes - 1
        if (class_map(i) /= -1) then
          ! Check if any character in class i has same signature
          ! We stored the signature hash as a proxy
          if (char_signature(class_map(i)) == sig) then
            char_to_class(c) = i
            found = .true.
            exit
          end if
        end if
      end do

      if (.not. found) then
        ! Create new class
        char_to_class(c) = num_classes
        class_map(num_classes) = c  ! Remember one char from this class
        num_classes = num_classes + 1
      end if
    end do

    ! Ensure at least one class
    if (num_classes == 0) num_classes = 1

  end subroutine compute_equiv_classes

  !---------------------------------------------------------------------------
  ! DFA Compilation: Convert NFA to DFA for O(n) matching
  !---------------------------------------------------------------------------

  subroutine compile_dfa(opt)
    !> Compile NFA to DFA using subset construction
    !> Uses character equivalence classes to reduce compilation time
    type(optimized_nfa_t), intent(inout) :: opt

    type(state_set_t) :: start_set, next_set
    integer :: worklist(MAX_DFA_STATES), work_head, work_tail
    integer :: dfa_idx, char_code, next_idx, old_num_states
    integer :: class_idx, c
    integer :: class_representative(0:255)  ! One char per class
    integer :: class_transitions(0:255)     ! Computed transition per class

    ! Allocate DFA states
    if (allocated(opt%dfa%states)) deallocate(opt%dfa%states)
    allocate(opt%dfa%states(MAX_DFA_STATES))
    opt%dfa%num_states = 0
    opt%dfa%compiled = .false.
    opt%dfa%too_large = .false.
    opt%use_dfa = .false.

    ! Compute character equivalence classes
    call compute_equiv_classes(opt%nfa, opt%dfa%char_to_class, opt%dfa%num_classes)
    opt%dfa%use_equiv_classes = (opt%dfa%num_classes < 256)

    ! Build representative character for each class
    class_representative = -1
    do c = 0, 255
      class_idx = opt%dfa%char_to_class(c)
      if (class_representative(class_idx) == -1) then
        class_representative(class_idx) = c
      end if
    end do

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

      ! First, compute transitions for each equivalence class (not all 256 chars)
      class_transitions = DFA_DEAD_STATE

      do class_idx = 0, opt%dfa%num_classes - 1
        char_code = class_representative(class_idx)
        if (char_code < 0) cycle

        call next_set%clear()

        ! Compute NFA transitions for this character
        ! Note: DFA is case-sensitive. Case-insensitive matching uses NFA path.
        call compute_char_transitions_simple(opt%nfa, opt%dfa%states(dfa_idx)%nfa_states, &
                                             char(char_code), next_set)

        ! Compute epsilon closure of result
        if (.not. next_set%is_empty()) then
          call expand_epsilon_closure_simple(opt, next_set)
        end if

        if (next_set%is_empty()) then
          class_transitions(class_idx) = DFA_DEAD_STATE
        else
          ! Find or create DFA state for this NFA state set
          old_num_states = opt%dfa%num_states
          next_idx = find_or_create_dfa_state(opt%dfa, next_set, opt%nfa)

          if (next_idx == -1) then
            opt%dfa%too_large = .true.
            opt%dfa%compiled = .false.
            return
          end if

          class_transitions(class_idx) = next_idx

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

      ! Now fill in the full 256-entry transition table from class transitions
      do c = 0, 255
        class_idx = opt%dfa%char_to_class(c)
        opt%dfa%states(dfa_idx)%transitions(c) = class_transitions(class_idx)
      end do
    end do

    ! Minimize DFA to reduce state count
    call minimize_dfa(opt%dfa)

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

  subroutine minimize_dfa(dfa)
    !> Minimize DFA using Hopcroft's algorithm
    !> Merges equivalent states to reduce DFA size
    type(compiled_dfa_t), intent(inout) :: dfa

    integer :: num_states, num_partitions
    integer, allocatable :: partition(:)      ! partition(state) = partition ID
    integer, allocatable :: part_size(:)      ! Size of each partition
    integer, allocatable :: representative(:) ! Representative state for each partition
    integer, allocatable :: new_state_id(:)   ! Mapping from old state to new state ID
    type(dfa_state_t), allocatable :: new_states(:)

    logical, allocatable :: in_worklist(:)    ! Is partition in worklist?
    integer, allocatable :: worklist(:)       ! Partitions to process
    integer :: work_head, work_tail

    integer :: i, c, state, target, part_id
    integer :: num_accept, num_reject
    integer :: old_part, new_part_id
    logical :: needs_split
    integer, allocatable :: split_marker(:)   ! Which states go to partition A on char c
    integer :: new_num_states

    num_states = dfa%num_states
    if (num_states <= 1) return  ! Nothing to minimize

    ! Allocate working arrays
    allocate(partition(num_states))
    allocate(part_size(num_states))
    allocate(representative(num_states))
    allocate(new_state_id(num_states))
    allocate(in_worklist(num_states))
    allocate(worklist(num_states))
    allocate(split_marker(num_states))

    ! Initialize partitions: accepting states = partition 1, non-accepting = partition 2
    partition = 0
    part_size = 0
    num_accept = 0
    num_reject = 0

    do i = 1, num_states
      if (dfa%states(i)%is_accept) then
        partition(i) = 1
        num_accept = num_accept + 1
      else
        partition(i) = 2
        num_reject = num_reject + 1
      end if
    end do

    part_size(1) = num_accept
    part_size(2) = num_reject
    num_partitions = 2

    ! Handle edge case: all accepting or all rejecting
    if (num_accept == 0 .or. num_reject == 0) then
      num_partitions = 1
      partition = 1
      part_size(1) = num_states
    end if

    ! Initialize worklist with smaller partition (Hopcroft optimization)
    in_worklist = .false.
    work_head = 1
    work_tail = 0

    if (num_partitions == 2) then
      if (num_accept <= num_reject) then
        work_tail = 1
        worklist(1) = 1
        in_worklist(1) = .true.
      else
        work_tail = 1
        worklist(1) = 2
        in_worklist(2) = .true.
      end if
    end if

    ! Main refinement loop
    do while (work_head <= work_tail)
      part_id = worklist(work_head)
      work_head = work_head + 1
      in_worklist(part_id) = .false.

      ! For each character, check if this partition splits others
      do c = 0, 255
        ! Mark states that transition to partition part_id on character c
        split_marker = 0
        do state = 1, num_states
          target = dfa%states(state)%transitions(c)
          if (target > 0 .and. target <= num_states) then
            if (partition(target) == part_id) then
              split_marker(state) = 1
            end if
          end if
        end do

        ! Check each existing partition for splits
        do old_part = 1, num_partitions
          ! Count states in this partition that go to part_id vs don't
          num_accept = 0  ! Reuse: count going to part_id
          num_reject = 0  ! Reuse: count not going to part_id

          do state = 1, num_states
            if (partition(state) == old_part) then
              if (split_marker(state) == 1) then
                num_accept = num_accept + 1
              else
                num_reject = num_reject + 1
              end if
            end if
          end do

          ! If partition needs splitting (has both types)
          needs_split = (num_accept > 0 .and. num_reject > 0)

          if (needs_split) then
            ! Create new partition for the smaller group
            num_partitions = num_partitions + 1
            new_part_id = num_partitions

            ! Move the smaller group to new partition
            if (num_accept <= num_reject) then
              ! Move states going to part_id to new partition
              do state = 1, num_states
                if (partition(state) == old_part .and. split_marker(state) == 1) then
                  partition(state) = new_part_id
                end if
              end do
              part_size(new_part_id) = num_accept
              part_size(old_part) = num_reject
            else
              ! Move states NOT going to part_id to new partition
              do state = 1, num_states
                if (partition(state) == old_part .and. split_marker(state) == 0) then
                  partition(state) = new_part_id
                end if
              end do
              part_size(new_part_id) = num_reject
              part_size(old_part) = num_accept
            end if

            ! Update worklist
            if (in_worklist(old_part)) then
              ! Both halves need to be in worklist
              work_tail = work_tail + 1
              worklist(work_tail) = new_part_id
              in_worklist(new_part_id) = .true.
            else
              ! Add smaller partition to worklist
              if (part_size(new_part_id) <= part_size(old_part)) then
                work_tail = work_tail + 1
                worklist(work_tail) = new_part_id
                in_worklist(new_part_id) = .true.
              else
                work_tail = work_tail + 1
                worklist(work_tail) = old_part
                in_worklist(old_part) = .true.
              end if
            end if
          end if
        end do
      end do
    end do

    ! Check if minimization actually reduced states
    if (num_partitions >= num_states) then
      ! No reduction possible
      deallocate(partition, part_size, representative, new_state_id)
      deallocate(in_worklist, worklist, split_marker)
      return
    end if

    ! Find representative for each partition (lowest numbered state)
    representative = 0
    do state = 1, num_states
      part_id = partition(state)
      if (representative(part_id) == 0) then
        representative(part_id) = state
      end if
    end do

    ! Build new state IDs (compact numbering)
    new_state_id = 0
    new_num_states = 0
    do part_id = 1, num_partitions
      if (representative(part_id) > 0) then
        new_num_states = new_num_states + 1
        ! Map all states in this partition to new state ID
        do state = 1, num_states
          if (partition(state) == part_id) then
            new_state_id(state) = new_num_states
          end if
        end do
      end if
    end do

    ! Build minimized DFA
    allocate(new_states(new_num_states))

    do part_id = 1, num_partitions
      state = representative(part_id)
      if (state == 0) cycle

      i = new_state_id(state)
      new_states(i)%is_accept = dfa%states(state)%is_accept
      new_states(i)%state_hash = dfa%states(state)%state_hash
      new_states(i)%nfa_states = dfa%states(state)%nfa_states

      ! Remap transitions
      do c = 0, 255
        target = dfa%states(state)%transitions(c)
        if (target > 0 .and. target <= num_states) then
          new_states(i)%transitions(c) = new_state_id(target)
        else
          new_states(i)%transitions(c) = DFA_DEAD_STATE
        end if
      end do
    end do

    ! Update DFA with minimized version
    deallocate(dfa%states)
    allocate(dfa%states(new_num_states))
    dfa%states = new_states
    dfa%start_state = new_state_id(dfa%start_state)
    dfa%num_states = new_num_states

    ! Cleanup
    deallocate(partition, part_size, representative, new_state_id)
    deallocate(in_worklist, worklist, split_marker, new_states)

  end subroutine minimize_dfa

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

  subroutine expand_epsilon_closure_simple(opt, state_set)
    !> Expand state set to include epsilon closure (in-place)
    !> Uses pre-computed closures for O(1) lookup per state
    type(optimized_nfa_t), intent(in) :: opt
    type(state_set_t), intent(inout) :: state_set

    integer :: word_idx, bit_idx, state, j
    integer(8) :: word, mask, original_bits(size(state_set%bits))

    ! If no pre-computed closures, fall back to computing on-the-fly
    if (.not. allocated(opt%epsilon_closures)) then
      call expand_epsilon_closure_simple_fallback(opt%nfa, state_set)
      return
    end if

    ! Save original bits to avoid processing newly added states
    original_bits = state_set%bits

    ! Expand using pre-computed closures - just OR the bit vectors
    do word_idx = 1, size(original_bits)
      word = original_bits(word_idx)
      if (word == 0) cycle

      do bit_idx = 0, 63
        mask = ishft(1_8, bit_idx)
        if (iand(word, mask) /= 0) then
          state = (word_idx - 1) * 64 + bit_idx + 1
          if (state >= 1 .and. state <= opt%nfa%num_states) then
            ! Merge pre-computed epsilon closure using bitwise OR
            do j = 1, size(state_set%bits)
              state_set%bits(j) = ior(state_set%bits(j), opt%epsilon_closures(state)%bits(j))
            end do
          end if
        end if
      end do
    end do
  end subroutine expand_epsilon_closure_simple

  subroutine expand_epsilon_closure_simple_fallback(nfa, state_set)
    !> Fallback: compute epsilon closure on-the-fly
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
  end subroutine expand_epsilon_closure_simple_fallback

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

    ! Backtracking path: use backtracking matcher for patterns with backreferences
    if (opt%has_backrefs) then
      res = backtrack_search(opt%nfa, text, text_len, ignore_case)
      return
    end if

    ! Fast path: use DFA if available (O(n) matching)
    ! DFA is case-sensitive; case-insensitive matching falls through to NFA path
    if (opt%use_dfa .and. .not. ignore_case) then
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
                ! Use fast bitwise character class test
                if (ignore_case) then
                  if (charclass_test_case_insensitive(trans%char_bits, c)) then
                    call next_set%add(trans%target)
                  end if
                else
                  if (charclass_test(trans%char_bits, c)) then
                    call next_set%add(trans%target)
                  end if
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
      ! ASCII uppercase A-Z -> a-z
      lower = char(ic + 32)
    else if (ic >= 192 .and. ic <= 214) then
      ! Latin-1 uppercase À-Ö (192-214) -> à-ö (224-246)
      lower = char(ic + 32)
    else if (ic >= 216 .and. ic <= 222) then
      ! Latin-1 uppercase Ø-Þ (216-222) -> ø-þ (248-254)
      lower = char(ic + 32)
    else if (ic >= 128 .and. ic <= 150) then
      ! UTF-8 continuation byte for uppercase Latin Extended-A (U+00C0-U+00D6)
      ! When preceded by 0xC3, these represent À-Ö, fold to à-ö
      lower = char(ic + 32)
    else if (ic >= 152 .and. ic <= 158) then
      ! UTF-8 continuation byte for uppercase Latin Extended-A (U+00D8-U+00DE)
      ! When preceded by 0xC3, these represent Ø-Þ, fold to ø-þ
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
    pat_len = pattern_len(pattern)  ! Use pattern_len to preserve whitespace patterns

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
            ! Add null terminator to preserve exact length
            if (alt_len < len(alternatives(num_alt))) then
              alternatives(num_alt)(alt_len+1:alt_len+1) = char(0)
            end if
          else
            ! Empty alternative - still valid
            num_alt = num_alt + 1
            alternatives(num_alt) = char(0)
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
            ! Add null terminator to preserve exact length
            if (alt_len < len(alternatives(num_alt))) then
              alternatives(num_alt)(alt_len+1:alt_len+1) = char(0)
            end if
          else
            num_alt = num_alt + 1
            alternatives(num_alt) = char(0)
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
        ! Add null terminator to preserve exact length
        if (alt_len < len(alternatives(num_alt))) then
          alternatives(num_alt)(alt_len+1:alt_len+1) = char(0)
        end if
      else
        alternatives(num_alt) = char(0)
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

  !---------------------------------------------------------------------------
  ! Backtracking Matcher for Backreferences
  !---------------------------------------------------------------------------

  function backtrack_search(nfa, text, text_len, ignore_case) result(res)
    !> Search for pattern with backreferences using backtracking
    !> Tries each starting position until a match is found
    type(nfa_t), intent(in) :: nfa
    character(len=*), intent(in) :: text
    integer, intent(in) :: text_len
    logical, intent(in) :: ignore_case
    type(match_result_t) :: res

    integer :: start_pos
    type(match_result_t) :: try_res

    res%matched = .false.

    do start_pos = 1, text_len + 1
      try_res = backtrack_match(nfa, text, text_len, start_pos, ignore_case)
      if (try_res%matched) then
        res = try_res
        return
      end if
    end do

  end function backtrack_search

  function backtrack_match(nfa, text, text_len, start_pos, ignore_case) result(res)
    !> Try to match NFA with backreferences starting at start_pos
    !> Uses recursive backtracking to track group captures
    type(nfa_t), intent(in) :: nfa
    character(len=*), intent(in) :: text
    integer, intent(in) :: text_len, start_pos
    logical, intent(in) :: ignore_case
    type(match_result_t) :: res

    integer :: group_starts(9), group_ends(9)
    integer :: best_end

    res%matched = .false.
    group_starts = 0
    group_ends = 0
    best_end = start_pos - 1

    ! Try to match from the start state
    if (backtrack_from_state(nfa, nfa%start_state, text, text_len, start_pos, &
                             ignore_case, group_starts, group_ends, best_end)) then
      res%matched = .true.
      res%match_start = start_pos
      res%match_end = best_end
      res%group_starts = group_starts
      res%group_ends = group_ends
    end if

  end function backtrack_match

  recursive function backtrack_from_state(nfa, state, text, text_len, pos, &
                                          ignore_case, group_starts, group_ends, best_end) result(matched)
    !> Recursive backtracking from a given NFA state
    !> Returns true if we can reach an accepting state
    type(nfa_t), intent(in) :: nfa
    integer, intent(in) :: state
    character(len=*), intent(in) :: text
    integer, intent(in) :: text_len, pos
    logical, intent(in) :: ignore_case
    integer, intent(inout) :: group_starts(9), group_ends(9)
    integer, intent(inout) :: best_end
    logical :: matched

    integer :: i, target, old_start, old_end
    integer :: backref_num, ref_start, ref_end, ref_len
    integer :: saved_starts(9), saved_ends(9)
    type(nfa_transition_t) :: trans
    character(len=1) :: c, c_lower, match_lower
    logical :: char_matches

    matched = .false.

    if (state < 1 .or. state > nfa%num_states) return

    ! Record group start if this state starts a group
    if (nfa%states(state)%group_start > 0 .and. nfa%states(state)%group_start <= 9) then
      old_start = group_starts(nfa%states(state)%group_start)
      group_starts(nfa%states(state)%group_start) = pos
    else
      old_start = 0
    end if

    ! Record group end if this state ends a group
    ! This must be done BEFORE processing transitions so backrefs can see the captured text
    if (nfa%states(state)%group_end > 0 .and. nfa%states(state)%group_end <= 9) then
      old_end = group_ends(nfa%states(state)%group_end)
      group_ends(nfa%states(state)%group_end) = pos - 1
    else
      old_end = 0
    end if

    ! Check if this is an accepting state
    if (nfa%states(state)%is_accept) then
      matched = .true.
      if (pos - 1 > best_end) best_end = pos - 1
      ! Continue to find longest match (greedy)
    end if

    ! Try each transition from this state
    do i = 1, nfa%states(state)%num_trans
      trans = nfa%states(state)%trans(i)
      target = trans%target

      select case (trans%trans_type)
        case (TRANS_EPSILON)
          ! Check for backreference (negative anchor_type)
          if (trans%anchor_type < 0) then
            backref_num = -trans%anchor_type
            if (backref_num >= 1 .and. backref_num <= 9) then
              ref_start = group_starts(backref_num)
              ref_end = group_ends(backref_num)

              ! If group hasn't been captured yet, backreference fails
              if (ref_start == 0 .or. ref_end == 0 .or. ref_end < ref_start) cycle

              ref_len = ref_end - ref_start + 1

              ! Check if we have enough text remaining
              if (pos + ref_len - 1 > text_len) cycle

              ! Check if the text matches the captured group
              if (ignore_case) then
                if (.not. strings_equal_icase(text(pos:pos+ref_len-1), &
                                              text(ref_start:ref_end))) cycle
              else
                if (text(pos:pos+ref_len-1) /= text(ref_start:ref_end)) cycle
              end if

              ! Backref matches - continue from target with advanced position
              saved_starts = group_starts
              saved_ends = group_ends
              if (backtrack_from_state(nfa, target, text, text_len, pos + ref_len, &
                                       ignore_case, group_starts, group_ends, best_end)) then
                matched = .true.
              else
                group_starts = saved_starts
                group_ends = saved_ends
              end if
            end if
          else
            ! Regular epsilon transition
            saved_starts = group_starts
            saved_ends = group_ends
            if (backtrack_from_state(nfa, target, text, text_len, pos, &
                                     ignore_case, group_starts, group_ends, best_end)) then
              matched = .true.
            else
              group_starts = saved_starts
              group_ends = saved_ends
            end if
          end if

        case (TRANS_ANCHOR)
          ! Check if anchor matches at this position
          if (anchor_matches_opt(trans%anchor_type, text, pos, text_len)) then
            saved_starts = group_starts
            saved_ends = group_ends
            if (backtrack_from_state(nfa, target, text, text_len, pos, &
                                     ignore_case, group_starts, group_ends, best_end)) then
              matched = .true.
            else
              group_starts = saved_starts
              group_ends = saved_ends
            end if
          end if

        case (TRANS_CHAR)
          ! Character transition - need text available
          if (pos <= text_len) then
            c = text(pos:pos)
            char_matches = .false.

            if (ignore_case) then
              c_lower = to_lower_char(c)
              match_lower = to_lower_char(trans%match_char)
              char_matches = (c_lower == match_lower)
            else
              char_matches = (c == trans%match_char)
            end if

            if (char_matches) then
              saved_starts = group_starts
              saved_ends = group_ends
              if (backtrack_from_state(nfa, target, text, text_len, pos + 1, &
                                       ignore_case, group_starts, group_ends, best_end)) then
                matched = .true.
              else
                group_starts = saved_starts
                group_ends = saved_ends
              end if
            end if
          end if

        case (TRANS_CLASS)
          ! Character class transition
          if (pos <= text_len) then
            c = text(pos:pos)
            if (ignore_case) then
              if (charclass_test_case_insensitive(trans%char_bits, c)) then
                saved_starts = group_starts
                saved_ends = group_ends
                if (backtrack_from_state(nfa, target, text, text_len, pos + 1, &
                                         ignore_case, group_starts, group_ends, best_end)) then
                  matched = .true.
                else
                  group_starts = saved_starts
                  group_ends = saved_ends
                end if
              end if
            else
              if (charclass_test(trans%char_bits, c)) then
                saved_starts = group_starts
                saved_ends = group_ends
                if (backtrack_from_state(nfa, target, text, text_len, pos + 1, &
                                         ignore_case, group_starts, group_ends, best_end)) then
                  matched = .true.
                else
                  group_starts = saved_starts
                  group_ends = saved_ends
                end if
              end if
            end if
          end if

        case (TRANS_ANY)
          ! Dot matches any character except newline
          if (pos <= text_len) then
            if (text(pos:pos) /= char(10)) then
              saved_starts = group_starts
              saved_ends = group_ends
              if (backtrack_from_state(nfa, target, text, text_len, pos + 1, &
                                       ignore_case, group_starts, group_ends, best_end)) then
                matched = .true.
              else
                group_starts = saved_starts
                group_ends = saved_ends
              end if
            end if
          end if

      end select
    end do

    ! Restore group start and end if we didn't match
    if (.not. matched) then
      if (nfa%states(state)%group_start > 0 .and. nfa%states(state)%group_start <= 9) then
        group_starts(nfa%states(state)%group_start) = old_start
      end if
      if (nfa%states(state)%group_end > 0 .and. nfa%states(state)%group_end <= 9) then
        group_ends(nfa%states(state)%group_end) = old_end
      end if
    end if

  end function backtrack_from_state

  function strings_equal_icase(s1, s2) result(equal)
    !> Compare two strings case-insensitively
    character(len=*), intent(in) :: s1, s2
    logical :: equal
    integer :: i, n

    equal = .false.
    n = len(s1)
    if (len(s2) /= n) return

    do i = 1, n
      if (to_lower_char(s1(i:i)) /= to_lower_char(s2(i:i))) return
    end do

    equal = .true.
  end function strings_equal_icase

end module regex_optimizer
