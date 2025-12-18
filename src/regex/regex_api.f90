module regex_api
  !> Public API for the FERP regex engine
  !> Provides high-level interface for pattern compilation and matching
  use regex_types
  use regex_lexer
  use regex_parser
  use regex_nfa
  use regex_engine
  use regex_optimizer
  implicit none
  private

  public :: regex_t
  public :: regex_compile, regex_match, regex_search
  public :: regex_free, regex_error_message
  public :: match_result_t

  !> Compiled regex type
  type :: regex_t
    private
    type(nfa_t) :: nfa
    type(optimized_nfa_t) :: opt_nfa  ! Optimized NFA for faster matching
    type(ast_pool_t) :: ast_pool
    logical :: compiled = .false.
    logical :: is_ere = .false.
    integer :: error_code = 0
    character(len=256) :: error_msg = ''
    integer :: num_groups = 0
  contains
    procedure :: is_compiled => regex_is_compiled
  end type regex_t

contains

  subroutine regex_compile(re, pattern, is_ere, ierr)
    !> Compile a regex pattern
    type(regex_t), intent(out) :: re
    character(len=*), intent(in) :: pattern
    logical, intent(in), optional :: is_ere
    integer, intent(out) :: ierr

    type(token_list_t) :: tokens
    integer :: root_idx
    logical :: extended

    ierr = 0
    re%compiled = .false.
    re%error_code = 0
    re%error_msg = ''

    extended = .false.
    if (present(is_ere)) extended = is_ere
    re%is_ere = extended

    ! Handle empty pattern
    if (len_trim(pattern) == 0) then
      call re%nfa%init()
      re%nfa%start_state = re%nfa%add_state()
      re%nfa%accept_state = re%nfa%add_state()
      re%nfa%states(re%nfa%accept_state)%is_accept = .true.
      ! Add epsilon transition for empty match
      call add_eps(re%nfa, re%nfa%start_state, re%nfa%accept_state)
      ! Optimize NFA for faster matching
      call optimize_nfa(re%opt_nfa, re%nfa)
      re%compiled = .true.
      re%num_groups = 0
      return
    end if

    ! Tokenize
    call tokenize(pattern, tokens, extended, ierr)
    if (ierr /= 0) then
      re%error_code = 1
      re%error_msg = 'Invalid pattern: tokenization failed'
      return
    end if

    ! Parse
    call parse(tokens, re%ast_pool, root_idx, re%num_groups, ierr)
    if (ierr /= 0) then
      re%error_code = 2
      re%error_msg = 'Invalid pattern: parse failed'
      return
    end if

    ! Build NFA
    call build_nfa(re%ast_pool, root_idx, re%nfa, ierr)
    if (ierr /= 0) then
      re%error_code = 3
      re%error_msg = 'Invalid pattern: NFA construction failed'
      return
    end if

    ! Optimize NFA for faster matching
    call optimize_nfa(re%opt_nfa, re%nfa)

    re%compiled = .true.

  contains
    subroutine add_eps(nfa, from, to)
      type(nfa_t), intent(inout) :: nfa
      integer, intent(in) :: from, to
      type(nfa_transition_t) :: trans
      trans%trans_type = TRANS_EPSILON
      trans%target = to
      call nfa%states(from)%add_trans(trans)
    end subroutine
  end subroutine regex_compile

  function regex_match(re, text, ignore_case) result(matched)
    !> Check if pattern matches anywhere in text
    type(regex_t), intent(inout) :: re  ! inout for DFA cache
    character(len=*), intent(in) :: text
    logical, intent(in), optional :: ignore_case
    logical :: matched

    type(match_result_t) :: res
    logical :: icase

    matched = .false.
    if (.not. re%compiled) return

    icase = .false.
    if (present(ignore_case)) icase = ignore_case

    ! Use optimized search with bit vectors and prefix skip
    res = optimized_search(re%opt_nfa, text, icase)
    matched = res%matched

  end function regex_match

  function regex_search(re, text, ignore_case) result(res)
    !> Search for pattern in text, return match result with positions
    type(regex_t), intent(inout) :: re  ! inout for DFA cache
    character(len=*), intent(in) :: text
    logical, intent(in), optional :: ignore_case
    type(match_result_t) :: res

    logical :: icase

    res%matched = .false.
    if (.not. re%compiled) return

    icase = .false.
    if (present(ignore_case)) icase = ignore_case

    ! Use optimized search with bit vectors and prefix skip
    res = optimized_search(re%opt_nfa, text, icase)

  end function regex_search

  subroutine regex_free(re)
    !> Free resources associated with compiled regex
    type(regex_t), intent(inout) :: re

    call re%nfa%cleanup()
    call re%ast_pool%cleanup()
    re%compiled = .false.

  end subroutine regex_free

  function regex_error_message(re) result(msg)
    !> Get error message from failed compilation
    type(regex_t), intent(in) :: re
    character(len=256) :: msg
    msg = re%error_msg
  end function regex_error_message

  function regex_is_compiled(this) result(res)
    class(regex_t), intent(in) :: this
    logical :: res
    res = this%compiled
  end function regex_is_compiled

end module regex_api
