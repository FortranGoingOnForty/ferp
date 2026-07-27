module pcre_api
  !> PCRE2 library bindings for Perl-compatible regular expressions
  !> Uses iso_c_binding for C interoperability with libpcre2-8
  use, intrinsic :: iso_c_binding
  use ferp_kinds, only: pattern_len
  implicit none
  private

  public :: pcre_t, pcre_match_result_t
  public :: pcre_compile, pcre_match, pcre_search
  public :: pcre_free, pcre_error_message
  public :: pcre_available

  !---------------------------------------------------------------------------
  ! PCRE2 Option Constants
  !---------------------------------------------------------------------------
  integer(c_int), parameter, public :: PCRE2_CASELESS        = int(z'00000008', c_int)
  integer(c_int), parameter, public :: PCRE2_MULTILINE       = int(z'00000400', c_int)
  integer(c_int), parameter, public :: PCRE2_DOTALL          = int(z'00000020', c_int)
  integer(c_int), parameter, public :: PCRE2_EXTENDED        = int(z'00000080', c_int)
  integer(c_int), parameter, public :: PCRE2_UTF             = int(z'00080000', c_int)
  integer(c_int), parameter, public :: PCRE2_UCP             = int(z'00020000', c_int)  ! Unicode properties
  integer(c_int), parameter, public :: PCRE2_NO_UTF_CHECK    = int(z'40000000', c_int)

  !---------------------------------------------------------------------------
  ! C Interface to PCRE2 Library (8-bit)
  !---------------------------------------------------------------------------
  interface
    ! pcre2_compile_8 - Compile a regular expression pattern
    function pcre2_compile_8(pattern, length, options, errorcode, erroroffset, ccontext) &
        bind(C, name="pcre2_compile_8")
      import :: c_ptr, c_char, c_size_t, c_int
      character(kind=c_char), intent(in) :: pattern(*)
      integer(c_size_t), value :: length
      integer(c_int), value :: options
      integer(c_int), intent(out) :: errorcode
      integer(c_size_t), intent(out) :: erroroffset
      type(c_ptr), value :: ccontext
      type(c_ptr) :: pcre2_compile_8
    end function pcre2_compile_8

    ! pcre2_match_data_create_from_pattern_8 - Create match data block
    function pcre2_match_data_create_from_pattern_8(code, gcontext) &
        bind(C, name="pcre2_match_data_create_from_pattern_8")
      import :: c_ptr
      type(c_ptr), value :: code
      type(c_ptr), value :: gcontext
      type(c_ptr) :: pcre2_match_data_create_from_pattern_8
    end function pcre2_match_data_create_from_pattern_8

    ! pcre2_match_8 - Match a compiled pattern against a subject string
    function pcre2_match_8(code, subject, length, startoffset, options, &
                           match_data, mcontext) bind(C, name="pcre2_match_8")
      import :: c_ptr, c_char, c_size_t, c_int
      type(c_ptr), value :: code
      character(kind=c_char), intent(in) :: subject(*)
      integer(c_size_t), value :: length
      integer(c_size_t), value :: startoffset
      integer(c_int), value :: options
      type(c_ptr), value :: match_data
      type(c_ptr), value :: mcontext
      integer(c_int) :: pcre2_match_8
    end function pcre2_match_8

    ! pcre2_get_ovector_pointer_8 - Get pointer to output vector
    function pcre2_get_ovector_pointer_8(match_data) &
        bind(C, name="pcre2_get_ovector_pointer_8")
      import :: c_ptr
      type(c_ptr), value :: match_data
      type(c_ptr) :: pcre2_get_ovector_pointer_8
    end function pcre2_get_ovector_pointer_8

    ! pcre2_get_ovector_count_8 - Get number of pairs in output vector
    function pcre2_get_ovector_count_8(match_data) &
        bind(C, name="pcre2_get_ovector_count_8")
      import :: c_ptr, c_int
      type(c_ptr), value :: match_data
      integer(c_int) :: pcre2_get_ovector_count_8
    end function pcre2_get_ovector_count_8

    ! pcre2_code_free_8 - Free a compiled pattern
    subroutine pcre2_code_free_8(code) bind(C, name="pcre2_code_free_8")
      import :: c_ptr
      type(c_ptr), value :: code
    end subroutine pcre2_code_free_8

    ! pcre2_match_data_free_8 - Free match data block
    subroutine pcre2_match_data_free_8(match_data) &
        bind(C, name="pcre2_match_data_free_8")
      import :: c_ptr
      type(c_ptr), value :: match_data
    end subroutine pcre2_match_data_free_8

    ! pcre2_get_error_message_8 - Get error message for error code
    function pcre2_get_error_message_8(errorcode, buffer, bufflen) &
        bind(C, name="pcre2_get_error_message_8")
      import :: c_ptr, c_char, c_size_t, c_int
      integer(c_int), value :: errorcode
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: bufflen
      integer(c_int) :: pcre2_get_error_message_8
    end function pcre2_get_error_message_8
  end interface

  !---------------------------------------------------------------------------
  ! Compiled PCRE Pattern Type
  !---------------------------------------------------------------------------
  type :: pcre_t
    private
    type(c_ptr) :: code = c_null_ptr        ! Compiled pattern
    type(c_ptr) :: match_data = c_null_ptr  ! Match data block
    logical :: compiled = .false.
    integer :: error_code = 0
    character(len=256) :: error_msg = ''
  contains
    procedure :: is_compiled => pcre_is_compiled
  end type pcre_t

  !---------------------------------------------------------------------------
  ! Match Result Type
  !---------------------------------------------------------------------------
  type :: pcre_match_result_t
    logical :: matched = .false.
    integer :: match_start = 0     ! 1-based start position
    integer :: match_end = 0       ! 1-based end position
    integer :: group_starts(20) = 0
    integer :: group_ends(20) = 0
    integer :: num_groups = 0
  end type pcre_match_result_t

  ! Module state
  logical, save :: pcre_checked = .false.
  logical, save :: pcre_is_available = .false.

contains

  !---------------------------------------------------------------------------
  ! Check if PCRE2 library is available
  !---------------------------------------------------------------------------
  function pcre_available() result(available)
    logical :: available

    if (.not. pcre_checked) then
      ! Try to compile a simple pattern to check availability
      ! If the library isn't linked, this will cause a runtime error
      ! For now, assume available if we got this far (library linked)
      pcre_is_available = .true.
      pcre_checked = .true.
    end if

    available = pcre_is_available
  end function pcre_available

  !---------------------------------------------------------------------------
  ! Compile a PCRE pattern
  !---------------------------------------------------------------------------
  subroutine pcre_compile(re, pattern, ignore_case, ierr)
    type(pcre_t), intent(out) :: re
    character(len=*), intent(in) :: pattern
    logical, intent(in), optional :: ignore_case
    integer, intent(out) :: ierr

    integer :: plen
    character(len=:), allocatable :: c_pattern
    integer(c_int) :: options, errorcode
    integer(c_size_t) :: erroroffset, pcre_pattern_len

    ierr = 0
    re%compiled = .false.
    re%error_code = 0
    re%error_msg = ''

    ! Get actual pattern length (preserving whitespace patterns)
    plen = pattern_len(pattern)

    ! Set options - enable UTF-8 and Unicode properties by default
    options = ior(PCRE2_UTF, PCRE2_UCP)
    if (present(ignore_case)) then
      if (ignore_case) options = ior(options, PCRE2_CASELESS)
    end if

    ! Prepare pattern as C string (use exact length, not trim)
    allocate(character(len=plen+1) :: c_pattern)
    c_pattern = pattern(1:plen) // c_null_char
    pcre_pattern_len = int(plen, c_size_t)

    ! Compile pattern
    re%code = pcre2_compile_8(c_pattern, pcre_pattern_len, options, &
                               errorcode, erroroffset, c_null_ptr)

    if (.not. c_associated(re%code)) then
      re%error_code = int(errorcode)
      call get_pcre_error(errorcode, re%error_msg)
      ierr = 1
      return
    end if

    ! Create match data block
    re%match_data = pcre2_match_data_create_from_pattern_8(re%code, c_null_ptr)
    if (.not. c_associated(re%match_data)) then
      call pcre2_code_free_8(re%code)
      re%code = c_null_ptr
      re%error_msg = 'Failed to create match data'
      ierr = 2
      return
    end if

    re%compiled = .true.

  end subroutine pcre_compile

  !---------------------------------------------------------------------------
  ! Match pattern against text (returns true if matches anywhere)
  !---------------------------------------------------------------------------
  !> Case sensitivity is fixed when the pattern is compiled, so pass
  !> ignore_case to pcre_compile rather than here.
  function pcre_match(re, text) result(matched)
    type(pcre_t), intent(in) :: re
    character(len=*), intent(in) :: text
    logical :: matched

    type(pcre_match_result_t) :: res

    matched = .false.
    if (.not. re%compiled) return

    res = pcre_search(re, text)
    matched = res%matched

  end function pcre_match

  !---------------------------------------------------------------------------
  ! Search for pattern in text, return match result with positions
  !---------------------------------------------------------------------------
  !> Case sensitivity is fixed when the pattern is compiled, so pass
  !> ignore_case to pcre_compile rather than here.
  function pcre_search(re, text, start_offset) result(res)
    type(pcre_t), intent(in) :: re
    character(len=*), intent(in) :: text
    integer, intent(in), optional :: start_offset
    type(pcre_match_result_t) :: res

    character(len=len(text)+1, kind=c_char) :: c_text
    integer(c_int) :: rc, options
    integer(c_size_t) :: text_len, startoffset
    integer(c_size_t) :: ovector_count
    type(c_ptr) :: ovector_ptr
    integer(c_size_t), pointer :: ovector(:)
    integer :: i

    res%matched = .false.
    res%match_start = 0
    res%match_end = 0
    res%num_groups = 0

    if (.not. re%compiled) return

    ! Prepare text as C string (without null terminator for length)
    c_text = text // c_null_char
    text_len = int(len(text), c_size_t)

    ! Set start offset
    startoffset = 0_c_size_t
    if (present(start_offset)) then
      if (start_offset > 0) startoffset = int(start_offset - 1, c_size_t)
    end if

    ! Match options (ignore_case was set at compile time)
    options = 0_c_int

    ! Execute match
    rc = pcre2_match_8(re%code, c_text, text_len, startoffset, options, &
                        re%match_data, c_null_ptr)

    if (rc < 0) then
      ! No match or error
      return
    end if

    res%matched = .true.

    ! Get output vector with match positions
    ovector_ptr = pcre2_get_ovector_pointer_8(re%match_data)
    if (.not. c_associated(ovector_ptr)) return

    ovector_count = int(pcre2_get_ovector_count_8(re%match_data), c_size_t)

    ! Map to Fortran array - ovector has pairs of (start, end) positions
    ! PCRE2 uses byte offsets (0-based), we need 1-based character positions
    call c_f_pointer(ovector_ptr, ovector, [ovector_count * 2])

    ! Overall match is in ovector(1) and ovector(2)
    res%match_start = int(ovector(1)) + 1  ! Convert 0-based to 1-based
    res%match_end = int(ovector(2))        ! End is exclusive in PCRE2, so this is correct

    ! Capture groups start at index 3 (pairs 2+)
    res%num_groups = min(int(rc) - 1, 20)
    do i = 1, res%num_groups
      if ((i * 2 + 1) <= int(ovector_count * 2)) then
        res%group_starts(i) = int(ovector(i * 2 + 1)) + 1
        res%group_ends(i) = int(ovector(i * 2 + 2))
      end if
    end do

  end function pcre_search

  !---------------------------------------------------------------------------
  ! Free PCRE compiled pattern resources
  !---------------------------------------------------------------------------
  subroutine pcre_free(re)
    type(pcre_t), intent(inout) :: re

    if (c_associated(re%match_data)) then
      call pcre2_match_data_free_8(re%match_data)
      re%match_data = c_null_ptr
    end if

    if (c_associated(re%code)) then
      call pcre2_code_free_8(re%code)
      re%code = c_null_ptr
    end if

    re%compiled = .false.

  end subroutine pcre_free

  !---------------------------------------------------------------------------
  ! Get error message for failed compilation
  !---------------------------------------------------------------------------
  function pcre_error_message(re) result(msg)
    type(pcre_t), intent(in) :: re
    character(len=256) :: msg
    msg = re%error_msg
  end function pcre_error_message

  !---------------------------------------------------------------------------
  ! Check if pattern is compiled
  !---------------------------------------------------------------------------
  function pcre_is_compiled(this) result(res)
    class(pcre_t), intent(in) :: this
    logical :: res
    res = this%compiled
  end function pcre_is_compiled

  !---------------------------------------------------------------------------
  ! Get PCRE error message from error code
  !---------------------------------------------------------------------------
  subroutine get_pcre_error(errorcode, msg)
    integer(c_int), intent(in) :: errorcode
    character(len=*), intent(out) :: msg

    character(len=256, kind=c_char) :: c_buffer
    integer(c_int) :: ret

    msg = ''
    ret = pcre2_get_error_message_8(errorcode, c_buffer, 256_c_size_t)

    if (ret > 0) then
      msg = c_to_fortran_string(c_buffer)
    else
      write(msg, '(A,I0)') 'PCRE error code: ', errorcode
    end if

  end subroutine get_pcre_error

  !---------------------------------------------------------------------------
  ! Convert C string to Fortran string
  !---------------------------------------------------------------------------
  function c_to_fortran_string(c_str) result(f_str)
    character(len=*, kind=c_char), intent(in) :: c_str
    character(len=len(c_str)) :: f_str
    integer :: i

    f_str = ''
    do i = 1, len(c_str)
      if (c_str(i:i) == c_null_char) exit
      f_str(i:i) = c_str(i:i)
    end do
  end function c_to_fortran_string

end module pcre_api
