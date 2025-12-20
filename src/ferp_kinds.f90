module ferp_kinds
  !> Portable kind parameters for FERP
  !> Ensures consistent behavior across platforms (Apple Silicon, Linux x86_64)
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  implicit none
  private

  public :: i32, i64, dp
  public :: initial_line_len, max_line_len, max_path_len, max_pattern_len
  public :: pattern_len

  !> Integer kinds
  integer, parameter :: i32 = int32
  integer, parameter :: i64 = int64

  !> Real kinds (for future use)
  integer, parameter :: dp = real64

  !> Buffer size constants
  integer, parameter :: initial_line_len = 8192   ! Initial buffer size for dynamic line reading
  integer, parameter :: max_line_len = 8192       ! Legacy - kept for compatibility, will be removed
  integer, parameter :: max_path_len = 4096
  integer, parameter :: max_pattern_len = 4096

contains

  pure function pattern_len(pattern) result(plen)
    !> Get the true length of a pattern, respecting null terminator if present
    !> This allows whitespace-only patterns like "  " to be handled correctly
    !> If no null terminator is found, returns the full string length (not len_trim)
    !> to properly handle whitespace-only patterns passed with explicit length
    character(len=*), intent(in) :: pattern
    integer :: plen

    integer :: i, slen

    slen = len(pattern)
    plen = slen

    ! Look for null terminator
    do i = 1, slen
      if (pattern(i:i) == char(0)) then
        plen = i - 1
        return
      end if
    end do

    ! No null terminator found - return full length
    ! (pattern was passed with explicit length, e.g., pattern(1:2))
    ! Note: Don't use len_trim here as it would break whitespace patterns

  end function pattern_len

end module ferp_kinds
