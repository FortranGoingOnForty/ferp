module ferp_kinds
  !> Portable kind parameters for FERP
  !> Ensures consistent behavior across platforms (Apple Silicon, Linux x86_64)
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  implicit none
  private

  public :: i32, i64, dp
  public :: max_line_len, max_path_len, max_pattern_len

  !> Integer kinds
  integer, parameter :: i32 = int32
  integer, parameter :: i64 = int64

  !> Real kinds (for future use)
  integer, parameter :: dp = real64

  !> Buffer size constants
  integer, parameter :: max_line_len = 8192
  integer, parameter :: max_path_len = 4096
  integer, parameter :: max_pattern_len = 4096

end module ferp_kinds
