module ferp_simd
  !> SIMD-accelerated scanning functions for FERP
  !> Uses ARM NEON on Apple Silicon, scalar fallback otherwise
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: simd_find_char_ptr, simd_find_char2_ptr, simd_count_char_ptr

  interface
    function c_simd_find_char(buf, len, start, needle) bind(C, name="simd_find_char")
      import :: c_ptr, c_int64_t, c_char
      type(c_ptr), value :: buf
      integer(c_int64_t), value :: len
      integer(c_int64_t), value :: start
      character(c_char), value :: needle
      integer(c_int64_t) :: c_simd_find_char
    end function c_simd_find_char

    function c_simd_find_char2(buf, len, start, c1, c2) bind(C, name="simd_find_char2")
      import :: c_ptr, c_int64_t, c_char
      type(c_ptr), value :: buf
      integer(c_int64_t), value :: len
      integer(c_int64_t), value :: start
      character(c_char), value :: c1
      character(c_char), value :: c2
      integer(c_int64_t) :: c_simd_find_char2
    end function c_simd_find_char2

    function c_simd_count_char(buf, len, needle) bind(C, name="simd_count_char")
      import :: c_ptr, c_int64_t, c_char
      type(c_ptr), value :: buf
      integer(c_int64_t), value :: len
      character(c_char), value :: needle
      integer(c_int64_t) :: c_simd_count_char
    end function c_simd_count_char
  end interface

contains

  function simd_find_char_ptr(buf_ptr, buf_len, start, needle) result(pos)
    !> Find first occurrence of needle starting at start (0-indexed C style)
    !> Returns position (0-indexed) or -1 if not found
    !> For use with mmap'd buffers
    type(c_ptr), intent(in) :: buf_ptr
    integer(c_int64_t), intent(in) :: buf_len
    integer(c_int64_t), intent(in) :: start
    character(len=1), intent(in) :: needle
    integer(c_int64_t) :: pos

    character(kind=c_char) :: c_needle

    ! Convert to C char kind explicitly
    c_needle = char(ichar(needle), kind=c_char)
    pos = c_simd_find_char(buf_ptr, buf_len, start, c_needle)
  end function simd_find_char_ptr

  function simd_find_char2_ptr(buf_ptr, buf_len, start, c1, c2) result(pos)
    !> Find first occurrence of 2-char sequence (0-indexed)
    !> Returns position (0-indexed) or -1 if not found
    type(c_ptr), intent(in) :: buf_ptr
    integer(c_int64_t), intent(in) :: buf_len
    integer(c_int64_t), intent(in) :: start
    character(len=1), intent(in) :: c1, c2
    integer(c_int64_t) :: pos

    character(kind=c_char) :: cc1, cc2

    ! Convert to C char kind explicitly
    cc1 = char(ichar(c1), kind=c_char)
    cc2 = char(ichar(c2), kind=c_char)
    pos = c_simd_find_char2(buf_ptr, buf_len, start, cc1, cc2)
  end function simd_find_char2_ptr

  function simd_count_char_ptr(buf_ptr, buf_len, needle) result(count)
    !> Count occurrences of needle in buffer
    type(c_ptr), intent(in) :: buf_ptr
    integer(c_int64_t), intent(in) :: buf_len
    character(len=1), intent(in) :: needle
    integer(c_int64_t) :: count

    character(kind=c_char) :: c_needle

    ! Convert to C char kind explicitly
    c_needle = char(ichar(needle), kind=c_char)
    count = c_simd_count_char(buf_ptr, buf_len, c_needle)
  end function simd_count_char_ptr

end module ferp_simd
