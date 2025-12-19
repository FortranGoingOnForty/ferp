module regex_charclass
  !> High-performance bitwise character class operations for FERP
  !> Uses 256-bit representation (4 x 64-bit integers) instead of boolean array
  !> Provides O(1) membership testing with minimal memory footprint
  implicit none
  private

  public :: char_class_bits_t
  public :: charclass_from_array, charclass_test, charclass_test_case_insensitive
  public :: charclass_set, charclass_clear, charclass_set_range
  public :: charclass_add_case_variants

  !> Bitwise character class - 256 bits in 4 words
  type :: char_class_bits_t
    integer(8) :: words(4) = 0_8  ! bits 0-63, 64-127, 128-191, 192-255
    logical :: negated = .false.
  end type char_class_bits_t

contains

  pure subroutine charclass_clear(cc)
    !> Clear all bits
    type(char_class_bits_t), intent(inout) :: cc
    cc%words = 0_8
    cc%negated = .false.
  end subroutine charclass_clear

  pure subroutine charclass_set(cc, char_code)
    !> Set a single character bit
    type(char_class_bits_t), intent(inout) :: cc
    integer, intent(in) :: char_code
    integer :: word_idx, bit_idx

    if (char_code < 0 .or. char_code > 255) return
    word_idx = char_code / 64 + 1  ! 1-based index
    bit_idx = mod(char_code, 64)
    cc%words(word_idx) = ior(cc%words(word_idx), ishft(1_8, bit_idx))
  end subroutine charclass_set

  pure subroutine charclass_set_range(cc, start_char, end_char)
    !> Set a range of character bits efficiently
    type(char_class_bits_t), intent(inout) :: cc
    integer, intent(in) :: start_char, end_char
    integer :: i

    do i = start_char, end_char
      if (i >= 0 .and. i <= 255) then
        call charclass_set(cc, i)
      end if
    end do
  end subroutine charclass_set_range

  pure function charclass_test(cc, c) result(res)
    !> Test if character is in class - O(1) bit test
    type(char_class_bits_t), intent(in) :: cc
    character(len=1), intent(in) :: c
    logical :: res

    integer :: char_code, word_idx, bit_idx

    char_code = ichar(c)
    word_idx = char_code / 64 + 1
    bit_idx = mod(char_code, 64)
    res = btest(cc%words(word_idx), bit_idx)

    if (cc%negated) res = .not. res
  end function charclass_test

  pure function charclass_test_case_insensitive(cc, c) result(res)
    !> Test character with case insensitivity - checks both cases in one call
    type(char_class_bits_t), intent(in) :: cc
    character(len=1), intent(in) :: c
    logical :: res

    integer :: char_code, word_idx, bit_idx, other_case

    char_code = ichar(c)
    word_idx = char_code / 64 + 1
    bit_idx = mod(char_code, 64)
    res = btest(cc%words(word_idx), bit_idx)

    ! Quick check for other case (only for a-z and A-Z)
    if (.not. res) then
      if (char_code >= 65 .and. char_code <= 90) then
        ! Uppercase A-Z -> check lowercase a-z
        other_case = char_code + 32
        word_idx = other_case / 64 + 1
        bit_idx = mod(other_case, 64)
        res = btest(cc%words(word_idx), bit_idx)
      else if (char_code >= 97 .and. char_code <= 122) then
        ! Lowercase a-z -> check uppercase A-Z
        other_case = char_code - 32
        word_idx = other_case / 64 + 1
        bit_idx = mod(other_case, 64)
        res = btest(cc%words(word_idx), bit_idx)
      end if
    end if

    if (cc%negated) res = .not. res
  end function charclass_test_case_insensitive

  pure subroutine charclass_from_array(cc, char_class_array, negated)
    !> Convert 256-element boolean array to bitwise format
    type(char_class_bits_t), intent(out) :: cc
    logical, intent(in) :: char_class_array(0:255)
    logical, intent(in) :: negated

    integer :: i, word_idx, bit_idx

    cc%words = 0_8
    cc%negated = negated

    do i = 0, 255
      if (char_class_array(i)) then
        word_idx = i / 64 + 1
        bit_idx = mod(i, 64)
        cc%words(word_idx) = ior(cc%words(word_idx), ishft(1_8, bit_idx))
      end if
    end do
  end subroutine charclass_from_array

  pure subroutine charclass_add_case_variants(cc)
    !> Pre-compute case variants into the character class
    !> After calling this, case-insensitive matching becomes a single test
    type(char_class_bits_t), intent(inout) :: cc

    integer :: i, word_idx, bit_idx, other_case
    integer(8) :: saved_words(4)

    saved_words = cc%words

    ! For each set bit, also set its case variant
    do i = 0, 255
      word_idx = i / 64 + 1
      bit_idx = mod(i, 64)

      if (btest(saved_words(word_idx), bit_idx)) then
        ! Character is in class - add its case variant
        if (i >= 65 .and. i <= 90) then
          ! Uppercase A-Z -> add lowercase a-z
          other_case = i + 32
          call charclass_set(cc, other_case)
        else if (i >= 97 .and. i <= 122) then
          ! Lowercase a-z -> add uppercase A-Z
          other_case = i - 32
          call charclass_set(cc, other_case)
        end if
      end if
    end do
  end subroutine charclass_add_case_variants

end module regex_charclass
