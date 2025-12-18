module ferp_search
  !> Fast string search algorithms for FERP
  !> Implements Boyer-Moore-Horspool for fixed string matching
  use ferp_kinds
  implicit none
  private

  public :: bm_search, bm_search_all
  public :: bm_pattern_t, bm_compile, bm_free

  !> Compiled Boyer-Moore pattern
  type :: bm_pattern_t
    character(len=:), allocatable :: pattern
    integer :: pattern_len = 0
    integer :: skip_table(0:255)  ! Bad character skip table
    logical :: case_insensitive = .false.
  end type bm_pattern_t

contains

  subroutine bm_compile(pat, pattern, case_insensitive)
    !> Compile a pattern for Boyer-Moore search
    type(bm_pattern_t), intent(out) :: pat
    character(len=*), intent(in) :: pattern
    logical, intent(in), optional :: case_insensitive

    integer :: i, c, pat_len
    character(len=:), allocatable :: work_pattern

    pat%case_insensitive = .false.
    if (present(case_insensitive)) pat%case_insensitive = case_insensitive

    pat_len = len(pattern)
    pat%pattern_len = pat_len

    ! Store pattern (lowercase if case-insensitive)
    if (pat%case_insensitive) then
      allocate(character(len=pat_len) :: work_pattern)
      do i = 1, pat_len
        work_pattern(i:i) = to_lower(pattern(i:i))
      end do
      pat%pattern = work_pattern
    else
      pat%pattern = pattern
    end if

    ! Initialize skip table - default skip is pattern length
    pat%skip_table = pat_len

    ! Build bad character table
    ! For each character in pattern (except last), set skip distance
    do i = 1, pat_len - 1
      if (pat%case_insensitive) then
        c = ichar(to_lower(pattern(i:i)))
      else
        c = ichar(pattern(i:i))
      end if
      pat%skip_table(c) = pat_len - i

      ! For case-insensitive, also set the other case
      if (pat%case_insensitive) then
        if (c >= ichar('a') .and. c <= ichar('z')) then
          pat%skip_table(c - 32) = pat_len - i  ! uppercase
        else if (c >= ichar('A') .and. c <= ichar('Z')) then
          pat%skip_table(c + 32) = pat_len - i  ! lowercase
        end if
      end if
    end do

  end subroutine bm_compile

  subroutine bm_free(pat)
    !> Free compiled pattern
    type(bm_pattern_t), intent(inout) :: pat
    if (allocated(pat%pattern)) deallocate(pat%pattern)
    pat%pattern_len = 0
  end subroutine bm_free

  function bm_search(text, pat) result(pos)
    !> Search for pattern in text using Boyer-Moore-Horspool
    !> Returns position of first match (1-based), or 0 if not found
    character(len=*), intent(in) :: text
    type(bm_pattern_t), intent(in) :: pat
    integer :: pos

    integer :: text_len, pat_len, i, j, skip
    character :: tc, pc

    pos = 0
    text_len = len(text)
    pat_len = pat%pattern_len

    if (pat_len == 0) then
      pos = 1  ! Empty pattern matches at start
      return
    end if

    if (text_len < pat_len) return

    i = pat_len  ! Start at position where pattern could first match

    do while (i <= text_len)
      ! Compare pattern right-to-left
      j = pat_len
      do while (j >= 1)
        if (pat%case_insensitive) then
          tc = to_lower(text(i - pat_len + j:i - pat_len + j))
        else
          tc = text(i - pat_len + j:i - pat_len + j)
        end if
        pc = pat%pattern(j:j)

        if (tc /= pc) exit
        j = j - 1
      end do

      if (j == 0) then
        ! Full match found
        pos = i - pat_len + 1
        return
      end if

      ! Skip based on bad character at current position
      if (pat%case_insensitive) then
        skip = pat%skip_table(ichar(to_lower(text(i:i))))
      else
        skip = pat%skip_table(ichar(text(i:i)))
      end if
      i = i + skip
    end do

  end function bm_search

  subroutine bm_search_all(text, pat, positions, count)
    !> Find all occurrences of pattern in text
    character(len=*), intent(in) :: text
    type(bm_pattern_t), intent(in) :: pat
    integer, intent(out) :: positions(:)  ! Array to store positions
    integer, intent(out) :: count         ! Number of matches found

    integer :: text_len, pat_len, i, j, skip, max_matches
    character :: tc, pc

    count = 0
    max_matches = size(positions)
    text_len = len(text)
    pat_len = pat%pattern_len

    if (pat_len == 0) then
      ! Empty pattern matches at every position
      do i = 1, min(text_len + 1, max_matches)
        count = count + 1
        positions(count) = i
      end do
      return
    end if

    if (text_len < pat_len) return

    i = pat_len

    do while (i <= text_len .and. count < max_matches)
      ! Compare pattern right-to-left
      j = pat_len
      do while (j >= 1)
        if (pat%case_insensitive) then
          tc = to_lower(text(i - pat_len + j:i - pat_len + j))
        else
          tc = text(i - pat_len + j:i - pat_len + j)
        end if
        pc = pat%pattern(j:j)

        if (tc /= pc) exit
        j = j - 1
      end do

      if (j == 0) then
        ! Full match found
        count = count + 1
        positions(count) = i - pat_len + 1
        ! Move past this match (non-overlapping)
        i = i + pat_len
      else
        ! Skip based on bad character
        if (pat%case_insensitive) then
          skip = pat%skip_table(ichar(to_lower(text(i:i))))
        else
          skip = pat%skip_table(ichar(text(i:i)))
        end if
        i = i + max(skip, 1)
      end if
    end do

  end subroutine bm_search_all

  pure function to_lower(ch) result(lower)
    !> Convert character to lowercase
    character, intent(in) :: ch
    character :: lower
    integer :: ic

    ic = ichar(ch)
    if (ic >= ichar('A') .and. ic <= ichar('Z')) then
      lower = char(ic + 32)
    else
      lower = ch
    end if
  end function to_lower

end module ferp_search
