module ferp_matcher
  !> Pattern matching orchestration for FERP
  use ferp_kinds
  use ferp_options
  use ferp_io
  use ferp_output
  use regex_api
  implicit none
  private

  public :: match_line, match_fixed_string
  public :: process_source, to_lower
  public :: compiled_patterns_t, compile_patterns, free_patterns

  !> Holds compiled regex patterns for reuse
  type :: compiled_patterns_t
    type(regex_t), allocatable :: regexes(:)
    integer :: count = 0
    logical :: compiled = .false.
  end type compiled_patterns_t

contains

  subroutine compile_patterns(patterns, opts, compiled, ierr)
    !> Compile all patterns once for reuse
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(in) :: opts
    type(compiled_patterns_t), intent(out) :: compiled
    integer, intent(out) :: ierr

    integer :: i, n
    logical :: is_ere
    character(len=max_pattern_len) :: pattern

    ierr = 0
    n = size(patterns)
    compiled%count = n
    allocate(compiled%regexes(n))

    is_ere = (opts%pattern_type == PATTERN_ERE)

    do i = 1, n
      pattern = patterns(i)

      ! Apply -w (word) transformation
      if (opts%word_regexp .and. opts%pattern_type /= PATTERN_FIXED) then
        pattern = '\<' // trim(pattern) // '\>'
      end if

      ! Apply -x (line) transformation
      if (opts%line_regexp .and. opts%pattern_type /= PATTERN_FIXED) then
        pattern = '^' // trim(pattern) // '$'
      end if

      call regex_compile(compiled%regexes(i), trim(pattern), is_ere, ierr)
      if (ierr /= 0) then
        compiled%compiled = .false.
        return
      end if
    end do

    compiled%compiled = .true.

  end subroutine compile_patterns

  subroutine free_patterns(compiled)
    !> Free compiled patterns
    type(compiled_patterns_t), intent(inout) :: compiled
    integer :: i

    if (allocated(compiled%regexes)) then
      do i = 1, compiled%count
        call regex_free(compiled%regexes(i))
      end do
      deallocate(compiled%regexes)
    end if
    compiled%count = 0
    compiled%compiled = .false.

  end subroutine free_patterns

  function match_line(line, patterns, opts, compiled) result(matches)
    !> Check if line matches any pattern according to options
    character(len=*), intent(in) :: line
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(in) :: opts
    type(compiled_patterns_t), intent(in), optional :: compiled
    logical :: matches

    integer :: i
    character(len=max_line_len) :: search_line
    character(len=max_pattern_len) :: search_pattern

    matches = .false.

    ! Prepare line for searching (for fixed string mode)
    if (opts%ignore_case .and. opts%pattern_type == PATTERN_FIXED) then
      search_line = to_lower(line)
    else
      search_line = line
    end if

    ! Try each pattern
    do i = 1, size(patterns)
      ! Match based on pattern type
      select case (opts%pattern_type)
        case (PATTERN_FIXED)
          if (opts%ignore_case) then
            search_pattern = to_lower(patterns(i))
          else
            search_pattern = patterns(i)
          end if
          matches = match_fixed_string(search_line, search_pattern, opts)

        case (PATTERN_BRE, PATTERN_ERE)
          if (present(compiled) .and. compiled%compiled) then
            matches = regex_match(compiled%regexes(i), line, opts%ignore_case)
          else
            ! Fallback if no compiled patterns (shouldn't happen in normal use)
            matches = match_regex_inline(line, patterns(i), opts)
          end if

        case (PATTERN_PERL)
          ! TODO: Implement Perl regex (stretch goal)
          ! Fall back to BRE for now
          if (present(compiled) .and. compiled%compiled) then
            matches = regex_match(compiled%regexes(i), line, opts%ignore_case)
          else
            matches = match_fixed_string(search_line, patterns(i), opts)
          end if
      end select

      if (matches) exit
    end do

    ! Apply invert match
    if (opts%invert_match) then
      matches = .not. matches
    end if

  end function match_line

  function match_regex_inline(line, pattern, opts) result(matches)
    !> Compile and match regex inline (less efficient, for fallback)
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: pattern
    type(grep_options), intent(in) :: opts
    logical :: matches

    type(regex_t) :: re
    integer :: ierr
    logical :: is_ere
    character(len=max_pattern_len) :: pat

    matches = .false.
    is_ere = (opts%pattern_type == PATTERN_ERE)

    pat = pattern
    if (opts%word_regexp) then
      pat = '\<' // trim(pattern) // '\>'
    end if
    if (opts%line_regexp) then
      pat = '^' // trim(pat) // '$'
    end if

    call regex_compile(re, trim(pat), is_ere, ierr)
    if (ierr /= 0) return

    matches = regex_match(re, line, opts%ignore_case)
    call regex_free(re)

  end function match_regex_inline

  function match_fixed_string(line, pattern, opts) result(matches)
    !> Fixed string matching (for -F mode)
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: pattern
    type(grep_options), intent(in) :: opts
    logical :: matches

    integer :: pos
    integer :: line_len, pat_len

    matches = .false.
    line_len = len_trim(line)
    pat_len = len_trim(pattern)

    if (pat_len == 0) then
      ! Empty pattern matches everything
      matches = .true.
      return
    end if

    ! Find pattern in line
    pos = index(line(1:line_len), trim(pattern))

    if (pos == 0) return

    ! Check word boundary if -w
    if (opts%word_regexp) then
      if (.not. is_word_match(line, pos, pat_len)) return
    end if

    ! Check line match if -x
    if (opts%line_regexp) then
      if (pos /= 1 .or. pat_len /= line_len) return
    end if

    matches = .true.

  end function match_fixed_string

  function is_word_match(line, pos, pat_len) result(is_word)
    !> Check if match at pos is a whole word
    character(len=*), intent(in) :: line
    integer, intent(in) :: pos, pat_len
    logical :: is_word

    integer :: line_len
    logical :: start_ok, end_ok

    is_word = .false.
    line_len = len_trim(line)

    ! Check character before match
    if (pos == 1) then
      start_ok = .true.
    else
      start_ok = .not. is_word_char(line(pos-1:pos-1))
    end if

    ! Check character after match
    if (pos + pat_len - 1 >= line_len) then
      end_ok = .true.
    else
      end_ok = .not. is_word_char(line(pos+pat_len:pos+pat_len))
    end if

    is_word = start_ok .and. end_ok

  end function is_word_match

  pure function is_word_char(c) result(is_word)
    !> Check if character is a word character (alphanumeric or underscore)
    character(len=1), intent(in) :: c
    logical :: is_word

    integer :: ic

    ic = ichar(c)
    is_word = (ic >= ichar('a') .and. ic <= ichar('z')) .or. &
              (ic >= ichar('A') .and. ic <= ichar('Z')) .or. &
              (ic >= ichar('0') .and. ic <= ichar('9')) .or. &
              (c == '_')

  end function is_word_char

  pure function to_lower(str) result(lower)
    !> Convert string to lowercase
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower

    integer :: i, ic

    do i = 1, len(str)
      ic = ichar(str(i:i))
      if (ic >= ichar('A') .and. ic <= ichar('Z')) then
        lower(i:i) = char(ic + 32)
      else
        lower(i:i) = str(i:i)
      end if
    end do

  end function to_lower

  function process_source(src, patterns, opts, compiled) result(found_match)
    !> Process a single input source, return true if any matches found
    type(input_source), intent(inout) :: src
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(inout) :: opts
    type(compiled_patterns_t), intent(in), optional :: compiled
    logical :: found_match

    character(len=max_line_len) :: line
    integer :: line_num
    integer(i64) :: byte_off
    integer :: match_count
    logical :: line_matched
    logical :: binary_matched

    found_match = .false.
    match_count = 0
    binary_matched = .false.

    ! Check for binary file
    if (.not. opts%text_mode .and. src%source_type == SOURCE_FILE) then
      call src%check_binary()
      if (src%is_binary .and. opts%ignore_binary) then
        return
      end if
    end if

    ! Process lines
    do while (src%read_line(line, line_num, byte_off))
      if (present(compiled)) then
        line_matched = match_line(line, patterns, opts, compiled)
      else
        line_matched = match_line(line, patterns, opts)
      end if

      if (line_matched) then
        found_match = .true.
        match_count = match_count + 1

        ! Handle binary files
        if (src%is_binary .and. .not. opts%text_mode) then
          if (.not. binary_matched) then
            call print_binary_match(src%filename, opts)
            binary_matched = .true.
          end if
          ! Stop processing binary file after first match
          exit
        end if

        ! Handle different output modes
        if (opts%quiet) then
          ! Quiet mode: exit immediately on match
          return
        else if (opts%files_with_matches) then
          ! -l: print filename and stop processing this file
          call print_filename(src%filename, opts)
          return
        else if (.not. opts%count_only .and. .not. opts%files_without_match) then
          ! Normal mode: print matching line
          call print_match(line, src%filename, line_num, byte_off, opts)
        end if

        ! Check max count
        if (opts%max_count > 0 .and. match_count >= opts%max_count) then
          exit
        end if
      end if
    end do

    ! Handle -c (count) mode
    if (opts%count_only) then
      call print_count(match_count, src%filename, opts)
    end if

    ! Handle -L (files without match) mode
    if (opts%files_without_match .and. .not. found_match) then
      call print_filename(src%filename, opts)
    end if

  end function process_source

end module ferp_matcher
