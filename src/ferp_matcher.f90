module ferp_matcher
  !> Pattern matching orchestration for FERP
  !> Thread-safe: no SAVE variables, all buffers are dynamically allocated
  use ferp_kinds, only: i64, max_pattern_len, pattern_len
  use ferp_options
  use ferp_io
  use ferp_output
  use ferp_search
  use regex_api
  use pcre_api
  implicit none
  private

  public :: match_line, match_fixed_string
  public :: process_source, to_lower
  public :: compiled_patterns_t, compile_patterns, free_patterns
  public :: find_matches

  !> Holds compiled regex patterns for reuse
  type :: compiled_patterns_t
    type(regex_t), allocatable :: regexes(:)
    type(pcre_t), allocatable :: pcres(:)       ! PCRE compiled patterns
    type(bm_pattern_t), allocatable :: bm_pats(:)  ! Boyer-Moore patterns for fixed strings
    integer :: count = 0
    logical :: compiled = .false.
    logical :: is_pcre = .false.                ! True if using PCRE
    logical :: is_fixed = .false.               ! True if using Boyer-Moore fixed strings
  end type compiled_patterns_t

  !> Context buffer entry - holds a line with its metadata
  type :: context_entry_t
    character(len=:), allocatable :: text
    integer :: line_num = 0
    integer(i64) :: byte_off = 0
  end type context_entry_t

contains

  subroutine compile_patterns(patterns, opts, compiled, ierr)
    !> Compile all patterns once for reuse
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(in) :: opts
    type(compiled_patterns_t), intent(out) :: compiled
    integer, intent(out) :: ierr

    integer :: i, n, plen
    logical :: is_ere
    character(len=max_pattern_len) :: pattern

    ierr = 0
    n = size(patterns)
    compiled%count = n
    compiled%is_pcre = (opts%pattern_type == PATTERN_PERL)
    compiled%is_fixed = (opts%pattern_type == PATTERN_FIXED)

    ! Use Boyer-Moore for fixed string patterns
    if (compiled%is_fixed) then
      allocate(compiled%bm_pats(n))

      do i = 1, n
        plen = pattern_len(patterns(i))
        ! For case-insensitive, convert pattern to lowercase
        if (opts%ignore_case) then
          call bm_compile(compiled%bm_pats(i), patterns(i)(1:plen), .true.)
        else
          call bm_compile(compiled%bm_pats(i), patterns(i)(1:plen), .false.)
        end if
      end do

      compiled%compiled = .true.
      return
    end if

    ! Use PCRE for Perl-compatible patterns
    if (compiled%is_pcre) then
      allocate(compiled%pcres(n))

      do i = 1, n
        plen = pattern_len(patterns(i))

        ! Apply -w (word) transformation using PCRE word boundaries
        if (opts%word_regexp) then
          pattern = '\b' // patterns(i)(1:plen) // '\b'
          plen = plen + 4  ! \b and \b
        ! Apply -x (line) transformation
        else if (opts%line_regexp) then
          pattern = '^' // patterns(i)(1:plen) // '$'
          plen = plen + 2  ! ^ and $
        else
          pattern = patterns(i)(1:plen)
        end if

        call pcre_compile(compiled%pcres(i), pattern(1:plen), opts%ignore_case, ierr)
        if (ierr /= 0) then
          compiled%compiled = .false.
          return
        end if
      end do

      compiled%compiled = .true.
      return
    end if

    ! Use Thompson NFA for BRE/ERE
    allocate(compiled%regexes(n))

    is_ere = (opts%pattern_type == PATTERN_ERE)

    do i = 1, n
      plen = pattern_len(patterns(i))

      ! Apply -w (word) transformation
      if (opts%word_regexp .and. opts%pattern_type /= PATTERN_FIXED) then
        pattern = '\<' // patterns(i)(1:plen) // '\>'
        plen = plen + 4  ! \< and \>
      ! Apply -x (line) transformation
      else if (opts%line_regexp .and. opts%pattern_type /= PATTERN_FIXED) then
        pattern = '^' // patterns(i)(1:plen) // '$'
        plen = plen + 2  ! ^ and $
      else
        pattern = patterns(i)(1:plen)
      end if

      ! Compile with exact pattern length
      call regex_compile(compiled%regexes(i), pattern(1:plen), is_ere, ierr)
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

    if (allocated(compiled%pcres)) then
      do i = 1, compiled%count
        call pcre_free(compiled%pcres(i))
      end do
      deallocate(compiled%pcres)
    end if

    if (allocated(compiled%bm_pats)) then
      do i = 1, compiled%count
        call bm_free(compiled%bm_pats(i))
      end do
      deallocate(compiled%bm_pats)
    end if

    compiled%count = 0
    compiled%compiled = .false.
    compiled%is_pcre = .false.
    compiled%is_fixed = .false.

  end subroutine free_patterns

  function match_line(line, patterns, opts, compiled) result(matches)
    !> Check if line matches any pattern according to options
    character(len=*), intent(in) :: line
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(in) :: opts
    type(compiled_patterns_t), intent(inout), optional :: compiled  ! inout for DFA cache
    logical :: matches

    integer :: i
    character(len=:), allocatable :: search_line
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
          ! Use Boyer-Moore if compiled patterns available
          if (present(compiled) .and. compiled%compiled .and. compiled%is_fixed) then
            matches = match_fixed_bm(line, compiled%bm_pats(i), opts)
          else
            ! Fallback to simple index search
            if (opts%ignore_case) then
              search_pattern = to_lower(patterns(i))
            else
              search_pattern = patterns(i)
            end if
            matches = match_fixed_string(search_line, search_pattern, opts)
          end if

        case (PATTERN_BRE, PATTERN_ERE)
          if (present(compiled) .and. compiled%compiled) then
            matches = regex_match(compiled%regexes(i), line, opts%ignore_case)
          else
            ! Fallback if no compiled patterns (shouldn't happen in normal use)
            matches = match_regex_inline(line, patterns(i), opts)
          end if

        case (PATTERN_PERL)
          ! Use PCRE2 for Perl-compatible regular expressions
          if (present(compiled) .and. compiled%compiled .and. compiled%is_pcre) then
            ! ignore_case is handled at compile time for PCRE
            matches = pcre_match(compiled%pcres(i), line)
          else
            ! Fallback if PCRE not available (shouldn't happen normally)
            if (present(compiled) .and. compiled%compiled) then
              matches = regex_match(compiled%regexes(i), line, opts%ignore_case)
            else
              matches = match_fixed_string(search_line, patterns(i), opts)
            end if
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
    pat_len = pattern_len(pattern)  ! Use pattern_len to preserve whitespace patterns

    if (pat_len == 0) then
      ! Empty pattern matches everything
      matches = .true.
      return
    end if

    ! Find pattern in line (use exact length, not trim)
    pos = index(line(1:line_len), pattern(1:pat_len))

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

  function match_fixed_bm(line, bm_pat, opts) result(matches)
    !> Fixed string matching using Boyer-Moore algorithm
    character(len=*), intent(in) :: line
    type(bm_pattern_t), intent(in) :: bm_pat
    type(grep_options), intent(in) :: opts
    logical :: matches

    integer :: pos
    integer :: line_len, pat_len

    matches = .false.
    line_len = len_trim(line)
    pat_len = bm_pat%pattern_len

    if (pat_len == 0) then
      ! Empty pattern matches everything
      matches = .true.
      return
    end if

    ! Find pattern using Boyer-Moore
    pos = bm_search(line(1:line_len), bm_pat)

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

  end function match_fixed_bm

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

  subroutine match_lines_batch(src, batch, patterns, opts, compiled, match_results)
    !> Match a batch of lines against patterns
    !> Returns array of match results (true/false for each line)
    type(input_source), intent(in) :: src
    type(line_batch_t), intent(in) :: batch
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(in) :: opts
    type(compiled_patterns_t), intent(inout) :: compiled
    logical, intent(out) :: match_results(BATCH_SIZE)

    integer :: i
    character(len=:), allocatable :: line

    match_results = .false.

    do i = 1, batch%count
      ! Extract line text from mmap
      line = src%get_line_text(batch%lines(i))
      ! Match this line
      match_results(i) = match_line(line, patterns, opts, compiled)
    end do

  end subroutine match_lines_batch

  function can_use_batch_mode(src, opts) result(can_batch)
    !> Check if we can use optimized batch processing
    !> Batch mode works for simple cases without context lines or special modes
    type(input_source), intent(in) :: src
    type(grep_options), intent(in) :: opts
    logical :: can_batch

    can_batch = .false.

    ! Must be mmap source (has the file in memory)
    if (src%source_type /= SOURCE_MMAP) return

    ! Can't use batch with context lines
    if (opts%before_context > 0 .or. opts%after_context > 0) return

    ! Can't use batch with invert match (need careful line tracking)
    if (opts%invert_match) return

    ! Can't use batch with only-matching mode
    if (opts%only_matching) return

    ! Can't use batch with files-without-match
    if (opts%files_without_match) return

    ! Can't use batch with null-data mode
    if (opts%null_data) return

    can_batch = .true.

  end function can_use_batch_mode

  function process_source_batch(src, patterns, opts, compiled) result(found_match)
    !> Process a source using batch mode for improved performance
    !> This is a fast path for simple search modes
    type(input_source), intent(inout) :: src
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(inout) :: opts
    type(compiled_patterns_t), intent(inout) :: compiled
    logical :: found_match

    type(line_batch_t) :: batch
    logical :: match_results(BATCH_SIZE)
    character(len=:), allocatable :: line
    integer :: i, match_count
    logical :: binary_matched

    ! For color mode
    integer, parameter :: MAX_MATCHES_PER_LINE = 100
    integer :: match_starts(MAX_MATCHES_PER_LINE)
    integer :: match_ends(MAX_MATCHES_PER_LINE)
    integer :: num_matches

    found_match = .false.
    match_count = 0
    binary_matched = .false.

    ! Process batches until EOF
    do while (src%read_lines_batch(batch))
      ! Match all lines in batch
      call match_lines_batch(src, batch, patterns, opts, compiled, match_results)

      ! Process matches
      do i = 1, batch%count
        if (match_results(i)) then
          found_match = .true.
          match_count = match_count + 1

          ! Handle binary files
          if (src%is_binary .and. .not. opts%text_mode) then
            if (.not. binary_matched) then
              call print_binary_match(src%filename, opts)
              binary_matched = .true.
            end if
            return
          end if

          ! Handle output modes
          if (opts%quiet) then
            return
          else if (opts%files_with_matches) then
            call print_filename(src%filename, opts)
            return
          else if (.not. opts%count_only) then
            ! Get line text and print it
            line = src%get_line_text(batch%lines(i))
            if (opts%color_mode == COLOR_ALWAYS) then
              call find_matches(line, patterns, opts, compiled, match_starts, match_ends, num_matches)
              call print_match_colored(line, src%filename, batch%lines(i)%line_num, &
                                       batch%lines(i)%byte_off, opts, match_starts, match_ends, num_matches)
            else
              call print_match(line, src%filename, batch%lines(i)%line_num, &
                              batch%lines(i)%byte_off, opts)
            end if
          end if

          ! Check max count
          if (opts%max_count > 0 .and. match_count >= opts%max_count) then
            if (opts%count_only) then
              call print_count(match_count, src%filename, opts)
            end if
            return
          end if
        end if
      end do
    end do

    ! Handle count mode
    if (opts%count_only) then
      call print_count(match_count, src%filename, opts)
    end if

  end function process_source_batch

  subroutine find_matches(line, patterns, opts, compiled, match_starts, match_ends, num_matches)
    !> Find all matches in a line, returning their positions
    !> For -o mode, this finds all non-overlapping matches
    character(len=*), intent(in) :: line
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(in) :: opts
    type(compiled_patterns_t), intent(inout), optional :: compiled  ! inout for DFA cache
    integer, intent(out) :: match_starts(:), match_ends(:)
    integer, intent(out) :: num_matches

    integer :: i, pos, line_len, pat_len
    type(match_result_t) :: res
    type(pcre_match_result_t) :: pcre_res
    character(len=:), allocatable :: search_line
    character(len=max_pattern_len) :: search_pattern

    num_matches = 0
    line_len = len_trim(line)
    if (line_len == 0) return

    ! For fixed string mode
    if (opts%pattern_type == PATTERN_FIXED) then
      if (opts%ignore_case) then
        search_line = to_lower(line)
      else
        search_line = line
      end if

      do i = 1, size(patterns)
        ! Get pattern length (preserving whitespace patterns)
        pat_len = pattern_len(patterns(i))
        if (pat_len == 0) cycle

        if (opts%ignore_case) then
          search_pattern = to_lower(patterns(i)(1:pat_len))
        else
          search_pattern = patterns(i)(1:pat_len)
        end if

        pos = 1
        do while (pos <= line_len)
          pos = index(search_line(pos:line_len), search_pattern(1:pat_len))
          if (pos == 0) exit

          ! Adjust for substring offset
          pos = pos + (pos - 1)
          if (pos > line_len) exit

          ! Record match
          if (num_matches < size(match_starts)) then
            num_matches = num_matches + 1
            match_starts(num_matches) = pos
            match_ends(num_matches) = pos + pat_len - 1
          end if

          ! Move past this match
          pos = pos + pat_len
        end do
      end do
      return
    end if

    ! For PCRE mode
    if (opts%pattern_type == PATTERN_PERL) then
      if (.not. present(compiled) .or. .not. compiled%compiled .or. .not. compiled%is_pcre) return

      do i = 1, size(patterns)
        pos = 1
        do while (pos <= line_len)
          pcre_res = pcre_search(compiled%pcres(i), line, start_offset=pos)
          if (.not. pcre_res%matched) exit

          ! Record match
          if (num_matches < size(match_starts)) then
            num_matches = num_matches + 1
            match_starts(num_matches) = pcre_res%match_start
            match_ends(num_matches) = pcre_res%match_end
          end if

          ! Move past this match (at least 1 char to avoid infinite loop)
          if (pcre_res%match_end >= pcre_res%match_start) then
            pos = pcre_res%match_end + 1
          else
            pos = pos + 1  ! Empty match, advance by 1
          end if
        end do
      end do
      return
    end if

    ! For BRE/ERE regex mode - try each pattern
    do i = 1, size(patterns)
      if (.not. present(compiled) .or. .not. compiled%compiled) cycle

      pos = 1
      do while (pos <= line_len)
        res = regex_search(compiled%regexes(i), line(pos:), opts%ignore_case)
        if (.not. res%matched) exit

        ! Record match (adjust for substring offset)
        if (num_matches < size(match_starts)) then
          num_matches = num_matches + 1
          match_starts(num_matches) = pos + res%match_start - 1
          match_ends(num_matches) = pos + res%match_end - 1
        end if

        ! Move past this match (at least 1 char to avoid infinite loop)
        if (res%match_end >= res%match_start) then
          pos = pos + res%match_end
        else
          pos = pos + 1  ! Empty match, advance by 1
        end if
      end do
    end do

  end subroutine find_matches

  function process_source(src, patterns, opts, compiled) result(found_match)
    !> Process a single input source, return true if any matches found
    !> Thread-safe: all buffers are locally allocated (no SAVE variables)
    type(input_source), intent(inout) :: src
    character(len=max_pattern_len), intent(in) :: patterns(:)
    type(grep_options), intent(inout) :: opts
    type(compiled_patterns_t), intent(inout), optional :: compiled  ! inout for DFA cache
    logical :: found_match

    character(len=:), allocatable :: line
    integer :: line_num
    integer(i64) :: byte_off
    integer :: match_count
    logical :: line_matched
    logical :: binary_matched

    ! For -o mode
    integer, parameter :: MAX_MATCHES_PER_LINE = 100
    integer :: match_starts(MAX_MATCHES_PER_LINE)
    integer :: match_ends(MAX_MATCHES_PER_LINE)
    integer :: num_matches, j

    ! For context lines - dynamically allocated (thread-safe)
    type(context_entry_t), allocatable :: before_buffer(:)
    integer :: buf_start, buf_count, buf_idx
    integer :: after_remaining  ! Lines of after-context still to print
    integer :: last_printed_line  ! Last line number we printed
    logical :: need_separator  ! Need to print -- before next output
    logical :: use_context
    integer :: k

    ! Try optimized batch mode for simple cases
    if (present(compiled) .and. compiled%compiled .and. can_use_batch_mode(src, opts)) then
      found_match = process_source_batch(src, patterns, opts, compiled)
      return
    end if

    found_match = .false.
    match_count = 0
    binary_matched = .false.
    buf_start = 1
    buf_count = 0
    after_remaining = 0
    last_printed_line = 0
    need_separator = .false.
    use_context = (opts%before_context > 0 .or. opts%after_context > 0)

    ! Allocate context buffer if needed
    if (use_context .and. opts%before_context > 0) then
      allocate(before_buffer(opts%before_context))
    end if

    ! Process lines (line-by-line fallback)
    do
      ! Read next line with dynamic allocation (no length limit)
      if (opts%null_data) then
        if (.not. src%read_line_null_dynamic(line, line_num, byte_off)) exit
      else
        if (.not. src%read_line_dynamic(line, line_num, byte_off)) exit
      end if

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
          exit
        end if

        ! Handle different output modes
        if (opts%quiet) then
          if (allocated(before_buffer)) deallocate(before_buffer)
          return
        else if (opts%files_with_matches) then
          call print_filename(src%filename, opts)
          if (allocated(before_buffer)) deallocate(before_buffer)
          return
        else if (opts%only_matching) then
          if (present(compiled)) then
            call find_matches(line, patterns, opts, compiled, match_starts, match_ends, num_matches)
          else
            call find_matches(line, patterns, opts, match_starts=match_starts, &
                             match_ends=match_ends, num_matches=num_matches)
          end if
          do j = 1, num_matches
            call print_only_match(line, match_starts(j), match_ends(j), &
                                  src%filename, line_num, byte_off, opts)
          end do
        else if (.not. opts%count_only .and. .not. opts%files_without_match) then
          ! Context and normal mode
          if (use_context) then
            ! Determine first line we'll print (for separator check)
            if (buf_count > 0 .and. opts%before_context > 0) then
              buf_idx = mod(buf_start - 1, buf_count) + 1
              k = before_buffer(buf_idx)%line_num  ! First buffered line number
            else
              k = line_num
            end if

            ! Print separator if there's a gap between context groups
            if (need_separator .and. last_printed_line > 0 .and. &
                k > last_printed_line + 1) then
              call print_separator(opts)
            end if
            need_separator = .true.

            ! Print before-context from buffer (in correct order)
            if (buf_count > 0 .and. opts%before_context > 0) then
              do k = 0, buf_count - 1
                ! Read from buffer in order: oldest to newest
                buf_idx = mod(buf_start + k - 1, buf_count) + 1
                if (before_buffer(buf_idx)%line_num > last_printed_line) then
                  call print_context_line(before_buffer(buf_idx)%text, src%filename, &
                       before_buffer(buf_idx)%line_num, before_buffer(buf_idx)%byte_off, opts)
                  last_printed_line = before_buffer(buf_idx)%line_num
                end if
              end do
              ! Clear buffer after printing
              buf_count = 0
              buf_start = 1
            end if
          end if

          ! Print the matching line
          if (line_num > last_printed_line) then
            ! Get match positions for color highlighting
            if (opts%color_mode == COLOR_ALWAYS) then
              if (present(compiled)) then
                call find_matches(line, patterns, opts, compiled, match_starts, match_ends, num_matches)
              else
                call find_matches(line, patterns, opts, match_starts=match_starts, &
                                 match_ends=match_ends, num_matches=num_matches)
              end if
              call print_match_colored(line, src%filename, line_num, byte_off, opts, &
                                       match_starts, match_ends, num_matches)
            else
              call print_match(line, src%filename, line_num, byte_off, opts)
            end if
            last_printed_line = line_num
          end if

          ! Reset after-context counter
          after_remaining = opts%after_context
        end if

        ! Check max count
        if (opts%max_count > 0 .and. match_count >= opts%max_count) then
          exit
        end if

      else
        ! Non-matching line
        if (use_context .and. .not. opts%count_only .and. .not. opts%quiet .and. &
            .not. opts%files_with_matches .and. .not. opts%files_without_match .and. &
            .not. opts%only_matching) then

          ! Print as after-context if needed
          if (after_remaining > 0 .and. line_num > last_printed_line) then
            call print_context_line(line, src%filename, line_num, byte_off, opts)
            last_printed_line = line_num
            after_remaining = after_remaining - 1
          else if (opts%before_context > 0) then
            ! Store in before-context buffer (circular buffer)
            if (buf_count < opts%before_context) then
              buf_count = buf_count + 1
              buf_idx = buf_count
            else
              ! Buffer is full, overwrite oldest entry
              buf_idx = buf_start
              buf_start = mod(buf_start, opts%before_context) + 1
            end if
            before_buffer(buf_idx)%text = line
            before_buffer(buf_idx)%line_num = line_num
            before_buffer(buf_idx)%byte_off = byte_off
          end if
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

    ! Clean up
    if (allocated(before_buffer)) deallocate(before_buffer)

  end function process_source

end module ferp_matcher
