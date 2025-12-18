module ferp_output
  !> Output formatting for FERP
  !> All output functions are thread-safe via OMP critical sections
  use ferp_kinds
  use ferp_options
  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
  use, intrinsic :: iso_c_binding, only: c_int
  implicit none
  private

  public :: print_match, print_count, print_filename
  public :: print_context_line, print_separator
  public :: print_binary_match, print_only_match
  public :: print_match_colored
  public :: stdout_is_tty

  ! C interface for isatty
  interface
    function c_isatty(fd) bind(C, name="isatty")
      import :: c_int
      integer(c_int), value :: fd
      integer(c_int) :: c_isatty
    end function c_isatty
  end interface

  ! ANSI color codes
  character(len=*), parameter :: COLOR_MATCH = char(27) // '[01;31m'  ! Bold red
  character(len=*), parameter :: COLOR_RESET = char(27) // '[0m'
  character(len=*), parameter :: COLOR_FILENAME = char(27) // '[35m'  ! Magenta
  character(len=*), parameter :: COLOR_LINENUM = char(27) // '[32m'   ! Green
  character(len=*), parameter :: COLOR_SEP = char(27) // '[36m'       ! Cyan

contains

  function stdout_is_tty() result(is_tty)
    !> Check if stdout is a terminal (for --color=auto)
    logical :: is_tty
    integer(c_int), parameter :: STDOUT_FILENO = 1

    is_tty = (c_isatty(STDOUT_FILENO) /= 0)
  end function stdout_is_tty

  subroutine print_match(line, filename, line_num, byte_off, opts)
    !> Print a matching line with appropriate prefixes
    !> Thread-safe via OMP critical section
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts

    ! Quiet mode - no output
    if (opts%quiet) return

    !$omp critical(output_lock)
    ! Print filename prefix
    if (opts%show_filename .and. .not. opts%hide_filename) then
      if (opts%null_after_filename) then
        write(output_unit, '(A,A)', advance='no') trim(filename), char(0)
      else
        write(output_unit, '(A,A)', advance='no') trim(filename), ':'
      end if
    end if

    ! Print line number prefix
    if (opts%show_line_number) then
      write(output_unit, '(I0,A)', advance='no') line_num, ':'
    end if

    ! Print byte offset prefix
    if (opts%show_byte_offset) then
      write(output_unit, '(I0,A)', advance='no') byte_off, ':'
    end if

    ! Print tab alignment if requested
    if (opts%initial_tab) then
      write(output_unit, '(A)', advance='no') char(9)  ! TAB
    end if

    ! Print the line
    if (opts%null_data) then
      write(output_unit, '(A,A)', advance='no') trim(line), char(0)
    else
      write(output_unit, '(A)') trim(line)
    end if

    ! Line-buffered mode
    if (opts%line_buffered) flush(output_unit)
    !$omp end critical(output_lock)

  end subroutine print_match

  subroutine print_context_line(line, filename, line_num, byte_off, opts)
    !> Print a context line (uses - instead of : as separator)
    !> Thread-safe via OMP critical section
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    !$omp critical(output_lock)
    ! Print filename prefix with - separator
    if (opts%show_filename .and. .not. opts%hide_filename) then
      if (opts%null_after_filename) then
        write(output_unit, '(A,A)', advance='no') trim(filename), char(0)
      else
        write(output_unit, '(A,A)', advance='no') trim(filename), '-'
      end if
    end if

    ! Print line number prefix with - separator
    if (opts%show_line_number) then
      write(output_unit, '(I0,A)', advance='no') line_num, '-'
    end if

    ! Print byte offset prefix with - separator
    if (opts%show_byte_offset) then
      write(output_unit, '(I0,A)', advance='no') byte_off, '-'
    end if

    ! Print tab alignment if requested
    if (opts%initial_tab) then
      write(output_unit, '(A)', advance='no') char(9)
    end if

    ! Print the line
    if (opts%null_data) then
      write(output_unit, '(A,A)', advance='no') trim(line), char(0)
    else
      write(output_unit, '(A)') trim(line)
    end if

    ! Line-buffered mode
    if (opts%line_buffered) flush(output_unit)
    !$omp end critical(output_lock)

  end subroutine print_context_line

  subroutine print_separator(opts)
    !> Print group separator between context groups
    !> Thread-safe via OMP critical section
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return
    if (opts%no_group_separator) return

    !$omp critical(output_lock)
    write(output_unit, '(A)') trim(opts%group_separator)

    ! Line-buffered mode
    if (opts%line_buffered) flush(output_unit)
    !$omp end critical(output_lock)

  end subroutine print_separator

  subroutine print_count(count, filename, opts)
    !> Print match count (for -c option)
    !> Thread-safe via OMP critical section
    integer, intent(in) :: count
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    !$omp critical(output_lock)
    if (opts%show_filename .and. .not. opts%hide_filename) then
      if (opts%null_after_filename) then
        write(output_unit, '(A,A,I0)') trim(filename), char(0), count
      else
        write(output_unit, '(A,A,I0)') trim(filename), ':', count
      end if
    else
      write(output_unit, '(I0)') count
    end if

    ! Line-buffered mode
    if (opts%line_buffered) flush(output_unit)
    !$omp end critical(output_lock)

  end subroutine print_count

  subroutine print_filename(filename, opts)
    !> Print just filename (for -l, -L options)
    !> Thread-safe via OMP critical section
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    !$omp critical(output_lock)
    if (opts%null_after_filename) then
      write(output_unit, '(A,A)', advance='no') trim(filename), char(0)
    else
      write(output_unit, '(A)') trim(filename)
    end if

    ! Line-buffered mode
    if (opts%line_buffered) flush(output_unit)
    !$omp end critical(output_lock)

  end subroutine print_filename

  subroutine print_binary_match(filename, opts)
    !> Print binary file match message
    !> Thread-safe via OMP critical section
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    !$omp critical(output_lock)
    write(output_unit, '(A)') 'Binary file ' // trim(filename) // ' matches'

    ! Line-buffered mode
    if (opts%line_buffered) flush(output_unit)
    !$omp end critical(output_lock)

  end subroutine print_binary_match

  subroutine print_only_match(line, match_start, match_end, filename, line_num, byte_off, opts)
    !> Print only the matched portion of a line (for -o option)
    !> Thread-safe via OMP critical section
    character(len=*), intent(in) :: line
    integer, intent(in) :: match_start, match_end
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    !$omp critical(output_lock)
    ! Print filename prefix
    if (opts%show_filename .and. .not. opts%hide_filename) then
      if (opts%null_after_filename) then
        write(output_unit, '(A,A)', advance='no') trim(filename), char(0)
      else
        write(output_unit, '(A,A)', advance='no') trim(filename), ':'
      end if
    end if

    ! Print line number prefix
    if (opts%show_line_number) then
      write(output_unit, '(I0,A)', advance='no') line_num, ':'
    end if

    ! Print byte offset prefix (offset to start of match)
    if (opts%show_byte_offset) then
      write(output_unit, '(I0,A)', advance='no') byte_off + match_start - 1, ':'
    end if

    ! Print tab alignment if requested
    if (opts%initial_tab) then
      write(output_unit, '(A)', advance='no') char(9)
    end if

    ! Print just the matched portion
    if (match_start >= 1 .and. match_end >= match_start .and. match_end <= len(line)) then
      if (opts%null_data) then
        write(output_unit, '(A,A)', advance='no') line(match_start:match_end), char(0)
      else
        write(output_unit, '(A)') line(match_start:match_end)
      end if
    end if

    ! Line-buffered mode
    if (opts%line_buffered) flush(output_unit)
    !$omp end critical(output_lock)

  end subroutine print_only_match

  subroutine print_match_colored(line, filename, line_num, byte_off, opts, &
                                  match_starts, match_ends, num_matches)
    !> Print a matching line with colored highlighting of matches
    !> Thread-safe via OMP critical section
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts
    integer, intent(in) :: match_starts(:), match_ends(:)
    integer, intent(in) :: num_matches

    integer :: i, pos, line_len
    logical :: use_color

    if (opts%quiet) return

    ! Determine if we should use color
    use_color = .false.
    if (opts%color_mode == COLOR_ALWAYS) then
      use_color = .true.
    else if (opts%color_mode == COLOR_AUTO) then
      use_color = stdout_is_tty()
    end if

    !$omp critical(output_lock)
    ! Print filename prefix
    if (opts%show_filename .and. .not. opts%hide_filename) then
      if (use_color) then
        write(output_unit, '(A)', advance='no') COLOR_FILENAME
      end if
      if (opts%null_after_filename) then
        write(output_unit, '(A,A)', advance='no') trim(filename), char(0)
      else
        write(output_unit, '(A)', advance='no') trim(filename)
      end if
      if (use_color) then
        write(output_unit, '(A)', advance='no') COLOR_RESET
      end if
      if (.not. opts%null_after_filename) then
        if (use_color) then
          write(output_unit, '(A,A,A)', advance='no') COLOR_SEP, ':', COLOR_RESET
        else
          write(output_unit, '(A)', advance='no') ':'
        end if
      end if
    end if

    ! Print line number prefix
    if (opts%show_line_number) then
      if (use_color) then
        write(output_unit, '(A,I0,A)', advance='no') COLOR_LINENUM, line_num, COLOR_RESET
        write(output_unit, '(A,A,A)', advance='no') COLOR_SEP, ':', COLOR_RESET
      else
        write(output_unit, '(I0,A)', advance='no') line_num, ':'
      end if
    end if

    ! Print byte offset prefix
    if (opts%show_byte_offset) then
      if (use_color) then
        write(output_unit, '(A,I0,A)', advance='no') COLOR_LINENUM, byte_off, COLOR_RESET
        write(output_unit, '(A,A,A)', advance='no') COLOR_SEP, ':', COLOR_RESET
      else
        write(output_unit, '(I0,A)', advance='no') byte_off, ':'
      end if
    end if

    ! Print tab alignment if requested
    if (opts%initial_tab) then
      write(output_unit, '(A)', advance='no') char(9)
    end if

    ! Print line with highlighted matches
    line_len = len_trim(line)
    if (num_matches > 0 .and. use_color) then
      pos = 1
      do i = 1, num_matches
        ! Print text before match
        if (match_starts(i) > pos) then
          write(output_unit, '(A)', advance='no') line(pos:match_starts(i)-1)
        end if
        ! Print highlighted match
        if (match_starts(i) >= 1 .and. match_ends(i) <= line_len) then
          write(output_unit, '(A)', advance='no') COLOR_MATCH
          write(output_unit, '(A)', advance='no') line(match_starts(i):match_ends(i))
          write(output_unit, '(A)', advance='no') COLOR_RESET
        end if
        pos = match_ends(i) + 1
      end do
      ! Print text after last match
      if (pos <= line_len) then
        if (opts%null_data) then
          write(output_unit, '(A,A)', advance='no') line(pos:line_len), char(0)
        else
          write(output_unit, '(A)') line(pos:line_len)
        end if
      else
        if (opts%null_data) then
          write(output_unit, '(A)', advance='no') char(0)
        else
          write(output_unit, '(A)') ''
        end if
      end if
    else
      ! No color - just print the line
      if (opts%null_data) then
        write(output_unit, '(A,A)', advance='no') trim(line), char(0)
      else
        write(output_unit, '(A)') trim(line)
      end if
    end if

    ! Line-buffered mode
    if (opts%line_buffered) flush(output_unit)
    !$omp end critical(output_lock)

  end subroutine print_match_colored

end module ferp_output
