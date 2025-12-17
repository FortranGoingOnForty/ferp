module ferp_output
  !> Output formatting for FERP
  use ferp_kinds
  use ferp_options
  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
  implicit none
  private

  public :: print_match, print_count, print_filename
  public :: print_context_line, print_separator
  public :: print_binary_match

contains

  subroutine print_match(line, filename, line_num, byte_off, opts)
    !> Print a matching line with appropriate prefixes
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts

    ! Quiet mode - no output
    if (opts%quiet) return

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
    write(output_unit, '(A)') trim(line)

  end subroutine print_match

  subroutine print_context_line(line, filename, line_num, byte_off, opts)
    !> Print a context line (uses - instead of : as separator)
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

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
    write(output_unit, '(A)') trim(line)

  end subroutine print_context_line

  subroutine print_separator(opts)
    !> Print group separator between context groups
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return
    if (opts%no_group_separator) return

    write(output_unit, '(A)') trim(opts%group_separator)

  end subroutine print_separator

  subroutine print_count(count, filename, opts)
    !> Print match count (for -c option)
    integer, intent(in) :: count
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    if (opts%show_filename .and. .not. opts%hide_filename) then
      if (opts%null_after_filename) then
        write(output_unit, '(A,A,I0)') trim(filename), char(0), count
      else
        write(output_unit, '(A,A,I0)') trim(filename), ':', count
      end if
    else
      write(output_unit, '(I0)') count
    end if

  end subroutine print_count

  subroutine print_filename(filename, opts)
    !> Print just filename (for -l, -L options)
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    if (opts%null_after_filename) then
      write(output_unit, '(A,A)', advance='no') trim(filename), char(0)
    else
      write(output_unit, '(A)') trim(filename)
    end if

  end subroutine print_filename

  subroutine print_binary_match(filename, opts)
    !> Print binary file match message
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    write(output_unit, '(A)') 'Binary file ' // trim(filename) // ' matches'

  end subroutine print_binary_match

end module ferp_output
