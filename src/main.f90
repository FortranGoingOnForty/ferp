program ferp
  !> FERP - Fortran Expression Regular Print
  !> A GNU grep clone written in Modern Fortran
  use ferp_kinds
  use ferp_options
  use ferp_cli
  use ferp_io
  use ferp_matcher
  use, intrinsic :: iso_c_binding, only: c_int
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none

  interface
    subroutine c_exit(status) bind(C, name="exit")
      import :: c_int
      integer(c_int), value :: status
    end subroutine c_exit
  end interface

  type(grep_options) :: opts
  character(len=max_pattern_len), allocatable :: patterns(:)
  character(len=max_path_len), allocatable :: files(:)
  type(input_source) :: src
  type(compiled_patterns_t) :: compiled
  integer :: ierr, i
  logical :: any_match, file_match

  ! Parse command-line arguments
  call parse_arguments(opts, patterns, files, ierr)
  if (ierr /= 0) then
    call c_exit(2_c_int)
  end if

  ! Compile patterns for regex modes
  if (opts%pattern_type /= PATTERN_FIXED) then
    call compile_patterns(patterns, opts, compiled, ierr)
    if (ierr /= 0) then
      write(error_unit, '(A)') 'ferp: Invalid regular expression'
      call c_exit(2_c_int)
    end if
  end if

  any_match = .false.

  ! Process input sources
  if (size(files) == 0) then
    ! No files specified - read from stdin
    opts%reading_stdin = .true.
    if (src%open('-')) then
      if (opts%pattern_type /= PATTERN_FIXED) then
        any_match = process_source(src, patterns, opts, compiled)
      else
        any_match = process_source(src, patterns, opts)
      end if
      call src%close()
    end if
  else
    ! Process each file
    do i = 1, size(files)
      if (src%open(trim(files(i)), opts%no_messages)) then
        if (opts%pattern_type /= PATTERN_FIXED) then
          file_match = process_source(src, patterns, opts, compiled)
        else
          file_match = process_source(src, patterns, opts)
        end if
        if (file_match) any_match = .true.
        call src%close()

        ! In quiet mode, exit on first match
        if (opts%quiet .and. any_match) exit
      end if
    end do
  end if

  ! Clean up compiled patterns
  if (opts%pattern_type /= PATTERN_FIXED) then
    call free_patterns(compiled)
  end if

  ! Exit with appropriate code
  ! 0 = match found, 1 = no match, 2 = error
  if (any_match) then
    call c_exit(0_c_int)
  else
    call c_exit(1_c_int)
  end if

end program ferp
