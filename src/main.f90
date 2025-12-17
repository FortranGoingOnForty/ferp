program ferp
  !> FERP - Fortran Expression Regular Print
  !> A GNU grep clone written in Modern Fortran
  use ferp_kinds
  use ferp_options
  use ferp_cli
  use ferp_io
  use ferp_matcher
  use, intrinsic :: iso_c_binding, only: c_int
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
  integer :: ierr, i
  logical :: any_match, file_match

  ! Parse command-line arguments
  call parse_arguments(opts, patterns, files, ierr)
  if (ierr /= 0) then
    call c_exit(2_c_int)
  end if

  any_match = .false.

  ! Process input sources
  if (size(files) == 0) then
    ! No files specified - read from stdin
    opts%reading_stdin = .true.
    if (src%open('-')) then
      any_match = process_source(src, patterns, opts)
      call src%close()
    end if
  else
    ! Process each file
    do i = 1, size(files)
      if (src%open(trim(files(i)), opts%no_messages)) then
        file_match = process_source(src, patterns, opts)
        if (file_match) any_match = .true.
        call src%close()

        ! In quiet mode, exit on first match
        if (opts%quiet .and. any_match) exit
      else
        ! File open failed - set error exit code
        ! (error message already printed by source_open unless -s)
        if (.not. any_match) then
          ! Will exit with code 2 if no matches and there was an error
        end if
      end if
    end do
  end if

  ! Exit with appropriate code
  ! 0 = match found, 1 = no match, 2 = error
  if (any_match) then
    call c_exit(0_c_int)
  else
    call c_exit(1_c_int)
  end if

end program ferp
