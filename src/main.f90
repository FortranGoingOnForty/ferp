program ferp
  !> FERP - Fortran Expression Regular Print
  !> A GNU grep clone written in Modern Fortran
  use ferp_kinds
  use ferp_options
  use ferp_cli
  use ferp_io
  use ferp_dir
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
  character(len=max_path_len), allocatable :: expanded_files(:)
  type(input_source) :: src
  type(compiled_patterns_t) :: compiled
  integer :: ierr, i, j, num_collected
  integer, parameter :: MAX_FILES = 10000
  character(len=max_path_len) :: collected_files(MAX_FILES)
  logical :: any_match, file_match

  ! Parse command-line arguments
  call parse_arguments(opts, patterns, files, ierr)
  if (ierr /= 0) then
    call c_exit(2_c_int)
  end if

  ! Handle recursive mode - expand directories to file lists
  if (opts%recursive) then
    if (size(files) == 0) then
      ! Default to current directory when no files specified with -r
      deallocate(files)
      allocate(files(1))
      files(1) = '.'
    end if

    ! Expand all paths (files stay as-is, directories get expanded)
    allocate(expanded_files(0))
    do i = 1, size(files)
      call collect_files(trim(files(i)), collected_files, num_collected, &
                         .true., opts%dereference_recursive, &
                         trim(opts%include_glob), trim(opts%exclude_glob), &
                         trim(opts%exclude_dir))
      do j = 1, num_collected
        call append_file_to_list(expanded_files, collected_files(j))
      end do
    end do

    ! Replace files with expanded list
    deallocate(files)
    allocate(files(size(expanded_files)))
    files = expanded_files
    deallocate(expanded_files)

    ! Update multiple_files flag
    opts%multiple_files = (size(files) > 1)
    if (opts%multiple_files .and. .not. opts%hide_filename) then
      opts%show_filename = .true.
    end if
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
    if (src%open('-', null_data=opts%null_data)) then
      src%filename = opts%label  ! Use --label if provided
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
      ! Check for directory and handle according to dir_action
      if (.not. opts%recursive .and. is_directory(trim(files(i)))) then
        select case (opts%dir_action)
          case (DIR_SKIP)
            cycle  ! Skip directories silently
          case (DIR_RECURSE)
            ! Enable recursive mode for this directory
            opts%recursive = .true.
            opts%dir_action = DIR_RECURSE
          case default  ! DIR_READ
            ! Will try to read directory as file (usually fails)
        end select
      end if

      ! Check for binary file BEFORE opening
      if (.not. opts%text_mode) then
        src%is_binary = check_binary_file(trim(files(i)))
        if (src%is_binary .and. opts%ignore_binary) cycle
      else
        src%is_binary = .false.
      end if

      if (src%open(trim(files(i)), opts%no_messages, opts%null_data)) then
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

contains

  subroutine append_file_to_list(file_list, filename)
    !> Append a file to an allocatable file list
    character(len=max_path_len), allocatable, intent(inout) :: file_list(:)
    character(len=*), intent(in) :: filename

    character(len=max_path_len), allocatable :: temp(:)
    integer :: n

    n = size(file_list)
    allocate(temp(n + 1))
    if (n > 0) temp(1:n) = file_list
    temp(n + 1) = filename
    call move_alloc(temp, file_list)
  end subroutine append_file_to_list

end program ferp
