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
#ifdef _OPENMP
  use omp_lib
#endif
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
  integer, parameter :: MAX_PATTERNS = 1000
  character(len=max_path_len), allocatable :: collected_files(:)
  character(len=max_path_len), save :: exclude_patterns(MAX_PATTERNS)
  character(len=max_path_len), save :: include_patterns(MAX_PATTERNS)
  integer :: num_exclude_patterns, num_include_patterns
  logical :: any_match, file_match
  logical :: found_early  ! For quiet mode early termination in parallel

  ! Parse command-line arguments
  call parse_arguments(opts, patterns, files, ierr)
  if (ierr /= 0) then
    call c_exit(2_c_int)
  end if

  ! Read exclude patterns from file if specified
  num_exclude_patterns = 0
  if (len_trim(opts%exclude_from_file) > 0) then
    call read_patterns_from_file(trim(opts%exclude_from_file), exclude_patterns, &
                                 num_exclude_patterns, ierr)
    if (ierr /= 0) then
      write(error_unit, '(A)') 'ferp: ' // trim(opts%exclude_from_file) // &
                               ': No such file or directory'
      call c_exit(2_c_int)
    end if
  end if

  ! Read include patterns from file if specified
  num_include_patterns = 0
  if (len_trim(opts%include_from_file) > 0) then
    call read_patterns_from_file(trim(opts%include_from_file), include_patterns, &
                                 num_include_patterns, ierr)
    if (ierr /= 0) then
      write(error_unit, '(A)') 'ferp: ' // trim(opts%include_from_file) // &
                               ': No such file or directory'
      call c_exit(2_c_int)
    end if
  end if

  ! Handle recursive mode - expand directories to file lists
  if (opts%recursive) then
    if (size(files) == 0) then
      ! Default to current directory when no files specified with -r
      deallocate(files)
      allocate(files(1))
      files(1) = '.'
    end if

    ! Allocate buffer for collected files (100K files per directory scan)
    allocate(collected_files(100000))

    ! Expand all paths (files stay as-is, directories get expanded)
    allocate(expanded_files(0))
    do i = 1, size(files)
      call collect_files(trim(files(i)), collected_files, num_collected, &
                         .true., opts%dereference_recursive, &
                         opts%include_globs, opts%num_include_globs, &
                         opts%exclude_globs, opts%num_exclude_globs, &
                         opts%exclude_dirs, opts%num_exclude_dirs)
      do j = 1, num_collected
        call append_file_to_list(expanded_files, collected_files(j))
      end do
    end do

    ! Replace files with expanded list
    deallocate(collected_files)  ! Free temporary buffer
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
  found_early = .false.

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
    ! Process each file with OpenMP parallelization (release builds)
    ! Thread-safe: all buffers are now dynamically allocated per-thread
    !$omp parallel do default(shared) private(src, file_match) &
    !$omp& reduction(.or.:any_match) schedule(dynamic)
    do i = 1, size(files)
      ! Early termination check for quiet mode
      if (opts%quiet .and. found_early) cycle

      ! Check for directory and handle according to dir_action
      if (.not. opts%recursive .and. is_directory(trim(files(i)))) then
        select case (opts%dir_action)
          case (DIR_SKIP)
            cycle  ! Skip directories silently
          case (DIR_RECURSE)
            ! Note: In parallel mode, we can't modify opts
            ! This path is rare - usually -r is specified explicitly
            cycle
          case default  ! DIR_READ
            ! Will try to read directory as file (usually fails)
        end select
      end if

      ! Check include patterns from file
      if (num_include_patterns > 0) then
        if (.not. matches_any_pattern(trim(files(i)), include_patterns, num_include_patterns)) then
          cycle
        end if
      end if

      ! Check exclude patterns from file
      if (num_exclude_patterns > 0) then
        if (matches_any_pattern(trim(files(i)), exclude_patterns, num_exclude_patterns)) then
          cycle
        end if
      end if

      ! Check for binary file BEFORE opening
      if (.not. opts%text_mode) then
        src%is_binary = check_binary_file(trim(files(i)))
        if (src%is_binary .and. opts%ignore_binary) cycle
      else
        src%is_binary = .false.
      end if

      if (src%open(trim(files(i)), opts%no_messages, opts%null_data)) then
        ! Critical section for output serialization (prevents interleaved output)
        !$omp critical(output_lock)
        if (opts%pattern_type /= PATTERN_FIXED) then
          file_match = process_source(src, patterns, opts, compiled)
        else
          file_match = process_source(src, patterns, opts)
        end if
        !$omp end critical(output_lock)
        if (file_match) then
          any_match = .true.
          ! Signal early termination for quiet mode
          if (opts%quiet) found_early = .true.
        end if
        call src%close()
      end if
    end do
    !$omp end parallel do
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
