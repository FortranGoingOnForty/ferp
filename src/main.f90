program ferp
  !> FERP - Fortran Expression Regular Print
  !> A GNU grep clone written in Modern Fortran
  use ferp_kinds
  use ferp_options
  use ferp_cli
  use ferp_io
  use ferp_dir
  use ferp_matcher
  use ferp_output, only: init_capture_slots, begin_capture, end_capture, emit_raw
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

  !> Holds one file's captured output so the parallel search can be replayed
  !> in command-line order. Unallocated for files that produced nothing.
  type :: outbuf_t
    character(len=:), allocatable :: s
  end type outbuf_t

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
  logical :: has_error  ! Track if any errors occurred (for exit code 2)
  type(outbuf_t), allocatable :: outbufs(:)
  logical :: use_capture

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

  ! Compile patterns (for all modes - regex uses NFA/PCRE, fixed uses Boyer-Moore)
  call compile_patterns(patterns, opts, compiled, ierr)
  if (ierr /= 0) then
    write(error_unit, '(A)') 'ferp: Invalid regular expression'
    call c_exit(2_c_int)
  end if

  any_match = .false.
  found_early = .false.
  has_error = .false.

  ! Process input sources
  if (size(files) == 0) then
    ! No files specified - read from stdin
    opts%reading_stdin = .true.
    if (src%open('-', null_data=opts%null_data)) then
      src%filename = opts%label  ! Use --label if provided
      any_match = process_source(src, patterns, opts, compiled)
      call src%close()
    end if
  else
    ! Searching files concurrently would interleave their output, but grep
    ! prints whole files in command-line order. So when we go parallel each
    ! file's output is captured to its own buffer and replayed in order below.
    !
    ! --line-buffered asks for output as it is produced, which is
    ! incompatible with buffering, so that case runs serially instead and
    ! streams straight to stdout. A single file streams for the same reason:
    ! it keeps `ferp pattern huge.log | head` responsive.
#ifdef _OPENMP
    use_capture = (size(files) > 1) .and. .not. opts%line_buffered
#else
    ! Serial build: the loop already emits files in order, so stream straight
    ! to stdout instead of holding every file's output in memory.
    use_capture = .false.
#endif
    if (use_capture) then
      allocate(outbufs(size(files)))
      call init_capture_slots()
    end if

    ! WARNING: this loop is NOT thread-safe, which is why the release build no
    ! longer passes -fopenmp. gfortran (checked through 16.1.1, and unaffected
    ! by -frecursive) stores the length temporary of every deferred-length
    ! `character(len=:), allocatable` assignment in shared static storage --
    ! `nm` shows them as `slen.*` in ferp_io.o, ferp_matcher.o and
    ! ferp_output.o. Two threads assigning such a string at once clobber each
    ! other's length, which truncates and duplicates output. ThreadSanitizer
    ! reports it against e.g. ferp_matcher.f90's `line = src%get_line_text(..)`.
    !
    ! Re-enabling -fopenmp requires first rewriting those paths to use
    ! fixed-length buffers with explicit length variables. The capture
    ! machinery below is kept because it is what keeps output in command-line
    ! order once the loop does run in parallel.
    !
    ! opts and compiled are firstprivate because process_source writes per-file
    ! state into both (opts%line_number_width for -T, and the optimizer's DFA
    ! cache inside compiled).
    !$omp parallel do if(use_capture) default(shared) private(src, file_match) &
    !$omp& firstprivate(opts, compiled) &
    !$omp& reduction(.or.:any_match,has_error) schedule(dynamic)
    do i = 1, size(files)
      ! Early termination check for quiet mode
      if (opts%quiet .and. found_early) cycle

      ! Check for directory and handle according to dir_action.
      ! Nested rather than .and. so the stat() call is only made when
      ! needed -- Fortran does not guarantee short-circuit evaluation.
      if (.not. opts%recursive) then
        if (is_directory(trim(files(i)))) then
          select case (opts%dir_action)
            case (DIR_SKIP)
              cycle  ! Skip directories silently
            case (DIR_RECURSE)
              ! Note: In parallel mode, we can't modify opts
              ! This path is rare - usually -r is specified explicitly
              cycle
            case default  ! DIR_READ
              ! Print error message and skip (like grep)
              if (.not. opts%no_messages) then
                !$omp critical(error_output)
                write(error_unit, '(A)') 'ferp: ' // trim(files(i)) // ': Is a directory'
                !$omp end critical(error_output)
              end if
              has_error = .true.
              cycle
          end select
        end if
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
        ! Capture wraps process_source only. Every cycle above happens before
        ! this point, so begin/end always pair up.
        if (use_capture) call begin_capture()
        file_match = process_source(src, patterns, opts, compiled)
        if (use_capture) call end_capture(outbufs(i)%s)
        if (file_match) then
          any_match = .true.
          ! Signal early termination for quiet mode
          if (opts%quiet) found_early = .true.
        end if
        call src%close()
      else
        ! File open failed - set error flag
        has_error = .true.
      end if
    end do
    !$omp end parallel do

    ! Replay the captured output in command-line order
    if (use_capture) then
      do i = 1, size(files)
        if (allocated(outbufs(i)%s)) call emit_raw(outbufs(i)%s)
      end do
      deallocate(outbufs)
    end if
  end if

  ! Clean up compiled patterns
  call free_patterns(compiled)

  ! Exit with appropriate code
  ! 0 = match found, 1 = no match, 2 = error
  ! Note: grep returns 2 if there's any error, even with matches
  if (has_error) then
    call c_exit(2_c_int)
  else if (any_match) then
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
