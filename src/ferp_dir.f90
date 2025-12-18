module ferp_dir
  !> Directory operations for FERP using POSIX C interop
  use ferp_kinds
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: is_directory, is_regular_file, collect_files
  public :: glob_match
  public :: read_patterns_from_file, matches_any_pattern

  ! C interfaces for POSIX directory functions
  interface
    function c_opendir(dirname) bind(C, name="opendir")
      import :: c_ptr, c_char
      character(kind=c_char), intent(in) :: dirname(*)
      type(c_ptr) :: c_opendir
    end function c_opendir

    function c_readdir(dirp) bind(C, name="readdir")
      import :: c_ptr
      type(c_ptr), value :: dirp
      type(c_ptr) :: c_readdir
    end function c_readdir

    function c_closedir(dirp) bind(C, name="closedir")
      import :: c_ptr, c_int
      type(c_ptr), value :: dirp
      integer(c_int) :: c_closedir
    end function c_closedir

    function c_stat(pathname, statbuf) bind(C, name="stat")
      import :: c_char, c_int, c_ptr
      character(kind=c_char), intent(in) :: pathname(*)
      type(c_ptr), value :: statbuf
      integer(c_int) :: c_stat
    end function c_stat

    function c_lstat(pathname, statbuf) bind(C, name="lstat")
      import :: c_char, c_int, c_ptr
      character(kind=c_char), intent(in) :: pathname(*)
      type(c_ptr), value :: statbuf
      integer(c_int) :: c_lstat
    end function c_lstat
  end interface

  ! Size of struct stat varies by platform, use generous size
  integer, parameter :: STAT_BUF_SIZE = 256

contains

  function is_directory(path) result(is_dir)
    !> Check if path is a directory
    character(len=*), intent(in) :: path
    logical :: is_dir

    type(c_ptr) :: dirp
    character(len=max_path_len+1) :: c_path
    integer(c_int) :: istat

    is_dir = .false.

    ! Try to open as directory
    c_path = trim(path) // c_null_char
    dirp = c_opendir(c_path)

    if (c_associated(dirp)) then
      is_dir = .true.
      istat = c_closedir(dirp)
    end if

  end function is_directory

  function is_regular_file(path) result(is_file)
    !> Check if path is a regular file (not directory, symlink, etc.)
    character(len=*), intent(in) :: path
    logical :: is_file

    logical :: exists

    is_file = .false.

    ! Use Fortran inquire
    inquire(file=path, exist=exists)
    if (.not. exists) return

    ! If it's not a directory, treat as regular file
    is_file = .not. is_directory(path)

  end function is_regular_file

  subroutine collect_files(start_path, file_list, num_files, recursive, &
                           follow_links, include_globs, num_include, &
                           exclude_globs, num_exclude, exclude_dirs, num_exclude_dirs)
    !> Collect files from a path, optionally recursively
    character(len=*), intent(in) :: start_path
    character(len=max_path_len), intent(out) :: file_list(:)
    integer, intent(out) :: num_files
    logical, intent(in) :: recursive
    logical, intent(in) :: follow_links
    character(len=max_path_len), intent(in) :: include_globs(:)
    integer, intent(in) :: num_include
    character(len=max_path_len), intent(in) :: exclude_globs(:)
    integer, intent(in) :: num_exclude
    character(len=max_path_len), intent(in) :: exclude_dirs(:)
    integer, intent(in) :: num_exclude_dirs

    ! SAVE used for large array - safe since file collection runs before parallel section
    ! MAX_DEPTH is the max number of directories that can be queued at once (not depth)
    integer, parameter :: MAX_DEPTH = 10000
    character(len=max_path_len), save :: dir_stack(MAX_DEPTH)
    integer :: stack_top
    character(len=max_path_len) :: current_dir, entry_path, entry_name
    type(c_ptr) :: dirp, entry_ptr
    integer(c_int) :: istat

    num_files = 0

    ! Check if start_path is a file or directory
    if (.not. is_directory(start_path)) then
      ! It's a file, just add it if it passes filters
      if (should_include_file_multi(start_path, include_globs, num_include, &
                                    exclude_globs, num_exclude)) then
        num_files = 1
        file_list(1) = start_path
      end if
      return
    end if

    ! Initialize directory stack
    stack_top = 1
    dir_stack(1) = start_path

    do while (stack_top > 0 .and. num_files < size(file_list))
      ! Pop directory from stack
      current_dir = dir_stack(stack_top)
      stack_top = stack_top - 1

      ! Open directory
      dirp = c_opendir(trim(current_dir) // c_null_char)
      if (.not. c_associated(dirp)) cycle

      ! Read directory entries
      do
        entry_ptr = c_readdir(dirp)
        if (.not. c_associated(entry_ptr)) exit

        ! Get entry name from dirent struct
        call get_dirent_name(entry_ptr, entry_name)

        ! Skip . and ..
        if (trim(entry_name) == '.' .or. trim(entry_name) == '..') cycle
        if (len_trim(entry_name) == 0) cycle

        ! Build full path
        if (current_dir(len_trim(current_dir):len_trim(current_dir)) == '/') then
          entry_path = trim(current_dir) // trim(entry_name)
        else
          entry_path = trim(current_dir) // '/' // trim(entry_name)
        end if

        ! Check if it's a directory
        if (is_directory(entry_path)) then
          if (recursive) then
            ! Check exclude-dir patterns
            if (num_exclude_dirs > 0) then
              if (matches_any_pattern(trim(entry_name), exclude_dirs, num_exclude_dirs)) cycle
            end if

            ! Push to stack for later processing
            if (stack_top < MAX_DEPTH) then
              stack_top = stack_top + 1
              dir_stack(stack_top) = entry_path
            end if
          end if
        else
          ! It's a file - check filters and add
          if (should_include_file_multi(entry_path, include_globs, num_include, &
                                        exclude_globs, num_exclude)) then
            if (num_files < size(file_list)) then
              num_files = num_files + 1
              file_list(num_files) = entry_path
            end if
          end if
        end if
      end do

      istat = c_closedir(dirp)
    end do

  end subroutine collect_files

  subroutine get_dirent_name(entry_ptr, name)
    !> Extract filename from dirent struct pointer
    type(c_ptr), intent(in) :: entry_ptr
    character(len=*), intent(out) :: name

    ! dirent.d_name starts at offset after d_ino and d_off (platform dependent)
    ! On most systems, d_name is at offset ~19-21 bytes
    ! We'll use a more robust approach: scan for printable chars
    character(len=256, kind=c_char), pointer :: raw_data
    integer :: i, start_pos, name_len

    name = ''
    if (.not. c_associated(entry_ptr)) return

    ! Map memory to character array
    call c_f_pointer(entry_ptr, raw_data)

    ! On macOS/Linux, d_name typically starts around byte 19-21
    ! Find start of name by scanning for first printable character after struct header
    ! Starting from offset 19 works on both platforms
    start_pos = 19

    ! Find actual start (first printable character after header)
    do i = start_pos, min(40, len(raw_data))
      if (ichar(raw_data(i:i)) >= 32 .and. ichar(raw_data(i:i)) < 127) then
        start_pos = i
        exit
      end if
    end do

    ! Copy name until null terminator
    name_len = 0
    do i = start_pos, min(start_pos + max_path_len - 1, len(raw_data))
      if (raw_data(i:i) == c_null_char) exit
      if (ichar(raw_data(i:i)) < 32 .or. ichar(raw_data(i:i)) >= 127) exit
      name_len = name_len + 1
      name(name_len:name_len) = raw_data(i:i)
    end do

  end subroutine get_dirent_name

  function should_include_file(filepath, include_glob, exclude_glob) result(include)
    !> Check if file should be included based on glob patterns
    character(len=*), intent(in) :: filepath
    character(len=*), intent(in) :: include_glob
    character(len=*), intent(in) :: exclude_glob
    logical :: include

    character(len=max_path_len) :: basename
    integer :: i

    include = .true.

    ! Extract basename
    basename = filepath
    do i = len_trim(filepath), 1, -1
      if (filepath(i:i) == '/') then
        basename = filepath(i+1:)
        exit
      end if
    end do

    ! Check include pattern (if specified, file must match)
    if (len_trim(include_glob) > 0) then
      include = glob_match(trim(basename), trim(include_glob))
      if (.not. include) return
    end if

    ! Check exclude pattern (if specified and matches, exclude)
    if (len_trim(exclude_glob) > 0) then
      if (glob_match(trim(basename), trim(exclude_glob))) then
        include = .false.
      end if
    end if

  end function should_include_file

  function should_include_file_multi(filepath, include_globs, num_include, &
                                     exclude_globs, num_exclude) result(include)
    !> Check if file should be included based on multiple glob patterns
    character(len=*), intent(in) :: filepath
    character(len=max_path_len), intent(in) :: include_globs(:)
    integer, intent(in) :: num_include
    character(len=max_path_len), intent(in) :: exclude_globs(:)
    integer, intent(in) :: num_exclude
    logical :: include

    character(len=max_path_len) :: basename
    integer :: i

    include = .true.

    ! Extract basename
    basename = filepath
    do i = len_trim(filepath), 1, -1
      if (filepath(i:i) == '/') then
        basename = filepath(i+1:)
        exit
      end if
    end do

    ! Check include patterns (if any specified, file must match at least one)
    if (num_include > 0) then
      include = .false.
      do i = 1, num_include
        if (glob_match(trim(basename), trim(include_globs(i)))) then
          include = .true.
          exit
        end if
      end do
      if (.not. include) return
    end if

    ! Check exclude patterns (if any match, exclude the file)
    if (num_exclude > 0) then
      do i = 1, num_exclude
        if (glob_match(trim(basename), trim(exclude_globs(i)))) then
          include = .false.
          return
        end if
      end do
    end if

  end function should_include_file_multi

  recursive function glob_match(str, pattern) result(matches)
    !> Simple glob pattern matching (* and ? wildcards)
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: pattern
    logical :: matches

    integer :: s, p, str_len, pat_len

    matches = .false.
    str_len = len(str)
    pat_len = len(pattern)

    ! Handle empty pattern
    if (pat_len == 0) then
      matches = (str_len == 0)
      return
    end if

    s = 1
    p = 1

    main_loop: do while (s <= str_len .and. p <= pat_len)
      if (pattern(p:p) == '*') then
        ! * matches zero or more characters
        ! Skip consecutive stars
        skip_stars: do while (p <= pat_len)
          if (pattern(p:p) /= '*') exit skip_stars
          p = p + 1
        end do skip_stars

        if (p > pat_len) then
          ! Pattern ends with * - matches rest of string
          matches = .true.
          return
        end if

        ! Try matching from each position in str
        try_positions: do while (s <= str_len)
          if (glob_match(str(s:str_len), pattern(p:pat_len))) then
            matches = .true.
            return
          end if
          s = s + 1
        end do try_positions

        ! Also try empty match (s > str_len)
        matches = glob_match('', pattern(p:pat_len))
        return
      else if (pattern(p:p) == '?') then
        ! ? matches exactly one character
        s = s + 1
        p = p + 1
      else if (pattern(p:p) == str(s:s)) then
        ! Exact character match
        s = s + 1
        p = p + 1
      else
        ! No match
        return
      end if
    end do main_loop

    ! Skip trailing wildcards in pattern
    trailing_stars: do while (p <= pat_len)
      if (pattern(p:p) /= '*') exit trailing_stars
      p = p + 1
    end do trailing_stars

    ! Match if both consumed
    matches = (s > str_len .and. p > pat_len)

  end function glob_match

  subroutine read_patterns_from_file(filename, patterns, num_patterns, ierr)
    !> Read glob patterns from a file, one per line
    character(len=*), intent(in) :: filename
    character(len=max_path_len), intent(out) :: patterns(:)
    integer, intent(out) :: num_patterns
    integer, intent(out) :: ierr

    integer :: unit_num, ios
    character(len=max_path_len) :: line

    num_patterns = 0
    ierr = 0

    open(newunit=unit_num, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      ierr = 1
      return
    end if

    do
      read(unit_num, '(A)', iostat=ios) line
      if (ios /= 0) exit

      ! Skip empty lines and comments
      if (len_trim(line) == 0) cycle
      if (line(1:1) == '#') cycle

      if (num_patterns < size(patterns)) then
        num_patterns = num_patterns + 1
        patterns(num_patterns) = trim(line)
      end if
    end do

    close(unit_num)

  end subroutine read_patterns_from_file

  function matches_any_pattern(filename, patterns, num_patterns) result(matches)
    !> Check if filename matches any pattern in the list
    character(len=*), intent(in) :: filename
    character(len=max_path_len), intent(in) :: patterns(:)
    integer, intent(in) :: num_patterns
    logical :: matches

    character(len=max_path_len) :: basename
    integer :: i

    matches = .false.
    if (num_patterns == 0) return

    ! Extract basename
    basename = filename
    do i = len_trim(filename), 1, -1
      if (filename(i:i) == '/') then
        basename = filename(i+1:)
        exit
      end if
    end do

    ! Check against each pattern
    do i = 1, num_patterns
      if (glob_match(trim(basename), trim(patterns(i)))) then
        matches = .true.
        return
      end if
    end do

  end function matches_any_pattern

end module ferp_dir
