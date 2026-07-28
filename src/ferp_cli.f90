module ferp_cli
  !> Command-line argument parsing for FERP
  use ferp_kinds
  use ferp_options
  use, intrinsic :: iso_fortran_env, only: error_unit
  use, intrinsic :: iso_c_binding, only: c_int
  implicit none
  private

  public :: parse_arguments, print_help, print_version

  !> Single source of truth for the version. CI checks that PKGBUILD,
  !> .SRCINFO and the Homebrew formula all agree with this string.
  character(len=*), parameter :: VERSION = '0.10.1'

  interface
    subroutine c_exit(status) bind(C, name="exit")
      import :: c_int
      integer(c_int), value :: status
    end subroutine c_exit
  end interface

contains

  subroutine parse_arguments(opts, patterns, files, ierr)
    !> Parse command-line arguments into options, patterns, and files
    type(grep_options), intent(out) :: opts
    character(len=max_pattern_len), allocatable, intent(out) :: patterns(:)
    character(len=max_path_len), allocatable, intent(out) :: files(:)
    integer, intent(out) :: ierr

    integer :: nargs, i, arg_len
    character(len=max_path_len) :: arg
    logical :: has_explicit_pattern
    logical :: end_of_options
    logical :: need_arg
    character(len=32) :: pending_option

    ierr = 0
    nargs = command_argument_count()
    has_explicit_pattern = .false.
    end_of_options = .false.
    need_arg = .false.
    pending_option = ''

    allocate(patterns(0))
    allocate(files(0))

    i = 1
    do while (i <= nargs)
      call get_command_argument(i, arg, arg_len)

      ! Handle pending option that needs an argument
      if (need_arg) then
        call handle_option_argument(opts, patterns, pending_option, arg, &
                                    has_explicit_pattern, ierr)
        if (ierr /= 0) return
        need_arg = .false.
        pending_option = ''
        i = i + 1
        cycle
      end if

      ! After --, everything is a file/pattern argument
      if (end_of_options) then
        if (.not. has_explicit_pattern .and. size(patterns) == 0) then
          ! Use exact length from get_command_argument to preserve whitespace patterns
          call append_pattern(patterns, arg(1:arg_len))
          has_explicit_pattern = .true.
        else
          call append_file(files, trim(arg))
        end if
        i = i + 1
        cycle
      end if

      ! Check for options
      if (arg(1:1) == '-' .and. arg_len > 1) then
        if (arg(1:2) == '--') then
          ! Long option
          if (arg_len == 2) then
            ! -- marks end of options
            end_of_options = .true.
          else
            call parse_long_option(opts, patterns, arg(3:), &
                                   has_explicit_pattern, need_arg, pending_option, ierr)
            if (ierr /= 0) return
          end if
        else if (is_numeric_option(arg(2:))) then
          ! -NUM shorthand for context (e.g., -3 = -C 3)
          call handle_numeric_context(opts, arg(2:), ierr)
          if (ierr /= 0) return
        else
          ! Short option(s)
          call parse_short_options(opts, patterns, arg(2:), &
                                   has_explicit_pattern, need_arg, pending_option, ierr)
          if (ierr /= 0) return
        end if
      else
        ! Non-option argument
        if (.not. has_explicit_pattern .and. size(patterns) == 0) then
          ! Use exact length from get_command_argument to preserve whitespace patterns
          call append_pattern(patterns, arg(1:arg_len))
          has_explicit_pattern = .true.
        else
          call append_file(files, trim(arg))
        end if
      end if

      i = i + 1
    end do

    ! Check for missing required argument
    if (need_arg) then
      write(error_unit, '(A)') 'ferp: option requires an argument -- ' // trim(pending_option)
      ierr = 2
      return
    end if

    ! Validate: need at least one pattern source (but empty pattern file is valid)
    if (.not. has_explicit_pattern) then
      write(error_unit, '(A)') 'ferp: no pattern specified'
      write(error_unit, '(A)') "Try 'ferp --help' for more information."
      ierr = 2
      return
    end if

    ! Set multiple_files flag
    opts%multiple_files = (size(files) > 1)

    ! Default filename display behavior
    if (.not. opts%hide_filename) then
      if (opts%multiple_files .or. opts%recursive) then
        opts%show_filename = .true.
      end if
    end if

  end subroutine parse_arguments

  subroutine parse_short_options(opts, patterns, optstr, has_pattern, need_arg, pending, ierr)
    type(grep_options), intent(inout) :: opts
    character(len=max_pattern_len), allocatable, intent(inout) :: patterns(:)
    character(len=*), intent(in) :: optstr
    logical, intent(inout) :: has_pattern
    logical, intent(out) :: need_arg
    character(len=32), intent(out) :: pending
    integer, intent(out) :: ierr

    integer :: j
    character(len=1) :: c

    ierr = 0
    need_arg = .false.
    pending = ''

    do j = 1, len_trim(optstr)
      c = optstr(j:j)

      select case (c)
        ! Pattern type
        case ('E')
          opts%pattern_type = PATTERN_ERE
        case ('F')
          opts%pattern_type = PATTERN_FIXED
        case ('G')
          opts%pattern_type = PATTERN_BRE
        case ('P')
          opts%pattern_type = PATTERN_PERL

        ! Matching control
        case ('i', 'y')  ! -y is obsolete synonym for -i
          opts%ignore_case = .true.
        case ('v')
          opts%invert_match = .true.
        case ('w')
          opts%word_regexp = .true.
        case ('x')
          opts%line_regexp = .true.

        ! Output control
        case ('c')
          opts%count_only = .true.
        case ('l')
          opts%files_with_matches = .true.
        case ('L')
          opts%files_without_match = .true.
        case ('o')
          opts%only_matching = .true.
        case ('q')
          opts%quiet = .true.
        case ('s')
          opts%no_messages = .true.

        ! Line prefix
        case ('n')
          opts%show_line_number = .true.
        case ('b')
          opts%show_byte_offset = .true.
        case ('H')
          opts%show_filename = .true.
        case ('h')
          opts%hide_filename = .true.
        case ('Z')
          opts%null_after_filename = .true.
        case ('T')
          opts%initial_tab = .true.

        ! File selection
        case ('r')
          opts%recursive = .true.
        case ('R')
          opts%recursive = .true.
          opts%dereference_recursive = .true.

        ! Binary
        case ('a')
          opts%text_mode = .true.
        case ('I')
          opts%ignore_binary = .true.
        case ('U')
          opts%text_mode = .false.

        ! Null-data mode
        case ('z')
          opts%null_data = .true.

        ! Options requiring arguments for directory/device action
        case ('d')
          if (j < len_trim(optstr)) then
            call handle_option_argument(opts, patterns, 'd', optstr(j+1:), has_pattern, ierr)
            return
          end if
          need_arg = .true.
          pending = 'd'
          return
        case ('D')
          if (j < len_trim(optstr)) then
            call handle_option_argument(opts, patterns, 'D', optstr(j+1:), has_pattern, ierr)
            return
          end if
          need_arg = .true.
          pending = 'D'
          return

        ! Options requiring arguments
        case ('e')
          if (j < len_trim(optstr)) then
            call handle_option_argument(opts, patterns, 'e', optstr(j+1:), has_pattern, ierr)
            return
          end if
          need_arg = .true.
          pending = 'e'
          return
        case ('f')
          if (j < len_trim(optstr)) then
            call handle_option_argument(opts, patterns, 'f', optstr(j+1:), has_pattern, ierr)
            return
          end if
          need_arg = .true.
          pending = 'f'
          return
        case ('m')
          if (j < len_trim(optstr)) then
            call handle_option_argument(opts, patterns, 'm', optstr(j+1:), has_pattern, ierr)
            return
          end if
          need_arg = .true.
          pending = 'm'
          return
        case ('A')
          if (j < len_trim(optstr)) then
            call handle_option_argument(opts, patterns, 'A', optstr(j+1:), has_pattern, ierr)
            return
          end if
          need_arg = .true.
          pending = 'A'
          return
        case ('B')
          if (j < len_trim(optstr)) then
            call handle_option_argument(opts, patterns, 'B', optstr(j+1:), has_pattern, ierr)
            return
          end if
          need_arg = .true.
          pending = 'B'
          return
        case ('C')
          if (j < len_trim(optstr)) then
            call handle_option_argument(opts, patterns, 'C', optstr(j+1:), has_pattern, ierr)
            return
          end if
          need_arg = .true.
          pending = 'C'
          return

        ! Help/version
        case ('V')
          call print_version()
          call c_exit(0_c_int)

        case default
          write(error_unit, '(A)') "ferp: invalid option -- '" // c // "'"
          write(error_unit, '(A)') "Try 'ferp --help' for more information."
          ierr = 2
          return
      end select
    end do
  end subroutine parse_short_options

  subroutine parse_long_option(opts, patterns, optstr, has_pattern, need_arg, pending, ierr)
    type(grep_options), intent(inout) :: opts
    character(len=max_pattern_len), allocatable, intent(inout) :: patterns(:)
    character(len=*), intent(in) :: optstr
    logical, intent(inout) :: has_pattern
    logical, intent(out) :: need_arg
    character(len=32), intent(out) :: pending
    integer, intent(out) :: ierr

    character(len=256) :: opt_name, opt_value
    integer :: eq_pos

    ierr = 0
    need_arg = .false.
    pending = ''

    ! Split on '=' if present
    eq_pos = index(optstr, '=')
    if (eq_pos > 0) then
      opt_name = optstr(1:eq_pos-1)
      opt_value = optstr(eq_pos+1:)
    else
      opt_name = optstr
      opt_value = ''
    end if

    select case (trim(opt_name))
      ! Pattern type
      case ('extended-regexp')
        opts%pattern_type = PATTERN_ERE
      case ('fixed-strings')
        opts%pattern_type = PATTERN_FIXED
      case ('basic-regexp')
        opts%pattern_type = PATTERN_BRE
      case ('perl-regexp')
        opts%pattern_type = PATTERN_PERL

      ! Matching control
      case ('ignore-case')
        opts%ignore_case = .true.
      case ('no-ignore-case')
        opts%ignore_case = .false.
      case ('invert-match')
        opts%invert_match = .true.
      case ('word-regexp')
        opts%word_regexp = .true.
      case ('line-regexp')
        opts%line_regexp = .true.

      ! Output control
      case ('count')
        opts%count_only = .true.
      case ('files-with-matches')
        opts%files_with_matches = .true.
      case ('files-without-match')
        opts%files_without_match = .true.
      case ('only-matching')
        opts%only_matching = .true.
      case ('quiet', 'silent')
        opts%quiet = .true.
      case ('no-messages')
        opts%no_messages = .true.

      ! Line prefix
      case ('line-number')
        opts%show_line_number = .true.
      case ('byte-offset')
        opts%show_byte_offset = .true.
      case ('with-filename')
        opts%show_filename = .true.
      case ('no-filename')
        opts%hide_filename = .true.
      case ('null')
        opts%null_after_filename = .true.
      case ('initial-tab')
        opts%initial_tab = .true.

      ! Context
      case ('after-context')
        if (eq_pos > 0) then
          read(opt_value, *, iostat=ierr) opts%after_context
        else
          need_arg = .true.
          pending = 'after-context'
        end if
      case ('before-context')
        if (eq_pos > 0) then
          read(opt_value, *, iostat=ierr) opts%before_context
        else
          need_arg = .true.
          pending = 'before-context'
        end if
      case ('context')
        if (eq_pos > 0) then
          read(opt_value, *, iostat=ierr) opts%before_context
          opts%after_context = opts%before_context
        else
          need_arg = .true.
          pending = 'context'
        end if
      case ('group-separator')
        if (eq_pos > 0) then
          opts%group_separator = opt_value(1:min(len(opt_value), 8))
        else
          need_arg = .true.
          pending = 'group-separator'
        end if
      case ('no-group-separator')
        opts%no_group_separator = .true.

      ! File selection
      case ('recursive')
        opts%recursive = .true.
      case ('dereference-recursive')
        opts%recursive = .true.
        opts%dereference_recursive = .true.
      case ('include')
        if (eq_pos > 0) then
          if (opts%num_include_globs < MAX_GLOBS) then
            opts%num_include_globs = opts%num_include_globs + 1
            opts%include_globs(opts%num_include_globs) = trim(opt_value)
          end if
        else
          need_arg = .true.
          pending = 'include'
        end if
      case ('exclude')
        if (eq_pos > 0) then
          if (opts%num_exclude_globs < MAX_GLOBS) then
            opts%num_exclude_globs = opts%num_exclude_globs + 1
            opts%exclude_globs(opts%num_exclude_globs) = trim(opt_value)
          end if
        else
          need_arg = .true.
          pending = 'exclude'
        end if
      case ('exclude-dir')
        if (eq_pos > 0) then
          if (opts%num_exclude_dirs < MAX_GLOBS) then
            opts%num_exclude_dirs = opts%num_exclude_dirs + 1
            opts%exclude_dirs(opts%num_exclude_dirs) = trim(opt_value)
          end if
        else
          need_arg = .true.
          pending = 'exclude-dir'
        end if
      case ('exclude-from')
        if (eq_pos > 0) then
          opts%exclude_from_file = trim(opt_value)
        else
          need_arg = .true.
          pending = 'exclude-from'
        end if
      case ('include-from')
        if (eq_pos > 0) then
          opts%include_from_file = trim(opt_value)
        else
          need_arg = .true.
          pending = 'include-from'
        end if

      ! Pattern specification
      case ('regexp')
        if (eq_pos > 0) then
          call append_pattern(patterns, trim(opt_value))
          has_pattern = .true.
        else
          need_arg = .true.
          pending = 'regexp'
        end if

      ! Binary
      case ('text')
        opts%text_mode = .true.
      case ('binary')
        opts%text_mode = .false.

      ! Color
      case ('color', 'colour')
        if (eq_pos > 0) then
          select case (trim(opt_value))
            case ('never')
              opts%color_mode = COLOR_NEVER
            case ('always')
              opts%color_mode = COLOR_ALWAYS
            case ('auto')
              opts%color_mode = COLOR_AUTO
          end select
        else
          opts%color_mode = COLOR_ALWAYS
        end if

      ! Max count
      case ('max-count')
        if (eq_pos > 0) then
          read(opt_value, *, iostat=ierr) opts%max_count
        else
          need_arg = .true.
          pending = 'max-count'
        end if

      ! Label for stdin
      case ('label')
        if (eq_pos > 0) then
          opts%label = trim(opt_value)
        else
          need_arg = .true.
          pending = 'label'
        end if

      ! Binary file handling
      case ('binary-files')
        if (eq_pos > 0) then
          select case (trim(opt_value))
            case ('binary')
              opts%text_mode = .false.
              opts%ignore_binary = .false.
            case ('without-match')
              opts%ignore_binary = .true.
            case ('text')
              opts%text_mode = .true.
            case default
              write(error_unit, '(A)') "ferp: invalid --binary-files type: " // trim(opt_value)
              ierr = 2
              return
          end select
        else
          need_arg = .true.
          pending = 'binary-files'
        end if

      ! Output mode
      case ('line-buffered')
        opts%line_buffered = .true.

      ! Null-data mode
      case ('null-data')
        opts%null_data = .true.

      ! Directory/device action
      case ('directories')
        if (eq_pos > 0) then
          select case (trim(opt_value))
            case ('read')
              opts%dir_action = DIR_READ
            case ('skip')
              opts%dir_action = DIR_SKIP
            case ('recurse')
              opts%dir_action = DIR_RECURSE
              opts%recursive = .true.
            case default
              write(error_unit, '(A)') "ferp: invalid --directories action: " // trim(opt_value)
              ierr = 2
              return
          end select
        else
          need_arg = .true.
          pending = 'directories'
        end if
      case ('devices')
        if (eq_pos > 0) then
          select case (trim(opt_value))
            case ('read')
              opts%dev_action = DEV_READ
            case ('skip')
              opts%dev_action = DEV_SKIP
            case default
              write(error_unit, '(A)') "ferp: invalid --devices action: " // trim(opt_value)
              ierr = 2
              return
          end select
        else
          need_arg = .true.
          pending = 'devices'
        end if

      ! Help/version
      case ('help')
        call print_help()
        call c_exit(0_c_int)
      case ('version')
        call print_version()
        call c_exit(0_c_int)

      case default
        write(error_unit, '(A)') "ferp: unrecognized option '--" // trim(opt_name) // "'"
        write(error_unit, '(A)') "Try 'ferp --help' for more information."
        ierr = 2
        return
    end select
  end subroutine parse_long_option

  subroutine handle_option_argument(opts, patterns, opt, arg, has_pattern, ierr)
    type(grep_options), intent(inout) :: opts
    character(len=max_pattern_len), allocatable, intent(inout) :: patterns(:)
    character(len=*), intent(in) :: opt, arg
    logical, intent(inout) :: has_pattern
    integer, intent(out) :: ierr

    ierr = 0

    select case (trim(opt))
      case ('e', 'regexp')
        call append_pattern(patterns, trim(arg))
        has_pattern = .true.
      case ('f', 'file')
        call read_patterns_from_file(patterns, trim(arg), ierr)
        if (ierr == 0) has_pattern = .true.
      case ('m', 'max-count')
        read(arg, *, iostat=ierr) opts%max_count
      case ('A', 'after-context')
        read(arg, *, iostat=ierr) opts%after_context
      case ('B', 'before-context')
        read(arg, *, iostat=ierr) opts%before_context
      case ('C', 'context')
        read(arg, *, iostat=ierr) opts%before_context
        opts%after_context = opts%before_context
      case ('group-separator')
        opts%group_separator = arg(1:min(len_trim(arg), 8))
      case ('include')
        if (opts%num_include_globs < MAX_GLOBS) then
          opts%num_include_globs = opts%num_include_globs + 1
          opts%include_globs(opts%num_include_globs) = trim(arg)
        end if
      case ('exclude')
        if (opts%num_exclude_globs < MAX_GLOBS) then
          opts%num_exclude_globs = opts%num_exclude_globs + 1
          opts%exclude_globs(opts%num_exclude_globs) = trim(arg)
        end if
      case ('exclude-dir')
        if (opts%num_exclude_dirs < MAX_GLOBS) then
          opts%num_exclude_dirs = opts%num_exclude_dirs + 1
          opts%exclude_dirs(opts%num_exclude_dirs) = trim(arg)
        end if
      case ('exclude-from')
        opts%exclude_from_file = trim(arg)
      case ('include-from')
        opts%include_from_file = trim(arg)
      case ('label')
        opts%label = trim(arg)
      case ('binary-files')
        select case (trim(arg))
          case ('binary')
            opts%text_mode = .false.
            opts%ignore_binary = .false.
          case ('without-match')
            opts%ignore_binary = .true.
          case ('text')
            opts%text_mode = .true.
          case default
            write(error_unit, '(A)') "ferp: invalid --binary-files type: " // trim(arg)
            ierr = 2
        end select
      case ('d', 'directories')
        select case (trim(arg))
          case ('read')
            opts%dir_action = DIR_READ
          case ('skip')
            opts%dir_action = DIR_SKIP
          case ('recurse')
            opts%dir_action = DIR_RECURSE
            opts%recursive = .true.
          case default
            write(error_unit, '(A)') "ferp: invalid --directories action: " // trim(arg)
            ierr = 2
        end select
      case ('D', 'devices')
        select case (trim(arg))
          case ('read')
            opts%dev_action = DEV_READ
          case ('skip')
            opts%dev_action = DEV_SKIP
          case default
            write(error_unit, '(A)') "ferp: invalid --devices action: " // trim(arg)
            ierr = 2
        end select
    end select

    if (ierr /= 0) then
      write(error_unit, '(A)') 'ferp: invalid argument for option: ' // trim(opt)
      ierr = 2
    end if
  end subroutine handle_option_argument

  subroutine read_patterns_from_file(patterns, filename, ierr)
    !> Read patterns from file, preserving exact line lengths (including whitespace-only lines)
    character(len=max_pattern_len), allocatable, intent(inout) :: patterns(:)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: ierr

    integer :: unit_num, ios, line_len
    character(len=max_pattern_len) :: line
    character(len=1) :: ch

    ierr = 0
    ! Use unformatted stream access for byte-by-byte reading
    open(newunit=unit_num, file=filename, status='old', action='read', &
         access='stream', form='unformatted', iostat=ios)
    if (ios /= 0) then
      write(error_unit, '(A)') 'ferp: ' // trim(filename) // ': No such file or directory'
      ierr = 2
      return
    end if

    line_len = 0
    line = ''

    do
      read(unit_num, iostat=ios) ch
      if (ios /= 0) then
        ! EOF or error - save current line if non-empty
        if (line_len > 0) then
          call append_pattern(patterns, line(1:line_len))
        end if
        exit
      end if

      if (ch == char(10)) then
        ! Newline - save pattern with exact length (even if zero for empty lines)
        if (line_len > 0) then
          call append_pattern(patterns, line(1:line_len))
        else
          ! Empty line - skip (grep ignores empty pattern lines)
        end if
        line_len = 0
        line = ''
      else if (ch == char(13)) then
        ! Carriage return - ignore (handle Windows line endings)
      else
        ! Regular character - add to line
        if (line_len < max_pattern_len) then
          line_len = line_len + 1
          line(line_len:line_len) = ch
        end if
      end if
    end do

    close(unit_num)
  end subroutine read_patterns_from_file

  subroutine append_pattern(patterns, pattern)
    !> Append a pattern to the patterns array, preserving its exact length
    !> Uses null terminator to mark the true end of the pattern
    character(len=max_pattern_len), allocatable, intent(inout) :: patterns(:)
    character(len=*), intent(in) :: pattern

    character(len=max_pattern_len), allocatable :: temp(:)
    integer :: n, plen

    n = size(patterns)
    allocate(temp(n + 1))
    if (n > 0) temp(1:n) = patterns

    ! Store pattern with null terminator to preserve exact length
    plen = len(pattern)
    if (plen > 0 .and. plen < max_pattern_len) then
      temp(n + 1) = pattern
      temp(n + 1)(plen + 1:plen + 1) = char(0)  ! Null terminator
    else if (plen == 0) then
      ! Empty pattern - store just null terminator
      temp(n + 1) = char(0)
    else
      ! Pattern too long - truncate
      temp(n + 1) = pattern(1:max_pattern_len)
    end if

    call move_alloc(temp, patterns)
  end subroutine append_pattern

  subroutine append_file(files, filename)
    character(len=max_path_len), allocatable, intent(inout) :: files(:)
    character(len=*), intent(in) :: filename

    character(len=max_path_len), allocatable :: temp(:)
    integer :: n

    n = size(files)
    allocate(temp(n + 1))
    if (n > 0) temp(1:n) = files
    temp(n + 1) = filename
    call move_alloc(temp, files)
  end subroutine append_file

  subroutine print_help()
    write(*, '(A)') 'Usage: ferp [OPTION]... PATTERN [FILE]...'
    write(*, '(A)') 'Search for PATTERN in each FILE.'
    write(*, '(A)') 'Example: ferp -i "hello world" menu.h main.c'
    write(*, '(A)') ''
    write(*, '(A)') 'Pattern selection and interpretation:'
    write(*, '(A)') '  -E, --extended-regexp     PATTERN is an extended regular expression'
    write(*, '(A)') '  -F, --fixed-strings       PATTERN is a set of newline-separated strings'
    write(*, '(A)') '  -G, --basic-regexp        PATTERN is a basic regular expression (default)'
    write(*, '(A)') '  -P, --perl-regexp         PATTERN is a Perl regular expression'
    write(*, '(A)') '  -e, --regexp=PATTERN      use PATTERN for matching'
    write(*, '(A)') '  -f, --file=FILE           obtain PATTERN from FILE'
    write(*, '(A)') '  -i, --ignore-case         ignore case distinctions'
    write(*, '(A)') '      --no-ignore-case      do not ignore case (default)'
    write(*, '(A)') '  -w, --word-regexp         force PATTERN to match only whole words'
    write(*, '(A)') '  -x, --line-regexp         force PATTERN to match only whole lines'
    write(*, '(A)') ''
    write(*, '(A)') 'Miscellaneous:'
    write(*, '(A)') '  -s, --no-messages         suppress error messages'
    write(*, '(A)') '  -v, --invert-match        select non-matching lines'
    write(*, '(A)') '      --help                display this help text and exit'
    write(*, '(A)') '  -V, --version             display version information and exit'
    write(*, '(A)') ''
    write(*, '(A)') 'Output control:'
    write(*, '(A)') '  -m, --max-count=NUM       stop after NUM matches'
    write(*, '(A)') '  -b, --byte-offset         print the byte offset with output lines'
    write(*, '(A)') '  -n, --line-number         print line number with output lines'
    write(*, '(A)') '  -H, --with-filename       print the file name for each match'
    write(*, '(A)') '  -h, --no-filename         suppress the file name prefix on output'
    write(*, '(A)') '  -o, --only-matching       show only the part of a line matching PATTERN'
    write(*, '(A)') '  -q, --quiet, --silent     suppress all normal output'
    write(*, '(A)') '  -c, --count               print only a count of matching lines per FILE'
    write(*, '(A)') '  -l, --files-with-matches  print only names of FILEs containing matches'
    write(*, '(A)') '  -L, --files-without-match print only names of FILEs containing no match'
    write(*, '(A)') '      --line-buffered       flush output on every line'
    write(*, '(A)') '      --color[=WHEN]        use markers to highlight the matching strings;'
    write(*, '(A)') '                            WHEN is "always", "never", or "auto"'
    write(*, '(A)') '      --label=LABEL         use LABEL as the standard input file name'
    write(*, '(A)') '  -T, --initial-tab         make tabs line up (if needed)'
    write(*, '(A)') '  -Z, --null                print 0 byte after FILE name'
    write(*, '(A)') '  -z, --null-data           treat input/output data as NUL-terminated lines'
    write(*, '(A)') ''
    write(*, '(A)') 'Context control:'
    write(*, '(A)') '  -B, --before-context=NUM  print NUM lines of leading context'
    write(*, '(A)') '  -A, --after-context=NUM   print NUM lines of trailing context'
    write(*, '(A)') '  -C, --context=NUM         print NUM lines of output context'
    write(*, '(A)') '      --group-separator=SEP use SEP as group separator (default: --)'
    write(*, '(A)') '      --no-group-separator  suppress group separator'
    write(*, '(A)') ''
    write(*, '(A)') 'File selection:'
    write(*, '(A)') '  -d, --directories=ACTION  how to handle directories;'
    write(*, '(A)') '                            ACTION is "read", "recurse", or "skip"'
    write(*, '(A)') '  -D, --devices=ACTION      how to handle devices; ACTION is "read" or "skip"'
    write(*, '(A)') '  -r, --recursive           equivalent to --directories=recurse'
    write(*, '(A)') '  -R, --dereference-recursive  likewise, but follow all symlinks'
    write(*, '(A)') '      --include=GLOB        search only files that match GLOB'
    write(*, '(A)') '      --include-from=FILE   read include patterns from FILE'
    write(*, '(A)') '      --exclude=GLOB        skip files that match GLOB'
    write(*, '(A)') '      --exclude-from=FILE   read exclude patterns from FILE'
    write(*, '(A)') '      --exclude-dir=GLOB    skip directories that match GLOB'
    write(*, '(A)') ''
    write(*, '(A)') 'Binary file handling:'
    write(*, '(A)') '  -a, --text                equivalent to --binary-files=text'
    write(*, '(A)') '  -I                        equivalent to --binary-files=without-match'
    write(*, '(A)') '  -U, --binary              do not strip CR at EOL (default)'
    write(*, '(A)') '      --binary-files=TYPE   assume binary files are TYPE;'
    write(*, '(A)') '                            TYPE is "binary", "text", or "without-match"'
    write(*, '(A)') ''
    write(*, '(A)') 'Exit status is 0 if any line is selected, 1 otherwise;'
    write(*, '(A)') 'if any error occurs and -q is not given, the exit status is 2.'
  end subroutine print_help

  subroutine print_version()
    write(*, '(A)') 'ferp (Fortran Expression Regular Print) ' // VERSION
    write(*, '(A)') 'Written in Modern Fortran.'
  end subroutine print_version

  function is_numeric_option(str) result(is_num)
    !> Check if string is all digits (for -NUM context shorthand)
    character(len=*), intent(in) :: str
    logical :: is_num
    integer :: i, ic

    is_num = .false.
    if (len_trim(str) == 0) return

    do i = 1, len_trim(str)
      ic = ichar(str(i:i))
      if (ic < ichar('0') .or. ic > ichar('9')) return
    end do

    is_num = .true.
  end function is_numeric_option

  subroutine handle_numeric_context(opts, numstr, ierr)
    !> Handle -NUM shorthand for context lines (e.g., -3 = -C 3)
    type(grep_options), intent(inout) :: opts
    character(len=*), intent(in) :: numstr
    integer, intent(out) :: ierr

    integer :: num, ios

    ierr = 0
    read(numstr, *, iostat=ios) num

    if (ios /= 0 .or. num < 0) then
      write(error_unit, '(A)') 'ferp: invalid context length: ' // trim(numstr)
      ierr = 2
      return
    end if

    opts%before_context = num
    opts%after_context = num
  end subroutine handle_numeric_context

end module ferp_cli
