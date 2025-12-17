module ferp_cli
  !> Command-line argument parsing for FERP
  use ferp_kinds
  use ferp_options
  use, intrinsic :: iso_fortran_env, only: error_unit
  use, intrinsic :: iso_c_binding, only: c_int
  implicit none
  private

  public :: parse_arguments, print_help, print_version

  character(len=*), parameter :: VERSION = '0.1.0'

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
          call append_pattern(patterns, trim(arg))
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
        else
          ! Short option(s)
          call parse_short_options(opts, patterns, arg(2:), &
                                   has_explicit_pattern, need_arg, pending_option, ierr)
          if (ierr /= 0) return
        end if
      else
        ! Non-option argument
        if (.not. has_explicit_pattern .and. size(patterns) == 0) then
          call append_pattern(patterns, trim(arg))
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

    ! Validate: need at least one pattern
    if (size(patterns) == 0) then
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
        case ('i')
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

        ! Options requiring arguments
        case ('e')
          need_arg = .true.
          pending = 'e'
          return  ! Rest of optstr is handled as argument or next arg
        case ('f')
          need_arg = .true.
          pending = 'f'
          return
        case ('m')
          need_arg = .true.
          pending = 'm'
          return
        case ('A')
          need_arg = .true.
          pending = 'A'
          return
        case ('B')
          need_arg = .true.
          pending = 'B'
          return
        case ('C')
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

      ! File selection
      case ('recursive')
        opts%recursive = .true.
      case ('dereference-recursive')
        opts%recursive = .true.
        opts%dereference_recursive = .true.
      case ('include')
        if (eq_pos > 0) then
          opts%include_glob = trim(opt_value)
        else
          need_arg = .true.
          pending = 'include'
        end if
      case ('exclude')
        if (eq_pos > 0) then
          opts%exclude_glob = trim(opt_value)
        else
          need_arg = .true.
          pending = 'exclude'
        end if
      case ('exclude-dir')
        if (eq_pos > 0) then
          opts%exclude_dir = trim(opt_value)
        else
          need_arg = .true.
          pending = 'exclude-dir'
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
      case ('include')
        opts%include_glob = trim(arg)
      case ('exclude')
        opts%exclude_glob = trim(arg)
      case ('exclude-dir')
        opts%exclude_dir = trim(arg)
    end select

    if (ierr /= 0) then
      write(error_unit, '(A)') 'ferp: invalid argument for option: ' // trim(opt)
      ierr = 2
    end if
  end subroutine handle_option_argument

  subroutine read_patterns_from_file(patterns, filename, ierr)
    character(len=max_pattern_len), allocatable, intent(inout) :: patterns(:)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: ierr

    integer :: unit_num, ios
    character(len=max_pattern_len) :: line

    ierr = 0
    open(newunit=unit_num, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(error_unit, '(A)') 'ferp: ' // trim(filename) // ': No such file or directory'
      ierr = 2
      return
    end if

    do
      read(unit_num, '(A)', iostat=ios) line
      if (ios /= 0) exit
      call append_pattern(patterns, trim(line))
    end do

    close(unit_num)
  end subroutine read_patterns_from_file

  subroutine append_pattern(patterns, pattern)
    character(len=max_pattern_len), allocatable, intent(inout) :: patterns(:)
    character(len=*), intent(in) :: pattern

    character(len=max_pattern_len), allocatable :: temp(:)
    integer :: n

    n = size(patterns)
    allocate(temp(n + 1))
    if (n > 0) temp(1:n) = patterns
    temp(n + 1) = pattern
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
    write(*, '(A)') '      --color[=WHEN]        use markers to highlight the matching strings;'
    write(*, '(A)') '                            WHEN is "always", "never", or "auto"'
    write(*, '(A)') '  -T, --initial-tab         make tabs line up (if needed)'
    write(*, '(A)') '  -Z, --null                print 0 byte after FILE name'
    write(*, '(A)') ''
    write(*, '(A)') 'Context control:'
    write(*, '(A)') '  -B, --before-context=NUM  print NUM lines of leading context'
    write(*, '(A)') '  -A, --after-context=NUM   print NUM lines of trailing context'
    write(*, '(A)') '  -C, --context=NUM         print NUM lines of output context'
    write(*, '(A)') ''
    write(*, '(A)') 'File selection:'
    write(*, '(A)') '  -r, --recursive           search directories recursively'
    write(*, '(A)') '  -R, --dereference-recursive  likewise, but follow all symlinks'
    write(*, '(A)') '      --include=GLOB        search only files that match GLOB'
    write(*, '(A)') '      --exclude=GLOB        skip files that match GLOB'
    write(*, '(A)') '      --exclude-dir=GLOB    skip directories that match GLOB'
    write(*, '(A)') ''
    write(*, '(A)') 'Exit status is 0 if any line is selected, 1 otherwise;'
    write(*, '(A)') 'if any error occurs and -q is not given, the exit status is 2.'
  end subroutine print_help

  subroutine print_version()
    write(*, '(A)') 'ferp (Fortran Expression Regular Print) ' // VERSION
    write(*, '(A)') 'Written in Modern Fortran.'
  end subroutine print_version

end module ferp_cli
