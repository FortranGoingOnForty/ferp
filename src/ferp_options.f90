module ferp_options
  !> FERP options - holds all command-line configuration
  use ferp_kinds, only: max_path_len, max_pattern_len
  implicit none
  private

  public :: grep_options
  public :: PATTERN_BRE, PATTERN_ERE, PATTERN_FIXED, PATTERN_PERL
  public :: COLOR_NEVER, COLOR_AUTO, COLOR_ALWAYS
  public :: DIR_READ, DIR_SKIP, DIR_RECURSE
  public :: DEV_READ, DEV_SKIP
  public :: MAX_GLOBS

  !> Pattern type constants
  integer, parameter :: PATTERN_BRE = 1
  integer, parameter :: PATTERN_ERE = 2
  integer, parameter :: PATTERN_FIXED = 3
  integer, parameter :: PATTERN_PERL = 4

  !> Color mode constants
  integer, parameter :: COLOR_NEVER = 0
  integer, parameter :: COLOR_AUTO = 1
  integer, parameter :: COLOR_ALWAYS = 2

  !> Directory action constants
  integer, parameter :: DIR_READ = 0
  integer, parameter :: DIR_SKIP = 1
  integer, parameter :: DIR_RECURSE = 2

  !> Device action constants
  integer, parameter :: DEV_READ = 0
  integer, parameter :: DEV_SKIP = 1

  !> Max glob patterns
  integer, parameter :: MAX_GLOBS = 64

  type :: grep_options
    !> Pattern type selection
    integer :: pattern_type = PATTERN_BRE

    !> Matching control
    logical :: ignore_case = .false.       ! -i, --ignore-case
    logical :: invert_match = .false.      ! -v, --invert-match
    logical :: word_regexp = .false.       ! -w, --word-regexp
    logical :: line_regexp = .false.       ! -x, --line-regexp

    !> Output control
    logical :: count_only = .false.        ! -c, --count
    logical :: files_with_matches = .false.    ! -l, --files-with-matches
    logical :: files_without_match = .false.   ! -L, --files-without-match
    logical :: only_matching = .false.     ! -o, --only-matching
    logical :: quiet = .false.             ! -q, --quiet, --silent
    logical :: no_messages = .false.       ! -s, --no-messages

    !> Line prefix control
    logical :: show_line_number = .false.  ! -n, --line-number
    logical :: show_byte_offset = .false.  ! -b, --byte-offset
    logical :: show_filename = .false.     ! -H, --with-filename
    logical :: hide_filename = .false.     ! -h, --no-filename
    logical :: null_after_filename = .false.   ! -Z, --null
    logical :: initial_tab = .false.       ! -T, --initial-tab

    !> Context control
    integer :: before_context = 0          ! -B, --before-context
    integer :: after_context = 0           ! -A, --after-context
    character(len=8) :: group_separator = '--'
    logical :: no_group_separator = .false.

    !> File selection
    logical :: recursive = .false.         ! -r, --recursive
    logical :: dereference_recursive = .false. ! -R, --dereference-recursive
    character(len=max_path_len) :: include_globs(MAX_GLOBS) = ''
    integer :: num_include_globs = 0
    character(len=max_path_len) :: exclude_globs(MAX_GLOBS) = ''
    integer :: num_exclude_globs = 0
    character(len=max_path_len) :: exclude_dirs(MAX_GLOBS) = ''
    integer :: num_exclude_dirs = 0
    character(len=max_path_len) :: exclude_from_file = ''
    character(len=max_path_len) :: include_from_file = ''

    !> Binary file handling
    logical :: text_mode = .false.         ! -a, --text
    logical :: ignore_binary = .false.     ! -I

    !> Misc
    integer :: color_mode = COLOR_AUTO     ! --color
    integer :: max_count = 0               ! -m, --max-count (0 = unlimited)
    character(len=max_path_len) :: label = '(standard input)'
    logical :: line_buffered = .false.     ! --line-buffered
    logical :: null_data = .false.         ! -z, --null-data
    integer :: dir_action = DIR_READ       ! -d, --directories
    integer :: dev_action = DEV_READ       ! -D, --devices

    !> Internal state
    logical :: multiple_files = .false.    ! Auto-set when >1 file
    logical :: reading_stdin = .false.     ! Auto-set when reading stdin
    integer :: line_number_width = 1       ! Width for -T padding (set per file)
  end type grep_options

end module ferp_options
