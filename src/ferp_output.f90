module ferp_output
  !> Output formatting for FERP
  !>
  !> Every routine here funnels its bytes through sink(), which either writes
  !> straight to stdout or, while a capture is active, appends to a per-thread
  !> buffer. That lets main.f90 search files in parallel and still emit whole
  !> files in command-line order, the way grep does. See begin_capture().
  use ferp_kinds
  use ferp_options
  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
  use, intrinsic :: iso_c_binding, only: c_int
#ifdef _OPENMP
  use omp_lib
#endif
  implicit none
  private

  public :: print_match, print_count, print_filename
  public :: print_context_line, print_separator
  public :: print_binary_match, print_only_match
  public :: print_match_colored
  public :: stdout_is_tty
  public :: init_capture_slots, begin_capture, end_capture, emit_raw

  ! C interface for isatty
  interface
    function c_isatty(fd) bind(C, name="isatty")
      import :: c_int
      integer(c_int), value :: fd
      integer(c_int) :: c_isatty
    end function c_isatty
  end interface

  ! ANSI color codes
  character(len=*), parameter :: COLOR_MATCH = char(27) // '[01;31m'  ! Bold red
  character(len=*), parameter :: COLOR_RESET = char(27) // '[0m'
  character(len=*), parameter :: COLOR_FILENAME = char(27) // '[35m'  ! Magenta
  character(len=*), parameter :: COLOR_LINENUM = char(27) // '[32m'   ! Green
  character(len=*), parameter :: COLOR_SEP = char(27) // '[36m'       ! Cyan

  character(len=*), parameter :: NL = char(10)
  character(len=*), parameter :: NUL = char(0)
  character(len=*), parameter :: TAB = char(9)

  integer, parameter :: CAP_INIT = 65536

  !> One capture slot per thread. Deliberately a shared array indexed by thread
  !> number rather than an !$omp threadprivate buffer: gfortran does not keep
  !> the length parameter of a deferred-length character threadprivate, which
  !> silently truncates one thread's buffer using another thread's length.
  !> Each thread only ever touches caps(slot()), so the array needs no lock.
  type :: capture_t
    character(len=:), allocatable :: buf
    integer :: used = 0
    logical :: active = .false.
  end type capture_t

  type(capture_t), allocatable :: caps(:)

contains

  function stdout_is_tty() result(is_tty)
    !> Check if stdout is a terminal (for --color=auto)
    logical :: is_tty
    integer(c_int), parameter :: STDOUT_FILENO = 1

    is_tty = (c_isatty(STDOUT_FILENO) /= 0)
  end function stdout_is_tty

  !---------------------------------------------------------------------------
  ! Capture control
  !---------------------------------------------------------------------------

  function slot() result(k)
    !> Index of the calling thread's capture slot
    integer :: k
#ifdef _OPENMP
    k = omp_get_thread_num() + 1
#else
    k = 1
#endif
  end function slot

  subroutine init_capture_slots()
    !> Allocate one slot per thread. Must be called outside any parallel
    !> region, before the first begin_capture.
    integer :: n

#ifdef _OPENMP
    n = omp_get_max_threads()
#else
    n = 1
#endif
    if (allocated(caps)) deallocate(caps)
    allocate(caps(n))
  end subroutine init_capture_slots

  subroutine begin_capture()
    !> Redirect this thread's output into its own slot until end_capture.
    !> Callers must pair the two within a single loop iteration.
    integer :: k

    k = slot()
    caps(k)%active = .true.
    caps(k)%used = 0
    if (.not. allocated(caps(k)%buf)) allocate(character(len=CAP_INIT) :: caps(k)%buf)
  end subroutine begin_capture

  subroutine end_capture(out)
    !> Stop capturing and hand back everything buffered since begin_capture.
    character(len=:), allocatable, intent(out) :: out

    integer :: k

    k = slot()
    caps(k)%active = .false.
    out = caps(k)%buf(1:caps(k)%used)
    caps(k)%used = 0
  end subroutine end_capture

  subroutine emit_raw(s)
    !> Write a previously captured buffer verbatim. Used by the serial
    !> drain after a parallel search, so it takes no lock.
    character(len=*), intent(in) :: s

    if (len(s) > 0) write(output_unit, '(A)', advance='no') s
  end subroutine emit_raw

  subroutine sink(s, opts)
    !> Single exit point for all output bytes.
    character(len=*), intent(in) :: s
    type(grep_options), intent(in) :: opts

    character(len=:), allocatable :: bigger
    integer :: need, k

    k = 0
    if (allocated(caps)) then
      k = slot()
      if (.not. caps(k)%active) k = 0
    end if

    if (k > 0) then
      need = caps(k)%used + len(s)
      if (need > len(caps(k)%buf)) then
        allocate(character(len=max(need, 2 * len(caps(k)%buf))) :: bigger)
        bigger(1:caps(k)%used) = caps(k)%buf(1:caps(k)%used)
        call move_alloc(bigger, caps(k)%buf)
      end if
      caps(k)%buf(caps(k)%used+1:need) = s
      caps(k)%used = need
    else
      !$omp critical(output_lock)
      write(output_unit, '(A)', advance='no') s
      if (opts%line_buffered) flush(output_unit)
      !$omp end critical(output_lock)
    end if
  end subroutine sink

  !---------------------------------------------------------------------------
  ! Integer formatting helpers
  !---------------------------------------------------------------------------

  function itoa(n) result(s)
    !> Left-justified integer, equivalent to the I0 edit descriptor
    integer, intent(in) :: n
    character(len=:), allocatable :: s
    character(len=32) :: buf

    write(buf, '(I0)') n
    s = trim(buf)
  end function itoa

  function i64toa(n) result(s)
    !> Left-justified 64-bit integer
    integer(i64), intent(in) :: n
    character(len=:), allocatable :: s
    character(len=32) :: buf

    write(buf, '(I0)') n
    s = trim(buf)
  end function i64toa

  function itoa_width(n, width) result(s)
    !> Right-justified integer in a fixed field, as Iw would render it
    integer, intent(in) :: n, width
    character(len=:), allocatable :: s
    character(len=64) :: buf, fmt

    if (width < 1 .or. width > 60) then
      s = itoa(n)
      return
    end if

    write(fmt, '(A,I0,A)') '(I', width, ')'
    write(buf, fmt) n
    s = buf(1:width)
  end function itoa_width

  !---------------------------------------------------------------------------
  ! Shared prefix builders
  !---------------------------------------------------------------------------

  function has_prefix(opts) result(res)
    !> True when any of the filename/line/offset prefixes will be emitted,
    !> which is what -T keys its alignment tab off of
    type(grep_options), intent(in) :: opts
    logical :: res

    res = (opts%show_filename .and. .not. opts%hide_filename) .or. &
          opts%show_line_number .or. opts%show_byte_offset
  end function has_prefix

  function line_prefix(filename, line_num, byte_off, opts, sep) result(s)
    !> Build the "file:line:offset:" prefix shared by the plain output paths.
    !> sep is ':' for matches and '-' for context lines.
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts
    character(len=1), intent(in) :: sep
    character(len=:), allocatable :: s

    s = ''

    if (opts%show_filename .and. .not. opts%hide_filename) then
      if (opts%null_after_filename) then
        s = s // trim(filename) // NUL
      else
        s = s // trim(filename) // sep
      end if
    end if

    if (opts%show_line_number) then
      if (opts%initial_tab .and. opts%line_number_width > 1) then
        s = s // itoa_width(line_num, opts%line_number_width) // sep
      else
        s = s // itoa(line_num) // sep
      end if
    end if

    if (opts%show_byte_offset) then
      s = s // i64toa(byte_off) // sep
    end if

    if (opts%initial_tab .and. has_prefix(opts)) then
      s = s // TAB
    end if
  end function line_prefix

  function body_terminator(opts) result(s)
    !> -z terminates output records with NUL instead of a newline
    type(grep_options), intent(in) :: opts
    character(len=:), allocatable :: s

    if (opts%null_data) then
      s = NUL
    else
      s = NL
    end if
  end function body_terminator

  !---------------------------------------------------------------------------
  ! Output routines
  !---------------------------------------------------------------------------

  subroutine print_match(line, filename, line_num, byte_off, opts)
    !> Print a matching line with appropriate prefixes
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    call sink(line_prefix(filename, line_num, byte_off, opts, ':') // &
              line // body_terminator(opts), opts)
  end subroutine print_match

  subroutine print_context_line(line, filename, line_num, byte_off, opts)
    !> Print a context line (uses - instead of : as separator)
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    call sink(line_prefix(filename, line_num, byte_off, opts, '-') // &
              line // body_terminator(opts), opts)
  end subroutine print_context_line

  subroutine print_separator(opts)
    !> Print group separator between context groups
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return
    if (opts%no_group_separator) return

    call sink(trim(opts%group_separator) // NL, opts)
  end subroutine print_separator

  subroutine print_count(count, filename, opts)
    !> Print match count (for -c option)
    integer, intent(in) :: count
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    if (opts%show_filename .and. .not. opts%hide_filename) then
      if (opts%null_after_filename) then
        call sink(trim(filename) // NUL // itoa(count) // NL, opts)
      else
        call sink(trim(filename) // ':' // itoa(count) // NL, opts)
      end if
    else
      call sink(itoa(count) // NL, opts)
    end if
  end subroutine print_count

  subroutine print_filename(filename, opts)
    !> Print just filename (for -l, -L options)
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    if (opts%null_after_filename) then
      call sink(trim(filename) // NUL, opts)
    else
      call sink(trim(filename) // NL, opts)
    end if
  end subroutine print_filename

  subroutine print_binary_match(filename, opts)
    !> Print binary file match message
    character(len=*), intent(in) :: filename
    type(grep_options), intent(in) :: opts

    if (opts%quiet) return

    call sink('Binary file ' // trim(filename) // ' matches' // NL, opts)
  end subroutine print_binary_match

  subroutine print_only_match(line, match_start, match_end, filename, line_num, byte_off, opts)
    !> Print only the matched portion of a line (for -o option)
    character(len=*), intent(in) :: line
    integer, intent(in) :: match_start, match_end
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts

    character(len=:), allocatable :: out

    if (opts%quiet) return

    ! -b reports the offset of the match itself, not of the line
    out = line_prefix(filename, line_num, byte_off + match_start - 1, opts, ':')

    if (match_start >= 1 .and. match_end >= match_start .and. match_end <= len(line)) then
      out = out // line(match_start:match_end) // body_terminator(opts)
    end if

    call sink(out, opts)
  end subroutine print_only_match

  subroutine print_match_colored(line, filename, line_num, byte_off, opts, &
                                  match_starts, match_ends, num_matches)
    !> Print a matching line with colored highlighting of matches
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts
    integer, intent(in) :: match_starts(:), match_ends(:)
    integer, intent(in) :: num_matches

    integer :: i, pos, line_len
    logical :: use_color
    character(len=:), allocatable :: out

    if (opts%quiet) return

    ! Determine if we should use color
    use_color = .false.
    if (opts%color_mode == COLOR_ALWAYS) then
      use_color = .true.
    else if (opts%color_mode == COLOR_AUTO) then
      use_color = stdout_is_tty()
    end if

    if (.not. use_color) then
      ! Identical to print_match once color is off
      call sink(line_prefix(filename, line_num, byte_off, opts, ':') // &
                line // body_terminator(opts), opts)
      return
    end if

    out = colored_prefix(filename, line_num, byte_off, opts)

    if (num_matches > 0) then
      line_len = len_trim(line)
      pos = 1
      do i = 1, num_matches
        ! Text before the match
        if (match_starts(i) > pos) then
          out = out // line(pos:match_starts(i)-1)
        end if
        ! The match itself, highlighted
        if (match_starts(i) >= 1 .and. match_ends(i) <= line_len) then
          out = out // COLOR_MATCH // line(match_starts(i):match_ends(i)) // COLOR_RESET
        end if
        pos = match_ends(i) + 1
      end do
      ! Remainder of the line after the last match
      if (pos <= line_len) then
        out = out // line(pos:line_len)
      end if
      out = out // body_terminator(opts)
    else
      out = out // line // body_terminator(opts)
    end if

    call sink(out, opts)
  end subroutine print_match_colored

  function colored_prefix(filename, line_num, byte_off, opts) result(s)
    !> Build the "file:line:offset:" prefix with ANSI colors applied
    character(len=*), intent(in) :: filename
    integer, intent(in) :: line_num
    integer(i64), intent(in) :: byte_off
    type(grep_options), intent(in) :: opts
    character(len=:), allocatable :: s

    s = ''

    if (opts%show_filename .and. .not. opts%hide_filename) then
      s = s // COLOR_FILENAME
      if (opts%null_after_filename) then
        s = s // trim(filename) // NUL // COLOR_RESET
      else
        s = s // trim(filename) // COLOR_RESET // COLOR_SEP // ':' // COLOR_RESET
      end if
    end if

    if (opts%show_line_number) then
      s = s // COLOR_LINENUM
      if (opts%initial_tab .and. opts%line_number_width > 1) then
        s = s // itoa_width(line_num, opts%line_number_width)
      else
        s = s // itoa(line_num)
      end if
      s = s // COLOR_RESET // COLOR_SEP // ':' // COLOR_RESET
    end if

    if (opts%show_byte_offset) then
      s = s // COLOR_LINENUM // i64toa(byte_off) // COLOR_RESET // &
          COLOR_SEP // ':' // COLOR_RESET
    end if

    if (opts%initial_tab .and. has_prefix(opts)) then
      s = s // TAB
    end if
  end function colored_prefix

end module ferp_output
