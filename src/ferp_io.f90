module ferp_io
  !> File I/O handling for FERP
  !> Supports dynamic line length (no fixed limit)
  !> Uses memory-mapped I/O for improved performance on files
  use ferp_kinds
  use ferp_mmap
  use, intrinsic :: iso_fortran_env, only: input_unit, error_unit, iostat_end, iostat_eor
  implicit none
  private

  public :: input_source
  public :: SOURCE_STDIN, SOURCE_FILE, SOURCE_MMAP
  public :: check_binary_file
  ! Re-export batch types from ferp_mmap
  public :: line_info_t, line_batch_t, BATCH_SIZE

  integer, parameter :: SOURCE_STDIN = 1
  integer, parameter :: SOURCE_FILE = 2
  integer, parameter :: SOURCE_MMAP = 3

  type :: input_source
    integer :: source_type = SOURCE_STDIN
    integer :: unit_num = input_unit
    character(len=max_path_len) :: filename = '(standard input)'
    logical :: is_open = .false.
    integer(i64) :: byte_offset = 0
    integer :: line_number = 0
    logical :: is_binary = .false.
    logical :: eof_reached = .false.
    logical :: null_data_mode = .false.
    type(mmap_file_t) :: mmap_file  ! Memory-mapped file handle
  contains
    procedure :: open => source_open
    procedure :: close => source_close
    procedure :: read_line_dynamic => source_read_line_dynamic
    procedure :: read_line_null_dynamic => source_read_line_null_dynamic
    procedure :: read_lines_batch => source_read_lines_batch
    procedure :: get_line_text => source_get_line_text
    procedure :: check_binary => source_check_binary
  end type input_source

contains

  function source_open(this, filename, suppress_errors, null_data) result(success)
    !> Open a file or stdin for reading
    class(input_source), intent(inout) :: this
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: suppress_errors
    logical, intent(in), optional :: null_data
    logical :: success

    integer :: ios
    character(len=256) :: errmsg
    logical :: quiet

    quiet = .false.
    if (present(suppress_errors)) quiet = suppress_errors

    this%null_data_mode = .false.
    if (present(null_data)) this%null_data_mode = null_data

    success = .false.

    ! Reset state (but preserve is_binary if already set by caller)
    this%byte_offset = 0
    this%line_number = 0
    this%eof_reached = .false.

    ! Handle stdin
    if (filename == '-' .or. len_trim(filename) == 0) then
      this%source_type = SOURCE_STDIN
      this%unit_num = input_unit
      this%filename = '(standard input)'
      this%is_open = .true.
      success = .true.
      return
    end if

    this%filename = filename

    ! For null-data mode, use stream access (can't use mmap easily)
    if (this%null_data_mode) then
      this%source_type = SOURCE_FILE
      open(newunit=this%unit_num, file=filename, status='old', action='read', &
           access='stream', form='unformatted', iostat=ios, iomsg=errmsg)
      if (ios /= 0) then
        if (.not. quiet) then
          write(error_unit, '(A)') 'ferp: ' // trim(filename) // ': ' // trim(errmsg)
        end if
        return
      end if
      this%is_open = .true.
      success = .true.
      return
    end if

    ! Try memory-mapped I/O first (fastest for regular files)
    if (this%mmap_file%open(filename)) then
      this%source_type = SOURCE_MMAP
      this%is_open = .true.
      success = .true.
      return
    end if

    ! Fall back to standard Fortran I/O
    this%source_type = SOURCE_FILE
    open(newunit=this%unit_num, file=filename, status='old', action='read', &
         iostat=ios, iomsg=errmsg)

    if (ios /= 0) then
      if (.not. quiet) then
        write(error_unit, '(A)') 'ferp: ' // trim(filename) // ': ' // trim(errmsg)
      end if
      return
    end if

    this%is_open = .true.
    success = .true.

  end function source_open

  subroutine source_close(this)
    !> Close the input source
    class(input_source), intent(inout) :: this

    if (this%is_open) then
      if (this%source_type == SOURCE_FILE) then
        close(this%unit_num)
      else if (this%source_type == SOURCE_MMAP) then
        call this%mmap_file%close()
      end if
    end if

    this%is_open = .false.
  end subroutine source_close

  function source_read_line_dynamic(this, line, line_num, byte_off) result(success)
    !> Read a line from the input source with dynamic allocation
    !> Uses mmap for files, standard I/O for stdin
    class(input_source), intent(inout) :: this
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: line_num
    integer(i64), intent(out) :: byte_off
    logical :: success

    integer :: ios
    integer :: line_len
    ! Use a generous fixed buffer for reading (64KB handles most lines)
    integer, parameter :: READ_BUFFER_SIZE = 65536
    character(len=READ_BUFFER_SIZE) :: buffer

    success = .false.
    line_num = 0
    byte_off = 0
    if (allocated(line)) deallocate(line)

    if (.not. this%is_open .or. this%eof_reached) return

    ! Use mmap for memory-mapped files (fastest path)
    if (this%source_type == SOURCE_MMAP) then
      success = this%mmap_file%read_line(line, line_num, byte_off)
      if (.not. success) this%eof_reached = .true.
      return
    end if

    ! Standard Fortran I/O for stdin and fallback
    read(this%unit_num, '(A)', iostat=ios) buffer

    if (ios == iostat_end) then
      this%eof_reached = .true.
      return
    end if

    if (ios /= 0) then
      ! Read error
      return
    end if

    ! Strip trailing carriage return for Windows line endings (\r\n)
    line_len = len_trim(buffer)
    if (line_len > 0) then
      if (buffer(line_len:line_len) == char(13)) then
        line_len = line_len - 1
      end if
    end if

    ! Allocate result string trimmed to actual length
    if (line_len > 0) then
      line = buffer(1:line_len)
    else
      line = ''
    end if

    ! Update state
    this%line_number = this%line_number + 1
    line_num = this%line_number
    byte_off = this%byte_offset

    ! Update byte offset (line length + newline)
    this%byte_offset = this%byte_offset + int(line_len, i64) + 1_i64

    success = .true.

  end function source_read_line_dynamic

  function source_read_line_null_dynamic(this, line, line_num, byte_off) result(success)
    !> Read a NUL-terminated line from the input source (for -z mode)
    !> Line buffer grows automatically to accommodate any line length
    class(input_source), intent(inout) :: this
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: line_num
    integer(i64), intent(out) :: byte_off
    logical :: success

    integer :: ios, pos, capacity
    character(len=1) :: ch
    character(len=:), allocatable :: new_buf

    success = .false.
    line_num = 0
    byte_off = 0
    if (allocated(line)) deallocate(line)

    if (.not. this%is_open .or. this%eof_reached) return

    ! Start with initial buffer
    capacity = initial_line_len
    allocate(character(len=capacity) :: line)
    pos = 0

    ! Read byte by byte until NUL or EOF
    do
      read(this%unit_num, iostat=ios) ch

      if (ios == iostat_end) then
        this%eof_reached = .true.
        if (pos > 0) then
          exit  ! Return what we have
        else
          deallocate(line)
          return
        end if
      end if

      if (ios /= 0) then
        deallocate(line)
        return
      end if

      ! Check for NUL terminator
      if (ch == char(0)) exit

      ! Skip carriage returns (for Windows line endings in data)
      if (ch == char(13)) cycle

      ! Convert embedded newlines to space
      if (ch == char(10)) ch = ' '

      ! Grow buffer if needed
      if (pos >= capacity) then
        capacity = capacity * 2
        allocate(character(len=capacity) :: new_buf)
        if (pos > 0) new_buf(1:pos) = line(1:pos)
        call move_alloc(new_buf, line)
      end if

      ! Add character to line
      pos = pos + 1
      line(pos:pos) = ch
    end do

    ! Trim to actual length
    if (pos > 0) then
      new_buf = line(1:pos)
      call move_alloc(new_buf, line)
    else
      line = ''
    end if

    ! Update state
    this%line_number = this%line_number + 1
    line_num = this%line_number
    byte_off = this%byte_offset

    ! Update byte offset (record length + NUL)
    this%byte_offset = this%byte_offset + int(len(line), i64) + 1_i64

    success = .true.

  end function source_read_line_null_dynamic

  function check_binary_file(filename) result(is_binary)
    !> Check if a file is binary by looking for NUL bytes or non-text chars
    !> This must be called BEFORE the file is opened for reading
    character(len=*), intent(in) :: filename
    logical :: is_binary

    integer, parameter :: CHECK_SIZE = 8192
    character(len=CHECK_SIZE) :: buffer
    integer :: ios, i, check_unit, bytes_read
    integer :: char_code
    logical :: file_exists

    is_binary = .false.

    ! Open file in stream mode to check for binary content
    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) return

    open(newunit=check_unit, file=filename, status='old', action='read', &
         access='stream', form='unformatted', iostat=ios)
    if (ios /= 0) return

    ! Initialize buffer to spaces
    buffer = ''

    read(check_unit, iostat=ios) buffer
    close(check_unit)

    ! Determine how many bytes were actually read
    bytes_read = CHECK_SIZE
    if (ios == iostat_end) then
      ! File was smaller than buffer - find actual length
      do i = CHECK_SIZE, 1, -1
        if (buffer(i:i) /= char(0)) then
          bytes_read = i
          exit
        end if
      end do
    else if (ios /= 0) then
      return
    end if

    ! Check each byte for binary indicators
    do i = 1, bytes_read
      char_code = ichar(buffer(i:i))

      ! NUL byte is definitive binary indicator
      if (char_code == 0) then
        is_binary = .true.
        return
      end if

      ! Non-printable control chars (except common text ones)
      ! Allow: tab (9), newline (10), carriage return (13), form feed (12)
      if (char_code < 32 .and. char_code /= 9 .and. char_code /= 10 &
          .and. char_code /= 13 .and. char_code /= 12) then
        is_binary = .true.
        return
      end if
    end do

  end function check_binary_file

  subroutine source_check_binary(this)
    !> Check if the source is binary (wrapper that calls check_binary_file)
    !> NOTE: This only works if called BEFORE the file is opened
    class(input_source), intent(inout) :: this

    this%is_binary = .false.
    if (this%source_type == SOURCE_STDIN) return

    this%is_binary = check_binary_file(trim(this%filename))

  end subroutine source_check_binary

  function source_read_lines_batch(this, batch) result(success)
    !> Read multiple lines as a batch (wrapper for mmap batch read)
    !> Only works for mmap sources; returns false for stdin/file
    class(input_source), intent(inout) :: this
    type(line_batch_t), intent(out) :: batch
    logical :: success

    success = .false.
    batch%count = 0

    if (.not. this%is_open .or. this%eof_reached) return

    ! Only mmap sources support batch reading
    if (this%source_type == SOURCE_MMAP) then
      success = this%mmap_file%read_lines_batch(batch)
      if (.not. success) this%eof_reached = .true.
    end if

  end function source_read_lines_batch

  function source_get_line_text(this, info) result(line)
    !> Get line text from mmap given line info (wrapper)
    class(input_source), intent(in) :: this
    type(line_info_t), intent(in) :: info
    character(len=:), allocatable :: line

    if (this%source_type == SOURCE_MMAP) then
      line = this%mmap_file%get_line_text(info)
    else
      line = ''
    end if

  end function source_get_line_text

end module ferp_io
