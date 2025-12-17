module ferp_io
  !> File I/O handling for FERP
  use ferp_kinds
  use, intrinsic :: iso_fortran_env, only: input_unit, error_unit, iostat_end
  implicit none
  private

  public :: input_source
  public :: SOURCE_STDIN, SOURCE_FILE

  integer, parameter :: SOURCE_STDIN = 1
  integer, parameter :: SOURCE_FILE = 2

  type :: input_source
    integer :: source_type = SOURCE_STDIN
    integer :: unit_num = input_unit
    character(len=max_path_len) :: filename = '(standard input)'
    logical :: is_open = .false.
    integer(i64) :: byte_offset = 0
    integer :: line_number = 0
    logical :: is_binary = .false.
    logical :: eof_reached = .false.
  contains
    procedure :: open => source_open
    procedure :: close => source_close
    procedure :: read_line => source_read_line
    procedure :: check_binary => source_check_binary
  end type input_source

contains

  function source_open(this, filename, suppress_errors) result(success)
    !> Open a file or stdin for reading
    class(input_source), intent(inout) :: this
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: suppress_errors
    logical :: success

    integer :: ios
    character(len=256) :: errmsg
    logical :: quiet

    quiet = .false.
    if (present(suppress_errors)) quiet = suppress_errors

    success = .false.

    ! Reset state
    this%byte_offset = 0
    this%line_number = 0
    this%is_binary = .false.
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

    this%source_type = SOURCE_FILE
    this%filename = filename

    ! Open file
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

    if (this%is_open .and. this%source_type == SOURCE_FILE) then
      close(this%unit_num)
    end if

    this%is_open = .false.
  end subroutine source_close

  function source_read_line(this, line, line_num, byte_off) result(success)
    !> Read a line from the input source
    class(input_source), intent(inout) :: this
    character(len=*), intent(out) :: line
    integer, intent(out) :: line_num
    integer(i64), intent(out) :: byte_off
    logical :: success

    integer :: ios
    integer :: line_len

    success = .false.
    line = ''
    line_num = 0
    byte_off = 0

    if (.not. this%is_open .or. this%eof_reached) return

    read(this%unit_num, '(A)', iostat=ios) line

    if (ios == iostat_end) then
      this%eof_reached = .true.
      return
    end if

    if (ios /= 0) then
      ! Read error
      return
    end if

    ! Update state
    this%line_number = this%line_number + 1
    line_num = this%line_number
    byte_off = this%byte_offset

    ! Update byte offset (line length + newline)
    line_len = len_trim(line)
    this%byte_offset = this%byte_offset + int(line_len, i64) + 1_i64

    success = .true.

  end function source_read_line

  subroutine source_check_binary(this)
    !> Check if the file is binary by looking for NUL bytes
    class(input_source), intent(inout) :: this

    character(len=512) :: buffer
    integer :: ios, i, check_unit
    logical :: file_exists

    this%is_binary = .false.

    if (this%source_type == SOURCE_STDIN) return

    ! Open file in stream mode to check for binary content
    inquire(file=this%filename, exist=file_exists)
    if (.not. file_exists) return

    open(newunit=check_unit, file=this%filename, status='old', action='read', &
         access='stream', form='unformatted', iostat=ios)
    if (ios /= 0) return

    read(check_unit, iostat=ios) buffer
    close(check_unit)

    if (ios /= 0 .and. ios /= iostat_end) return

    ! Check for NUL bytes
    do i = 1, len_trim(buffer)
      if (ichar(buffer(i:i)) == 0) then
        this%is_binary = .true.
        return
      end if
    end do

  end subroutine source_check_binary

end module ferp_io
