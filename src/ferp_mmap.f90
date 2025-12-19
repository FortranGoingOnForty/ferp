module ferp_mmap
  !> Memory-mapped file I/O for FERP
  !> Uses POSIX mmap for efficient file reading
  !> Uses SIMD for fast newline scanning on ARM64
  use ferp_kinds
  use ferp_simd
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: mmap_file_t
  public :: mmap_open, mmap_close, mmap_get_line
  public :: line_info_t, line_batch_t
  public :: BATCH_SIZE

  ! POSIX constants
  integer(c_int), parameter :: PROT_READ = 1
  integer(c_int), parameter :: MAP_PRIVATE = 2
  integer(c_int), parameter :: MAP_FAILED = -1

  ! Batch processing constants
  integer, parameter :: BATCH_SIZE = 256  ! Number of lines per batch

  !> Line info for batch processing (pointers into mmap'd memory)
  type :: line_info_t
    integer(c_size_t) :: start_pos = 0    ! Start position in mmap (0-based)
    integer(c_size_t) :: length = 0       ! Length of line (excluding newline)
    integer :: line_num = 0               ! Line number (1-based)
    integer(i64) :: byte_off = 0          ! Byte offset in file
  end type line_info_t

  !> Batch of line info for bulk processing
  type :: line_batch_t
    type(line_info_t) :: lines(BATCH_SIZE)
    integer :: count = 0                  ! Number of valid lines in batch
  end type line_batch_t

  ! C interfaces
  interface
    function c_open(pathname, flags) bind(C, name="open")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: pathname(*)
      integer(c_int), value :: flags
      integer(c_int) :: c_open
    end function c_open

    function c_close(fd) bind(C, name="close")
      import :: c_int
      integer(c_int), value :: fd
      integer(c_int) :: c_close
    end function c_close

    function c_mmap(addr, length, prot, flags, fd, offset) bind(C, name="mmap")
      import :: c_ptr, c_size_t, c_int, c_long
      type(c_ptr), value :: addr
      integer(c_size_t), value :: length
      integer(c_int), value :: prot
      integer(c_int), value :: flags
      integer(c_int), value :: fd
      integer(c_long), value :: offset
      type(c_ptr) :: c_mmap
    end function c_mmap

    function c_munmap(addr, length) bind(C, name="munmap")
      import :: c_ptr, c_size_t, c_int
      type(c_ptr), value :: addr
      integer(c_size_t), value :: length
      integer(c_int) :: c_munmap
    end function c_munmap

    function c_fstat(fd, statbuf) bind(C, name="fstat")
      import :: c_int, c_ptr
      integer(c_int), value :: fd
      type(c_ptr), value :: statbuf
      integer(c_int) :: c_fstat
    end function c_fstat

    function c_lseek(fd, offset, whence) bind(C, name="lseek")
      import :: c_int, c_long
      integer(c_int), value :: fd
      integer(c_long), value :: offset
      integer(c_int), value :: whence
      integer(c_long) :: c_lseek
    end function c_lseek
  end interface

  ! lseek whence values
  integer(c_int), parameter :: SEEK_SET = 0
  integer(c_int), parameter :: SEEK_END = 2

  !> Memory-mapped file type
  type :: mmap_file_t
    type(c_ptr) :: data = c_null_ptr
    integer(c_size_t) :: size = 0
    integer(c_size_t) :: pos = 0      ! Current position in file
    integer :: line_number = 0
    integer(i64) :: byte_offset = 0
    logical :: is_open = .false.
    character(len=max_path_len) :: filename = ''
  contains
    procedure :: open => mmap_open_method
    procedure :: close => mmap_close_method
    procedure :: read_line => mmap_read_line
    procedure :: read_lines_batch => mmap_read_lines_batch
    procedure :: get_line_text => mmap_get_line_text
    procedure :: reset => mmap_reset
  end type mmap_file_t

contains

  function mmap_open(filename, mfile) result(success)
    !> Open a file with memory mapping
    character(len=*), intent(in) :: filename
    type(mmap_file_t), intent(out) :: mfile
    logical :: success

    integer(c_int) :: fd, istat
    integer(c_int), parameter :: O_RDONLY = 0
    integer(c_size_t) :: file_size
    integer(c_long) :: size_long

    success = .false.
    mfile%is_open = .false.
    mfile%filename = filename

    ! Open file
    fd = c_open(trim(filename) // c_null_char, O_RDONLY)
    if (fd < 0) return

    ! Get file size via lseek to end
    size_long = c_lseek(fd, 0_c_long, SEEK_END)
    if (size_long < 0) then
      istat = c_close(fd)
      return
    end if
    file_size = int(size_long, c_size_t)

    ! Seek back to beginning
    size_long = c_lseek(fd, 0_c_long, SEEK_SET)

    if (file_size == 0) then
      istat = c_close(fd)
      mfile%size = 0
      mfile%is_open = .true.
      success = .true.
      return
    end if

    ! Memory map the file
    mfile%data = c_mmap(c_null_ptr, file_size, PROT_READ, MAP_PRIVATE, fd, 0_c_long)
    istat = c_close(fd)  ! Can close fd after mmap

    if (.not. c_associated(mfile%data)) return

    mfile%size = file_size
    mfile%pos = 0
    mfile%line_number = 0
    mfile%byte_offset = 0
    mfile%is_open = .true.
    success = .true.

  end function mmap_open

  function mmap_open_method(this, filename) result(success)
    class(mmap_file_t), intent(inout) :: this
    character(len=*), intent(in) :: filename
    logical :: success
    success = mmap_open(filename, this)
  end function mmap_open_method

  subroutine mmap_close(mfile)
    !> Close memory-mapped file
    type(mmap_file_t), intent(inout) :: mfile

    integer(c_int) :: istat

    if (mfile%is_open .and. c_associated(mfile%data)) then
      istat = c_munmap(mfile%data, mfile%size)
    end if

    mfile%data = c_null_ptr
    mfile%size = 0
    mfile%pos = 0
    mfile%is_open = .false.

  end subroutine mmap_close

  subroutine mmap_close_method(this)
    class(mmap_file_t), intent(inout) :: this
    call mmap_close(this)
  end subroutine mmap_close_method

  subroutine mmap_reset(this)
    !> Reset to beginning of file
    class(mmap_file_t), intent(inout) :: this
    this%pos = 0
    this%line_number = 0
    this%byte_offset = 0
  end subroutine mmap_reset

  function mmap_get_line(mfile, line, line_num, byte_off) result(success)
    !> Get next line from memory-mapped file
    type(mmap_file_t), intent(inout) :: mfile
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: line_num
    integer(i64), intent(out) :: byte_off
    logical :: success

    success = mfile%read_line(line, line_num, byte_off)
  end function mmap_get_line

  function mmap_read_line(this, line, line_num, byte_off) result(success)
    !> Read next line from memory-mapped file (SIMD-accelerated newline scanning)
    class(mmap_file_t), intent(inout) :: this
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: line_num
    integer(i64), intent(out) :: byte_off
    logical :: success

    character(len=1, kind=c_char), pointer :: file_data(:)
    integer(c_size_t) :: start_pos, end_pos, line_len
    integer(c_int64_t) :: newline_pos
    integer :: i

    success = .false.
    line_num = 0
    byte_off = 0
    if (allocated(line)) deallocate(line)

    if (.not. this%is_open) return
    if (this%pos >= this%size) return

    ! Map the C pointer to a Fortran character array
    call c_f_pointer(this%data, file_data, [this%size])

    ! Find start and end of line
    start_pos = this%pos + 1  ! 1-based for Fortran

    ! Use SIMD to find newline (16 bytes at a time on ARM64)
    newline_pos = simd_find_char_ptr(this%data, int(this%size, c_int64_t), &
                                      int(this%pos, c_int64_t), char(10))

    if (newline_pos < 0) then
      ! No newline found - rest of file is the line
      end_pos = this%size + 1
    else
      ! Found newline - convert from 0-indexed to 1-indexed
      end_pos = int(newline_pos, c_size_t) + 1
    end if

    ! Calculate line length (excluding newline)
    line_len = end_pos - start_pos
    if (line_len > 0 .and. end_pos > start_pos) then
      ! Check for CR before LF (Windows line ending)
      if (file_data(end_pos - 1) == char(13)) then
        line_len = line_len - 1
      end if
    end if

    ! Allocate and copy line
    if (line_len > 0) then
      allocate(character(len=line_len) :: line)
      do i = 1, int(line_len)
        line(i:i) = file_data(start_pos + i - 1)
      end do
    else
      line = ''
    end if

    ! Update state
    this%line_number = this%line_number + 1
    line_num = this%line_number
    byte_off = int(this%pos, i64)

    ! Move past the newline
    if (end_pos <= this%size) then
      this%pos = end_pos  ! Position after newline (0-based)
    else
      this%pos = this%size
    end if
    this%byte_offset = int(this%pos, i64)

    success = .true.

  end function mmap_read_line

  function mmap_read_lines_batch(this, batch) result(success)
    !> Read up to BATCH_SIZE lines from memory-mapped file
    !> Returns line positions without copying data (zero-copy batch read)
    class(mmap_file_t), intent(inout) :: this
    type(line_batch_t), intent(out) :: batch
    logical :: success

    character(len=1, kind=c_char), pointer :: file_data(:)
    integer(c_size_t) :: start_pos, end_pos, line_len
    integer(c_int64_t) :: newline_pos
    integer :: i

    success = .false.
    batch%count = 0

    if (.not. this%is_open) return
    if (this%pos >= this%size) return
    if (.not. c_associated(this%data)) return

    ! Map the C pointer to a Fortran character array
    call c_f_pointer(this%data, file_data, [this%size])

    ! Read up to BATCH_SIZE lines
    do i = 1, BATCH_SIZE
      if (this%pos >= this%size) exit

      start_pos = this%pos  ! 0-based position

      ! Use SIMD to find newline
      newline_pos = simd_find_char_ptr(this%data, int(this%size, c_int64_t), &
                                        int(this%pos, c_int64_t), char(10))

      if (newline_pos < 0) then
        ! No newline found - rest of file is the line
        end_pos = this%size
      else
        end_pos = int(newline_pos, c_size_t)
      end if

      ! Calculate line length (excluding newline and CR)
      line_len = end_pos - start_pos
      if (line_len > 0 .and. end_pos > start_pos) then
        ! Check for CR before LF (Windows line ending)
        if (file_data(end_pos) == char(13)) then
          line_len = line_len - 1
        end if
      end if

      ! Store line info
      batch%count = batch%count + 1
      batch%lines(batch%count)%start_pos = start_pos
      batch%lines(batch%count)%length = line_len
      this%line_number = this%line_number + 1
      batch%lines(batch%count)%line_num = this%line_number
      batch%lines(batch%count)%byte_off = int(start_pos, i64)

      ! Move past the newline
      if (end_pos < this%size) then
        this%pos = end_pos + 1  ! Position after newline (0-based)
      else
        this%pos = this%size
      end if
      this%byte_offset = int(this%pos, i64)
    end do

    success = (batch%count > 0)

  end function mmap_read_lines_batch

  function mmap_get_line_text(this, info) result(line)
    !> Extract line text from mmap'd memory given line info
    class(mmap_file_t), intent(in) :: this
    type(line_info_t), intent(in) :: info
    character(len=:), allocatable :: line

    character(len=1, kind=c_char), pointer :: file_data(:)
    integer :: i

    if (.not. this%is_open .or. .not. c_associated(this%data)) then
      line = ''
      return
    end if

    if (info%length == 0) then
      line = ''
      return
    end if

    ! Map the C pointer to a Fortran character array
    call c_f_pointer(this%data, file_data, [this%size])

    ! Allocate and copy line (start_pos is 0-based, array is 1-based)
    allocate(character(len=info%length) :: line)
    do i = 1, int(info%length)
      line(i:i) = file_data(info%start_pos + i)
    end do

  end function mmap_get_line_text

end module ferp_mmap
