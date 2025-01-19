module GridFileReaderModule

  use KindModule
  use SimModule, only: store_error, store_error_unit
  use ConstantsModule, only: LINELENGTH
  use BaseDisModule, only: DisBaseType
  use DisModule, only: DisType
  use DisvModule, only: DisvType
  use DisuModule, only: DisuType
  use InputOutputModule, only: urword, read_line
  use LongLineReaderModule, only: LongLineReaderType

  implicit none

  private
  public :: GridFileReaderType

  type :: GridFileReaderType
    integer(I4B), public :: inunit
    ! character(len=:), public, allocatable :: grid_type
    ! character(len=:), public, allocatable :: version
    ! integer(I4B), public :: ntxt
    ! integer(I4B), public :: lentxt
    type(LongLineReaderType) :: line_reader
  contains
    procedure, public :: initialize
    procedure, public :: load_grb
    procedure, public :: finalize
    procedure :: load_dis
    procedure :: load_disv
    procedure :: load_disu
  end type GridFileReaderType

  contains

  !< @brief Initialize the grid file reader.
  subroutine initialize(this, iu)
    class(GridFileReaderType) :: this
    integer(I4B), intent(in) :: iu
    this%inunit = iu
  end subroutine initialize

  !> @brief Load a discretization from a binary grid file.
  function load_grb(inunit, iout) result(dis)
    ! dummy
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in), optional :: iout
    class(DisBaseType), pointer :: dis
    ! local
    character(len=:), allocatable :: line
    character(len=10) :: grid, key, dtype
    real(DP) :: rval
    integer(I4B) :: liout, lloc, istart, istop, ival, d
    logical(LGP) :: end_of_header
    integer(I4B), allocatable :: shp(:)

    if (present(iout)) then
      liout = iout
    else
      liout = 0
    end if

    ! grid type
    call this%line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    grid = line(istart:istop)
    call upcase(grid)

    ! version
    call this%line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    version = ival

    ! ntxt
    call this%line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    ntxt = ival

    ! lentxt
    call this%line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    lentxt = ival

    ! ncells
    call this%line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    ncells = ival
    
    ! load grid-specific data
    select case (grid)
    case ('DIS')
        dis => load_dis()
    case ('DISV')
        dis => load_disv()
    case ('DISU')
        dis => load_disu()
    end select

    ! rewind file
    rewind (this%inunit)
  end function load_grb

  function load_dis()

  end function load_dis

  function load_disv()
    ! remaining lines
    do
      call this%line_reader%rdcom(inunit, liout, line, ierr)
      lloc = 1

      ! data type
      call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
      dtype = line(istart:istop)
      call upcase(dtype)
      if (dtype == "INTEGER") then
        ncode = 2
      else if (dtype == "DOUBLE") then
        ncode = 3
      end if

      ! dimensions
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
      ndim = ival

      ! shape or scalar value
      if (ndim == 0) then
        call urword(line, lloc, istart, istop, ncode, ival, rval, 0, 0)
        select case (key)
        case ('NCELLS')
          ncells = ival
        case ('NLAY')
          nlay = ival
        case ('NROW')
          nrow = ival
        case ('NCOL')
          ncol = ival
        case ('NCPL')
          ncpl = ival
        case ('NVERT')
          nvert = ival
        case ('NJAVERT')
          njavert = ival
        case ('NJA')
          nja = ival
        case ('XORIGIN')
          xorigin = rval
        case ('YORIGIN')
          yorigin = rval
        case ('ANGROT')
          angrot = rval
        end select
      else
        allocate (shp(ndim))
        do d = 1, ndim
          call urword(line, lloc, istart, istop, ncode, ival, rval, 0, 0)
          shp(d) = ival
        end do
      end if

      ! last text key, rest is binary
      if (key == 'ICELLTYPE') exit
    end do

    ! TODO binary data
  end function load_disv

  function load_disu()

  end function load_disu

  !> @brief Finalize the grid file reader.
  subroutine finalize(this)
    class(GridFileReaderType) :: this
    close (this%inunit)
  end subroutine finalize

end module GridFileReaderModule
