!> @brief Output control data module.
module OutputControlDataModule

  use BaseDisModule, only: DisBaseType
  use InputOutputModule, only: print_format
  use KindModule, only: DP, I4B, LGP
  use PrintSaveManagerModule, only: PrintSaveManagerType, create_psm

  implicit none
  private
  public OutputControlDataType, ocd_cr

  !> @brief Output control data type.
  !!
  !! Determines whether output data should be printed to a list file or saved to disk.
  !! This type can be assigned to different variables, such as head or concentration.
  !! This type controls the logging and saving of output data in a consistent manner.
  !<
  type OutputControlDataType
    character(len=16), pointer :: cname => null() !< name of variable, such as HEAD
    character(len=60), pointer :: cdatafmp => null() !< fortran format for printing
    integer(I4B), pointer :: idataun => null() !< fortran unit number for binary output
    character(len=1), pointer :: editdesc => null() !< fortran format type (I, G, F, S, E)
    integer(I4B), pointer :: nvaluesp => null() !< number of values per line for printing
    integer(I4B), pointer :: nwidthp => null() !< width of the number for printing
    real(DP), pointer :: dnodata => null() !< no data value
    integer(I4B), pointer :: inodata => null() !< integer no data value
    real(DP), dimension(:), pointer, contiguous :: dblvec => null() !< double precision data array
    integer(I4B), dimension(:), pointer, contiguous :: intvec => null() !< integer data array
    class(DisBaseType), pointer :: dis => null() !< discretization package
    type(PrintSaveManagerType), pointer :: psm => null() !< print/save manager
  contains
    procedure :: allocate_scalars
    procedure :: init_int
    procedure :: init_dbl
    procedure :: set_option
    procedure :: ocd_rp_check
    procedure :: ocd_ot
    procedure :: ocd_da
  end type OutputControlDataType

contains

  !> @ brief Create a new output control data type.
  subroutine ocd_cr(ocdobj)
    type(OutputControlDataType), pointer :: ocdobj !< this instance
    
    allocate (ocdobj)
    call ocdobj%allocate_scalars()
  end subroutine ocd_cr

  !> @ brief Check the output control data type for consistency.
  subroutine ocd_rp_check(this, inunit)
    ! modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit
    ! dummy
    class(OutputControlDataType) :: this !< this instance
    integer(I4B), intent(in) :: inunit !< output unit number
    ! locals
    character(len=LINELENGTH) :: errmsg
    ! formats
    character(len=*), parameter :: fmtocsaveerr = &
      "(1X,'REQUESTING TO SAVE ',A,' BUT ',A,' SAVE FILE NOT SPECIFIED. ', &
       &A,' SAVE FILE MUST BE SPECIFIED IN OUTPUT CONTROL OPTIONS.')"
    
    ! If saving is enabled, make sure an output file was specified
    if (this%psm%save_steps%any_selected()) then
      if (this%idataun == 0) then
        write (errmsg, fmtocsaveerr) trim(adjustl(this%cname)), &
          trim(adjustl(this%cname)), &
          trim(adjustl(this%cname))
        call store_error(errmsg)
      end if
    end if
    
    if (count_errors() > 0) then
      call store_error_unit(inunit)
    end if
  end subroutine ocd_rp_check

  !> @brief Write to list file and/or save to binary file, depending on settings.
  subroutine ocd_ot(this, ipflg, kstp, endofperiod, iout, iprint_opt, isav_opt)
    ! dummy
    class(OutputControlDataType) :: this !< OutputControlDataType object
    integer(I4B), intent(inout) :: ipflg !< Flag indicating if something was printed
    integer(I4B), intent(in) :: kstp !< Current time step
    logical(LGP), intent(in) :: endofperiod !< End of period logical flag
    integer(I4B), intent(in) :: iout !< Unit number for output
    integer(I4B), optional, intent(in) :: iprint_opt !< Optional print flag override
    integer(I4B), optional, intent(in) :: isav_opt !< Optional save flag override
    ! local
    integer(I4B) :: iprint
    integer(I4B) :: idataun
    
    ! Initialize
    iprint = 0
    ipflg = 0
    idataun = 0
    
    ! Determine whether or not to print the array.  The present
    ! check allows a caller to override the print/save manager
    if (present(iprint_opt)) then
      if (iprint_opt /= 0) then
        iprint = 1
        ipflg = 1
      end if
    else
      if (this%psm%should_print(kstp, endofperiod)) then
        iprint = 1
        ipflg = 1
      end if
    end if
    
    ! Determine whether to save the array to a file
    if (present(isav_opt)) then
      if (isav_opt /= 0) then
        idataun = this%idataun
      end if
    else
      if (this%psm%should_save(kstp, endofperiod)) idataun = this%idataun
    end if
    
    ! Record double precision array
    if (associated(this%dblvec)) &
      call this%dis%record_array(this%dblvec, iout, iprint, idataun, &
                                 this%cname, this%cdatafmp, this%nvaluesp, &
                                 this%nwidthp, this%editdesc, this%dnodata)
    
    ! Record integer array (not supported yet)
    !if(associated(this%intvec)) &
    !call this%dis%record_array(this%intvec, iout, iprint, idataun, &
    !                               this%cname, this%cdatafmp, this%nvaluesp, &
    !                               this%nwidthp, this%editdesc, this%inodata)
  end subroutine ocd_ot

  !> @brief Deallocate the output control data type
  subroutine ocd_da(this)
    class(OutputControlDataType) :: this
    
    deallocate (this%cname)
    deallocate (this%cdatafmp)
    deallocate (this%idataun)
    deallocate (this%editdesc)
    deallocate (this%nvaluesp)
    deallocate (this%nwidthp)
    deallocate (this%dnodata)
    deallocate (this%inodata)
  end subroutine ocd_da

  !> @brief Initialize the output control data type for double precision data.
  subroutine init_dbl(this, cname, dblvec, dis, cdefpsm, cdeffmp, iout, &
                      dnodata)
    ! dummy
    class(OutputControlDataType) :: this !< OutputControlDataType object
    character(len=*), intent(in) :: cname !< Name of variable
    real(DP), dimension(:), pointer, contiguous, intent(in) :: dblvec !< Data array that will be managed by this object
    class(DisBaseType), pointer, intent(in) :: dis !< Discretization package
    character(len=*), intent(in) :: cdefpsm !< String for defining the print/save manager
    character(len=*), intent(in) :: cdeffmp !< String for print format
    integer(I4B), intent(in) :: iout !< Unit number for output
    real(DP), intent(in) :: dnodata !< No data value
    
    this%cname = cname
    this%dblvec => dblvec
    this%dis => dis
    this%dnodata = dnodata
    this%psm => create_psm()
    if (cdefpsm /= '') call this%psm%rp(cdefpsm, iout)
    call print_format(cdeffmp, this%cdatafmp, &
                      this%editdesc, this%nvaluesp, this%nwidthp, 0)
  end subroutine init_dbl

  !> @ brief Initialize the output control data type for integer data.
  subroutine init_int(this, cname, intvec, dis, cdefpsm, cdeffmp, iout, &
                      inodata)
    ! dummy
    class(OutputControlDataType) :: this !< OutputControlDataType object
    character(len=*), intent(in) :: cname !< Name of variable
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: intvec !< Data array that will be managed by this object
    class(DisBaseType), pointer, intent(in) :: dis !< Discretization package
    character(len=*), intent(in) :: cdefpsm !< String for defining the print/save manager
    character(len=*), intent(in) :: cdeffmp !< String for print format
    integer(I4B), intent(in) :: iout !< Unit number for output
    integer(I4B), intent(in) :: inodata !< No data value
    
    this%cname = cname
    this%intvec => intvec
    this%dis => dis
    this%inodata = inodata
    this%editdesc = 'I'
    this%psm => create_psm()
    if (cdefpsm /= '') call this%psm%rp(cdefpsm, iout)
    call print_format(cdeffmp, this%cdatafmp, this%editdesc, this%nvaluesp, &
                      this%nwidthp, 0)
  end subroutine init_int

  !> @ brief Allocate scalar variables
  subroutine allocate_scalars(this)
    ! modules
    use ConstantsModule, only: DZERO
    ! dummy
    class(OutputControlDataType) :: this !< OutputControlDataType object
    
    allocate (this%cname)
    allocate (this%cdatafmp)
    allocate (this%idataun)
    allocate (this%editdesc)
    allocate (this%nvaluesp)
    allocate (this%nwidthp)
    allocate (this%dnodata)
    allocate (this%inodata)
    
    this%cname = ''
    this%cdatafmp = ''
    this%idataun = 0
    this%editdesc = ''
    this%nvaluesp = 0
    this%nwidthp = 0
    this%dnodata = DZERO
    this%inodata = 0
  end subroutine allocate_scalars

  !> @ brief Set FILEOUT and PRINT_FORMAT based on an input string.
  subroutine set_option(this, linein, inunit, iout)
    ! modules
    use ConstantsModule, only: MNORMAL
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: urword, getunit, openfile
    use SimModule, only: store_error, store_error_unit, count_errors
    ! dummy
    class(OutputControlDataType) :: this !< OutputControlDataType object
    character(len=*), intent(in) :: linein !< Character string with options
    integer(I4B), intent(in) :: inunit !< Unit number for input
    integer(I4B), intent(in) :: iout !< Unit number for output
    ! local
    character(len=len(linein)) :: line
    integer(I4B) :: lloc, istart, istop, ival
    real(DP) :: rval
    ! format
    character(len=*), parameter :: fmtocsave = &
      "(4X,A,' INFORMATION WILL BE WRITTEN TO:', &
      &/,6X,'UNIT NUMBER: ', I0,/,6X, 'FILE NAME: ', A)"
    
    line(:) = linein(:)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
    select case (line(istart:istop))
    case ('FILEOUT')
      call urword(line, lloc, istart, istop, 0, ival, rval, 0, 0)
      this%idataun = getunit()
      write (iout, fmtocsave) trim(adjustl(this%cname)), this%idataun, &
        line(istart:istop)
      call openfile(this%idataun, iout, line(istart:istop), 'DATA(BINARY)', &
                    form, access, 'REPLACE', MNORMAL)
    case ('PRINT_FORMAT')
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      call print_format(line(istart:), this%cdatafmp, this%editdesc, &
                        this%nvaluesp, this%nwidthp, inunit)
    case default
      call store_error('Looking for FILEOUT or PRINT_FORMAT.  Found:')
      call store_error(trim(adjustl(line)))
      call store_error_unit(inunit)
    end select
  end subroutine set_option

end module OutputControlDataModule
