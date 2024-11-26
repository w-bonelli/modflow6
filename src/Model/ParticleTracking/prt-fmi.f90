module PrtFmiModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, LENAUXNAME, LENPACKAGENAME
  use SimModule, only: store_error, store_error_unit
  use SimVariablesModule, only: errmsg
  use FlowModelInterfaceModule, only: FlowModelInterfaceType
  use BaseDisModule, only: DisBaseType
  use BudgetObjectModule, only: BudgetObjectType
  use CellModule, only: MAX_POLY_CELLS

  implicit none
  private
  public :: PrtFmiType
  public :: fmi_cr

  character(len=LENPACKAGENAME) :: text = '    PRTFMI'

  type, extends(FlowModelInterfaceType) :: PrtFmiType

    double precision, allocatable, public :: SourceFlows(:) ! cell source flows array
    double precision, allocatable, public :: SinkFlows(:) ! cell sink flows array
    double precision, allocatable, public :: StorageFlows(:) ! cell storage flows array
    double precision, allocatable, public :: BoundaryFlows(:) ! cell boundary flows array

  contains

    procedure :: fmi_ad
    procedure :: fmi_df => prtfmi_df
    procedure, private :: accumulate_flows

  end type PrtFmiType

contains

  !> @brief Create a new PrtFmi object
  subroutine fmi_cr(fmiobj, name_model, inunit, iout)
    ! dummy
    type(PrtFmiType), pointer :: fmiobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(inout) :: inunit
    integer(I4B), intent(in) :: iout
    !
    ! Create the object
    allocate (fmiobj)
    !
    ! create name and memory path
    call fmiobj%set_names(1, name_model, 'FMI', 'FMI')
    fmiobj%text = text
    !
    ! Allocate scalars
    call fmiobj%allocate_scalars()
    !
    ! Set variables
    fmiobj%inunit = inunit
    fmiobj%iout = iout
    !
    ! Initialize block parser
    call fmiobj%parser%Initialize(fmiobj%inunit, fmiobj%iout)
    !
    ! Assign dependent variable label
    fmiobj%depvartype = 'TRACKS          '

  end subroutine fmi_cr

  !> @brief Time step advance
  subroutine fmi_ad(this)
    ! modules
    use ConstantsModule, only: DHDRY
    ! dummy
    class(PrtFmiType) :: this
    ! local
    integer(I4B) :: n
    character(len=15) :: nodestr
    character(len=*), parameter :: fmtdry = &
     &"(/1X,'WARNING: DRY CELL ENCOUNTERED AT ',a,';  RESET AS INACTIVE')"
    character(len=*), parameter :: fmtrewet = &
     &"(/1X,'DRY CELL REACTIVATED AT ', a)"

    ! Set flag to indicated that flows are being updated
    this%iflowsupdated = 1

    ! If reading flows from a budget file, read the next set of records
    if (this%iubud /= 0) &
      call this%advance_bfr()

    ! If reading heads from a head file, read the next set of records
    if (this%iuhds /= 0) &
      call this%advance_hfr()

    ! If mover flows are being read from file, read the next set of records
    if (this%iumvr /= 0) &
      call this%mvrbudobj%bfr_advance(this%dis, this%iout)

    ! Accumulate flows
    call this%accumulate_flows()

    ! Deactivate dry cells and reactivate rewet cells
    do n = 1, this%dis%nodes
      if (this%gwfsat(n) > DZERO) then
        this%ibdgwfsat0(n) = 1
      else
        this%ibdgwfsat0(n) = 0
      end if

      if (this%ibound(n) > 0) then
        if (this%gwfhead(n) == DHDRY) then
          this%ibound(n) = 0
          call this%dis%noder_to_string(n, nodestr)
          write (this%iout, fmtdry) trim(nodestr)
        end if
      else
        if (this%gwfhead(n) /= DHDRY) then
          this%ibound(n) = 1
          call this%dis%noder_to_string(n, nodestr)
          write (this%iout, fmtrewet) trim(nodestr)
        end if
      end if
    end do

  end subroutine fmi_ad

  !> @brief Define the flow model interface
  subroutine prtfmi_df(this, dis, idryinactive)
    ! modules
    use SimModule, only: store_error
    ! dummy
    class(PrtFmiType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), intent(in) :: idryinactive
    !
    ! Call parent class define
    call this%FlowModelInterfaceType%fmi_df(dis, idryinactive)
    !
    ! Allocate arrays
    allocate (this%StorageFlows(this%dis%nodes))
    allocate (this%SourceFlows(this%dis%nodes))
    allocate (this%SinkFlows(this%dis%nodes))
    allocate (this%BoundaryFlows(this%dis%nodes * MAX_POLY_CELLS))

  end subroutine prtfmi_df

  !> @brief Accumulate flows
  subroutine accumulate_flows(this)
    implicit none
    ! dummy
    class(PrtFmiType) :: this
    ! local
    integer(I4B) :: j, i, ip, ib
    integer(I4B) :: ioffset, iflowface, iauxiflowface
    real(DP) :: qbnd
    character(len=LENAUXNAME) :: auxname
    integer(I4B) :: naux
    !
    this%StorageFlows = DZERO
    if (this%igwfstrgss /= 0) &
      this%StorageFlows = this%StorageFlows + &
                          this%gwfstrgss
    if (this%igwfstrgsy /= 0) &
      this%StorageFlows = this%StorageFlows + &
                          this%gwfstrgsy

    this%SourceFlows = DZERO
    this%SinkFlows = DZERO
    this%BoundaryFlows = DZERO
    do ip = 1, this%nflowpack
      iauxiflowface = 0
      naux = this%gwfpackages(ip)%naux
      if (naux > 0) then
        do j = 1, naux
          auxname = this%gwfpackages(ip)%auxname(j)
          if (trim(adjustl(auxname)) == "IFLOWFACE") then
            iauxiflowface = j
            exit
          end if
        end do
      end if
      do ib = 1, this%gwfpackages(ip)%nbound
        i = this%gwfpackages(ip)%nodelist(ib)
        if (i <= 0) cycle
        if (this%ibound(i) <= 0) cycle
        qbnd = this%gwfpackages(ip)%get_flow(ib)
        ! todo, after initial release: default iflowface values for different packages
        iflowface = 0
        if (iauxiflowface > 0) then
          iflowface = NINT(this%gwfpackages(ip)%auxvar(iauxiflowface, ib))
          ! this maps bot -2 -> 9, top -1 -> 10; see note re: max faces below
          if (iflowface < 0) iflowface = iflowface + MAX_POLY_CELLS + 1
        end if
        if (iflowface .gt. 0) then
          ioffset = (i - 1) * MAX_POLY_CELLS
          this%BoundaryFlows(ioffset + iflowface) = &
            this%BoundaryFlows(ioffset + iflowface) + qbnd
        else if (qbnd .gt. DZERO) then
          this%SourceFlows(i) = this%SourceFlows(i) + qbnd
        else if (qbnd .lt. DZERO) then
          this%SinkFlows(i) = this%SinkFlows(i) + qbnd
        end if
      end do
    end do

  end subroutine accumulate_flows

end module PrtFmiModule
