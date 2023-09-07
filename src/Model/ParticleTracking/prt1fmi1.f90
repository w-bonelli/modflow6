module PrtFmiModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, LENPACKAGENAME
  use SimModule, only: store_error, store_error_unit
  use SimVariablesModule, only: errmsg
  use FlowModelInterfaceModule, only: FlowModelInterfaceType, BudObjPtrArray
  use BaseDisModule, only: DisBaseType
  use BudgetObjectModule, only: BudgetObjectType

  implicit none
  private
  public :: PrtFmiType
  public :: prtfmi_cr

  character(len=LENPACKAGENAME) :: text = '    PRTFMI'

  type, extends(FlowModelInterfaceType) :: PrtFmiType

    double precision, allocatable, public :: SourceFlows(:) ! cell source flows array
    double precision, allocatable, public :: SinkFlows(:) ! cell sink flows array
    double precision, allocatable, public :: StorageFlows(:) ! cell storage flows array
    double precision, allocatable, public :: BoundaryFlows(:) ! cell boundary flows array

  contains

    procedure :: prtfmi_df
    procedure :: prtfmi_ad
    procedure, private :: accumulate_flows

  end type PrtFmiType

contains

  !> @brief Create a new PrtFmi object
  subroutine prtfmi_cr(prtfmiobj, name_model, inunit, iout)
    ! -- dummy
    type(PrtFmiType), pointer :: prtfmiobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(inout) :: inunit
    integer(I4B), intent(in) :: iout
    !
    ! -- Create the object
    allocate (prtfmiobj)
    !
    ! -- create name and memory path
    call prtfmiobj%set_names(1, name_model, 'FMI', 'FMI')
    prtfmiobj%text = text
    !
    ! -- Allocate scalars
    call prtfmiobj%allocate_scalars()
    !
    ! -- if inunit == 0, then there is no file to read, but it still needs
    !    to be active in order to manage pointers to gwf model
    !if (inunit == 0) inunit = 1
    !
    ! -- Set variables
    prtfmiobj%inunit = inunit
    prtfmiobj%iout = iout
    !
    ! -- Initialize block parser
    call prtfmiobj%parser%Initialize(prtfmiobj%inunit, prtfmiobj%iout)
    !
    ! -- Return
    return
  end subroutine prtfmi_cr

  !> @brief Define the flow model interface
  subroutine prtfmi_df(this, dis)
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(PrtFmiType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtfmi = &
      "(1x,/1x,'PRTFMI -- PRT FLOW MODEL INTERFACE, VERSION 1, 8/29/2017',     &
      &' INPUT READ FROM UNIT ', i0, //)" ! kluge note: update
    character(len=*), parameter :: fmtfmi0 = &
"(1x,/1x,'PRTFMI -- PRT FLOW MODEL INTERFACE, &
&VERSION 1, 8/29/2017')" ! kluge note: update
    !
    ! --print a message identifying the FMI package.
    if (this%inunit /= 0) then
      write (this%iout, fmtfmi) this%inunit
    else
      write (this%iout, fmtfmi0)
      if (this%flows_from_file) then
        write (this%iout, '(a)') '  FLOWS ARE ASSUMED TO BE ZERO.'
      else
        write (this%iout, '(a)') '  FLOWS PROVIDED BY A GWF MODEL IN THIS &
          &SIMULATION'
      end if
    end if
    !
    ! -- Call parent class define
    call this%FlowModelInterfaceType%fmi_df(dis)
    !
    ! -- Allocate arrays
    allocate (this%StorageFlows(this%dis%nodes)) ! kluge note: need allocate_arrays subroutine
    allocate (this%SourceFlows(this%dis%nodes))
    allocate (this%SinkFlows(this%dis%nodes))
    allocate (this%BoundaryFlows(this%dis%nodes * 10)) ! kluge note: hardwired to max 8 polygon faces plus top and bottom for now
    !
    ! -- Return
    return
  end subroutine prtfmi_df

  !> @brief Time step advance
  subroutine prtfmi_ad(this)
    ! -- modules
    use ConstantsModule, only: DHDRY
    ! -- dummy
    class(PrtFmiType) :: this
    ! -- local
    integer(I4B) :: n
    character(len=15) :: nodestr
    character(len=*), parameter :: fmtdry = &
     &"(/1X,'WARNING: DRY CELL ENCOUNTERED AT ',a,';  RESET AS INACTIVE')"
    character(len=*), parameter :: fmtrewet = &
     &"(/1X,'DRY CELL REACTIVATED AT ', a)"
    !
    ! -- Call parent class advance
    call this%FlowModelInterfaceType%fmi_ad()
    !
    ! ! -- If advanced package flows are being read from file, read the next set of records
    ! if (this%flows_from_file .and. this%inunit /= 0) then
    !   do n = 1, size(this%aptbudobj)
    !     ! kluge note: need GWF advanced-package flows from separate files?
    !     call this%aptbudobj(n)%ptr%bfr_advance(this%dis, this%iout)
    !   end do
    ! end if
    ! -- Accumulate flows
    call this%accumulate_flows()
    !
    ! -- if flow cell is dry, then set this%ibound = 0
    do n = 1, this%dis%nodes
      !
      ! -- Calculate the ibound-like array that has 0 if saturation
      !    is zero and 1 otherwise
      if (this%gwfsat(n) > DZERO) then
        this%ibdgwfsat0(n) = 1
      else
        this%ibdgwfsat0(n) = 0
      end if
      !
      ! -- Check if active model cell is inactive for flow
      if (this%ibound(n) > 0) then
        if (this%gwfhead(n) == DHDRY) then
          ! -- cell should be made inactive
          this%ibound(n) = 0
          call this%dis%noder_to_string(n, nodestr)
          write (this%iout, fmtdry) trim(nodestr)
        end if
      end if
      !
      ! -- Convert dry model cell to active if flow has rewet
      if (this%ibound(n) == 0) then
        if (this%gwfhead(n) /= DHDRY) then
          ! -- cell is now wet
          this%ibound(n) = 1
          call this%dis%noder_to_string(n, nodestr)
          write (this%iout, fmtrewet) trim(nodestr)
        end if
      end if
    end do
    !
    ! -- Return
    return
  end subroutine prtfmi_ad

  !> @brief Accumulate flows
  subroutine accumulate_flows(this)
    use GwfDisvModule ! kluge???
    use ConstantsModule, only: LENAUXNAME ! kluge???
    implicit none
    ! -- dummy
    class(PrtFmiType) :: this
    ! -- local
    integer :: j, i, ip, ib
    integer :: ioffset, iflowface, iauxiflowface !, iface
    double precision :: qbnd
    character(len=LENAUXNAME) :: auxname
    integer(I4B) :: naux
    !
    this%StorageFlows = 0d0
    if (this%igwfstrgss /= 0) &
      this%StorageFlows = this%StorageFlows + &
                          this%gwfstrgss
    if (this%igwfstrgsy /= 0) &
      this%StorageFlows = this%StorageFlows + &
                          this%gwfstrgsy
    ! kluge note: need separate SourceFlows and SinkFlows? just for budget-reporting?
    ! kluge note: SinkFlows used to identify weak sinks
    this%SourceFlows = 0d0
    this%SinkFlows = 0d0
    this%BoundaryFlows = 0d0
    do ip = 1, this%nflowpack
      iauxiflowface = 0
      naux = this%gwfpackages(ip)%naux
      if (naux > 0) then
        do j = 1, naux
          auxname = this%gwfpackages(ip)%auxname(j)
          if (trim(adjustl(auxname)) == "IFLOWFACE") then
            iauxiflowface = j
            exit
            ! else if (trim(adjustl(auxname)) == "IFACE") then   ! kluge note: allow IFACE and do conversion???
            !   iauxiflowface = -j
            !   exit
          end if
        end do
      end if
      do ib = 1, this%gwfpackages(ip)%nbound
        i = this%gwfpackages(ip)%nodelist(ib)
        ! if (this%gwfibound(i) <= 0) cycle
        if (this%ibound(i) <= 0) cycle
        qbnd = this%gwfpackages(ip)%get_flow(ib)
        iflowface = 0 ! kluge note: eventually have default iflowface values for different packages
        if (iauxiflowface > 0) then
          ! expected int here... ok to round??
          iflowface = NINT(this%gwfpackages(ip)%auxvar(iauxiflowface, ib))
          if (iflowface < 0) iflowface = iflowface + 11 ! bot -> 9, top -> 10; see note re: max faces below
          ! else if (iauxiflowface < 0) then                    ! kluge note: allow IFACE and do conversion???
          !   ! kluge note: is it possible to check for a rectangular-celled grid here???
          !   iface = this%gwfpackages(ip)%auxvar(-iauxiflowface, ib)
          !   iflowface = iface   ! kluge note: will need to convert
        end if
        if (iflowface .gt. 0) then
          ioffset = (i - 1) * 10 ! kluge note: hardwired for max 8 polygon faces plus top and bottom for now
          this%BoundaryFlows(ioffset + iflowface) = &
            this%BoundaryFlows(ioffset + iflowface) + qbnd
        else if (qbnd .gt. 0d0) then
          this%SourceFlows(i) = this%SourceFlows(i) + qbnd
        else if (qbnd .lt. 0d0) then
          this%SinkFlows(i) = this%SinkFlows(i) + qbnd
        end if
      end do
    end do
    !
    return
    !
  end subroutine accumulate_flows

end module PrtFmiModule
