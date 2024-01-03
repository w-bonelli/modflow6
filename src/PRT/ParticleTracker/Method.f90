!> @brief Particle tracking strategies
module MethodModule

  use KindModule, only: DP, I4B, LGP
  use ErrorUtilModule, only: pstop
  use SubcellModule, only: SubcellType
  use ParticleModule
  use BaseDisModule, only: DisBaseType
  use PrtFmiModule, only: PrtFmiType
  use CellModule, only: CellType
  use CellDefnModule, only: CellDefnType
  use TrackModule, only: TrackControlType
  implicit none

  private
  public :: MethodType, get_iatop

  !> @brief Base type for particle tracking methods.
  !!
  !! The PRT tracking algorithm invokes a "tracking method" for each
  !! domain. A domain can be a model, cell in a model, or subcell in
  !! a cell. Tracking proceeds recursively, delegating to a possibly
  !! arbitrary number of subdomains (currently, only the three above
  !! are recognized). A tracking method is responsible for advecting
  !! a particle through a domain, delegating to subdomains as needed
  !! depending on cell geometry (implementing the strategy pattern).
  !<
  type, abstract :: MethodType
    character(len=40), pointer, public :: type ! method name
    logical(LGP), public :: delegates ! whether the method delegates
    type(PrtFmiType), pointer, public :: fmi => null() !< ptr to fmi
    class(CellType), pointer, public :: cell => null() ! ptr to a cell
    class(SubcellType), pointer, public :: subcell => null() ! ptr to a subcell
    type(TrackControlType), pointer, public :: trackctl => null() ! ptr to track file control
    integer(I4B), dimension(:), pointer, contiguous, public :: izone => null() !< pointer to zone numbers
    real(DP), dimension(:), pointer, contiguous, public :: flowja => null() !< pointer to intercell flows
    real(DP), dimension(:), pointer, contiguous, public :: porosity => null() !< pointer to aquifer porosity
    real(DP), dimension(:), pointer, contiguous, public :: retfactor => null() !< pointer to retardation factor
  contains
    ! Implemented in all subtypes
    procedure(apply), deferred :: apply
    procedure(destroy), deferred :: destroy
    ! Overridden in subtypes that delegate
    procedure :: pass
    procedure :: load
    ! Implemented in this class
    procedure :: init
    procedure :: track
    procedure :: try_pass
    procedure :: update
  end type MethodType

  abstract interface
    subroutine apply(this, particle, tmax)
      import DP
      import MethodType
      import ParticleType
      class(MethodType), intent(inout) :: this
      type(ParticleType), pointer, intent(inout) :: particle
      real(DP), intent(in) :: tmax
    end subroutine apply
    subroutine destroy(this)
      import MethodType
      class(MethodType), intent(inout) :: this
    end subroutine destroy
  end interface

contains

  subroutine init(this, fmi, cell, subcell, trackctl, &
                  izone, flowja, porosity, retfactor)
    class(MethodType), intent(inout) :: this
    type(PrtFmiType), intent(in), pointer, optional :: fmi
    class(CellType), intent(in), pointer, optional :: cell
    class(SubcellType), intent(in), pointer, optional :: subcell
    type(TrackControlType), intent(in), pointer, optional :: trackctl
    integer(I4B), intent(in), pointer, optional :: izone(:)
    real(DP), intent(in), pointer, optional :: flowja(:)
    real(DP), intent(in), pointer, optional :: porosity(:)
    real(DP), intent(in), pointer, optional :: retfactor(:)

    if (present(fmi)) this%fmi => fmi
    if (present(cell)) this%cell => cell
    if (present(subcell)) this%subcell => subcell
    if (present(trackctl)) this%trackctl => trackctl
    if (present(izone)) this%izone => izone
    if (present(flowja)) this%flowja => flowja
    if (present(porosity)) this%porosity => porosity
    if (present(retfactor)) this%retfactor => retfactor
  end subroutine init

  !> @brief Track particle through subdomains
  recursive subroutine track(this, particle, level, tmax)
    ! dummy
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer :: level
    real(DP), intent(in) :: tmax
    ! local
    logical :: advancing
    integer :: next_level
    class(methodType), pointer :: submethod

    advancing = .true.
    next_level = level + 1
    do while (advancing)
      call this%load(particle, next_level, submethod)
      call submethod%apply(particle, tmax)
      call this%try_pass(particle, next_level, advancing)
    end do
  end subroutine track

  !> @brief Try passing the particle to the next subdomain
  subroutine try_pass(this, particle, next_level, advancing)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer :: next_level
    logical :: advancing

    ! tracking submethod marked tracking complete?
    ! reset domain boundary flag and don't advance
    if (.not. particle%advancing) then
      particle%iboundary = 0
      advancing = .false.
    else
      ! otherwise pass particle to next subdomain
      ! and if it's on a boundary, stop advancing
      call this%pass(particle)
      if (particle%iboundary(next_level - 1) .ne. 0) &
        advancing = .false.
    end if
  end subroutine try_pass

  !> @brief Load subdomain tracking method (submethod)
  subroutine load(this, particle, next_level, submethod)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer, intent(in) :: next_level
    class(MethodType), pointer, intent(inout) :: submethod
    call pstop(1, "load must be overridden")
  end subroutine load

  !> @brief Pass a particle to the next subdomain
  subroutine pass(this, particle)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    call pstop(1, "pass must be overridden")
  end subroutine pass

  !> @brief Update particle state and check termination conditions
  !!
  !! Update the particle's properties (e.g. advancing flag, zone number,
  !! status). If any termination conditions apply, the particle's status
  !! will be set to the appropriate termination value. If any reporting
  !! conditions apply, save particle state with the proper reason code.
  !<
  subroutine update(this, particle, cell_defn)
    ! -- modules
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    type(CellDefnType), pointer, intent(inout) :: cell_defn

    particle%izone = cell_defn%izone
    if (cell_defn%izone .ne. 0) then
      if (particle%istopzone .eq. cell_defn%izone) then
        particle%advancing = .false.
        particle%istatus = 6
        call this%trackctl%save(particle, kper=kper, &
                                kstp=kstp, reason=3) ! reason=3: termination
      end if
    else if (cell_defn%inoexitface .ne. 0) then
      particle%advancing = .false.
      particle%istatus = 5
      call this%trackctl%save(particle, kper=kper, &
                              kstp=kstp, reason=3) ! reason=3: termination
    else if (cell_defn%iweaksink .ne. 0) then
      if (particle%istopweaksink .ne. 0) then
        particle%advancing = .false.
        particle%istatus = 3
        call this%trackctl%save(particle, kper=kper, &
                                kstp=kstp, reason=3) ! reason=3: termination
      else
        call this%trackctl%save(particle, kper=kper, &
                                kstp=kstp, reason=4) ! reason=4: exited weak sink
      end if
    end if
  end subroutine update

  !> @brief Get the index corresponding to top elevation of a cell in the grid.
  !! This is -1 if the cell is in the top layer and 1 otherwise.
  function get_iatop(ncpl, icu) result(iatop)
    integer, intent(in) :: ncpl, icu
    integer :: iatop

    if (icu .le. ncpl) then
      iatop = -1
    else
      iatop = 1
    end if
  end function get_iatop

end module MethodModule
