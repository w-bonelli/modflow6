module MethodCellPassToBotModule

  use KindModule, only: DP, I4B
  use MethodModule, only: MethodType
  use CellDefnModule, only: CellDefnType, create_defn
  use PrtFmiModule, only: PrtFmiType
  use BaseDisModule, only: DisBaseType
  use ParticleModule
  use CellModule, only: CellType
  use SubcellModule, only: SubcellType
  use TrackModule, only: TrackControlType
  implicit none

  private
  public :: MethodCellPassToBotType
  public :: create_method_cell_ptb

  type, extends(MethodType) :: MethodCellPassToBotType
    private
    type(CellDefnType), pointer :: defn
  contains
    procedure, public :: apply => apply_ptb
    procedure, public :: destroy
  end type MethodCellPassToBotType

contains

  !> @brief Create a new tracking method
  subroutine create_method_cell_ptb(method)
    type(MethodCellPassToBotType), pointer :: method
    allocate (method)
    allocate (method%type)
    method%type = "passtobottom"
    method%delegates = .false.
    call create_defn(method%defn)
  end subroutine create_method_cell_ptb

  !> @brief Destroy the tracking method
  subroutine destroy(this)
    class(MethodCellPassToBotType), intent(inout) :: this
    deallocate (this%type)
  end subroutine destroy

  !> @brief Pass particle vertically and instantaneously to the cell bottom
  subroutine apply_ptb(this, particle, tmax)
    ! -- modules
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(MethodCellPassToBotType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax

    call this%update(particle, this%defn)
    if (.not. particle%advancing) return
    particle%z = this%defn%bot
    particle%iboundary(2) = this%defn%npolyverts + 2
    call this%trackctl%save(particle, kper=kper, &
                            kstp=kstp, reason=1) ! reason=1: cell transition
  end subroutine apply_ptb

end module MethodCellPassToBotModule
