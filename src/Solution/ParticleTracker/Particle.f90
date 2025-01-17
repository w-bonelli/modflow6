module ParticleModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE, LENMEMPATH, LENBOUNDNAME
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, &
                                 mem_reallocate
  implicit none

  private
  public :: ParticleType, ParticleStoreType, &
            create_particle, allocate_particle_store

  ! tracking levels (1: model, 2: cell, 3: subcell)
  integer, parameter, public :: levelmax = 4

  !> @brief Particle tracked by the PRT model.
  !!
  !! Record-type to conveniently shuffle a particle's
  !! state to/from storage before/after its trajectory
  !! is solved for each time step.
  !!
  !! Particle coordinates may be local to the cell or
  !! global/model. Routines are provided to convert a
  !! particle's global coordinates to/from cell-local
  !! coordinates for tracking through cell subdomains.
  !!
  !! Particles are identified by composite key, i.e.,
  !! combinations of properties imdl, iprp, irpt, and
  !! trelease. An optional label may be provided, but
  !! need not be unique
  !<
  type ParticleType
    private
    ! identity
    character(len=LENBOUNDNAME), public :: name = '' !< optional particle name
    integer(I4B), public :: imdl !< index of model the particle originated in
    integer(I4B), public :: iprp !< index of release package the particle is from
    integer(I4B), public :: irpt !< index of release point the particle is from
    integer(I4B), public :: ip !< index of particle in the particle list
    ! stop criteria
    integer(I4B), public :: istopweaksink !< weak sink option (0: do not stop, 1: stop)
    integer(I4B), public :: istopzone !< stop zone number
    integer(I4B), public :: idrymeth !< dry tracking method
    ! state
    integer(I4B), allocatable, public :: idomain(:) !< tracking domain hierarchy ! TODO: rename to itdomain? idomain
    integer(I4B), allocatable, public :: iboundary(:) !< tracking domain boundaries
    integer(I4B), public :: icp !< previous cell number (reduced)
    integer(I4B), public :: icu !< user cell number
    integer(I4B), public :: ilay !< grid layer
    integer(I4B), public :: izone !< current zone number
    integer(I4B), public :: izp !< previous zone number
    integer(I4B), public :: istatus !< tracking status
    real(DP), public :: x !< x coordinate
    real(DP), public :: y !< y coordinate
    real(DP), public :: z !< z coordinate
    real(DP), public :: trelease !< release time
    real(DP), public :: tstop !< stop time
    real(DP), public :: ttrack !< time tracked so far
    real(DP), public :: xorigin !< x origin for coordinate transformation from model to local
    real(DP), public :: yorigin !< y origin for coordinate transformation from model to local
    real(DP), public :: zorigin !< z origin for coordinate transformation from model to local
    real(DP), public :: sinrot !< sine of rotation angle for coordinate transformation from model to local
    real(DP), public :: cosrot !< cosine of rotation angle for coordinate transformation from model to local
    real(DP), public :: extol !< tolerance for iterative solution of particle exit location and time in generalized Pollock's method
    logical(LGP), public :: transformed !< whether coordinates have been transformed from model to local
    logical(LGP), public :: advancing !< whether particle is still being tracked for current time step
    integer(I4B), public :: ifrctrn !< whether to force solving the particle with the ternary method
    integer(I4B), public :: iexmeth !< method for iterative solution of particle exit location and time in generalized Pollock's method
    integer(I4B), public :: iextend !< whether to extend tracking beyond the end of the simulation
  contains
    procedure, public :: get_model_coords
    procedure, public :: load_particle
    procedure, public :: transform => transform_coords
    procedure, public :: reset_transform
  end type ParticleType

  !> @brief Structure of arrays to store particles.
  type ParticleStoreType
    private
    ! identity
    character(len=LENBOUNDNAME), dimension(:), pointer, public, contiguous :: name !< optional particle label
    integer(I4B), dimension(:), pointer, public, contiguous :: imdl !< index of model particle originated in
    integer(I4B), dimension(:), pointer, public, contiguous :: iprp !< index of release package the particle originated in
    integer(I4B), dimension(:), pointer, public, contiguous :: irpt !< index of release point in the particle release package the particle originated in
    ! stopping criteria
    integer(I4B), dimension(:), pointer, public, contiguous :: istopweaksink !< weak sink option: 0 = do not stop, 1 = stop
    integer(I4B), dimension(:), pointer, public, contiguous :: istopzone !< stop zone number
    integer(I4B), dimension(:), pointer, public, contiguous :: idrymeth !< stop in dry cells
    ! state
    integer(I4B), dimension(:, :), pointer, public, contiguous :: idomain !< array of indices for domains in the tracking domain hierarchy
    integer(I4B), dimension(:, :), pointer, public, contiguous :: iboundary !< array of indices for tracking domain boundaries
    integer(I4B), dimension(:), pointer, public, contiguous :: icu !< cell number (user)
    integer(I4B), dimension(:), pointer, public, contiguous :: ilay !< layer
    integer(I4B), dimension(:), pointer, public, contiguous :: izone !< current zone number
    integer(I4B), dimension(:), pointer, public, contiguous :: izp !< previous zone number
    integer(I4B), dimension(:), pointer, public, contiguous :: istatus !< particle status
    real(DP), dimension(:), pointer, public, contiguous :: x !< model x coord of particle
    real(DP), dimension(:), pointer, public, contiguous :: y !< model y coord of particle
    real(DP), dimension(:), pointer, public, contiguous :: z !< model z coord of particle
    real(DP), dimension(:), pointer, public, contiguous :: trelease !< particle release time
    real(DP), dimension(:), pointer, public, contiguous :: tstop !< particle stop time
    real(DP), dimension(:), pointer, public, contiguous :: ttrack !< current tracking time
    integer(I4B), dimension(:), pointer, public, contiguous :: ifrctrn !< force ternary method
    integer(I4B), dimension(:), pointer, public, contiguous :: iexmeth !< method for iterative solution of particle exit location and time in generalized Pollock's method
    real(DP), dimension(:), pointer, public, contiguous :: extol !< tolerance for iterative solution of particle exit location and time in generalized Pollock's method
    integer(LGP), dimension(:), pointer, public, contiguous :: extend !< whether to extend tracking beyond the end of the simulation
  contains
    procedure, public :: deallocate
    procedure, public :: num_stored
    procedure, public :: resize
    procedure, public :: save_particle
  end type ParticleStoreType

contains

  !> @brief Create a new particle
  subroutine create_particle(particle)
    type(ParticleType), pointer :: particle !< particle
    allocate (particle)
    allocate (particle%idomain(levelmax))
    allocate (particle%iboundary(levelmax))
  end subroutine create_particle

  !> @brief Create a new particle store
  subroutine allocate_particle_store(this, np, mempath)
    type(ParticleStoreType), pointer :: this !< store
    integer(I4B), intent(in) :: np !< number of particles
    character(*), intent(in) :: mempath !< path to memory

    allocate (this)
    call mem_allocate(this%imdl, np, 'PLIMDL', mempath)
    call mem_allocate(this%irpt, np, 'PLIRPT', mempath)
    call mem_allocate(this%iprp, np, 'PLIPRP', mempath)
    call mem_allocate(this%name, LENBOUNDNAME, np, 'PLNAME', mempath)
    call mem_allocate(this%icu, np, 'PLICU', mempath)
    call mem_allocate(this%ilay, np, 'PLILAY', mempath)
    call mem_allocate(this%izone, np, 'PLIZONE', mempath)
    call mem_allocate(this%izp, np, 'PLIZP', mempath)
    call mem_allocate(this%istatus, np, 'PLISTATUS', mempath)
    call mem_allocate(this%x, np, 'PLX', mempath)
    call mem_allocate(this%y, np, 'PLY', mempath)
    call mem_allocate(this%z, np, 'PLZ', mempath)
    call mem_allocate(this%trelease, np, 'PLTRELEASE', mempath)
    call mem_allocate(this%tstop, np, 'PLTSTOP', mempath)
    call mem_allocate(this%ttrack, np, 'PLTTRACK', mempath)
    call mem_allocate(this%istopweaksink, np, 'PLISTOPWEAKSINK', mempath)
    call mem_allocate(this%istopzone, np, 'PLISTOPZONE', mempath)
    call mem_allocate(this%idrymeth, np, 'PLIDRYMETH', mempath)
    call mem_allocate(this%ifrctrn, np, 'PLIFRCTRN', mempath)
    call mem_allocate(this%iexmeth, np, 'PLIEXMETH', mempath)
    call mem_allocate(this%extol, np, 'PLEXTOL', mempath)
    call mem_allocate(this%extend, np, 'PLIEXTEND', mempath)
    call mem_allocate(this%idomain, np, levelmax, 'PLIDOMAIN', mempath)
    call mem_allocate(this%iboundary, np, levelmax, 'PLIBOUNDARY', mempath)
  end subroutine allocate_particle_store

  !> @brief Deallocate particle arrays
  subroutine deallocate (this, mempath)
    class(ParticleStoreType), intent(inout) :: this !< store
    character(*), intent(in) :: mempath !< path to memory

    call mem_deallocate(this%imdl, 'PLIMDL', mempath)
    call mem_deallocate(this%iprp, 'PLIPRP', mempath)
    call mem_deallocate(this%irpt, 'PLIRPT', mempath)
    call mem_deallocate(this%name, 'PLNAME', mempath)
    call mem_deallocate(this%icu, 'PLICU', mempath)
    call mem_deallocate(this%ilay, 'PLILAY', mempath)
    call mem_deallocate(this%izone, 'PLIZONE', mempath)
    call mem_deallocate(this%izp, 'PLIZP', mempath)
    call mem_deallocate(this%istatus, 'PLISTATUS', mempath)
    call mem_deallocate(this%x, 'PLX', mempath)
    call mem_deallocate(this%y, 'PLY', mempath)
    call mem_deallocate(this%z, 'PLZ', mempath)
    call mem_deallocate(this%trelease, 'PLTRELEASE', mempath)
    call mem_deallocate(this%tstop, 'PLTSTOP', mempath)
    call mem_deallocate(this%ttrack, 'PLTTRACK', mempath)
    call mem_deallocate(this%istopweaksink, 'PLISTOPWEAKSINK', mempath)
    call mem_deallocate(this%istopzone, 'PLISTOPZONE', mempath)
    call mem_deallocate(this%idrymeth, 'PLIDRYMETH', mempath)
    call mem_deallocate(this%ifrctrn, 'PLIFRCTRN', mempath)
    call mem_deallocate(this%iexmeth, 'PLIEXMETH', mempath)
    call mem_deallocate(this%extol, 'PLEXTOL', mempath)
    call mem_deallocate(this%extend, 'PLIEXTEND', mempath)
    call mem_deallocate(this%idomain, 'PLIDOMAIN', mempath)
    call mem_deallocate(this%iboundary, 'PLIBOUNDARY', mempath)
  end subroutine deallocate

  !> @brief Reallocate particle arrays
  subroutine resize(this, np, mempath)
    ! dummy
    class(ParticleStoreType), intent(inout) :: this !< particle store
    integer(I4B), intent(in) :: np !< number of particles
    character(*), intent(in) :: mempath !< path to memory

    ! resize arrays
    call mem_reallocate(this%imdl, np, 'PLIMDL', mempath)
    call mem_reallocate(this%iprp, np, 'PLIPRP', mempath)
    call mem_reallocate(this%irpt, np, 'PLIRPT', mempath)
    call mem_reallocate(this%name, LENBOUNDNAME, np, 'PLNAME', mempath)
    call mem_reallocate(this%icu, np, 'PLICU', mempath)
    call mem_reallocate(this%ilay, np, 'PLILAY', mempath)
    call mem_reallocate(this%izone, np, 'PLIZONE', mempath)
    call mem_reallocate(this%izp, np, 'PLIZP', mempath)
    call mem_reallocate(this%istatus, np, 'PLISTATUS', mempath)
    call mem_reallocate(this%x, np, 'PLX', mempath)
    call mem_reallocate(this%y, np, 'PLY', mempath)
    call mem_reallocate(this%z, np, 'PLZ', mempath)
    call mem_reallocate(this%trelease, np, 'PLTRELEASE', mempath)
    call mem_reallocate(this%tstop, np, 'PLTSTOP', mempath)
    call mem_reallocate(this%ttrack, np, 'PLTTRACK', mempath)
    call mem_reallocate(this%istopweaksink, np, 'PLISTOPWEAKSINK', mempath)
    call mem_reallocate(this%istopzone, np, 'PLISTOPZONE', mempath)
    call mem_reallocate(this%idrymeth, np, 'PLIDRYMETH', mempath)
    call mem_reallocate(this%ifrctrn, np, 'PLIFRCTRN', mempath)
    call mem_reallocate(this%iexmeth, np, 'PLIEXMETH', mempath)
    call mem_reallocate(this%extol, np, 'PLEXTOL', mempath)
    call mem_reallocate(this%extend, np, 'PLIEXTEND', mempath)
    call mem_reallocate(this%idomain, np, levelmax, 'PLIDOMAIN', mempath)
    call mem_reallocate(this%iboundary, np, levelmax, 'PLIBOUNDARY', mempath)
  end subroutine resize

  !> @brief Load a particle from the particle store.
  !!
  !! This routine is used to initialize a particle for tracking.
  !! The advancing flag and coordinate transformation are reset.
  !<
  subroutine load_particle(this, store, imdl, iprp, ip)
    class(ParticleType), intent(inout) :: this !< particle
    type(ParticleStoreType), intent(in) :: store !< particle storage
    integer(I4B), intent(in) :: imdl !< index of model particle originated in
    integer(I4B), intent(in) :: iprp !< index of particle release package particle originated in
    integer(I4B), intent(in) :: ip !< index into the particle list

    call this%reset_transform()
    this%imdl = imdl
    this%iprp = iprp
    this%irpt = store%irpt(ip)
    this%ip = ip
    this%name = store%name(ip)
    this%istopweaksink = store%istopweaksink(ip)
    this%istopzone = store%istopzone(ip)
    this%idrymeth = store%idrymeth(ip)
    this%icp = 0
    this%icu = store%icu(ip)
    this%ilay = store%ilay(ip)
    this%izone = store%izone(ip)
    this%izp = store%izp(ip)
    this%istatus = store%istatus(ip)
    this%x = store%x(ip)
    this%y = store%y(ip)
    this%z = store%z(ip)
    this%trelease = store%trelease(ip)
    this%tstop = store%tstop(ip)
    this%ttrack = store%ttrack(ip)
    this%advancing = .true.
    this%idomain(1:levelmax) = &
      store%idomain(ip, 1:levelmax)
    this%idomain(1) = imdl
    this%iboundary(1:levelmax) = &
      store%iboundary(ip, 1:levelmax)
    this%ifrctrn = store%ifrctrn(ip)
    this%iexmeth = store%iexmeth(ip)
    this%extol = store%extol(ip)
    this%iextend = store%extend(ip)
  end subroutine load_particle

  !> @brief Save a particle's state to the particle store
  subroutine save_particle(this, particle, ip)
    class(ParticleStoreType), intent(inout) :: this !< particle storage
    type(ParticleType), intent(in) :: particle !< particle
    integer(I4B), intent(in) :: ip !< particle index

    this%imdl(ip) = particle%imdl
    this%iprp(ip) = particle%iprp
    this%irpt(ip) = particle%irpt
    this%name(ip) = particle%name
    this%istopweaksink(ip) = particle%istopweaksink
    this%istopzone(ip) = particle%istopzone
    this%idrymeth(ip) = particle%idrymeth
    this%icu(ip) = particle%icu
    this%ilay(ip) = particle%ilay
    this%izone(ip) = particle%izone
    this%izp(ip) = particle%izp
    this%istatus(ip) = particle%istatus
    this%x(ip) = particle%x
    this%y(ip) = particle%y
    this%z(ip) = particle%z
    this%trelease(ip) = particle%trelease
    this%tstop(ip) = particle%tstop
    this%ttrack(ip) = particle%ttrack
    this%idomain( &
      ip, &
      1:levelmax) = &
      particle%idomain(1:levelmax)
    this%iboundary( &
      ip, &
      1:levelmax) = &
      particle%iboundary(1:levelmax)
    this%ifrctrn(ip) = particle%ifrctrn
    this%iexmeth(ip) = particle%iexmeth
    this%extol(ip) = particle%extol
    this%extend(ip) = particle%iextend
  end subroutine save_particle

  !> @brief Transform particle coordinates.
  !!
  !! Apply a translation and/or rotation to particle coordinates.
  !! No rescaling. It's also possible to invert a transformation.
  !! Be sure to reset the transformation after using it.
  !<
  subroutine transform_coords(this, xorigin, yorigin, zorigin, &
                              sinrot, cosrot, invert)
    use GeomUtilModule, only: transform, compose
    class(ParticleType), intent(inout) :: this !< particle
    real(DP), intent(in), optional :: xorigin !< x coordinate of origin
    real(DP), intent(in), optional :: yorigin !< y coordinate of origin
    real(DP), intent(in), optional :: zorigin !< z coordinate of origin
    real(DP), intent(in), optional :: sinrot !< sine of rotation angle
    real(DP), intent(in), optional :: cosrot !< cosine of rotation angle
    logical(LGP), intent(in), optional :: invert !< whether to invert

    call transform(this%x, this%y, this%z, &
                   this%x, this%y, this%z, &
                   xorigin, yorigin, zorigin, &
                   sinrot, cosrot, invert)

    call compose(this%xorigin, this%yorigin, this%zorigin, &
                 this%sinrot, this%cosrot, &
                 xorigin, yorigin, zorigin, &
                 sinrot, cosrot, invert)

    this%transformed = .true.
  end subroutine transform_coords

  subroutine reset_transform(this)
    class(ParticleType), intent(inout) :: this !< particle

    this%xorigin = DZERO
    this%yorigin = DZERO
    this%zorigin = DZERO
    this%sinrot = DZERO
    this%cosrot = DONE
    this%cosrot = DONE
    this%transformed = .false.
  end subroutine reset_transform

  !> @brief Return the particle's model coordinates.
  subroutine get_model_coords(this, x, y, z)
    use GeomUtilModule, only: transform, compose
    class(ParticleType), intent(inout) :: this !< particle
    real(DP), intent(out) :: x !< x coordinate
    real(DP), intent(out) :: y !< y coordinate
    real(DP), intent(out) :: z !< z coordinate

    if (this%transformed) then
      ! Untransform coordinates
      call transform(this%x, this%y, this%z, x, y, z, &
                     this%xorigin, this%yorigin, this%zorigin, &
                     this%sinrot, this%cosrot, invert=.true.)
    else
      x = this%x
      y = this%y
      z = this%z
    end if
  end subroutine get_model_coords

  integer function num_stored(this) result(n)
    class(ParticleStoreType) :: this
    n = size(this%imdl)
  end function num_stored

end module ParticleModule
