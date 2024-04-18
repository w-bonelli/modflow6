module ParticleModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE, LENMEMPATH, LENBOUNDNAME
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, &
                                 mem_reallocate
  implicit none

  private
  public :: ParticleType, ParticleStoreType, &
            create_particle, create_particle_store, &
            get_particle_id

  ! min/max tracking levels (1: model, 2: cell, 3: subcell)
  integer, parameter, public :: levelmin = 0, levelmax = 4

  !> @brief A particle tracked by the PRT model.
  !!
  !! Record-type used mainly for convenience to shuffle
  !! data into and out of storage as tracking proceeds.
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
    ! state
    integer(I4B), allocatable, public :: idomain(:) !< tracking domain hierarchy
    integer(I4B), allocatable, public :: iboundary(:) !< tracking domain boundaries
    integer(I4B), public :: icu !< user cell (node) number
    integer(I4B), public :: ilay !< grid layer
    integer(I4B), public :: izone !< zone number
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
    logical(LGP), public :: transformed !< whether coordinates have been transformed from model to local
    logical(LGP), public :: advancing !< whether particle is still being tracked for current time step
    
    integer(I4B), public :: ivvorig    ! kluge note: devoption for now
    integer(I4B), public :: ifrctrn    ! kluge note: devoption for now
  contains
    procedure, public :: destroy => destroy_particle
    procedure, public :: get_model_coords
    procedure, public :: load_from_store
    procedure, public :: transform => transform_coords
  end type ParticleType

  !> @brief Structure of arrays to store particles.
  type ParticleStoreType
    ! identity
    character(len=LENBOUNDNAME), dimension(:), pointer, contiguous :: name !< optional particle label
    integer(I4B), dimension(:), pointer, contiguous :: imdl !< index of model particle originated in
    integer(I4B), dimension(:), pointer, contiguous :: iprp !< index of release package the particle originated in
    integer(I4B), dimension(:), pointer, contiguous :: irpt !< index of release point in the particle release package the particle originated in
    ! stopping criteria
    integer(I4B), dimension(:), pointer, contiguous :: istopweaksink !< weak sink option: 0 = do not stop, 1 = stop
    integer(I4B), dimension(:), pointer, contiguous :: istopzone !< stop zone number
    ! state
    integer(I4B), dimension(:, :), allocatable :: idomain !< array of indices for domains in the tracking domain hierarchy
    integer(I4B), dimension(:, :), allocatable :: iboundary !< array of indices for tracking domain boundaries
    integer(I4B), dimension(:), pointer, contiguous :: icu !< cell number (user, not reduced)
    integer(I4B), dimension(:), pointer, contiguous :: ilay !< layer
    integer(I4B), dimension(:), pointer, contiguous :: izone !< current zone number
    integer(I4B), dimension(:), pointer, contiguous :: istatus !< particle status
    real(DP), dimension(:), pointer, contiguous :: x !< model x coord of particle
    real(DP), dimension(:), pointer, contiguous :: y !< model y coord of particle
    real(DP), dimension(:), pointer, contiguous :: z !< model z coord of particle
    real(DP), dimension(:), pointer, contiguous :: trelease !< particle release time
    real(DP), dimension(:), pointer, contiguous :: tstop !< particle stop time
    real(DP), dimension(:), pointer, contiguous :: ttrack !< current tracking time
    
    integer(I4B), dimension(:), pointer, contiguous :: ivvorig   ! kluge note: devoption for now
    integer(I4B), dimension(:), pointer, contiguous :: ifrctrn   ! kluge note: devoption for now
  contains
    procedure, public :: destroy => destroy_store
    procedure, public :: resize => resize_store
    procedure, public :: load_from_particle
  end type ParticleStoreType

contains

  !> @brief Create a new particle
  subroutine create_particle(particle)
    type(ParticleType), pointer :: particle !< particle
    allocate (particle)
    allocate (particle%idomain(levelmin:levelmax))
    allocate (particle%iboundary(levelmin:levelmax))
  end subroutine create_particle

  !> @brief Destroy a particle
  subroutine destroy_particle(this)
    class(ParticleType), intent(inout) :: this !< particle
    deallocate (this%idomain)
    deallocate (this%iboundary)
  end subroutine destroy_particle

  !> @brief Create a new particle store
  subroutine create_particle_store(this, np, mempath)
    type(ParticleStoreType), pointer :: this !< store
    integer(I4B), intent(in) :: np !< number of particles
    character(*), intent(in) :: mempath !< path to memory

    allocate (this)
    call mem_allocate(this%imdl, np, 'PLIMDL', mempath)
    call mem_allocate(this%irpt, np, 'PLIRPT', mempath)
    call mem_allocate(this%iprp, np, 'PLIPRP', mempath)
    call mem_allocate(this%name, LENBOUNDNAME, np, 'PLNAME', mempath)
    ! -- kluge todo: update mem_allocate to allow custom range of indices?
    !    e.g. here we want to allocate 0-4 for trackdomain levels, not 1-5
    allocate (this%idomain(np, levelmin:levelmax))
    allocate (this%iboundary(np, levelmin:levelmax))
    call mem_allocate(this%icu, np, 'PLICU', mempath)
    call mem_allocate(this%ilay, np, 'PLILAY', mempath)
    call mem_allocate(this%izone, np, 'PLIZONE', mempath)
    call mem_allocate(this%istatus, np, 'PLISTATUS', mempath)
    call mem_allocate(this%x, np, 'PLX', mempath)
    call mem_allocate(this%y, np, 'PLY', mempath)
    call mem_allocate(this%z, np, 'PLZ', mempath)
    call mem_allocate(this%trelease, np, 'PLTRELEASE', mempath)
    call mem_allocate(this%tstop, np, 'PLTSTOP', mempath)
    call mem_allocate(this%ttrack, np, 'PLTTRACK', mempath)
    call mem_allocate(this%istopweaksink, np, 'PLISTOPWEAKSINK', mempath)
    call mem_allocate(this%istopzone, np, 'PLISTOPZONE', mempath)
    
    call mem_allocate(this%ivvorig, np, 'PLIVVORIG', mempath)  ! kluge note: devoption for now
    call mem_allocate(this%ifrctrn, np, 'PLIFRCTRN', mempath)  ! kluge note: devoption for now
  end subroutine create_particle_store

  !> @brief Deallocate particle arrays
  subroutine destroy_store(this, mempath)
    class(ParticleStoreType), intent(inout) :: this !< store
    character(*), intent(in) :: mempath !< path to memory

    call mem_deallocate(this%imdl, 'PLIMDL', mempath)
    call mem_deallocate(this%iprp, 'PLIPRP', mempath)
    call mem_deallocate(this%irpt, 'PLIRPT', mempath)
    call mem_deallocate(this%name, 'PLNAME', mempath)
    deallocate (this%idomain)
    deallocate (this%iboundary)
    call mem_deallocate(this%icu, 'PLICU', mempath)
    call mem_deallocate(this%ilay, 'PLILAY', mempath)
    call mem_deallocate(this%izone, 'PLIZONE', mempath)
    call mem_deallocate(this%istatus, 'PLISTATUS', mempath)
    call mem_deallocate(this%x, 'PLX', mempath)
    call mem_deallocate(this%y, 'PLY', mempath)
    call mem_deallocate(this%z, 'PLZ', mempath)
    call mem_deallocate(this%trelease, 'PLTRELEASE', mempath)
    call mem_deallocate(this%tstop, 'PLTSTOP', mempath)
    call mem_deallocate(this%ttrack, 'PLTTRACK', mempath)
    call mem_deallocate(this%istopweaksink, 'PLISTOPWEAKSINK', mempath)
    call mem_deallocate(this%istopzone, 'PLISTOPZONE', mempath)

    call mem_deallocate(this%ivvorig, 'PLIVVORIG', mempath)  ! kluge note: devoption for now
    call mem_deallocate(this%ifrctrn, 'PLIFRCTRN', mempath)  ! kluge note: devoption for now
  end subroutine destroy_store

  !> @brief Reallocate particle arrays
  subroutine resize_store(this, np, mempath)
    ! -- modules
    use ArrayHandlersModule, only: ExpandArray2D
    ! -- dummy
    class(ParticleStoreType), intent(inout) :: this !< particle store
    integer(I4B), intent(in) :: np !< number of particles
    character(*), intent(in) :: mempath !< path to memory

    ! resize 1D arrays
    call mem_reallocate(this%imdl, np, 'PLIMDL', mempath)
    call mem_reallocate(this%iprp, np, 'PLIPRP', mempath)
    call mem_reallocate(this%irpt, np, 'PLIRPT', mempath)
    call mem_reallocate(this%name, LENBOUNDNAME, np, 'PLNAME', mempath)
    call mem_reallocate(this%icu, np, 'PLICU', mempath)
    call mem_reallocate(this%ilay, np, 'PLILAY', mempath)
    call mem_reallocate(this%izone, np, 'PLIZONE', mempath)
    call mem_reallocate(this%istatus, np, 'PLISTATUS', mempath)
    call mem_reallocate(this%x, np, 'PLX', mempath)
    call mem_reallocate(this%y, np, 'PLY', mempath)
    call mem_reallocate(this%z, np, 'PLZ', mempath)
    call mem_reallocate(this%trelease, np, 'PLTRELEASE', mempath)
    call mem_reallocate(this%tstop, np, 'PLTSTOP', mempath)
    call mem_reallocate(this%ttrack, np, 'PLTTRACK', mempath)
    call mem_reallocate(this%istopweaksink, np, 'PLISTOPWEAKSINK', mempath)
    call mem_reallocate(this%istopzone, np, 'PLISTOPZONE', mempath)
    
    call mem_reallocate(this%ivvorig, np, 'PLIVVORIG', mempath)    ! kluge note: devoption for now
    call mem_reallocate(this%ifrctrn, np, 'PLIFRCTRN', mempath)    ! kluge note: devoption for now
    
    ! resize first dimension of 2D arrays
    ! todo: memory manager support?
    call ExpandArray2D( &
      this%idomain, &
      np - size(this%idomain, 1), &
      0)
    call ExpandArray2D( &
      this%iboundary, &
      np - size(this%iboundary, 1), &
      0)
  end subroutine resize_store

  !> @brief Initialize particle from particle list.
  !!
  !! This routine is used to initialize a particle from the list
  !! so it can be tracked by prt_solve. The particle's advancing
  !! flag is set and local coordinate transformations are reset.
  !<
  subroutine load_from_store(this, store, imdl, iprp, ip)
    class(ParticleType), intent(inout) :: this !< particle
    type(ParticleStoreType), intent(in) :: store !< particle storage
    integer(I4B), intent(in) :: imdl !< index of model particle originated in
    integer(I4B), intent(in) :: iprp !< index of particle release package particle originated in
    integer(I4B), intent(in) :: ip !< index into the particle list

    call this%transform(reset=.true.)
    this%imdl = imdl
    this%iprp = iprp
    this%irpt = store%irpt(ip)
    this%ip = ip
    this%name = store%name(ip)
    this%istopweaksink = store%istopweaksink(ip)
    this%istopzone = store%istopzone(ip)
    this%icu = store%icu(ip)
    this%ilay = store%ilay(ip)
    this%izone = store%izone(ip)
    this%istatus = store%istatus(ip)
    this%x = store%x(ip)
    this%y = store%y(ip)
    this%z = store%z(ip)
    this%trelease = store%trelease(ip)
    this%tstop = store%tstop(ip)
    this%ttrack = store%ttrack(ip)
    this%advancing = .true.
    this%idomain(levelmin:levelmax) = &
      store%idomain(ip, levelmin:levelmax)
    this%idomain(1) = imdl
    this%iboundary(levelmin:levelmax) = &
      store%iboundary(ip, levelmin:levelmax)
    
    this%ivvorig = store%ivvorig(ip)  ! kluge note: devoption for now
    this%ifrctrn = store%ifrctrn(ip)  ! kluge note: devoption for now
  end subroutine load_from_store

  !> @brief Update particle store from particle
  subroutine load_from_particle(this, particle, ip)
    class(ParticleStoreType), intent(inout) :: this !< particle storage
    type(ParticleType), intent(in) :: particle !< particle
    integer(I4B), intent(in) :: ip !< particle index

    this%imdl(ip) = particle%imdl
    this%iprp(ip) = particle%iprp
    this%irpt(ip) = particle%irpt
    this%name(ip) = particle%name
    this%istopweaksink(ip) = particle%istopweaksink
    this%istopzone(ip) = particle%istopzone
    this%icu(ip) = particle%icu
    this%ilay(ip) = particle%ilay
    this%izone(ip) = particle%izone
    this%istatus(ip) = particle%istatus
    this%x(ip) = particle%x
    this%y(ip) = particle%y
    this%z(ip) = particle%z
    this%trelease(ip) = particle%trelease
    this%tstop(ip) = particle%tstop
    this%ttrack(ip) = particle%ttrack
    this%idomain( &
      ip, &
      levelmin:levelmax) = &
      particle%idomain(levelmin:levelmax)
    this%iboundary( &
      ip, &
      levelmin:levelmax) = &
      particle%iboundary(levelmin:levelmax)
    
    this%ivvorig = particle%ivvorig  ! kluge note: devoption for now
    this%ifrctrn = particle%ifrctrn  ! kluge note: devoption for now
  end subroutine load_from_particle

  !> @brief Apply the given global-to-local transformation to the particle.
  subroutine transform_coords(this, xorigin, yorigin, zorigin, &
                              sinrot, cosrot, invert, reset)
    use GeomUtilModule, only: transform, compose
    class(ParticleType), intent(inout) :: this !< particle
    real(DP), intent(in), optional :: xorigin !< x coordinate of origin
    real(DP), intent(in), optional :: yorigin !< y coordinate of origin
    real(DP), intent(in), optional :: zorigin !< z coordinate of origin
    real(DP), intent(in), optional :: sinrot !< sine of rotation angle
    real(DP), intent(in), optional :: cosrot !< cosine of rotation angle
    logical(LGP), intent(in), optional :: invert !< whether to invert
    logical(LGP), intent(in), optional :: reset !< whether to reset

    ! -- reset if requested
    if (present(reset)) then
      if (reset) then
        this%xorigin = DZERO
        this%yorigin = DZERO
        this%zorigin = DZERO
        this%sinrot = DZERO
        this%cosrot = DONE
        this%cosrot = DONE
        this%transformed = .false.
        return
      end if
    end if

    ! -- Otherwise, transform coordinates
    call transform(this%x, this%y, this%z, &
                   this%x, this%y, this%z, &
                   xorigin, yorigin, zorigin, &
                   sinrot, cosrot, invert)

    ! -- Modify transformation from model coordinates to particle's new
    ! -- local coordinates by incorporating this latest transformation
    call compose(this%xorigin, this%yorigin, this%zorigin, &
                 this%sinrot, this%cosrot, &
                 xorigin, yorigin, zorigin, &
                 sinrot, cosrot, invert)

    ! -- Set isTransformed flag to true. Note that there is no check
    ! -- to see whether the modification brings the coordinates back
    ! -- to model coordinates (in which case the origin would be very
    ! -- close to zero and sinrot and cosrot would be very close to 0.
    ! -- and 1., respectively, allowing for roundoff error).
    this%transformed = .true.
  end subroutine transform_coords

  !> @brief Return the particle's model (global) coordinates.
  subroutine get_model_coords(this, x, y, z)
    use GeomUtilModule, only: transform, compose
    class(ParticleType), intent(inout) :: this !< particle
    real(DP), intent(out) :: x !< x coordinate
    real(DP), intent(out) :: y !< y coordinate
    real(DP), intent(out) :: z !< z coordinate

    if (this%transformed) then
      ! -- Transform back from local to model coordinates
      call transform(this%x, this%y, this%z, x, y, z, &
                     this%xorigin, this%yorigin, this%zorigin, &
                     this%sinrot, this%cosrot, .true.)
    else
      ! -- Already in model coordinates
      x = this%x
      y = this%y
      z = this%z
    end if
  end subroutine get_model_coords

  !> @brief Return the particle's composite ID.
  !!
  !! Particles are uniquely identified by model index, PRP index,
  !! location index, and release time.
  !<
  pure function get_particle_id(particle) result(id)
    class(ParticleType), intent(in) :: particle !< particle
    character(len=LENMEMPATH) :: id !< particle id

    write (id, '(I0,"-",I0,"-",I0,"-",F0.0)') &
      particle%imdl, particle%iprp, particle%irpt, particle%trelease
  end function get_particle_id

end module ParticleModule
