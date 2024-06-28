module PrtPrpModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DEM1, DONE, LENFTYPE, LINELENGTH, &
                             LENBOUNDNAME, LENPAKLOC, TABLEFT, TABCENTER, &
                             MNORMAL
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use TableModule, only: TableType, table_cr
  use TimeSeriesModule, only: TimeSeriesType
  use TimeSeriesRecordModule, only: TimeSeriesRecordType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  use PrtFmiModule, only: PrtFmiType
  use ParticleModule, only: ParticleType, ParticleStoreType, &
                            create_particle, allocate_particle_store
  use SimModule, only: count_errors, store_error, store_error_unit, &
                       store_warning
  use SimVariablesModule, only: errmsg, warnmsg
  use TrackModule, only: TrackFileControlType
  use GeomUtilModule, only: point_in_polygon, get_ijk, get_jk
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, &
                                 mem_reallocate
  use TimeSelectModule, only: TimeSelectType

  implicit none

  private
  public :: PrtPrpType
  public :: prp_create

  character(len=LENFTYPE) :: ftype = 'PRP'
  character(len=16) :: text = '             PRP'

  !> @brief Particle release point (PRP) package
  type, extends(BndType) :: PrtPrpType
    type(PrtFmiType), pointer :: fmi => null() !< flow model interface
    type(ParticleStoreType), pointer :: particles => null() !< particle store
    type(TrackFileControlType), pointer :: trackfilectl => null() !< track file control
    integer(I4B), pointer :: nreleasepts => null() !< number of release points
    integer(I4B), pointer :: nparticles => null() !< number of particles released
    integer(I4B), pointer :: istopweaksink => null() !< weak sink option: 0 = no stop, 1 = stop
    integer(I4B), pointer :: istopzone => null() !< optional stop zone number: 0 = no stop zone
    integer(I4B), pointer :: idrape => null() !< drape option: 0 = do not drape, 1 = drape to topmost active cell
    integer(I4B), pointer :: itrkout => null() !< binary track file
    integer(I4B), pointer :: itrkhdr => null() !< track header file
    integer(I4B), pointer :: itrkcsv => null() !< CSV track file
    integer(I4B), pointer :: irlstls => null() !< release time file
    logical(LGP), pointer :: localz => null() !< compute z coordinates local to the cell
    integer(I4B), pointer :: extend => null() !< extend tracking beyond simulation's end
    logical(LGP), pointer :: rlsall => null() !< release in all time step
    logical(LGP), pointer :: rlsfirst => null() !< release in first time step
    logical(LGP), pointer :: rlstimelist => null() !< use global release time
    real(DP), pointer :: offset => null() !< release time offset
    real(DP), pointer :: stoptime => null() !< stop time for all release points
    real(DP), pointer :: stoptraveltime => null() !< stop travel time for all points
    integer(I4B), pointer, contiguous :: rlskstp(:) !< time steps selected for release
    integer(I4B), pointer, contiguous :: rptnode(:) => null() !< release point reduced nns
    integer(I4B), pointer, contiguous :: rptzone(:) => null() !< release point zone numbers
    real(DP), pointer, contiguous :: rptx(:) => null() !< release point x coordinates
    real(DP), pointer, contiguous :: rpty(:) => null() !< release point y coordinates
    real(DP), pointer, contiguous :: rptz(:) => null() !< release point z coordinates
    real(DP), pointer, contiguous :: locz(:) => null() !< release point local z coordinates
    real(DP), pointer, contiguous :: rptmass(:) => null() !< total mass released from point
    character(len=LENBOUNDNAME), pointer, contiguous :: rptname(:) => null() !< release point names
    type(TimeSelectType), pointer :: releasetimes !< user-specified particle release times
    integer(I4B), pointer :: ifrctrn => null() !< force ternary solution for quad grids
    integer(I4B), pointer :: iexmethod => null() !< method for iterative solution of particle exit location and time in generalized Pollock's method
    real(DP), pointer :: extol => null() !< tolerance for iterative solution of particle exit location and time in generalized Pollock's method
    logical(LGP), pointer :: foundtol => null() !< whether tolerance option was found

  contains
    procedure :: prp_allocate_arrays
    procedure :: prp_allocate_scalars
    procedure :: bnd_ar => prp_ar
    procedure :: bnd_ad => prp_ad
    procedure :: bnd_rp => prp_rp
    procedure :: bnd_cq_simrate => prp_cq_simrate
    procedure :: bnd_da => prp_da
    procedure :: define_listlabel
    procedure :: prp_set_pointers
    procedure :: bnd_options => prp_options
    procedure :: read_dimensions => prp_read_dimensions
    procedure :: prp_read_packagedata
    procedure, public :: bnd_obs_supported => prp_obs_supported
    procedure, public :: bnd_df_obs => prp_df_obs
  end type PrtPrpType

contains

  !> @brief Create a new particle release point package
  subroutine prp_create(packobj, id, ibcnum, inunit, iout, namemodel, &
                        pakname, fmi)
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    type(PrtFmiType), pointer :: fmi
    ! -- local
    type(PrtPrpType), pointer :: prpobj
    ! -- formats
    character(len=*), parameter :: fmtheader = &
      "(1x, /1x, 'PRP -- PARTICLE RELEASE POINT PACKAGE', &
       &' INPUT READ FROM UNIT ', i0, /)"

    ! -- allocate the object and assign values to object variables
    allocate (prpobj)
    packobj => prpobj

    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    prpobj%text = text

    ! -- allocate scalars
    call prpobj%prp_allocate_scalars()

    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 4
    packobj%iscloc = 1

    ! -- store pointer to flow model interface
    prpobj%fmi => fmi

    ! -- if prp is enabled, print a message identifying it
    if (inunit > 0) write (iout, fmtheader) inunit
  end subroutine prp_create

  !> @brief Deallocate memory
  subroutine prp_da(this)
    class(PrtPrpType) :: this

    ! -- deallocate parent
    call this%BndType%bnd_da()

    ! -- deallocate scalars
    call mem_deallocate(this%rlsall)
    call mem_deallocate(this%rlsfirst)
    call mem_deallocate(this%rlstimelist)
    call mem_deallocate(this%localz)
    call mem_deallocate(this%extend)
    call mem_deallocate(this%offset)
    call mem_deallocate(this%stoptime)
    call mem_deallocate(this%stoptraveltime)
    call mem_deallocate(this%istopweaksink)
    call mem_deallocate(this%istopzone)
    call mem_deallocate(this%idrape)
    call mem_deallocate(this%nreleasepts)
    call mem_deallocate(this%nparticles)
    call mem_deallocate(this%itrkout)
    call mem_deallocate(this%itrkhdr)
    call mem_deallocate(this%itrkcsv)
    call mem_deallocate(this%irlstls)
    call mem_deallocate(this%ifrctrn)
    call mem_deallocate(this%iexmethod)
    call mem_deallocate(this%extol)
    call mem_deallocate(this%foundtol)

    ! -- deallocate arrays
    call mem_deallocate(this%rptx)
    call mem_deallocate(this%rpty)
    call mem_deallocate(this%rptz)
    call mem_deallocate(this%locz)
    call mem_deallocate(this%rptnode)
    call mem_deallocate(this%rptmass)
    call mem_deallocate(this%rlskstp)
    call mem_deallocate(this%rptname, 'RPTNAME', this%memoryPath)

    ! -- deallocate particle store
    call this%particles%deallocate(this%memoryPath)
    deallocate (this%particles)

    ! -- deallocate release time selection
    call this%releasetimes%deallocate()
    deallocate (this%releasetimes)
  end subroutine prp_da

  !> @ brief Set pointers to model variables
  subroutine prp_set_pointers(this, ibound, izone, trackfilectl)
    ! -- dummy variables
    class(PrtPrpType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    integer(I4B), dimension(:), pointer, contiguous :: izone
    type(TrackFileControlType), pointer :: trackfilectl

    this%ibound => ibound
    this%rptzone => izone
    this%trackfilectl => trackfilectl
  end subroutine prp_set_pointers

  !> @brief Allocate arrays
  subroutine prp_allocate_arrays(this, nodelist, auxvar)
    ! -- dummy
    class(PrtPrpType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
    integer(I4B) :: nps

    ! -- Allocate particle store, starting with the number
    !    of release points (arrays resized if/when needed)
    call allocate_particle_store( &
      this%particles, &
      this%nreleasepts, &
      this%memoryPath)

    ! -- Allocate arrays
    call mem_allocate(this%rptx, this%nreleasepts, 'RPTX', this%memoryPath)
    call mem_allocate(this%rpty, this%nreleasepts, 'RPTY', this%memoryPath)
    call mem_allocate(this%rptz, this%nreleasepts, 'RPTZ', this%memoryPath)
    call mem_allocate(this%locz, this%nreleasepts, 'LOCZ', this%memoryPath)
    call mem_allocate(this%rptmass, this%nreleasepts, 'RPTMASS', this%memoryPath)
    call mem_allocate(this%rptnode, this%nreleasepts, 'RPTNODER', &
                      this%memoryPath)
    call mem_allocate(this%rlskstp, 1, 'RLSKSTP', this%memoryPath)
    call mem_allocate(this%rptname, LENBOUNDNAME, this%nreleasepts, &
                      'RPTNAME', this%memoryPath)

    ! -- Initialize arrays
    this%rlskstp(1) = 1 ! single release in first time step by default
    do nps = 1, this%nreleasepts
      this%rptmass(nps) = DZERO
    end do
  end subroutine prp_allocate_arrays

  !> @brief Allocate scalars
  subroutine prp_allocate_scalars(this)
    class(PrtPrpType) :: this

    ! -- Allocate release time selection
    allocate (this%releasetimes)
    call this%releasetimes%init()

    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()

    ! -- Allocate scalars for this type
    call mem_allocate(this%rlsall, 'RLSALL', this%memoryPath)
    call mem_allocate(this%rlsfirst, 'RLSFIRST', this%memoryPath)
    call mem_allocate(this%rlstimelist, 'RELEASETIME', this%memoryPath)
    call mem_allocate(this%localz, 'LOCALZ', this%memoryPath)
    call mem_allocate(this%extend, 'EXTEND', this%memoryPath)
    call mem_allocate(this%offset, 'OFFSET', this%memoryPath)
    call mem_allocate(this%stoptime, 'STOPTIME', this%memoryPath)
    call mem_allocate(this%stoptraveltime, 'STOPTRAVELTIME', this%memoryPath)
    call mem_allocate(this%istopweaksink, 'ISTOPWEAKSINK', this%memoryPath)
    call mem_allocate(this%istopzone, 'ISTOPZONE', this%memoryPath)
    call mem_allocate(this%idrape, 'IDRAPE', this%memoryPath)
    call mem_allocate(this%nreleasepts, 'NRELEASEPTS', this%memoryPath)
    call mem_allocate(this%nparticles, 'NPART', this%memoryPath)
    call mem_allocate(this%itrkout, 'ITRKOUT', this%memoryPath)
    call mem_allocate(this%itrkhdr, 'ITRKHDR', this%memoryPath)
    call mem_allocate(this%itrkcsv, 'ITRKCSV', this%memoryPath)
    call mem_allocate(this%irlstls, 'IRLSTLS', this%memoryPath)
    call mem_allocate(this%ifrctrn, 'IFRCTRN', this%memoryPath)
    call mem_allocate(this%iexmethod, 'IEXMETHOD', this%memoryPath)
    call mem_allocate(this%extol, 'IEXTOL', this%memoryPath)
    call mem_allocate(this%foundtol, 'FOUNDTOL', this%memoryPath)

    ! -- Set values
    this%rlsall = .false.
    this%rlsfirst = .false.
    this%rlstimelist = .false.
    this%localz = .false.
    this%extend = 0
    this%offset = DZERO
    this%stoptime = huge(1d0)
    this%stoptraveltime = huge(1d0)
    this%istopweaksink = 0
    this%istopzone = 0
    this%idrape = 0
    this%nreleasepts = 0
    this%nparticles = 0
    this%itrkout = 0
    this%itrkhdr = 0
    this%itrkcsv = 0
    this%irlstls = 0
    this%ifrctrn = 0
    this%iexmethod = 1
    this%extol = DZERO
    this%foundtol = .false. ! exit_solve_tolerance is a required option
  end subroutine prp_allocate_scalars

  !> @ brief Allocate and read period data
  subroutine prp_ar(this)
    ! -- dummy variables
    class(PrtPrpType), intent(inout) :: this
    ! -- local variables
    integer(I4B) :: n

    call this%obs%obs_ar()
    call this%BndType%allocate_arrays()
    if (this%inamedbound /= 0) then
      do n = 1, this%nreleasepts
        this%boundname(n) = this%rptname(n)
      end do
    end if
    do n = 1, this%nreleasepts
      this%nodelist(n) = this%rptnode(n)
    end do
    ! if (this%imover /= 0) then
    !   allocate(this%pakmvrobj)
    !   call this%pakmvrobj%ar(this%maxbound, this%maxbound, this%memoryPath)
    ! endif
  end subroutine prp_ar

  !> @brief Advance a time step and release particles if appropriate.
  !!
  !! Releases may be scheduled via a global RELEASETIME, or within a
  !! stress period via ALL, FIRST, FREQUENCY or STEPS (with optional
  !! FRACTION). If no release option is specified, a single release
  !! is conducted at the first moment of the first time step of the
  !! first stress period.
  !<
  subroutine prp_ad(this)
    ! -- modules
    use TdisModule, only: totimc, delt, kstp
    use DisModule, only: DisType
    use DisvModule, only: DisvType
    ! -- dummy
    class(PrtPrpType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: ic, icu, nps, nts, nrel, &
                    nreleasets, np, irow, icol, ilay, icpl
    real(DP) :: x, y, z, trelease, tend, top, bot, hds
    real(DP), allocatable :: polyverts(:, :)
    type(ParticleType), pointer :: particle

    ! -- Check if there's a release to make
    if (.not. ( &
        ! all time steps?
        this%rlsall .or. &
        ! first time step?
        (this%rlsfirst .and. kstp == 1) .or. &
        ! specified time steps?
        any(this%rlskstp == kstp) .or. &
        ! specified release times?
        this%rlstimelist)) return

    if (this%rlstimelist) then
      nreleasets = size(this%releasetimes%times)
    else
      nreleasets = 1
    end if
    nrel = this%nreleasepts * nreleasets

    ! -- Reset mass release for time step
    do nps = 1, this%nreleasepts
      this%rptmass(nps) = DZERO
    end do

    ! -- Resize particle store if another set
    !    of particles will exceed its capacity
    if ((this%nparticles + nrel) > size(this%particles%irpt)) &
      call this%particles%resize( &
      size(this%particles%irpt) + nrel, &
      this%memoryPath)

    ! -- Release a particle from each point...
    do nps = 1, this%nreleasepts
      ic = this%rptnode(nps)
      icu = this%dis%get_nodeuser(ic)
      ! -- ...for each release time in the current time step
      tsloop: do nts = 1, nreleasets
        if (this%rlstimelist) then
          trelease = this%releasetimes%times(nts)
          tend = totimc + delt
          if (trelease < totimc .or. trelease >= tend) cycle tsloop
        else
          trelease = totimc + this%offset * delt
        end if

        np = this%nparticles + 1
        this%nparticles = np

        ! -- Particle release point. Calculate global from local z if needed.
        x = this%rptx(nps)
        y = this%rpty(nps)
        if (this%localz) then
          top = this%fmi%dis%top(ic)
          bot = this%fmi%dis%bot(ic)
          hds = this%fmi%gwfhead(ic)
          z = bot + this%rptz(nps) * (hds - bot)
        else
          z = this%rptz(nps)
        end if

        ! -- Check release point is within the specified cell
        !    and not above/below grid top/bottom respectively
        call this%dis%get_polyverts(ic, polyverts)
        if (.not. point_in_polygon(x, y, polyverts)) then
          write (errmsg, '(a,g0,a,g0,a,i0)') &
            'Error: release point (x=', x, ', y=', y, ') is not in cell ', icu
          call store_error(errmsg, terminate=.false.)
          call store_error_unit(this%inunit, terminate=.true.)
        end if
        if (z > maxval(this%dis%top)) then
          write (errmsg, '(a,g0,a,g0,a,i0)') &
            'Error: release point (z=', z, ') is above grid top ', &
            maxval(this%dis%top)
          call store_error(errmsg, terminate=.false.)
          call store_error_unit(this%inunit, terminate=.true.)
        else if (z < minval(this%dis%bot)) then
          write (errmsg, '(a,g0,a,g0,a,i0)') &
            'Error: release point (z=', z, ') is below grid bottom ', &
            minval(this%dis%bot)
          call store_error(errmsg, terminate=.false.)
          call store_error_unit(this%inunit, terminate=.true.)
        end if

        ! -- Initialize particle and add it to particle store
        ! -- Todo: branch depending on exchange PRP or a normal PRP.
        !    if exchange PRP, particle identity properties should be
        !    passed in (e.g. imdl, iprp, irpt, trelease, name).
        !    if normal PRP, imdl and iprp should be set from pointers
        !    provided to PRP by PRT model; irpt and trelease as below.
        allocate (particle)
        call create_particle(particle)
        if (size(this%boundname) /= 0) then
          particle%name = this%boundname(nps)
        else
          particle%name = ''
        end if
        particle%irpt = nps
        particle%istopweaksink = this%istopweaksink
        particle%istopzone = this%istopzone
        particle%icu = icu
        select type (dis => this%dis)
        type is (DisType)
          call get_ijk(icu, dis%nrow, dis%ncol, dis%nlay, irow, icol, ilay)
        type is (DisvType)
          call get_jk(icu, dis%ncpl, dis%nlay, icpl, ilay)
        end select
        particle%ilay = ilay
        particle%izone = this%rptzone(ic)
        particle%istatus = 0
        ! Handle inactive cells
        if (this%ibound(ic) == 0) then
          ! -- If drape option activated, release in highest active
          !    cell vertically below release point.
          if (this%idrape /= 0) &
            call this%dis%highest_active(ic, this%ibound)
          ! -- If returned cell is inactive, do not release particle
          if (this%ibound(ic) == 0) &
            particle%istatus = 8 ! permanently unreleased
        end if
        particle%x = x
        particle%y = y
        particle%z = z
        particle%trelease = trelease
        ! Set stopping time to earlier of times specified by STOPTIME and STOPTRAVELTIME
        if (this%stoptraveltime == huge(1d0)) then
          particle%stoptime = this%stoptime
        else
          particle%stoptime = particle%trelease + this%stoptraveltime
          if (this%stoptime < particle%stoptime) particle%stoptime = this%stoptime
        end if
        particle%ttrack = particle%trelease
        particle%idomain(1) = 0
        particle%iboundary(1) = 0
        particle%idomain(2) = ic
        particle%iboundary(2) = 0
        particle%idomain(3) = 0
        particle%iboundary(3) = 0
        particle%ifrctrn = this%ifrctrn
        particle%iexmethod = this%iexmethod
        particle%extol = this%extol
        particle%extend = this%extend

        ! -- Persist particle to particle store
        call this%particles%save(particle, np)

        ! -- Accumulate mass release from this point
        this%rptmass(nps) = this%rptmass(nps) + DONE
      end do tsloop
    end do
  end subroutine prp_ad

  !> @ brief Read and prepare period data for particle input
  subroutine prp_rp(this)
    ! -- modules
    use TdisModule, only: kper, nper, nstp
    use InputOutputModule, only: urword
    ! -- dummy variables
    class(PrtPrpType), intent(inout) :: this
    ! -- local variables
    integer(I4B) :: ierr
    integer(I4B) :: n, i
    integer(I4B) :: lloc, istart, istop, ival
    real(DP) :: dval
    logical(LGP) :: isfound
    logical(LGP) :: endOfBlock
    logical(LGP) :: use_last
    logical(LGP) :: noperiodblocks
    character(len=LINELENGTH) :: keyword
    character(len=:), allocatable :: line
    ! -- formats
    character(len=*), parameter :: fmtblkerr = &
                      "('Looking for BEGIN PERIOD iper.  &
                      &Found ', a, ' instead.')"
    character(len=*), parameter :: fmt_steps = &
                                   "(6x,'TIME STEP(S) ',50(I0,' '))" ! 50 limit is similar to STEPS in OC
    character(len=*), parameter :: fmt_freq = &
                                   "(6x,'EVERY ',I0,' TIME STEP(S)')"
    character(len=*), parameter :: fmt_fracs = &
                                   "(6x,50(f10.3,' '))"

    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if (this%inunit == 0) return

    ! -- get stress period data
    noperiodblocks = .false.
    if (this%ionper < kper) then
      ! -- get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true., &
                                blockRequired=.false.)
      if (isfound) then
        ! -- read ionper and check for increasing period numbers
        call this%read_check_ionper()
      else
        ! -- PERIOD block not found
        if (ierr < 0) then
          if (kper == 1) then
            ! -- End of file found; no period data for the simulation.
            noperiodblocks = .true.
          else
            ! -- End of file found; no more period data.
            this%ionper = nper + 1
          end if
        else
          ! -- Found invalid block
          call this%parser%GetCurrentLine(line)
          write (errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg, terminate=.TRUE.)
        end if
      end if
    end if

    ! -- If no period data for the simulation default to single
    !    release at beginning of first period's first time step.
    !    Otherwise read release timing settings from the period
    !    data block of the package input file.
    if (noperiodblocks) then
      if (kper == 1) then
        call mem_reallocate(this%rlskstp, 1, &
                            "RLSKSTP", this%memoryPath)
        this%rlsfirst = .true.
        use_last = .false.
      end if
      ! -- If the current stress period matches the
      !    block we are reading continue parsing it
    else if (this%ionper == kper) then
      use_last = .false.
      recordloop: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('ALL')
          this%rlsall = .true.
        case ('STEPS')
          call mem_reallocate(this%rlskstp, 0, &
                              "RLSKSTP", this%memoryPath)
          call this%parser%GetRemainingLine(line)
          lloc = 1
          stepslistsearch: do
            call urword(line, lloc, istart, istop, 2, ival, dval, -1, 0)
            if (ival > 0) then
              n = size(this%rlskstp)
              call mem_reallocate(this%rlskstp, n + 1, &
                                  'RLSKSTP', this%memoryPath)
              this%rlskstp(n + 1) = ival
              cycle stepslistsearch
            end if
            exit stepslistsearch
          end do stepslistsearch
        case ('FIRST')
          this%rlsfirst = .true.
        case ('FREQUENCY')
          ival = this%parser%GetInteger()
          if (ival < 0) then
            errmsg = "FREQUENCY must be non-negative"
            call store_error(errmsg)
            call this%parser%StoreErrorUnit(terminate=.true.)
          end if
          call mem_reallocate(this%rlskstp, 0, 'RLSKSTP', this%memoryPath)
          do i = 1, nstp(this%ionper)
            if (mod(i, ival) == 0) then
              n = size(this%rlskstp)
              call mem_reallocate(this%rlskstp, n + 1, &
                                  'RLSKSTP', this%memoryPath)
              this%rlskstp(n + 1) = i
            end if
          end do
        case ('FRACTION')
          dval = this%parser%GetDouble()
          this%offset = dval
        case default
          write (errmsg, '(2a)') &
            'Looking for ALL, STEPS, FIRST, FREQUENCY, or FRACTION. Found: ', &
            trim(adjustl(keyword))
          call store_error(errmsg, terminate=.TRUE.)
        end select
      end do recordloop
    else
      ! -- else repeat period settings
      use_last = .true.
    end if

    ! -- write settings to list file
    if (this%iprpak > 0) then
      if (.not. any(this%rlskstp > 0)) then
        write (this%iout, "(1x,/1x,a)") 'NO PARTICLE RELEASES IN THIS STRESS '// &
          'PERIOD'
      else if (use_last) then
        write (this%iout, "(1x,/1x,a)") 'REUSING PARTICLE RELEASE SETTINGS '// &
          'FROM LAST STRESS PERIOD'
      else
        ! -- write particle release setting
        write (this%iout, "(1x,/1x,a)", advance='no') 'PARTICLE RELEASE:'
        if (any(this%rlskstp > 0)) then
          n = size(this%rlskstp)
          if (n > 0) write (this%iout, fmt_steps, advance='no') this%rlskstp
        end if
        write (this%iout, "(1x,a)", advance='no') 'AT OFFSET'
        write (this%iout, fmt_fracs) (/this%offset/)
        write (this%iout, '(A)')
      end if
    end if
  end subroutine prp_rp

  !> @ brief Calculate flow between package and model.
  subroutine prp_cq_simrate(this, hnew, flowja, imover)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy variables
    class(PrtPrpType) :: this
    real(DP), dimension(:), intent(in) :: hnew
    real(DP), dimension(:), intent(inout) :: flowja !< flow between package and model
    integer(I4B), intent(in) :: imover !< flag indicating if the mover package is active
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: node
    integer(I4B) :: idiag
    real(DP) :: rrate

    ! -- If no boundaries, skip flow calculations.
    if (this%nbound <= 0) return

    ! -- Loop through each boundary calculating flow.
    do i = 1, this%nbound
      node = this%nodelist(i)
      rrate = DZERO
      ! -- If cell is no-flow or constant-head, then ignore it.
      if (node > 0) then
        ! -- Calculate the flow rate into the cell.
        idiag = this%dis%con%ia(node)
        rrate = this%rptmass(i) * (DONE / delt) ! reciprocal of tstp length
        flowja(idiag) = flowja(idiag) + rrate
      end if

      ! -- Save simulated value to simvals array.
      this%simvals(i) = rrate
    end do
  end subroutine prp_cq_simrate

  subroutine define_listlabel(this)
    class(PrtPrpType), intent(inout) :: this
    ! not implemented, not used
  end subroutine define_listlabel

  !> @brief Indicates whether observations are supported.
  logical function prp_obs_supported(this)
    class(PrtPrpType) :: this
    prp_obs_supported = .true.
  end function prp_obs_supported

  !> @brief Store supported observations
  subroutine prp_df_obs(this)
    ! -- dummy
    class(PrtPrpType) :: this
    ! -- local
    integer(I4B) :: indx
    call this%obs%StoreObsType('prp', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor

    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
  end subroutine prp_df_obs

  !> @brief Set options specific to PrtPrpType
  subroutine prp_options(this, option, found)
    use OpenSpecModule, only: access, form
    use ConstantsModule, only: MAXCHARLEN, DZERO
    use InputOutputModule, only: urword, getunit, openfile
    use TrackModule, only: TRACKHEADER, TRACKDTYPES
    ! -- dummy
    class(PrtPrpType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical(LGP), intent(inout) :: found
    ! -- locals
    real(DP) :: dval
    integer(I4B) :: i, ios, nlines
    logical(LGP) :: success
    character(len=MAXCHARLEN) :: fname
    character(len=MAXCHARLEN) :: keyword
    character(len=:), allocatable :: line
    ! -- formats
    character(len=*), parameter :: fmttrkbin = &
      "(4x, 'PARTICLE TRACKS WILL BE SAVED TO BINARY FILE: ', a, /4x, &
    &'OPENED ON UNIT: ', I0)"
    character(len=*), parameter :: fmttrkcsv = &
      "(4x, 'PARTICLE TRACKS WILL BE SAVED TO CSV FILE: ', a, /4x, &
    &'OPENED ON UNIT: ', I0)"

    select case (option)
    case ('STOPTIME')
      this%stoptime = this%parser%GetDouble()
      found = .true.
    case ('STOPTRAVELTIME')
      this%stoptraveltime = this%parser%GetDouble()
      found = .true.
    case ('STOP_AT_WEAK_SINK')
      this%istopweaksink = 1
      found = .true.
    case ('ISTOPZONE')
      this%istopzone = this%parser%GetInteger()
      found = .true.
    case ('DRAPE')
      this%idrape = 1
      found = .true.
    case ('RELEASE_TIMES')
      if (this%rlstimelist) then
        errmsg = "RELEASE TIMES ALREADY SPECIFIED"
        call store_error(errmsg)
        call this%parser%StoreErrorUnit(terminate=.true.)
      end if
      rtloop: do
        success = .false.
        call this%parser%TryGetDouble(dval, success)
        if (.not. success) exit rtloop
        call this%releasetimes%expand()
        this%releasetimes%times(size(this%releasetimes%times)) = dval
      end do rtloop
      if (.not. this%releasetimes%increasing()) then
        errmsg = "RELEASE TIMES MUST STRICTLY INCREASE"
        call store_error(errmsg)
        call this%parser%StoreErrorUnit(terminate=.true.)
      end if
      this%rlstimelist = .true.
      found = .true.
    case ('RELEASE_TIMESFILE')
      if (this%rlstimelist) then
        errmsg = "RELEASE TIMES ALREADY SPECIFIED"
        call store_error(errmsg)
        call this%parser%StoreErrorUnit(terminate=.true.)
      end if
      call this%parser%GetString(fname)
      call openfile(this%irlstls, this%iout, fname, 'TLS')
      nlines = 0
      rtfloop: do
        read (this%irlstls, '(A)', iostat=ios) line
        if (ios /= 0) exit rtfloop
        nlines = nlines + 1
      end do rtfloop
      call this%releasetimes%expand(nlines)
      rewind (this%irlstls)
      allocate (character(len=LINELENGTH) :: line)
      do i = 1, nlines
        read (this%irlstls, '(A)') line
        read (line, '(f30.0)') dval
        this%releasetimes%times(i) = dval
      end do
      if (.not. this%releasetimes%increasing()) then
        errmsg = "RELEASE TIMES MUST STRICTLY INCREASE"
        call store_error(errmsg)
        call this%parser%StoreErrorUnit(terminate=.true.)
      end if
      this%rlstimelist = .true.
      found = .true.
    case ('TRACK')
      call this%parser%GetStringCaps(keyword)
      if (keyword == 'FILEOUT') then
        ! parse filename
        call this%parser%GetString(fname)
        ! open binary output file
        this%itrkout = getunit()
        call openfile(this%itrkout, this%iout, fname, 'DATA(BINARY)', &
                      form, access, filstat_opt='REPLACE', &
                      mode_opt=MNORMAL)
        write (this%iout, fmttrkbin) trim(adjustl(fname)), this%itrkout
        ! open and write ascii header spec file
        this%itrkhdr = getunit()
        fname = trim(fname)//'.hdr'
        call openfile(this%itrkhdr, this%iout, fname, 'CSV', &
                      filstat_opt='REPLACE', mode_opt=MNORMAL)
        write (this%itrkhdr, '(a,/,a)') TRACKHEADER, TRACKDTYPES
      else
        call store_error('OPTIONAL TRACK KEYWORD MUST BE '// &
                         'FOLLOWED BY FILEOUT')
      end if
      found = .true.
    case ('TRACKCSV')
      call this%parser%GetStringCaps(keyword)
      if (keyword == 'FILEOUT') then
        ! parse filename
        call this%parser%GetString(fname)
        ! open CSV output file and write headers
        this%itrkcsv = getunit()
        call openfile(this%itrkcsv, this%iout, fname, 'CSV', &
                      filstat_opt='REPLACE')
        write (this%iout, fmttrkcsv) trim(adjustl(fname)), this%itrkcsv
        write (this%itrkcsv, '(a)') TRACKHEADER
      else
        call store_error('OPTIONAL TRACKCSV KEYWORD MUST BE &
          &FOLLOWED BY FILEOUT')
      end if
      found = .true.
    case ('LOCAL_Z')
      this%localz = .true.
      found = .true.
    case ('EXTEND_TRACKING')
      this%extend = 1
      found = .true.
    case ('DEV_FORCETERNARY')
      call this%parser%DevOpt()
      this%ifrctrn = 1
      write (this%iout, '(4x,a)') &
        'TRACKING WILL BE DONE USING THE TERNARY METHOD REGARDLESS OF CELL TYPE'
      found = .true.
    case ('DEV_EXIT_SOLVE_METHOD')
      this%iexmethod = this%parser%GetInteger()
      if (.not. (this%iexmethod /= 1 .or. this%iexmethod /= 2)) &
        call store_error('DEV_EXIT_SOLVE_METHOD MUST BE &
          &1 (BRENT) OR 2 (CHANDRUPATLA)')
      found = .true.
    case ('EXIT_SOLVE_TOLERANCE')
      this%extol = this%parser%GetDouble()
      if (this%extol <= DZERO) &
        call store_error('EXIT_SOLVE_TOLERANCE MUST BE POSITIVE')
      found = .true.
      this%foundtol = .true.
    case default
      found = .false.
    end select

  end subroutine prp_options

  !> @brief Read the packagedata for this package
  subroutine prp_read_packagedata(this)
    ! -- dummy
    class(PrtPrpType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: cellid
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp
    character(len=9) :: cno
    logical :: isfound
    logical :: endOfBlock
    integer(I4B) :: ival
    integer(I4B) :: n
    integer(I4B) :: ierr
    character(len=LENBOUNDNAME), dimension(:), allocatable :: nametxt
    integer(I4B), dimension(:), allocatable :: nboundchk
    integer(I4B), dimension(:), allocatable :: noder
    real(DP), dimension(:), allocatable :: x
    real(DP), dimension(:), allocatable :: y
    real(DP), dimension(:), allocatable :: z
    real(DP), dimension(:), allocatable :: tstop
    ! -- format
    character(len=*), parameter :: fmttend = &
      "('end time (', G0, ') must be greater than or equal to the              &
     &begin time (', G0, ').')"

    ! -- allocate and initialize temporary variables
    allocate (noder(this%nreleasepts))
    allocate (x(this%nreleasepts))
    allocate (y(this%nreleasepts))
    allocate (z(this%nreleasepts))
    allocate (tstop(this%nreleasepts))
    allocate (nametxt(this%nreleasepts))
    allocate (nboundchk(this%nreleasepts))

    ! -- initialize temporary variables
    do n = 1, this%nreleasepts
      nboundchk(n) = 0
    end do

    ! -- read particle release point data
    ! -- get particle release points block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
                              supportopenclose=.true.)

    ! -- parse block if detected
    if (isfound) then
      write (this%iout, '(/1x,a)') 'PROCESSING '//trim(adjustl(this%packName)) &
        //' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        ival = this%parser%GetInteger()
        n = ival

        if (n < 1 .or. n > this%nreleasepts) then
          write (errmsg, '(a,1x,i0,a)') &
            'Release point number must be greater than 0 and less than ', &
            'or equal to', this%nreleasepts, '.'
          call store_error(errmsg)
          cycle
        end if

        ! -- increment nboundchk
        nboundchk(n) = nboundchk(n) + 1

        ! -- node number
        call this%parser%GetCellid(this%dis%ndim, cellid)
        noder(n) = this%dis%noder_from_cellid(cellid, this%inunit, this%iout)

        ! -- x, y, z coordinates
        x(n) = this%parser%GetDouble()
        y(n) = this%parser%GetDouble()
        z(n) = this%parser%GetDouble()

        if (this%localz .and. (z(n) < 0 .or. z(n) > 1)) then
          call store_error('Local z coordinate must fall in the interval [0, 1]')
          cycle
        end if

        ! -- set default boundname
        write (cno, '(i9.9)') n
        bndName = 'PRP'//cno

        ! -- read boundnames from file, if provided
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') &
            bndName = bndNameTemp
        else
          bndName = ''
        end if

        ! -- store temp boundnames
        nametxt(n) = bndName
      end do

      write (this%iout, '(1x,a)') &
        'END OF '//trim(adjustl(this%packName))//' PACKAGEDATA'

      ! -- check for duplicate or missing particle release points
      do n = 1, this%nreleasepts
        if (nboundchk(n) == 0) then
          write (errmsg, '(a,a,1x,i0,a)') 'No data specified for particle ', &
            'release point', n, '.'
          call store_error(errmsg)
        else if (nboundchk(n) > 1) then
          write (errmsg, '(a,1x,i0,1x,a,1x,i0,1x,a)') &
            'Data for particle release point', n, 'specified', nboundchk(n), &
            'times.'
          call store_error(errmsg)
        end if
      end do
    else
      call store_error('Required packagedata block not found.')
    end if

    ! -- terminate if any errors were detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if

    ! -- fill particle release point data with data stored in temporary local arrays
    do n = 1, this%nreleasepts
      this%rptnode(n) = noder(n)
      this%rptx(n) = x(n)
      this%rpty(n) = y(n)
      this%rptz(n) = z(n)
      this%rptname(n) = nametxt(n)
    end do

    ! -- deallocate local storage
    deallocate (noder)
    deallocate (x)
    deallocate (y)
    deallocate (z)
    deallocate (tstop)
    deallocate (nametxt)
    deallocate (nboundchk)
  end subroutine prp_read_packagedata

  !> @brief Read package dimensions
  subroutine prp_read_dimensions(this)
    ! -- dummy
    class(PrtPrpType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock

    if (.not. this%foundtol) &
      call store_error('EXIT_SOLVE_TOLERANCE MISSING, VALUE REQUIRED')

    ! -- get dimension block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)

    ! -- parse dimension block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING PARTICLE INPUT DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('NRELEASEPTS')
          this%nreleasepts = this%parser%GetInteger()
        case default
          write (errmsg, &
                 '(4x,a,a)') '****ERROR. UNKNOWN PARTICLE INPUT DIMENSION: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF PARTICLE INPUT DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
    end if

    ! -- set maxbound and nbound to nreleasepts
    this%maxbound = this%nreleasepts
    this%nbound = this%nreleasepts

    ! -- allocate arrays for prp package
    call this%prp_allocate_arrays()

    ! -- read packagedata
    call this%prp_read_packagedata()
  end subroutine prp_read_dimensions

end module PrtPrpModule
