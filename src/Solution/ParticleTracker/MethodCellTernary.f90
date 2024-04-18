module MethodCellTernaryModule

  use KindModule, only: DP, I4B
  use ErrorUtilModule, only: pstop
  use MethodModule
  use MethodSubcellPoolModule
  use CellPolyModule
  use CellDefnModule
  use SubcellTriModule, only: SubcellTriType, create_subcell_tri
  use ParticleModule
  use TrackModule, only: TrackFileControlType
  implicit none

  private
  public :: MethodCellTernaryType
  public :: create_method_cell_ternary

  type, extends(MethodType) :: MethodCellTernaryType
    private
    integer(I4B) :: nverts !< number of vertices
    real(DP), allocatable, dimension(:) :: xvert
    real(DP), allocatable, dimension(:) :: yvert !< cell vertex coordinates
    real(DP), allocatable, dimension(:) :: vne !< cell edge normal velocities
    real(DP), allocatable, dimension(:) :: vv0x
    real(DP), allocatable, dimension(:) :: vv0y
    real(DP), allocatable, dimension(:) :: vv1x
    real(DP), allocatable, dimension(:) :: vv1y !< cell vertex velocities
    real(DP) :: xctr
    real(DP) :: yctr !< cell center coordinates
    real(DP) :: vctrx
    real(DP) :: vctry !< cell center velocities
    real(DP) :: ztop
    real(DP) :: zbot !< cell top and bottom elevations
    real(DP) :: dz !< cell thickness
    real(DP) :: vztop
    real(DP) :: vzbot !< cell top and bottom velocities
    integer(I4B), allocatable, dimension(:) :: iprev !< array of shifted indices
    real(DP), allocatable, dimension(:) :: xvertnext
    real(DP), allocatable, dimension(:) :: yvertnext !< arrays of shifted cell vertex coordinates
    integer(I4B), public, pointer :: zeromethod
  contains
    procedure, public :: apply => apply_mct
    procedure, public :: destroy => destroy_mct
    procedure, public :: load => load_mct
    procedure, public :: load_subcell
    procedure, public :: pass => pass_mct
    procedure         :: vertvelo_orig
    procedure         :: vertvelo
    procedure         :: calc_thru_hcsum
  end type MethodCellTernaryType

contains

  !> @brief Create a tracking method
  subroutine create_method_cell_ternary(method)
    ! dummy
    type(MethodCellTernaryType), pointer :: method
    ! local
    type(CellPolyType), pointer :: cell
    type(SubcellTriType), pointer :: subcell

    allocate (method)
    allocate (method%zeromethod)
    call create_cell_poly(cell)
    method%cell => cell
    method%type => method%cell%type
    method%delegates = .true.
    call create_subcell_tri(subcell)
    method%subcell => subcell
    method%zeromethod = 0
  end subroutine create_method_cell_ternary

  !> @brief Destroy the tracking method
  subroutine destroy_mct(this)
    class(MethodCellTernaryType), intent(inout) :: this
    deallocate (this%type)
  end subroutine destroy_mct

  !> @brief Load subcell into tracking method
  subroutine load_mct(this, particle, next_level, submethod)
    ! dummy
    class(MethodCellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer(I4B), intent(in) :: next_level
    class(MethodType), pointer, intent(inout) :: submethod

    select type (subcell => this%subcell)
    type is (SubcellTriType)
      call this%load_subcell(particle, subcell)
    end select
    call method_subcell_tern%init( &
      cell=this%cell, &
      subcell=this%subcell, &
      trackfilectl=this%trackfilectl, &
      tracktimes=this%tracktimes)
    submethod => method_subcell_tern
    method_subcell_tern%zeromethod = this%zeromethod
  end subroutine load_mct

  !> @brief Pass particle to next subcell if there is one, or to the cell face
  subroutine pass_mct(this, particle)
    ! dummy
    class(MethodCellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    ! local
    integer(I4B) :: isc
    integer(I4B) :: exitFace
    integer(I4B) :: inface

    exitFace = particle%iboundary(3)
    isc = particle%idomain(3)

    select case (exitFace)
    case (0)
      ! Subcell interior (cell interior)
      inface = -1
    case (1)
      ! Subcell face 1 (cell face)
      inface = isc
      if (inface .eq. 0) inface = this%nverts
    case (2)
      ! Subcell face --> next subcell in "cycle" (cell interior)
      isc = isc + 1
      if (isc .gt. this%nverts) isc = 1
      particle%idomain(3) = isc
      particle%iboundary(3) = 3
      inface = 0
    case (3)
      ! Subcell face --> preceding subcell in "cycle" (cell interior)
      isc = isc - 1
      if (isc .lt. 1) isc = this%nverts
      particle%idomain(3) = isc
      particle%iboundary(3) = 2
      inface = 0
    case (4)
      ! Subcell bottom (cell bottom)
      inface = this%nverts + 2
    case (5)
      ! Subcell top (cell top)
      inface = this%nverts + 3
    end select
    if (inface .eq. -1) then
      particle%iboundary(2) = 0
    else if (inface .eq. 0) then
      particle%iboundary(2) = 0
    else
      particle%iboundary(2) = inface
    end if
  end subroutine pass_mct

  !> @brief Apply the ternary method to a polygonal cell
  subroutine apply_mct(this, particle, tmax)
    use ConstantsModule, only: DZERO, DONE, DHALF
    ! dummy
    class(MethodCellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax
    ! local
    integer(I4B) :: i

    ! Update particle state, checking whether any reporting or
    ! termination conditions apply
    call this%update(particle, this%cell%defn)

    ! Return early if particle is done advancing
    if (.not. particle%advancing) return

    ! If the particle is above the top of the cell (presumed water table)
    ! pass it vertically and instantaneously to the cell top and save the
    ! particle state to file
    if (particle%z > this%cell%defn%top) then
      particle%z = this%cell%defn%top
      call this%save(particle, reason=1) ! reason=1: cell transition
    end if
    
    select type (cell => this%cell)
    type is (CellPolyType)
      ! Number of vertices
      this%nverts = cell%defn%npolyverts
      ! (Re)allocate type-bound arrays
      if (allocated(this%xvert)) then
        deallocate (this%xvert)
        deallocate (this%yvert)
        deallocate (this%vne)
        deallocate (this%vv0x)
        deallocate (this%vv0y)
        deallocate (this%vv1x)
        deallocate (this%vv1y)
        deallocate (this%iprev)
        deallocate (this%xvertnext)
        deallocate (this%yvertnext)
      end if
      allocate (this%xvert(this%nverts))
      allocate (this%yvert(this%nverts))
      allocate (this%vne(this%nverts))
      allocate (this%vv0x(this%nverts))
      allocate (this%vv0y(this%nverts))
      allocate (this%vv1x(this%nverts))
      allocate (this%vv1y(this%nverts))
      allocate (this%iprev(this%nverts))
      allocate (this%xvertnext(this%nverts))
      allocate (this%yvertnext(this%nverts))
      ! Cell vertices
      do i = 1, this%nverts
        this%xvert(i) = cell%defn%polyvert(1, i)
        this%yvert(i) = cell%defn%polyvert(2, i)
      end do
      ! Top, bottom, and thickness
      this%ztop = cell%defn%top
      this%zbot = cell%defn%bot
      this%dz = this%ztop - this%zbot
      ! Shifted arrays
      do i = 1, this%nverts
        this%iprev(i) = i
      end do
      this%iprev = cshift(this%iprev, -1)
      this%xvertnext = cshift(this%xvert, 1)
      this%yvertnext = cshift(this%yvert, 1)
    end select
    
    ! Calculate vertex velocities
    if (particle%ivvorig > 0) then
      call this%vertvelo_orig()   ! kluge note: devoption for now
    else
      call this%vertvelo()
    end if

    ! Track across subcells
    call this%track(particle, 2, tmax) ! kluge, hardwired to level 2

  end subroutine apply_mct

  !> @brief Loads a triangular subcell from the polygonal cell
  subroutine load_subcell(this, particle, subcell)
    ! modules
    use ParticleModule, only: get_particle_id
    ! dummy
    class(MethodCellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    class(SubcellTriType), intent(inout) :: subcell
    ! local
    integer(I4B) :: ic
    integer(I4B) :: isc
    integer(I4B) :: iv0
!    integer(I4B) :: iv1
!    integer(I4B) :: ipv0
!    integer(I4B) :: ipv1
!    integer(I4B) :: iv
    real(DP) :: x0
    real(DP) :: y0
    real(DP) :: x1
    real(DP) :: y1
    real(DP) :: x2
    real(DP) :: y2
    real(DP) :: x1rel
    real(DP) :: y1rel
    real(DP) :: x2rel
    real(DP) :: y2rel
    real(DP) :: xi
    real(DP) :: yi
    real(DP) :: di2
    real(DP) :: d02
    real(DP) :: d12
    real(DP) :: di1
    real(DP) :: d01
    real(DP) :: alphai
    real(DP) :: betai
    real(DP) :: betatol

    select type (cell => this%cell)
    type is (CellPolyType)
      ic = cell%defn%icell
      subcell%icell = ic
      isc = particle%idomain(3)
      if (isc .le. 0) then
        xi = particle%x
        yi = particle%y
        do iv0 = 1, this%nverts
!          iv0 = iv
!          iv1 = iv + 1
!          if (iv1 .gt. this%nverts) iv1 = 1
!          ipv0 = iv0
!          ipv1 = iv1
!          x0 = this%xvert(ipv0)
!          y0 = this%yvert(ipv0)
!          x1 = this%xvert(ipv1)
!          y1 = this%yvert(ipv1)
          x0 = this%xvert(iv0)
          y0 = this%yvert(iv0)
          x1 = this%xvertnext(iv0)
          y1 = this%yvertnext(iv0)
          x2 = this%xctr
          y2 = this%yctr
          x1rel = x1 - x0
          y1rel = y1 - y0
          x2rel = x2 - x0
          y2rel = y2 - y0
          di2 = xi * y2rel - yi * x2rel
          d02 = x0 * y2rel - y0 * x2rel
          d12 = x1rel * y2rel - y1rel * x2rel
          di1 = xi * y1rel - yi * x1rel
          d01 = x0 * y1rel - y0 * x1rel
          alphai = (di2 - d02) / d12
          betai = -(di1 - d01) / d12
          ! kluge note: can iboundary(2) be used to identify the subcell?
          betatol = -1e-7 ! kluge
          ! kluge note: think this handles points on triangle boundaries ok
          if ((alphai .ge. 0d0) .and. &
              (betai .ge. betatol) .and. &
              (alphai + betai .le. 1d0)) then
            isc = iv0 ! but maybe not!!!!!!!!!!!!
            exit ! kluge note: doesn't handle particle smack on cell center
          end if
        end do
        if (isc .le. 0) then
          print *, "error -- initial triangle not found for particle ", &
            get_particle_id(particle), " in cell ", ic
          call pstop(1)
        else
          ! subcellTri%isubcell = isc
          ! kluge note: as a matter of form, do we want to allow
          ! this subroutine to modify the particle???
          particle%idomain(3) = isc
        end if
      end if
      subcell%isubcell = isc

      ! Set coordinates and velocities at vertices of triangular subcell
      iv0 = isc
!      iv1 = isc + 1
!      if (iv1 .gt. this%nverts) iv1 = 1
!      ipv0 = iv0
!      ipv1 = iv1
!      subcell%x0 = this%xvert(ipv0)
!      subcell%y0 = this%yvert(ipv0)
!      subcell%x1 = this%xvert(ipv1)
!      subcell%y1 = this%yvert(ipv1)
      subcell%x0 = this%xvert(iv0)
      subcell%y0 = this%yvert(iv0)
      subcell%x1 = this%xvertnext(iv0)
      subcell%y1 = this%yvertnext(iv0)
      subcell%x2 = this%xctr
      subcell%y2 = this%yctr
      subcell%v0x = this%vv0x(iv0)
      subcell%v0y = this%vv0y(iv0)
      subcell%v1x = this%vv1x(iv0)   ! kluge note: the indices here actually refer to subcells, not vertices
      subcell%v1y = this%vv1y(iv0)
      subcell%v2x = this%vctrx
      subcell%v2y = this%vctry
      subcell%ztop = this%ztop
      subcell%zbot = this%zbot
      subcell%dz = this%dz
      subcell%vzbot = this%vzbot
      subcell%vztop = this%vztop
    end select
  end subroutine load_subcell

  !> @brief Calculate vertex velocities the original way    ! kluge note: temporary, for testing
  subroutine vertvelo_orig(this)
    use ConstantsModule, only: DZERO, DONE, DHALF
    ! dummy
    class(MethodCellTernaryType), intent(inout) :: this
    ! local
    integer(I4B) :: iv
    integer(I4B) :: ivp1
    integer(I4B) :: ivm1
    real(DP) :: retfactor
    real(DP) :: x0
    real(DP) :: y0
    real(DP) :: x1
    real(DP) :: y1
    real(DP) :: x2
    real(DP) :: y2
    real(DP) :: xsum
    real(DP) :: ysum
    real(DP) :: vxsum
    real(DP) :: vysum
    real(DP) :: flow0
    real(DP) :: flow1
    real(DP) :: v0x
    real(DP) :: v0y
    real(DP) :: d01x
    real(DP) :: d01y
    real(DP) :: d02x
    real(DP) :: d02y
    real(DP) :: det
    real(DP) :: area
    real(DP) :: term

    select type (cell => this%cell)
    type is (CellPolyType)

      xsum = DZERO
      ysum = DZERO
      vxsum = DZERO
      vysum = DZERO
      area = DZERO
      this%ztop = cell%defn%top
      this%zbot = cell%defn%bot
      this%dz = this%ztop - this%zbot
      do iv = 1, this%nverts
        ivp1 = iv + 1
        if (ivp1 .gt. this%nverts) ivp1 = 1
        ivm1 = iv - 1
        if (ivm1 .lt. 1) ivm1 = this%nverts
        x0 = cell%defn%polyvert(1, iv)
        y0 = cell%defn%polyvert(2, iv)
        x2 = cell%defn%polyvert(1, ivp1)
        y2 = cell%defn%polyvert(2, ivp1)
        x1 = cell%defn%polyvert(1, ivm1)
        y1 = cell%defn%polyvert(2, ivm1)
        term = DONE / (cell%defn%porosity * this%dz)
        flow0 = cell%defn%faceflow(iv) * term
        flow1 = cell%defn%faceflow(ivm1) * term
        d01x = x1 - x0 ! kluge note: do this more efficiently, not recomputing things so much???
        d01y = y1 - y0
        d02x = x2 - x0
        d02y = y2 - y0
        ! kluge note: can det ever be zero, like maybe for a 180-deg vertex???
        ! oodet = DONE/(d01y*d02x - d02y*d01x)
        ! velmult = particle%velmult
        ! kluge note: "flow" is volumetric (face) flow rate per unit thickness, divided by porosity
        ! v0x = -velmult*oodet*(d02x*flow1 + d01x*flow0)
        ! v0y = -velmult*oodet*(d02y*flow1 + d01y*flow0)   !
        det = d01y * d02x - d02y * d01x
        retfactor = cell%defn%retfactor
        ! kluge note: can det ever be zero, like maybe for a 180-deg vertex???
        ! term = velfactor/det
        ! kluge note: can det ever be zero, like maybe for a 180-deg vertex???
        term = DONE / (retfactor * det)
        ! kluge note: "flow" here is volumetric flow rate (MODFLOW face flow)
        v0x = -term * (d02x * flow1 + d01x * flow0)
        ! per unit thickness, divided by porosity
        v0y = -term * (d02y * flow1 + d01y * flow0)
        this%vv0x(iv) = v0x
        this%vv0y(iv) = v0y
        this%vv1x(ivm1) = v0x   ! kluge note: the indices here actually refer to subcells, not vertices
        this%vv1y(ivm1) = v0y
        xsum = xsum + x0
        ysum = ysum + y0
        vxsum = vxsum + v0x
        vysum = vysum + v0y
        this%xvert(iv) = x0
        this%yvert(iv) = y0
        area = area + x0 * y1 - x1 * y0
      end do
      area = area * DHALF
      term = DONE / (retfactor * cell%defn%porosity * area)
      this%vzbot = cell%defn%faceflow(this%nverts + 2) * term
      this%vztop = -cell%defn%faceflow(this%nverts + 3) * term
      this%xctr = xsum / dble(this%nverts)
      this%yctr = ysum / dble(this%nverts)
      this%vctrx = vxsum / dble(this%nverts)
      this%vctry = vysum / dble(this%nverts)

    end select
  end subroutine vertvelo_orig
  
  !> @brief Calculate vertex velocities
  subroutine vertvelo(this)
    use ConstantsModule, only: DZERO, DONE, DHALF
    ! dummy
    class(MethodCellTernaryType), intent(inout) :: this
    ! local
    integer(I4B)                        :: iv
    integer(I4B)                        :: ivm1
    real(DP)                            :: term
    integer(I4B)                        :: i
    real(DP)                            :: perturb
    real(DP), allocatable, dimension(:) :: xvals
    real(DP), allocatable, dimension(:) :: yvals
    real(DP)                            :: sixa
    real(DP)                            :: vm0i0
    real(DP)                            :: vm0ival
    real(DP)                            :: hcsum0
    real(DP)                            :: hcsum
    real(DP)                            :: jac
    real(DP), allocatable, dimension(:) :: wk1
    real(DP), allocatable, dimension(:) :: wk2
    real(DP), allocatable, dimension(:) :: fact
    real(DP), allocatable, dimension(:) :: unixnext
    real(DP), allocatable, dimension(:) :: uniynext
    real(DP), allocatable, dimension(:) :: le
    real(DP), allocatable, dimension(:) :: unex
    real(DP), allocatable, dimension(:) :: uney
    real(DP)                            :: areacell
    real(DP), allocatable, dimension(:) :: areasub
    real(DP)                            :: divcell
    real(DP), allocatable, dimension(:) :: li
    real(DP), allocatable, dimension(:) :: unix
    real(DP), allocatable, dimension(:) :: uniy
    real(DP), allocatable, dimension(:) :: xmid
    real(DP), allocatable, dimension(:) :: ymid
    real(DP), allocatable, dimension(:) :: lm
    real(DP), allocatable, dimension(:) :: umx
    real(DP), allocatable, dimension(:) :: umy
    real(DP), allocatable, dimension(:) :: kappax
    real(DP), allocatable, dimension(:) :: kappay
!    real(DP), allocatable, dimension(:) :: vm0i
!    real(DP), allocatable, dimension(:) :: vm0e
!    real(DP), allocatable, dimension(:) :: vm1i
!    real(DP), allocatable, dimension(:) :: vm1e
    real(DP), allocatable, dimension(:) :: vm0x
    real(DP), allocatable, dimension(:) :: vm0y
    real(DP), allocatable, dimension(:) :: vm1x
    real(DP), allocatable, dimension(:) :: vm1y

    select type (cell => this%cell)
    type is (CellPolyType)
 
      ! Allocate local arrays
      allocate (le(this%nverts))           ! lengths of exterior (cell) edges
      allocate (unex(this%nverts))         ! x components of unit normals to exterior edges
      allocate (uney(this%nverts))         ! y components of unit normals to exterior edges
      allocate (areasub(this%nverts))      ! subcell areas
      allocate (li(this%nverts))           ! lengths of interior edges ("spokes")
      allocate (unix(this%nverts))         ! x components of unit normals to interior edges
      allocate (uniy(this%nverts))         ! y components of unit normals to interior edges
      allocate (xmid(this%nverts))         ! x coordinates of midpoints
      allocate (ymid(this%nverts))         ! y coordinates of midpoints
      allocate (lm(this%nverts))           ! lengths of midpoint connectors
      allocate (umx(this%nverts))          ! x components of midpoint-connector (ccw) unit vectors
      allocate (umy(this%nverts))          ! y components of midpoint-connector (ccw) unit vectors
      allocate (kappax(this%nverts))       ! x components of kappa vectors
      allocate (kappay(this%nverts))       ! y components of kappa vectors
!      allocate (vm0i(this%nverts))         ! component of vm0 normal to the interior edge it's on
!      allocate (vm0e(this%nverts))         ! component of vm0 in the direction normal to the corresponding exterior edge
!      allocate (vm1i(this%nverts))         ! component of vm1 normal to the interior edge it's on
!      allocate (vm1e(this%nverts))         ! component of vm1 in the direction normal to the corresponding exterior edge
      allocate (vm0x(this%nverts))         ! x component of vm0
      allocate (vm0y(this%nverts))         ! y component of vm0
      allocate (vm1x(this%nverts))         ! x component of vm1
      allocate (vm1y(this%nverts))         ! y component of vm1
      allocate (unixnext(this%nverts))     ! vector of "next" interior unit-normal x coordinates defined for convenience
      allocate (uniynext(this%nverts))     ! vector of "next" interior unit-normal y coordinates defined for convenience
      allocate (wk1(this%nverts))
      allocate (wk2(this%nverts))
      allocate (xvals(3))
      allocate (yvals(3))
      
      ! Exterior edge unit normals (outward) and lengths
      wk1 = this%xvertnext - this%xvert
      wk2 = this%yvertnext - this%yvert
      le = dsqrt(wk1 * wk1 + wk2 * wk2)
      unex = wk2 / le
      uney = -wk1 / le
  
      ! Cell area
      areacell = areapoly(this%xvert, this%yvert)
      
      ! Cell centroid   ! kluge note: in general, this is NOT the average of the vertex coordinates
      sixa = areacell * 6.d0
      wk1 = this%xvert * this%yvertnext - this%xvertnext * this%yvert
      this%xctr = sum((this%xvert + this%xvertnext) * wk1) / sixa
      this%yctr = sum((this%yvert + this%yvertnext) * wk1) / sixa
  
      ! Subcell areas
      do i = 1, this%nverts
        xvals(1) = this%xvert(i)
        xvals(2) = this%xvertnext(i)
        xvals(3) = this%xctr
        yvals(1) = this%yvert(i)
        yvals(2) = this%yvertnext(i)
        yvals(3) = this%yctr
        areasub(i) = areapoly(xvals, yvals)
      end do
      
      ! Cell-edge normal velocities
      term = DONE / (cell%defn%porosity * cell%defn%retfactor * this%dz)
      do i = 1, this%nverts
        this%vne(i) = cell%defn%faceflow(i) * term / le(i)
      end do
  
      ! Cell divergence (2D)
      divcell = sum(le * this%vne) / areacell
  
      ! Interior edge (ccw) unit normals and lengths
      wk1 = this%xvert - this%xctr
      wk2 = this%yvert - this%yctr
      li = dsqrt(wk1 * wk1 + wk2 * wk2)
      unix = -wk2 / li
      uniy = wk1 / li
      ! Shifted arrays for convenience
      unixnext = cshift(unix, 1)
      uniynext = cshift(uniy, 1)
  
      ! Midpoints of interior edges
      xmid = 5.d-1 * (this%xvert + this%xctr)
      ymid = 5.d-1 * (this%yvert + this%yctr)
      
      ! Unit midpoint-connector (ccw) vectors and lengths
      wk1 = cshift(xmid, 1) - xmid
      wk2 = cshift(ymid, 1) - ymid
      lm = dsqrt(wk1 * wk1 + wk2 * wk2)
      umx = wk1 / lm
      umy = wk2 / lm
  
      ! Kappa vectors (K tensor times unit midpoint-connector vectors)
      kappax = umx   ! kluge (isotropic K=1.)
      kappay = umy   ! kluge (isotropic K=1.)
  
      ! Use linearity to find vm0i[0] such that curl of the head gradient
      ! is zero
      perturb = 1.d-2     ! kluge?
      ! Calculations at base value
      vm0i0 = 0.d0
      call this%calc_thru_hcsum(vm0i0, divcell, le, li, lm, areasub,            &
                                unix, uniy, unex, uney, unixnext, uniynext,     &
                                kappax, kappay, vm0x, vm0y, vm1x, vm1y, hcsum0)
      ! Calculations at perturbed value
      vm0ival = vm0i0 + perturb
      call this%calc_thru_hcsum(vm0ival, divcell, le, li, lm, areasub,          &
                                unix, uniy, unex, uney, unixnext, uniynext,     &
                                kappax, kappay, vm0x, vm0y, vm1x, vm1y, hcsum)
      ! Calculations at root value
      jac = (hcsum - hcsum0) / perturb
      vm0ival = vm0i0 - hcsum0 / jac
      call this%calc_thru_hcsum(vm0ival, divcell, le, li, lm, areasub,          &
                                unix, uniy, unex, uney, unixnext, uniynext,     &
                                kappax, kappay, vm0x, vm0y, vm1x, vm1y, hcsum)
          
      ! Project linearly to get corner (vertex) velocities. Note that velocity
      ! vv1 is at the next vertex ccw from vv0, so vv0(i) and vv1(i) are the
      ! two vertex velocities used by triangular subcell i.
      this%vv0x = 2.d0 * vm0x - this%vctrx
      this%vv0y = 2.d0 * vm0y - this%vctry
      this%vv1x = 2.d0 * vm1x - this%vctrx
      this%vv1y = 2.d0 * vm1y - this%vctry

      ! Set top and bottom velocities
      term = DONE / (cell%defn%retfactor * cell%defn%porosity * areacell)
      this%vzbot = cell%defn%faceflow(this%nverts + 2) * term
      this%vztop = -cell%defn%faceflow(this%nverts + 3) * term

      ! Deallocate local arrays
      deallocate (le)
      deallocate (unex)
      deallocate (uney)
      deallocate (areasub)
      deallocate (li)
      deallocate (unix)
      deallocate (uniy)
      deallocate (xmid)
      deallocate (ymid)
      deallocate (lm)
      deallocate (umx)
      deallocate (umy)
      deallocate (kappax)
      deallocate (kappay)
      deallocate (vm0x)
      deallocate (vm0y)
      deallocate (vm1x)
      deallocate (vm1y)
      deallocate (unixnext)
      deallocate (uniynext)
      deallocate (wk1)
      deallocate (wk2)
      deallocate (xvals)
      deallocate (yvals)
      
    end select
  end subroutine vertvelo

  subroutine calc_thru_hcsum(this, vm0ival, divcell, le, li, lm, areasub,       &
                             unix, uniy, unex, uney, unixnext, uniynext,        &
                             kappax, kappay, vm0x, vm0y, vm1x, vm1y, hcsum)
    ! dummy
    class(MethodCellTernaryType), intent(inout) :: this
    real(DP)                            :: vm0ival
    real(DP)                            :: divcell
    real(DP)                            :: hcsum
    real(DP), dimension(:)              :: le
    real(DP), dimension(:)              :: li
    real(DP), dimension(:)              :: lm
    real(DP), dimension(:)              :: areasub
    real(DP), dimension(:)              :: unix
    real(DP), dimension(:)              :: uniy
    real(DP), dimension(:)              :: unex
    real(DP), dimension(:)              :: uney
    real(DP), dimension(:)              :: unixnext
    real(DP), dimension(:)              :: uniynext
    real(DP), dimension(:)              :: kappax
    real(DP), dimension(:)              :: kappay
    real(DP), dimension(:)              :: vm0x
    real(DP), dimension(:)              :: vm0y
    real(DP), dimension(:)              :: vm1x
    real(DP), dimension(:)              :: vm1y
    ! local
    real(DP), allocatable, dimension(:) :: vm0i
    real(DP), allocatable, dimension(:) :: vm0e
    real(DP), allocatable, dimension(:) :: vm1i
    real(DP), allocatable, dimension(:) :: vm1e
    real(DP), allocatable, dimension(:) :: uprod
    real(DP), allocatable, dimension(:) :: det
    real(DP), allocatable, dimension(:) :: wt
    real(DP), allocatable, dimension(:) :: bi0x
    real(DP), allocatable, dimension(:) :: be0x
    real(DP), allocatable, dimension(:) :: bi0y
    real(DP), allocatable, dimension(:) :: be0y
    real(DP), allocatable, dimension(:) :: bi1x
    real(DP), allocatable, dimension(:) :: be1x
    real(DP), allocatable, dimension(:) :: bi1y
    real(DP), allocatable, dimension(:) :: be1y
    real(DP), allocatable, dimension(:) :: be01x
    real(DP), allocatable, dimension(:) :: be01y
    real(DP)                            :: emxx
    real(DP)                            :: emxy
    real(DP)                            :: emyx
    real(DP)                            :: emyy
    real(DP)                            :: rx
    real(DP)                            :: ry
    real(DP)                            :: emdet
    integer(I4B)                        :: i
    integer(I4B)                        :: ip
    
    ! Allocate local arrays
    allocate (vm0i(this%nverts))
    allocate (vm0e(this%nverts))
    allocate (vm1i(this%nverts))
    allocate (vm1e(this%nverts))
    allocate (uprod(this%nverts))
    allocate (det(this%nverts))
    allocate (wt(this%nverts))
    allocate (bi0x(this%nverts))
    allocate (be0x(this%nverts))
    allocate (bi0y(this%nverts))
    allocate (be0y(this%nverts))
    allocate (bi1x(this%nverts))
    allocate (be1x(this%nverts))
    allocate (bi1y(this%nverts))
    allocate (be1y(this%nverts))
    allocate (be01x(this%nverts))
    allocate (be01y(this%nverts))
    
    ! Set vm0i(1)
    vm0i(1) = vm0ival
    
    ! Get remaining vm0i's sequentially using divergence conditions
    do i = 2, this%nverts
        ip = this%iprev(i)
        vm0i(i) = (li(ip) * vm0i(ip) - le(ip) * this%vne(ip)                    &
                   + areasub(ip) * divcell) / li(i)
    end do

    ! Get vm1i's from vm0i's using continuity conditions
    vm1i = cshift(vm0i, 1)

    ! Get centroid velocity by setting up and solving 2x2 linear system
    uprod = unix * unex + uniy * uney
    det = 1.d0 - uprod * uprod
    bi0x = (unix - unex * uprod) / det
    be0x = (unex - unix * uprod) / det
    bi0y = (uniy - uney * uprod) / det
    be0y = (uney - uniy * uprod) / det
    uprod = unixnext * unex + uniynext * uney
    det = 1.d0 - uprod * uprod
    bi1x = (unixnext - unex * uprod) / det
    be1x = (unex - unixnext * uprod) / det
    bi1y = (uniynext - uney * uprod) / det
    be1y = (uney - uniynext * uprod) / det
    be01x = 5.d-1 * (be0x + be1x)
    be01y = 5.d-1 * (be0y + be1y)
    wt = 1.d0 / dble(this%nverts)      ! kluge (equal weights)
    emxx = 2.d0 - sum(wt * be01x * unex)
    emxy = -sum(wt * be01x * uney)
    emyx = -sum(wt * be01y * unex)
    emyy = 2.d0 - sum(wt * be01y * uney)
    rx = sum(wt * (bi0x * vm0i + bi1x * vm1i + be01x * this%vne))
    ry = sum(wt * (bi0y * vm0i + bi1y * vm1i + be01y * this%vne))
    emdet = emxx * emyy - emxy * emyx
    this%vctrx = (emyy * rx - emxy * ry) / emdet
    this%vctry = (emxx * ry - emyx * rx) / emdet
    
    ! Get vm0e's using "known" conditions
    vm0e = 5.d-1 * (this%vne + unex * this%vctrx + uney * this%vctry)
    
    ! Get vm1e's from uniformity along exterior edges
    vm1e = vm0e
    
    ! Transform vm0 and vm1 to (x, y) coordinates
    vm0x = bi0x * vm0i + be0x * vm0e
    vm0y = bi0y * vm0i + be0y * vm0e
    vm1x = bi1x * vm1i + be1x * vm0e
    vm1y = bi1y * vm1i + be1y * vm0e
    
    ! Calculate head-cycle summation (which is proportional to
    ! the curl of the head gradient)
    hcsum = sum(lm * (kappax * (vm0x + vm1x) + kappay * (vm0y + vm1y)))
    
    ! Deallocate local arrays
    deallocate (vm0i)
    deallocate (vm0e)
    deallocate (vm1i)
    deallocate (vm1e)
    deallocate (uprod)
    deallocate (det)
    deallocate (wt)
    deallocate (bi0x)
    deallocate (be0x)
    deallocate (bi0y)
    deallocate (be0y)
    deallocate (bi1x)
    deallocate (be1x)
    deallocate (bi1y)
    deallocate (be1y)
    deallocate (be01x)
    deallocate (be01y)
    
    return

  end subroutine calc_thru_hcsum

function areapoly(xv, yv) result(area)   ! kluge note: should this be packaged with other utilities?
    ! dummy
    double precision, dimension(:) :: xv
    double precision, dimension(:) :: yv
    ! result
    double precision               :: area

    area = 5.d-1 * sum(xv(:) * cshift(yv(:), 1) - cshift(xv(:), 1) * yv(:))

end function areapoly

end module MethodCellTernaryModule

