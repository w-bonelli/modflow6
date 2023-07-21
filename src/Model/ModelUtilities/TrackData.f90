module TrackDataModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE
  use ParticleModule, only: ParticleType
  use UtilMiscModule, only: transform_coords

  implicit none

  private
  public :: TrackDataType

  integer(I4B), parameter, public :: INITIAL_TRACK_SIZE = 1000

  character(len=*), parameter, public :: TRACKHEADERS = &
                'kper,kstp,imdl,iprp,irpt,ilay,icell,izone,istatus,ireason,&
                &trelease,t,x,y,z'

  character(len=*), parameter, public :: TRACKTYPES = &
                             '<i4,<i4,<i4,<i4,<i4,<i4,<i4,<i4,<i4,<i4,&
                             &<f8,<f8,<f8,<f8,<f8'

  type :: TrackDataType

    ! Notes
    ! -----
    !
    ! Structure of arrays to hold particle tracks.
    ! Each particle's track across the simulation domain consists of 1+ rows.
    !
    ! There is no particle ID column. Particles can be uniquely identified by
    ! "composite key", i.e. combination of column values:
    !   - imdl: originating model ID
    !   - iprp: originating PRP ID
    !   - irpt: particle release location ID
    !   - trelease: particle release time
    !
    ! Enumerations:
    !   istatus (like MODPATH 7's status):
    !     0: pending release (kluge: is this necessary?)
    !     1: active
    !     2: terminated at boundary face
    !     3: terminated in weak sink cell
    !     4: terminated in weak source cell
    !     5: terminated in cell with no exit face
    !     6: terminated in cell with specified zone number
    !     7: terminated in inactive cell
    !     8: permanently unreleased (e.g. released into an inactive cell, into a cell in a termination
    !                                zone, into a cell with no exit face, into a stop zone cell, etc)
    !     9: terminated for unknown reason (kluge: is this necessary?)
    !
    !   ireason
    !     0: release
    !     1: cross spatial boundary (cell? subcell? or generic feature? worth distinguishing?)
    !     2: cross temporal boundary (time step end)
    !     3: termination
    !     4: inactive

    ! kluge??? if mem_reallocate was called from the PRT module
    ! instead of within this module, this wouldn't be necessary
    character(len=:), pointer :: mempath => null() ! memory path of track data

    ! integer arrays
    integer(I4B), pointer :: ntrack => null() ! total count of track data
    integer(I4B), dimension(:), pointer, contiguous :: kper ! stress period
    integer(I4B), dimension(:), pointer, contiguous :: kstp ! time step
    integer(I4B), dimension(:), pointer, contiguous :: imdl ! originating model index
    integer(I4B), dimension(:), pointer, contiguous :: iprp ! originating PRP index
    integer(I4B), dimension(:), pointer, contiguous :: irpt ! release point index
    integer(I4B), dimension(:), pointer, contiguous :: ilay ! layer
    integer(I4B), dimension(:), pointer, contiguous :: icell ! cell number (user, not reduced)
    integer(I4B), dimension(:), pointer, contiguous :: izone ! todo zone number
    integer(I4B), dimension(:), pointer, contiguous :: istatus ! particle status
    integer(I4B), dimension(:), pointer, contiguous :: ireason ! reason for datum

    ! double arrays
    real(DP), dimension(:), pointer, contiguous :: trelease ! particle's release time
    real(DP), dimension(:), pointer, contiguous :: t ! current time
    real(DP), dimension(:), pointer, contiguous :: x ! current x coordinate
    real(DP), dimension(:), pointer, contiguous :: y ! current y coordinate
    real(DP), dimension(:), pointer, contiguous :: z ! current z coordinate

  contains
    procedure, public :: allocate_arrays
    procedure, public :: deallocate_arrays
    procedure, public :: reallocate_arrays
    procedure, public :: add_track_data
    procedure, public :: reset_track_data
    procedure, public :: save_track_data
  end type TrackDataType

contains

  !> @brief Allocate track data arrays
  !! This routine only accepts a mempath parameter because
  !! the memory manager mem_reallocate routine requires it.
  subroutine allocate_arrays(this, nt, mempath)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(TrackDataType), intent(inout) :: this
    integer(I4B), intent(in) :: nt
    character(len=*), intent(in) :: mempath

    ! -- this routine only accepts a mempath parameter because
    ! -- the memory manager mem_reallocate routine requires it

    ! print *, 'allocating ', nt, ' slots for track data arrays'
    allocate (character(len=len(mempath)) :: this%mempath)
    ! call mem_allocate(this%mempath, size(mempath), 'TRACKMEMPATH', mempath)
    this%mempath = mempath ! kluge!!!
    ! call mem_setptr(mempath, 'TRACKMEMPATH', mempath)
    call mem_allocate(this%kper, nt, 'TRACKKPER', mempath)
    call mem_allocate(this%kstp, nt, 'TRACKKSTP', mempath)
    call mem_allocate(this%imdl, nt, 'TRACKIMDL', mempath)
    call mem_allocate(this%iprp, nt, 'TRACKIPRP', mempath)
    call mem_allocate(this%irpt, nt, 'TRACKIRPT', mempath)
    call mem_allocate(this%ilay, nt, 'TRACKILAY', mempath)
    call mem_allocate(this%icell, nt, 'TRACKICELL', mempath)
    call mem_allocate(this%izone, nt, 'TRACKIZONE', mempath)
    call mem_allocate(this%istatus, nt, 'TRACKISTATUS', mempath)
    call mem_allocate(this%ireason, nt, 'TRACKIREASON', mempath)
    call mem_allocate(this%trelease, nt, 'TRACKTRELEASE', mempath)
    call mem_allocate(this%t, nt, 'TRACKT', mempath)
    call mem_allocate(this%x, nt, 'TRACKX', mempath)
    call mem_allocate(this%y, nt, 'TRACKY', mempath)
    call mem_allocate(this%z, nt, 'TRACKZ', mempath)
    !
    return
  end subroutine allocate_arrays

  !> @brief Deallocate track data arrays
  subroutine deallocate_arrays(this, mempath)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(TrackDataType), intent(inout) :: this
    character(len=*), intent(in) :: mempath
    !
    deallocate (this%mempath) ! kluge!!!
    call mem_deallocate(this%kper)
    call mem_deallocate(this%kstp)
    call mem_deallocate(this%imdl)
    call mem_deallocate(this%iprp)
    call mem_deallocate(this%irpt)
    call mem_deallocate(this%ilay)
    call mem_deallocate(this%icell)
    call mem_deallocate(this%izone)
    call mem_deallocate(this%istatus)
    call mem_deallocate(this%ireason)
    call mem_deallocate(this%trelease)
    call mem_deallocate(this%t)
    call mem_deallocate(this%x)
    call mem_deallocate(this%y)
    call mem_deallocate(this%z)
    !
    return
  end subroutine deallocate_arrays

  !> @brief Resize track data arrays
  subroutine reallocate_arrays(this, nt, mempath)
    ! -- modules
    use MemoryManagerModule, only: mem_reallocate
    ! -- dummy
    class(TrackDataType), intent(inout) :: this
    integer(I4B), intent(in) :: nt
    character(len=*), intent(in) :: mempath
    !
    call mem_reallocate(this%kper, nt, 'TRACKKPER', mempath)
    call mem_reallocate(this%kstp, nt, 'TRACKKSTP', mempath)
    call mem_reallocate(this%imdl, nt, 'TRACKIMDL', mempath)
    call mem_reallocate(this%iprp, nt, 'TRACKIPRP', mempath)
    call mem_reallocate(this%irpt, nt, 'TRACKIRPT', mempath)
    call mem_reallocate(this%ilay, nt, 'TRACKILAY', mempath)
    call mem_reallocate(this%icell, nt, 'TRACKICELL', mempath)
    call mem_reallocate(this%izone, nt, 'TRACKIZONE', mempath)
    call mem_reallocate(this%istatus, nt, 'TRACKISTATUS', mempath)
    call mem_reallocate(this%ireason, nt, 'TRACKIREASON', mempath)
    call mem_reallocate(this%trelease, nt, 'TRACKTRELEASE', mempath)
    call mem_reallocate(this%t, nt, 'TRACKT', mempath)
    call mem_reallocate(this%x, nt, 'TRACKX', mempath)
    call mem_reallocate(this%y, nt, 'TRACKY', mempath)
    call mem_reallocate(this%z, nt, 'TRACKZ', mempath)
    !
    return
  end subroutine reallocate_arrays

  !> @brief Add track data from a particle
  subroutine add_track_data(this, particle, kper, kstp, reason, level)
    ! -- dummy
    class(TrackDataType), intent(inout) :: this
    type(ParticleType), pointer, intent(in) :: particle
    integer(I4B), intent(in) :: kper, kstp
    integer(I4B), intent(in) :: reason
    integer(I4B), intent(in), optional :: level
    ! -- local
    integer(I4B) :: itrack, ntracksize
    integer(I4B) :: resizefactor
    logical(LGP) :: ladd
    real(DP) :: xmodel, ymodel, zmodel
    !
    ! -- Determine whether to add track data
    if (.not. present(level)) then
      ! -- If optional argument level is not present, track data will be added
      ! -- by default
      ladd = .true.
    else
      ! If optional argument level is present, check criteria
      ladd = .false.
      if (level == 3) ladd = .true. ! kluge note: adds after each subcell-level track
    end if
    !
    if (ladd) then
      !
      ! -- Expand track arrays by factor of 10 if at capacity.
      resizefactor = 10
      ntracksize = size(this%irpt)
      if ((ntracksize - this%ntrack) < 1) then
        ! print *, 'Expanding track arrays from ', ntracksize, &
        !   ' to ', ntracksize * resizefactor
        call this%reallocate_arrays(ntracksize * resizefactor, this%mempath)
      end if
      !
      ! -- Get model coordinates
      call particle%get_model_coords(xmodel, ymodel, zmodel)
      !
      ! -- Add track data
      itrack = this%ntrack + 1
      this%ntrack = itrack
      this%kper(itrack) = kper
      this%kstp(itrack) = kstp
      this%imdl(itrack) = particle%imdl
      this%iprp(itrack) = particle%iprp
      this%irpt(itrack) = particle%irpt
      this%ilay(itrack) = particle%ilay
      this%icell(itrack) = particle%icu
      this%izone(itrack) = particle%izone
      if (particle%istatus .lt. 0) then
        this%istatus(itrack) = 1
      else
        this%istatus(itrack) = particle%istatus
      end if
      this%ireason(itrack) = reason
      this%trelease(itrack) = particle%trelease
      this%t(itrack) = particle%ttrack
      this%x(itrack) = xmodel
      this%y(itrack) = ymodel
      this%z(itrack) = zmodel

    end if
    !
    return
  end subroutine add_track_data

  !> @brief Reset track data
  subroutine reset_track_data(this)
    ! -- dummy
    class(TrackDataType), intent(inout) :: this
    !
    this%ntrack = DZERO ! kluge note: zero out arrays, too, just for cleanliness?
    !
    return
  end subroutine reset_track_data

  !> @brief Write track data to a binary or CSV output file.
  !!
  !! Arguments itrack1 and itrack2 may be provided to select a subset of
  !! track data to write to file. This can be used to write data for one
  !! or multiple contiguous PRPs, instead of all particles in the model.
  subroutine save_track_data(this, itrkun, csv, itrack1, itrack2)
    ! -- dummy
    class(TrackDataType), intent(inout) :: this
    integer(I4B), intent(in) :: itrkun
    logical(LGP), intent(in) :: csv
    integer(I4B), intent(in), optional :: itrack1, itrack2
    ! -- local
    integer(I4B) :: itrack, itrackmin, itrackmax
    integer(I4B) :: kper, kstp
    integer(I4B) :: imdl, iprp, irpt, ilay, icell, izone, istatus, ireason
    real(DP) :: trelease, t, x, y, z

    ! -- select subset of track data between itrack1 and itrack2
    !    if provided (can be used to select data for distinct PRPs)
    if (present(itrack1)) then
      itrackmin = itrack1
    else
      itrackmin = 1
    end if
    if (present(itrack2)) then
      itrackmax = itrack2
    else
      itrackmax = this%ntrack
    end if

    ! -- write rows to file
    do itrack = itrackmin, itrackmax
      kper = this%kper(itrack)
      kstp = this%kstp(itrack)
      imdl = this%imdl(itrack)
      iprp = this%iprp(itrack)
      irpt = this%irpt(itrack)
      ilay = this%ilay(itrack)
      icell = this%icell(itrack)
      izone = this%izone(itrack)
      istatus = this%istatus(itrack)
      ireason = this%ireason(itrack)
      trelease = this%trelease(itrack)
      t = this%t(itrack)
      x = this%x(itrack)
      y = this%y(itrack)
      z = this%z(itrack)

      if (csv) then
        write (itrkun, '(*(G0,:,","))') &
          kper, kstp, imdl, iprp, irpt, ilay, icell, izone, istatus, ireason, &
          trelease, t, x, y, z
      else
        write (itrkun) &
          kper, kstp, imdl, iprp, irpt, ilay, icell, izone, istatus, ireason, &
          trelease, t, x, y, z
      end if
    end do

  end subroutine save_track_data

end module TrackDataModule