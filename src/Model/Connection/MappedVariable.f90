module MappedVariableModule
  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME
  use MemoryTypeModule, only: MemoryType
  use MemoryManagerModule, only: get_from_memorylist

  implicit none
  private

  public :: CastAsMappedVariable
  public :: MappedVariableType

  type :: MappedVariableType
    integer(I4B) :: controller_id
    integer(I4B) :: sync_stage
    character(len=LENVARNAME) :: src_name
    character(len=LENMEMPATH) :: src_path
    type(MemoryType), pointer :: src !< cached memory item
    character(len=LENVARNAME) :: tgt_name
    character(len=LENMEMPATH) :: tgt_path
    type(MemoryType), pointer :: tgt !< cached memory item
    integer(I4B), dimension(:), pointer :: src_idx !< source indexes to copy from
    integer(I4B), dimension(:), pointer :: tgt_idx !< target indexes to copy to
    integer(I4B), dimension(:), pointer :: sign !< optional sign (or null) to negate copied value
  contains
    procedure :: sync
    procedure :: skip_sync !< possibility to skip synchronization, e.g. when src variable not allocated and should remain at default
    ! private stuff
    procedure, private :: sync_int1d
    procedure, private :: apply_sgn_int1d
    procedure, private :: sync_dbl1d
    procedure, private :: apply_sgn_dbl1d
    procedure, private :: sync_dbl2d
    procedure, private :: apply_sgn_dbl2d

  end type MappedVariableType

contains

  subroutine sync(this)
    class(MappedVariableType) :: this
    ! local
    logical(LGP) :: found

    if (.not. associated(this%src)) then
      ! cache
      call get_from_memorylist(this%src_name, this%src_path, this%src, found)
      call get_from_memorylist(this%tgt_name, this%tgt_path, this%tgt, found)
    end if

    if (this%skip_sync()) return

    if (associated(this%tgt%aint1d)) call this%sync_int1d()
    if (associated(this%tgt%adbl1d)) call this%sync_dbl1d()
    if (associated(this%tgt%adbl2d)) call this%sync_dbl2d()

    if (associated(this%sign)) then
      if (associated(this%tgt%aint1d)) call this%apply_sgn_int1d()
      if (associated(this%tgt%adbl1d)) call this%apply_sgn_dbl1d()
      if (associated(this%tgt%adbl2d)) call this%apply_sgn_dbl2d()
    end if

  end subroutine sync

  function skip_sync(this) result(skip)
    class(MappedVariableType) :: this
    logical(LGP) :: skip

    skip = (this%src%isize == 0)

  end function skip_sync

  !> @brief Copy 1d integer array with map.
  !< TODO_MJR: should this maybe move to the memory manager for more convenient maintenance?
  subroutine sync_int1d(this)
    class(MappedVariableType) :: this
    ! local
    integer(I4B) :: i

    do i = 1, size(this%tgt_idx)
      this%tgt%aint1d(this%tgt_idx(i)) = this%src%aint1d(this%src_idx(i))
    end do

  end subroutine sync_int1d

  subroutine apply_sgn_int1d(this)
    class(MappedVariableType) :: this
    ! local
    integer(I4B) :: i

    do i = 1, size(this%tgt_idx)
      this%tgt%aint1d(this%tgt_idx(i)) = this%tgt%aint1d(this%tgt_idx(i)) * &
                                         this%sign(i)
    end do

  end subroutine apply_sgn_int1d

  !> @brief Copy 1d double array with map.
  !<
  subroutine sync_dbl1d(this)
    class(MappedVariableType) :: this
    ! local
    integer(I4B) :: i

    do i = 1, size(this%tgt_idx)
      this%tgt%adbl1d(this%tgt_idx(i)) = this%src%adbl1d(this%src_idx(i))
    end do

  end subroutine sync_dbl1d

  subroutine apply_sgn_dbl1d(this)
    class(MappedVariableType) :: this
    ! local
    integer(I4B) :: i

    do i = 1, size(this%tgt_idx)
      this%tgt%adbl1d(this%tgt_idx(i)) = this%tgt%adbl1d(this%tgt_idx(i)) * &
                                         this%sign(i)
    end do

  end subroutine apply_sgn_dbl1d

  !> @brief Copy 2d double array with map.
  !< NB: only dim=2 is mapped.
  subroutine sync_dbl2d(this)
    class(MappedVariableType) :: this
    ! local
    integer(I4B) :: i, k

    do i = 1, size(this%tgt_idx)
      do k = 1, size(this%src%adbl2d, dim=1)
        this%tgt%adbl2d(k, this%tgt_idx(i)) = this%src%adbl2d(k, this%src_idx(i))
      end do
    end do

  end subroutine sync_dbl2d

  subroutine apply_sgn_dbl2d(this)
    class(MappedVariableType) :: this
    ! local
    integer(I4B) :: i, k

    do i = 1, size(this%tgt_idx)
      do k = 1, size(this%src%adbl2d, dim=1)
        this%tgt%adbl2d(k, this%tgt_idx(i)) = &
          this%tgt%adbl2d(k, this%tgt_idx(i)) * this%sign(i)
      end do
    end do

  end subroutine apply_sgn_dbl2d

  function CastAsMappedVariable(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(MappedVariableType), pointer :: res

    res => null()

    select type (obj)
    class is (MappedVariableType)
      res => obj
    end select

  end function CastAsMappedVariable

end module MappedVariableModule
