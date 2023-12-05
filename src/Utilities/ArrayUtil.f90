module ArrayUtilModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DZERO, DTEN
  use SimVariablesModule, only: iout
  use GenericUtilitiesModule, only: sim_message, stop_with_error
  implicit none
  private
  public :: ExpandArray, ExpandArray2D, ExpandArrayWrapper, ExtendPtrArray
  public :: ConcatArray
  public :: ifind
  public :: remove_character

  interface ExpandArrayWrapper
    module procedure expand_integer_wrapper
  end interface

  interface ExpandArray
    ! This interface is for use with ALLOCATABLE arrays.
    ! IMPORTANT: Do not use pointers to elements of arrays when using
    ! ExpandArray to increase the array size!  The locations of array
    ! elements in memory are changed when ExpandArray is invoked.
    module procedure expand_integer, expand_double, expand_logical, &
      expand_character
  end interface ExpandArray

  interface ExpandArray2D
    ! This interface is for use with ALLOCATABLE arrays.
    ! IMPORTANT: Do not use pointers to elements of arrays when using
    ! ExpandArray2D to increase the array size!  The locations of array
    ! elements in memory are changed when ExpandArray2D is invoked.
    module procedure expand_integer_2d, expand_double_2d
  end interface ExpandArray2D

  interface ExtendPtrArray
    ! This interface is for use with POINTERS to arrays.
    module procedure extend_double, extend_integer, &
      extend_string
  end interface

  interface ConcatArray
    module procedure concat_integer
  end interface

  interface ifind
    module procedure ifind_character, ifind_integer
  end interface ifind

contains

  subroutine expand_integer_wrapper(nsize, array, minvalue, loginc)
    ! -- dummy
    integer(I4B), intent(in) :: nsize
    integer(I4B), allocatable, intent(inout) :: array(:)
    integer(I4B), intent(in), optional :: minvalue
    logical(LGP), intent(in), optional :: loginc
    ! -- local
    logical(LGP) :: log_increment
    integer(I4B) :: minimum_increment
    integer(I4B) :: increment
    integer(I4B) :: isize
    integer(I4B) :: n
    !
    ! -- process optional variables
    if (present(minvalue)) then
      minimum_increment = minvalue
    else
      minimum_increment = 100
    end if
    if (present(loginc)) then
      log_increment = loginc
    else
      log_increment = .FALSE.
    end if
    !
    ! -- determine current size of the array
    isize = size(array)
    !
    ! -- expand the array if necessary
    if (nsize > isize) then
      !
      ! -- increase array size by 1, 10, 100, 1000, etc.
      !    from 1 to 9, 10 to 99, 100 to 999, 1000 to 10000, etc.
      if (loginc) then
        increment = int(log10(real(nsize, DP)), I4B)
        increment = int(DTEN**increment, I4B)
        !
        ! -- increase increment by a multiplier and a value no
        !    smaller than a default or specified minimum size
      else
        increment = int(nsize * 0.2_DP)
        increment = max(minimum_increment, increment)
      end if
      !
      ! -- expand the array
      call ExpandArray(array, increment)
      !
      ! -- initialize expanded array elements
      do n = isize + 1, size(array)
        array(n) = 0
      end do
    end if

  end subroutine expand_integer_wrapper

  ! -- Specific procedures that implement ExpandArray for allocatable arrays

  subroutine expand_integer(array, increment)
    ! -- dummy
    integer(I4B), allocatable, intent(inout) :: array(:)
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    integer(I4B) :: inclocal, isize, newsize
    integer(I4B), allocatable, dimension(:) :: array_temp
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    end if
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (allocated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate (array_temp(newsize))
      array_temp(1:isize) = array
      deallocate (array)
      call move_alloc(array_temp, array)
    else
      allocate (array(inclocal))
    end if

  end subroutine expand_integer

  subroutine expand_double(array, increment)
    ! -- dummy
    real(DP), allocatable, intent(inout) :: array(:)
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    integer(I4B) :: inclocal, isize, newsize
    real(DP), allocatable, dimension(:) :: array_temp
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    end if
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (allocated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate (array_temp(newsize))
      array_temp(1:isize) = array
      deallocate (array)
      call move_alloc(array_temp, array)
    else
      allocate (array(inclocal))
    end if

  end subroutine expand_double

  subroutine expand_logical(array, increment)
    ! -- dummy
    logical(LGP), allocatable, intent(inout) :: array(:)
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    integer(I4B) :: inclocal, isize, newsize
    logical(LGP), allocatable, dimension(:) :: array_temp
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    end if
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (allocated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate (array_temp(newsize))
      array_temp(1:isize) = array
      deallocate (array)
      call move_alloc(array_temp, array)
    else
      allocate (array(inclocal))
    end if

  end subroutine expand_logical

  subroutine expand_character(array, increment)
    ! -- dummy
    character(len=*), allocatable, intent(inout) :: array(:)
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=MAXCHARLEN), allocatable, dimension(:) :: array_temp
    integer(I4B) :: i, inclocal, isize, lenc, newsize
    ! -- format
    character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/,1x,a)"
    !
    ! -- check character length
    lenc = len(array)
    if (lenc > MAXCHARLEN) then
      write (line, '(a)') 'Error in ArrayUtilModule: '// &
        'Need to increase MAXCHARLEN'
      call sim_message(line, iunit=iout, fmt=stdfmt)
      call sim_message(line, fmt=stdfmt)
      !
      ! -- stop message
      write (line, '(a)') 'Stopping...'
      call sim_message(line, iunit=iout)
      call sim_message(line)
      call stop_with_error(138)
    end if
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    end if
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    ! [Ned TODO: may be able to use mold here, e.g.:
    !       allocate(values(num), mold=proto)]
    if (allocated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate (array_temp(isize))
      do i = 1, isize
        array_temp(i) = array(i)
      end do
      deallocate (array)
      allocate (array(newsize))
      do i = 1, isize
        array(i) = array_temp(i)
      end do
      do i = isize + 1, newsize
        array(i) = ''
      end do
      deallocate (array_temp)
    else
      allocate (array(inclocal))
    end if

  end subroutine expand_character

  ! -- Specific procedures that implement ExtendArray2D

  subroutine expand_integer_2d(array, increment1, increment2)
    ! -- dummy
    integer(I4B), allocatable, intent(inout) :: array(:, :)
    integer(I4B), optional, intent(in) :: increment1
    integer(I4B), optional, intent(in) :: increment2
    ! -- local
    integer(I4B) :: inclocal1, inclocal2, isize1, isize2, newsize1, newsize2
    integer(I4B), allocatable, dimension(:, :) :: array_temp
    !
    ! -- initialize
    if (present(increment1)) then
      inclocal1 = increment1
    else
      inclocal1 = 1
    end if
    if (present(increment2)) then
      inclocal2 = increment2
    else
      inclocal2 = 1
    end if
    !
    ! -- increase size of array by inclocal corresponding to each dim,
    !    retaining contained data
    if (allocated(array)) then
      isize1 = size(array, 1)
      isize2 = size(array, 2)
      newsize1 = isize1 + inclocal1
      newsize2 = isize2 + inclocal2
      allocate (array_temp(newsize1, newsize2))
      array_temp(1:isize1, 1:isize2) = array
      deallocate (array)
      call move_alloc(array_temp, array)
    else
      allocate (array(inclocal1, inclocal2))
    end if

  end subroutine expand_integer_2d

  subroutine expand_double_2d(array, increment1, increment2)
    ! -- dummy
    real(DP), allocatable, intent(inout) :: array(:, :)
    integer(I4B), optional, intent(in) :: increment1
    integer(I4B), optional, intent(in) :: increment2
    ! -- local
    integer(I4B) :: inclocal1, inclocal2, isize1, isize2, newsize1, newsize2
    real(DP), allocatable, dimension(:, :) :: array_temp
    !
    ! -- initialize
    if (present(increment1)) then
      inclocal1 = increment1
    else
      inclocal1 = 1
    end if
    if (present(increment2)) then
      inclocal2 = increment2
    else
      inclocal2 = 1
    end if
    !
    ! -- increase size of array by inclocal corresponding to each dim,
    !    retaining contained data
    if (allocated(array)) then
      isize1 = size(array, 1)
      isize2 = size(array, 2)
      newsize1 = isize1 + inclocal1
      newsize2 = isize2 + inclocal2
      allocate (array_temp(newsize1, newsize2))
      array_temp(1:isize1, 1:isize2) = array
      deallocate (array)
      call move_alloc(array_temp, array)
    else
      allocate (array(inclocal1, inclocal2))
    end if

  end subroutine expand_double_2d

  ! -- Specific procedures that implement ExtendPtrArray for pointer arrays

  subroutine extend_double(array, increment)
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: array
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=100) :: ermsg
    integer(I4B) :: i, inclocal, isize, istat, newsize
    real(DP), dimension(:), pointer, contiguous :: array_temp => null()
    ! -- format
    character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/,1x,a)"
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    end if
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (associated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate (array_temp(newsize), stat=istat, errmsg=ermsg)
      if (istat /= 0) goto 99
      do i = 1, isize
        array_temp(i) = array(i)
      end do
      deallocate (array)
      array => array_temp
    else
      allocate (array(inclocal))
    end if
    !
    ! -- normal return
    return
    !
    ! -- Error reporting
99  continue

    write (line, '(a)') 'Error in ArrayUtilModule: '// &
      'Could not increase array size'
    call sim_message(line, iunit=iout, fmt=stdfmt)
    call sim_message(line, fmt=stdfmt)
    !
    ! -- error message
    call sim_message(ermsg, iunit=iout)
    call sim_message(ermsg)
    !
    ! -- stop message
    write (line, '(a)') 'Stopping...'
    call sim_message(line, iunit=iout)
    call sim_message(line)
    call stop_with_error(138)

  end subroutine extend_double

  subroutine extend_integer(array, increment)
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: array
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=100) :: ermsg
    integer(I4B) :: i, inclocal, isize, istat, newsize
    integer(I4B), dimension(:), pointer, contiguous :: array_temp => null()
    ! -- format
    character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/,1x,a)"
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    end if
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (associated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate (array_temp(newsize), stat=istat, errmsg=ermsg)
      if (istat /= 0) goto 99
      do i = 1, isize
        array_temp(i) = array(i)
      end do
      deallocate (array)
      array => array_temp
    else
      allocate (array(inclocal))
    end if
    !
    ! -- normal return
    return
    !
    ! -- Error reporting
99  continue

    write (line, '(a)') 'Error in ArrayUtilModule: '// &
      'Could not increase array size'
    call sim_message(line, iunit=iout, fmt=stdfmt)
    call sim_message(line, fmt=stdfmt)
    !
    ! -- error message
    call sim_message(ermsg, iunit=iout)
    call sim_message(ermsg)
    !
    ! -- stop message
    write (line, '(a)') 'Stopping...'
    call sim_message(line, iunit=iout)
    call sim_message(line)
    call stop_with_error(138)

  end subroutine extend_integer

  subroutine extend_string(array, increment)
    character(len=*), dimension(:), pointer, contiguous :: array
    integer(I4B), optional :: increment
    ! local
    integer(I4B) :: inc_local
    integer(I4B) :: i, old_size, new_size
    character(len=len(array)), dimension(:), pointer, contiguous :: temp_array

    if (present(increment)) then
      inc_local = increment
    else
      inc_local = 1
    end if

    if (associated(array)) then
      old_size = size(array)
      new_size = old_size + inc_local
      temp_array => array
      allocate (array(new_size))
      do i = 1, old_size
        array(i) = temp_array(i)
      end do
      deallocate (temp_array)
    else
      allocate (array(inc_local))
    end if

  end subroutine extend_string

  !> @brief Concatenate integer arrays.
  subroutine concat_integer(array, array_to_add)
    integer(I4B), dimension(:), pointer, contiguous :: array
    integer(I4B), dimension(:), pointer, contiguous :: array_to_add
    ! local
    integer(I4B) :: i, old_size

    old_size = size(array)
    call ExtendPtrArray(array, increment=size(array_to_add))
    do i = 1, size(array_to_add)
      array(old_size + i) = array_to_add(i)
    end do

  end subroutine concat_integer

  !> @brief Find the 1st array element containing str, or -1 if not found.
  function ifind_character(array, str)
    ! -- return
    integer(I4B) :: ifind_character
    ! -- dummy
    character(len=*), dimension(:) :: array
    character(len=*) :: str
    ! -- local
    integer(I4B) :: i
    ifind_character = -1
    findloop: do i = 1, size(array)
      if (array(i) == str) then
        ifind_character = i
        exit findloop
      end if
    end do findloop
  end function ifind_character

  !> @brief Find the first element containing str, or -1 if not found.
  function ifind_integer(iarray, ival)
    ! -- return
    integer(I4B) :: ifind_integer
    ! -- dummy
    integer(I4B), dimension(:) :: iarray
    integer(I4B) :: ival
    ! -- local
    integer(I4B) :: i
    ifind_integer = -1
    findloop: do i = 1, size(iarray)
      if (iarray(i) == ival) then
        ifind_integer = i
        exit findloop
      end if
    end do findloop
  end function ifind_integer

  !> @brief Remove the element at ipos from the array.
  subroutine remove_character(array, ipos)
    ! -- dummy
    character(len=*), allocatable, intent(inout) :: array(:)
    integer(I4B), intent(in) :: ipos
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=MAXCHARLEN), allocatable, dimension(:) :: array_temp
    integer(I4B) :: i, isize, lenc, newsize, inew
    ! -- format
    character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/,1x,a)"
    !
    ! -- check character length
    lenc = len(array)
    if (lenc > MAXCHARLEN) then

      write (line, '(a)') 'Error in ArrayUtilModule: '// &
        'Need to increase MAXCHARLEN'
      call sim_message(line, iunit=iout, fmt=stdfmt)
      call sim_message(line, fmt=stdfmt)
      !
      ! -- stop message
      write (line, '(a)') 'Stopping...'
      call sim_message(line, iunit=iout)
      call sim_message(line)
      call stop_with_error(138)
    end if
    !
    ! -- calculate sizes
    isize = size(array)
    newsize = isize - 1
    !
    ! -- copy array to array_temp
    allocate (array_temp(isize))
    do i = 1, isize
      array_temp(i) = array(i)
    end do
    !
    deallocate (array)
    allocate (array(newsize))
    inew = 1
    do i = 1, isize
      if (i /= ipos) then
        array(inew) = array_temp(i)
        inew = inew + 1
      end if
    end do
    deallocate (array_temp)

  end subroutine remove_character

end module ArrayUtilModule
