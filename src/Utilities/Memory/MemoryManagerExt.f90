module MemoryManagerExtModule

  use KindModule, only: DP, LGP, I4B, I8B
  use SimModule, only: store_error
  use MemoryTypeModule, only: MemoryType
  use MemoryManagerModule, only: memorylist, get_from_memorylist

  implicit none
  private
  public :: mem_set_value
  public :: memorylist_remove

  interface mem_set_value
    module procedure mem_set_value_logical, mem_set_value_int, &
      mem_set_value_int_setval, mem_set_value_str_mapped_int, &
      mem_set_value_int1d, mem_set_value_int1d_mapped, &
      mem_set_value_int2d, mem_set_value_int3d, mem_set_value_dbl, &
      mem_set_value_dbl1d, mem_set_value_dbl1d_mapped, &
      mem_set_value_dbl2d, mem_set_value_dbl3d, mem_set_value_str
  end interface mem_set_value

contains

  subroutine memorylist_remove(component, subcomponent, context)
    use MemoryHelperModule, only: create_mem_path
    use ConstantsModule, only: LENMEMPATH
    character(len=*), intent(in) :: component !< name of the solution, model, or exchange
    character(len=*), intent(in), optional :: subcomponent !< name of the package (optional)
    character(len=*), intent(in), optional :: context !< name of the context (optional)
    character(len=LENMEMPATH) :: memory_path !< the memory path
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical(LGP) :: removed

    memory_path = create_mem_path(component, subcomponent, context)
    removed = .true. !< initialize the loop

    do while (removed)
      removed = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if (mt%path == memory_path .and. mt%mt_associated()) then
          call mt%mt_deallocate()
          deallocate (mt)
          call memorylist%remove(ipos, .false.)
          removed = .true.
          exit
        end if
      end do
    end do
  end subroutine memorylist_remove

  !> @brief Set pointer to value of memory list logical variable
  !<
  subroutine mem_set_value_logical(p_mem, varname, memory_path, found)
    logical(LGP), pointer, intent(inout) :: p_mem !< pointer to logical scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'LOGICAL') then
      p_mem = mt%logicalsclr
    end if
  end subroutine mem_set_value_logical

  !> @brief Set pointer to value of memory list int variable
  !<
  subroutine mem_set_value_int(p_mem, varname, memory_path, found)
    integer(I4B), pointer, intent(inout) :: p_mem !< pointer to int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      p_mem = mt%intsclr
    end if
  end subroutine mem_set_value_int

  subroutine mem_set_value_int_setval(p_mem, varname, memory_path, setval, found)
    integer(I4B), pointer, intent(inout) :: p_mem !< pointer to int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    integer(I4B), intent(in) :: setval !< set p_mem to setval if varname found
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found) then
      p_mem = setval
    end if
  end subroutine mem_set_value_int_setval

  subroutine mem_set_value_str_mapped_int(p_mem, varname, memory_path, str_list, &
                                          found)
    integer(I4B), pointer, intent(inout) :: p_mem !< pointer to int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    character(len=*), dimension(:), intent(in) :: str_list
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'STRING') then
      do i = 1, size(str_list)
        if (mt%strsclr == str_list(i)) then
          p_mem = i
        end if
      end do
    end if
  end subroutine mem_set_value_str_mapped_int

  !> @brief Set pointer to value of memory list 1d int array variable
  !<
  subroutine mem_set_value_int1d(p_mem, varname, memory_path, found)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: n

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      if (size(mt%aint1d) /= size(p_mem)) then
        call store_error('mem_set_value() size mismatch int1d, varname='//&
                         &trim(varname), terminate=.TRUE.)
      end if
      do n = 1, size(mt%aint1d)
        p_mem(n) = mt%aint1d(n)
      end do
    end if
  end subroutine mem_set_value_int1d

  !> @brief Set pointer to value of memory list 1d int array variable with mapping
  !<
  subroutine mem_set_value_int1d_mapped(p_mem, varname, memory_path, map, &
                                        found)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: map !< pointer to 1d int mapping array
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: n

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      if (associated(map)) then
        do n = 1, size(p_mem)
          p_mem(n) = mt%aint1d(map(n))
        end do
      else
        if (size(mt%aint1d) /= size(p_mem)) then
          call store_error('mem_set_value() size mismatch int1d, varname='//&
                           &trim(varname), terminate=.TRUE.)
        end if
        do n = 1, size(mt%aint1d)
          p_mem(n) = mt%aint1d(n)
        end do
      end if
    end if
  end subroutine mem_set_value_int1d_mapped

  !> @brief Set pointer to value of memory list 2d int array variable
  !<
  subroutine mem_set_value_int2d(p_mem, varname, memory_path, found)
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 2d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, j

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      if (size(mt%aint2d, dim=1) /= size(p_mem, dim=1) .or. &
          size(mt%aint2d, dim=2) /= size(p_mem, dim=2)) then
        call store_error('mem_set_value() size mismatch int2d, varname='//&
                         &trim(varname), terminate=.TRUE.)
      end if
      do j = 1, size(mt%aint2d, dim=2)
        do i = 1, size(mt%aint2d, dim=1)
          p_mem(i, j) = mt%aint2d(i, j)
        end do
      end do
    end if
  end subroutine mem_set_value_int2d

  !> @brief Set pointer to value of memory list 3d int array variable
  !<
  subroutine mem_set_value_int3d(p_mem, varname, memory_path, found)
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 3d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, j, k

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      if (size(mt%aint3d, dim=1) /= size(p_mem, dim=1) .or. &
          size(mt%aint3d, dim=2) /= size(p_mem, dim=2) .or. &
          size(mt%aint3d, dim=3) /= size(p_mem, dim=3)) then
        call store_error('mem_set_value() size mismatch int3d, varname='//&
                         &trim(varname), terminate=.TRUE.)
      end if
      do k = 1, size(mt%aint3d, dim=3)
        do j = 1, size(mt%aint3d, dim=2)
          do i = 1, size(mt%aint3d, dim=1)
            p_mem(i, j, k) = mt%aint3d(i, j, k)
          end do
        end do
      end do
    end if
  end subroutine mem_set_value_int3d

  !> @brief Set pointer to value of memory list double variable
  !<
  subroutine mem_set_value_dbl(p_mem, varname, memory_path, found)
    real(DP), pointer, intent(inout) :: p_mem !< pointer to dbl scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      p_mem = mt%dblsclr
    end if
  end subroutine mem_set_value_dbl

  !> @brief Set pointer to value of memory list 1d dbl array variable
  !<
  subroutine mem_set_value_dbl1d(p_mem, varname, memory_path, found)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: n

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      if (size(mt%adbl1d) /= size(p_mem)) then
        call store_error('mem_set_value() size mismatch dbl1d, varname='//&
                         &trim(varname), terminate=.TRUE.)
      end if
      do n = 1, size(mt%adbl1d)
        p_mem(n) = mt%adbl1d(n)
      end do
    end if
  end subroutine mem_set_value_dbl1d

  !> @brief Set pointer to value of memory list 1d dbl array variable with mapping
  !<
  subroutine mem_set_value_dbl1d_mapped(p_mem, varname, memory_path, map, &
                                        found)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: map !< pointer to 1d int mapping array
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: n

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      if (associated(map)) then
        do n = 1, size(p_mem)
          p_mem(n) = mt%adbl1d(map(n))
        end do
      else
        if (size(mt%adbl1d) /= size(p_mem)) then
          call store_error('mem_set_value() size mismatch dbl1d, varname='//&
                           &trim(varname), terminate=.TRUE.)
        end if
        do n = 1, size(mt%adbl1d)
          p_mem(n) = mt%adbl1d(n)
        end do
      end if
    end if
  end subroutine mem_set_value_dbl1d_mapped

  !> @brief Set pointer to value of memory list 2d dbl array variable
  !<
  subroutine mem_set_value_dbl2d(p_mem, varname, memory_path, found)
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, j

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      if (size(mt%adbl2d, dim=1) /= size(p_mem, dim=1) .or. &
          size(mt%adbl2d, dim=2) /= size(p_mem, dim=2)) then
        call store_error('mem_set_value() size mismatch dbl2d, varname='//&
                         &trim(varname), terminate=.TRUE.)
      end if
      do j = 1, size(mt%adbl2d, dim=2)
        do i = 1, size(mt%adbl2d, dim=1)
          p_mem(i, j) = mt%adbl2d(i, j)
        end do
      end do
    end if
  end subroutine mem_set_value_dbl2d

  !> @brief Set pointer to value of memory list 3d dbl array variable
  !<
  subroutine mem_set_value_dbl3d(p_mem, varname, memory_path, found)
    real(DP), dimension(:, :, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 3d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, j, k

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      if (size(mt%adbl3d, dim=1) /= size(p_mem, dim=1) .or. &
          size(mt%adbl3d, dim=2) /= size(p_mem, dim=2) .or. &
          size(mt%adbl3d, dim=3) /= size(p_mem, dim=3)) then
        call store_error('mem_set_value() size mismatch dbl3d, varname='//&
                         &trim(varname), terminate=.TRUE.)
      end if
      do k = 1, size(mt%adbl3d, dim=3)
        do j = 1, size(mt%adbl3d, dim=2)
          do i = 1, size(mt%adbl3d, dim=1)
            p_mem(i, j, k) = mt%adbl3d(i, j, k)
          end do
        end do
      end do
    end if
  end subroutine mem_set_value_dbl3d

  subroutine mem_set_value_str(p_mem, varname, memory_path, found)
    character(len=*), intent(inout) :: p_mem !< pointer to str scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    logical(LGP), intent(inout) :: found
    type(MemoryType), pointer :: mt
    logical(LGP) :: checkfail = .false.

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'STRING') then
      p_mem = mt%strsclr
    end if
  end subroutine mem_set_value_str

end module MemoryManagerExtModule
