module TestTimeStepSelectModule
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use TimeStepSelectModule, only: TimeStepSelectType
  use ConstantsModule, only: LINELENGTH

  implicit none
  private
  public :: collect_period_block_timing

contains

  subroutine collect_period_block_timing(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("first", test_first), &
                new_unittest("last", test_last), &
                new_unittest("all", test_all), &
                new_unittest("freq", test_freq), &
                new_unittest("step", test_step) &
                ]
  end subroutine collect_period_block_timing

  subroutine test_first(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "FIRST"

    call steps%init()
    call steps%read(line)

    call check(error, steps%is_selected(0, .false.))
    if (allocated(error)) return

    call check(error, .not. steps%is_selected(1, .false.))
    if (allocated(error)) return

  end subroutine test_first

  subroutine test_last(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "LAST"

    call steps%init()
    call steps%read(line)

    call check(error, .not. steps%is_selected(0, .false.))
    if (allocated(error)) return
    
    call check(error, steps%is_selected(0, .true.))
    if (allocated(error)) return

  end subroutine test_last

  subroutine test_all(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "ALL"

    call steps%init()
    call steps%read(line)

    call check(error, steps%is_selected(0, .true.))
    if (allocated(error)) return

    call check(error, steps%is_selected(0, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(1, .true.))
    if (allocated(error)) return

    call check(error, steps%is_selected(1, .false.))
    if (allocated(error)) return

  end subroutine test_all

  subroutine test_freq(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "FREQUENCY 2"

    call steps%init()
    call steps%read(line)

    call check(error, steps%is_selected(0, .false.))
    if (allocated(error)) return

    call check(error, .not. steps%is_selected(1, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(2, .false.))
    if (allocated(error)) return

    call check(error, .not. steps%is_selected(3, .false.))
    if (allocated(error)) return

  end subroutine test_freq

  subroutine test_step(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "STEP 1"

    call steps%init()
    call steps%read(line)

    call check(error, .not. steps%is_selected(0, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(1, .false.))
    if (allocated(error)) return

    call check(error, .not. steps%is_selected(2, .false.))
    if (allocated(error)) return

  end subroutine test_step

end module TestTimeStepSelectModule