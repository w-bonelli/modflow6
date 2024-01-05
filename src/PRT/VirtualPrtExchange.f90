module VirtualPrtPrtExchangeModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualBaseModule
  use VirtualDataListsModule, only: virtual_exchange_list
  use VirtualExchangeModule
  implicit none
  private

  public :: add_virtual_prtprt_exchange

  type, public, extends(VirtualExchangeType) :: VirtualPrtPrtExchangeType
    type(VirtualDbl1dType), pointer :: gwfsimvals => null()
  contains
    procedure :: create => vtx_create
    procedure :: destroy => vtx_destroy
    procedure :: prepare_stage => vtx_prepare_stage
    ! private
    procedure, private :: init_virtual_data
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualPrtPrtExchangeType

contains

  !> @brief Add a virtual PRT-PRT exchange to the simulation
  !<
  subroutine add_virtual_prtprt_exchange(name, exchange_id, &
                                         model1_id, model2_id)
    character(len=*) :: name
    integer(I4B) :: exchange_id
    integer(I4B) :: model1_id
    integer(I4B) :: model2_id
    ! noop
  end subroutine add_virtual_prtprt_exchange

  !> @brief Create a virtual PRT-PRT exchange
  !<
  subroutine vtx_create(this, name, exg_id, m1_id, m2_id)
    class(VirtualPrtPrtExchangeType) :: this
    character(len=*) :: name
    integer(I4B) :: exg_id
    integer(I4B) :: m1_id
    integer(I4B) :: m2_id
    ! noop
  end subroutine vtx_create

  subroutine init_virtual_data(this)
    class(VirtualPrtPrtExchangeType) :: this
    ! noop
  end subroutine init_virtual_data

  subroutine vtx_prepare_stage(this, stage)
    class(VirtualPrtPrtExchangeType) :: this
    integer(I4B) :: stage
    ! noop
  end subroutine vtx_prepare_stage

  subroutine vtx_destroy(this)
    class(VirtualPrtPrtExchangeType) :: this
    ! noop
  end subroutine vtx_destroy

  subroutine allocate_data(this)
    class(VirtualPrtPrtExchangeType) :: this
    ! noop
  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualPrtPrtExchangeType) :: this
    ! noop
  end subroutine deallocate_data

end module VirtualPrtPrtExchangeModule
