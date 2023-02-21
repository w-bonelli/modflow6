module VirtualGwtExchangeModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualBaseModule
  use VirtualDataListsModule, only: virtual_exchange_list
  use VirtualDataContainerModule, only: VDC_GWTEXG_TYPE
  use VirtualExchangeModule
  implicit none
  private

  public :: add_virtual_gwt_exchange

  type, public, extends(VirtualExchangeType) :: VirtualGwtExchangeType
    type(VirtualDbl1dType), pointer :: gwfsimvals => null()
  contains
    procedure :: create => vtx_create
    procedure :: destroy => vtx_destroy
    procedure :: prepare_stage => vtx_prepare_stage
    ! private
    procedure, private :: create_virtual_fields
    procedure, private :: deallocate_data
  end type VirtualGwtExchangeType

contains

!> @brief Add a virtual GWT-GWT exchange to the simulation
!<
  subroutine add_virtual_gwt_exchange(name, exchange_id, model1_id, model2_id)
    character(len=*) :: name
    integer(I4B) :: exchange_id
    integer(I4B) :: model1_id
    integer(I4B) :: model2_id
    ! local
    class(VirtualGwtExchangeType), pointer :: v_exg
    class(*), pointer :: obj_ptr

    allocate (v_exg)
    call v_exg%create(name, exchange_id, model1_id, model2_id)

    obj_ptr => v_exg
    call virtual_exchange_list%Add(obj_ptr)

  end subroutine add_virtual_gwt_exchange

!> @brief Create a virtual GWT-GWT exchange
!<
  subroutine vtx_create(this, name, exg_id, m1_id, m2_id)
    class(VirtualGwtExchangeType) :: this
    character(len=*) :: name
    integer(I4B) :: exg_id
    integer(I4B) :: m1_id
    integer(I4B) :: m2_id

    ! create base
    call this%VirtualExchangeType%create(name, exg_id, m1_id, m2_id)
    this%container_type = VDC_GWTEXG_TYPE

    ! allocate gwtgwt field(s)
    call this%create_virtual_fields()

  end subroutine vtx_create

  subroutine create_virtual_fields(this)
    class(VirtualGwtExchangeType) :: this

    allocate (this%gwfsimvals)
    call this%create_field(this%gwfsimvals%to_base(), 'GWFSIMVALS', '')

  end subroutine create_virtual_fields

  subroutine vtx_prepare_stage(this, stage)
    class(VirtualGwtExchangeType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: nexg

    ! prepare base exchange data items
    call this%VirtualExchangeType%prepare_stage(stage)

    if (stage == STG_BEFORE_AR) then
      nexg = this%nexg%get()
      call this%map(this%gwfsimvals%to_base(), nexg, &
                    (/STG_BEFORE_AD/), MAP_ALL_TYPE)
    end if

  end subroutine vtx_prepare_stage

  subroutine vtx_destroy(this)
    class(VirtualGwtExchangeType) :: this

    call this%VirtualExchangeType%destroy()
    call this%deallocate_data()

  end subroutine vtx_destroy

  subroutine deallocate_data(this)
    class(VirtualGwtExchangeType) :: this

    deallocate (this%gwfsimvals)

  end subroutine deallocate_data

end module VirtualGwtExchangeModule
