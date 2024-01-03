! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmGwfDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use GwfChdInputModule
  use GwfDisInputModule
  use GwfDisuInputModule
  use GwfDisvInputModule
  use GwfDrnInputModule
  use GwfEvtInputModule
  use GwfEvtaInputModule
  use GwfGhbInputModule
  use GwfIcInputModule
  use GwfNpfInputModule
  use GwfRchInputModule
  use GwfRchaInputModule
  use GwfRivInputModule
  use GwfWelInputModule
  use GwfNamInputModule

  implicit none
  private
  public :: gwf_param_definitions
  public :: gwf_aggregate_definitions
  public :: gwf_block_definitions
  public :: gwf_idm_multi_package
  public :: gwf_idm_integrated

contains

  subroutine set_param_pointer(input_dfn, input_dfn_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_dfn
    type(InputParamDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine set_param_pointer

  subroutine set_block_pointer(input_dfn, input_dfn_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_dfn
    type(InputBlockDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine set_block_pointer

  function gwf_param_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('CHD')
      call set_param_pointer(input_definition, gwf_chd_param_definitions)
    case ('DIS')
      call set_param_pointer(input_definition, gwf_dis_param_definitions)
    case ('DISU')
      call set_param_pointer(input_definition, gwf_disu_param_definitions)
    case ('DISV')
      call set_param_pointer(input_definition, gwf_disv_param_definitions)
    case ('DRN')
      call set_param_pointer(input_definition, gwf_drn_param_definitions)
    case ('EVT')
      call set_param_pointer(input_definition, gwf_evt_param_definitions)
    case ('EVTA')
      call set_param_pointer(input_definition, gwf_evta_param_definitions)
    case ('GHB')
      call set_param_pointer(input_definition, gwf_ghb_param_definitions)
    case ('IC')
      call set_param_pointer(input_definition, gwf_ic_param_definitions)
    case ('NPF')
      call set_param_pointer(input_definition, gwf_npf_param_definitions)
    case ('RCH')
      call set_param_pointer(input_definition, gwf_rch_param_definitions)
    case ('RCHA')
      call set_param_pointer(input_definition, gwf_rcha_param_definitions)
    case ('RIV')
      call set_param_pointer(input_definition, gwf_riv_param_definitions)
    case ('WEL')
      call set_param_pointer(input_definition, gwf_wel_param_definitions)
    case ('NAM')
      call set_param_pointer(input_definition, gwf_nam_param_definitions)
    case default
    end select
    return
  end function gwf_param_definitions

  function gwf_aggregate_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('CHD')
      call set_param_pointer(input_definition, gwf_chd_aggregate_definitions)
    case ('DIS')
      call set_param_pointer(input_definition, gwf_dis_aggregate_definitions)
    case ('DISU')
      call set_param_pointer(input_definition, gwf_disu_aggregate_definitions)
    case ('DISV')
      call set_param_pointer(input_definition, gwf_disv_aggregate_definitions)
    case ('DRN')
      call set_param_pointer(input_definition, gwf_drn_aggregate_definitions)
    case ('EVT')
      call set_param_pointer(input_definition, gwf_evt_aggregate_definitions)
    case ('EVTA')
      call set_param_pointer(input_definition, gwf_evta_aggregate_definitions)
    case ('GHB')
      call set_param_pointer(input_definition, gwf_ghb_aggregate_definitions)
    case ('IC')
      call set_param_pointer(input_definition, gwf_ic_aggregate_definitions)
    case ('NPF')
      call set_param_pointer(input_definition, gwf_npf_aggregate_definitions)
    case ('RCH')
      call set_param_pointer(input_definition, gwf_rch_aggregate_definitions)
    case ('RCHA')
      call set_param_pointer(input_definition, gwf_rcha_aggregate_definitions)
    case ('RIV')
      call set_param_pointer(input_definition, gwf_riv_aggregate_definitions)
    case ('WEL')
      call set_param_pointer(input_definition, gwf_wel_aggregate_definitions)
    case ('NAM')
      call set_param_pointer(input_definition, gwf_nam_aggregate_definitions)
    case default
    end select
    return
  end function gwf_aggregate_definitions

  function gwf_block_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('CHD')
      call set_block_pointer(input_definition, gwf_chd_block_definitions)
    case ('DIS')
      call set_block_pointer(input_definition, gwf_dis_block_definitions)
    case ('DISU')
      call set_block_pointer(input_definition, gwf_disu_block_definitions)
    case ('DISV')
      call set_block_pointer(input_definition, gwf_disv_block_definitions)
    case ('DRN')
      call set_block_pointer(input_definition, gwf_drn_block_definitions)
    case ('EVT')
      call set_block_pointer(input_definition, gwf_evt_block_definitions)
    case ('EVTA')
      call set_block_pointer(input_definition, gwf_evta_block_definitions)
    case ('GHB')
      call set_block_pointer(input_definition, gwf_ghb_block_definitions)
    case ('IC')
      call set_block_pointer(input_definition, gwf_ic_block_definitions)
    case ('NPF')
      call set_block_pointer(input_definition, gwf_npf_block_definitions)
    case ('RCH')
      call set_block_pointer(input_definition, gwf_rch_block_definitions)
    case ('RCHA')
      call set_block_pointer(input_definition, gwf_rcha_block_definitions)
    case ('RIV')
      call set_block_pointer(input_definition, gwf_riv_block_definitions)
    case ('WEL')
      call set_block_pointer(input_definition, gwf_wel_block_definitions)
    case ('NAM')
      call set_block_pointer(input_definition, gwf_nam_block_definitions)
    case default
    end select
    return
  end function gwf_block_definitions

  function gwf_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('CHD')
      multi_package = gwf_chd_multi_package
    case ('DIS')
      multi_package = gwf_dis_multi_package
    case ('DISU')
      multi_package = gwf_disu_multi_package
    case ('DISV')
      multi_package = gwf_disv_multi_package
    case ('DRN')
      multi_package = gwf_drn_multi_package
    case ('EVT')
      multi_package = gwf_evt_multi_package
    case ('EVTA')
      multi_package = gwf_evta_multi_package
    case ('GHB')
      multi_package = gwf_ghb_multi_package
    case ('IC')
      multi_package = gwf_ic_multi_package
    case ('NPF')
      multi_package = gwf_npf_multi_package
    case ('RCH')
      multi_package = gwf_rch_multi_package
    case ('RCHA')
      multi_package = gwf_rcha_multi_package
    case ('RIV')
      multi_package = gwf_riv_multi_package
    case ('WEL')
      multi_package = gwf_wel_multi_package
    case ('NAM')
      multi_package = gwf_nam_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWF"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwf_idm_multi_package

  function gwf_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('CHD')
      integrated = .true.
    case ('DIS')
      integrated = .true.
    case ('DISU')
      integrated = .true.
    case ('DISV')
      integrated = .true.
    case ('DRN')
      integrated = .true.
    case ('EVT')
      integrated = .true.
    case ('EVTA')
      integrated = .true.
    case ('GHB')
      integrated = .true.
    case ('IC')
      integrated = .true.
    case ('NPF')
      integrated = .true.
    case ('RCH')
      integrated = .true.
    case ('RCHA')
      integrated = .true.
    case ('RIV')
      integrated = .true.
    case ('WEL')
      integrated = .true.
    case ('NAM')
      integrated = .true.
    case default
    end select
    return
  end function gwf_idm_integrated

end module IdmGwfDfnSelectorModule
