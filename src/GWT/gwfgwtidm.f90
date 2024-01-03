! ** Do Not Modify! MODFLOW 6 system generated file. **
module ExgGwfgwtInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public exg_gwfgwt_param_definitions
  public exg_gwfgwt_aggregate_definitions
  public exg_gwfgwt_block_definitions
  public ExgGwfgwtParamFoundType
  public exg_gwfgwt_multi_package

  type ExgGwfgwtParamFoundType
  end type ExgGwfgwtParamFoundType

  logical :: exg_gwfgwt_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    exg_gwfgwt_param_definitions(*) = &
    [ &
    InputParamDefinitionType &
    ( &
    '', & ! component
    '', & ! subcomponent
    '', & ! block
    '', & ! tag name
    '', & ! fortran variable
    '', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputParamDefinitionType), parameter :: &
    exg_gwfgwt_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType &
    ( &
    '', & ! component
    '', & ! subcomponent
    '', & ! block
    '', & ! tag name
    '', & ! fortran variable
    '', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    exg_gwfgwt_block_definitions(*) = &
    [ &
    InputBlockDefinitionType &
    ( &
    '', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_varaible
    ) &
    ]

end module ExgGwfgwtInputModule
