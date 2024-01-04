! ** Do Not Modify! MODFLOW 6 system generated file. **
module ExgGwfprtInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public exg_gwfprt_param_definitions
  public exg_gwfprt_aggregate_definitions
  public exg_gwfprt_block_definitions
  public ExgGwfprtParamFoundType
  public exg_gwfprt_multi_package

  type ExgGwfprtParamFoundType
  end type ExgGwfprtParamFoundType

  logical :: exg_gwfprt_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    exg_gwfprt_param_definitions(*) = &
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
    exg_gwfprt_aggregate_definitions(*) = &
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
    exg_gwfprt_block_definitions(*) = &
    [ &
    InputBlockDefinitionType &
    ( &
    '', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_varaible
    ) &
    ]

end module ExgGwfprtInputModule
