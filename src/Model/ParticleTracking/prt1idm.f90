! ** Do Not Modify! MODFLOW 6 system generated file. **
module PrtNamInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public prt_nam_param_definitions
  public prt_nam_aggregate_definitions
  public prt_nam_block_definitions
  public PrtNamParamFoundType
  public prt_nam_multi_package

  type PrtNamParamFoundType
    logical :: list = .false.
    logical :: print_input = .false.
    logical :: print_flows = .false.
    logical :: save_flows = .false.
    logical :: ftype = .false.
    logical :: fname = .false.
    logical :: pname = .false.
  end type PrtNamParamFoundType

  logical :: prt_nam_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    prtnam_list = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'LIST', & ! tag name
    'LIST', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtnam_print_input = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'PRINT_INPUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtnam_print_flows = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'PRINT_FLOWS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtnam_save_flows = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'SAVE_FLOWS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtnam_ftype = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'NAM', & ! subcomponent
    'PACKAGES', & ! block
    'FTYPE', & ! tag name
    'FTYPE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtnam_fname = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'NAM', & ! subcomponent
    'PACKAGES', & ! block
    'FNAME', & ! tag name
    'FNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtnam_pname = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'NAM', & ! subcomponent
    'PACKAGES', & ! block
    'PNAME', & ! tag name
    'PNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prt_nam_param_definitions(*) = &
    [ &
    prtnam_list, &
    prtnam_print_input, &
    prtnam_print_flows, &
    prtnam_save_flows, &
    prtnam_ftype, &
    prtnam_fname, &
    prtnam_pname &
    ]

  type(InputParamDefinitionType), parameter :: &
    prtnam_packages = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'NAM', & ! subcomponent
    'PACKAGES', & ! block
    'PACKAGES', & ! tag name
    'PACKAGES', & ! fortran variable
    'RECARRAY FTYPE FNAME PNAME', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prt_nam_aggregate_definitions(*) = &
    [ &
    prtnam_packages &
    ]

  type(InputBlockDefinitionType), parameter :: &
    prt_nam_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'PACKAGES', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module PrtNamInputModule
