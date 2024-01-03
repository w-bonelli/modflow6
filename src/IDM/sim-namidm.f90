! ** Do Not Modify! MODFLOW 6 system generated file. **
module SimNamInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public sim_nam_param_definitions
  public sim_nam_aggregate_definitions
  public sim_nam_block_definitions
  public SimNamParamFoundType
  public sim_nam_multi_package

  type SimNamParamFoundType
    logical :: continue = .false.
    logical :: nocheck = .false.
    logical :: prmem = .false.
    logical :: maxerrors = .false.
    logical :: print_input = .false.
    logical :: tdis6 = .false.
    logical :: mtype = .false.
    logical :: mfname = .false.
    logical :: mname = .false.
    logical :: exgtype = .false.
    logical :: exgfile = .false.
    logical :: exgmnamea = .false.
    logical :: exgmnameb = .false.
    logical :: mxiter = .false.
    logical :: slntype = .false.
    logical :: slnfname = .false.
    logical :: slnmnames = .false.
  end type SimNamParamFoundType

  logical :: sim_nam_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    simnam_continue = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'CONTINUE', & ! tag name
    'CONTINUE', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_nocheck = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'NOCHECK', & ! tag name
    'NOCHECK', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_prmem = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'MEMORY_PRINT_OPTION', & ! tag name
    'PRMEM', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_maxerrors = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'MAXERRORS', & ! tag name
    'MAXERRORS', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_print_input = InputParamDefinitionType &
    ( &
    'SIM', & ! component
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
    simnam_tdis6 = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'TIMING', & ! block
    'TDIS6', & ! tag name
    'TDIS6', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_mtype = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'MODELS', & ! block
    'MTYPE', & ! tag name
    'MTYPE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_mfname = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'MODELS', & ! block
    'MFNAME', & ! tag name
    'MFNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_mname = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'MODELS', & ! block
    'MNAME', & ! tag name
    'MNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_exgtype = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'EXCHANGES', & ! block
    'EXGTYPE', & ! tag name
    'EXGTYPE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_exgfile = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'EXCHANGES', & ! block
    'EXGFILE', & ! tag name
    'EXGFILE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_exgmnamea = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'EXCHANGES', & ! block
    'EXGMNAMEA', & ! tag name
    'EXGMNAMEA', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_exgmnameb = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'EXCHANGES', & ! block
    'EXGMNAMEB', & ! tag name
    'EXGMNAMEB', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_mxiter = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'SOLUTIONGROUP', & ! block
    'MXITER', & ! tag name
    'MXITER', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_slntype = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'SOLUTIONGROUP', & ! block
    'SLNTYPE', & ! tag name
    'SLNTYPE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_slnfname = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'SOLUTIONGROUP', & ! block
    'SLNFNAME', & ! tag name
    'SLNFNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_slnmnames = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'SOLUTIONGROUP', & ! block
    'SLNMNAMES', & ! tag name
    'SLNMNAMES', & ! fortran variable
    'STRING', & ! type
    ':', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    sim_nam_param_definitions(*) = &
    [ &
    simnam_continue, &
    simnam_nocheck, &
    simnam_prmem, &
    simnam_maxerrors, &
    simnam_print_input, &
    simnam_tdis6, &
    simnam_mtype, &
    simnam_mfname, &
    simnam_mname, &
    simnam_exgtype, &
    simnam_exgfile, &
    simnam_exgmnamea, &
    simnam_exgmnameb, &
    simnam_mxiter, &
    simnam_slntype, &
    simnam_slnfname, &
    simnam_slnmnames &
    ]

  type(InputParamDefinitionType), parameter :: &
    simnam_models = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'MODELS', & ! block
    'MODELS', & ! tag name
    'MODELS', & ! fortran variable
    'RECARRAY MTYPE MFNAME MNAME', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_exchanges = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'EXCHANGES', & ! block
    'EXCHANGES', & ! tag name
    'EXCHANGES', & ! fortran variable
    'RECARRAY EXGTYPE EXGFILE EXGMNAMEA EXGMNAMEB', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simnam_solutiongroup = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'NAM', & ! subcomponent
    'SOLUTIONGROUP', & ! block
    'SOLUTIONGROUP', & ! tag name
    'SOLUTIONGROUP', & ! fortran variable
    'RECARRAY SLNTYPE SLNFNAME SLNMNAMES', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    sim_nam_aggregate_definitions(*) = &
    [ &
    simnam_models, &
    simnam_exchanges, &
    simnam_solutiongroup &
    ]

  type(InputBlockDefinitionType), parameter :: &
    sim_nam_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'TIMING', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'MODELS', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'EXCHANGES', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'SOLUTIONGROUP', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .true. & ! block_variable
    ) &
    ]

end module SimNamInputModule
