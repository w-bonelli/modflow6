! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfRchInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_rch_param_definitions
  public gwf_rch_aggregate_definitions
  public gwf_rch_block_definitions
  public GwfRchParamFoundType
  public gwf_rch_multi_package

  type GwfRchParamFoundType
    logical :: fixed_cell = .false.
    logical :: auxiliary = .false.
    logical :: auxmultname = .false.
    logical :: boundnames = .false.
    logical :: iprpak = .false.
    logical :: iprflow = .false.
    logical :: ipakcb = .false.
    logical :: ts_filerecord = .false.
    logical :: ts6 = .false.
    logical :: filein = .false.
    logical :: ts6_filename = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: recharge = .false.
    logical :: auxvar = .false.
    logical :: boundname = .false.
  end type GwfRchParamFoundType

  logical :: gwf_rch_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    gwfrch_fixed_cell = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'FIXED_CELL', & ! tag name
    'FIXED_CELL', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_auxiliary = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'AUXILIARY', & ! tag name
    'AUXILIARY', & ! fortran variable
    'STRING', & ! type
    'NAUX', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_auxmultname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'AUXMULTNAME', & ! tag name
    'AUXMULTNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_boundnames = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'BOUNDNAMES', & ! tag name
    'BOUNDNAMES', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_iprpak = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'IPRPAK', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_iprflow = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'IPRFLOW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_ipakcb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'IPAKCB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_ts_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'TS_FILERECORD', & ! tag name
    'TS_FILERECORD', & ! fortran variable
    'RECORD TS6 FILEIN TS6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_ts6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'TS6', & ! tag name
    'TS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_filein = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'FILEIN', & ! tag name
    'FILEIN', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_ts6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'TS6_FILENAME', & ! tag name
    'TS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_obs_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'OBS_FILERECORD', & ! tag name
    'OBS_FILERECORD', & ! fortran variable
    'RECORD OBS6 FILEIN OBS6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_obs6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6', & ! tag name
    'OBS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_obs6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6_FILENAME', & ! tag name
    'OBS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_maxbound = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'DIMENSIONS', & ! block
    'MAXBOUND', & ! tag name
    'MAXBOUND', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_cellid = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'PERIOD', & ! block
    'CELLID', & ! tag name
    'CELLID', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_recharge = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'PERIOD', & ! block
    'RECHARGE', & ! tag name
    'RECHARGE', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_auxvar = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'PERIOD', & ! block
    'AUX', & ! tag name
    'AUXVAR', & ! fortran variable
    'DOUBLE1D', & ! type
    'NAUX', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrch_boundname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'PERIOD', & ! block
    'BOUNDNAME', & ! tag name
    'BOUNDNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_rch_param_definitions(*) = &
    [ &
    gwfrch_fixed_cell, &
    gwfrch_auxiliary, &
    gwfrch_auxmultname, &
    gwfrch_boundnames, &
    gwfrch_iprpak, &
    gwfrch_iprflow, &
    gwfrch_ipakcb, &
    gwfrch_ts_filerecord, &
    gwfrch_ts6, &
    gwfrch_filein, &
    gwfrch_ts6_filename, &
    gwfrch_obs_filerecord, &
    gwfrch_obs6, &
    gwfrch_obs6_filename, &
    gwfrch_maxbound, &
    gwfrch_cellid, &
    gwfrch_recharge, &
    gwfrch_auxvar, &
    gwfrch_boundname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfrch_spd = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCH', & ! subcomponent
    'PERIOD', & ! block
    'STRESS_PERIOD_DATA', & ! tag name
    'SPD', & ! fortran variable
    'RECARRAY CELLID RECHARGE AUX BOUNDNAME', & ! type
    'MAXBOUND', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_rch_aggregate_definitions(*) = &
    [ &
    gwfrch_spd &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_rch_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'PERIOD', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .true. & ! block_variable
    ) &
    ]

end module GwfRchInputModule
