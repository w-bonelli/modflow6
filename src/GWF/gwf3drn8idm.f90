! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfDrnInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_drn_param_definitions
  public gwf_drn_aggregate_definitions
  public gwf_drn_block_definitions
  public GwfDrnParamFoundType
  public gwf_drn_multi_package

  type GwfDrnParamFoundType
    logical :: auxiliary = .false.
    logical :: auxmultname = .false.
    logical :: auxdepthname = .false.
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
    logical :: mover = .false.
    logical :: icubicsfac = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: elev = .false.
    logical :: cond = .false.
    logical :: auxvar = .false.
    logical :: boundname = .false.
  end type GwfDrnParamFoundType

  logical :: gwf_drn_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    gwfdrn_auxiliary = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_auxmultname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_auxdepthname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
    'OPTIONS', & ! block
    'AUXDEPTHNAME', & ! tag name
    'AUXDEPTHNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdrn_boundnames = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_iprpak = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_iprflow = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_ipakcb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_ts_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_ts6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_filein = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_ts6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_obs_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_obs6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_obs6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_mover = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
    'OPTIONS', & ! block
    'MOVER', & ! tag name
    'MOVER', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdrn_icubicsfac = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_CUBIC_SCALING', & ! tag name
    'ICUBICSFAC', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdrn_maxbound = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_cellid = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_elev = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
    'PERIOD', & ! block
    'ELEV', & ! tag name
    'ELEV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdrn_cond = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
    'PERIOD', & ! block
    'COND', & ! tag name
    'COND', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdrn_auxvar = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwfdrn_boundname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
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
    gwf_drn_param_definitions(*) = &
    [ &
    gwfdrn_auxiliary, &
    gwfdrn_auxmultname, &
    gwfdrn_auxdepthname, &
    gwfdrn_boundnames, &
    gwfdrn_iprpak, &
    gwfdrn_iprflow, &
    gwfdrn_ipakcb, &
    gwfdrn_ts_filerecord, &
    gwfdrn_ts6, &
    gwfdrn_filein, &
    gwfdrn_ts6_filename, &
    gwfdrn_obs_filerecord, &
    gwfdrn_obs6, &
    gwfdrn_obs6_filename, &
    gwfdrn_mover, &
    gwfdrn_icubicsfac, &
    gwfdrn_maxbound, &
    gwfdrn_cellid, &
    gwfdrn_elev, &
    gwfdrn_cond, &
    gwfdrn_auxvar, &
    gwfdrn_boundname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfdrn_spd = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DRN', & ! subcomponent
    'PERIOD', & ! block
    'STRESS_PERIOD_DATA', & ! tag name
    'SPD', & ! fortran variable
    'RECARRAY CELLID ELEV COND AUX BOUNDNAME', & ! type
    'MAXBOUND', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_drn_aggregate_definitions(*) = &
    [ &
    gwfdrn_spd &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_drn_block_definitions(*) = &
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

end module GwfDrnInputModule
