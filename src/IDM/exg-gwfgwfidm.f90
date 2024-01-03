! ** Do Not Modify! MODFLOW 6 system generated file. **
module ExgGwfgwfInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public exg_gwfgwf_param_definitions
  public exg_gwfgwf_aggregate_definitions
  public exg_gwfgwf_block_definitions
  public ExgGwfgwfParamFoundType
  public exg_gwfgwf_multi_package

  type ExgGwfgwfParamFoundType
    logical :: auxiliary = .false.
    logical :: boundnames = .false.
    logical :: iprpak = .false.
    logical :: iprflow = .false.
    logical :: ipakcb = .false.
    logical :: cell_averaging = .false.
    logical :: cvoptions = .false.
    logical :: variablecv = .false.
    logical :: dewatered = .false.
    logical :: newton = .false.
    logical :: xt3d = .false.
    logical :: gnc_filerecord = .false.
    logical :: filein = .false.
    logical :: gnc6 = .false.
    logical :: gnc6_filename = .false.
    logical :: mvr_filerecord = .false.
    logical :: mvr6 = .false.
    logical :: mvr6_filename = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: dev_ifmod_on = .false.
    logical :: nexg = .false.
    logical :: cellidm1 = .false.
    logical :: cellidm2 = .false.
    logical :: ihc = .false.
    logical :: cl1 = .false.
    logical :: cl2 = .false.
    logical :: hwva = .false.
    logical :: aux = .false.
    logical :: boundname = .false.
  end type ExgGwfgwfParamFoundType

  logical :: exg_gwfgwf_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_auxiliary = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_boundnames = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_iprpak = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_iprflow = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_ipakcb = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_cell_averaging = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'CELL_AVERAGING', & ! tag name
    'CELL_AVERAGING', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_cvoptions = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'CVOPTIONS', & ! tag name
    'CVOPTIONS', & ! fortran variable
    'RECORD VARIABLECV DEWATERED', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_variablecv = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'VARIABLECV', & ! tag name
    'VARIABLECV', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_dewatered = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'DEWATERED', & ! tag name
    'DEWATERED', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_newton = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'NEWTON', & ! tag name
    'NEWTON', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_xt3d = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'XT3D', & ! tag name
    'XT3D', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_gnc_filerecord = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'GNC_FILERECORD', & ! tag name
    'GNC_FILERECORD', & ! fortran variable
    'RECORD GNC6 FILEIN GNC6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_filein = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_gnc6 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'GNC6', & ! tag name
    'GNC6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_gnc6_filename = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'GNC6_FILENAME', & ! tag name
    'GNC6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_mvr_filerecord = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'MVR_FILERECORD', & ! tag name
    'MVR_FILERECORD', & ! fortran variable
    'RECORD MVR6 FILEIN MVR6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_mvr6 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'MVR6', & ! tag name
    'MVR6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_mvr6_filename = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'MVR6_FILENAME', & ! tag name
    'MVR6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_obs_filerecord = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_obs6 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_obs6_filename = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
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
    exggwfgwf_dev_ifmod_on = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_INTERFACEMODEL_ON', & ! tag name
    'DEV_IFMOD_ON', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_nexg = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'DIMENSIONS', & ! block
    'NEXG', & ! tag name
    'NEXG', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_cellidm1 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'CELLIDM1', & ! tag name
    'CELLIDM1', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_cellidm2 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'CELLIDM2', & ! tag name
    'CELLIDM2', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_ihc = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'IHC', & ! tag name
    'IHC', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_cl1 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'CL1', & ! tag name
    'CL1', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_cl2 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'CL2', & ! tag name
    'CL2', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_hwva = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'HWVA', & ! tag name
    'HWVA', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_aux = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'AUX', & ! tag name
    'AUX', & ! fortran variable
    'DOUBLE1D', & ! type
    'NAUX', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_boundname = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
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
    exg_gwfgwf_param_definitions(*) = &
    [ &
    exggwfgwf_auxiliary, &
    exggwfgwf_boundnames, &
    exggwfgwf_iprpak, &
    exggwfgwf_iprflow, &
    exggwfgwf_ipakcb, &
    exggwfgwf_cell_averaging, &
    exggwfgwf_cvoptions, &
    exggwfgwf_variablecv, &
    exggwfgwf_dewatered, &
    exggwfgwf_newton, &
    exggwfgwf_xt3d, &
    exggwfgwf_gnc_filerecord, &
    exggwfgwf_filein, &
    exggwfgwf_gnc6, &
    exggwfgwf_gnc6_filename, &
    exggwfgwf_mvr_filerecord, &
    exggwfgwf_mvr6, &
    exggwfgwf_mvr6_filename, &
    exggwfgwf_obs_filerecord, &
    exggwfgwf_obs6, &
    exggwfgwf_obs6_filename, &
    exggwfgwf_dev_ifmod_on, &
    exggwfgwf_nexg, &
    exggwfgwf_cellidm1, &
    exggwfgwf_cellidm2, &
    exggwfgwf_ihc, &
    exggwfgwf_cl1, &
    exggwfgwf_cl2, &
    exggwfgwf_hwva, &
    exggwfgwf_aux, &
    exggwfgwf_boundname &
    ]

  type(InputParamDefinitionType), parameter :: &
    exggwfgwf_exchangedata = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFGWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'EXCHANGEDATA', & ! tag name
    'EXCHANGEDATA', & ! fortran variable
    'RECARRAY CELLIDM1 CELLIDM2 IHC CL1 CL2 HWVA AUX BOUNDNAME', & ! type
    'NEXG', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exg_gwfgwf_aggregate_definitions(*) = &
    [ &
    exggwfgwf_exchangedata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    exg_gwfgwf_block_definitions(*) = &
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
    'EXCHANGEDATA', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module ExgGwfgwfInputModule
