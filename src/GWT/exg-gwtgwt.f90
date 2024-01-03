!> @brief This module contains the GwtGwtExchangeModule Module
!!
!! This module contains the code for connecting two GWT Models.
!! The methods are based on the simple two point flux approximation
!! with the option to use ghost nodes to improve accuracy.  This
!! exchange is used by GwtGwtConnection with the more sophisticated
!! interface model coupling approach when XT3D is needed.
!!
!<
module GwtGwtExchangeModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg, model_loc_idx
  use SimModule, only: store_error, store_error_filename, &
                       count_errors, ustop
  use BaseModelModule, only: BaseModelType, GetBaseModelFromList
  use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
  use ConstantsModule, only: LENBOUNDNAME, NAMEDBOUNDFLAG, LINELENGTH, &
                             TABCENTER, TABLEFT, LENAUXNAME, DNODATA, &
                             LENMODELNAME
  use ListModule, only: ListType
  use ListsModule, only: basemodellist
  use VirtualModelModule, only: get_virtual_model
  use DisConnExchangeModule, only: DisConnExchangeType
  use GwtModule, only: GwtModelType
  use TspMvtModule, only: TspMvtType
  use VirtualModelModule, only: VirtualModelType
  use ObserveModule, only: ObserveType
  use ObsModule, only: ObsType
  use TableModule, only: TableType, table_cr
  use MatrixBaseModule

  implicit none

  private
  public :: GwtExchangeType
  public :: gwtgwt_cr
  public :: GetGwtExchangeFromList
  public :: CastAsGwtExchange

  !> @brief Derived type for GwtExchangeType
  !!
  !! This derived type contains information and methods for
  !! connecting two GWT models.
  !<
  type, extends(DisConnExchangeType) :: GwtExchangeType
    !
    ! -- names of the GWF models that are connected by this exchange
    character(len=LENMODELNAME) :: gwfmodelname1 = '' !< name of gwfmodel that corresponds to gwtmodel1
    character(len=LENMODELNAME) :: gwfmodelname2 = '' !< name of gwfmodel that corresponds to gwtmodel2
    real(DP), dimension(:), pointer, contiguous :: gwfsimvals => null() !< simulated gwf flow rate for each exchange
    !
    ! -- pointers to gwt models
    class(GwtModelType), pointer :: gwtmodel1 => null() !< pointer to GWT Model 1
    class(GwtModelType), pointer :: gwtmodel2 => null() !< pointer to GWT Model 2
    !
    ! -- GWT specific option block:
    integer(I4B), pointer :: inewton => null() !< unneeded newton flag allows for mvt to be used here
    integer(I4B), pointer :: iAdvScheme !< the advection scheme at the interface:
                                                                                 !! 0 = upstream, 1 = central, 2 = TVD
    !
    ! -- Mover transport package
    integer(I4B), pointer :: inmvt => null() !< unit number for mover transport (0 if off)
    type(TspMvtType), pointer :: mvt => null() !< water mover object
    !
    ! -- Observation package
    integer(I4B), pointer :: inobs => null() !< unit number for GWT-GWT observations
    type(ObsType), pointer :: obs => null() !< observation object
    !
    ! -- internal data
    real(DP), dimension(:), pointer, contiguous :: cond => null() !< conductance
    real(DP), dimension(:), pointer, contiguous :: simvals => null() !< simulated flow rate for each exchange
    !
    ! -- table objects
    type(TableType), pointer :: outputtab1 => null()
    type(TableType), pointer :: outputtab2 => null()

  contains

    procedure :: exg_df => gwt_gwt_df
    procedure :: exg_ar => gwt_gwt_ar
    procedure :: exg_rp => gwt_gwt_rp
    procedure :: exg_ad => gwt_gwt_ad
    procedure :: exg_fc => gwt_gwt_fc
    procedure :: exg_bd => gwt_gwt_bd
    procedure :: exg_ot => gwt_gwt_ot
    procedure :: exg_da => gwt_gwt_da
    procedure :: exg_fp => gwt_gwt_fp
    procedure :: connects_model => gwt_gwt_connects_model
    procedure :: use_interface_model
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: source_options
    procedure :: read_mvt
    procedure :: gwt_gwt_bdsav
    procedure, private :: gwt_gwt_bdsav_model
    procedure, private :: gwt_gwt_df_obs
    procedure, private :: gwt_gwt_rp_obs
    procedure, public :: gwt_gwt_save_simvals
    procedure, private :: validate_exchange
  end type GwtExchangeType

contains

  !> @ brief Create GWT GWT exchange
  !!
  !! Create a new GWT to GWT exchange object.
  !<
  subroutine gwtgwt_cr(filename, name, id, m1_id, m2_id, input_mempath)
    ! -- modules
    use BaseModelModule, only: BaseModelType
    use ListsModule, only: baseexchangelist
    use ObsModule, only: obs_cr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    character(len=*), intent(in) :: filename !< filename to read
    character(len=*) :: name !< exchange name
    integer(I4B), intent(in) :: id !< exchange id
    integer(I4B), intent(in) :: m1_id !< model 1 id
    integer(I4B), intent(in) :: m2_id !< model 2 id
    character(len=*), intent(in) :: input_mempath
    ! -- local
    type(GwtExchangeType), pointer :: exchange
    class(BaseModelType), pointer :: mb
    class(BaseExchangeType), pointer :: baseexchange
    integer(I4B) :: m1_index, m2_index
    !
    ! -- Create a new exchange and add it to the baseexchangelist container
    allocate (exchange)
    baseexchange => exchange
    call AddBaseExchangeToList(baseexchangelist, baseexchange)
    !
    ! -- Assign id and name
    exchange%id = id
    exchange%name = name
    exchange%memoryPath = create_mem_path(exchange%name)
    exchange%input_mempath = input_mempath
    !
    ! -- allocate scalars and set defaults
    call exchange%allocate_scalars()
    exchange%filename = filename
    exchange%typename = 'GWT-GWT'
    exchange%iAdvScheme = 0
    exchange%ixt3d = 1
    !
    ! -- set gwtmodel1
    m1_index = model_loc_idx(m1_id)
    mb => GetBaseModelFromList(basemodellist, m1_index)
    if (m1_index > 0) then
      select type (mb)
      type is (GwtModelType)
        exchange%model1 => mb
        exchange%gwtmodel1 => mb
      end select
    end if
    exchange%v_model1 => get_virtual_model(m1_id)
    !
    ! -- set gwtmodel2
    m2_index = model_loc_idx(m2_id)
    if (m2_index > 0) then
      mb => GetBaseModelFromList(basemodellist, m2_index)
      select type (mb)
      type is (GwtModelType)
        exchange%model2 => mb
        exchange%gwtmodel2 => mb
      end select
    end if
    exchange%v_model2 => get_virtual_model(m2_id)
    !
    ! -- Verify that gwt model1 is of the correct type
    if (.not. associated(exchange%gwtmodel1) .and. m1_index > 0) then
      write (errmsg, '(3a)') 'Problem with GWT-GWT exchange ', &
        trim(exchange%name), &
        '.  First specified GWT Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Verify that gwt model2 is of the correct type
    if (.not. associated(exchange%gwtmodel2) .and. m2_index > 0) then
      write (errmsg, '(3a)') 'Problem with GWT-GWT exchange ', &
        trim(exchange%name), &
        '.  Second specified GWT Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Create the obs package
    call obs_cr(exchange%obs, exchange%inobs)
    !
    ! -- Return
    return
  end subroutine gwtgwt_cr

  !> @ brief Define GWT GWT exchange
  !!
  !! Define GWT to GWT exchange object.
  !<
  subroutine gwt_gwt_df(this)
    ! -- modules
    use SimVariablesModule, only: iout
    use InputOutputModule, only: getunit, openfile
    use GhostNodeModule, only: gnc_cr
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    !
    ! -- log the exchange
    write (iout, '(/a,a)') ' Creating exchange: ', this%name
    !
    ! -- Ensure models are in same solution
    if (associated(this%gwtmodel1) .and. associated(this%gwtmodel2)) then
      if (this%gwtmodel1%idsoln /= this%gwtmodel2%idsoln) then
        call store_error('Two models are connected in a GWT '// &
                         'exchange but they are in different solutions. '// &
                         'GWT models must be in same solution: '// &
                         trim(this%gwtmodel1%name)//' '// &
                         trim(this%gwtmodel2%name))
        call store_error_filename(this%filename)
      end if
    end if
    !
    ! -- source options
    call this%source_options(iout)
    !
    ! -- source dimensions
    call this%source_dimensions(iout)
    !
    ! -- allocate arrays
    call this%allocate_arrays()
    !
    ! -- source exchange data
    call this%source_data(iout)
    !
    ! -- Read mover information
    if (this%inmvt > 0) then
      call this%read_mvt(iout)
      call this%mvt%mvt_df(this%gwtmodel1%dis)
    end if
    !
    ! -- Store obs
    call this%gwt_gwt_df_obs()
    if (associated(this%gwtmodel1)) then
      call this%obs%obs_df(iout, this%name, 'GWT-GWT', this%gwtmodel1%dis)
    end if
    !
    ! -- validate
    call this%validate_exchange()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_df

  !> @brief validate exchange data after reading
  !<
  subroutine validate_exchange(this)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    ! Ensure gwfmodel names were entered
    if (this%gwfmodelname1 == '') then
      write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
                            ' requires that GWFMODELNAME1 be entered in the &
                            &OPTIONS block.'
      call store_error(errmsg)
    end if
    if (this%gwfmodelname2 == '') then
      write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
                            ' requires that GWFMODELNAME2 be entered in the &
                            &OPTIONS block.'
      call store_error(errmsg)
    end if
    !
    ! Periodic boundary condition in exchange don't allow XT3D (=interface model)
    if (associated(this%model1, this%model2)) then
      if (this%ixt3d > 0) then
        write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
          ' is a periodic boundary condition which cannot'// &
          ' be configured with XT3D'
        call store_error(errmsg)
      end if
    end if
    !
    ! Check to see if dispersion is on in either model1 or model2.
    ! If so, then ANGLDEGX must be provided as an auxiliary variable for this
    ! GWT-GWT exchange (this%ianglex > 0).
    if (associated(this%gwtmodel1) .and. associated(this%gwtmodel2)) then
      if (this%gwtmodel1%indsp /= 0 .or. this%gwtmodel2%indsp /= 0) then
        if (this%ianglex == 0) then
          write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
            ' requires that ANGLDEGX be specified as an'// &
            ' auxiliary variable because dispersion was '// &
            'specified in one or both transport models.'
          call store_error(errmsg)
        end if
      end if
    end if
    !
    if (this%ixt3d > 0 .and. this%ianglex == 0) then
      write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
        ' requires that ANGLDEGX be specified as an'// &
        ' auxiliary variable because XT3D is enabled'
      call store_error(errmsg)
    end if
    !
    if (count_errors() > 0) then
      call ustop()
    end if
    !
    ! -- Return
    return
  end subroutine validate_exchange

  !> @ brief Allocate and read
  !!
  !! Allocated and read and calculate saturated conductance
  !<
  subroutine gwt_gwt_ar(this)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    ! -- If mover is active, then call ar routine
    if (this%inmvt > 0) call this%mvt%mvt_ar()
    !
    ! -- Observation AR
    call this%obs%obs_ar()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_ar

  !> @ brief Read and prepare
  !!
  !! Read new data for mover and obs
  !<
  subroutine gwt_gwt_rp(this)
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Read and prepare for mover
    if (this%inmvt > 0) call this%mvt%mvt_rp()
    !
    ! -- Read and prepare for observations
    call this%gwt_gwt_rp_obs()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_rp

  !> @ brief Advance
  !!
  !! Advance mover and obs
  !<
  subroutine gwt_gwt_ad(this)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    ! -- Advance mover
    !if(this%inmvt > 0) call this%mvt%mvt_ad()
    !
    ! -- Push simulated values to preceding time step
    call this%obs%obs_ad()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_ad

  !> @ brief Fill coefficients
  !!
  !! Calculate conductance and fill coefficient matrix
  !<
  subroutine gwt_gwt_fc(this, kiter, matrix_sln, rhs_sln, inwtflag)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    real(DP), dimension(:), intent(inout) :: rhs_sln
    integer(I4B), optional, intent(in) :: inwtflag
    !
    ! -- Call mvt fc routine
    if (this%inmvt > 0) call this%mvt%mvt_fc(this%gwtmodel1%x, this%gwtmodel2%x)
    !
    ! -- Return
    return
  end subroutine gwt_gwt_fc

  !> @ brief Budget
  !!
  !! Accumulate budget terms
  !<
  subroutine gwt_gwt_bd(this, icnvg, isuppress_output, isolnid)
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    use BudgetModule, only: rate_accumulator
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
    character(len=LENBUDTXT), dimension(1) :: budtxt
    real(DP), dimension(2, 1) :: budterm
    real(DP) :: ratin, ratout
    !
    ! -- initialize
    budtxt(1) = '    FLOW-JA-FACE'
    !
    ! -- Calculate ratin/ratout and pass to model budgets
    call rate_accumulator(this%simvals, ratin, ratout)
    !
    ! -- Add the budget terms to model 1
    if (associated(this%gwtmodel1)) then
      budterm(1, 1) = ratin
      budterm(2, 1) = ratout
      call this%gwtmodel1%model_bdentry(budterm, budtxt, this%name)
    end if
    !
    ! -- Add the budget terms to model 2
    if (associated(this%gwtmodel2)) then
      budterm(1, 1) = ratout
      budterm(2, 1) = ratin
      call this%gwtmodel2%model_bdentry(budterm, budtxt, this%name)
    end if
    !
    ! -- Call mvt bd routine
    if (this%inmvt > 0) call this%mvt%mvt_bd(this%gwtmodel1%x, this%gwtmodel2%x)
    !
    ! -- Return
    return
  end subroutine gwt_gwt_bd

  !> @ brief Budget save
  !!
  !! Output individual flows to listing file and binary budget files
  !<
  subroutine gwt_gwt_bdsav(this)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: icbcfl, ibudfl
    !
    ! -- budget for model1
    if (associated(this%gwtmodel1)) then
      call this%gwt_gwt_bdsav_model(this%gwtmodel1)
    end if
    !
    ! -- budget for model2
    if (associated(this%gwtmodel2)) then
      call this%gwt_gwt_bdsav_model(this%gwtmodel2)
    end if
    !
    ! -- Set icbcfl, ibudfl to zero so that flows will be printed and
    !    saved, if the options were set in the MVT package
    icbcfl = 1
    ibudfl = 1
    !
    ! -- Call mvt bd routine
    !cdl todo: if(this%inmvt > 0) call this%mvt%mvt_bdsav(icbcfl, ibudfl, isuppress_output)
    !
    ! -- Calculate and write simulated values for observations
    if (this%inobs /= 0) then
      call this%gwt_gwt_save_simvals()
    end if
    !
    ! -- Return
    return
  end subroutine gwt_gwt_bdsav

  !> @ brief Budget save
  !!
  !! Output individual flows to listing file and binary budget files
  !<
  subroutine gwt_gwt_bdsav_model(this, model)
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    class(GwtModelType), pointer :: model
    ! -- local
    character(len=LENBOUNDNAME) :: bname
    character(len=LENPACKAGENAME + 4) :: packname
    character(len=LENBUDTXT), dimension(1) :: budtxt
    type(TableType), pointer :: output_tab
    class(VirtualModelType), pointer :: nbr_model
    character(len=20) :: nodestr
    integer(I4B) :: ntabrows
    integer(I4B) :: nodeu
    integer(I4B) :: i, n1, n2, n1u, n2u
    integer(I4B) :: ibinun
    real(DP) :: ratin, ratout, rrate
    logical(LGP) :: is_for_model1
    integer(I4B) :: isuppress_output
    !
    ! -- initialize local variables
    isuppress_output = 0
    budtxt(1) = '    FLOW-JA-FACE'
    packname = 'EXG '//this%name
    packname = adjustr(packname)
    if (associated(model, this%gwtmodel1)) then
      output_tab => this%outputtab1
      nbr_model => this%v_model2
      is_for_model1 = .true.
    else
      output_tab => this%outputtab2
      nbr_model => this%v_model1
      is_for_model1 = .false.
    end if
    !
    ! -- update output tables
    if (this%iprflow /= 0) then
      !
      ! -- update titles
      if (model%oc%oc_save('BUDGET')) then
        call output_tab%set_title(packname)
      end if
      !
      ! -- set table kstp and kper
      call output_tab%set_kstpkper(kstp, kper)
      !
      ! -- update maxbound of tables
      ntabrows = 0
      do i = 1, this%nexg
        n1 = this%nodem1(i)
        n2 = this%nodem2(i)
        !
        ! -- If both cells are active then calculate flow rate
        if (this%v_model1%ibound%get(n1) /= 0 .and. &
            this%v_model2%ibound%get(n2) /= 0) then
          ntabrows = ntabrows + 1
        end if
      end do
      if (ntabrows > 0) then
        call output_tab%set_maxbound(ntabrows)
      end if
    end if
    !
    ! -- Print and write budget terms for model 1
    !
    ! -- Set binary unit numbers for saving flows
    if (this%ipakcb /= 0) then
      ibinun = model%oc%oc_save_unit('BUDGET')
    else
      ibinun = 0
    end if
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if (.not. model%oc%oc_save('BUDGET')) ibinun = 0
    if (isuppress_output /= 0) then
      ibinun = 0
    end if
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if (ibinun /= 0) then
      call model%dis%record_srcdst_list_header(budtxt(1), &
                                               model%name, &
                                               this%name, &
                                               nbr_model%name, &
                                               this%name, &
                                               this%naux, this%auxname, &
                                               ibinun, this%nexg, &
                                               model%iout)
    end if
    !
    ! Initialize accumulators
    ratin = DZERO
    ratout = DZERO
    !
    ! -- Loop through all exchanges
    do i = 1, this%nexg
      !
      ! -- Assign boundary name
      if (this%inamedbound > 0) then
        bname = this%boundname(i)
      else
        bname = ''
      end if
      !
      ! -- Calculate the flow rate between n1 and n2
      rrate = DZERO
      n1 = this%nodem1(i)
      n2 = this%nodem2(i)
      !
      ! -- If both cells are active then calculate flow rate
      if (this%v_model1%ibound%get(n1) /= 0 .and. &
          this%v_model2%ibound%get(n2) /= 0) then
        rrate = this%simvals(i)
        !
        ! -- Print the individual rates to model list files if requested
        if (this%iprflow /= 0) then
          if (model%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            if (is_for_model1) then
              nodeu = model%dis%get_nodeuser(n1)
              call model%dis%nodeu_to_string(nodeu, nodestr)
              call output_tab%print_list_entry(i, trim(adjustl(nodestr)), &
                                               rrate, bname)
            else
              nodeu = model%dis%get_nodeuser(n2)
              call model%dis%nodeu_to_string(nodeu, nodestr)
              call output_tab%print_list_entry(i, trim(adjustl(nodestr)), &
                                               -rrate, bname)
            end if
          end if
        end if
        if (rrate < DZERO) then
          ratout = ratout - rrate
        else
          ratin = ratin + rrate
        end if
      end if
      !
      ! -- If saving cell-by-cell flows in list, write flow
      n1u = this%v_model1%dis_get_nodeuser(n1)
      n2u = this%v_model2%dis_get_nodeuser(n2)
      if (ibinun /= 0) then
        if (is_for_model1) then
          call model%dis%record_mf6_list_entry( &
            ibinun, n1u, n2u, rrate, this%naux, this%auxvar(:, i), &
            .false., .false.)
        else
          call model%dis%record_mf6_list_entry( &
            ibinun, n2u, n1u, -rrate, this%naux, this%auxvar(:, i), &
            .false., .false.)
        end if
      end if
      !
    end do
    !
    ! -- Return
    return
  end subroutine gwt_gwt_bdsav_model

  !> @ brief Output
  !!
  !! Write output
  !<
  subroutine gwt_gwt_ot(this)
    ! -- modules
    use SimVariablesModule, only: iout
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: iexg, n1, n2
    integer(I4B) :: ibudfl
    real(DP) :: flow
    character(len=LINELENGTH) :: node1str, node2str
    ! -- format
    character(len=*), parameter :: fmtheader = &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /, &
       &2a16, 5a16, /, 112('-'))"
    character(len=*), parameter :: fmtheader2 = &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /, &
       &2a16, 4a16, /, 96('-'))"
    character(len=*), parameter :: fmtdata = &
                                   "(2a16, 5(1pg16.6))"
    !
    ! -- Call bdsave
    call this%gwt_gwt_bdsav()
    !
    ! -- Write a table of exchanges
    if (this%iprflow /= 0) then
      write (iout, fmtheader2) trim(adjustl(this%name)), this%id, 'NODEM1', &
        'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
      do iexg = 1, this%nexg
        n1 = this%nodem1(iexg)
        n2 = this%nodem2(iexg)
        flow = this%simvals(iexg)
        call this%v_model1%dis_noder_to_string(n1, node1str)
        call this%v_model2%dis_noder_to_string(n2, node2str)
        write (iout, fmtdata) trim(adjustl(node1str)), &
          trim(adjustl(node2str)), &
          this%cond(iexg), this%v_model1%x%get(n1), &
          this%v_model2%x%get(n2), flow
      end do
    end if
    !
    !cdl Implement when MVT is ready
    ! -- Mover budget output
    ibudfl = 1
    if (this%inmvt > 0) call this%mvt%mvt_ot_bdsummary(ibudfl)
    !
    ! -- OBS output
    call this%obs%obs_ot()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_ot

  !> @ brief Source options
  !!
  !! Source the options block
  !<
  subroutine source_options(this, iout)
    ! -- modules
    use ConstantsModule, only: LENVARNAME
    use InputOutputModule, only: getunit, openfile
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use ExgGwtGwtInputModule, only: ExgGwtGwtParamFoundType
    use SourceCommonModule, only: filein_fname
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    type(ExgGwtGwtParamFoundType) :: found
    character(len=LENVARNAME), dimension(3) :: adv_scheme = &
      &[character(len=LENVARNAME) :: 'UPSTREAM', 'CENTRAL', 'TVD']
    character(len=LINELENGTH) :: mvt_fname
    !
    ! -- update defaults with values sourced from input context
    call mem_set_value(this%gwfmodelname1, 'GWFMODELNAME1', this%input_mempath, &
                       found%gwfmodelname1)
    call mem_set_value(this%gwfmodelname2, 'GWFMODELNAME2', this%input_mempath, &
                       found%gwfmodelname2)
    call mem_set_value(this%iAdvScheme, 'ADV_SCHEME', this%input_mempath, &
                       adv_scheme, found%adv_scheme)
    call mem_set_value(this%ixt3d, 'DSP_XT3D_OFF', this%input_mempath, &
                       found%dsp_xt3d_off)
    call mem_set_value(this%ixt3d, 'DSP_XT3D_RHS', this%input_mempath, &
                       found%dsp_xt3d_rhs)
    !
    write (iout, '(1x,a)') 'PROCESSING GWT-GWT EXCHANGE OPTIONS'
    !
    ! -- source base class options
    call this%DisConnExchangeType%source_options(iout)
    !
    if (found%gwfmodelname1) then
      write (iout, '(4x,a,a)') &
        'GWFMODELNAME1 IS SET TO: ', trim(this%gwfmodelname1)
    end if
    !
    if (found%gwfmodelname2) then
      write (iout, '(4x,a,a)') &
        'GWFMODELNAME2 IS SET TO: ', trim(this%gwfmodelname2)
    end if
    !
    if (found%adv_scheme) then
      ! -- count from 0
      this%iAdvScheme = this%iAdvScheme - 1
      write (iout, '(4x,a,a)') &
        'ADVECTION SCHEME METHOD HAS BEEN SET TO: ', &
        trim(adv_scheme(this%iAdvScheme + 1))
    end if
    !
    if (found%dsp_xt3d_off .and. found%dsp_xt3d_rhs) then
      errmsg = 'DSP_XT3D_OFF and DSP_XT3D_RHS cannot both be set as options.'
      call store_error(errmsg)
      call store_error_filename(this%filename)
    else if (found%dsp_xt3d_off) then
      this%ixt3d = 0
      write (iout, '(4x,a)') 'XT3D FORMULATION HAS BEEN SHUT OFF.'
    else if (found%dsp_xt3d_rhs) then
      this%ixt3d = 2
      write (iout, '(4x,a)') 'XT3D RIGHT-HAND SIDE FORMULATION IS SELECTED.'
    end if
    !
    ! -- enforce 0 or 1 MVR6_FILENAME entries in option block
    if (filein_fname(mvt_fname, 'MVT6_FILENAME', this%input_mempath, &
                     this%filename)) then
      this%inmvt = getunit()
      call openfile(this%inmvt, iout, mvt_fname, 'MVT')
      write (iout, '(4x,a)') &
        'WATER MOVER TRANSPORT INFORMATION WILL BE READ FROM ', trim(mvt_fname)
    end if
    !
    ! -- enforce 0 or 1 OBS6_FILENAME entries in option block
    if (filein_fname(this%obs%inputFilename, 'OBS6_FILENAME', &
                     this%input_mempath, this%filename)) then
      this%obs%active = .true.
      this%obs%inUnitObs = GetUnit()
      call openfile(this%obs%inUnitObs, iout, this%obs%inputFilename, 'OBS')
    end if
    !
    write (iout, '(1x,a)') 'END OF GWT-GWT EXCHANGE OPTIONS'
    !
    ! -- return
    return
  end subroutine source_options

  !> @ brief Read mover
  !!
  !! Read and process movers
  !<
  subroutine read_mvt(this, iout)
    ! -- modules
    use TspMvtModule, only: mvt_cr
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(in) :: iout
    !
    ! -- Create and initialize the mover object  Here, fmi is set to the one
    !    for gwtmodel1 so that a call to save flows has an associated dis
    !    object.
    call mvt_cr(this%mvt, this%name, this%inmvt, iout, this%gwtmodel1%fmi, &
                this%gwtmodel1%eqnsclfac, &
                gwfmodelname1=this%gwfmodelname1, &
                gwfmodelname2=this%gwfmodelname2, &
                fmi2=this%gwtmodel2%fmi)
    !
    ! -- Return
    return
  end subroutine read_mvt

  !> @ brief Allocate scalars
  !!
  !! Allocate scalar variables
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    call this%DisConnExchangeType%allocate_scalars()
    !
    call mem_allocate(this%inewton, 'INEWTON', this%memoryPath)
    call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    call mem_allocate(this%iAdvScheme, 'IADVSCHEME', this%memoryPath)
    this%inewton = 0
    this%inobs = 0
    this%iAdvScheme = 0
    !
    call mem_allocate(this%inmvt, 'INMVT', this%memoryPath)
    this%inmvt = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @ brief Deallocate
  !!
  !! Deallocate memory associated with this object
  !<
  subroutine gwt_gwt_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    ! -- objects
    if (this%inmvt > 0) then
      call this%mvt%mvt_da()
      deallocate (this%mvt)
    end if
    call this%obs%obs_da()
    deallocate (this%obs)
    !
    ! -- arrays
    call mem_deallocate(this%cond)
    call mem_deallocate(this%simvals)
    call mem_deallocate(this%gwfsimvals, 'GWFSIMVALS', this%memoryPath) ! linked memory
    !
    ! -- output table objects
    if (associated(this%outputtab1)) then
      call this%outputtab1%table_da()
      deallocate (this%outputtab1)
      nullify (this%outputtab1)
    end if
    if (associated(this%outputtab2)) then
      call this%outputtab2%table_da()
      deallocate (this%outputtab2)
      nullify (this%outputtab2)
    end if
    !
    ! -- scalars
    deallocate (this%filename)
    call mem_deallocate(this%inewton)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%iAdvScheme)
    call mem_deallocate(this%inmvt)
    !
    ! -- deallocate base
    call this%DisConnExchangeType%disconnex_da()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_da

  !> @ brief Allocate arrays
  !!
  !! Allocate arrays
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcol, i
    !
    call this%DisConnExchangeType%allocate_arrays()
    !
    call mem_allocate(this%cond, this%nexg, 'COND', this%memoryPath)
    call mem_allocate(this%simvals, this%nexg, 'SIMVALS', this%memoryPath)
    !
    ! -- Initialize
    do i = 1, this%nexg
      this%cond(i) = DNODATA
    end do
    !
    ! -- allocate and initialize the output table
    if (this%iprflow /= 0) then
      !
      ! -- dimension table
      ntabcol = 3
      if (this%inamedbound > 0) then
        ntabcol = ntabcol + 1
      end if
      !
      ! -- initialize the output table objects
      !    outouttab1
      if (this%v_model1%is_local) then
        call table_cr(this%outputtab1, this%name, '    ')
        call this%outputtab1%table_df(this%nexg, ntabcol, this%gwtmodel1%iout, &
                                      transient=.TRUE.)
        text = 'NUMBER'
        call this%outputtab1%initialize_column(text, 10, alignment=TABCENTER)
        text = 'CELLID'
        call this%outputtab1%initialize_column(text, 20, alignment=TABLEFT)
        text = 'RATE'
        call this%outputtab1%initialize_column(text, 15, alignment=TABCENTER)
        if (this%inamedbound > 0) then
          text = 'NAME'
          call this%outputtab1%initialize_column(text, 20, alignment=TABLEFT)
        end if
      end if
      !    outouttab2
      if (this%v_model2%is_local) then
        call table_cr(this%outputtab2, this%name, '    ')
        call this%outputtab2%table_df(this%nexg, ntabcol, this%gwtmodel2%iout, &
                                      transient=.TRUE.)
        text = 'NUMBER'
        call this%outputtab2%initialize_column(text, 10, alignment=TABCENTER)
        text = 'CELLID'
        call this%outputtab2%initialize_column(text, 20, alignment=TABLEFT)
        text = 'RATE'
        call this%outputtab2%initialize_column(text, 15, alignment=TABCENTER)
        if (this%inamedbound > 0) then
          text = 'NAME'
          call this%outputtab2%initialize_column(text, 20, alignment=TABLEFT)
        end if
      end if
    end if
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  !> @ brief Define observations
  !!
  !! Define the observations associated with this object
  !<
  subroutine gwt_gwt_df_obs(this)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: indx
    !
    ! -- Store obs type and assign procedure pointer
    !    for gwt-gwt observation type.
    call this%obs%StoreObsType('flow-ja-face', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => gwt_gwt_process_obsID
    !
    ! -- Return
    return
  end subroutine gwt_gwt_df_obs

  !> @ brief Read and prepare observations
  !!
  !! Handle observation exchanges exchange-boundary names.
  !<
  subroutine gwt_gwt_rp_obs(this)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    class(ObserveType), pointer :: obsrv => null()
    character(len=LENBOUNDNAME) :: bname
    logical :: jfound
    ! -- formats
10  format('Exchange "', a, '" for observation "', a, &
           '" is invalid in package "', a, '"')
20  format('Exchange id "', i0, '" for observation "', a, &
           '" is invalid in package "', a, '"')
    !
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      !
      ! -- indxbnds needs to be reset each stress period because
      !    list of boundaries can change each stress period.
      ! -- Not true for exchanges, but leave this in for now anyway.
      call obsrv%ResetObsIndex()
      obsrv%BndFound = .false.
      !
      bname = obsrv%FeatureName
      if (bname /= '') then
        ! -- Observation location(s) is(are) based on a boundary name.
        !    Iterate through all boundaries to identify and store
        !    corresponding index(indices) in bound array.
        jfound = .false.
        do j = 1, this%nexg
          if (this%boundname(j) == bname) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call obsrv%AddObsIndex(j)
          end if
        end do
        if (.not. jfound) then
          write (errmsg, 10) trim(bname), trim(obsrv%ObsTypeId), trim(this%name)
          call store_error(errmsg)
        end if
      else
        ! -- Observation location is a single exchange number
        if (obsrv%intPak1 <= this%nexg .and. obsrv%intPak1 > 0) then
          jfound = .true.
          obsrv%BndFound = .true.
          obsrv%CurrentTimeStepEndValue = DZERO
          call obsrv%AddObsIndex(obsrv%intPak1)
        else
          jfound = .false.
        end if
        if (.not. jfound) then
          write (errmsg, 20) obsrv%intPak1, trim(obsrv%ObsTypeId), trim(this%name)
          call store_error(errmsg)
        end if
      end if
    end do
    !
    ! -- write summary of error messages
    if (count_errors() > 0) then
      call store_error_filename(this%obs%inputFilename)
    end if
    !
    ! -- Return
    return
  end subroutine gwt_gwt_rp_obs

  !> @ brief Final processing
  !!
  !! Conduct any final processing
  !<
  subroutine gwt_gwt_fp(this)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    ! -- Return
    return
  end subroutine gwt_gwt_fp

  !> @brief Return true when this exchange provides matrix coefficients for
  !! solving @param model
  !<
  function gwt_gwt_connects_model(this, model) result(is_connected)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    class(BaseModelType), pointer, intent(in) :: model !< the model to which the exchange might hold a connection
    ! -- return
    logical(LGP) :: is_connected !< true, when connected
    !
    is_connected = .false.
    !
    ! only connected when model is GwtModelType of course
    select type (model)
    class is (GwtModelType)
      if (associated(this%gwtmodel1, model)) then
        is_connected = .true.
      else if (associated(this%gwtmodel2, model)) then
        is_connected = .true.
      end if
    end select
    !
    ! -- Return
    return
  end function gwt_gwt_connects_model

  !> @brief Should interface model be used for this exchange
  !!
  !! For now this always returns true, since we do not support
  !! a classic-style two-point flux approximation for GWT-GWT.
  !! If we ever add logic to support a simpler non-interface
  !! model flux calculation, then logic should be added here to
  !! set the return accordingly.
  !<
  function use_interface_model(this) result(use_im)
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- return
    logical(LGP) :: use_im !< true when interface model should be used
    !
    ! For now set use_im to .true. since the interface model approach
    ! must currently be used for any GWT-GWT exchange.
    use_im = .true.
    !
    ! -- Return
    return
  end function

  !> @ brief Save simulated flow observations
  !!
  !! Save the simulated flows for each exchange
  !<
  subroutine gwt_gwt_save_simvals(this)
    ! -- dummy
    use SimVariablesModule, only: errmsg
    use ConstantsModule, only: DZERO
    use ObserveModule, only: ObserveType
    class(GwtExchangeType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n1
    integer(I4B) :: n2
    integer(I4B) :: iexg
    real(DP) :: v
    type(ObserveType), pointer :: obsrv => null()
    !
    ! -- Write simulated values for all gwt-gwt observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        do j = 1, obsrv%indxbnds_count
          iexg = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
          case ('FLOW-JA-FACE')
            n1 = this%nodem1(iexg)
            n2 = this%nodem2(iexg)
            v = this%simvals(iexg)
          case default
            errmsg = 'Unrecognized observation type: '// &
                     trim(obsrv%ObsTypeId)
            call store_error(errmsg)
            call store_error_filename(this%obs%inputFilename)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
    end if
    !
    ! -- Return
    return
  end subroutine gwt_gwt_save_simvals

  !> @ brief Obs ID processer
  !!
  !! Process observations for this exchange
  !<
  subroutine gwt_gwt_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: urword
    use ObserveModule, only: ObserveType
    use BaseDisModule, only: DisBaseType
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: inunitobs
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: n, iexg, istat
    integer(I4B) :: icol, istart, istop
    real(DP) :: r
    character(len=LINELENGTH) :: strng
    !
    strng = obsrv%IDstring
    icol = 1
    ! -- get exchange index
    call urword(strng, icol, istart, istop, 1, n, r, iout, inunitobs)
    read (strng(istart:istop), '(i10)', iostat=istat) iexg
    if (istat == 0) then
      obsrv%intPak1 = iexg
    else
      ! Integer can't be read from strng; it's presumed to be an exchange
      ! boundary name (already converted to uppercase)
      obsrv%FeatureName = trim(adjustl(strng))
      ! -- Observation may require summing rates from multiple exchange
      !    boundaries, so assign intPak1 as a value that indicates observation
      !    is for a named exchange boundary or group of exchange boundaries.
      obsrv%intPak1 = NAMEDBOUNDFLAG
    end if
    !
    ! -- Return
    return
  end subroutine gwt_gwt_process_obsID

  !> @ brief Cast polymorphic object as exchange
  !!
  !! Cast polymorphic object as exchange
  !<
  function CastAsGwtExchange(obj) result(res)
    implicit none
    ! -- dummy
    class(*), pointer, intent(inout) :: obj
    ! -- return
    class(GwtExchangeType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (GwtExchangeType)
      res => obj
    end select
    !
    ! -- Return
    return
  end function CastAsGwtExchange

  !> @ brief Get exchange from list
  !!
  !! Return an exchange from the list for specified index
  !<
  function GetGwtExchangeFromList(list, idx) result(res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    ! -- return
    class(GwtExchangeType), pointer :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsGwtExchange(obj)
    !
    ! -- Return
    return
  end function GetGwtExchangeFromList

end module GwtGwtExchangeModule

