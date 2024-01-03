module SimulationCreateModule

  use KindModule, only: DP, I4B, LGP, write_kindinfo
  use ConstantsModule, only: LINELENGTH, LENMODELNAME, LENBIGLINE, &
                             DZERO, LENEXCHANGENAME, LENMEMPATH, LENPACKAGETYPE

  use CharacterStringModule, only: CharacterStringType
  use SimVariablesModule, only: iout, simulation_mode, proc_id, &
                                nr_procs, model_names, model_ranks, &
                                model_loc_idx
  use SimModule, only: store_error, count_errors, &
                       store_error_filename, MaxErrors
  use VersionModule, only: write_listfile_header
  use InputOutputModule, only: getunit, urword, openfile
  use ArrayHandlersModule, only: expandarray, ifind
  use BaseModelModule, only: BaseModelType
  use BaseSolutionModule, only: BaseSolutionType, AddBaseSolutionToList, &
                                GetBaseSolutionFromList
  use SolutionGroupModule, only: SolutionGroupType, AddSolutionGroupToList
  use BaseExchangeModule, only: BaseExchangeType, GetBaseExchangeFromList
  use ListsModule, only: basesolutionlist, basemodellist, &
                         solutiongrouplist, baseexchangelist
  use BaseModelModule, only: GetBaseModelFromList
  use ListModule, only: ListType
  use ModelsCreateModule, only: models_create

  implicit none
  private
  public :: simulation_cr
  public :: simulation_da

contains

  !> @brief Source control file input context and initialize models, exchanges
  !<
  subroutine simulation_cr()
    call source_simulation_nam()
  end subroutine simulation_cr

  !> @brief Deallocate simulation variables
  !<
  subroutine simulation_da()
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    !
    ! -- Deallocate input memory
    call memorylist_remove('SIM', 'NAM', idm_context)
    call memorylist_remove(component='SIM', context=idm_context)
    !
    ! -- variables
    deallocate (model_names)
    deallocate (model_loc_idx)

  end subroutine simulation_da

  !> @brief Source the simulation name file
  !!
  !! Source from the simulation nam input context to initialize the models,
  !! exchanges, solutions, solutions groups.  Then add the exchanges to
  !! the appropriate solutions.
  !!
  !<
  subroutine source_simulation_nam()
    !
    ! -- Process OPTIONS block in namfile
    call options_create()
    !
    ! -- Process TIMING block in namfile
    call timing_create()
    !
    ! -- Process MODELS block in namfile
    call models_create()
    !
    ! -- Process EXCHANGES block in namfile
    call exchanges_create()
    !
    ! -- Process SOLUTION_GROUPS blocks in namfile
    call solution_groups_create()
    !
    ! -- Go through each model and make sure that it has been assigned to
    !    a solution.
    call check_model_assignment()
    !
    ! -- Go through each solution and assign exchanges accordingly
    call assign_exchanges()

  end subroutine source_simulation_nam

  !> @brief Set the simulation options
  !<
  subroutine options_create()
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    use MemoryManagerModule, only: mem_set_print_option
    use SimVariablesModule, only: isimcontinue, isimcheck
    ! -- locals
    character(len=LENMEMPATH) :: input_mempath
    integer(I4B), pointer :: simcontinue, nocheck, maxerror
    character(len=:), pointer :: prmem
    character(len=LINELENGTH) :: errmsg
    !
    ! -- set input memory path
    input_mempath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context option params
    call mem_setptr(simcontinue, 'CONTINUE', input_mempath)
    call mem_setptr(nocheck, 'NOCHECK', input_mempath)
    call mem_setptr(prmem, 'PRMEM', input_mempath)
    call mem_setptr(maxerror, 'MAXERRORS', input_mempath)
    !
    ! -- update sim options
    isimcontinue = simcontinue
    isimcheck = nocheck
    call MaxErrors(maxerror)
    !
    if (prmem /= '') then
      errmsg = ''
      call mem_set_print_option(iout, prmem, errmsg)
      if (errmsg /= '') then
        call store_error(errmsg, .true.)
      end if
    end if
    !
    ! -- log values to list file
    if (iout > 0) then
      write (iout, '(/1x,a)') 'READING SIMULATION OPTIONS'
      !
      if (isimcontinue == 1) then
        write (iout, '(4x, a)') &
          'SIMULATION WILL CONTINUE EVEN IF THERE IS NONCONVERGENCE.'
      end if
      !
      if (isimcheck == 0) then
        write (iout, '(4x, a)') &
          'MODEL DATA WILL NOT BE CHECKED FOR ERRORS.'
      end if
      !
      write (iout, '(4x, a, i0)') &
        'MAXIMUM NUMBER OF ERRORS THAT WILL BE STORED IS ', maxerror
      !
      if (prmem /= '') then
        write (iout, '(4x, a, a, a)') &
          'MEMORY_PRINT_OPTION SET TO "', trim(prmem), '".'
      end if
      !
      write (iout, '(1x,a)') 'END OF SIMULATION OPTIONS'
    end if

  end subroutine options_create

  !> @brief Set the timing module to be used for the simulation
  !<
  subroutine timing_create()
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    use TdisModule, only: tdis_cr
    ! -- locals
    character(len=LENMEMPATH) :: input_mempath
    character(len=:), pointer :: tdis6
    logical :: terminate = .true.
    !
    ! -- set input memory path
    input_mempath = create_mem_path('SIM', 'NAM', idm_context)
    !
    write (iout, '(/1x,a)') 'READING SIMULATION TIMING'
    !
    ! -- set pointers to input context timing params
    call mem_setptr(tdis6, 'TDIS6', input_mempath)
    !
    ! -- create timing
    if (tdis6 /= '') then
      call tdis_cr(tdis6)
    else
      call store_error('TIMING block variable TDIS6 is unset'// &
                       ' in simulation control input.', terminate)
    end if
    !
    write (iout, '(1x,a)') 'END OF SIMULATION TIMING'

  end subroutine timing_create

  !> @brief Set the exchanges to be used for the simulation
  !<
  subroutine exchanges_create()
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    use GwfGwfExchangeModule, only: gwfexchange_create
    use GwfGwtExchangeModule, only: gwfgwt_cr
    use GwtGwtExchangeModule, only: gwtexchange_create
    use VirtualGwfExchangeModule, only: add_virtual_gwf_exchange
    use VirtualGwtExchangeModule, only: add_virtual_gwt_exchange
    ! -- locals
    character(len=LENMEMPATH) :: input_mempath
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: etypes !< exg types
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: efiles !< exg file names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: emnames_a !< model a names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: emnames_b !< model b names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: emempaths
    character(len=LINELENGTH) :: exgtype
    integer(I4B) :: exg_id
    integer(I4B) :: m1_id, m2_id
    character(len=LINELENGTH) :: fname, name1, name2
    character(len=LENEXCHANGENAME) :: exg_name
    character(len=LENMEMPATH) :: exg_mempath
    integer(I4B) :: n
    character(len=LINELENGTH) :: errmsg
    logical(LGP) :: terminate = .true.
    logical(LGP) :: both_remote, both_local
    ! -- formats
    character(len=*), parameter :: fmtmerr = "('Error in simulation control ', &
      &'file.  Could not find model: ', a)"
    !
    ! -- set input memory path
    input_mempath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context exchange attribute arrays
    call mem_setptr(etypes, 'EXGTYPE', input_mempath)
    call mem_setptr(efiles, 'EXGFILE', input_mempath)
    call mem_setptr(emnames_a, 'EXGMNAMEA', input_mempath)
    call mem_setptr(emnames_b, 'EXGMNAMEB', input_mempath)
    call mem_setptr(emempaths, 'EXGMEMPATHS', input_mempath)
    !
    ! -- open exchange logging block
    write (iout, '(/1x,a)') 'READING SIMULATION EXCHANGES'
    !
    ! -- initialize
    exg_id = 0
    !
    ! -- create exchanges
    do n = 1, size(etypes)
      !
      ! -- attributes for this exchange
      exgtype = etypes(n)
      fname = efiles(n)
      name1 = emnames_a(n)
      name2 = emnames_b(n)
      exg_mempath = emempaths(n)

      exg_id = exg_id + 1

      ! find model index in list
      m1_id = ifind(model_names, name1)
      if (m1_id < 0) then
        write (errmsg, fmtmerr) trim(name1)
        call store_error(errmsg, terminate)
      end if
      m2_id = ifind(model_names, name2)
      if (m2_id < 0) then
        write (errmsg, fmtmerr) trim(name2)
        call store_error(errmsg, terminate)
      end if

      ! both models on other process? then don't create it here...
      both_remote = (model_loc_idx(m1_id) == -1 .and. &
                     model_loc_idx(m2_id) == -1)
      both_local = (model_loc_idx(m1_id) > 0 .and. &
                    model_loc_idx(m2_id) > 0)
      if (.not. both_remote) then
        write (iout, '(4x,a,a,i0,a,i0,a,i0)') trim(exgtype), ' exchange ', &
          exg_id, ' will be created to connect model ', m1_id, &
          ' with model ', m2_id
      end if

      select case (exgtype)
      case ('GWF6-GWF6')
        write (exg_name, '(a,i0)') 'GWF-GWF_', exg_id
        if (.not. both_remote) then
          call gwfexchange_create(fname, exg_name, exg_id, m1_id, m2_id, &
                                  exg_mempath)
        end if
        call add_virtual_gwf_exchange(exg_name, exg_id, m1_id, m2_id)
      case ('GWF6-GWT6')
        if (both_local) then
          call gwfgwt_cr(fname, exg_id, m1_id, m2_id)
        end if
      case ('GWT6-GWT6')
        write (exg_name, '(a,i0)') 'GWT-GWT_', exg_id
        if (.not. both_remote) then
          call gwtexchange_create(fname, exg_name, exg_id, m1_id, m2_id, &
                                  exg_mempath)
        end if
        call add_virtual_gwt_exchange(exg_name, exg_id, m1_id, m2_id)
      case default
        write (errmsg, '(a,a)') &
          'Unknown simulation exchange type: ', trim(exgtype)
        call store_error(errmsg, terminate)
      end select
    end do
    !
    ! -- close exchange logging block
    write (iout, '(1x,a)') 'END OF SIMULATION EXCHANGES'

  end subroutine exchanges_create

  !> @brief Check a solution_group to be used for the simulation
  !<
  subroutine solution_group_check(sgp, sgid, isgpsoln)
    ! -- dummy
    type(SolutionGroupType), pointer, intent(inout) :: sgp
    integer(I4B), intent(in) :: sgid
    integer(I4B), intent(in) :: isgpsoln
    ! -- local
    character(len=LINELENGTH) :: errmsg
    logical :: terminate = .true.
    ! -- formats
    character(len=*), parameter :: fmterrmxiter = &
      "('MXITER is set to ', i0, ' but there is only one solution', &
      &' in SOLUTION GROUP ', i0, '. Set MXITER to 1 in simulation control', &
      &' file.')"
    !
    ! -- error check completed group
    if (sgid > 0) then
      !
      ! -- Make sure there is a solution in this solution group
      if (isgpsoln == 0) then
        write (errmsg, '(a,i0)') &
          'There are no solutions for solution group ', sgid
        call store_error(errmsg, terminate)
      end if
      !
      ! -- If there is only one solution then mxiter should be 1.
      if (isgpsoln == 1 .and. sgp%mxiter > 1) then
        write (errmsg, fmterrmxiter) sgp%mxiter, isgpsoln
        call store_error(errmsg, terminate)
      end if
    end if

  end subroutine solution_group_check

  !> @brief Set the solution_groups to be used for the simulation
  !<
  subroutine solution_groups_create()
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context, simulation_mode
    use SolutionGroupModule, only: SolutionGroupType, &
                                   solutiongroup_create
    use SolutionFactoryModule, only: create_ims_solution, create_ems_solution
    use BaseSolutionModule, only: BaseSolutionType
    use BaseModelModule, only: BaseModelType
    use BaseExchangeModule, only: BaseExchangeType
    use InputOutputModule, only: parseline, upcase
    ! -- local
    character(len=LENMEMPATH) :: input_mempath
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: slntype
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: slnfname
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: slnmnames
    integer(I4B), dimension(:), contiguous, pointer :: blocknum
    character(len=LINELENGTH) :: stype, fname
    character(len=:), allocatable :: mnames
    type(SolutionGroupType), pointer :: sgp
    class(BaseSolutionType), pointer :: sp
    class(BaseModelType), pointer :: mp
    integer(I4B) :: isoln
    integer(I4B) :: isgpsoln
    integer(I4B) :: sgid
    integer(I4B) :: glo_mid
    integer(I4B) :: loc_idx
    integer(I4B) :: i, j, istat, mxiter
    integer(I4B) :: nwords
    character(len=LENMODELNAME), dimension(:), allocatable :: words
    character(len=:), allocatable :: parse_str
    character(len=LINELENGTH) :: errmsg
    logical :: terminate = .true.
    !
    ! -- set memory path
    input_mempath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context solution attribute arrays
    call mem_setptr(slntype, 'SLNTYPE', input_mempath)
    call mem_setptr(slnfname, 'SLNFNAME', input_mempath)
    call mem_setptr(slnmnames, 'SLNMNAMES', input_mempath)
    call mem_setptr(blocknum, 'SOLUTIONGROUPNUM', input_mempath)
    !
    ! -- open solution group logging block
    write (iout, '(/1x,a)') 'READING SOLUTIONGROUP'
    !
    ! -- initialize
    sgid = 0 ! integer id of soln group, tracks with blocknum
    isoln = 0 ! cumulative solution number
    !
    ! -- create solution groups
    do i = 1, size(blocknum)
      !
      ! -- allocate slnmnames string
      allocate (character(slnmnames(i)%strlen()) :: mnames)
      !
      ! -- attributes for this solution
      stype = slntype(i)
      fname = slnfname(i)
      mnames = slnmnames(i)

      if (blocknum(i) /= sgid) then
        !
        ! -- check for new soln group
        if (blocknum(i) == sgid + 1) then
          !
          ! -- error check completed group
          call solution_group_check(sgp, sgid, isgpsoln)
          !
          ! -- reinitialize
          nullify (sgp)
          isgpsoln = 0 ! solution counter for this solution group
          !
          ! -- set sgid
          sgid = blocknum(i)
          !
          ! -- create new soln group and add to global list
          call solutiongroup_create(sgp, sgid)
          call AddSolutionGroupToList(solutiongrouplist, sgp)
        else
          write (errmsg, '(a,i0,a,i0,a)') &
            'Solution group blocks are not listed consecutively. Found ', &
            blocknum(i), ' when looking for ', sgid + 1, '.'
          call store_error(errmsg, terminate)
        end if
      end if
      !
      ! --
      select case (stype)
        !
      case ('MXITER')
        read (fname, *, iostat=istat) mxiter
        if (istat == 0) then
          sgp%mxiter = mxiter
        end if
      case ('IMS6')
        !
        ! -- increment solution counters
        isoln = isoln + 1
        isgpsoln = isgpsoln + 1
        !
        ! -- create soln and add to group
        sp => create_ims_solution(simulation_mode, fname, isoln)
        call sgp%add_solution(isoln, sp)
        !
        ! -- parse model names
        parse_str = trim(mnames)//' '
        call parseline(parse_str, nwords, words)
        !
        ! -- Find each model id and get model
        do j = 1, nwords
          call upcase(words(j))
          glo_mid = ifind(model_names, words(j))
          if (glo_mid == -1) then
            write (errmsg, '(a,a)') 'Invalid model name: ', trim(words(j))
            call store_error(errmsg, terminate)
          end if
          !
          loc_idx = model_loc_idx(glo_mid)
          if (loc_idx == -1) then
            if (simulation_mode == 'PARALLEL') then
              ! this is still ok
              cycle
            end if
          end if
          !
          mp => GetBaseModelFromList(basemodellist, loc_idx)
          !
          ! -- Add the model to the solution
          call sp%add_model(mp)
          mp%idsoln = isoln
        end do
      case ('EMS6')
        !
        ! -- increment solution counters
        isoln = isoln + 1
        isgpsoln = isgpsoln + 1
        !
        ! -- create soln and add to group
        sp => create_ems_solution(simulation_mode, fname, isoln)
        call sgp%add_solution(isoln, sp)
        !
        ! -- parse model names
        parse_str = trim(mnames)//' '
        call parseline(parse_str, nwords, words)
        !
        ! -- Find each model id and get model
        do j = 1, nwords
          call upcase(words(j))
          glo_mid = ifind(model_names, words(j))
          if (glo_mid == -1) then
            write (errmsg, '(a,a)') 'Invalid model name: ', trim(words(j))
            call store_error(errmsg, terminate)
          end if
          !
          loc_idx = model_loc_idx(glo_mid)
          if (loc_idx == -1) then
            if (simulation_mode == 'PARALLEL') then
              ! this is still ok
              cycle
            end if
          end if
          !
          mp => GetBaseModelFromList(basemodellist, loc_idx)
          !
          ! -- Add the model to the solution
          call sp%add_model(mp)
          mp%idsoln = isoln
        end do
      case default
      end select
      !
      ! -- clean up
      deallocate (mnames)
    end do
    !
    ! -- error check final group
    call solution_group_check(sgp, sgid, isgpsoln)
    !
    ! -- close exchange logging block
    write (iout, '(1x,a)') 'END OF SOLUTIONGROUP'
    !
    ! -- Check and make sure at least one solution group was found
    if (solutiongrouplist%Count() == 0) then
      call store_error('There are no solution groups.', terminate)
    end if

  end subroutine solution_groups_create

  !> @brief Check for dangling models, and break with
  !! error when found
  !<
  subroutine check_model_assignment()
    character(len=LINELENGTH) :: errmsg
    class(BaseModelType), pointer :: mp
    integer(I4B) :: im

    do im = 1, basemodellist%Count()
      mp => GetBaseModelFromList(basemodellist, im)
      if (mp%idsoln == 0) then
        write (errmsg, '(a,a)') &
          'Model was not assigned to a solution: ', mp%name
        call store_error(errmsg)
      end if
    end do
    if (count_errors() > 0) then
      call store_error_filename('mfsim.nam')
    end if

  end subroutine check_model_assignment

  !> @brief Assign exchanges to solutions
  !!
  !! This assigns NumericalExchanges to NumericalSolutions,
  !! based on the link between the models in the solution and
  !! those exchanges. The BaseExchange%connects_model() function
  !! should be overridden to indicate if such a link exists.
  !<
  subroutine assign_exchanges()
    ! -- local
    class(BaseSolutionType), pointer :: sp
    class(BaseExchangeType), pointer :: ep
    class(BaseModelType), pointer :: mp
    type(ListType), pointer :: models_in_solution
    integer(I4B) :: is, ie, im

    do is = 1, basesolutionlist%Count()
      sp => GetBaseSolutionFromList(basesolutionlist, is)
      !
      ! -- now loop over exchanges
      do ie = 1, baseexchangelist%Count()
        ep => GetBaseExchangeFromList(baseexchangelist, ie)
        !
        ! -- and add when it affects (any model in) the solution matrix
        models_in_solution => sp%get_models()
        do im = 1, models_in_solution%Count()
          mp => GetBaseModelFromList(models_in_solution, im)
          if (ep%connects_model(mp)) then
            !
            ! -- add to solution (and only once)
            call sp%add_exchange(ep)
            exit
          end if
        end do
      end do
    end do
  end subroutine assign_exchanges

end module SimulationCreateModule
