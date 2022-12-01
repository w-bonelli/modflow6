!> @brief This module contains simulation methods
!!
!! This module contains simulation methods for storing warning and error
!! messages and notes. This module also has methods for counting warnings,
!! errors, and notes in addition to stopping the simulation. The module does
!! not have any dependencies on models, exchanges, or solutions in a
!! simulation.
!!
!<
module SimModule

  use KindModule, only: DP, I4B
  use DefinedMacros, only: get_os
  use ConstantsModule, only: MAXCHARLEN, LINELENGTH, &
                             DONE, &
                             IUSTART, IULAST, &
                             VSUMMARY, VALL, VDEBUG, &
                             OSWIN, OSUNDEF
  use SimVariablesModule, only: istdout, iout, isim_level, ireturnerr, &
                                iforcestop, iunext, &
                                warnmsg
  use GenericUtilitiesModule, only: sim_message, stop_with_error
  use MessageModule, only: MessageType

  implicit none

  private
  public :: count_errors
  public :: store_error
  public :: ustop
  public :: converge_reset
  public :: converge_check
  public :: initial_message
  public :: final_message
  public :: store_warning
  public :: deprecation_warning
  public :: store_note
  public :: count_warnings
  public :: count_notes
  public :: store_error_unit
  public :: store_error_filename
  public :: MaxErrors

  type(MessageType) :: sim_errors
  type(MessageType) :: sim_uniterrors
  type(MessageType) :: sim_warnings
  type(MessageType) :: sim_notes

contains

  !> @brief Return number of errors
    !!
    !!  Function to return the number of errors messages that have been stored.
    !!
    !!  @return  ncount number of error messages stored
    !!
  !<
  function count_errors() result(ncount)
    ! -- return variable
    integer(I4B) :: ncount
    !
    ! -- set ncount
    ncount = sim_errors%count_message()
    !
    ! -- return
    return
  end function count_errors

  !> @brief Return number of warnings
    !!
    !!  Function to return the number of warning messages that have been stored.
    !!
    !!  @return  ncount number of warning messages stored
    !!
  !<
  function count_warnings() result(ncount)
    ! -- return variable
    integer(I4B) :: ncount
    !
    ! -- set ncount
    ncount = sim_warnings%count_message()
    !
    ! -- return
    return
  end function count_warnings

  !> @brief Return number of notes
    !!
    !!  Function to return the number of notes that have been stored.
    !!
    !!  @return  ncount number of notes stored
    !!
  !<
  function count_notes() result(ncount)
    ! -- return variable
    integer(I4B) :: ncount
    !
    ! -- set ncount
    ncount = sim_notes%count_message()
    !
    ! -- return
    return
  end function count_notes

  !> @brief Set the maximum number of errors stored
    !!
    !!  Subroutine to set the maximum number of error messages that will be stored
    !!  in a simulation.
    !!
  !<
  subroutine MaxErrors(imax)
    ! -- dummy variables
    integer(I4B), intent(in) :: imax !< maximum number of error messages that will be stored
    !
    ! -- set the maximum number of error messages that will be saved
    call sim_errors%set_max_message(imax)
    !
    ! -- return
    return
  end subroutine MaxErrors

  !> @brief Store error message
    !!
    !!  Subroutine to store a error message for printing at the end of
    !!  the simulation.
    !!
  !<
  subroutine store_error(msg, terminate)
    ! -- dummy variable
    character(len=*), intent(in) :: msg !< error message
    logical, optional, intent(in) :: terminate !< boolean indicating if the simulation should be terminated
    ! -- local variables
    logical :: lterminate
    !
    ! -- process optional variables
    if (present(terminate)) then
      lterminate = terminate
    else
      lterminate = .FALSE.
    end if
    !
    ! -- store error
    call sim_errors%store_message(msg)
    !
    ! -- terminate the simulation
    if (lterminate) then
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine store_error

  !> @brief Get the file name
    !!
    !!  Subroutine to get the file name from the unit number for a open file.
    !!  If the INQUIRE function returns the full path (for example, the INTEL
    !!  compiler) then the returned file name (fname) is limited to the filename
    !!  without the path.
    !!
  !<
  subroutine get_filename(iunit, fname)
    ! -- dummy variables
    integer(I4B), intent(in) :: iunit !< open file unit number
    character(len=*), intent(inout) :: fname !< file name attached to the open file unit number
    ! -- local variables
    integer(I4B) :: ipos
    integer(I4B) :: ios
    integer(I4B) :: ilen
    !
    ! -- get file name from unit number
    inquire (unit=iunit, name=fname)
    !
    ! -- determine the operating system
    ios = get_os()
    !
    ! -- extract filename from full path, if present
    !    forward slash on linux, unix, and osx
    if (ios /= OSWIN) then
      ipos = index(fname, '/', back=.TRUE.)
    end if
    !
    ! -- check for backslash on windows or undefined os and
    !    forward slashes were not found
    if (ios == OSWIN .or. ios == OSUNDEF) then
      if (ipos < 1) then
        ipos = index(fname, '\', back=.TRUE.)
      end if
    end if
    !
    ! -- exclude the path from the file name
    if (ipos > 0) then
      ilen = len_trim(fname)
      write (fname, '(a)') fname(ipos + 1:ilen)//' '
    end if
    !
    ! -- return
    return
  end subroutine get_filename

  !> @brief Store the file unit number
    !!
    !!  Subroutine to convert the unit number for a open file to a file name
    !!  and indicate that there is an error reading from the file. By default,
    !!  the simulation is terminated when this subroutine is called.
    !!
  !<
  subroutine store_error_unit(iunit, terminate)
    ! -- dummy variables
    integer(I4B), intent(in) :: iunit !< open file unit number
    logical, optional, intent(in) :: terminate !< boolean indicating if the simulation should be terminated
    ! -- local variables
    logical :: lterminate
    character(len=LINELENGTH) :: fname
    character(len=LINELENGTH) :: errmsg
    !
    ! -- process optional variables
    if (present(terminate)) then
      lterminate = terminate
    else
      lterminate = .TRUE.
    end if
    !
    ! -- store error unit
    inquire (unit=iunit, name=fname)
    write (errmsg, '(3a)') &
      "ERROR OCCURRED WHILE READING FILE '", trim(adjustl(fname)), "'"
    call sim_uniterrors%store_message(errmsg)
    !
    ! -- terminate the simulation
    if (lterminate) then
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine store_error_unit

  !> @brief Store the erroring file name
    !!
    !!  Subroutine to store the file name issuing an error. By default,
    !!  the simulation is terminated when this subroutine is called
    !!
  !<
  subroutine store_error_filename(filename, terminate)
    ! -- dummy variables
    character(len=*), intent(in) :: filename !< erroring file name
    logical, optional, intent(in) :: terminate !< boolean indicating if the simulation should be terminated
    ! -- local variables
    logical :: lterminate
    character(len=LINELENGTH) :: errmsg
    !
    ! -- process optional variables
    if (present(terminate)) then
      lterminate = terminate
    else
      lterminate = .TRUE.
    end if
    !
    ! -- store error unit
    write (errmsg, '(3a)') &
      "ERROR OCCURRED WHILE READING FILE '", trim(adjustl(filename)), "'"
    call sim_uniterrors%store_message(errmsg)
    !
    ! -- terminate the simulation
    if (lterminate) then
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine store_error_filename

  !> @brief Store warning message
    !!
    !!  Subroutine to store a warning message for printing at the end of
    !!  the simulation.
    !!
  !<
  subroutine store_warning(msg, substring)
    ! -- dummy variables
    character(len=*), intent(in) :: msg !< warning message
    character(len=*), intent(in), optional :: substring !< optional string that can be used
                                                        !! to prevent storing duplicate messages
    !
    ! -- store warning
    if (present(substring)) then
      call sim_warnings%store_message(msg, substring)
    else
      call sim_warnings%store_message(msg)
    end if
    !
    ! -- return
    return
  end subroutine store_warning

  !> @brief Store deprecation warning message
    !!
    !!  Subroutine to store a warning message for deprecated variables
    !!  and printing at the end of simulation.
    !!
  !<
  subroutine deprecation_warning(cblock, cvar, cver, endmsg, iunit)
    ! -- modules
    use ArrayHandlersModule, only: ExpandArray
    ! -- dummy variables
    character(len=*), intent(in) :: cblock !< block name
    character(len=*), intent(in) :: cvar !< variable name
    character(len=*), intent(in) :: cver !< version when variable was deprecated
    character(len=*), intent(in), optional :: endmsg !< optional user defined message to append
                                                     !! at the end of the deprecation warning
    integer(I4B), intent(in), optional :: iunit !< optional input file unit number with
                                                !! the deprecated variable
    ! -- local variables
    character(len=MAXCHARLEN) :: message
    character(len=LINELENGTH) :: fname
    !
    ! -- build message
    write (message, '(a)') &
      trim(cblock)//" BLOCK VARIABLE '"//trim(cvar)//"'"
    if (present(iunit)) then
      call get_filename(iunit, fname)
      write (message, '(a,1x,3a)') &
        trim(message), "IN FILE '", trim(fname), "'"
    end if
    write (message, '(a)') &
      trim(message)//' WAS DEPRECATED IN VERSION '//trim(cver)//'.'
    if (present(endmsg)) then
      write (message, '(a,1x,2a)') trim(message), trim(endmsg), '.'
    end if
    !
    ! -- store warning
    call sim_warnings%store_message(message)
    !
    ! -- return
    return
  end subroutine deprecation_warning

  !> @brief Store note
    !!
    !!  Subroutine to store a note for printing at the end of the simulation.
    !!
  !<
  subroutine store_note(note)
    ! -- modules
    use ArrayHandlersModule, only: ExpandArray
    ! -- dummy variables
    character(len=*), intent(in) :: note !< note
    !
    ! -- store note
    call sim_notes%store_message(note)
    !
    ! -- return
    return
  end subroutine store_note

  !> @brief Stop the simulation.
    !!
    !!  Subroutine to stop the simulations with option to print message
    !!  before stopping with the active error code.
    !!
  !<
  subroutine ustop(stopmess, ioutlocal)
    ! -- dummy variables
    character, optional, intent(in) :: stopmess * (*) !< optional message to print before
                                                      !! stopping the simulation
    integer(I4B), optional, intent(in) :: ioutlocal !< optional output file to
                                                    !! final message to
    !
    ! -- print the final message
    call print_final_message(stopmess, ioutlocal)
    !
    ! -- return appropriate error codes when terminating the program
    call stop_with_error(ireturnerr)

  end subroutine ustop

  !> @brief Print the final messages
    !!
    !!  Subroutine to print the notes, warnings, errors and the final message (if passed).
    !!  The subroutine also closes all open files.
    !!
  !<
  subroutine print_final_message(stopmess, ioutlocal)
    ! -- dummy variables
    character, optional, intent(in) :: stopmess * (*) !< optional message to print before
                                                      !! stopping the simulation
    integer(I4B), optional, intent(in) :: ioutlocal !< optional output file to
                                                    !! final message to
    ! -- local variables
    character(len=*), parameter :: fmt = '(1x,a)'
    character(len=*), parameter :: msg = 'Stopping due to error(s)'
    !
    ! -- print the accumulated messages
    call sim_notes%print_message('NOTES:', 'note(s)', &
                                 iunit=iout, level=VALL)
    call sim_warnings%print_message('WARNING REPORT:', 'warning(s)', &
                                    iunit=iout, level=VALL)
    call sim_errors%print_message('ERROR REPORT:', 'error(s)', iunit=iout)
    call sim_uniterrors%print_message('UNIT ERROR REPORT:', &
                                      'file unit error(s)', iunit=iout)
    !
    ! -- write a stop message, if one is passed
    if (present(stopmess)) then
      if (stopmess .ne. ' ') then
        call sim_message(stopmess, fmt=fmt, iunit=iout)
        call sim_message(stopmess, fmt=fmt)
        if (present(ioutlocal)) then
          if (ioutlocal > 0 .and. ioutlocal /= iout) then
            write (ioutlocal, fmt) trim(stopmess)
            close (ioutlocal)
          end if
        end if
      end if
    end if
    !
    ! -- determine if an error condition has occurred
    if (sim_errors%count_message() > 0) then
      ireturnerr = 2
      if (present(ioutlocal)) then
        if (ioutlocal > 0 .and. ioutlocal /= iout) write (ioutlocal, fmt) msg
      end if
    end if
    !
    ! -- close all open files
    call sim_closefiles()
    !
    ! -- return
    return
  end subroutine print_final_message

  !> @brief Reset the simulation convergence flag
    !!
    !!  Subroutine to reset the simulation convergence flag.
    !!
  !<
  subroutine converge_reset()
    ! -- modules
    use SimVariablesModule, only: isimcnvg
    !
    ! -- reset simulation convergence flag
    isimcnvg = 1
    !
    ! -- return
    return
  end subroutine converge_reset

  !> @brief Simulation convergence check
    !!
    !!  Subroutine to check simulation convergence. If the continue option is
    !!  set the simulation convergence flag is set to True if the simulation
    !!  did not actually converge for a time step and the non-convergence counter
    !!  is incremented.
    !!
  !<
  subroutine converge_check(hasConverged)
    ! -- modules
    use SimVariablesModule, only: isimcnvg, numnoconverge, isimcontinue
    ! -- dummy variables
    logical, intent(inout) :: hasConverged !< boolean indicting if the
                                           !! simulation is considered converged
    ! -- format
    character(len=*), parameter :: fmtfail = &
      "(1x, 'Simulation convergence failure.', &
      &' Simulation will terminate after output and deallocation.')"
    !
    ! -- Initialize hasConverged to True
    hasConverged = .true.
    !
    ! -- Count number of failures
    if (isimcnvg == 0) then
      numnoconverge = numnoconverge + 1
    end if
    !
    ! -- Continue if 'CONTINUE' specified in simulation control file
    if (isimcontinue == 1) then
      if (isimcnvg == 0) then
        isimcnvg = 1
      end if
    end if
    !
    ! -- save simulation failure message
    if (isimcnvg == 0) then
      call sim_message('', fmt=fmtfail, iunit=iout)
      hasConverged = .false.
    end if
    !
    ! -- return
    return
  end subroutine converge_check

  !> @brief Print the header and initializes messaging
    !!
    !! Subroutine that prints the initial message and initializes the notes,
    !! warning messages, unit errors, and error messages.
    !!
  !<
  subroutine initial_message()
    ! -- modules
    use VersionModule, only: write_listfile_header
    !
    ! -- initialize message lists
    call sim_errors%init_message()
    call sim_uniterrors%init_message()
    call sim_warnings%init_message()
    call sim_notes%init_message()
    !
    ! -- Write banner to screen (unit stdout)
    call write_listfile_header(istdout, write_kind_info=.false., &
                               write_sys_command=.false.)
    !
  end subroutine initial_message

  !> @brief Create final message
    !!
    !! Subroutine that creates the appropriate final message and
    !! terminates the program with an error message, if necessary.
    !!
  !<
  subroutine final_message()
    ! -- modules
    use SimVariablesModule, only: isimcnvg, numnoconverge, ireturnerr, &
                                  isimcontinue
    ! -- formats
    character(len=*), parameter :: fmtnocnvg = &
      &"(1x, 'Simulation convergence failure occurred ', i0, ' time(s).')"
    !
    ! -- Write message if nonconvergence occured in at least one timestep
    if (numnoconverge > 0) then
      write (warnmsg, fmtnocnvg) numnoconverge
      if (isimcontinue == 0) then
        call sim_errors%store_message(warnmsg)
      else
        call sim_warnings%store_message(warnmsg)
      end if
    end if
    !
    ! -- write final message
    if (isimcnvg == 0) then
      call print_final_message('Premature termination of simulation.', iout)
    else
      call print_final_message('Normal termination of simulation.', iout)
    end if
    !
    ! -- If the simulation did not converge and the continue
    !    option was not set, then set the return code to 1.  The
    !    purpose of setting the returncode this way is that the
    !    program will terminate without a stop code if the simulation
    !    reached the end and the continue flag was set, even if the
    !    the simulation did not converge.
    if (isimcnvg == 0 .and. isimcontinue == 0) then
      ireturnerr = 1
    end if
    !
    ! -- destroy messages
    call sim_errors%deallocate_message()
    call sim_uniterrors%deallocate_message()
    call sim_warnings%deallocate_message()
    call sim_notes%deallocate_message()
    !
    ! -- return or halt
    if (iforcestop == 1) then
      call stop_with_error(ireturnerr)
    end if

  end subroutine final_message

  !> @brief Close all open files
    !!
    !! Subroutine that closes all open files at the end of the simulation.
    !!
  !<
  subroutine sim_closefiles()
    ! -- modules
    ! -- dummy
    ! -- local variables
    integer(I4B) :: i
    logical :: opened
    character(len=7) :: output_file
    !
    ! -- close all open file units
    do i = iustart, iunext - 1
      !
      ! -- determine if file unit i is open
      inquire (unit=i, opened=opened)
      !
      ! -- skip file units that are no longer open
      if (.not. opened) then
        cycle
      end if
      !
      ! -- flush the file if it can be written to
      inquire (unit=i, write=output_file)
      if (trim(adjustl(output_file)) == 'YES') then
        flush (i)
      end if
      !
      ! -- close file unit i
      close (i)
    end do
    !
    ! -- return
    return
  end subroutine sim_closefiles

end module SimModule
