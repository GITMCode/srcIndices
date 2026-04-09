
! This has both the capability to read sme & the onset files. Call the appropriate one

!==============================================================================

module ModAE

  use ModTimeConvert
  use ModErrors
  use ModIoUnit
  use ModKind

contains

  subroutine read_sme(filename, times, ae, au, al)

    implicit none

    character(len=*), intent(in) :: filename
    type(TimeType), dimension(nIndexValuesMax), intent(out) :: times
    real, dimension(nIndexValuesMax), intent(out) :: ae, au, al

    integer :: ierror, j, npts, ioUnit, iPt
    logical :: IsDone
    ! One line of input
    character(len=200) :: line

    real(Real8_) :: TimeDelay, BufferTime = 180.0
    real(Real8_) :: aeval, auval, alval

    type(TimeType) :: iTime
    !------------------------------------------------------------------------

    IsDone = .false.

    npts = 0
    iPt = 1
    TimeDelay = 0.0

    ! "Zero" everything
    ae = rBadValue
    au = rBadValue
    al = rBadValue
    iTime%FracSecond = 0.0d0
    iTime%time = 0.0d0

    ioUnit = io_unit_new()

    open(ioUnit, file=filename, status="old", iostat=ierror)

    ! Test the type of file, skip header lines
    read(ioUnit, *, iostat=iError) line
    if (line(1:4) == "File") then
      IsDone = .false.
      do while (.not. IsDone)
        read(ioUnit, *, iostat=iError) line
        if (iError /= 0) IsDone = .true.
        if (line(1:6) == "<year>") IsDone = .true.
      enddo
      IsDone = .false.
    else
      rewind(ioUnit)
    endif

    if (ierror .ne. 0) then
      call set_error("(read_sme) - Error reading "//filename)
      return
    endif

    do while (.not. IsDone)

      read(ioUnit, *, iostat=iError) &
        iTime%iYear, iTime%iMonth, iTime%iDay, itime%iHour, iTime%iMinute, iTime%iSecond, &
        aeval, &
        alval, &
        auval

      if (ierror /= 0) then
        IsDone = .true.

      else

        call time_int_to_real(iTime)
        times(iPt) = iTime
        ae(iPt) = aeval
        au(iPt) = auval
        al(iPt) = alval

        iPt = iPt + 1

        if (iPt > nIndexValuesMax) call set_error("(read_sme) Maximum number of values read!")

      endif

    enddo

    close(ioUnit)

  end subroutine read_sme

! ===========================  !

! subroutine read_al_onset_list(iOutputError, StartTime, EndTime)

! end subroutine read_al_onset_list

!==============================================================================
end module ModAE
