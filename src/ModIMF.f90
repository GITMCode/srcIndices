
module ModIMF

  use ModIndConsts
  use ModTimeConvert
  use ModErrors
  use ModIoUnit
  use ModKind

  implicit none

contains

  subroutine read_omni(filename, times, bx, by, bz, vx, vy, vz, n, t)

    character(len=*), intent(in) :: filename
    type(TimeType), dimension(nIndexValuesMax), intent(out) :: times
    real, dimension(nIndexValuesMax), intent(out) :: bx, by, bz, vx, vy, vz, n, t

    logical :: done, done_inner, IsFirstLine = .true.
    character(len=200) :: line

    real(Real8_) :: TimeDelay
    type(TimeType) :: itime
    integer :: iError, IOUnit_, iPt

    iError = 0

    done = .false.

    TimeDelay = 0.0

    bx = rBadValue
    by = rBadValue
    bz = rBadValue
    vx = rBadValue
    vy = rBadValue
    vz = rBadValue
    n = rBadValue
    t = rBadValue

    IOUnit_ = io_unit_new()
    open(IOUnit_, file=trim(filename), status="old", iostat=iError)
    if (ierror .ne. 0) then
      call set_error("(read_omni) File could not be opened "//trim(filename))
      return
    endif

    do while (.not. done)
      read(IOUnit_, '(a)', iostat=ierror) line
      if (ierror /= 0) done = .true.

      if (index(line, '#DELAY') > 0) then
        read(IOUnit_, *, iostat=iError) TimeDelay
        if (iError /= 0) done = .true.
      endif

      if (index(line, '#START') > 0) then

        done_inner = .false.

        iPt = 1

        do while (.not. done_inner)

          read(IOUnit_, *, iostat=iError) &
            iTime%iYear, iTime%iMonth, iTime%iDay, &
            iTime%iHour, iTime%iMinute, iTime%iSecond, &
            iTime%FracSecond, &
            bx(iPt), &
            by(iPt), &
            bz(iPt), &
            vx(iPt), &
            vy(iPt), &
            vz(iPt), &
            n(iPt), &
            t(iPt)

          if (iError /= 0) then
            ! done reading the file
            done_inner = .true.
          else
            iTime%FracSecond = iTime%FracSecond/1000.0
            ! Convert time to real
            call time_int_to_real(iTime)

            iTime%Time = iTime%Time + TimeDelay
            call time_real_to_int(iTime)
            times(iPt) = iTime

            iPt = iPt + 1
          endif

          if (iPt > nIndexValuesMax) call set_error("(read_omni) Maximum number of values read!")

        enddo

        done = done_inner
      endif
    enddo

    close(IOUnit_)

  end subroutine read_omni

end module ModIMF
