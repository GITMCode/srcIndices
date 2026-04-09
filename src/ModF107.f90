
Module ModF107

  use ModTimeConvert
  use ModErrors
  use ModIoUnit

contains
  subroutine read_f107(filename, times, values)

    implicit none

    character(len=*), intent(in) :: filename
    type(TimeType), dimension(nIndexValuesMax), intent(out) :: times
    real, dimension(nIndexValuesMax), intent(out) :: values

    integer :: iError, iPt, ioUnit
    type(TimeType) :: iTime
    character(len=200) :: line
    real :: f107val
    logical :: done

    values = rBadValue
    done = .false.
    ioUnit = io_unit_new()

    open(ioUnit, file=filename, status="old", iostat=iError)
    if (iError /= 0) then
      call set_error("F107 reading was unsuccessful!")
      return
    endif

    ! Skip header lines (lines starting with #)
    do
      read(ioUnit, '(A)', iostat=iError) line
      if (iError /= 0) then
        call set_error("F107: unexpected end of file in header")
        close(ioUnit)
        return
      endif
      if (line(1:1) /= '#') exit
    enddo

    ! First non-header line is already in 'line', parse it
    ! Format: yyyy-MM-dd HH:mm  value  qualifier  description
    iPt = 1
    do while (.not. done)
      iTime%Time = 0.0d0
      iTime%iHour = 0
      iTime%iMinute = 0
      iTime%iSecond = 0
      iTime%FracSecond = 0.0d0

      read(line, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,F7.0)', iostat=iError) &
        iTime%iYear, iTime%iMonth, iTime%iDay, &
        iTime%iHour, iTime%iMinute, f107val

      if (iError /= 0) then
        done = .true.
      else
        call time_int_to_real(iTime)
        times(iPt) = iTime
        values(iPt) = f107val
        iPt = iPt + 1
        if (iPt > nIndexValuesMax) call set_error("(read_f107) Maximum number of values read!")
      endif

      if (.not. done) then
        read(ioUnit, '(A)', iostat=iError) line
        if (iError /= 0) done = .true.
      endif
    enddo

    close(ioUnit)

  end subroutine read_f107

end module ModF107
