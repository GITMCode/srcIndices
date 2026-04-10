
  subroutine set_time_real(timeIn)
    real(Real8_), intent(in) :: timeIn
    currentTime%Time = timeIn
    call time_real_to_int(currentTime)
  end subroutine set_time_real

  subroutine set_time_type(timeIn)
    type(TimeType), intent(inout) :: timeIn
    call time_int_to_real(timeIn)
    currentTime = timeIn
  end subroutine set_time_type

  subroutine set_time_components(iYear, iMonth, iDay, iHour, iMinute, iSecond)
    integer, intent(in) :: iYear, iMonth, iDay
    integer, intent(in), optional :: iHour, iMinute, iSecond
    currentTime%iYear = iYear
    currentTime%iMonth = iMonth
    currentTime%iDay = iDay
    currentTime%iHour = 0
    currentTime%iMinute = 0
    currentTime%iSecond = 0
    currentTime%FracSecond = 0.0d0
    if (present(iHour)) currentTime%iHour = iHour
    if (present(iMinute)) currentTime%iMinute = iMinute
    if (present(iSecond)) currentTime%iSecond = iSecond
    call time_int_to_real(currentTime)
  end subroutine set_time_components
