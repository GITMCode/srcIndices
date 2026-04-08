
  subroutine set_index(iIndex, times, values, nPts)
    integer, intent(in) :: iIndex, nPts
    type(TimeType), dimension(nPts), intent(in) :: times
    real, dimension(nPts), intent(in) :: values

    if (iIndex < 1 .or. iIndex > nValidIndices) then
      call set_error("set_index: invalid index ID")
      return
    endif

    if (allocated(allIndices(iIndex)%value)) &
      deallocate(allIndices(iIndex)%value)
    if (allocated(allIndices(iIndex)%time)) &
      deallocate(allIndices(iIndex)%time)

    allocate(allIndices(iIndex)%value(nPts))
    allocate(allIndices(iIndex)%time(nPts))

    allIndices(iIndex)%iIndex = iIndex
    allIndices(iIndex)%nValues = nPts
    allIndices(iIndex)%value = values(1:nPts)
    allIndices(iIndex)%time = times(1:nPts)

  end subroutine set_index

  subroutine set_time_real(timeIn)
    real(Real8_), intent(in) :: timeIn
    currentTime = timeIn
  end subroutine set_time_real

  subroutine set_time_type(timeIn)
    type(TimeType), intent(inout) :: timeIn
    call time_int_to_real(timeIn)
    currentTime = timeIn%Time
  end subroutine set_time_type

  subroutine set_time_components(iYear, iMonth, iDay, iHour, iMinute, iSecond)
    integer, intent(in) :: iYear, iMonth, iDay
    integer, intent(in), optional :: iHour, iMinute, iSecond
    type(TimeType) :: timeTmp
    timeTmp%iYear = iYear
    timeTmp%iMonth = iMonth
    timeTmp%iDay = iDay
    timeTmp%iHour = 0
    timeTmp%iMinute = 0
    timeTmp%iSecond = 0
    timeTmp%FracSecond = 0.0d0
    if (present(iHour)) timeTmp%iHour = iHour
    if (present(iMinute)) timeTmp%iMinute = iMinute
    if (present(iSecond)) timeTmp%iSecond = iSecond
    call time_int_to_real(timeTmp)
    currentTime = timeTmp%Time
  end subroutine set_time_components

  ! get_index with explicit time
  subroutine get_index_int_wtime(iIndex, timeIn, value, iError)
    integer, intent(in) :: iIndex
    real(Real8_), intent(in) :: timeIn
    real, intent(out) :: value
    integer, intent(out) :: iError

    integer :: iMin, iMax, iCenter, nVals
    real :: DtNorm
    logical :: IsFound

    iError = 0
    value = -1.0e32

    if (iIndex < 1 .or. iIndex > nValidIndices) then
      iError = 1
      return
    endif

    nVals = allIndices(iIndex)%nValues

    if (nVals == 0) then
      iError = 2
      return
    endif

    ! Single value = constant
    if (nVals == 1) then
      value = allIndices(iIndex)%value(1)
      return
    endif

    ! Before first time: clamp to first value
    if (timeIn <= allIndices(iIndex)%time(1)%Time) then
      value = allIndices(iIndex)%value(1)
      if ((allIndices(iIndex)%time(1)%Time - timeIn) > &
          5.0d0 * (allIndices(iIndex)%time(2)%Time - &
                   allIndices(iIndex)%time(1)%Time)) then
        iError = 3
      endif
      return
    endif

    ! After last time: clamp to last value
    if (timeIn >= allIndices(iIndex)%time(nVals)%Time) then
      value = allIndices(iIndex)%value(nVals)
      if ((timeIn - allIndices(iIndex)%time(nVals)%Time) > &
          5.0d0 * (allIndices(iIndex)%time(2)%Time - &
                   allIndices(iIndex)%time(1)%Time)) then
        iError = 3
      endif
      return
    endif

    ! Binary search for bracketing interval
    iMin = 1
    iMax = nVals
    IsFound = .false.

    do while (.not. IsFound)
      iCenter = (iMin + iMax) / 2

      if (iCenter >= iMax .or. iCenter <= iMin) then
        IsFound = .true.
      else
        if (timeIn == allIndices(iIndex)%time(iCenter)%Time) then
          iMin = iCenter
          iMax = iCenter
        else if (timeIn < allIndices(iIndex)%time(iCenter)%Time) then
          iMax = iCenter
        else
          iMin = iCenter
        endif
      endif
    enddo

    ! Linear interpolation
    if (iMin == iMax) then
      value = allIndices(iIndex)%value(iCenter)
    else
      DtNorm = 1.0 - real(allIndices(iIndex)%time(iMax)%Time - timeIn) / &
               real(allIndices(iIndex)%time(iMax)%Time - &
                    allIndices(iIndex)%time(iMin)%Time + 1.0d-6)
      value = DtNorm * allIndices(iIndex)%value(iMax) + &
              (1.0 - DtNorm) * allIndices(iIndex)%value(iMin)
    endif

  end subroutine get_index_int_wtime

  ! get_index using stored currentTime
  subroutine get_index_int_wotime(iIndex, value, iError)
    integer, intent(in) :: iIndex
    real, intent(out) :: value
    integer, intent(out) :: iError

    if (currentTime < 0.0d0) then
      iError = 5
      value = -1.0e32
      call set_error("get_index: no time set. Call set_time first.")
      return
    endif

    call get_index_int_wtime(iIndex, currentTime, value, iError)
  end subroutine get_index_int_wotime

  ! get_index from str using stored currentTime
  subroutine get_index_char_wotime(cIndex, value, iError)
    character(*), intent(in) :: cIndex
    real, intent(out) :: value
    integer, intent(out) :: iError
    integer :: iIndex

    if (currentTime < 0.0d0) then
      iError = 5
      value = -1.0e32
      call set_error("get_index: no time set. Call set_time first.")
      return
    endif

    iIndex = decode_index(cIndex)

    call get_index_int_wtime(iIndex, currentTime, value, iError)

  end subroutine get_index_char_wotime

  
  ! get_index from str using stored currentTime
  subroutine get_index_char_wtime(cIndex, timein, value, iError)
    character(*), intent(in) :: cIndex
    real(Real8_), intent(in) :: timeIn
    real, intent(out) :: value
    integer, intent(out) :: iError
    integer :: iIndex

    iIndex = decode_index(cIndex)

    call get_index_int_wtime(iIndex, timein, value, iError)

  end subroutine get_index_char_wtime
