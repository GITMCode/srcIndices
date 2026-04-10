module ModHPI

  use ModIndConsts
  use ModIoUnit
  use ModTimeConvert
  use ModErrors
  use ModKind

  implicit none
contains

  subroutine read_NOAA_HPI(filename, times, hpi)

    character(*), intent(in) :: filename
    type(TimeType), dimension(nIndexValuesMax), intent(out) :: times
    real, dimension(nIndexValuesMax), intent(out) :: hpi

    character(len=200) :: line
    integer :: iPt, yr
    integer :: IOUnit_, iError, datatype

    ioUnit_ = io_unit_new()

    open(IOUnit_, file=filename, status="old", iostat=iError)
    if (iError .ne. 0) then
      call set_error("(read_NOAA_HPI) File could not be opened "//trim(filename))
      return
    endif

    times(:)%iYear = 0
    times(:)%iMonth = 0
    times(:)%iDay = 0
    times(:)%iHour = 0
    times(:)%iMinute = 0
    times(:)%iSecond = 0
    times(:)%fracSecond = 0.0d0
    hpi = rBadValue

    ! Header scan: find "Normalizing factor" to pick datatype.
    ! If EOF hits first, default to modern format and rewind so row 1 isn't lost.
    datatype = 2
    do
      read(IOUnit_, '(a)', iostat=iError) line

      if (index(line, 'Normalizing factor') > 0) then
        if (index(line, 'F8.3') > 0) datatype = 1
        if (index(line, 'F7.2') > 0) datatype = 2
        exit
      endif
    enddo

    read(IOUnit_, '(a)', iostat=iError) line

    iPt = 1
    do
      if (iPt > nIndexValuesMax) exit

      if (datatype .eq. 1) then
        ! OLD NOAA HPI FILES
        if (iPt == 1) read(IOUnit_, '(i4)', iostat=iError) yr
        if (iError /= 0) exit
        times(iPt)%iYear = yr
        times(iPt)%iMonth = 1
        read(IOUnit_, '(a10,f3.0,f2.0,f2.0,f8.1)', iostat=iError) &
          line, times(iPt)%iDay, times(iPt)%iHour, times(iPt)%iMinute, hpi(iPt)
        if (iError /= 0) exit
        if (times(iPt)%iDay < 1) exit
      endif

      if (datatype .eq. 2) then
        ! NEW NOAA HPI FILES
        read(IOUnit_, '(i4,a1,i2,a1,i2,a1,i2,a1,i2,a1,i2,a15,f8.1)', iostat=iError) &
          times(iPt)%iYear, line, times(iPt)%iMonth, line, times(iPt)%iDay, line, &
          times(iPt)%iHour, line, times(iPt)%iMinute, line, times(iPt)%iSecond, &
          line, hpi(iPt)
        if (iError /= 0) exit
      endif

      call time_int_to_real(times(iPt))
      iPt = iPt + 1
    enddo

    close(IOUnit_)

  end subroutine read_NOAA_HPI

  subroutine AE_to_HPI(ae, hpi)

    real, dimension(:), intent(in) :: ae
    real, dimension(:), allocatable, intent(out) :: hpi

    integer :: nPtsAE

    nPtsAE = size(ae) ! don't have modIndices available yet so can't: get_nValues('ae')
    if (nPtsAE == 0) then
      call set_error("(AE_to_HPI) - Cannot initialize HPI before AE")
      return
    endif

    allocate(HPI(nPtsAE))

    ! AE -> HP uses formula from Wu et al, 2021
    ! see: https://doi.org/10.1029/2020SW002629
    hpi = 0.102*ae + 8.953

  end subroutine AE_to_HPI

end module ModHPI
