
subroutine f107(filename)
  use ModF107, only: read_f107

  character(*), intent(in) :: filename

  integer, parameter :: iF107 = 1
  type(TimeType), dimension(nIndexValuesMax) :: times
  real, dimension(nIndexValuesMax) :: values_TMP
  integer :: nPts, i

  call read_f107(filename, times, values_TMP)

  if (.not. isOk) return

  ! Determine how many valid values we have
  nPts = nIndexValuesMax
  do i = 1, nIndexValuesMax
    if (values_TMP(i) == iBadValue) then
      nPts = i - 1
      exit
    endif
  enddo

  call set_index(iF107, times(1:nPts), values_TMP(1:nPts), nPts)

end subroutine f107
