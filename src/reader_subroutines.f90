
subroutine f107(filename)
  use ModF107, only: read_f107

  character(*), intent(in) :: filename

  integer :: iF107, iF107a
  type(TimeType), dimension(nIndexValuesMax) :: times
  real, dimension(nIndexValuesMax) :: values_TMP
  real, dimension(:), allocatable :: f107a_vals
  type(TimeType), dimension(:), allocatable :: f107a_times
  
  integer :: nPts, i, nPtsa

  call read_f107(filename, times, values_TMP)

  if (.not. isOk) return

  ! Determine how many valid values we have
  nPts = nIndexValuesMax
  do i = 1, nIndexValuesMax
    if (values_TMP(i) == rBadValue) then
      nPts = i - 1
      exit
    endif
  enddo

  ! Set F107 values...
  iF107 = decode_index("f107")
  call set_index(iF107, times(1:nPts), values_TMP(1:nPts), nPts)

  ! Now we need to make the F107a values
  nPtsa = nPts-80
  allocate(f107a_times(nPtsa), f107a_vals(nPtsa))
  do i=41, nPts-40
    f107a_times(i-40) = times(i)
    f107a_vals(i-40) = sum(values_TMP(i-40:i+40)) / 81.0
  enddo
  iF107a = decode_index("f107a")
  call set_index(iF107a, f107a_times(1:nPtsa), f107a_vals(1:nPtsa), nPtsa)

end subroutine f107
