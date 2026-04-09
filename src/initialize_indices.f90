
subroutine init_f107(filename)
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
  nPtsa = nPts - 80
  allocate(f107a_times(nPtsa), f107a_vals(nPtsa))
  do i = 41, nPts - 40
    f107a_times(i - 40) = times(i)
    f107a_vals(i - 40) = sum(values_TMP(i - 40:i + 40))/81.0
  enddo
  iF107a = decode_index("f107a")
  call set_index(iF107a, f107a_times(1:nPtsa), f107a_vals(1:nPtsa), nPtsa)

end subroutine init_f107

subroutine init_ae(filename)
  use ModAE, only: read_sme

  character(*), intent(in) :: filename

  integer :: iPt, iAE, iAU, iAL
  type(TimeType), dimension(nIndexValuesMax) :: times
  real, dimension(nIndexValuesMax) :: ae_tmp, au_tmp, al_tmp
  real, dimension(:), allocatable :: ae_vals, au_vals, al_vals

  integer :: nPts, i

  call read_sme(filename, times, ae_tmp, au_tmp, al_tmp)

  if (.not. isOk) return

  ! Determine how many valid values we have
  nPts = nIndexValuesMax
  do i = 1, nIndexValuesMax
    if (ae_tmp(i) == rBadValue) then
      nPts = i - 1
      exit
    endif
  enddo

  ! Set AE, AL, AU values...
  iAE = decode_index("ae")
  iAU = decode_index("au")

  iAL = decode_index("al")
  call set_index(iAE, times(1:nPts), ae_tmp(1:nPts), nPts)
  call set_index(iAU, times(1:nPts), au_tmp(1:nPts), nPts)
  call set_index(iAL, times(1:nPts), al_tmp(1:nPts), nPts)

end subroutine init_ae

subroutine init_imf(filename)

    use ModIMF, only: read_omni

    character(*), intent(in) :: filename

    integer :: iPt, iBx, iBy, iBz, iVx, iVy, iVz, iDen, iT
    type(TimeType), dimension(nIndexValuesMax) :: times
    real, dimension(nIndexValuesMax) :: bx_tmp, by_tmp, bz_tmp
    real, dimension(nIndexValuesMax) :: vx_tmp, vy_tmp, vz_tmp
    real, dimension(nIndexValuesMax) :: den_tmp, temp_tmp

    integer :: nPts, i

    call read_omni(filename, times, &
                   bx_tmp, by_tmp, bz_tmp, &
                   vx_tmp, vy_tmp, vz_tmp, &
                   den_tmp, temp_tmp)

    if (.not. isOk) return

    ! Determine how many valid values we have
    nPts = nIndexValuesMax
    do i = 1, nIndexValuesMax
      if (bz_tmp(i) == rBadValue) then
        nPts = i - 1
        exit
      endif
    enddo

    ! Set F107 values...
    iBx = decode_index("imfbx")
    call set_index(iBx, times(1:nPts), bx_tmp(1:nPts), nPts)
    iBy = decode_index("imfby")
    call set_index(iBy, times(1:nPts), by_tmp(1:nPts), nPts)
    iBz = decode_index("imfbz")
    call set_index(iBz, times(1:nPts), bz_tmp(1:nPts), nPts)
    
    iVx = decode_index("swvx")
    call set_index(iVx, times(1:nPts), vx_tmp(1:nPts), nPts)
    iVy = decode_index("swvy")
    call set_index(iVy, times(1:nPts), vy_tmp(1:nPts), nPts)
    iVz = decode_index("swvz")
    call set_index(iVz, times(1:nPts), vz_tmp(1:nPts), nPts)

    iDen = decode_index("swn")
    call set_index(iDen, times(1:nPts), den_tmp(1:nPts), nPts)
    iT = decode_index("swt")
    call set_index(iT, times(1:nPts), temp_tmp(1:nPts), nPts)

  end subroutine init_imf
