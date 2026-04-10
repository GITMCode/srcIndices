program testIO

  use ModTimeConvert
  use ModIndices
  use ModErrors
  use ModKind

  type(TimeType) :: now
  real(Real8_) :: JD
  integer :: iIndex
  character(50) :: indexName
  real :: f107val, f107aval
  real :: ae, au, al, bx, by, bz, vx, den, t, hpi, hpin
  integer :: iMM, iDD, iAL, iBz

  print *, "Beginning main..."

  ! ================================================ !
  ! Time testing
  ! ================================================ !

  print *, "> setting time to 2011-03-16 00:12:15.0"
  now%iYear = 2011
  now%iMonth = 3
  now%iDay = 16
  now%iHour = 0
  now%iMinute = 12
  now%iSecond = 15
  print *, now

  print *, "> converting to real"
  call time_int_to_real(now)
  print *, now
  print *, "> current 'time' is: ", now%String
  print *, "-------------"
  print *, ""
  print *, '> Testing (incorrect) usage of day of year conversion'
  print *, '> This should not work...'
  now%iMonth = 2
  now%iDay = 160
  call time_int_to_real(now)
  print *, now

  call report_errors()

  if (isOk) then
    print *, "Everything is ok. It shouldn't be!"
  else
    print *, "Correctly found the time error. Nice!!"
    call flush_errors
  endif

  print *, "-------------"
  print *, ""
  print *, "> Testing day of year:"

  now%iMonth = 1
  now%iDay = 160
  call time_int_to_real(now)
  print *, now

  print *, "-------------"
  print *, ""
  print *, "> What (internal) number is f107??:  ", decode_index('f107')

  print *, "> What variable is #1??:   ", trim(decode_index(1))

  call report_errors

  ! ================================================ !
  ! Reading & retrieving actual indices
  ! ================================================ !
  ! This is testing and demonstrating the available methods
  ! Below this block is an example of how the library could actually be used

  ! F107 first...
  print *, "------------------------"
  print *, ""
  print *, "> Reading f107 file..."
  call init_f107("data/f107.txt")
  ! Good idea to make sure there are no errors after init
  ! isOk = F if anything has errored
  if (.not. isOk) then
    call report_errors
    stop
  endif

  print *, "> f107 file read successfully!"
  print *, "> Number of f107 values: ", allIndices(1)%nValues
  print *, ">> Number of f107a values: ", allIndices(2)%nValues

  ! Test get_index with explicit time
  print *, ""
  print *, "> Testing get_index with explicit time..."
  now%iYear = 2011
  now%iMonth = 3
  now%iDay = 16
  now%iHour = 12
  now%iMinute = 0
  now%iSecond = 0
  call time_int_to_real(now)

  call get_index(1, now%Time, f107val)
  print *, ">> f107 at 2011-03-16 12:00 = ", f107val

  ! set_time, then get_index without specifying time
  print *, ""
  print *, "> Testing set_time + get_index..."
  call set_time(now%Time)
  call get_index(1, f107val)

  print *, ">> f107 (via set_time)    = ", f107val

  ! Can retrieve indices with str or integers
  print *, ""
  print *, "> Testing set_time + get_index with chars..."
  call set_time(now%Time)
  call get_index("f107", f107val)

  print *, ">> f107 (via chars & set_time)    = ", f107val

  ! For completeness, can also specify str & time in call to get_index
  print *, ""
  print *, "> Testing get_index with manual time & chars..."
  call get_index("f107", now%Time, f107val)
  print *, ">> f107 (via chars & real-time)    = ", f107val

  ! This is to verify that things are working. Easy to compare with 'tail f107.txt'
  print *, ""
  print *, "> Getting the last few times in the F107 file. Verify these!"
  ! reset time
  now%iYear = 2025
  now%iHour = 0
  now%iMinute = 0
  now%iSecond = 0
  print *, "================================="
  write(*, '(A20, A10, A10)') "time", "f107", "f107a"
  do iMM = 3, 5
    do iDD = 1, 30, 15
      now%iMonth = iMM
      now%iDay = iDD
      call set_time(now)
      call get_index("f107", f107val)
      call get_index("f107a", f107aval)
      write(*, '(A20, F10.1, F10.1)') now%String, f107val, f107aval
    enddo
  enddo

  ! Example of incorrect usage. AE has not been read yet!
  print *, ""
  print *, " > here's a check for data we should not have"
  call get_index("ae", ae)
  print *, "AE= ", ae, " ... isOK:", isOk
  call flush_errors ! Use this sparingly. Will clear stored errors!

  print *, ""
  print *, ""

  ! ================================================ !
  ! Example of actual usage
  ! ================================================ !

  ! Typical use case is to read all the indices, then start the model
  ! Time is set on the module-level so can be used for multiple calls to get_index
  ! isOk will be T if there are no errors, and any error will toggle it to F and persist
  !   so multiple indices can be read without needing to check 'isOk' ifter each.

  ! Read AE and IMF data
  print *, "> Reading AE and IMF data..."
  call init_ae("data/ae20021221.dat")
  call init_imf("data/imf20021221.dat")
  ! Then we'll set HPI to be derived from AE
  iAE = decode_index('ae')
  call init_hpi(allIndices(iAE))
  ! then check if all is ok
  if (isOk) then
    print *, "> Read was successful!"
  else
    print *, " -->> Read was unseccessful."
    call report_errors
    stop
  endif

  ! Check how many pts were read in. Could check any index, really...
  iAL = decode_index('al')
  print *, "> nPts AL = ", allIndices(iAL)%nValues, " ... isOK:", isOk
  print *, ''
  print *, ''

  print *, " > Table of a few things read in:"

  ! header
  write(*, "(A19, 11(A10))") 'time', 'IMF Bx', 'IMF By', 'IMF Bz', &
    'SW Vx', 'SW den', 'SW Temp', &
    'AE', 'AU', 'AL', 'HPI', 'HPI_NH'
  ! Set (most of the) date
  now%iYear = 2002
  now%iMonth = 12
  now%iDay = 21
  now%iHour = 23
  now%iSecond = 0
  now%fracSecond = 0.0d0
  do iMM = 50, 59
    ! Set minute....
    now%iMinute = iMM
    ! Tell library what time it is
    call set_time(now)
    ! Get the indices we want to print
    call get_index('imfbx', bx)
    call get_index('imfby', by)
    call get_index('imfbz', bz)
    call get_index('swvx', vx)
    call get_index('swn', den)
    call get_index('swt', t)
    call get_index('ae', ae)
    call get_index('au', au)
    call get_index('al', al)
    call get_index('hpi', hpi)
    call get_index('hpin', hpin)
    write(*, "(A19, 11(F10.1))") now%String, bx, by, bz, vx, den, t, ae, au, al, hpi, hpin
  enddo

  print *, ""
  call report_warnings
  call report_errors

end program
