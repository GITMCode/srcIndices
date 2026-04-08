program testIO

  use ModTimeIO
  use ModIndices
  use ModErrors
  use ModKind

  type(TimeType) :: now
  real(Real8_) :: JD
  integer :: iIndex
  character(50) :: indexName
  real :: f107val, f107aval
  real :: ae, au, al
  integer :: iMM, iDD, iAL


  print*, "Beginning main..."

  print*, "> setting time to 2011-03-16 00:12:15.0"
  now%iYear = 2011
  now%iMonth = 3
  now%iDay = 16
  now%iHour = 0
  now%iMinute = 12
  now%iSecond = 15
  print*, now

  print*, "> converting to real"
  call time_int_to_real(now)
  print*, now
  print*, "> current 'time' is: ", now%String
  print*, "-------------"
  print*, ""
  print*, '> Testing (incorrect) usage of day of year conversion'
  print*, '> This should not work...'
  now%iMonth = 2
  now%iDay = 160
  call time_int_to_real(now)
  print*, now

  call report_errors()

  if (isOk) then
    print*, "Everything is ok. It shouldn't be!"
  else
    print*, "Correctly found the time error. Nice!!"
    call flush_errors
  endif

  print*, "-------------"
  print*, ""
  print*, "> Testing day of year:"

  now%iMonth = 1
  now%iDay = 160
  call time_int_to_real(now)
  print*, now

  print*, "-------------"
  print*, ""
  print*, "> What (internal) number is f107??:  ",  decode_index('f107')

  print*, "> What variable is #1??:   ", trim(decode_index(1))

  call report_errors

  print*, "------------------------"
  print*, ""
  print*, "> Reading f107 file..."
  call init_f107("data/f107.txt")
  call report_errors

  if (.not. isOk) stop

  print*, "> f107 file read successfully!"
  print*, "> Number of f107 values: ", allIndices(1)%nValues
  print*, ">> Number of f107a values: ", allIndices(2)%nValues

  ! Test get_index with explicit time
  print*, ""
  print*, "> Testing get_index with explicit time..."
  now%iYear = 2011
  now%iMonth = 3
  now%iDay = 16
  now%iHour = 12
  now%iMinute = 0
  now%iSecond = 0
  call time_int_to_real(now)

  call get_index(1, now%Time, f107val)
  print*, ">> f107 at 2011-03-16 12:00 = ", f107val

  ! Test set_time + get_index without time
  print*, ""
  print*, "> Testing set_time + get_index..."
  call set_time(now%Time)
  call get_index(1, f107val)
  
  print*, ">> f107 (via set_time)    = ", f107val


  ! Test set_time + get_index without specifying time in call
  print*, ""
  print*, "> Testing set_time + get_index with chars..."
  call set_time(now%Time)
  call get_index("f107", f107val)

  print*, ">> f107 (via chars & set_time)    = ", f107val


  print*, ""
  print*, "> Testing get_index with manual time & chars..."
  call get_index("f107", now%time, f107val)
  print*, ">> f107 (via chars & real-time)    = ", f107val


  print*, ""
  print*, "> Getting the last few times in the F107 file. Verify these!"
  ! reset time
  now%iYear = 2025
  now%iHour = 0
  now%iMinute = 0
  now%iSecond = 0
  print*, "================================="
  write(*, '(A20, A10, A10)') "time", "f107", "f107a"
  do iMM=3,5
    do iDD=1,30,15
      now%iMonth=iMM
      now%iDay=iDD
      call set_time(now)
      call get_index("f107", f107val)
      call get_index("f107a", f107aval)
      write(*, '(A20, F10.1, F10.1)') now%String, f107val, f107aval
    enddo
  enddo

  print*, ""
  print*, " > here's a check for data we should not have"
  call get_index("ae", ae)
  print*, "AE= ", ae, " ... isOK:", isOk
 call flush_errors

  
  print*, ""
  print*, " >OK cool now read the SME data & look at it"
  call init_ae("data/ae20021221.dat")
  ! Check how many pts were read in
  iAL = decode_index('al')
  print*, "nPts AL = ", allIndices(iAL)%nValues, " ... isOK:", isOk
  now %iYear = 2002
  now%iMonth=12
  now%iDay=21
  now%iHour = 23
  now%iSecond = 0
  now%fracSecond = 0.0d0
  do iMM=50,59
    now%iMinute=iMM
    call set_time(now)
    call get_index('ae', ae)
    call get_index('al', al)
    call get_index('au', au)
    print*, "min:", now%iMinute, 'ae', ae, 'al', al, 'au', au
  enddo

  print*, ""
  call report_warnings
  call report_errors


end program