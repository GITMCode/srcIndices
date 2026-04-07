program testIO

  use ModTimeIO
  use ModIndices
  use ModErrors
  use ModKind

  type(TimeType) :: now
  real(Real8_) :: JD
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



  call report_errors

end program