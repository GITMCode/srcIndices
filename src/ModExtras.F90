!  ModExtras.F90 - Standalone module definitions
!
!  When compiling standalone (-DSTANDALONE), this file provides modules
!  that would otherwise be supplied by the host model (e.g., SWMF/GITM).
!  When coupled, the host model can provide
!  ModKind, ModIoUnit, ModErrors, and ModTimeConvert.

!----------------------------------------------------------------------
! ModKind - define various precisions in a machine independent way
!----------------------------------------------------------------------

#if defined(STANDALONE) || defined(NEEDMODKIND)
module ModKind

  implicit none

  integer, parameter :: Real4_ = selected_real_kind(6, 30)
  integer, parameter :: Real8_ = selected_real_kind(12, 100)
  integer, parameter :: Int8_ = selected_int_kind(16)

  ! Number of bytes in the default real number (precision)
  integer, parameter :: nByteReal = 4 + (1.00000000041 - 1.0)*10000000000.0

end module ModKind
#endif

!----------------------------------------------------------------------
! ModIoUnit - general utilities for Fortran I/O units.
!----------------------------------------------------------------------
#if defined(STANDALONE) || defined(NEEDMODIOUNIT)
module ModIoUnit

  implicit none

  private ! except

  !PUBLIC MEMBER FUNCTIONS:

  public :: io_unit_new    ! Return an unused unit number for extended use
  public :: io_unit_clean  ! Close open units, delete empty files

  !PUBLIC DATA MEMBERS:


  integer, parameter, public :: StdIn_ = 5  ! Standard input
  integer, parameter, public :: StdOut_ = 6  ! Standard output

  ! For open read/write close without intervening open
  integer, parameter, public :: UnitTmp_ = 9  ! 1st Temporary unit number
  integer, parameter, public :: UnitTmp2_ = 8  ! 2nd Temporary unit number

  !LOCAL VARIABLES:

  integer, parameter :: MinUnitNumber = 20    ! Smallest allowed unit number
  integer, parameter :: MaxUnitNumber = 1000  ! Largest allowed unit number

  integer :: iUnitMax = UnitTmp_              ! The largest unit number used

  character(len=*), parameter :: NameMod = 'ModIoUnit'

contains

  function io_unit_new() result(iUnit)

    !  Returns a unit number of a unit that exists and is not connected
    integer :: iUnit
    logical :: IsExisting, IsOpened
    integer :: iError

    character(len=*), parameter :: NameSub = NameMod//'::io_unit_new'
    !--------------------------------------------------------------------

    do iUnit = MinUnitNumber, MaxUnitNumber
      inquire( &
        unit=iUnit, &
        exist=IsExisting, &
        opened=IsOpened, &
        iostat=iError)
      if (IsExisting .and. .not. IsOpened .and. iError == 0) then
        iUnitMax = max(iUnitMax, iUnit)
        return
      endif
    enddo

    iUnit = -1

  end function io_unit_new
  !===========================================================================
  subroutine io_unit_clean

    ! Close all open units for this processor
    integer :: iUnit, iError
    logical :: IsOpen
    character(len=100) :: Name
    character :: String
    !------------------------------------------------------------------------
    do iUnit = UNITTMP_, iUnitMax

      inquire(iUnit, OPENED=IsOpen, NAME=Name)
      if (IsOpen) then
        ! Close file so that output is flushed
        close(iUnit)
        ! Try to open file and read 1 character
        open(iUnit, FILE=Name, STATUS='old', IOSTAT=iError)
        if (iError /= 0) CYCLE
        read(iUnit, '(a1)', IOSTAT=iError) String
        if (iError < 0) then
          ! Delete empty files
          close(iUnit, STATUS='delete')
        else
          ! Close file again
          close(iUnit)
        endif
      endif
    enddo

  end subroutine io_unit_clean

end module ModIoUnit
#endif

!----------------------------------------------------------------------
! ModErrors - Error and warning tracking
!----------------------------------------------------------------------

#if defined(STANDALONE) || defined(NEEDMODERRORS)
module ModErrors

  implicit none

  integer, parameter :: nErrorsMax = 1000, nWarningsMax = 1000
  character(len=200), dimension(nErrorsMax) :: cErrorCodes, cWarningCodes

  integer :: nErrors = 0, nWarnings = 0
  logical :: isOk = .true.

contains

  subroutine set_error(cError)
    character(len=*), intent(in) :: cError
    nErrors = nErrors + 1
    cErrorCodes(nErrors) = cError
    isOk = .false.
  end subroutine set_error

  subroutine report_errors()
    integer :: iError
    if (nErrors == 0) write(*, *) "No errors to report!"
    do iError = 1, nErrors
      write(*, *) "--> Error : ", trim(cErrorCodes(iError))
    enddo
  end subroutine report_errors

  subroutine flush_errors()
    integer :: iError
    if (nErrors /= 0) then
      write(*, *) "Clearing", nErrors, "errors!"
      cErrorCodes = [('', iError=1, nErrors)]
      nErrors = 0
      isOk = .true.
    endif
  end subroutine flush_errors

  ! This is for things that should not stop the model, but notify user now & later
  subroutine raise_warning(cWarning)
    character(len=*), intent(in) :: cWarning
    nWarnings = nWarnings + 1
    cWarningCodes(nWarnings) = cWarning
    write(*, *) " -> Warning: ", trim(cWarningCodes(nWarnings))
  end subroutine raise_warning

  subroutine report_warnings()
    integer :: iWarning
    if (nWarnings == 0) write(*, *) "No warnings to report!"
    do iWarning = 1, nWarnings
      write(*, *) "---> Warning : ", trim(cWarningCodes(iWarning))
    enddo
  end subroutine report_warnings

end module ModErrors
#endif

!----------------------------------------------------------------------
! ModTimeConvert - Time type and conversion routines
!----------------------------------------------------------------------

#if defined(STANDALONE) || defined(NEEDMODTIMECONVERT)
module ModTimeConvert

  use ModKind
  use ModErrors, only: set_error

  implicit none

  save

  private ! except

  !PUBLIC TYPES:
  public :: TimeType
  type TimeType
    integer           :: iYear
    integer           :: iMonth
    integer           :: iDay
    integer           :: iHour
    integer           :: iMinute
    integer           :: iSecond
    real(Real8_)      :: FracSecond   ! 0 -> 1
    real(Real8_)      :: Time         ! seconds since base time
    character(len=22) :: String       ! string YYYY-MM-DD HH:MM:SS.ss
  end type TimeType

  public :: n_day_of_year    ! day of year

  public :: time_int_to_real ! Convert integer time info to real
  interface time_int_to_real
    module procedure time_int_to_real1, time_int_to_real2
  end interface

  public :: time_real_to_int ! Convert real time info into integer
  interface time_real_to_int
    module procedure time_real_to_int1, time_real_to_int2
  end interface
  public :: time_real_to_julian, time_int_to_julian

  !PUBLIC DATA MEMBERS:

  ! The earliest year which is already correctly handled
  integer, parameter :: iYearMin = 1965

  ! Time units
  real(Real8_), parameter:: cSecondPerYear = 31536000.0d0
  real(Real8_), parameter:: cSecondPerDay = 86400.0d0
  real(Real8_), parameter:: cSecondPerHour = 3600.0d0
  real(Real8_), parameter:: cSecondPerMinute = 60.0d0

  ! Julian day of YearMin-01-01 UT00:00:
  ! General formula for Julian day may be found in ModTimeConvert:
  real(Real8_), parameter:: JulianDayBase = 367*iYearMin - &
                            ((7*iYearMin)/4) + 1721044.5D0  ! = 0.24387615D+07

  ! February will be adjusted.....
  integer, dimension(1:12), private :: nDayInMonth_I = [ &
                                       31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

contains
  !============================================================================

  logical function is_valid_int_time(Time)
    type(TimeType), intent(inout) :: Time

    ! Check if the integer description of the time is valid.
    ! The year may be corrected, e.g. 66 --> 1966, 03 --> 2003.
    ! Return false if time is not valid.

    !--------------------------------------------------------------------------
    call fix_year(Time%iYear)
    is_valid_int_time = .false.
    !    if(Time % iYear < iYearMin) RETURN
    if (Time%iMonth > 12 .or. Time%iMonth < 1) RETURN
    if (Time%iMonth == 2) call fix_february(Time%iYear)
    if (Time%iDay > nDayInMonth_I(Time%iMonth)) RETURN
    if (Time%iHour < 0 .or. Time%iHour > 23) RETURN
    if (Time%iMinute < 0 .or. Time%iMinute > 59) RETURN
    if (Time%iSecond < 0 .or. Time%iSecond > 59) RETURN
    if (Time%FracSecond < 0.0_Real8_ .or. Time%FracSecond > 1.0_Real8_) &
      RETURN
    is_valid_int_time = .true.

  end function is_valid_int_time
  !============================================================================

  subroutine time_int_to_string(Time)
    type(TimeType), intent(inout) :: Time
    ! Convert integer time info into the string field of the Time variable

    character(len=*), parameter:: NameSub = 'time_int_to_string'
    !--------------------------------------------------------------------------
    if (.not. is_valid_int_time(Time)) then
      write(*, *) NameSub, ': invalid Time = ', Time
      !  call CON_stop(NameSub//' ERROR invalid time')
    endif

    write(Time%String, "(i4.4,'-',i2.2,'-',i2.2,' ',i2.2,':',i2.2,':',i2.2,'.',i2.2)") &
      Time%iYear, Time%iMonth, Time%iDay, &
      Time%iHour, Time%iMinute, Time%iSecond, nint(Time%FracSecond*100.0)

  end subroutine time_int_to_string
  !============================================================================

  subroutine time_int_to_real2(iTime_I, Time)

    integer, intent(in) :: iTime_I(1:7)

    real(real8_), intent(out) :: Time
    ! Convert an integer array containing
    ! year, month, day, hour, minute, second, millisecond
    ! into the number of seconds since 00:00 January 1 of the base year.
    type(TimeType) :: TimeTmp
    character(len=*), parameter:: NameSub = 'time_int_to_real2'
    !--------------------------------------------------------------------------
    TimeTmp%iYear = iTime_I(1)
    TimeTmp%iMonth = iTime_I(2)
    TimeTmp%iDay = iTime_I(3)
    TimeTmp%iHour = iTime_I(4)
    TimeTmp%iMinute = iTime_I(5)
    TimeTmp%iSecond = iTime_I(6)
    TimeTmp%FracSecond = iTime_I(7)/1000.0

    call time_int_to_real(TimeTmp)

    Time = TimeTmp%Time

  end subroutine time_int_to_real2
  !============================================================================

  subroutine time_int_to_real1(Time)

    type(TimeType), intent(inout) :: Time

    ! Convert the integer fields containing year ... second and the
    ! fractional second into the double precision seconds counted
    ! from the beginning of the base year. Also fill in the string field
    ! of Time.

    character(len=*), parameter:: NameSub = 'time_int_to_real1'

    if (Time%iMonth == 1) then
      do while (Time%iDay >= nDayInMonth_I(Time%iMonth))
        Time%iDay = Time%iDay - nDayInMonth_I(Time%iMonth)
        Time%iMonth = Time%iMonth + 1
      enddo
    endif
    !--------------------------------------------------------------------------
    if (.not. is_valid_int_time(Time)) then
      write(*, *) NameSub, ': invalid Time = ', Time
      call set_error(NameSub//' ERROR invalid time')
    endif

    Time%Time = &
      ((Time%iYear - iYearMin)*365 + n_leap_day(Time%iYear) + &
       n_day_of_year(Time%iYear, Time%iMonth, Time%iDay) - 1)*cSecondPerDay + &
      Time%iHour*cSecondPerHour + &
      Time%iMinute*cSecondPerMinute + &
      Time%iSecond + &
      Time%FracSecond

    call time_int_to_string(Time)

  end subroutine time_int_to_real1
  !============================================================================

  subroutine time_real_to_int1(Time)

    type(TimeType), intent(inout) :: Time
    ! Convert the number of seconds counted from the beginning of the base year
    ! to the integer fields and the fractional second field. Also fill in
    ! the string field of Time.

    integer :: iYear, iMonth, iDay, nLeapYear
    real(Real8_) :: TimeRemaining

    character(len=*), parameter:: NameSub = 'time_real_to_int1'
    !--------------------------------------------------------------------------
    iYear = floor(Time%Time/cSecondPerYear) + iYearMin
    do
      nLeapYear = n_leap_day(iYear)
      iDay = floor((Time%Time - (iYear - iYearMin)*cSecondPerYear)/ &
                   cSecondPerDay) - nLeapYear
      if (iDay >= 0) EXIT
      iYear = iYear - 1
    enddo

    TimeRemaining = Time%Time - (iYear - iYearMin)*cSecondPerYear
    TimeRemaining = TimeRemaining - (iDay + nLeapYear)*cSecondPerDay

    Time%iHour = floor(TimeRemaining/cSecondPerHour)
    TimeRemaining = TimeRemaining - Time%iHour*cSecondPerHour

    Time%iMinute = floor(TimeRemaining/cSecondPerMinute)
    TimeRemaining = TimeRemaining - Time%iMinute*cSecondPerMinute

    Time%iSecond = floor(TimeRemaining)

    Time%FracSecond = TimeRemaining - Time%iSecond

    iMonth = 1; 
    call fix_february(iYear)

    do while (iDay >= nDayInMonth_I(iMonth))
      iDay = iDay - nDayInMonth_I(iMonth)
      iMonth = iMonth + 1
    enddo

    Time%iYear = iYear
    Time%iMonth = iMonth
    Time%iDay = iDay + 1

    call time_int_to_string(Time)

  end subroutine time_real_to_int1
  !============================================================================

  subroutine time_real_to_int2(Time, iTime_I)
    real(real8_), intent(in) :: Time
    integer, intent(out) :: iTime_I(1:7)
    ! Convert the double precision number of seconds since the beginning
    ! of the base year into an integer array of year, month, day, hour,
    ! minute, second, millisecond.
    type(TimeType) :: TimeTmp
    character(len=*), parameter:: NameSub = 'time_real_to_int2'
    !--------------------------------------------------------------------------
    TimeTmp%Time = Time

    call time_real_to_int(TimeTmp)

    iTime_I(1) = TimeTmp%iYear
    iTime_I(2) = TimeTmp%iMonth
    iTime_I(3) = TimeTmp%iDay
    iTime_I(4) = TimeTmp%iHour
    iTime_I(5) = TimeTmp%iMinute
    iTime_I(6) = TimeTmp%iSecond
    iTime_I(7) = TimeTmp%FracSecond*1000.0

  end subroutine time_real_to_int2
  !============================================================================

  subroutine fix_february(iYear)
    integer, intent(in) :: iYear
    !--------------------------------------------------------------------------
    if (is_leap_year(iYear)) then
      nDayInMonth_I(2) = 29
    else
      nDayInMonth_I(2) = 28
    endif
  end subroutine fix_february
  !============================================================================

  subroutine fix_year(iYear)
    integer, intent(inout) :: iYear

    ! Attempt to fix 2 digit years. Assumption :
    ! begin{verbatim}
    !  0-49 --> 2000-2049
    ! 50-99 --> 1950-1999
    ! end{verbatim}
    ! Using a 4 digit year is safer. You should convert before using.

    character(len=*), parameter:: NameSub = 'fix_year'
    !--------------------------------------------------------------------------
    select case (iYear)
    case (0:49)
      iYear = iYear + 2000
    case (50:99)
      iYear = iYear + 1900
    end select

  end subroutine fix_year
  !============================================================================

  integer function n_leap_day(iYear)

    ! Return the number of leap days from base year to the year preceeding iYear.
    ! The leap day in iYear itself is not counted!

    integer, intent(in) :: iYear

    ! local variables

    integer, parameter :: iYearBase100 = 100*(iYearMin/100) + 1
    integer, parameter :: iYearBase400 = 400*(iYearMin/400) + 1

    character(len=*), parameter:: NameSub = 'n_leap_day'
    !--------------------------------------------------------------------------
    n_leap_day = &
      (iYear - iYearMin)/4 &
      - (iYear - iYearBase100)/100 &
      + (iYear - iYearBase400)/400
  end function n_leap_day
  !============================================================================
  logical function is_leap_year(iYear)

    integer, intent(in) :: iYear
    !--------------------------------------------------------------------------
    is_leap_year = modulo(iYear, 4) == 0 .and. &
                   (modulo(iYear, 100) /= 0 .or. modulo(iYear, 400) == 0)

  end function is_leap_year
  !============================================================================
  integer function n_day_of_year(iYear, iMonth, iDay)

    ! Calculate the number of days since the beginning of iYear.
    ! January 1 returns 1. Leap years are taken into account.

    integer, intent(in) :: iYear, iMonth, iDay

    character(len=*), parameter:: NameSub = 'n_day_of_year'
    !--------------------------------------------------------------------------
    call fix_february(iYear)
    n_day_of_year = sum(nDayInMonth_I(1:iMonth - 1)) + iDay

  end function n_day_of_year
  !============================================================================
  subroutine time_real_to_julian(Time, JulianDay)
    ! convert real time info into real julian time
    real, intent(in)  :: Time
    real, intent(out) :: JulianDay

    integer:: iTime_I(7)
    !--------------------------------------------------------------------------
    JulianDay = JulianDayBase + Time/cSecondPerDay

  end subroutine time_real_to_julian
  !============================================================================
  subroutine time_int_to_julian(iTime_I, JulianDay)

    ! convert integer time info into real julian time
    integer, intent(in)  :: iTime_I(1:7)
    real, intent(out) :: JulianDay

    ! formula is valid for date after March, 1900 to yar 2099
    !--------------------------------------------------------------------------
    JulianDay = 367*iTime_I(1) - &
                floor(7*(iTime_I(1) + floor((iTime_I(2) + 9)/12.))/4.) + &
                floor(275*iTime_I(2)/9.) + &
                iTime_I(3) + 1721013.5 + &
                (iTime_I(4) + iTime_I(5)/60.+iTime_I(6)/3600.)/24.
  end subroutine time_int_to_julian
  !============================================================================

end module ModTimeConvert
#endif