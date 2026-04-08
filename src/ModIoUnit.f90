
module ModIoUnits

  implicit none

  private ! except

  !PUBLIC MEMBER FUNCTIONS:

  public :: io_unit_new    ! Return an unused unit number for extended use
  public :: io_unit_clean  ! Close open units, delete empty files

  !PUBLIC DATA MEMBERS:

  integer, parameter, public :: nIndexValuesMax = 500000 ! hopefully this is enough :)
  real, parameter, public  :: rBadValue = -6e6

  integer, parameter, public :: StdIn_ = 5  ! Standard input
  integer, parameter, public :: StdOut_ = 6  ! Standard output

  ! For open read/write close without intervening open
  integer, parameter, public :: UnitTmp_ = 9  ! 1st Temporary unit number
  integer, parameter, public :: UnitTmp2_ = 8  ! 2nd Temporary unit number

  !LOCAL VARIABLES:

  integer, parameter :: MinUnitNumber = 20    ! Smallest allowed unit number
  integer, parameter :: MaxUnitNumber = 1000  ! Largest allowed unit number

  integer :: iUnitMax = UnitTmp_              ! The largest unit number used

  !REVISION HISTORY:
  ! 01Aug03  Gabor Toth <gtoth@umich.edu> - initial prototype/prolog/code
  ! 20Aug04  Gabor Toth                     added debugging for io_unit_new
  !EOP ___________________________________________________________________

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

end module ModIoUnits
