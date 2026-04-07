
MODULE ModIndices

  use ModCharSize
  use ModTimeIO
  use ModIoUnit
  use ModErrors
  use ModKind

  implicit none

  private

    integer, parameter, public :: iZero_ = 0

    TYPE LookupTable
      INTEGER   :: iIndex
      CHARACTER(len=16) :: idxName
    ENDTYPE LookupTable

    integer, parameter :: nValidIndices = 16
    type(LookupTable), dimension(nValidIndices) :: indicesLookup = [ &
      LookupTable(1,  "f107"),  &
      LookupTable(2,  "f107a"), &
      LookupTable(3,  "imfbx"), &
      LookupTable(4,  "imfby"), &
      LookupTable(5,  "imfbz"), &
      LookupTable(6,  "swvx"),  &
      LookupTable(7,  "swvy"),  &
      LookupTable(8,  "swvx"),  &
      LookupTable(9,  "swn"),   &
      LookupTable(10, "swt"),   &
      LookupTable(11, "ae"),    &
      LookupTable(12, "au"),    &
      LookupTable(13, "al"),    &
      LookupTable(14, "hpi"),   &
      LookupTable(15, "hpin"),  &
      LookupTable(16, "hpis")   &
      ]

    integer, external :: efield_interpret_name
    integer, external :: aurora_interpret_name

  ! public

  ! Here's where we store whether the library has read in a certain index type
  ! logical :: haveF107 = .false.
  ! logical :: haveIMF = .false.
  ! logical :: haveHPI = .false. ! special case since it can be derived from AE/SME
  ! logical :: haveSME = .false.

  type, public :: indices

    integer :: iIndex ! f107, bz, etc.
    integer :: nValues = 0
    ! Each index stores arrays of the time & value @ that time
    real, allocatable, dimension(:) :: values
    real, allocatable, dimension(:) :: times



  end type indices

  interface decode_index
    module procedure get_index_name
    module procedure get_index_id
  end interface decode_index

contains

  ! INCLUDE "ModFileIO.f90"

  INCLUDE "indices_lookup.f90"

end MODULE ModIndices
