
MODULE ModIndices

  use ModTimeConvert
  use ModIoUnit
  use ModErrors
  use ModKind

  implicit none

  private

  integer, parameter, public :: iZero_ = 0

  TYPE LookupTable
    integer           :: iIndex
    character(len=30) :: idxName

  END TYPE LookupTable

  integer, parameter :: nValidIndices = 17
  type(LookupTable), parameter, dimension(nValidIndices) :: &
    indicesLookup = [LookupTable(1, "f107"), &
                     LookupTable(2, "f107a"), &
                     LookupTable(3, "imfbx"), &
                     LookupTable(4, "imfby"), &
                     LookupTable(5, "imfbz"), &
                     LookupTable(6, "swvx"), &
                     LookupTable(7, "swvy"), &
                     LookupTable(8, "swvz"), &
                     LookupTable(9, "swvmag"), &
                     LookupTable(10, "swn"), &
                     LookupTable(11, "swt"), &
                     LookupTable(12, "ae"), &
                     LookupTable(13, "au"), &
                     LookupTable(14, "al"), &
                     LookupTable(15, "hpi"), &
                     LookupTable(16, "hpin"), &
                     LookupTable(17, "hpis") &
                     ]

  public :: init_f107
  public :: init_ae
  public :: init_imf
  public :: init_hpi

  interface init_hpi
    module procedure init_noaa_hpi
    module procedure init_hpi_from_ae
  end interface init_hpi

  public :: indexType
  type IndexType
    integer :: iIndex ! f107, bz, etc.
    integer :: nValues = 0
    ! Each index stores arrays of the times & values
    real, allocatable, dimension(:) :: values
    type(TimeType), allocatable, dimension(:) :: times
  end type indexType

  ! Central storage for all loaded indices
  type(indexType), dimension(nValidIndices), public :: allIndices

  ! Current time for "set once, query many" pattern
  type(TimeType), public :: currentTime = &
                            TimeType(0, 0, 0, 0, 0, 0, 0.0d0, -1.0d0, "")

  public :: decode_index
  interface decode_index
    module procedure get_index_name
    module procedure get_index_id
  end interface decode_index

  public :: get_index
  interface get_index
    module procedure get_index_int_wtime
    module procedure get_index_int_wotime
    module procedure get_index_char_wtime
    module procedure get_index_char_wotime
  end interface get_index

  public :: set_index

  public :: set_time
  interface set_time
    module procedure set_time_real
    module procedure set_time_type
    module procedure set_time_components
  end interface set_time

contains

  INCLUDE "time_subroutines.inc"

  INCLUDE "indices_lookup.inc"

  INCLUDE "initialize_indices.inc"

  INCLUDE "index_retrieval.inc"

end MODULE ModIndices
