
MODULE ModIndices

  use ModCharSize
  use ModTimeIO
  use ModIoUnits
  use ModErrors
  use ModKind

  implicit none

  private

  integer, parameter, public :: iZero_ = 0

  TYPE LookupTable
    INTEGER   :: iIndex
    CHARACTER(len=30) :: idxName
  END TYPE LookupTable

  integer, parameter :: nValidIndices = 16
  type(LookupTable), dimension(nValidIndices) :: indicesLookup = [ &
                                                 LookupTable(1, "f107"), &
                                                 LookupTable(2, "f107a"), &
                                                 LookupTable(3, "imfbx"), &
                                                 LookupTable(4, "imfby"), &
                                                 LookupTable(5, "imfbz"), &
                                                 LookupTable(6, "swvx"), &
                                                 LookupTable(7, "swvy"), &
                                                 LookupTable(8, "swvz"), &
                                                 LookupTable(9, "swn"), &
                                                 LookupTable(10, "swt"), &
                                                 LookupTable(11, "ae"), &
                                                 LookupTable(12, "au"), &
                                                 LookupTable(13, "al"), &
                                                 LookupTable(14, "hpi"), &
                                                 LookupTable(15, "hpin"), &
                                                 LookupTable(16, "hpis") &
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
    ! Each index stores arrays of the time & value @ that time
    real, allocatable, dimension(:) :: value
    type(TimeType), allocatable, dimension(:) :: time
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

  ! INCLUDE "ModFileIO.f90"

  INCLUDE "time_subroutines.f90"

  INCLUDE "indices_lookup.f90"

  INCLUDE "initialize_indices.f90"

  INCLUDE "index_retrieval.f90"

end MODULE ModIndices
