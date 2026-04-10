
  character(50) function get_index_name(iIndex) result(name)
    integer, intent(in) :: iIndex

    name = trim(indicesLookup(iIndex)%idxName)

  end function get_index_name

  integer function get_index_id(indName) result(iIndex)
    integer :: i
    character(len=*), intent(in) :: indName

    iIndex = -1
    do i = 1, nValidIndices ! Find where the index name matches what we're given
      if (trim(indicesLookup(i)%idxName) == trim(indName)) then
        iIndex = indicesLookup(i)%iIndex
        return
      endif
    enddo
    if (iIndex == -1) then
      isOk = .false.
    endif

  end function get_index_id

integer function get_nValues_from_int(iIndex) result(nValues)
  integer, intent(in) :: iIndex

  nValues = allIndices(iIndex)%nValues

end function get_nValues_from_int

integer function get_nValues_from_char(cIndex) result(nValues)
  character(*), intent(in) :: cIndex
  integer :: iIndex
  iIndex = decode_index(cIndex)
  nValues = allIndices(iIndex)%nValues

end function get_nValues_from_char