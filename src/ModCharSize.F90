
module ModCharSize

#ifndef STANDALONE
  use ModCharLen, only: iCharLenIO_ => iCharLen_
#endif

  implicit none

#ifdef STANDALONE
  integer, parameter :: iCharLenIO_ = 400
#endif

end module ModCharSize
