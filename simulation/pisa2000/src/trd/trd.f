*-- Modified: Kirill Filimonov  13/09/95, an option of new PC2/PC3 geometry
*             is added

c     Revised March 29, 1996  Charles F. Maguire to split out PC2/PC3


      subroutine trd
      implicit none

c     control routine for the TRD/TEC detectors geometry

#include "guphnx.inc"

c     local variables


c     begin execution


      call tecgeo
      write(6,*)' TRD <I>: TEC is installed'
      call tecdet
 
      return
      end
