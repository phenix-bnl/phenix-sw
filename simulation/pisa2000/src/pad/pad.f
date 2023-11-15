      subroutine pad(full, nh)
      implicit none

c     Original Author: Charles F. Maguire, March 29, 1996

c     Installs PC2/PC3 geometry

c     Global Variables

#include "guphnx.inc"

c     Local variables

      integer nmpd23  ! number of PC23 sectors
      integer full, nh

c     Revision History


c     Begin execution

      IF(CVOLU_OPT(1,9) .EQ. 'FULL' .OR. CVOLU_OPT(1,9) .EQ. 'VOLS')
     &  THEN

        WRITE(6,*)'pad - old geometry for PC2/PC3'
        call pd23gem
        call pd23det
      ELSEIF(CVOLU_OPT(1,9) .EQ. 'PC96') THEN
        WRITE(6,*)'pad - AAR NEW GEOM DEFINE'
        call pc23gem(nmpd23)
        call pc23det(nmpd23)
      ELSEIF(CVOLU_OPT(1,9) .EQ. 'PC98') THEN
        write(6,*)'pad - 1998 geometry (PC2/3 Updated)'
        call pc23gem98(nmpd23)
        call pc23det(nmpd23)

      ELSE
        WRITE(6,*)'pad - No PC2/PC3 volumes defined'
      ENDIF

      return
      end
