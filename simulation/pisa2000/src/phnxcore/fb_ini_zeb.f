*CMZ :  2.04/00 05/06/93  19.32.32  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
      SUBROUTINE FB_INI_ZEB

C     Setup link area for the VERTEX counter
C     CFM 4/7/92

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,FPBLINK.
#include "sublink.inc"
#include "fpblink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FBLINK/',
     #             lFB_link,
     #             lFB_lref,
     #             lFB_last)
      RETURN
      END
