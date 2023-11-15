*CMZ :  2.04/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE FE_INI_ZEB

C     Setup link area for the EMC counter
C     CFM 4/7/92

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,FELINK.
#include "sublink.inc"
#include "fpelink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FELINK/',
     #             lFE_link,
     #             lFE_lref,
     #             lFE_last)
      RETURN
      END
