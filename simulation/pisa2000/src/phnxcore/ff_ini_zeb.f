*CMZ :  2.04/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE FF_INI_ZEB

C     Setup link area for the TOF counter
C     CFM 4/7/92

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,FFLINK.
#include "sublink.inc"
#include "fpflink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FFLINK/',
     #             lFF_link,
     #             lFF_lref,
     #             lFF_last)
      RETURN
      END
