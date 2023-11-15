*CMZ :  2.04/00 23/11/94  13.54.45  by  Charles F. Maguire
*CMZU:  2.02/00 07/05/93  13.01.28  by  Unknown
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE FI_INI_ZEB

C     Setup link area for the INTERMED trackers
C     CFM 4/7/92

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPILINK.
#include "fpilink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FILINK/',
     #             lFI_link,
     #             lFI_lref,
     #             lFI_last)
      RETURN
      END
