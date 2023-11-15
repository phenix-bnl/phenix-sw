*CMZ :  2.04/00 23/11/94  13.18.49  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE FC_INI_ZEB

C     Setup link area for the CERENKOV counter
C     CFM 4/7/92

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPCLINK.
#include "fpclink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FCLINK/',
     #             lFC_link,
     #             lFC_lref,
     #             lFC_last)
      RETURN
      END
