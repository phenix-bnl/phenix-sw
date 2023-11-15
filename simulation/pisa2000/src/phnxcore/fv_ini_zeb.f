*CMZ :  2.04/00 23/11/94  14.13.03  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author : Charles F. Maguire
      SUBROUTINE FV_INI_ZEB

C     Setup link area for the VERTEX counter
C     CFM 4/7/92

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPVLINK.
#include "fpvlink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FVLINK/',
     #             lFV_link,
     #             lFV_lref,
     #             lFV_last)
      RETURN
      END
