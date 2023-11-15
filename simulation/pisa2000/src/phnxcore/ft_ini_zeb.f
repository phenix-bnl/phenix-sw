*CMZ :  2.04/00 23/11/94  13.55.23  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE FT_INI_ZEB

C     Setup link area for TRD
C     /mk/ 21/02/90

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPTLINK.
#include "fptlink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FTLINK/',
     #             lFT_link,
     #             lFT_lref,
     #             lFT_last)
      RETURN
      END
