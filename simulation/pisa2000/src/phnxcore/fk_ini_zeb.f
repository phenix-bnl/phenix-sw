*CMZ :  2.04/00 16/12/94  10.43.03  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE FK_INI_ZEB

C     Setup link area for the CSI hodoscope
C     CFM 4/30/92

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,FPKLINK.
#include "sublink.inc"
#include "fpklink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FKLINK/',
     #             lFK_link,
     #             lFK_lref,
     #             lFK_last)
      RETURN
      END
