*CMZ :  2.04/00 27/10/94  21.00.01  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE FN_INI_ZEB

C     Setup link area for the MUON counter
C     CFM 4/7/92
C     CFM 4/30/92 Split into Tracking Station and Muon pieces

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPNLINK.
#include "fpnlink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,           ! muon ID
     #             '/FNLINK/',
     #             lFN_link,
     #             lFN_lref,
     #             lFN_last)
      RETURN
      END
