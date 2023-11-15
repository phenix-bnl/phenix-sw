*CMZ :  2.04/00 27/10/94  21.00.24  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE FM_INI_ZEB

C     Setup link area for the MUON trackers
C     CFM 4/7/92
C     CFM 4/30/92 Split into Tracking Station and Muon pieces
C     CFM 9/25/92 Separate entirely the MUON TRACKER from the MUON ID

      IMPLICIT NONE
*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPMLINK.
#include "fpmlink.inc"
*KEND.

C *** setup link area(s)

      CALL MZLINK( ixstor_F,           ! tracking stations
     #             '/FPMLINK/',
     #             lFM_link,
     #             lFM_lref,
     #             lFM_last)
      RETURN
      END
