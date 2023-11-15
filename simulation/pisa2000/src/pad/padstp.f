      SUBROUTINE PADSTP
*     ===============================

***************************************************
*                                                 *
*    USER ROUTINE CALLED FROM GUSTEP              *
*    TO ORGANIZE THE HITS DATA FOR PAD2,3         *
*                                                 *
***************************************************


c   PC2/PC3 routine called from GUSTEP
c   Originally written by N. Smirnov (Yale) 1992-95

c     Revision History
c    Name           Date            Comment
c    C.F. Maguire   Feb. 15, 1998   Re-edit and clean up old N. Smirnov version

c                                   Add global coordinates and path length
c                                   Include option for neutral particle output


      IMPLICIT NONE
 
#include "gckine.inc"
#include "gctmed.inc"
#include "guphnx.inc"
 
      IF ( ISVOL.GT. 0 .AND. (CHARGE.NE.0.0 .OR.
     +     CVOLU_OPT(3,9).EQ.'PHOT') ) THEN
         IF( NUMED.EQ.671 .OR. NUMED.EQ.694)then
             CALL PD2STP
         ENDIF
         IF( NUMED.EQ.673 .OR. NUMED.EQ.699)then
             CALL PD3STP
         ENDIF
      ENDIF

      return
      END
