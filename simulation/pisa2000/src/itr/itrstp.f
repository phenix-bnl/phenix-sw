      SUBROUTINE ITRSTP
*     ===============================

***************************************************
*                                                 *
*    USER ROUTINE CALLED FROM ITRSTEP             *
*    TO ORGANIZE THE HITS DATA FOR ITR_DETECTOR   *
*    Called by GUSTEP based on INTR pseudovolume  *
***************************************************

c     Revision History
c    Name           Date            Comment
c    C.F. Maguire   Feb. 15, 1998   Re-edit and clean up old N. Smirnov version

c                                   Add option to store neutrals

      IMPLICIT NONE
 
#include "gckine.inc"
#include "gctmed.inc"
#include "guphnx.inc"
 
      IF ( ISVOL.GT. 0 .AND. (CHARGE.NE.0.0 .OR.
     +     CVOLU_OPT(3,4).EQ.'PHOT') ) THEN
         IF( NUMED.EQ. 480 .AND. CHARGE.NE.0.0) THEN
             CALL DCHSTP
         ENDIF  ! check on DC1 NUMED value 

         IF( NUMED.EQ. 481 .AND. CHARGE.NE.0.0) THEN
             CALL DCHSTP
         ENDIF  ! check on DC2 NUMED value

         IF( NUMED .EQ. 461 .or. NUMED .EQ. 494 ) THEN
             CALL PD1STP
         ENDIF  ! check on PC1 NUMED value

      ENDIF  ! check on sensitive volume and charge
      return
      END
