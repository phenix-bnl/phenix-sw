      SUBROUTINE MuonPad_stp
      IMPLICIT NONE
#include "gckine.inc"
#include "gctmed.inc"
#include "guphnx.inc"
 
      IF ( ISVOL.GT. 0 .AND. (CHARGE.NE.0.0 .OR.
     +     CVOLU_OPT(3,9).EQ.'PHOT') ) THEN
         IF( NUMED.EQ.1001)then
             CALL MuonPC_stp
         ENDIF
         IF( NUMED.EQ.1002)then
             CALL MuonPC_stp
         ENDIF
         IF( NUMED.EQ.1003)then
             CALL MuonPC_stp
         ENDIF
      ENDIF

      return
      END
