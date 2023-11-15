      SUBROUTINE MuonPC_stp

*     USER ROUTINE CALLED AT THE END OF EACH TRACKING STEP     *
*           ( FROM GUSTEP )                                    *
*        INWVOL IS DIFFERENT FROM ZERO WHEN THE TRACK          *
*        IS INCREASING OR HAS REACHED A VOLUME BOUNDARY        *
*         ISTOP IS DIFFERENT FROM 0 IF TRACK HAS STOPPED       *
****************************************************************
 
      IMPLICIT NONE
 
#include "gckine.inc"
#include "gcsets.inc"
#include "gctrak.inc"
#include "gctmed.inc"
#include "guphnx.inc"
 

c    Local variables

 
      REAL XM(3), XD(3), HITSD( 13), XMS(3), XD1(3), XD2(3), DELE
      INTEGER INOUTL, NUMDD, IT, K   
 
      SAVE XMS, XD1, XD2, INOUTL, DELE, HITSD
      DATA INOUTL/0/

c      The track medium, i.e. ISVOL must be defined
      IF ( ISVOL.GT. 0 .AND. (CHARGE.NE.0.0 .OR.
     +     CVOLU_OPT(3,4).EQ.'PHOT') ) THEN



c  FIRST PAD DETECTOR INPUT POINT

      IF ( INWVOL .EQ. 1 ) THEN
         INOUTL = 1
         dele = 0.0

         DO K = 1,3
            XM( K) = VECT( K)
            XMS(K) = VECT( K)
         END DO
         CALL GMEDIA ( XM, NUMDD )
         CALL GMTOD ( XM, XD1, 1 )

c   Global coordinates at entrance

         HITSD(10) = XM(1)
         HITSD(11) = XM(2)
         HITSD(12) = XM(3)
      ENDIF


c     add the energy loss

      dele = dele + destep*1.e+6 ! convert from GeV to keV
 
      IF ((INWVOL.EQ.2.OR.ISTOP.GT.0) .AND.INOUTL.GT.0) THEN
         DO K = 1,3
            XM( K) = VECT( K)
         END DO
         INOUTL = 0
         CALL GMEDIA ( XMS,NUMDD)
         CALL GMTOD ( XM, XD2, 1 )

         HITSD( 1) = XD1( 1)
         HITSD( 2) = XD1( 2)
         HITSD( 3) = XD1( 3)
         HITSD( 4) = XD2( 1)
         HITSD( 5) = XD2( 2)
         HITSD( 6) = XD2( 3)
         HITSD( 7) = TOFG * 1.E9
         HITSD( 8) = FLOAT(IPART)
         HITSD( 9) = dele
         HITSD(13) = SLENG
         CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, HITSD, IT )
      ENDIF
 999  CONTINUE

      end if

      RETURN
      END
