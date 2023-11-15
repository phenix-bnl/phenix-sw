          SUBROUTINE TECSTP


c    Revision History
c  Name             Date           Comment
c  C.F. Maguire     Feb. 15, 1998  Add global coordinates to output
c                                  Saves CPU time in DIO/STAF, and
c                                  decouples more STAF from GEANT

c  C.F. Maguire     Aug. 18, 1999  Fix HITSD dimension as 13 instead of 10
c                                  Bug caused sporadic problems


*       =============================

****************************************************************
*     USER ROUTINE CALLED AT THE END OF EACH TRACKING STEP     *
*           ( FROM TRDSTP AFTER GUSTEP)                        *
*        INWVOL IS DIFFERENT FROM ZERO WHEN THE TRACK          *
*        IS BEGINNING OR HAS REACHED A VOLUME BOUNDARY         *
*         ISTOP IS DIFFERENT FROM 0 IF TRACK HAS STOPPED       *
****************************************************************
 

C    SUBROUTINE IS CALLED BY TRDSTP WHICH IS CALLED FROM GUSTEP
C    GUSTEP CALL OCCURS WHEN SECT IS SEEN AS THE TOP VOLUME NAME
C    ONLY NLEVEL = 5 CORRESPONDS TO THE ACTUAL DETECTOR VOLUME XE

 
           IMPLICIT NONE

#include "gckine.inc"
#include "gcsets.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
 

c    Local variables

      REAL XM1(3), XM2(3), HITSD( 13), TOF1, TOF2, DELE
      real xd1(3)
      real xd2(3)
      INTEGER INOUTL, K, IT
 
      SAVE XM1, XM2,  INOUTL, TOF1, DELE
      DATA INOUTL/ 0/

c     begin execution

      IF(NLEVEL.NE.5)THEN
         RETURN
      ENDIF
      IF( INWVOL .EQ. 1 ) THEN
         DELE = 0.0
         INOUTL = 1
         DO K = 1,3
           XM1( K) = VECT( K)
         END DO
         TOF1 = TOFG * 1.E9
      ENDIF
      DELE = DELE + DESTEP*1.e+06  ! convert from GeV to keV
      IF((INWVOL.EQ.2.OR.ISTOP.GT.0) .AND.INOUTL.GT.0) THEN
         DO K = 1,3
           XM2( K) = VECT( K)
         END DO
         INOUTL = 0
c *** MITCH *** changed on 4/24/96 to store local coordinates
         call gmtod(xm1,xd1,1)   ! debug insertion
         call gmtod(xm2,xd2,1)   ! debug insertion
c         HITSD( 1) = XM1( 1)
c         HITSD( 2) = XM1( 2)
c         HITSD( 3) = XM1( 3)
c         HITSD( 4) = XM2( 1)
c         HITSD( 5) = XM2( 2)
c         HITSD( 6) = XM2( 3)
         HITSD( 1) = XD1( 1)
         HITSD( 2) = XD1( 2)
         HITSD( 3) = XD1( 3)
         HITSD( 4) = XD2( 1)
         HITSD( 5) = XD2( 2)
         HITSD( 6) = XD2( 3)

         HITSD( 7) = TOF1
         HITSD( 8) = FLOAT(IPART)
         TOF2 = TOFG * 1.E9
         HITSD( 9) = TOF2
         HITSD(10) = DELE
         HITSD(11) = XM1( 1)
         HITSD(12) = XM1( 2)
         HITSD(13) = XM1( 3)
         IT = 0
         CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, HITSD, IT )
         IF(IT.EQ.0)THEN
            WRITE(6,2)
 2          FORMAT(/,1X,'TECSTP <E>: HIT WAS NOT STORED ??')
         ENDIF  ! SAFETY CHECK
      ENDIF
      RETURN
      END
