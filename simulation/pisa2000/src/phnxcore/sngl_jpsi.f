      SUBROUTINE SNGL_JPSI

      implicit none

#include "g77trigdef.inc"


c     Original Author: Kenta Shigaki
c     Creation Date: January 21, 2000
c     Based on: RV_JPSI by Charles F. Maguire

c     Generates J/Psi or Psi-prime particles (PKINE(1) = 0[1] or 10[11])
c     Will have dielectron or dimuon decay (PKINE(1) = 0[10] or 1[11])
c     uniform Y  distribution from PKINE(3) to PKINE(4)
c     uniform pt distribution from PKINE(5) to PKINE(6)

c     Filter according to value of PKINE(2) 0 ===> no filter
c                                           1 ===> require one decay particle
c                                           2 ===> require two decay particles
c                                           3 ===> require pair in PC1/PC2/PC3
c     Low and high theta filter limits are in PKINE(7) and PKINE(8)
c     Minimum momentum filter limit is in PKINE(9)

c     NRVJPSI_EVT counts the number of J/Psi generated before filter cuts
c     NRVJPSI_EVT (in GUEVGEN common) is printed out by GULAST at end of run

c      MAP
c        Called by GUEVGEN
c        Calls HBOOK, HFILL1, GRNDM

c      Returns PPTOT and IDTOT information in GUEVGEN common

#include "gcflag.inc"
#include "guevgen.inc"
#include "gckine.inc"
#include "subevt.inc"
#include "pisa_parts.inc"

c     local specifications

      INTEGER NFIRST
      REAL P1(3), AMSQ, RAPID, PTRN, TRAN, PTOT, PHI
      REAL TH1, TH2, PD1(3), PD2(3), PTOT1, PTOT2

C     J/PSI MASS (GEV) SQUARED

      PARAMETER (AMSQ=9.59097542)
      INTEGER I
      LOGICAL HEXIST, LOGPSIP, LOGELE, LOGONE, LOGTWO
      INTEGER IEOM, IJOP
      DATA NFIRST/1/
      SAVE NFIRST,LOGONE,LOGTWO,IEOM,IJOP
      SAVE LOGPSIP
 

C     BEGIN EXECUTION

      NUMEVT = NUMEVT + 1
      MXTOT = 2
      IF(NFIRST.GT.1.AND.PKINE(10).NE.-1.0)GO TO 5
      PKINE(10) = 0.0
      IF(PKINE(1).EQ.0.0.OR.PKINE(1).EQ.10.0)THEN
         PRINT *,' '
         PRINT *,'  SNGL_JPSI <I> FOR ELECTRON DECAY'
         PRINT *,' '
         LOGELE = .TRUE.
         IEOM = 1
         CHEVT_NAME = 'SNGL_JPSI INTO DIELECTRONS'
      ELSE
         PRINT *,' '
         PRINT *,'  SNGL_JPSI <I> FOR MUON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         IEOM = 0
         CHEVT_NAME = 'SNGL_JPSI INTO DIMUONS'
      ENDIF
      IF(PKINE(1).EQ.0.0.OR.PKINE(1).EQ.1.0)THEN
         PRINT *,' '
         PRINT *,' '
         PRINT *,' SNGL_JPSI <I> INITIALIZING FOR J/PSI 3.096 GeV'
         PRINT *,' '
         LOGPSIP = .FALSE.
         IJOP = 1
      ELSE
         PRINT *,' '
         PRINT *,' SNGL_JPSI <I> INITIALIZING FOR  PSI-prime 3.685 GEV'
         PRINT *,' '
         LOGPSIP = .TRUE.
         IJOP = 2
      ENDIF

C     INITIALIZATION

      WRITE(6,'(A,/,A,F9.3,A,F9.3,/,A,F9.3,A,F9.3)')
     1            '  SNGL_JPSI:',
     2            '  RAPIDITY OF PARENT ',PKINE(3),' TO ',PKINE(4),
     3            '  PT OF PARENT       ',PKINE(5),' TO ',PKINE(6)
      IF(PKINE(2).GE.1.0)THEN
         WRITE(6,'(A,F9.3,A,F9.3,/,A,F9.3)')
     1            '  THETA OF DAUGHTER  ',PKINE(7),' TO ',PKINE(8),
     2            '  MINIMUM MOMENTUM OF DAUGHTER ABOVE ',PKINE(9)
         IF(PKINE(2).EQ.1.0)THEN
            LOGONE = .TRUE.
            LOGTWO = .FALSE.
            WRITE(6,'(A,/)')'  REQUIRE ONLY ONE PARTICLE'
         ELSEIF(PKINE(2).EQ.2.0)THEN
            LOGONE = .FALSE.
            LOGTWO = .TRUE.
            WRITE(6,'(A,/)')'  REQUIRE BOTH PARTICLES'
         ELSE
            LOGONE = .FALSE.
            LOGTWO = .TRUE.
            WRITE(6,'(A,/)')'  REQUIRE BOTH PARTICLES ON CHAMBERS'
         ENDIF
      ELSE
         LOGONE = .FALSE.
         LOGTWO = .FALSE.
         WRITE(6,'(/,A,/)')'  SNGL_JPSI: ALL DECAYS ACCEPTED'
      ENDIF  ! check on kinematics filter
 
      NRVJPSI_EVT = 0
      NFIRST=2

C     END INITIALIZATION

 
5     CONTINUE   ! BRANCH POINT FOR BACKWARD JUMP IN KINEMATICS FILTER
      NRVJPSI_EVT = NRVJPSI_EVT + 1

      CALL GRNDM(TRAN,1)
      RAPID=PKINE(3)+(PKINE(4)-PKINE(3))*TRAN
      CALL GRNDM(TRAN,1)
      PTRN =PKINE(5)+(PKINE(6)-PKINE(5))*TRAN
      CALL GRNDM(TRAN,1)
      PHI=6.28319*TRAN
      P1(1)=PTRN*COS(PHI)
      P1(2)=PTRN*SIN(PHI)
      P1(3)=SQRT(AMSQ+PTRN*PTRN)*SINH(RAPID)
      PTOT=SQRT(P1(3)*P1(3)+PTRN*PTRN)

      IF(LOGONE.OR.LOGTWO)THEN

C     RV_JPSI2 WILL DECAY THE JPSI
C     PD1,PD2 ARE THE THREE MOMENTA OF THE DECAY DAUGHTERS

         CALL RV_JPSI2(PTOT,IEOM,IJOP,P1,PD1,PD2)
         PTOT1 = SQRT(PD1(1)*PD1(1) + PD1(2)*PD1(2) +
     1                PD1(3)*PD1(3))
         PTOT2 = SQRT(PD2(1)*PD2(1) + PD2(2)*PD2(2) +
     1                PD2(3)*PD2(3))
         IF(LOGONE)THEN
            IF(PTOT1.LT.PKINE(9).AND.PTOT2.LT.PKINE(9))GO TO 5
         ELSE
            IF(PTOT1.LT.PKINE(9).OR.PTOT2.LT.PKINE(9))GO TO 5
         ENDIF

         IF(PTOT1.GT.0.)THEN
            TH1 = ACOSD(PD1(3)/PTOT1)
         ELSE
            TH1 = 0.0
         ENDIF
         IF(PTOT2.GT.0.0)THEN
            TH2 = ACOSD(PD2(3)/PTOT2)
         ELSE
            TH2 = 0.0
         ENDIF
         IF((LOGONE.AND.TH1.GE.PKINE(7).AND.TH1.LE.PKINE(8)).OR.
     1      (LOGONE.AND.TH2.GE.PKINE(7).AND.TH2.LE.PKINE(8)).OR.
     2      (LOGTWO.AND.TH2.GE.PKINE(7).AND.TH2.LE.PKINE(8).AND.
     3                  TH1.GE.PKINE(7).AND.TH1.LE.PKINE(8)))THEN
            IF(LOGELE)THEN
               IDTOT(1) = 2
               IDTOT(2) = 3
               IF(LOGPSIP)THEN
                  ID_PARENT(1) = PP_PSIP_EE
                  ID_PARENT(2) = PP_PSIP_EE
               ELSE
                  ID_PARENT(1) = PP_JPSI_EE
                  ID_PARENT(2) = PP_JPSI_EE
               ENDIF
            ELSE
               IDTOT(1) = 5
               IDTOT(2) = 6
               IF(LOGPSIP)THEN
                  ID_PARENT(1) = PP_PSIP_MM
                  ID_PARENT(2) = PP_PSIP_MM
               ELSE
                  ID_PARENT(1) = PP_JPSI_MM
                  ID_PARENT(2) = PP_JPSI_MM
               ENDIF
            ENDIF
            PPTOT(2,1) = PD1(1)
            PPTOT(3,1) = PD1(2)
            PPTOT(4,1) = PD1(3)
            PPTOT(2,2) = PD2(1)
            PPTOT(3,2) = PD2(2)
            PPTOT(4,2) = PD2(3)
            GO TO 20
         ELSE

C     NEITHER DAUGHTER WITHIN LIMITS, TRY AGAIN

            GO TO 5
         ENDIF   ! check on decay particle(s) meeting filter condition
      ELSE
         STOP ' SNGL_JPSI requires the J/Psi to decay into a filter'
      ENDIF   ! CHECK ON KINEMATICS FILTER REQUEST
20    CONTINUE
      IF(.NOT.HEXIST(61))CALL HBOOK1(61,'PTRN',100,
     1                                 0.,7.,0.)
      IF(.NOT.HEXIST(63))CALL HBOOK1(63,'PTOT',100,
     1                                 0.,100.,0.)
      IF(.NOT.HEXIST(64))CALL HBOOK1(64,'PZ',100,
     1                                 0.,100.,0.)
      IF(.NOT.HEXIST(65))CALL HBOOK1(65,'RAPIDITY',200,
     1                                 -4.,4.,0.)
      CALL HFILL(61,PTRN,0.,1.)
      CALL HFILL(63,PTOT,0.,1.)
      CALL HFILL(64,P1(3),0.,1.)
      CALL HFILL(65,RAPID,0.,1.)
      NTRU_EVT = IEVENT
      END_EVTFLG = .TRUE.
      NSUB_EVT = 1
      RETURN
      END
