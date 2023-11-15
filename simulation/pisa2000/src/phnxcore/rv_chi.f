      SUBROUTINE RV_CHI

      implicit none

#include "g77trigdef.inc"


c     Original Author: Charles F. Maguire (based on RV_PHI and RV_JPSI)
c     Creation Date: June 15, 1999

c     Revision History


c     Generate Chi particle with dimuon or dielectron decay (PKINE(1) = 0 or 1)
c     P_T and Y distribution interpolated from R. Vogt table for J/Psi
c     Filter according to value of PKINE(2) 0 ===> no filter
c                                           1 ===> require one decay particle
c                                           2 ===> require two decay particles
c                                           3 ===> write out only events with
c                                                  accepted decay particles

c     Low and high theta filter limits are in PKINE(3) and PKINE(4)
c     Minimum momentum filter limit is in PKINE(5)

c     NRVCHI_EVT counts the number of CHIs generated before filter cuts
c     NRVCHI_EVT (in GUEVGEN common) is printed out by GULAST at end of run

c     MAP
c       Called by GUEVGEN
c       Calls HBOOK1, HFILL, HBFUN2, HRNDM2, HSCALE, GRNDM

#include "gcflag.inc"
#include "guevgen.inc"
#include "gckine.inc"
#include "subevt.inc"
#include "pisa_parts.inc"
#include "secubuf.inc"

c     local specifications


      INTEGER NFIRST,IEOM
      INTEGER NUM_CHI, NUM_DECAY
      REAL P1(3), ETOTAL, AMSQ, RAPID, PTRN, THEDEG
      REAL MTRAN
      REAL TRAN
      REAL TH1, TH2, PD1(3), PD2(3), PTOT1, PTOT2

      REAL PD3(3)

C     CHI MASS (GEV) SQUARED

      PARAMETER (AMSQ=3.51*3.51)
 
      REAL PHIDEG, PTOT
      LOGICAL HEXIST, LFIRST, LOGELE, LOGONE, LOGTWO, LOGMU
 
      DATA LFIRST/.TRUE./
      DATA NFIRST /1/

      EXTERNAL RV_CHI_FUNC
 
      REAL Y_MIN, Y_MAX
 
      SAVE LFIRST,LOGONE,LOGTWO,LOGELE,NFIRST,IEOM,LOGMU

c     begin execution

      NUMEVT = NUMEVT + 1
      MXTOT = 0
       IF(NFIRST.GT.1.AND.PKINE(9).NE.-1.0)GO TO 5
      PKINE(9) = 0.0
      Y_MIN = PKINE(6)
      Y_MAX = PKINE(7)
      NUM_CHI = PKINE(8)
      NUM_DECAY = NUM_CHI + NUM_CHI + NUM_CHI

C     INITIALIZATION

      NFIRST=2
      IF(PKINE(1).EQ.1.0)THEN
         PRINT *,' '
         PRINT *,'  RV_CHI <I> FOR J/Psi ELECTRON DECAY'
         PRINT *,' '
         LOGELE = .TRUE.
         LOGMU = .FALSE.
         IEOM = 1
         CHEVT_NAME = ' RV_CHI J/Psi DIELECTRON DECAY'
      ENDIF
      IF(PKINE(1).EQ.0.0)THEN
         PRINT *,' '
         PRINT *,'  RV_CHI <I> FOR J/Psi MUON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         LOGMU = .TRUE.
         IEOM = 0
         CHEVT_NAME = ' RV_CHI J/Psi DIMUON DECAY'
      ENDIF

      IF(PKINE(2).GE.1.0.and.PKINE(2).LE.3.0)THEN
         WRITE(6,'(/,A,F9.3,A,F9.3,/,A,F9.3,A)')
     1            '  RV_CHI: LOW THETA LIMIT',
     2            PKINE(3),'  HIGH THETA LIMIT',PKINE(4),
     3            '  MINIMUM TOTAL MOMENTUM ',PKINE(5),' GeV/c'
         IF(PKINE(2).EQ.1.0)THEN
            LOGONE = .TRUE.
            LOGTWO = .FALSE.
            WRITE(6,'(A,/)')' REQUIRE ONLY ONE PARTICLE'
         ELSE IF (PKINE(2).EQ.2.0) THEN
            LOGONE = .FALSE.
            LOGTWO = .TRUE.
            WRITE(6,'(A,/)')' REQUIRE BOTH PARTICLES'
         ELSE IF (PKINE(2).EQ.3.0) THEN
            LOGONE = .FALSE.
            LOGTWO = .TRUE.
            WRITE(6,'(A,/)')' REQUIRE BOTH PARTICLES ACCEPTED'
         ENDIF
      ELSE
         LOGONE = .FALSE.
         LOGTWO = .FALSE.
         WRITE(6,'(/,A,/)')'  RV_CHI: NO DECAY PARTICLE CUTS'
      ENDIF
      WRITE(6,1)PKINE(6),PKINE(7),NUM_CHI
 1    FORMAT(' Y_MIN = ',f8.2,'  Y_MAX = ',f8.2,/,
     +       ' REQUESTING' ,i6,' CHI PARTICLES PER EVENT',/)
      NRVCHI_EVT = 0

C     Y rapidity from 0 to 5
C     P_T transverse momentum from 0 to 5 GeV/c

      CALL HBFUN2(79,'HRNMD2 for RV_CHI',200,0.,5.,
     +            200,0.,5.,RV_CHI_FUNC)
      CALL HSCALE(79,0.0)

C     END INITIALIZATION

5     CONTINUE
      NRVCHI_EVT = NRVCHI_EVT + 1

C     CYCLE OVER REQUESTED CHI

      CALL HRNDM2(79,RAPID,PTRN)
      CALL GRNDM(TRAN,1)
      IF(TRAN.LE.0.5)RAPID=-RAPID
      IF(RAPID.LT.Y_MIN.OR.RAPID.GT.Y_MAX)THEN
         GO TO 5
      ENDIF  ! check on rapidity within cuts
      MTRAN = SQRT(AMSQ + PTRN*PTRN)
      P1(3) = MTRAN*(0.5*(EXP(RAPID) - EXP(-RAPID)))
      PTOT=SQRT(P1(3)*P1(3)+PTRN*PTRN)
      THEDEG = ACOSD(P1(3)/PTOT)
      CALL GRNDM(TRAN,1)
      PHIDEG=360.0*TRAN
      P1(1)=PTRN*COSD(PHIDEG)
      P1(2)=PTRN*SIND(PHIDEG)         ! MOM. COMPONENTS

C     IMPLEMENT KINEMATIC FILTER

      IF(LOGONE.OR.LOGTWO)THEN

C     RV_CHI2 will decay the Chi into a J/Psi and a photon

         CALL RV_CHI2(PTOT,P1,PD1,PD2)

C     SAVE THE PHOTON PD2 VALUES INTO PD3 ARRAY

         PD3(1) = PD2(1)
         PD3(2) = PD2(2)
         PD3(3) = PD2(3)

C     MAKE UP THE J/PSI VALUES FOR NEXT DECAY

         PTOT = SQRT(PD1(1)*PD1(1) + PD1(2)*PD1(2) +
     +               PD1(3)*PD1(3))
         P1(1) = PD1(1)
         P1(2) = PD1(2)
         P1(3) = PD1(3)

C     RV_JPSI2 WILL DECAY THE J/Psi
C     PD1,PD2 ARE THE THREE MOMENTA OF THE DECAY DAUGHTERS

         CALL RV_JPSI2(PTOT,IEOM,1,P1,PD1,PD2)
         PTOT1 = SQRT(PD1(1)*PD1(1) + PD1(2)*PD1(2) +
     1                PD1(3)*PD1(3))
         PTOT2 = SQRT(PD2(1)*PD2(1) + PD2(2)*PD2(2) +
     1                PD2(3)*PD2(3))
         IF(LOGONE)THEN
            IF(PTOT1.LT.PKINE(5).AND.PTOT2.LT.PKINE(5))GO TO 5
         ELSE
            IF(PTOT1.LT.PKINE(5).OR.PTOT2.LT.PKINE(5))GO TO 5
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
         IF((LOGONE.AND.TH1.GE.PKINE(3).AND.TH1.LE.PKINE(4)).OR.
     1      (LOGONE.AND.TH2.GE.PKINE(3).AND.TH2.LE.PKINE(4)).OR.
     2      (LOGTWO.AND.TH2.GE.PKINE(3).AND.TH2.LE.PKINE(4).AND.
     3                  TH1.GE.PKINE(3).AND.TH1.LE.PKINE(4)))THEN
            MXTOT = MXTOT + 1
            IF(LOGELE)THEN
               IDTOT(MXTOT) = 2
               IDTOT(MXTOT+1) = 3
               ID_PARENT(MXTOT) = PP_RJPSI_EE
               ID_PARENT(MXTOT+1) = PP_RJPSI_EE
            ENDIF
            IF(LOGMU)THEN
               IDTOT(MXTOT) = 5
               IDTOT(MXTOT+1) = 6
               ID_PARENT(MXTOT) = PP_RJPSI_MM
               ID_PARENT(MXTOT+1) = PP_RJPSI_MM
            ENDIF
            PPTOT(2,MXTOT) = PD1(1)
            PPTOT(3,MXTOT) = PD1(2)
            PPTOT(4,MXTOT) = PD1(3)
            MXTOT = MXTOT + 1
            PPTOT(2,MXTOT) = PD2(1)
            PPTOT(3,MXTOT) = PD2(2)
            PPTOT(4,MXTOT) = PD2(3)
            MXTOT = MXTOT + 1
            PPTOT(2,MXTOT) = PD3(1)
            PPTOT(3,MXTOT) = PD3(2)
            PPTOT(4,MXTOT) = PD3(3)
            IDTOT(MXTOT) = 1
            ID_PARENT(MXTOT) = PP_CHI_EE
            if(abs(ptot1-2.09).lt.0.10.and.
     +         abs(ptot2-2.96).lt.0.10)then
               write(6,123)NRVCHI_EVT,pd1,pd2
123            format(/,' Test pair:',i6,' pd1',3e14.5,/,
     +                5x,' pd2',3e14.5)
            endif
            GO TO 20
         ELSE

C     NEITHER J/Psi DAUGHTER WITHIN LIMITS, TRY AGAIN

           GO TO 5
        ENDIF
      ENDIF
      MXTOT = MXTOT + 1
      IF(LOGELE)THEN
         IDTOT(MXTOT) = PP_CHI_EE
      ENDIF
      IF(LOGMU)THEN
         IDTOT(MXTOT) = PP_CHI_MM
      ENDIF  ! MUON CHECK
      PPTOT(2,MXTOT)=P1(1)
      PPTOT(3,MXTOT)=P1(2)
      PPTOT(4,MXTOT)=P1(3)
20    CONTINUE
      IF(.NOT.HEXIST(61))CALL HBOOK1(61,'PTRN',100,
     1                                    0.,5.,0.)
      IF(.NOT.HEXIST(62))CALL HBOOK1(62,'ETOTAL',100,
     1                                    0.,100.,0.)
      IF(.NOT.HEXIST(63))CALL HBOOK1(63,'PTOT',100,
     1                                    0.,100.,0.)
      IF(.NOT.HEXIST(64))CALL HBOOK1(64,'PZ',100,
     1                                    0.,100.,0.)
      IF(.NOT.HEXIST(65))CALL HBOOK1(65,'RAPIDITY',200,
     1                                    -5.,5.,0.)
      CALL HFILL(61,PTRN,0.,1.)
      CALL HFILL(62,ETOTAL,0.,1.)
      CALL HFILL(63,PTOT,0.,1.)
      CALL HFILL(64,P1(3),0.,1.)
      CALL HFILL(65,RAPID,0.,1.)
      IF(.NOT.HEXIST(66))CALL HBOOK1(66,'THETA',180,
     1                                    0.,180.,0.)
      CALL HFILL(66,THEDEG,0.,1.)
      IF(.NOT.HEXIST(67))CALL HBOOK1(67,'LOWTH',100,
     1                                    0.,5.0,0.)
      CALL HFILL(67,THEDEG,0.,1.)

C     CHECK IF MORE CHI DECAYS NEEDED IN THIS EVENT

      IF(LOGONE.OR.LOGTWO)THEN
         IF(MXTOT.LT.NUM_DECAY)THEN
            GO TO 5
         ENDIF
      ELSE
         IF(MXTOT.LT.NUM_CHI)THEN
            GO TO 5
         ENDIF
      ENDIF
      NTRU_EVT = IEVENT
      END_EVTFLG = .TRUE.
      NSUB_EVT = 1
      
      RETURN
      END
