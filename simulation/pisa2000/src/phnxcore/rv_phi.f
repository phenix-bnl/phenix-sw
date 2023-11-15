      SUBROUTINE RV_PHI

      implicit none

#include "g77trigdef.inc"


c     Generate phi particle with dimuon or dielectron decay (PKINE(1) = 0 or 1)
c     P_T and Y distribution interpolated from R. Vogt table
c     Filter according to value of PKINE(2) 0 ===> no filter
c                                           1 ===> require one decay particle
c                                           2 ===> require two decay particles
c                                           3 ===> write out only events with
c                                                  accepted decay particles

c     Low and high theta filter limits are in PKINE(3) and PKINE(4)
c     Minimum momentum filter limit is in PKINE(5)

c     NRVPHI_EVT counts the number of PHIs generated before filter cuts
c     NRVPHI_EVT (in GUEVGEN common) is printed out by GULAST at end of run

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
      INTEGER NUM_PHI, NUM_DECAY  ! if NUM_PHI is input as negative, then BWMASS is set to 1
      INTEGER BWMASS /0/   ! BWMASS = 0 mean no intrinsic width, BWMASS = 1 means 4.26 MeV Breit-Wigner Gamma width
      REAL P1(3), ETOTAL, AMSQ, RAPID, PTRN, THEDEG
      REAL MTRAN
      REAL TRAN
      REAL TH1, TH2, PD1(3), PD2(3), PTOT1, PTOT2

C     PHI MASS (GEV) SQUARED

      PARAMETER (AMSQ=1.0404)
 
      REAL PHIDEG, PTOT
      LOGICAL HEXIST, LFIRST, LOGELE, LOGONE, LOGTWO, LOGMU, LOGK
      LOGICAL LOGNDK
 
      DATA LFIRST/.TRUE./
      DATA NFIRST /1/

      EXTERNAL RV_PHI_FUNC
 
      REAL Y_MIN, Y_MAX
 
      SAVE LFIRST,LOGONE,LOGTWO,LOGELE,NFIRST,IEOM,LOGMU,LOGK
      SAVE LOGNDK

c     begin execution

      NUMEVT = NUMEVT + 1
      MXTOT = 0
      IF(NFIRST.GT.1.AND.PKINE(9).NE.-1.0)GO TO 5
      PKINE(9) = 0.0
      Y_MIN = PKINE(6)
      Y_MAX = PKINE(7)
      NUM_PHI = PKINE(8)
      IF(NUM_PHI.LT.0) THEN
         NUM_PHI = - NUM_PHI
         BWMASS = 1
      ENDIF
      NUM_DECAY = NUM_PHI + NUM_PHI

C     INITIALIZATION

      NFIRST=2
      IF(PKINE(1).EQ.1.0)THEN
         PRINT *,' '
         PRINT *,'  RV_PHI <I> FOR ELECTRON DECAY'
         PRINT *,' '
         LOGELE = .TRUE.
         LOGMU = .FALSE.
         LOGK = .FALSE.
         LOGNDK = .FALSE.
         IEOM = 1
         CHEVT_NAME = ' RV_PHI DIELECTRON DECAY'
      ENDIF
      IF(PKINE(1).EQ.0.0)THEN
         PRINT *,' '
         PRINT *,'  RV_PHI <I> FOR MUON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         LOGMU = .TRUE.
         LOGK = .FALSE.
         LOGNDK = .FALSE.
         IEOM = 0
         CHEVT_NAME = ' RV_PHI DIMUON DECAY'
      ENDIF
      IF(PKINE(1).EQ.2.0)THEN
         PRINT *,' '
         PRINT *,'  RV_PHI <I> FOR KAON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         LOGMU = .FALSE.
         LOGK = .TRUE.
         LOGNDK = .FALSE.
         IEOM = 2
         CHEVT_NAME = ' RV_PHI DIKAON DECAY'
      ENDIF
      IF(PKINE(1).EQ.3.0)THEN
         PRINT *,' '
         PRINT *,'  RV_PHI <I> FOR SPECIAL KAON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         LOGMU = .FALSE.
         LOGK = .FALSE.
         LOGNDK = .TRUE.
         IEOM = 2
         CHEVT_NAME = ' RV_PHI SPECIAL DIKAON DECAY'
      ENDIF

      IF(PKINE(2).GE.1.0)THEN
         WRITE(6,'(/,A,F9.3,A,F9.3,/,A,F9.3,A)')
     1            '  RV_PHI: LOW THETA LIMIT',
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
         WRITE(6,'(/,A,/)')'  RV_PHI: NO DECAY PARTICLE CUTS'
      ENDIF
      WRITE(6,1)PKINE(6),PKINE(7),NUM_PHI
 1    FORMAT(' Y_MIN = ',f8.2,'  Y_MAX = ',f8.2,/,
     +       ' REQUESTING' ,i6,' PHI PARTICLES PER EVENT',/)

      IF(BWMASS.EQ.1)THEN
         WRITE(6,2)
 2       FORMAT('  Using 4.26 MeV Breit-Wigner width for the Phi mass')
      ELSE
         WRITE(6,3)
 3       FORMAT('  Using 0 MeV width for the Phi mass')
      ENDIF

      NRVPHI_EVT = 0

C     Y rapidity from 0 to 5
C     P_T transverse momentum from 0 to 5 GeV/c

      CALL HBFUN2(79,'HRNMD2 for RV_PHI',200,0.,5.,
     +            200,0.,5.,RV_PHI_FUNC)
      CALL HSCALE(79,0.0)

C     END INITIALIZATION

5     CONTINUE
      NRVPHI_EVT = NRVPHI_EVT + 1

C     CYCLE OVER REQUESTED PHI

      CALL HRNDM2(79,RAPID,PTRN)
      CALL GRNDM(TRAN,1)
      IF(TRAN.LE.0.5)RAPID=-RAPID
      IF(RAPID.LT.Y_MIN.OR.RAPID.GT.Y_MAX)THEN
         GO TO 5
      ENDIF  ! check on rapidity within cuts
      MTRAN = SQRT(AMSQ + PTRN*PTRN)
      P1(3) = MTRAN*(0.5*(EXP(RAPID) - EXP(-RAPID)))
      PTOT=SQRT(P1(3)*P1(3)+PTRN*PTRN)
      CALL GRNDM(TRAN,1)
      PHIDEG=360.0*TRAN
      P1(1)=PTRN*COSD(PHIDEG)
      P1(2)=PTRN*SIND(PHIDEG)         ! MOM. COMPONENTS
      IF(LOGONE.OR.LOGTWO)THEN

C     RV_PHI2 WILL DECAY THE PHI
C     PD1,PD2 ARE THE THREE MOMENTA OF THE DECAY DAUGHTERS

         CALL RV_PHI2(PTOT,IEOM,BWMASS,P1,PD1,PD2)
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
               ID_PARENT(MXTOT) = PP_PHI_EE
               ID_PARENT(MXTOT+1) = PP_PHI_EE
            ENDIF
            IF(LOGMU)THEN
               IDTOT(MXTOT) = 5
               IDTOT(MXTOT+1) = 6
               ID_PARENT(MXTOT) = PP_PHI_MM
               ID_PARENT(MXTOT+1) = PP_PHI_MM
            ENDIF
            IF(LOGK)THEN
               IDTOT(MXTOT) = 11
               IDTOT(MXTOT+1) = 12
               ID_PARENT(MXTOT) = PP_PHI_KK
               ID_PARENT(MXTOT+1) = PP_PHI_KK
            ENDIF
            IF(LOGNDK)THEN
               IDTOT(MXTOT) = 51
               IDTOT(MXTOT+1) = 52
               ID_PARENT(MXTOT) = PP_PHI_NDKK
               ID_PARENT(MXTOT+1) = PP_PHI_NDKK
            ENDIF
            PPTOT(2,MXTOT) = PD1(1)
            PPTOT(3,MXTOT) = PD1(2)
            PPTOT(4,MXTOT) = PD1(3)
            MXTOT = MXTOT + 1
            PPTOT(2,MXTOT) = PD2(1)
            PPTOT(3,MXTOT) = PD2(2)
            PPTOT(4,MXTOT) = PD2(3)
            GO TO 20
         ELSE

C     NEITHER DAUGHTER WITHIN LIMITS, TRY AGAIN

           GO TO 5
        ENDIF
      ENDIF
      MXTOT = MXTOT + 1
      IF(LOGELE)THEN
         IDTOT(MXTOT) = PP_PHI_EE    ! ORIGINALLY WAS 55
      ENDIF
      IF(LOGMU)THEN
         IDTOT(MXTOT) = PP_PHI_MM    ! ORIGINALLY WAS 54
      ENDIF  ! MUON CHECK
      IF(LOGK)THEN
         IDTOT(MXTOT) = PP_PHI_KK
      ENDIF
      IF(LOGNDK)THEN
         IDTOT(MXTOT) = PP_PHI_NDKK
      ENDIF
      PPTOT(2,MXTOT)=P1(1)
      PPTOT(3,MXTOT)=P1(2)
      PPTOT(4,MXTOT)=P1(3)
20    CONTINUE
      THEDEG = ACOSD(P1(3)/PTOT)
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

C     CHECK IF MORE PHI DECAYS NEEDED IN THIS EVENT

      IF(LOGONE.OR.LOGTWO)THEN
         IF(MXTOT.LT.NUM_DECAY)THEN
            GO TO 5
         ENDIF
      ELSE
         IF(MXTOT.LT.NUM_PHI)THEN
            GO TO 5
         ENDIF
      ENDIF
      NTRU_EVT = IEVENT
      END_EVTFLG = .TRUE.
      NSUB_EVT = 1
      
      RETURN
      END
