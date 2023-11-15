      SUBROUTINE SNGL_PHI

      implicit none

#include "g77trigdef.inc"


c     Original Author: Kenta Shigaki
c     Creation Date: January 21, 2000
c     Based on: RV_PHI by Charles F. Maguire

c     Generate phi particle with dimuon or dielectron decay (PKINE2(1) = 0 or 1)
c     uniform Y  distribution from PKINE2(3) to PKINE2(4)
c     uniform pt distribution from PKINE2(5) to PKINE2(6)

c     Filter according to value of PKINE2(2) 0 ===> no filter
c                                           1 ===> require one decay particle
c                                           2 ===> require two decay particles
c                                           3 ===> write out only events with
c                                                  accepted decay particles
c     Low and high theta filter limits are in PKINE2(7) and PKINE2(8)
c     Minimum momentum filter limit is in PKINE2(9)

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


      INTEGER NFIRST,IEOM, IONE, ITWO
      INTEGER NUM_PHI /1/ ! if NUM_PHI is input as negative, then BWMASS is set to 1
      INTEGER BWMASS /0/   ! BWMASS = 0 mean no intrinsic width, BWMASS = 1 means 4.26 MeV Breit-Wigner Gamma width
      REAL P1(3), AMSQ, RAPID, PTRN, TRAN, PTOT, PHI
      REAL TH1, TH2, PD1(3), PD2(3), PTOT1, PTOT2, PHI1, PHI2
      REAL PHIMINWEST /-80.0/
      REAL PHIMAXWEST /+100.0/
      REAL PHIMINEAST /+90.0/
      REAL PHIMAXEAST /+270.0/

C     PHI MASS (GEV) SQUARED

      PARAMETER (AMSQ=1.0404)
      LOGICAL HEXIST, LOGELE, LOGONE, LOGTWO, LOGMU, LOGK
      LOGICAL LOGNDK
 
      DATA NFIRST /1/

      SAVE LOGONE,LOGTWO,LOGELE,NFIRST,IEOM,LOGMU,LOGK
      SAVE LOGNDK

c     begin execution

      NUMEVT = NUMEVT + 1
      MXTOT = 2  ! One (only 1) Phi meson is required to decay in this version
      IF(NFIRST.GT.1.AND.PKINE2(10).NE.-1.0)GO TO 5
      PKINE2(10) = 0.0

C     INITIALIZATION

      IF(PKINE(2).EQ.1.0)THEN
         PRINT *,' '
         PRINT *,'  SNGL_PHI <I> FOR ELECTRON DECAY'
         PRINT *,' '
         LOGELE = .TRUE.
         LOGMU = .FALSE.
         LOGK = .FALSE.
         LOGNDK = .FALSE.
         IEOM = 1
         CHEVT_NAME = ' SNGL_PHI DIELECTRON DECAY'
      ENDIF
      IF(PKINE(1).EQ.0.0)THEN
         PRINT *,' '
         PRINT *,'  SNGL_PHI <I> FOR MUON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         LOGMU = .TRUE.
         LOGK = .FALSE.
         LOGNDK = .FALSE.
         IEOM = 0
         CHEVT_NAME = ' SNGL_PHI DIMUON DECAY'
      ENDIF
      IF(PKINE(1).EQ.2.0)THEN
         PRINT *,' '
         PRINT *,'  SNGL_PHI <I> FOR KAON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         LOGMU = .FALSE.
         LOGK = .TRUE.
         LOGNDK = .FALSE.
         IEOM = 2
         CHEVT_NAME = ' SNGL_PHI DIKAON DECAY'
      ENDIF
      IF(PKINE(1).EQ.3.0)THEN
         PRINT *,' '
         PRINT *,'  SNGL_PHI <I> FOR SPECIAL KAON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         LOGMU = .FALSE.
         LOGK = .FALSE.
         LOGNDK = .TRUE.
         IEOM = 2
         CHEVT_NAME = ' SNGL_PHI SPECIAL DIKAON DECAY'
      ENDIF

      WRITE(6,'(A,/,A,F9.3,A,F9.3,/,A,F9.3,A,F9.3,/)')
     1            '  SNGL_PHI:',
     2            '  RAPIDITY OF PARENT ',PKINE2(1),' TO ',PKINE2(2),
     3            '  PT OF PARENT       ',PKINE2(3),' TO ',PKINE2(4)
      IF(PKINE2(5).LE.0.0)THEN
         PKINE2(5) = -90.0
      ENDIF
      IF(PKINE2(6).GE.360.0)THEN
         PKINE2(6) = +270.0
      ENDIF
      IF(PKINE2(5).LT.PKINE2(6))THEN
         PHIMINWEST = PKINE2(5)
         PHIMINEAST = PKINE2(5)
         PHIMAXWEST = PKINE2(6)
         PHIMAXEAST = PKINE2(6)
      ELSE
         WRITE(6,'(/,A,/,A,F9.3,A,F9.3/,A,F9.3,A,F9.3)')
     +        ' EAST AND WEST AZIMUTH RESTRICTION',
     +        ' WEST MIN ', PHIMINWEST, ', WEST MAX ', PHIMAXWEST,
     +        ' EAST MIN ', PHIMINEAST, ', EAST MAX ', PHIMAXEAST
      ENDIF
      IF(IKINE2(2).GE.1)THEN
         WRITE(6,'(A,F9.3,A,F9.3,/,A,F9.3,A,F9.3,/,A,F9.3)')
     +            '  PHI OF DAUGHTER  ',PKINE2(5),' TO ',PKINE2(6),
     1            '  THETA OF DAUGHTER  ',PKINE2(7),' TO ',PKINE2(8),
     2            '  MINIMUM MOMENTUM OF DAUGHTER ABOVE ',PKINE2(9)
         IF(IKINE2(2).EQ.1)THEN
            LOGONE = .TRUE.
            LOGTWO = .FALSE.
            WRITE(6,'(A,/)')'  REQUIRE ONLY ONE PARTICLE'
         ELSEIF(IKINE2(2).EQ.2)THEN
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
         WRITE(6,'(/,A,/)')'  SNGL_PHI: ALL DECAYS ACCEPTED'
      ENDIF  ! check on kinematics filter

      NRVPHI_EVT = 0
      NFIRST=2

C     END INITIALIZATION

5     CONTINUE
      NRVPHI_EVT = NRVPHI_EVT + 1

      CALL GRNDM(TRAN,1)
      RAPID=PKINE2(1)+(PKINE2(2)-PKINE2(1))*TRAN
      CALL GRNDM(TRAN,1)
      PTRN=PKINE2(3)+(PKINE2(4)-PKINE2(3))*TRAN
      CALL GRNDM(TRAN,1)
      PHI=6.28319*TRAN
      P1(1)=PTRN*COS(PHI)
      P1(2)=PTRN*SIN(PHI)
      P1(3)=SQRT(AMSQ+PTRN*PTRN)*SINH(RAPID)
      PTOT=SQRT(P1(3)*P1(3)+PTRN*PTRN)

      IF(LOGONE.OR.LOGTWO)THEN

C     RV_PHI2 WILL DECAY THE PHI
C     PD1,PD2 ARE THE THREE MOMENTA OF THE DECAY DAUGHTERS

         CALL RV_PHI2(PTOT,IEOM,BWMASS,P1,PD1,PD2)
         PTOT1 = SQRT(PD1(1)*PD1(1) + PD1(2)*PD1(2) +
     1                PD1(3)*PD1(3))
         PTOT2 = SQRT(PD2(1)*PD2(1) + PD2(2)*PD2(2) +
     1                PD2(3)*PD2(3))
         IF(LOGONE)THEN
            IF(PTOT1.LT.PKINE2(9).AND.PTOT2.LT.PKINE2(9))GO TO 5
         ELSE
            IF(PTOT1.LT.PKINE2(9).OR.PTOT2.LT.PKINE2(9))GO TO 5
         ENDIF

         IF(PTOT1.GT.0.)THEN
            TH1 = ACOSD(PD1(3)/PTOT1)
            PHI1 = ATAN2D(PD1(2), PD1(1))
            IF(PHI1.LT.-90.0)THEN
               PHI1 = 360.0 + PHI1
            ENDIF
         ELSE
            TH1 = 0.0
            PHI1 = 0.0
         ENDIF
         IF(PTOT2.GT.0.0)THEN
            TH2 = ACOSD(PD2(3)/PTOT2)
            PHI2 = ATAN2D(PD2(2), PD2(1))
            IF(PHI2.LT.-90.0)THEN
               PHI2 = 360.0 + PHI2
            ENDIF
         ELSE
            TH2 = 0.0
            PHI2 = 0.0
         ENDIF
         IF((LOGONE.AND.TH1.GE.PKINE2(7).AND.TH1.LE.PKINE2(8)).OR.
     1      (LOGONE.AND.TH2.GE.PKINE2(7).AND.TH2.LE.PKINE2(8)).OR.
     2      (LOGTWO.AND.TH2.GE.PKINE2(7).AND.TH2.LE.PKINE2(8).AND.
     3                  TH1.GE.PKINE2(7).AND.TH1.LE.PKINE2(8).AND.
     4       ((PHI1.GE.PHIMINEAST.AND.PHI1.LE.PHIMAXEAST.AND.
     5         PHI2.GE.PHIMINEAST.AND.PHI2.LE.PHIMAXEAST).OR.
     6        (PHI1.GE.PHIMINWEST.AND.PHI1.LE.PHIMAXWEST.AND.
     7         PHI2.GE.PHIMINWEST.AND.PHI2.LE.PHIMAXWEST))
     8        ))THEN
            CALL GRNDM(TRAN,1)
            IONE = 1
            ITWO = 2
            IF(TRAN>0.5)THEN
               IONE = 2
               ITWO = 1
            ENDIF
            IF(LOGELE)THEN
               IDTOT(IONE) = 2
               IDTOT(ITWO) = 3
               ID_PARENT(IONE) = PP_PHI_EE
               ID_PARENT(ITWO) = PP_PHI_EE
            ENDIF
            IF(LOGMU)THEN
               IDTOT(IONE) = 5
               IDTOT(ITWO) = 6
               ID_PARENT(IONE) = PP_PHI_MM
               ID_PARENT(ITWO) = PP_PHI_MM
            ENDIF
            IF(LOGK)THEN
               IDTOT(IONE) = 11
               IDTOT(ITWO) = 12
               ID_PARENT(IONE) = PP_PHI_KK
               ID_PARENT(ITWO) = PP_PHI_KK
            ENDIF
            IF(LOGNDK)THEN
               IDTOT(IONE) = 51
               IDTOT(ITWO) = 52
               ID_PARENT(IONE) = PP_PHI_NDKK
               ID_PARENT(ITWO) = PP_PHI_NDKK
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
         ENDIF  ! check on decay particle(s) meeting filter condition
      ELSE
         STOP ' SNGL_PHI requires the Phi to decay into a filter'
      ENDIF  ! check on requiring one or two decay particles in filter
20    CONTINUE
      IF(.NOT.HEXIST(61))CALL HBOOK1(61,'PTRN',100,
     1                                    0.,5.,0.)
      IF(.NOT.HEXIST(63))CALL HBOOK1(63,'PTOT',100,
     1                                    0.,100.,0.)
      IF(.NOT.HEXIST(64))CALL HBOOK1(64,'PZ',100,
     1                                    0.,100.,0.)
      IF(.NOT.HEXIST(65))CALL HBOOK1(65,'RAPIDITY',200,
     1                                    -5.,5.,0.)
      CALL HFILL(61,PTRN,0.,1.)
      CALL HFILL(63,PTOT,0.,1.)
      CALL HFILL(64,P1(3),0.,1.)
      CALL HFILL(65,RAPID,0.,1.)

      NTRU_EVT = IEVENT
      END_EVTFLG = .TRUE.
      NSUB_EVT = 1
      
      RETURN
      END
