      SUBROUTINE RV_JPSI

      implicit none

#include "g77trigdef.inc"


c     Original Author: Charles F. Maguire
c     Creation Date: October 10, 1992

c      Generates J/Psi or Psi-prime particles (PKINE(1) = 0[1] or 10[11])
c      Will have dielectron or dimuon decay (PKINE(1) = 0[10] or 1[11])
c      Y and P_T distribution from interpolation of R. Vogt tables

c     Filter according to value of PKINE(2) 0 ===> no filter
c                                           1 ===> require one decay particle
c                                           2 ===> require two decay particles
c                                           3 ===> require pair in PC1/PC2/PC3
c     Low and high theta filter limits are in PKINE(3) and PKINE(4)
c     Minimum momentum filter limit is in PKINE(5)


c     Modified by Marzia Rosati on Feb 19 1999
c     added phi acceptance filter
c     Min-Max phi angle for arm 1 filter limit are in PKINE(6),PKINE(7)
c     Min-Max phi angle for arm 2 filter limit are in PKINE(8),PKINE(9

c     NRVJPSI_EVT counts the number of J/Psi generated before filter cuts
c     NRVJPSI_EVT (in GUEVGEN common) is printed out by GULAST at end of run

c     MAP
c       Called by GUEVGEN
c       Calls HBOOK1, HFILL, HBFUN2, HRNDM2, HSCALE, GRNDM

c     Modified by C.F. Maguire on Feb 26 1999
c     Change to GEANT random number calls

c     C.F. Maguire   June 17, 2002  Change randomization for Y and PT


c      MAP
c        Called by GUEVGEN
c        Calls EULER, HBOOK, HFILL1, GRNDM

c      Returns PPTOT and IDTOT information in GUEVGEN common

#include "gcflag.inc"
#include "guevgen.inc"
#include "gckine.inc"
#include "subevt.inc"
#include "pisa_parts.inc"

      external rv_jpsi_func

c     local specifications

      INTEGER NFIRST
      REAL P1(3), AMSQ, RAPID, PTRN, THEDEG
      REAL TH1, TH2, PD1(3), PD2(3), PTOT1, PTOT2
      REAL PHI1, PHI2

C     J/PSI MASS (GEV) SQUARED

      PARAMETER (AMSQ=9.59096657)

      REAL TRAN, MTRAN, ETOTAL
      REAL  PHIC, THC, PHI, PTOT
      LOGICAL HEXIST, LFIRST, LOGPSIP, LOGELE, LOGONE, LOGTWO
      INTEGER IEOM, IJOP
      INTEGER IPHIACC1,IPHIACC2

      DATA NFIRST /1/
      DATA LFIRST/.TRUE./
      SAVE NFIRST,LOGONE,LOGTWO,IEOM,IJOP
      SAVE LOGPSIP, LFIRST
 

C     BEGIN EXECUTION

      NUMEVT = NUMEVT + 1
      IF(NFIRST.GT.1.AND.PKINE(10).NE.-1.0)GO TO 5
      PKINE(10) = 0.0
      IF(PKINE(1).EQ.0.0.OR.PKINE(1).EQ.10.0)THEN
         PRINT *,' '
         PRINT *,'  RV_JPSI <I> FOR ELECTRON DECAY'
         PRINT *,' '
         LOGELE = .TRUE.
         IEOM = 1
         CHEVT_NAME = 'RV_JPSI INTO DIELECTRONS'
      ELSE
         PRINT *,' '
         PRINT *,'  RV_JPSI <I> FOR MUON DECAY'
         PRINT *,' '
         LOGELE = .FALSE.
         IEOM = 0
         CHEVT_NAME = 'RV_JPSI INTO DIMUONS'
      ENDIF
      IF(PKINE(1).EQ.0.0.OR.PKINE(1).EQ.1.0)THEN
         PRINT *,' '
         PRINT *,' '
         PRINT *,' RV_JPSI <I> INITIALIZING FOR J/PSI 3.096 GeV'
         PRINT *,' '
         LOGPSIP = .FALSE.
         IJOP = 1
      ELSE
         PRINT *,' '
         PRINT *,' RV_JPSI <I> INITIALIZING FOR  PSI-prime 3.685 GEV'
         PRINT *,' '
         LOGPSIP = .TRUE.
         IJOP = 2
      ENDIF

C     INITIALIZATION

 
      IF(PKINE(2).GE.1.0)THEN
         WRITE(6,'(/,A,F9.3,A,F9.3,/,A,F9.3,A,/,a,2f9.3,/,a,2f9.3)')
     1            '  RV_PHI: LOW THETA LIMIT',
     2            PKINE(3),'  HIGH THETA LIMIT',PKINE(4),
     3            '  MINIMUM TOTAL MOMENTUM ',PKINE(5),' GeV/c',
     4            '  LOW HIGH PHI LIMIT ARM 1',PKINE(6),PKINE(7),
     5            '  LOW HIGH PHI LIMIT ARM 2',PKINE(8),PKINE(9)
         IF(PKINE(2).EQ.1.0)THEN
            LOGONE = .TRUE.
            LOGTWO = .FALSE.
            WRITE(6,'(A,/)')' REQUIRE ONLY ONE PARTICLE'
         ELSE
            LOGONE = .FALSE.
            LOGTWO = .TRUE.
            WRITE(6,'(A,/)')' REQUIRE BOTH PARTICLES'
         ENDIF
      ELSE
         LOGONE = .FALSE.
         LOGTWO = .FALSE.
         WRITE(6,'(/,A,/)')'  RV_PHI: ALL ANGLES ACCEPTED'
      ENDIF  ! check on kinematics filter
 
      NRVJPSI_EVT = 0
 
      NFIRST=2


C     Y rapidity from 0 to 5
C     P_T transverse momentum from 0 to 5 GeV/c

      CALL HBFUN2(78,'HRNMD2 for RV_JPSI',200,0.,5.,
     +            200,0.,5.,RV_JPSI_FUNC)
      CALL HSCALE(78,0.0)

      IF(.NOT.HEXIST(61))CALL HBOOK1(61,'CUTPTRN',100,
     1                                 0.,7.,0.)
      IF(.NOT.HEXIST(62))CALL HBOOK1(62,'CUTETOT',100,
     1                                 0.,100.,0.)
      IF(.NOT.HEXIST(63))CALL HBOOK1(63,'CUTPTOT',100,
     1                                 0.,100.,0.)
      IF(.NOT.HEXIST(64))CALL HBOOK1(64,'CUTPZ',100,
     1                                 0.,100.,0.)
      IF(.NOT.HEXIST(65))CALL HBOOK1(65,'CUTRAPID',200,
     1                                 -4.,4.,0.)
  
      IF(.NOT.HEXIST(66))CALL HBOOK1(66,'THETA',180,
     1                                 0.,180.,0.)
      IF(.NOT.HEXIST(67))CALL HBOOK1(67,'LOWTH',100,
     1                                 0.,5.0,0.)


C     ORIGINAL DISTRIBUTIONS BEFORE FILTER CUTS

      IF(.NOT.HEXIST(68))CALL HBOOK1(68,'PTRAN',100,
     1                                 0.,7.,0.)
      IF(.NOT.HEXIST(69))CALL HBOOK1(69,'RAPIDITY',200,
     1                                 -4.,4.,0.)


C     END INITIALIZATION

 
5     CONTINUE   ! BRANCH POINT FOR BACKWARD JUMP IN KINEMATICS FILTER
 
      NRVJPSI_EVT = NRVJPSI_EVT + 1

C     CYCLE OVER REQUESTED J/PSI

      CALL HRNDM2(78,RAPID,PTRN)                      ! pickup rapidity and transverse momentum
      CALL GRNDM(TRAN,1)
      IF(TRAN.LT.0.5)RAPID=-RAPID                     ! choose positive or negative rapidity
      CALL HFILL(68,PTRN,0.,1.)                       ! original distribution before filter cuts
      CALL HFILL(69,RAPID,0.,1.)                      ! original distribution before filter cuts
      MTRAN = SQRT(AMSQ + PTRN*PTRN)                  ! transverse mass
      P1(3) = MTRAN*(0.5*(EXP(RAPID) - EXP(-RAPID)))  ! Z component of J/Psi momentum
      PTOT=SQRT(P1(3)*P1(3)+PTRN*PTRN)
      ETOTAL = SQRT(PTOT*PTOT + AMSQ)
      THEDEG=ACOSD(P1(3)/PTOT)
      P1(1)=PTOT*SIND(THEDEG)*COS(PHI)
      P1(2)=PTOT*SIND(THEDEG)*SIN(PHI)         ! MOM. COMPONENTS
      CALL EULER(P1,PHIC,THC,0.,1)             ! ROTATE IN GEANT FRAME
      CALL GRNDM(TRAN,1)
      PHI=6.283185*TRAN                         ! random azimuth from 0 to 2pi radians
      P1(1)=PTRN*COS(PHI)
      P1(2)=PTRN*SIN(PHI)
      MXTOT = 1
      IF(LOGONE.OR.LOGTWO)THEN

C     RV_JPSI2 WILL DECAY THE PHI
C     PD1,PD2 ARE THE THREE MOMENTA OF THE DECAY DAUGHTERS

         CALL RV_JPSI2(PTOT,IEOM,IJOP,P1,PD1,PD2)
         PTOT1 = SQRT(PD1(1)*PD1(1) + PD1(2)*PD1(2) +
     1                PD1(3)*PD1(3))
         PTOT2 = SQRT(PD2(1)*PD2(1) + PD2(2)*PD2(2) +
     1                PD2(3)*PD2(3))
         IF(LOGONE)THEN
            IF(PTOT1.LT.PKINE(5).AND.PTOT2.LT.PKINE(5))GO TO 5
         ELSE
            IF(PTOT1.LT.PKINE(5).OR.PTOT2.LT.PKINE(5))GO TO 5
         ENDIF

c   phi convention is -22.5 to 67.5 for first arm  (CFM: off by 11.25 deg)
c                     112.5 to 202.5 for second arm (CFM: off by 11.25 deg) 

         phi1 = atan2d(pd1(2),pd1(1))
         phi2 = atan2d(pd2(2),pd2(1))
         if(phi1.lt.-90.0)phi1 = 360.0 + phi1
         if(phi2.lt.-90.0)phi2 = 360.0 + phi2
         iphiacc1=0
         iphiacc2=0
c  check arm 1 acceptance
           if(phi1.gt.pkine(6).and.phi1.lt.pkine(7))iphiacc1=1
           if(phi2.gt.pkine(6).and.phi2.lt.pkine(7))iphiacc2=1
c  check arm 2 acceptance
           if(phi1.gt.pkine(8).and.phi1.lt.pkine(9))iphiacc1=1
           if(phi2.gt.pkine(8).and.phi2.lt.pkine(9))iphiacc2=1

           IF(LOGONE)THEN
             IF(IPHIACC1.EQ.0.AND.IPHIACC2.EQ.0)GO TO 5
           ELSE
             IF(IPHIACC1.EQ.0.OR.IPHIACC2.EQ.0)GO TO 5
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
            MXTOT = 2
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
         ENDIF
      ELSE

C      NO KINEMATICS FILTER REQUESTED

         IF(LOGPSIP)THEN
            IF(LOGELE)THEN
               IDTOT(1) = PP_PSIP_EE    ! NEW DEFINITION, ORIGINALLY 62
            ELSE
               IDTOT(1) = PP_PSIP_MM    ! NEW DEFINITION, ORIGINALLY 61
            ENDIF  ! DIELECTRON OR DIMUN DECAY FOR PSI_PRIME
         ELSE
            IF(LOGELE)THEN
               IDTOT(1) = PP_JPSI_EE    ! NEW DEFINITION, ORIGINALLY 50
            ELSE
               IDTOT(1) = PP_JPSI_MM    ! NEW DEFINITION, ORIGINALLY 51
            ENDIF  ! DIELCETRON OR DIMUON DECAY FOR J/PSI
         ENDIF  ! CHECK FOR J/PSI or PSI-PRIME
         PPTOT(2,1)=P1(1)
         PPTOT(3,1)=P1(2)
         PPTOT(4,1)=P1(3)
      ENDIF   ! CHECK ON KINEMATICS FILTER REQUEST
20    CONTINUE

C     DISTRIBUTIONS AFTER FILTER CUTS

      CALL HFILL(61,PTRN,0.,1.)
      CALL HFILL(62,ETOTAL,0.,1.)
      CALL HFILL(63,PTOT,0.,1.)
      CALL HFILL(64,P1(3),0.,1.)
      CALL HFILL(65,RAPID,0.,1.)
      CALL HFILL(66,THEDEG,0.,1.)
      CALL HFILL(67,THEDEG,0.,1.)
      NTRU_EVT = IEVENT
      END_EVTFLG = .TRUE.
      NSUB_EVT = 1
      RETURN
      END
