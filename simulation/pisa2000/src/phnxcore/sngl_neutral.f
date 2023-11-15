      SUBROUTINE SNGL_NEUTRAL

      implicit none

#include "g77trigdef.inc"


c     Original Author: Charles F. Maguire 
c     Creation Date: May 27, 2002
c     Based on: SNGL_NEUTRAL by Kenta Shigaki and RV_PHI by Charles F. Maguire

c     Generate pizero (7) or eta (17) particle with two photon decay (IKINE2(1) = 9 or 17)
c     uniform Y  distribution from PKINE2(1) to PKINE2(2)
c     uniform pt distribution from PKINE2(3) to PKINE2(4)

c     Filter according to value of IKINE2(2) 0 ===> no filter
c                                            1 ===> require one decay photon
c                                            2 ===> require two decay photons
c                                            3 ===> require two decay photons towards EMCal
c                                            4 ===> write out only events with
c                                                   accepted decay photons
c     Low and high phi filter limits are in PKINE2(5) and PKINE2(6)
c     Low and high theta filter limits are in PKINE2(7) and PKINE2(8)
c     Minimum momentum filter limit is in PKINE(9)

c     NEUTRAL_EVT counts the number of NEUTRALs generated before filter cuts
c     NEUTRAL_EVT (in GUEVGEN common) is printed out by GULAST at end of run

c     MAP
c       Called by GUEVGEN

#include "gcflag.inc"
#include "guevgen.inc"
#include "gckine.inc"
#include "subevt.inc"
#include "pisa_parts.inc"
#include "secubuf.inc"

c     local specifications


      INTEGER NFIRST
      INTEGER NUM_NEUTRAL, NUM_DECAY
      REAL P1(3), AMSQ, RAPID, PTRN, TRAN, PTOT, PHI, NMASS, TRAN2
      REAL TH1, TH2, PD1(3), PD2(3), PTOT1, PTOT2
      REAL PH1, PH2

      INTEGER IWRITE /0/
      INTEGER ICALL
      INTEGER PRECALL
      INTEGER ZUNIFORM /0/
      REAL DZWIDTH
      REAL OSCARPARTICLE(10)


c     limits of EMCal acceptance
c     expand THETA limits by +/-5 degrees to allow for not having Z0 = 0 vertex

      REAL THEMIN /70.0/     ! degrees (adjust event-by-event for Z0)
      REAL THEMAX /110.0/    ! degrees (adjust event-by-event for Z0)
      REAL EMCALR /500.0/    ! nominal distance for EMCal
      REAL EMCALZ /181.985/  ! Z length of EMCal at Theta = 70 degrees for R = 500 cm
      
      REAL PHWMIN, PHWMAX, PHEMIN, PHEMAX  ! degrees 
      PARAMETER (PHWMIN = -33.75)
      PARAMETER (PHWMAX = +56.25)
      PARAMETER (PHEMIN = 123.75)
      PARAMETER (PHEMAX = 213.75)
      REAL APIZERO
      PARAMETER (APIZERO = 0.134973)
      REAL AETA
      PARAMETER (AETA = 0.54745)

C     NEUTRAL MASS (GEV) SQUARED

      LOGICAL LOGONE, LOGTWO, LOGPIZ, LOGETA
 
      DATA NFIRST /1/

      REAL Y_MIN, Y_MAX
      SAVE LOGONE,LOGTWO,NFIRST, LOGPIZ, LOGETA, DZWIDTH, ZUNIFORM
      SAVE IWRITE

c     begin execution

      NUMEVT = NUMEVT + 1
      MXTOT = 2  ! One (only 1) Neutral meson is required to decay in this version
      IF(NFIRST.GT.1.AND.PKINE2(10).NE.-1.0)GO TO 5
      PKINE2(10) = 0.0
      Y_MIN = PKINE2(1)
      Y_MAX = PKINE2(2)
      DZWIDTH = PKINE2(11)
      IF(DZWIDTH.LT.0.0)THEN
         ZUNIFORM = 1
      ENDIF
      IF(DZWIDTH.GT.0.0)THEN
         ZUNIFORM = -1
      ENDIF

      NUM_NEUTRAL = 1  ! Allow only one Neutral meson per event in this version
      NUM_DECAY = NUM_NEUTRAL + NUM_NEUTRAL  ! counts number of decay particles

C     INITIALIZATION

      CALL OPENOSCARFILE(0, 0)
      OSCARPARTICLE(2) = 0          ! pnum, counts from 0
      OSCARPARTICLE(3) = IKINE2(1)  ! pid
      IF(IKINE2(1).EQ.7)THEN
         PRINT *,' '
         PRINT *,'  SNGL_NEUTRAL <I> FOR PIZERO DECAY'
         PRINT *,' '
         LOGPIZ = .TRUE.
         LOGETA = .FALSE.
         NMASS = APIZERO
         AMSQ = APIZERO*APIZERO
         CHEVT_NAME = ' SNGL_NEUTRAL PIZERO DECAY'
      ENDIF
      IF(IKINE2(1).EQ.17)THEN
         PRINT *,' '
         PRINT *,'  SNGL_NEUTRAL <I> FOR ETA DECAY'
         PRINT *,' '
         LOGPIZ = .FALSE.
         LOGETA = .TRUE.
         NMASS = AETA
         AMSQ = AETA*AETA
         CHEVT_NAME = ' SNGL_NEUTRAL ETA DECAY'
      ENDIF

      WRITE(6,'(A,/,A,F9.3,A,F9.3,/,A,F9.3,A,F9.3)')
     1            '  SNGL_NEUTRAL:',
     2            '  RAPIDITY OF PARENT ',PKINE2(1),' TO ',PKINE2(2),
     3            '  PT OF PARENT       ',PKINE2(3),' TO ',PKINE2(4)
      IF(IKINE2(2).GE.1)THEN
         WRITE(6,'(A,F9.3,A,F9.3,/)')
     1            '  PHI OF DAUGHTER  ',PKINE2(5),' TO ',PKINE2(6)
         WRITE(6,'(A,F9.3,A,F9.3,/,A,F9.3)')
     1            '  THETA OF DAUGHTER  ',PKINE2(7),' TO ',PKINE2(8),
     2            '  MINIMUM MOMENTUM OF DAUGHTER ABOVE ',PKINE2(9)
         IF(IKINE2(2).EQ.1)THEN
            LOGONE = .TRUE.
            LOGTWO = .FALSE.
            WRITE(6,'(A,/)')'  REQUIRE ONLY ONE PARTICLE'
         ELSEIF(IKINE2(2).EQ.2)THEN
            LOGONE = .FALSE.
            LOGTWO = .TRUE.
            WRITE(6,'(A,/)')'  REQUIRE BOTH PARTICLES IN ANGLES'
         ELSEIF(IKINE2(2).EQ.3 .or. IKINE2(2).EQ.4)THEN
            LOGONE = .FALSE.
            LOGTWO = .TRUE.
         ELSE
            LOGONE = .FALSE.
            LOGTWO = .TRUE.
            WRITE(6,'(A,/)')'  REQUIRE BOTH PARTICLES ENTER EMCAL'
         ENDIF
      ELSE
         LOGONE = .FALSE.
         LOGTWO = .FALSE.
         WRITE(6,'(/,A,/)')'  SNGL_NEUTRAL: ALL DECAYS ACCEPTED'
      ENDIF  ! check on kinematics filter

      NEUTRAL_EVT = 0
      NFIRST=2

      IF(IKINE2(3).GT.0)THEN
         PRECALL = 1000000*IKINE2(3)
         WRITE(6,11)PRECALL
 11      FORMAT(//,'  SNGL_NEUTRAL <I>: GRNDM PRE-CALL = ',i10,/)
         DO ICALL = 1, PRECALL
            CALL GRNDM(TRAN,1)
         ENDDO
      ENDIF  ! PRE-CALLS TO GRNDM

C     END INITIALIZATION

5     CONTINUE
      NEUTRAL_EVT = NEUTRAL_EVT + 1

      OSCARPARTICLE(1) = NEUTRAL_EVT

      CALL GRNDM(TRAN,1)
      RAPID=PKINE2(1)+(PKINE2(2)-PKINE2(1))*TRAN
      CALL GRNDM(TRAN,1)
      PTRN =PKINE2(3)+(PKINE2(4)-PKINE2(3))*TRAN
      CALL GRNDM(TRAN,1)
      PHI=6.28319*TRAN
      P1(1)=PTRN*COS(PHI)
      P1(2)=PTRN*SIN(PHI)
      P1(3)=SQRT(AMSQ+PTRN*PTRN)*SINH(RAPID)
      PTOT=SQRT(P1(3)*P1(3)+PTRN*PTRN)

      OSCARPARTICLE(4) = P1(1)
      OSCARPARTICLE(5) = P1(2)
      OSCARPARTICLE(6) = P1(3)
      OSCARPARTICLE(7) = SQRT(AMSQ + PTOT*PTOT)

      XYZ(1) = 0.0
      XYZ(2) = 0.0
      XYZ(3) = 0.0
      IF(ZUNIFORM.EQ.1)THEN
         CALL GRNDM(TRAN,1)
         XYZ(3) = 2.0*DZWIDTH*(TRAN-0.5)
      ENDIF
      IF(ZUNIFORM.EQ.-1)THEN
         CALL GRANOR(TRAN, TRAN2)
         XYZ(3) = DZWIDTH*TRAN
      ENDIF

      IF(ZUNIFORM.NE.0)THEN

C     Recompute THEMIN and THEMAX for THIS Z0

         THEMIN = ATAND(EMCALR/(EMCALZ - XYZ(3)))
         THEMAX = 180.0 - ATAND(EMCALR/(EMCALZ + XYZ(3)))
      ENDIF

      OSCARPARTICLE(8) = XYZ(1)*1.0E+13
      OSCARPARTICLE(9) = XYZ(2)*1.0E+13
      OSCARPARTICLE(10) = XYZ(3)*1.0E+13

      CALL FILLOSCARFILE(OSCARPARTICLE)

      IF(LOGONE.OR.LOGTWO)THEN

C     NEUTDK WILL DECAY THE NEUTRAL MASS INTO TWO PHOTONS
C     PD1,PD2 ARE THE THREE MOMENTA OF THE DECAY DAUGHTERS

         CALL NEUTDK(PTOT,NMASS,P1,PD1,PD2)
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
         ELSE
            TH1 = 0.0
         ENDIF
         PH1 = 90.0  ! default not in the acceptance
         IF(PTOT1.GT.0.0.AND.PD1(1).NE.0.)THEN
            PH1 = ATAN2D(PD1(2), PD1(1))
            IF(PH1.LT.-90.0)THEN
               PH1 = 360.0 + PH1
            ENDIF
         ENDIF
         IF(PTOT2.GT.0.0)THEN
            TH2 = ACOSD(PD2(3)/PTOT2)
         ELSE
            TH2 = 0.0
         ENDIF
         PH2 = 90.0  ! default not in the acceptance
         IF(PTOT2.GT.0.0.AND.PD2(1).NE.0.)THEN
            PH2 = ATAN2D(PD2(2), PD2(1))
            IF(PH2.LT.-90.0)THEN
               PH2 = 360.0 + PH2
            ENDIF
         ENDIF
         IF((LOGONE.AND.TH1.GE.PKINE2(7).AND.TH1.LE.PKINE2(8).AND.
     +       PH1.GT.PKINE2(5).AND.PH1.LE.PKINE2(6)).OR.
     +      (LOGONE.AND.TH2.GE.PKINE2(7).AND.TH2.LE.PKINE2(8).AND.
     +       PH2.GT.PKINE2(5).AND.PH2.LE.PKINE2(6)).OR.
     +      (LOGTWO.AND.TH2.GE.PKINE2(7).AND.TH2.LE.PKINE2(8).AND.
     +       TH1.GE.PKINE2(7).AND.TH1.LE.PKINE2(8).AND.
     +       PH1.GT.PKINE2(5).AND.PH1.LE.PKINE2(6).AND.
     +       PH2.GT.PKINE2(5).AND.PH2.LE.PKINE2(6)))THEN
            IF(IKINE2(2).EQ.3.OR.IKINE2(2).EQ.4)THEN

c     CHECK THAT BOTH PHOTONS ARE WITHIN EMCal ANGLES

               IF(TH1.LT.THEMIN.OR.TH1.GT.THEMAX.OR.
     +            TH2.LT.THEMIN.OR.TH2.GT.THEMAX)THEN
                  GO TO 5  ! AT LEAST ONE PHOTON MISSED ONE OF THE THETA LIMITS
               ENDIF  ! check Theta Limits
               IF(((PH1.GE.PHWMIN.AND.PH1.LE.PHWMAX).OR.
     +            (PH1.GE.PHEMIN.AND.PH1.LE.PHEMAX)).AND.
     +            ((PH2.GE.PHWMIN.AND.PH2.LE.PHWMAX).OR.
     +            (PH2.GE.PHEMIN.AND.PH2.LE.PHEMAX)))THEN
                  IF(IWRITE.LT.11)THEN
                     IWRITE = IWRITE + 1
                     write(6,7)th1, ph1, th2, ph2, xyz(3),
     +                         themin, themax
 7                format(' Photon1 th1 ',f8.3,', ph1 ',f9.3,
     +                   ';  Photon2  th2 ',f8.3,', ph2 ',f9.3,
     +                   '   Z = ', f8.2, ',  THMN ', f8.4,
     +                   ',  THMX ',f8.4)
                  ENDIF   ! check on writing information
                  GO TO 6  ! BOTH PHOTONS WITHIN EAST OR WEST PHI LIMITS
               ELSE
                  GO TO 5
               ENDIF
 6             CONTINUE  ! BRANCH POINT WHEN EMCAL ANGLE LIMITS ARE SATISFIED
            ENDIF
            IF(LOGPIZ)THEN
               IDTOT(1) = 1
               IDTOT(2) = 1
               ID_PARENT(1) = 7
               ID_PARENT(2) = 7
            ENDIF
            IF(LOGETA)THEN
               IDTOT(1) = 1
               IDTOT(2) = 1
               ID_PARENT(1) = 17
               ID_PARENT(2) = 17
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
         STOP ' SNGL_NEUTRAL requires Neutral to decay into a filter'
      ENDIF  ! check on requiring one or two decay particles in filter

20    CONTINUE  ! BRANCH POINT FOR ACCEPTING THE TWO PHOTONS

      NTRU_EVT = IEVENT
      END_EVTFLG = .TRUE.
      NSUB_EVT = 1
      
      RETURN
      END
