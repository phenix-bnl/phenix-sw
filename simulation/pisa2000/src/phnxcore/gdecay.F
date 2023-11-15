      SUBROUTINE GDECAY

      implicit none

C.  S.R.Tonse: took new GDECAY code from Cern, applied cmz update cradle
C   and then put in Pisa-specific lines that call DALDEC1 if a pi0 is
C   having a 3-body decay. 15th Apr 1993.
C.    ******************************************************************
C.    *                                                                *
C.    *  Control routine for generation of particle decays.            *
C.    *                                                                *
C.    *  MODE(I)        Ith decay mode of current particle            *
C.    *  BRATIO(I)      branching ratio for Ith decay mode of         *
C.    *                 current particle.                              *
C.    *                                                                *
C.    *    ==>Called by : GHSTOP,GTHADR,GTNEUT,GTMUON                  *
C.    *       Author    G.Patrick *********                            *
C.    *                                                                *
C.    ******************************************************************
C.
#include "gcbank.inc"
#include "gctrak.inc"
#include "gconsp.inc"
#include "geant321/gcking.inc"
#include "gckine.inc"
#include "gcunit.inc"
#include "gcphys.inc"
#include "secubuf.inc"
#include "guevgen.inc"
#include "g77trigdef.inc"

      integer mode, ntype
      integer locatf
      integer k, jp1, jp2, jp3, mxx, id, i, nbr
      integer jdk1, jdk2, jpa

      integer kpart, ixyz,  kOffset

      integer icall /0/
      integer np94
      parameter (np94 = 33)
      character*8 ch94(np94) /'PXREAC1R', 'PYREAC1R', 'PZREAC1R',
     +     'PXREAC2R', 'PYREAC2R', 'PZREAC2R',
     +     'ALPHA1', 'BETA1', 'PHI1R', 'THETA1R',
     +     'ALPHA2', 'BETA2', 'PHI2R', 'THETA2R',
     +     'PXREAC1L', 'PYREAC1L', 'PZREAC1L', 'PHI1L', 'THETA1L',
     +     'PXREAC2L', 'PYREAC2L', 'PZREAC2L', 'PHI2L', 'THETA2L',
     +     'PXPARNTL', 'PYPARNTL', 'PZPARNTL', 'PTPARNL',
     +     'RHO00', 'V2CONST', 'RAPIDITY', 'RESTMASS',
     +     'EVENT'/
      real evt94(np94)
    
      real barea, beta, bratio, pcm, xm, rndm
      real sinph, cosph, sinth, costh, e0, p0, rmass
      real xmtot, brand, bradd, dmass, rapidity

      real RADTODEG
      parameter (RADTODEG = 180.0/3.14159)
      real pxreac, pyreac, ptran, ptot, pyz, pxlab, pylab

      DIMENSION BAREA(7)
      DIMENSION BETA(4)
      DIMENSION BRATIO(6)
      DIMENSION MODE(6)
      DIMENSION NTYPE(3)
      DIMENSION PCM(4,3)
      DIMENSION XM(3)
      DIMENSION RNDM(1)
      LOGICAL ROTATE
      DOUBLE PRECISION DUX, DUY, DUZ, DSITH, DSITH2, ONE
      PARAMETER (ONE=1)

      integer iFirst /1/


c     begin execution

      if(ipart.eq.7)then
         nubuf = 1
         ubuf(1) = itra
      endif  ! save parent track information

C             Search for parent particle in decay list.

      KCASE   = NAMEC(5)
      NGKINE  = 0
      IF(IDCAY.EQ.2) THEN
         DESTEP = DESTEP+GETOT
         GO TO 99
      ENDIF
      DMASS = AMASS
      JPA = LQ(JPART-IPART)
      JDK1 = LQ(JPA-1)
      JDK2 = LQ(JPA-2)
      IF (JDK1.LE.0)                               GO TO 90
      IF (JDK2.LE.0)                               GO TO 90
      DO 5 I=1,6
         BRATIO(I)=Q(JDK1+I)
         MODE(I)=IQ(JDK2+I)
   5  CONTINUE

C             Generate branching ratio and select decay mode.

      NBR      = 1
      BAREA(1) = 0.
      DO 10 I=2,7
         BRADD    = BRATIO(I-1)
         IF (BRADD.EQ.0.) GO TO 20
         NBR      = NBR+1
         BAREA(I) = BAREA(I-1)+BRADD
  10  CONTINUE

  20  CALL GRNDM(RNDM,1)
      BRAND    = 100.*RNDM(1)
      IF (BRAND.GE.BAREA(NBR)) GO TO 99
      ID       = IABS((LOCATF(BAREA,NBR,BRAND)))

C             Unpack decay mode.

      MXX      = MODE(ID)
      NTYPE(1) = MOD(MXX,100)
      NTYPE(2) = MOD(MXX/100,100)
      JP1 = LQ(JPART-NTYPE(1))
      JP2 = LQ(JPART-NTYPE(2))
      XM(1) = Q(JP1+7)
      XM(2) = Q(JP2+7)
      IF (MXX.LT.10000)THEN

C             Two body decay.

         if(ipart.eq.56)then

c          Special photon which is forced to decay in PHENIX volume
c          All the boost, rotations, and GKIN handled in the called routine
c          So we exit after the return via statement 99

           call photonForcedConversion
           go to 99
         endif  ! check on forced photon conversion particle
         NGKINE  = 2
         if(iFirst.eq.1)then
            iFirst = 0
            
            write(6,39) DMASS, LorentzDisable, TLIFE, IPART
 39         format(
     +        'gdecay - ',2x,/,
     +        'gdecay - DMASS = ', e14.5,/,
     +        'gdecay - LorentzDisable = ',i2,/,
     +        'gdecay - TLIFE = ', e14.5,/,
     +        'gdecay - IPART = ',i5)
                  
         endif  ! check on first call
         IF (TLIFE.LT.1.E-15.AND.(LorentzDisable.eq.0.OR.
     +       IPART.LT.50)) THEN
            XMTOT = XM(1)+XM(2)
            DO 30 I=1,1000
C--  Create Lorentz distributed energy with FWHM HBAR/TLIFE.
C--  (via integral-transformation of Lorentz-distribution)
C--                 (M.Guckes)
              CALL GRNDM(RNDM,1)
              RMASS = DMASS
     1                + 3.291086E-25/TLIFE * TAN(PI*(RNDM(1)-0.5))
              IF (RMASS.GE.XMTOT) GO TO 40
 30         CONTINUE
            WRITE(CHMAIL,1000) IPART, NTYPE(1), NTYPE(2)
            CALL GMAIL(0,0)
            NGKINE=0
            GO TO 99
 40         DMASS = RMASS
         END IF
         if(ipart.eq.idpolar.or.idpolar.eq.-1)then
            icall = icall + 1
            if(icall.eq.1)then
               call hbookn(94, 'Rest Frame Momenta', np94, 'GEANHIST',
     +              25000, ch94)
            endif
            if(RHO000.lt.0.0)then
               RHO00 = -RHO000
            else

c     RHO00 depends upon the PT of the parent meson

               ptran = vect(7)*sqrt(vect(4)*vect(4) + vect(5)*vect(5))
               RHO00 = RHO000 + 
     +              (1./3. - RHO000)*(2.0/3.14159)*atan(ptran/0.5)
               if(abs(RHO000 - 0.333333).le.1.e-05)then
                  RHO00 = 1.0/3.0
               endif            ! no polarization limit independent of PT
            endif  ! check if fixed RHO00

c     The rest frame axes are assumed to be the (x,y) of the reaction plane
c     with the z along the beam axis

c     So we must rotate this (x,y) rest frame to the lab (x,y) frame before boosting to the lab

            call gdecay2Polarized(dmass, xm(1), xm(2), pcm)
            evt94(1) = pcm(1,1)
            evt94(2) = pcm(2,1)
            evt94(3) = pcm(3,1)
            evt94(4) = pcm(1,2)
            evt94(5) = pcm(2,2)
            evt94(6) = pcm(3,2)
            ptran = sqrt(evt94(1)*evt94(1) + evt94(2)*evt94(2))
            ptot = sqrt(ptran*ptran + evt94(3)*evt94(3))
            pyz = sqrt(ptot*ptot - evt94(1)*evt94(1))
            if(ptot.gt.0.0)then
               evt94(7) = alphaRestFrame
               evt94(8) = betaRestFrame
               evt94(9) = atan2d(evt94(2), evt94(1))
               evt94(10) = acosd(evt94(3)/ptot)
            endif
            ptran = sqrt(evt94(4)*evt94(4) + evt94(5)*evt94(5))
            ptot = sqrt(ptran*ptran + evt94(6)*evt94(6))
            pyz = sqrt(ptot*ptot - evt94(4)*evt94(4))
            if(ptot.gt.0.0)then
               evt94(11) = acosd(evt94(5)/ptot)
               evt94(12) = atan2d(evt94(6), evt94(5))
               evt94(13) = atan2d(evt94(5), evt94(4))
               evt94(14) = acosd(evt94(6)/ptot)
            endif

c     Rotate the reaction plane (x,y) frame to the lab (x,y) frame

            do kpart = 1,2
               pxreac = pcm(1,kpart)
               pyreac = pcm(2,kpart)
               pcm(1,kpart) = pxreac*cosd(reactionPlaneAngle) -
     +                        pyreac*sind(reactionPlaneAngle)
               pcm(2,kpart) = pxreac*sind(reactionPlaneAngle) +
     +                        pyreac*cosd(reactionPlaneAngle)
            enddo            
         else
            CALL GDECA2(DMASS,XM(1),XM(2),PCM)
         endif  ! check if special polarized decay particle
      ELSE

C             Three body decay.


c (7 <--> PI0; 1067 <--> pi0 that decays only to Dalitz)

      IF(IPART .EQ. 7 .OR. IPART .EQ. 1067) THEN
C  takes care of 3 body PI0 Dalitz decay. Boost, rotations & GKIN filling are
C  also done inside this routine, so just exit when you come out. SRT.
          CALL DALDEC1
        GO TO 99
      ELSE IF(IPART .EQ. 58 .OR. (IPART.EQ.17.AND.MXX.EQ.10302)) THEN
c (58 <--> eta decays only to Dalitz or gamma+gamma)
c (17 <--> eta. 10302 is "gamma(1)"+"e-(3)"+"e+(2)")

          CALL DALDEC2
          GOTO 99
      ELSE
         NTYPE(3) = MXX/10000
         NGKINE  = 3
         JP3 = LQ(JPART-NTYPE(3))
         XM(3) = Q(JP3+7)
         IF (TLIFE.LT.1.E-15) THEN
            XMTOT = XM(1)+XM(2)+XM(3)
            DO 31 I=1,1000
C--  Create Lorentz distributed energy with FWHM HBAR/TLIFE.
C--  (via integral-transformation of Lorentz-distribution)
              CALL GRNDM(RNDM,1)
              RMASS = DMASS
     1                + 3.291086E-25/TLIFE * TAN(PI*(RNDM(1)-0.5))
              IF (RMASS.GE.XMTOT) GO TO 41
 31         CONTINUE
            WRITE(CHMAIL,1000) IPART, NTYPE(1), NTYPE(2), NTYPE(3)
            CALL GMAIL(0,0)
            NGKINE=0
            GO TO 99
 41         DMASS = RMASS
         END IF
         CALL GDECA3(DMASS,XM(1),XM(2),XM(3),PCM)
      ENDIF                                     !if pi0
      ENDIF

C             LORENTZ boost into LAB system defined along parent vector
C             followed by rotation back into GEANT system.

      P0       = VECT(7)
      E0       = SQRT(P0*P0+DMASS*DMASS)
      BETA(1)  = 0.
      BETA(2)  = 0.
      BETA(3)  = -P0/E0
      BETA(4)  = E0/DMASS
      ROTATE   =.TRUE.
      DUX      = VECT(4)
      DUY      = VECT(5)
      DUZ      = VECT(6)
      if(ipart.eq.idpolar.or.idpolar.eq.-1)then

c     For the special polarized particle decays the PCM coordinates
c     point in the lab frame orientation
c     So we only have to boost with the parent beta, and we do
c     not have to do any rotation

         ROTATE = .FALSE.

c     Convert the BETA along the parent direction
c     to the components in the lab frame

         BETA(1) = BETA(3)*DUX
         BETA(2) = BETA(3)*DUY
         BETA(3) = BETA(3)*DUZ
      endif
      COSTH    = DUZ
      IF(ABS(COSTH).GE.0.85) THEN
         DSITH2=DUX**2+DUY**2
         IF(DSITH2.GT.0.) THEN
            COSTH  = SIGN(1.,COSTH)*SQRT(ONE-DSITH2)
            DSITH  = SQRT(DSITH2)
            SINTH  = DSITH
            COSPH  = DUX/DSITH
            SINPH  = DUY/DSITH
         ELSEIF(COSTH.GE.1.)THEN
            ROTATE = .FALSE.
         ELSE
            COSTH = -1.
            SINTH =  0.
            COSPH =  1.
            SINPH =  0.
         ENDIF
      ELSE
         DSITH  = SQRT((ONE+DUZ)*(ONE-DUZ))
         SINTH  = DSITH
         COSPH  = DUX/DSITH
         SINPH  = DUY/DSITH
      ENDIF

      DO 60 K=1,NGKINE
         IF (P0.LE.0.) THEN
            DO 59 I = 1,3
   59       GKIN(I,K) = PCM(I,K)
         ELSE
            CALL GLOREN (BETA, PCM(1,K), GKIN(1,K))
         ENDIF
         IF(ROTATE) CALL GDROT  (GKIN(1,K),COSTH,SINTH,COSPH,SINPH)
         GKIN(4,K)=SQRT(GKIN(1,K)**2+GKIN(2,K)**2+GKIN(3,K)**2+XM(K)**2)
         GKIN(5,K)=NTYPE(K)
         TOFD(K)=0.
         if(ipart.eq.idpolar.or.idpolar.eq.-1)then

c     Transform boosted lab PX and PY to boosted reaction plane PX and PY

            kOffset = 5*(k-1)
            pxlab = gkin(1,k)
            pylab = gkin(2,k)
            pxreac = pxLab*cosd(reactionPlaneAngle) +
     +               pyLab*sind(reactionPlaneAngle)
            pyreac = -pxLab*sind(reactionPlaneAngle) +
     +               pyLab*cosd(reactionPlaneAngle)
            evt94(15+kOffset) = pxreac
            evt94(16+kOffset) = pyreac
            evt94(17+kOffset) = gkin(3,k)
            ptran = sqrt(pxreac*pxreac + pyreac*pyreac)
            ptot = sqrt(ptran*ptran +
     +                  evt94(17+kOffset)*evt94(17+kOffset))
            if(ptot.gt.0.0)then
               evt94(18+kOffset) = RADTODEG*atan2(pyreac, pxreac)
               evt94(19+kOffset) = RADTODEG*acos(evt94(17+kOffset)/ptot)
            endif
            if(k.eq.2)then
               ptran = p0*sinth
               evt94(25) = ptran*cosph
               evt94(26) = ptran*sinph
               evt94(27) = p0*costh
               evt94(28) = ptran
               evt94(29) = RHO00
               rapidity = 0.5*log((e0 + evt94(27))/
     +                            (e0 - evt94(27)))
               if(v2const.gt.-0.99)then
                  evt94(30) = v2const
               endif
               if(abs(v2const + 1.0).lt.0.05)then
                  evt94(30)=0.22/(1.0+exp(-5.0*(evt94(28)/2.0 - 0.35)))
     +                        - 0.06
               endif  ! recombination model
               if(abs(v2const + 2.0).lt.0.05)then
                  evt94(30) = 1.5*(0.25 - 0.01*(ptran - 5.0)*
     +                                         (ptran - 5.0))*
     +                        exp(-0.5*(rapidity*rapidity/6.25))
               endif
               evt94(31) = rapidity
               evt94(32) = dmass
               evt94(33) = icall
c              call hfn(94, evt94) ! store values for diagnostic
               call fillpairpolarfile(evt94)
            endif
         endif

C	GEANT 3.21 additions

	 GPOS(1,K) = VECT(1)
         GPOS(2,K) = VECT(2)
         GPOS(3,K) = VECT(3)
  60  CONTINUE
      GO TO 99

C             No branching ratio defined. Call user routine

  90  CALL GUDCAY

  99  RETURN
 1000 FORMAT(' ***** GDECAY ERROR : Not enough energy available for ',
     +       'decay of resonance',I3,' to',3I3,'; no decay.')
      END
