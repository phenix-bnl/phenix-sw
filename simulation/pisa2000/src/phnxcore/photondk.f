      SUBROUTINE PHOTONDK(PTOT,AMASS,P1,PD1,PD2)
      IMPLICIT NONE

c    Author: Charles F. Maguire
c    Creation Date: May 27, 2002


C    Purpose: DECAYS THE PHOTON "MASS" INTO AN ELECTRON/POSITRON PAIR 
C             Returns the daughter photon momenta in lab frame


c     Subroutine arguments:
c          PTOT: total lab frame momentum of parent (input)
c          AMASS: mass of neutral (input)
c          P1:   momentum vector components of parent (input)
c          PD1:  momentum vector component of first daughter   (output)
c          PD2:  momentum vector component of second daughter  (output)

c     Map:
c         Called by photonForcedConversion 
c         Calls GDECA2, GLOREN, GDROT

#include "g77trigdef.inc"

c     local specifications

      INTEGER IXYZ
      integer icall/0/
      REAL PTOT
      REAL P1(3)
      REAL PD1(3)
      REAL PD2(3)
      REAL AMASS
      REAL BETA(4)
      REAL PCM(4,3)
      REAL GKIN(4,2)
      REAL ELEMASS
      PARAMETER (ELEMASS = 0.000511)
      REAL SINTH, COSTH, COSPH, SINPH, THETA, PTRAN
      REAL E0

      REAL PX1, PY1, PZ1, ET1
      REAL PX2, PY2, PZ2, ET2
      REAL PXSUM, PYSUM, PZSUM
      REAL PAIRPTOT , PAIRMASS

      save icall

c     begin execution

      CALL GDECA2(AMASS,ELEMASS,ELEMASS,PCM)
      E0 = SQRT(PTOT*PTOT + AMASS*AMASS)
      BETA(1) = 0.0
      BETA(2) = 0.0
      BETA(3) = -PTOT/E0
      BETA(4) = E0/AMASS
      COSTH = P1(3)/PTOT
      THETA = ACOSD(COSTH)
      PTRAN = SQRT(P1(1)*P1(1) + P1(2)*P1(2))
      SINTH = SQRT(ABS(1.0 - COSTH*COSTH))
      IF(SINTH.NE.0.0)THEN
         COSPH = P1(1)/PTRAN
         SINPH = P1(2)/PTRAN
      ELSE
         SINPH = 0.0
         COSPH = 1.0
      ENDIF
      DO IXYZ = 1,3
         GKIN(IXYZ,1) = PCM(IXYZ,1)
         GKIN(IXYZ,2) = PCM(IXYZ,2)
      ENDDO
      CALL GLOREN(BETA,PCM(1,1),GKIN(1,1))
      CALL GDROT(GKIN(1,1),COSTH,SINTH,COSPH,SINPH)
      CALL GLOREN(BETA,PCM(1,2),GKIN(1,2))
      CALL GDROT(GKIN(1,2),COSTH,SINTH,COSPH,SINPH)
      DO IXYZ = 1,3
         PD1(IXYZ) = GKIN(IXYZ,1)
         PD2(IXYZ) = GKIN(IXYZ,2)
      ENDDO
      if(icall.lt.0)then       ! diagnostic printout disabled
         icall = icall + 1

c     pair mass check for decay photons

         px1 = pd1(1)
         py1 = pd1(2)
         pz1 = pd1(3)
         et1 = sqrt(px1*px1 + py1*py1 + pz1*pz1 + elemass*elemass)
         px2 = pd2(1)
         py2 = pd2(2)
         pz2 = pd2(3)
         et2 = sqrt(px2*px2 + py2*py2 + pz2*pz2 + elemass*elemass)
         pxsum = px1 + px2
         pysum = py1 + py2
         pzsum = pz1 + pz2
         pairptot = sqrt(pxsum*pxsum + pysum*pysum + pzsum*pzsum)
         pairmass = (et1+et2)*(et1+et2) - pairptot*pairptot
         if(pairmass.gt.0.0)then
            pairmass = sqrt(pairmass)
         endif

         write(6,1)ptot,p1,theta,ptran,pairmass,pairptot,
     +             et1+et2,
     +             sinth, costh, sinph, cosph,
     +             pd1,pd2,beta,(gkin(ixyz,1),ixyz=1,3),
     1             (gkin(ixyz,2),ixyz=1,3),(pcm(ixyz,1),ixyz=1,3),
     2             (pcm(ixyz,2),ixyz=1,3),amass
1     format(/,2x,'ptot,p1',4e14.5,
     +       /,2x,'parent theta ', e14.5, ' deg,  ptran ',e14.5,
     +       /,2x,'pair mass ', e14.5, ',  pair mom ', e14.5,
     +       /,2x,'et1 + et2 ', e14.5,
     +       /,2x,'sinth, costh ', 2e14.5, ', sinph, cosph ', 2e14.5,
     1       /,2x,'pd1 ',3e14.5,
     2       /,2x,'pd2 ',3e14.5,
     3       /,2x,'beta ',4e14.5,
     4       /,2x,'gkin1 ',3e14.5,
     5       /,2x,'gkin2 ',3e14.5,
     6       /,2x,'pcm1 ',3e14.5,
     7       /,2x,'pcm2 ',3e14.5,
     7       /,2x,'amass ',f8.5)
      endif
      RETURN
      END
