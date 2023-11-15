#include "geant321/pilot.h"
      SUBROUTINE GDECAY2POLARIZED(XM0,XM1,XM2,PCM)
      implicit none
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Simulates two body decay process with polarized angular       *
C.    *  distribution in CMS.                                          *
C.    *                                                                *
C.    *    ==>Called by : GDECAY                                       *
C.    *       Author C.F. Maguire from original GDECA2 routine         *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gconsp.inc"

      include "guevgen.inc"

      real RADTODEG
      parameter (RADTODEG = 180.0/3.14159)

      real xm0, xm1, xm2
      real pcm(4,2), rndm(2)
      real costh, sinth, phi, e1, p1

      real afact            !  1 - rho00
      real bfact            !  3*rh00 - 1
      real bfact3           !  bfact/3
      real bfact3afact      !  bfact3/afact

      integer iTry
      real tempCube
      real cosValue
      real cosAlpha, cosAlphaCube
      real alpha, alphaTest
      real alphaRndm, rndmSum

      real dFdAlpha
      real fAlpha

      real epsilon
      parameter (epsilon = 0.0001)
      real diff

      real beta

      logical afact0  /.false./

      integer kLoop, kCount
      integer kMax
      parameter (kMax = 1000)
      integer kMax10
      parameter (kMax10 = kMax*10)
      real deltaAlpha
      parameter (deltaAlpha = 3.14159/kMax10)
      real minF,  minAlpha
      integer kMin /0/
      integer mLoop
      real maxF /-1.0E11/
      real maxRho /-1.0/

      integer iCall /1/
      integer kWrite /0/

      save iCall, afact, bfact3, bfact3afact, afact0


c     Polarized two body decay in rest frame
c     Based on Liang and Wang preprint nucl-th/0411101v1 (Nov. 25, 2004)

c     Vector mesons subject to polarization in reaction plane

c     Polarization induces an angular correlation of the decay hadron
c     with the direction of the reaction plane

c     If the angle between the decay hadron's rest frame momentum vector
c     and the diection of the reaction plane is designated as alpha, then the
c     angular correlation function is given by

c           W(alpha) = 0.75*[ (1-rho00) + (3*rho00 - 1)*cos**2(alpha) ]

c     where rho00 is a spin density matrix parameter whose value is 1/3 for
c     the case of no polarization

c     Begin execution


      if(iCall.eq.1)then
         iCall = 0
         write(6,1)IDPOLAR, RHO00
 1       format(//, ' gdecay2Polarized <I>: ',
     +   '  IDPOLAR = ',i5,',  RHO00 = ', f10.7,//)

      endif ! first call check

      bfact = 3.0*RHO00 - 1.0
      if(abs(RHO00 - 0.333333).le. 0.00001)then
         bfact = 0.0
      endif
      bfact3 = bfact/3.0

      afact = 1.0 - RHO00
      if(RHO00.eq.1.0)then
         afact0 = .true.
      else
         bfact3afact = bfact3/afact
         afact0 = .false.
      endif

C.
C.    ------------------------------------------------------------------
C.
C             Generate first decay product in CMS.

      E1=(XM0*XM0+XM1*XM1-XM2*XM2)/(2.*XM0)
      P1=SQRT(ABS((E1-XM1)*(E1+XM1)))

C     Generate two random numbers

      CALL GRNDM(RNDM,2)

C             Polarized decay angular distribution

C     First generate alpha angle (0 to pi), direction cosine of momentum w.r.t. y axis in reaction
C     plane system where the x axis is along the impact parameter and the z axis is along the beam.
C     Then generate independent beta angle (0 to 2pi) in the (x,z) plane.
C     Phi angle in (x,y) reaction plane will be atan2(cos(alpha), sin(alpha)*cos(beta)).
C     Theta angle in (x,y,z) will be acos(sin(alpha)*sin(beta)).


      beta=TWOPI*RNDM(2)    ! random azimuthal distribution in (y,z) plane

c     Correlated alpha angle distribution

      alphaRndm = 4.0*rndm(1)/3.0
      rndmSum = afact + bfact3 - alphaRndm

c     The angle alpha is a solution to the transcendental equation

c     afact*cos(alpha) + bfact3*cos^3(alpha) = afact + bfact3 - alphaRndm = rndmSum

c     If afact = 0 (meaning RHO00 = 1) then the solution is exactly obtained
c        alpha = acos[((bfact3 - alphaRndm)**1/3)/bfact3]

c     If afact is not 0, then the solution is by first order derivative correction
c     for the function f(alpha) = afact*cos(alpha) + bfact3*cos^3(alpha) - rndmSum = 0

c     alphaTest = acos[rndmSum/afact]
c     alpha = alphaTest - f(alpha)/dFdAlpha

c     where  dFdAlpha is the first derivative for f(alpha)
c     check for abs(alphaTest - alpha) less than convergence criterion 

      if(afact0)then
         tempCube = (bfact3 - alphaRndm)/bfact3
         cosValue = (abs(tempCube))**(1.0/3.0)
         if(tempCube.lt.0.0)then
            cosValue = -cosValue
         endif
         if(abs(cosValue).le.1.0)then
            alpha = acos(cosValue)
         else
            write(6,2)bfact3, alphaRndm, 
     +           (abs(tempCube))**(1.0/3.0), rho00, bfact, afact
 2          format(/,' gdecay2Polarized <F>: bfact3 ', e14.5,
     +             ',  alphaRndm ', e14.5, ',  temp ', e14.5,
     +            /, ' rho00 ', e14.5, ' bfact ', e14.5,
     +               ' afact ', e14.5)
            write(6,3)cosValue
 3          format(//,' gdecay2Polarized <F>: cosValue ', e14.5,//)
            stop ' cosValue too large, afact0 is true'
         endif
      else
         kCount = 0
         iTry = 0
         alphaTest = rndmSum/afact
         if(alphaTest.gt.1.0)then
            alphaTest = 0.999999
         endif
         if(alphaTest.lt.-1.0)then
            alphaTest = -0.999999
         endif
         alphaTest = acos(alphaTest)

         do kLoop = 1,kMax
            kCount = kCount+1

            cosAlpha = cos(alphaTest)
            alphaTest = acos(cosAlpha)  ! restrict range as 0 to pi radians

            dFdAlpha = -sin(alphaTest)*(afact +
     +           bfact*cosAlpha*cosAlpha)
            if(dFdAlpha.ne.0.0)then
               fAlpha = afact*cosAlpha +
     +              bfact3*cosAlpha*cosAlpha*cosAlpha -
     +              rndmSum
               diff = fAlpha/dFdAlpha
               alpha = alphaTest - diff
               if(kLoop.le.0.or.kLoop.gt.kMax)then
                  write(6,11)kLoop, alphaTest, alpha, diff,
     +                 rndmSum
 11               format(' kL ',i7, ' alphaTest ', e14.5,
     +                 ' alpha ', e14.5,
     +                 '  diff ', e14.5, ' rndmS ',e14.5)
               endif
               if(abs(diff).le.epsilon.or.
     +              abs(diff).le.epsilon*alpha)then
                  go to 20
               endif
               alphaTest = alpha
            else
               write(6,14)
 14            format('  gdecay2Polarized <W>: divide by 0 attempt')
               go to 15
            endif               ! divide by 0 safety check
         enddo  ! kLoop
         diff = abs(alpha - alphaTest)
         if(diff.le.(epsilon*alpha) .or.
     +      diff.le.epsilon)then
            go to 20
         endif
 15      continue


c     Iterations did not converge, so step through pi radians to find minimum

         minF = 1.0E+06
         mLoop = -1
         alpha = 0.0
         do kLoop = 1,kMax10
            cosAlpha = cos(alpha)
            fAlpha = afact*cosAlpha +
     +           bfact3*cosAlpha*cosAlpha*cosAlpha -
     +           rndmSum
            if(abs(fAlpha).lt.minF)then
               mLoop = kLoop
               minAlpha = alpha
               minF = abs(fAlpha)
            endif
            alpha = alpha + deltaAlpha
         enddo
         if(mLoop.ne.-1)then

c     For debugging purposes

            kMin = kMin + 1
            if(abs(minF).gt.maxF)then
               maxF = abs(minF)
            endif
            if(rho00.gt.maxRho)then
               maxRho = rho00
            endif
         else
            stop ' mLoop error'
         endif        
 20      continue
      endif
 
      alphaRestFrame = RADTODEG*alpha
      betaRestFrame = RADTODEG*beta

      costh = sin(alpha)*sin(beta)
      phi = atan2(cos(alpha), sin(alpha)*cos(beta))


c     rest of code is same as original isotropic GDECA2 routine
c     the (1,2,3) directions of the c.m. frame are (x,y,z) directions in the reaction plane


      IF(ABS(COSTH).GE.1.0) THEN
         COSTH=SIGN(1.,COSTH)
         SINTH=0.
      ELSE
         SINTH=SQRT((1.-COSTH)*(1.+COSTH))
      ENDIF

C             Polar co-ordinates to momentum components.

      PCM(1,1)=P1*SINTH*COS(PHI)
      PCM(2,1)=P1*SINTH*SIN(PHI)
      PCM(3,1)=P1*COSTH
      PCM(4,1)=E1

C             Generate second decay product.

      PCM(1,2)=-PCM(1,1)
      PCM(2,2)=-PCM(2,1)
      PCM(3,2)=-PCM(3,1)
      PCM(4,2)=SQRT(PCM(1,2)**2+PCM(2,2)**2+PCM(3,2)**2+XM2*XM2)

      END
