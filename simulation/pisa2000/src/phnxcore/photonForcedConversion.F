      subroutine photonForcedConversion
      implicit none

c     Original Author: Charles F. Maguire
c     Creation Date: June 16, 2002

c     Purpose: Force a photon to change to an e+e- pair

c     Method: Special particle #56 is used
c             The particle is decayed immediately with a call from GDECAY
c             The decay conserves momentum
c             The position of the decay in the Central Arm PHENIX volume is set by the user
c             The deacy products are given the decay position as their vertex.
c             The decay products have a start time given by the photon transport to the position

c     Calling map: Called by GDECAY

c     Revision History



c     Global variables

#include "gctrak.inc"
#include "gckine.inc"
#include "geant321/gcking.inc"
#include "guevgen.inc"


c     local variables

      real eleMass
      parameter (eleMass = 0.000511)
      real eleMassSq
      parameter (eleMassSq = eleMass*eleMass)

      real photonMass /0.0015/
      real photonMom(3)
      real eleMom(3)
      real posMom(3)
      real pTot

      real tran
      real rConv
      real vertStore(3)
      real flightPath
      real flightTime
      integer ntbeam
      integer nttarg
      real ubuf(1)
      integer nubuf /1/
      real tofStore
      real vertNew(3)
      real rTran

      integer i


c     begin execution


c     write(6,1)
1     format(//,'  Call to forced photon conversion',/)


c     determine the decay position (x,y,z) according to the user specifications
c     normally the user specifieds a range of transverse positions (Radial position)

c     after fixing the radial position, determine the Z position for the decay vertex
c     we need to know the initial vertex for this purpose
c     the decay vertex also determines the start time for the decay particles

      pTot = vect(7)
      photonMom(1) = vect(4)*pTot
      photonMom(2) = vect(5)*pTot
      photonMom(3) = vect(6)*pTot

      call photondk(pTot, photonMass, photonMom, eleMom, posMom)

      ngkine = 2
      do i = 1,3
         gkin(i,1) = eleMom(i)
         gkin(i,2) = posMom(i)
      enddo

      gkin(4,1) = sqrt(eleMom(1)*eleMom(1) +
     +                 eleMom(2)*eleMom(2) +
     +                 eleMom(3)*eleMom(3) +
     +                 eleMassSq)

      gkin(4,2) = sqrt(posMom(1)*posMom(1) +
     +                 posMom(2)*posMom(2) +
     +                 posMom(3)*posMom(3) +
     +                 eleMassSq)

      call grndm(tran,1)
      if(tran<0.4)then
         gkin(5,1) = 3.0        ! electron
         gkin(5,2) = 2.0        ! positron
      else
         gkin(5,2) = 3.0        ! electron
         gkin(5,1) = 2.0        ! positron
      endif


c     set the position of the decay particles
c     first determine the radial position
c     for now we assume a uniform distribution from the PHCONV command
c     later revisions can use the phConvOpt parameter for different distributions

      if(phConvRmax.lt.phConvRmin) then
         write(6,111) phConvRmax, phConvRmin
 111     format(/,'  photonForcedConversion <E>: ',
     +          /,'   maximum radius = ', f8.2,
     +          /,'   minimum radius = ', f8.2,
     +          /,'   maximum < minium ??',/)
         stop ' PISA exiting in photonForcedConversion'
      endif  ! safety check

      call grndm(tran,1)
      rconv = phConvRmin + tran*(phConvRmax - PhConvRmin);

c     get the primary vertex

      call gfvert(1, vertStore, ntbeam, nttarg, tofStore, ubuf, nubuf)

c     compute the conversion position according to the present momentum

      rTran = sqrt(vect(4)*vect(4) + vect(5)*vect(5))
      if(rTran.gt.0.0) then
         vertNew(1) = (vect(4)/rTran)*rconv
         vertNew(2) = (vect(5)/rTran)*rconv
         vertNew(3) = (vect(6)/rTran)*rconv + vertStore(3)
      else
         write(6,112)rtran
 112     format(/,' Invalid radial position ',f8.2)
         stop ' PISA exiting in photonForcedConversion'
      endif  ! safety check

      do i=1,3
         gpos(i,1) = vertNew(i)
         gpos(i,2) = vertNew(i)
      enddo


c     set the TOF delay for the decay particles


      flightPath = sqrt((vertNew(1)-vect(1))*(vertNew(1)-vect(1)) +
     +                  (vertNew(2)-vect(2))*(vertNew(2)-vect(2)) +
     +                  (vertNew(3)-vect(3))*(vertNew(3)-vect(3)))

      flightTime = flightPath/(30.0)  ! time in nanoseconds
      tofd(1) = flightTime*1.0e-09    ! GEANT uses seconds
      tofd(2) = flightTime*1.0e-09    ! GEANT uses seconds

      return
      end
