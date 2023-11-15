*-- Author :    Charles F. Maguire   11/06/93
      subroutine xy_rndm

      implicit none

#include "g77trigdef.inc"


c     PURPOSE: Randomize the transverse momentum of the particles
c              as requested

c        IXY_RNDM = 1 ===> same random phi angle for all particles
c        IXY_RNDM = 2 ===> different phi angle each particle

#include "guevgen.inc"

c    local variables

      integer kpart    ! do loop index from 1 to mxtot
      real    phi_rndm ! random phi angle (degrees)
      real    rndm     ! external function
      real    ptran    ! transverse momentum
      real    cosrnd   ! fixed cosine
      real    sinrnd   ! fixed sine

c     begin execution

      if(ixy_rndm.ne.1.and.ixy_rndm.ne.2)then
         write(6,1)ixy_rndm
1        format(/,2x,'IXY_RNMD <E>: called with IXY_RNDM = ',
     1               i6,/,' PISA stopping')
         stop '  programming error ?'
      endif
      if(ixy_rndm.eq.1)then

c     All particles rotated to the same angle

         phi_rndm = 360.0*rndm(0)
         cosrnd = cosd(phi_rndm)
         sinrnd = sind(phi_rndm)
         do kpart = 1,mxtot
            ptran = pptot(2,kpart)*pptot(2,kpart) +
     1              pptot(3,kpart)*pptot(3,kpart)
            if(ptran.gt.0.0)then  ! should always be >=0.0
               ptran = sqrt(ptran)
            else

c     some mahines will screw up sqrt(0)

               ptran = 0.0
            endif  ! saftey check
            pptot(2,kpart) = ptran*cosrnd
            pptot(3,kpart) = ptran*sinrnd
         enddo  ! loop over all particles
      else

c    Each particle rotated to a different angle

         do kpart = 1,mxtot
            phi_rndm = 360.0*rndm(kpart)   ! using CERNs suggestion
            ptran = pptot(2,kpart)*pptot(2,kpart) +
     1              pptot(3,kpart)*pptot(3,kpart)
            if(ptran.gt.0.0)then  ! should always be >=0.0
               ptran = sqrt(ptran)
            else

c     some mahines will screw up sqrt(0)

               ptran = 0.0
            endif  ! saftey check
            pptot(2,kpart) = ptran*cosd(phi_rndm)
            pptot(3,kpart) = ptran*sind(phi_rndm)
         enddo  ! loop over all particles
      endif  ! check on ixy_rndm = 1 or 2
      return
      end
