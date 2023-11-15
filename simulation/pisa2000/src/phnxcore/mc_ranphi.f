      subroutine mc_ranphi
      implicit none

c     event generator for single particle
c     uniformly distributed in phi

c     Basanta K. Nandi, March 16, 2000

c     Feb. 12, 1997  C.F. Maguire minor fix of cone logic

#include "gconst.inc"
#include "guphnx.inc"
#include "guevgen.inc"


c------------------------------------------------------------

c         pmc2(1)  part. type
c         pmc2(2)  magnitude of momentum
c         pmc2(3)  minimum theta angle ( deg.)
c         pmc2(4)  maximum theta angle ( deg.)
c         pmc2(5)  Empty ( put 0)
c         pmc2(6)  number of particles per call
c         pmc2(7)  vertex x
c         pmc2(8)  vertex y
c         pmc2(9)  vertex z
c         pmc2(10) Z vertex width
c-------------------------------------------------------------
c     local variables

      integer  i
      real    p1(3)
      real    rad/0.0174533/
      real    rndm
      real    xnor

      real    theta, theta_min, theta_max
      real    ctheta, stheta
      real    phi, cphi, sphi
      real    ptrans, pabs
      real    VEC(2)

c      if(iu_ini_par.eq.0)then             !initialize (removed CFM, 5/5/93)
       Do i=1,3
          xyz(i)=pmc2(i+6)                ! Vertex
       enddo
 

c     new insertion June 15, 1994 to randomize along Z

       if(pmc2(10).ne.0.0)then
          if(pmc2(10).gt.0.0)then

c     Gaussian distribution about pmc2(9)

 1           continue
             call norran(xnor)
             if(abs(xnor).gt.2.0)go to 1
             xyz(3) = xyz(3) + pmc2(10)*xnor
          else

c     Uniform random distribution about pmc2(9)

             xyz(3) = xyz(3) + pmc2(10)*(0.5 - rndm(0))
          endif  ! check on pmc2(10) > 0.0
       endif  ! check on randomizing along Z
 
      pabs      = pmc2(2)
      theta_min = pmc2(3)
      theta_max = pmc2(4)
      mxtot     = pmc2(6)       ! Multiplicity
      
      do i=1,mxtot
         idtot(i)=pmc2(1)       ! Particle ID
      enddo

      do i=1,mxtot


c     Generate two random number for theta and phi
c     theta is randomly generated between theta(min) and theta(max)
c     phi is randomly generated between 0 to 360

 100     CONTINUE
         
         call RANMAR(VEC,2)
         theta = vec(1)*theta_max
         phi   = vec(2)*360.0
         if(theta.ge.theta_min.AND.theta.lt.theta_max)then

c         write(6,*)theta, theta_min, theta_max, phi

            theta  = theta*rad
            phi    = phi*rad
            stheta = sin(theta)
            ctheta = cos(theta)
            sphi   = sin(phi)
            cphi   = cos(phi)
            
            p1(1) = pabs*stheta*cphi
            p1(2) = pabs*stheta*sphi
            p1(3) = pabs*ctheta
            
         else
            GOTO 100
         endif
         
         pptot(2,i)=p1(1)
         pptot(3,i)=p1(2)
         pptot(4,i)=p1(3)
         
      enddo

      return
      end
