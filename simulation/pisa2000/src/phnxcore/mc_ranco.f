      subroutine mc_ranco
      implicit none

c     event generator for events in cone


c     Feb. 12, 1997  C.F. Maguire minor fix of cone logic

#include "gconst.inc"
#include "guphnx.inc"
#include "guevgen.inc"

c     The parameters are in pmc1(10)       General
c     pmc2(10)       Special
c------------------------------------------------------------
c     structure for n particles in cone
c         pmc2(1)  part. type
c         pmc2(2)  magnitude of momentum
c         pmc2(3)  cone center angle thc ( deg.)
c         pmc2(4)  cone center angle phic (deg.)
c         pmc2(5)  cone half opening angle ( deg)
c         pmc2(6)  number of particles per call
c         pmc2(7)  vertex x
c         pmc2(8)  vertex y
c         pmc2(9)  vertex z
c         pmc2(10) Z vertex width
c-------------------------------------------------------------
c     local variables

      integer  i
      real    thc, phic, th_o, p, cos_th, d_th
      real    p1(3)
      real    mtheta, mphi
      real    rad/0.0174533/
      real    rndm
      real    xnor

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
 
       thc=pmc2(3)*rad
       phic=pmc2(4)*rad+pi/2             ! Angle conversions
       th_o = pmc2(5)*rad
       p=abs(pmc2(2))                    ! Momentum in GeV
       mxtot=pmc2(6)                     ! Multiplicity
       do i=1,mxtot
        idtot(i)=pmc2(1)                 ! Particle ID
       enddo
       cos_th=cos(th_o)
       d_th=1-cos_th
c      endif            ! removed CFM (for interactive reasons )

      do i=1,mxtot

c     June 15, 1994
c     decide to use pmc2(10) for the z width instead of momentum increment

c      p = p + pmc2(10)       ! next momentum

c     November 3, 1996
c     Use PMC2(5) < 0 as a new key for azimuthal isotropy
c     Use PCM2(2) < 0 for random momentum total

       if(pmc2(2).lt.0.0)then
          p = abs(pmc2(2))*rndm(i)     ! allow up to maximum of pmc2(2)
       endif
       if(pmc2(5).ge.0.0)then
          mtheta=acos(1.-rndm(i)*d_th) ! theta random in cone
          mphi= 2.*pi*rndm(i)              ! phi random in cone
          p1(1)=p*sin(mtheta)*cos(mphi)
          p1(2)=p*sin(mtheta)*sin(mphi)         ! mom. components
          p1(3)=p*cos(mtheta)                  ! in theta/phi dir.
      else
         mtheta = thc + th_o*(0.5 - rndm(i))
         p1(3) = p*cos(mtheta)
         mphi = 2.*pi*rndm(i)
         p1(1) = p*sin(mtheta)*cos(mphi)
         p1(2) = p*sin(mtheta)*sin(mphi)
      endif

c     June 15, 1994
c     call to EULER is a remnant from the FOPI program

       if(pmc2(5).ge.0.0)then                 ! fix from .gt. to .ge.
          call euler(p1,phic,thc,0.,1)        ! rotate in Geant frame
       endif
       pptot(2,i)=p1(1)
       pptot(3,i)=p1(2)
       pptot(4,i)=p1(3)
      enddo
      return
      end
