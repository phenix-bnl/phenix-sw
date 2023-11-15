      subroutine event_hist

      implicit none

#include "g77trigdef.inc"


c     Original author: Charles F. Maguire

c     Revision history

c        2/18/96   C. F. Maguire     Put in ATAND projection 
c                                    Alpha execution time errors
c                                    when THETA = 0 or THETA = 180 deg.
c     Called by GUKINE
c     Calls GFPART
c     Creates dN/dn histogram

 
#include "guevgen.inc"
 

c     local variables

      real eta         ! pseudo-rapidity
      real theta       ! polar angle (degrees)
      real ptotal      ! total momentum
      real px          ! x component of momentum
      real py          ! y component of momentum
      real pz          ! z component of momentum
      real eta_lim     ! limits on the eta histogram
      parameter (eta_lim = 10.0)
      integer n_eta    ! number of bins in the eta histogram
      parameter (n_eta = 200)
      integer kpart    ! do loop index
 

c     variables for the GFPART call to get the charge of the particle

 
      character*20 chpart ! particle descriptor
      integer itrtyp      ! type of tracking routine for this particle
      real amass          ! particle mass
      real charge         ! particle charge
      real tlife          ! particle lifetime
      integer nubuf/1/    ! buffer size (not used)
      integer ubuf(1)     ! buffer array (not used)
      integer iqval       ! 0 ===> neutral, > 0 ===> charged
 
      integer ifirst   ! initial call
      data ifirst /0/
      save ifirst

c     begin execution

 
      if(ifirst.eq.0)then
         ifirst = 1

c     book the histogram

         call hbook1(100, 'Primary Particle DN/DETA',
     +               n_eta, -eta_lim, +eta_lim, 0.0)
      endif  ! initialization
 

c     event by event processing (full event calls only)

      do kpart = 1,mxtot
         px = pptot(2,kpart)
         py = pptot(3,kpart)
         pz = pptot(4,kpart)
         ptotal = sqrt(px*px + py*py + pz*pz)
         if(ptotal.ne.0.0)then
            if(pz.ne.0.0)then
               theta = acosd(pz/ptotal)

c     Floating point divide by zero exception on Alpha machine

               if(abs(theta).lt.1.e-06)then
                  theta = 1.e-6
               endif ! protection against THETA = 0  degrees
               if(abs(theta).gt.179.999)then
                  theta = 179.999
               endif ! protection against THETA = 180 degrees

c     End insertions to protect against divide by 0 on Alpha machine

               eta = alog(1.0/tand(theta/2.))   ! now protected 
            else
               eta = 0.0
            endif  ! check on 90 degree emission
         else
            eta = 0.0 ! default to 90 degree emission if ptotal = 0
         endif ! check on ptotal > 0

c     As per the request of John Sullivan, only charged particles are counted
c     This conditional structure could be moved to the "top" of the loop but
c     it is being placed here in case one later wants to see the total
c     distribution at some point.

         if(idtot(kpart).gt.0.and.idtot(kpart).lt.49)then  ! original GEANT set
            call gfpart(idtot(kpart), chpart, itrtyp, amass, charge,
     1                  tlife, ubuf, nubuf)
            iqval = nint(charge)
            if(iqval.ne.0)then
               call hf1(100,eta,1.0)
            endif  ! check on non-zero charge value
         endif  ! check on particle ID being in the original GEANT particle set
      enddo  ! loop over all stored particles
 
      return
      end
