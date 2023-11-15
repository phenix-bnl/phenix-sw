      subroutine tofstp
      implicit none

c     Original Author: Charles F. Maguire
c     Creation Date: May 25, 2002 (based on similar routines for PCs)

c     Purpose: Store GEANT hits information at the middle of the TOF tracking volume

c     Method: Called by GUSTEP whenever the tracking medium corresponds to one
c             of the sensitive volume numbers in TOF (currently 700, 710, or 720)
c             Stores the GEANT information when the particle enters and then when it leaves
c             Information put in the stored hit ZEBRA bank (GSAHIT call) is the average
c             except for the accumulated energy loss


c     Revision History
c     C.F. Maguire        May 31, 2002   Fix the momentum px,py,pz assignments


c********FUNCTION OF GEANT VARIABLES INWVOL and ISTOP***********
c                                                              *
c        INWVOL IS DIFFERENT FROM ZERO WHEN THE TRACK          *
c        IS INCREASING OR HAS REACHED A VOLUME BOUNDARY        *
c         ISTOP IS DIFFERENT FROM 0 IF TRACK HAS STOPPED       *
c                                                              *
c***************************************************************

#include "gckine.inc"
#include "gcsets.inc"
#include "gctrak.inc"


c     Local variables

      integer inoutl /0/     ! logical to indicate an entrance hit has been seen
      integer nHitDim        ! number of hit parameters in the sensitive detector
      parameter (nHitDim = 11)

      real xmIn(3)           ! values of master position coordinates at entrance
      real xmOut(3)          ! values of master position coordinates at exit
      real pMomIn(3)         ! values of local momentum at entrance
      real pMomOut(3)        ! values of local momentum at exit

      real hitStore(nHitDim) ! values of hit parameters which are stored 

      real deleSum           ! accumulated energy loss

      real tofIn, tofOut     ! tof at entrance and exit

      integer iStore         ! return value from GSAHIT call 

      save xmIn, xmOut, pMomIn, pMomOut, deleSum, inoutl   ! save values (also g77 linker option)
      save tofIn, tofOut


c     begin execution

      if(inwvol.eq.1) then
         inoutl = 1       ! indicate that we have an entrance hit
         deleSum = 0      ! initialize the accumulated energy loss

         xmIn(1) = vect(1)  ! x position at entrance
         xmIn(2) = vect(2)  ! y position at entrance
         xmIn(3) = vect(3)  ! z position at entrance

         pMomIn(1) = vect(4)*vect(7)  ! x momentum at entrance
         pMomIn(2) = vect(5)*vect(7)  ! y momentum at entrance
         pMomIn(3) = vect(6)*vect(7)  ! z momentum at entrance

         tofIn = tofG*1.0e9   ! TOF at entrance (in nanoseconds)

      endif  ! check if particle is entering the TOF sensitive volume


c     accumulate the energy loss in GeV

      deleSum = deleSum + destep  ! assumes that there has been an entrance hit

      if((inwvol.eq.2.or.istop.gt.0).and.inoutl.gt.0)then

         xmOut(1) = vect(1)  ! x position at exit
         xmOut(2) = vect(2)  ! y position at exit
         xmOut(3) = vect(3)  ! z position at exit

         pMomOut(1) = vect(4)*vect(7)  ! x momentum at exit
         pMomOut(2) = vect(5)*vect(7)  ! y momentum at exit
         pMomOut(3) = vect(6)*vect(7)  ! z momentum at exit

         tofOut = tofG*1.0e9   ! TOF at exit (in nanoseconds)

         hitStore(1) = 0.5*(xmIn(1) + xmOut(1))
         hitStore(2) = 0.5*(xmIn(2) + xmOut(2))
         hitStore(3) = 0.5*(xmIn(3) + xmOut(3))

         hitStore(4) = deleSum
         hitStore(5) = 0.5*(tofIn + tofOut)
         hitStore(6) = float(ipart)  ! GEANT particle ID

         hitStore(7) = 0.5*(pMomIn(1) + pMomOut(1))
         hitStore(8) = 0.5*(pMomIn(2) + pMomOut(2))
         hitStore(9) = 0.5*(pMomIn(3) + pMomOut(3))

         call gsahit(iset, idet, itra, numbv, hitStore, iStore)
         if(iStore.eq.0)then
            write(6,10)iStore
10          format(//,'  TOFSTP <F>: GSAHIT fails to store values',//)
            stop ' PISA is stopping in TOFSTP'
         endif

         inoutl = 0  ! reset the entrance hit switch

      endif  ! check if particle is exiting the TOF or has stopped, and there was an entrance

      return
      end
