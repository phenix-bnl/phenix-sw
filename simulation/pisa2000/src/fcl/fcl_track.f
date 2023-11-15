      subroutine fcl_track(itrack, svrtx, pvrtx, spart, snvert,
     1                     itin, kcycle)
      implicit none

c     Routine to get the track ancestry of a hit in FCL detector
c     Returns track information of incident particle into FCL system

c     Author Charles F. Maguire
c     First version June 19, 2003 (based on emc_track routine)

c     Called by FCL_DIGI
c     Calls GFKINE

c     Calling variables

      integer itrack   ! track number from GFHITS
      real    svrtx(3) ! track vertex position
      real    pvrtx(4) ! track momentum at vertex
      integer spart    ! particle ID
      integer snvert   ! vertex number
      integer itin     ! incident track number
      integer kcycle   ! number of ancestry cycles

c     Global variables

#include "secubuf.inc"

c     Local variables

      integer    ncycle, mxcycle
      parameter  (mxcycle=50)
      integer    ittest
      real       rtest
      real       fcl_zmax    ! maximum Z extent of FCLal system
      parameter (fcl_zmax = 240.)

      real FCLRMAX
      parameter (FCLRMAX = 100.0)

      real FCLZMIN
      parameter (FCLZMIN = 1800.0)
      

c     Begin execution

      ittest = itrack
      ncycle = 0
      itin = -ittest
      kcycle = 0
 10   continue
      call gfkine(ittest,svrtx,pvrtx,spart,snvert,
     1            ubuf,nubuf)
      rtest = sqrt(svrtx(1)*svrtx(1) + svrtx(2)*svrtx(2))

c     Check less than minimum Z value FCLZMIN or 
c           bigger than maximum R value FCLRMAX or
c           for primary vertex


      if(rtest.gt.FCLRMAX.or.
     +   abs(svrtx(3)).lt.FCLZMIN.or.
     +   snvert.eq.1)then

c   Should do medium or volume check instead?

         itin = ittest
         go to 20   ! terminate search
      endif  ! check on FCL position

c    check if the user buffer has useful information (ubuf is not reset)

      if(nubuf.eq.1.or.nubuf.eq.2)then

c   NUBUF set to 1 in GUSTEP for some secondaries stored on the GEANT stack
c   NUBUF set to 2 in GUSTEP for muons (UBUF(2) is then a tag variable)

         itin = ubuf(1)  ! parent track number
         ncycle = ncycle + 1  
         if(ncycle.le.mxcycle)then
            ittest = itin
            go to 10   ! repeat call to gfkine
         else
            go to 20   ! terminate search
         endif
      endif  ! check on nubuf = 1 or 2
      if(nubuf.eq.3)then

c     NUBUF is set to 3 in GUKINE from the GEN_EVT event generator
c     These are "orphan" tracks since they have no parent track number from
c     the event generator.  For these tracks UBUF(1) = 0

         itin = -9999999  ! indicate an orphan track
      endif  ! check on nubuf = 3
 20   continue
      kcycle = ncycle
      return
      end
