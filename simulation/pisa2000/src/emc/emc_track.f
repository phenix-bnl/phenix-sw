
      subroutine emc_track(itrack, svrtx, pvrtx, spart, snvert,
     1                     itin, kcycle)
      implicit none

c     Routine to get the track ancestry of a hit in EMCal detector
c     Returns track information of incident particle into EMCal system

c     Author Charles F. Maguire
c     First version July 30, 1995

c     Called by EMC_DIGI
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
      real       emc_zmax    ! maximum Z extent of EMCal system
      parameter (emc_zmax = 240.)
      

c     Begin execution

      ittest = itrack
      ncycle = 0
      itin = -ittest
      kcycle = 0
 10   continue
      call gfkine(ittest,svrtx,pvrtx,spart,snvert,
     1            ubuf,nubuf)
      rtest = sqrt(svrtx(1)*svrtx(1) + svrtx(2)*svrtx(2))

c     Check for inside smallest EMCal radius or 
c           for outside maximum EMCal Z extent or
c           for primary vertex

c     Should R value be different for PbG (IWALL = 7 or 8)

      if(rtest.lt.505.0.or.
     +   abs(svrtx(3)).gt.emc_zmax.or.
     +   snvert.eq.1)then

c   Should do medium or volume check instead?

         itin = ittest
         go to 20   ! terminate search
      endif  ! check on EMCal radius

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
      if(rtest.gt.505.0)then
         itin = itin
      endif ! debug check
      return
      end
