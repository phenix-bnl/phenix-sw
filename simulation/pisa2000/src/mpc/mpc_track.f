
      subroutine mpc_track(itrack, svrtx, pvrtx, spart, snvert,
     +                     itin, kcycle)
      implicit none

c     Routine to get the track ancestry of a hit in MPC detector
c     Returns track information of incident particle into MPC system

c     Author Mickey Chiu
c     Derived from code by Charles F. Maguire for the EMC

c     Called by MPC_DIGI
c     Calls GFKINE

c     Calling variables

      integer itrack   ! track number from GFHITS
      real    svrtx(3) ! track vertex position
      real    pvrtx(4) ! track momentum at vertex
      integer spart    ! particle ID
      integer snvert   ! vertex number
      integer iadr     ! for GSKINU
      integer itin     ! incident track number
      integer kcycle   ! number of ancestry cycles

c     Global variables

#include "secubuf.inc"

c     Local variables

      integer    ncycle, mxcycle
      parameter  (mxcycle=50)
      integer    ittest
      real       r_vert      ! r of track vertex

c     Begin execution

      ittest = itrack
      ncycle = 0
      itin = -ittest
      kcycle = 0
 10   continue
      call gfkine(ittest,svrtx,pvrtx,spart,snvert,
     +            ubuf,nubuf)

      r_vert = sqrt(svrtx(1)*svrtx(1) + svrtx(2)*svrtx(2))

c   Check for geometrically inside MPC or for primary vertex
c   We include the inactive regions of the MPC for this
c   Right now this is only for the South MPC, we might need
c   to do this for the north
c   Active region of south is 222.083<z<240.129
c   South Front plate is at 220.079

c   Could (should) do medium or volume check instead?

      if ( .not. (abs(svrtx(3)).gt.218.729
     +            .and. abs(svrtx(3)).lt.242.35
     +            .and. r_vert.lt.22.05
     +            .and. r_vert.gt.4.0)
     +      .or. (snvert.eq.1) ) then

         itin = ittest
C         write(*,*) 'found parent, is itin',itin
         go to 20   ! terminate search
      endif  ! check on if not inside MPC or is primary

c    check if the user buffer has useful information (ubuf is not reset)

      if(nubuf.eq.1.or.nubuf.eq.2)then

c   NUBUF set to 1 in GUSTEP for some secondaries stored on the GEANT stack
c   NUBUF set to 2 in GUSTEP for muons (UBUF(2) is then a tag variable)

C         write(*,*) 'mpc_track: nubuf=1or2 ', ittest,' ',ncycle
C        write(*,*) ' ',nubuf,' ',ubuf(1),' ',ubuf(2),' ',ubuf(3)
C        write(*,*) ' ',spart,' ',svrtx(1),' ',svrtx(2),' ',svrtx(3)
C        write(*,*) ' ',snvert,' ',pvrtx(1),' ',pvrtx(2),' ',pvrtx(3)

         itin = ubuf(1)  ! parent track number
         ncycle = ncycle + 1  
         if(ncycle.le.mxcycle)then
            ittest = itin
C           write(*,*) 'ncycle mxcyle ',ncycle,' ',mxcycle,' ',ittest
            go to 10   ! repeat call to gfkine
         else
            go to 20   ! terminate search
         endif
      endif  ! check on nubuf = 1 or 2

      if(nubuf.gt.3)then

c     NUBUF is set to 3 in GUKINE from the GEN_EVT event generator
c     These are "orphan" tracks since they have no parent track number from
c     the event generator.  For these tracks UBUF(1) = 0
C
C     MC Actually, it looks like the gukine info gets corrupted when nubuf=3?

C        write(*,*) 'mpc_track2: nubuf=3 ', ittest,' ',ncycle
C        write(*,*) ' ',nubuf,' ',ubuf(1),' ',ubuf(2),' ',ubuf(3)
C        write(*,*) ' ',spart,' ',svrtx(1),' ',svrtx(2),' ',svrtx(3)
C        write(*,*) ' ',snvert,' ',pvrtx(1),' ',pvrtx(2),' ',pvrtx(3)

C         ubuf(1) = 0.;
C         ubuf(2) = 0.;
C         ubuf(3) = 0.;
C         call gskinu(itin,1,ubuf,iadr)

C         write(*,*) ' call gpkine(0)'
C         call gpkine(0)

         itin = -9999999  ! indicate an orphan track
C         keep itin equal to what the last call said it was

      endif  ! check on nubuf = 3
 20   continue
      kcycle = ncycle

C      if ( abs(svrtx(3)).gt.218.729 .and. abs(svrtx(3)).lt.242.35
C     +     .and. r_vert.lt.22.05 .and. r_vert.gt.4.0 ) then
C      else
C         
C      endif ! debug check

      return
      end
