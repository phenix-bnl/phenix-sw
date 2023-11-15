      subroutine muidStageCheck
      implicit none

c     Original Author: C.F. Maguire
c     Creation Date: January 27, 2007

c     Purpose: Implements the final filter condition for the intra-event staged simulations

c     Method: Decision on whether to retain the event is based on the final filter condition variable finalFilter
c             = 1 means take the event which as any hit in the 5th MuID layer
c                 only hits in subsystems which have this track number or a direct ancestor will be retained

c     Revision History



c     Global variables

      include 'guevgen.inc'    ! contains final filter condition variable
#include "gcflag.inc"


c     Local variables

      integer iFirst /1/       ! first time call variable which is checked for each call
      save iFirst              ! not necessary in FORTRAN since iFirst was initialized, but saving good practice anyway

      integer testEvent /15/
      integer maxKPrint/0/
      integer kPrint/0/
      save kPrint, testEvent


c     variables for using gfhits call

      character*4 namep
      integer nhits, ih
      integer nhmax
      parameter (nhmax=3999)
      integer itrah(nhmax)
      integer nubv(2,nhmax)
      integer nuvs(5)
      real hitsh(11,nhmax)
      integer hitsWarnMax
      parameter (hitsWarnMax = 100)
      integer hitsWarn /0/
      save hitsWarn
      

c     variables for checking the MuID layers and saving track numbers in the event

      integer iplane
      integer thisTrackNumber
      integer parentTrackNumber
      integer lastTrackNumber
      integer searchTrackNumber
      integer kSearch           ! current backward generation search number
      integer MAXSEARCH         ! limit on how many backward generation searches are allowed
      parameter (MAXSEARCH = 100)

      integer iNumber /0/       ! number of particles being stored for this event
      integer kNumber

      logical foundOneTrack     ! used by final filter condition 2 (only one track saved per event)

      integer gknubuf           ! for gfkine call
      real gkubuf(10)           ! for gfkine call
      integer spart, snvert     ! for gfkine call
      real pvrtx(4), svrtx(3)   ! for gfkine call
      real totalMomentum

c     Begin executable code

      if(iFirst.eq.1)then
         iFirst = 0
         write(6,1)finalFilter
 1       format(//, ' muidStageCheck has finalFilter condition =',i3,//)
      endif  ! first time call check


c   Code from mun_digi, with some names changes of the arrays


      write( namep, '( a4)' ) 'MUGS'  ! mun_digi used CUDET variable name
      call gfhits(
     &        'MUN '            ! set identifier
     &        , namep           ! detector identifier
     &        , 2               ! dim of path identification
     &        , 11              ! dim of hit array
     &        , nhmax           ! max number of returned hits
     &        , 0               ! take all tracks
     &        , nuvs            ! volume descriptor
     &        , itrah           ! array of hit producing tracks
     &        , nubv            ! volume descriptor number
     &        , hitsh           ! hit values
     &        , nhits )         ! number of hits

      if(nhits.le.0.and.
     +   (kPrint.lt.maxKPrint.or.ievent.eq.testEvent))then
         kPrint = kPrint + 1
         write(6,2)ievent,nhits
 2       format(//, '  muidStageCheck <I>: At event ',i5,
     +        ' nhits = ', i6, ' immediate return',/)
         return                 ! immediate return if no MuID layers were hit
      endif                     ! check if any MuID layers were hit

      if(nhits.gt.1000)then
         write(6,3)ievent, nhits
 3       format(//, '  muidStageCheck <W>: At event ',i5,
     +        ' nhits = ', i6, ' exceeds limit of 1000',
     +        ' which is hardcoded in mun_digi.f',/)
      endif

      if(nhits.gt.nhmax)then
         write(6,4)ievent, nhits, nhmax
 4       format(//, '  muidStageCheck <W>: At event ',i5,
     +        ' nhits = ', i6, ' exceeds limit of', i6,
     +        ' which is hardcoded in this routine',/)
      endif

      if(finalFilter.eq.1 .or. finalFilter.eq.2)then
         lastTrackNumber = -1
         foundOneTrack = .false.

c     Now we decipher the plane number

         do ih = 1,nhits
            iplane = nubv(1,ih)

c     Assume that there a six MuID layers in each arm; could check with qf(lfn_para + 2)

            if(iplane.gt.6) then
               iplane = iplane - 6
            endif               !  check for South arm

            if(kPrint.lt.maxKPrint.or.ievent.eq.testEvent)then
c              write(6,11)ih, nhits, nubv(1,ih), iplane
 11            format(//, ' muidStageCheck <I>: ',
     +              ' ih, nhits, nubv(1,ih), iplane = ',
     +              4i8)
            endif
            
            if(iplane.lt.1 .or. iplane.gt.6)then
               write(6,12)ievent, ih, nhits, nubv(1,ih), iplane
 12            format(//, ' muidStageCheck <E>: bad MuID plane number ',
     +              ' ih, nhits, nubv(1,ih), iplane = ',
     +              5i8)
               hitsWarn = hitsWarn + 1
               if(hitsWarn.ge.hitsWarnMax)then
                  stop ' program error'
               endif
            endif               ! safety check on plane number


c     Hard code the fifth (or sixth) plane here
c     Store all the different track numbers

            if(iplane.ge.5)then
               foundOneTrack = .true.
               thisTrackNumber = itrah(ih)
               if(kPrint.lt.maxKPrint.or.ievent.eq.testEvent.or.
     +            finalFilter.eq.2)then
                  kPrint = kPrint + 1
                  write(6,13)ievent, iplane, nubv(1,ih), nhits, ih,
     +                 thisTrackNumber
 13               format(//,' muidStageCheck <I>: accepted event ', i5,
     +                 ', plane ', i2, ' nubv ',i3,
     +                 ', nhits ', i5, ' ihit ', i5, ' track ',i5,/)
               endif            ! check on printing
               
               if(thisTrackNumber.ne.lastTrackNumber)then

c     Check previously stored tracks to see if this track number has already been been stored

                  if(iNumber.gt.0)then
                     do kNumber = 1, iNumber
                        if(thisTrackNumber.eq.
     +                       storeAncestor(kNumber))then
                           go to 18
                        endif
                     enddo      ! loop over previously stored track numbers
                  endif

c     Track number was not previously stored

                  if(iNumber.eq.MAXANCESTOR)then
                     stop ' muidStage: iNumber too large thisTrack'
                  endif
                  iNumber = iNumber + 1
                  storeAncestor(iNumber) = thisTrackNumber

c     Now search for the parents of this track number

                  kSearch = 0
                  parentTrackNumber = thisTrackNumber
                  do while(kSearch.lt.MAXSEARCH.and.
     +                 parentTrackNumber.gt.mxtot)
                     kSearch= kSearch + 1
                     call gfkine(parentTrackNumber,svrtx,pvrtx,spart,
     +                    snvert,gkubuf,gknubuf)
                     searchTrackNumber = parentTrackNumber
                     parentTrackNumber = gkubuf(1)  ! parent track number

                     if(kPrint.lt.maxKPrint.or.ievent.eq.testEvent)then
                        totalMomentum = sqrt(pvrtx(1)*pvrtx(1) +
     +                            pvrtx(2)*pvrtx(2) + pvrtx(3)*pvrtx(3))
                        write(6,14)searchTrackNumber, parentTrackNumber,
     +                       kSearch, svrtx(3), spart, totalMomentum
 14                     format(1h ,' track', i6, ',  parent', i6,
     +                       ',  kSearch', i3, '  z ', f8.2,
     +                       ', part', i3, ' momentum ', f8.3)
                     endif

c     Now check if this parentTrackNumber has already been stored

                     do kNumber = 1, iNumber
                        if(parentTrackNumber.eq.
     +                       storeAncestor(kNumber))then
                           go to 15
                        endif
                     enddo      ! loop over previously stored track numbers

                     if(iNumber.eq.MAXANCESTOR)then
                        stop ' muidStage: iNumber too large parentTrack'
                     endif
                     iNumber = iNumber + 1
                     storeAncestor(iNumber) = parentTrackNumber                    
 15                  continue   ! branch point for jumping out of parentTrack number search

                  enddo         ! do while loop over parent track search
                  
 18               continue      ! branch point for jumping out of thisTrack number search
       
               endif            ! check for that thisTrack does not equal lastTrack already stored
               lastTrackNumber = thisTrackNumber
            endif               ! found a hit in the specified MuID plane

            if(finalFilter.eq.2 .and. foundOneTrack)then
               go to 19
            endif  ! check if found a track when finalFilter = 2

         enddo                  ! loop over the hits

 19      continue               ! branch point to jump out of hits loop (finalFilter = 2)

         if(kPrint.lt.maxKPrint.or.ievent.eq.testEvent .or.
     +      finalFilter.eq.2)then
            kPrint = kPrint + 1
            write(6,20)iNumber, ievent,
     +           (storeAncestor(kNumber),kNumber=1,iNumber)
 20         format(/,' Track numbers stored = ',i5, ' for event', i3,
     +           100(/,10(i5,2x)))
         endif
         nStoreAncestor = iNumber
      endif                     ! finalFilter = 1 or 2

      hitsWarn =  0
      iNumber = 0

      return
      end
