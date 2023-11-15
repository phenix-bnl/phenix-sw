      subroutine muonArmStageOutput(iOption)

c     Original author: Charles F. Maguire
c     Creation date: February 23, 2004

c     Last revision: C.F. Maguire, March 2, 2004
c                    Added more comment lines

c     Purpose: Implement particle acceptance conditions for staging the Muon Arm simulations

c     Calling map
c         Called by gustep at each Z tracking step for every particle when STAGE command has been given
c         Called by g_user at the end of the event when all tracking has been completed



c     Calling parameter

      integer iOption           ! 1 means check track, -1 means do output


c     Global common blocks

#include "gckine.inc"
#include "gctrak.inc"
#include "guevgen.inc"
#include "gcflag.inc"


c     Local variables

      integer iFirst /1/        ! first time call switch
      save iFirst

      real zBoundary            ! current Z Boundary at this stage
      save zBoundary

      integer kPrint/0/         ! diagnostic print counter
      save kPrint

      integer iPrint /0/        ! diagnostic print counter
      save iPrint

      integer lastStage /-1/    ! remembers the stage number of the previous call
      save lastStage

      integer itparent          ! parent track number
      integer idparent          ! parent ID number

      integer itorigin          ! primary particle track number
      integer idorigin          ! primary particle ID number

      integer kSearch           ! current backward generation search number
      integer MAXSEARCH         ! limit on how many backward generation searches are allowed
      parameter (MAXSEARCH = 100)

      integer MAXNUM
      parameter (MAXNUM = 1000) ! maximum number of particles which can be stored in any one event
      integer iNumber /0/       ! number of particles being stored for this event
      integer kNumber

c     Array to store the event particles into ROOT NTUPLE  (from rootPISA.cc source file)

c     EvtParticleNtuple = new TNtuple("particle", "Event Particles",
c				      "event:pnum:pid:px:py:pz:E:xvtx:yvtx:zvtx:"// 10
c                                     "ivert:pxvert:pyvert:pzvert:xvtxanc:yvtxanc:"// 16
c                                     "zvtxanc:itparent:idparent:pxorig:pyorig:pzorig:"// 22
c				      "xorig:yorig:zorig:itorig:idorig:itra:ksearch:mxtot"); // 30

      real evtParticle(30, MAXNUM)
      integer storeTrack(MAXNUM)

      integer nubuf             ! for gfkine call
      real ubuf(10)             ! for gfkine call
      integer spart, snvert     ! for gfkine call
      real pvrtx(4), svrtx(3)   ! for gfkine call
      integer origvert          ! for gfkine call
      real porig(4), vorig(3)   ! for gfkine call
      integer ixyz

      integer jTrack            ! temporary track variable
      integer mTrack            ! temporary track variable

      integer lastTrack/-1/     ! remembers the track number from the previous call
      save lastTrack

      if(iFirst.eq.1 .or. NSTAGE .ne. lastStage) then

         iFirst = 0
         lastTrack = -1
         lastStage = NSTAGE

         zBoundary = ZSTAGE

         write(6,1)zBoundary, NSTAGE, PSTAGE, FILTER
 1       format('  zBoundary for this stage = ', f9.2, ' cm,',
     +          '  nStage = ', i2, '  with minimum ',
     +          'momentum = ', f8.3, ' GeV/c',
     +          ', filter = ', i2,/)

      endif                     ! initialization sequence

      if(iOption .eq. 1) then   ! iOption = 1 is used by call from GUSTEP


c     Check the particle

         ztest = abs(VECT(3))   ! VECT is a 7 element array in a GEANT common block

         if(VECT(3).NE.0.0 .and. VECT(6).NE.0.0)then
            if(VECT(3)/VECT(6) .lt. 0.0) then
               ISTOP = 1
               return           !  particle is going in the wrong Z direction
            endif               !  check for good direction of a particle
         endif                  !  check for non-zero Z and PZ

         if(IEVENT.eq.0)then
            write(6,22)IEVENT, VECT(3), lastTrack, ITRA,
     +                 abs(zTest-zBoundary)
 22         format('  IEVENT ', i3, '  VECT3 ', f8.2,
     +             '  lastTrack ', i3, '  ITRA ', i3,
     +             '  diff ', f10.3)
         endif                  ! diagnostic print out for a specific event, if needed

         if(zTest.lt.zBoundary)then
            return
         endif  ! don't check until particle gets to zBoundary

         if(IEVENT.eq.0)then
            write(6,23)IEVENT, ztest, IPART
 23         format('  IEVENT ', i3, '  ztest ', f8.2, ' ID ',i3)
         endif                  ! diagnostic print out for a specific event, if needed


c     ISTOP is a GEANT tracking variable, 1 means stop tracking the particle

         iStop = 1              ! kill particle once it has reached the zBoundary


c     Check if this track number is the same as the previous track number
c     This should not be happening since we kill the track after we test it here

         
         if(lastTrack.eq.ITRA)then
            return              ! should check if this condition is ever true
         endif

         if(IPART.le.3 .or. IPART.eq.7)then
            return;
         endif                  ! don't store photons, e+ or e-, or pizero

         lastTrack = ITRA       ! indicate that this track has already been checked for output

c     Momentum condition for accepting particles into output file

c     We could defer this momentum cut until we look at all the particles at the end-of-event call
c     So we could have all particles accepted if only one of these was above the PSTAGE cut

         if(VECT(7) .lt. PSTAGE)then
            return;
         endif                  ! minimum momentum for output from this stage


c     default primary particle assignments for track ancestry

         itparent = -ITRA
         idparent = 0
         itorigin = ITRA          ! default for primary particle
         idorigin = IPART         ! default for primary particle
         do ixyz = 1,3
            porig(ixyz) = pvert(ixyz)
            vorig(ixyz) = vert(ixyz)
         enddo                  ! initialize primary particle vertex values


c     Check for parent and primary particle ancestor information
c     Results will be stored in output OSCAR NTUPLE for diagnosis

         kSearch = 0
         if(ITRA.gt.mxtot)then  ! mxtot is the number of primary particles in the event

c     parent particle check

            call gfkine(ITRA,svrtx,pvrtx,spart,snvert,ubuf,
     +           nubuf)
            itparent = ubuf(1)
            if(itparent.gt.0)then
               call gfkine(itparent,svrtx,pvrtx,spart,snvert,ubuf,
     +           nubuf)
               if(itparent.gt.mxtot)then
                  idparent = -spart ! indicate that the parent particle is a secondary
                  itparent = -itparent
               else
                  idparent = spart  !
               endif
            endif               ! check if immediate parent was a primary particle


c     check for primary particle ancestor

            jTrack = ITRA       ! ITRA is known to be greater than mxtot (number of primary particles)
            idorigin = -999999  ! these numbers will be re-assigned
            itorigin = -999999  ! these numbers will be re-assigned
            do ixyz = 1,3
               porig(ixyz) = -9999.0
               vorig(ixyz) = -9999.0
            enddo               ! set up vertex coordinates

 5          continue
            kSearch = kSearch + 1
            call gfkine(jTrack,svrtx,pvrtx,spart,snvert,ubuf,
     +           nubuf)
            jTrack = ubuf(1)    ! parent track number
            if(jTrack.gt.mxtot.and.kSearch.lt.MAXSEARCH)then
               go to 5          ! backward jump, limited to 100 times
            endif


c     At this point jTrack will be less than mxtot, or kSearch = MAXSEARCH

            if(jTrack.gt.0)then
               itorigin = jTrack
               call gfkine(jTrack,svrtx,pvrtx,spart,snvert,ubuf,
     +              nubuf)
               idorigin = spart
               do ixyz = 1,3
                  porig(ixyz) = pvrtx(ixyz)
                  vorig(ixyz) = svrtx(ixyz)
               enddo            ! set up vertex coordinates
            else
               write(6,881)IEVENT, jTrack
 881           format(' muonArmStageOutput <E>: At event ', i8,
     +                ' primary particle track number = ', i8)
               stop ' programming error '
            endif  !  safety check on jTrack > 0

            if(iNumber.gt.0)then

c     Check previously stored tracks to see if the parent particle has been stored
c     If parent has been stored, then this particle is a descendant
c     Descendant should not be stored since that will amount to double counting
c     This effect is caused by the zBoundary window

               do kNumber = 1, iNumber
                  if(iabs(itparent).eq.storeTrack(kNumber))then
                     return
                  endif         ! check for track number equality
               enddo            ! check previously stored tracks
               
            endif               ! check if at least one previous store

         endif                  ! check on vertex number greater than 1

         iNumber = iNumber + 1

         if(iNumber.le.MAXNUM)then
            evtParticle(1, iNumber) = IEVENT
            evtParticle(3, iNumber) = IPART
            evtParticle(4, iNumber) = VECT(7)*VECT(4)
            evtParticle(5, iNumber) = VECT(7)*VECT(5)
            evtParticle(6, iNumber) = VECT(7)*VECT(6)
            evtParticle(7, iNumber) = sqrt(VECT(7)*VECT(7) +
     +           AMASS*AMASS)
            evtParticle(8, iNumber) = VECT(1)*1.0E+13
            evtParticle(9, iNumber) = VECT(2)*1.0E+13
            evtParticle(10, iNumber) = VECT(3)*1.0E+13
            evtParticle(11, iNumber) = IVERT
            evtParticle(12, iNumber) = PVERT(1)
            evtParticle(13, iNumber) = PVERT(2)
            evtParticle(14, iNumber) = PVERT(3)
            evtParticle(15, iNumber) = VERT(1)
            evtParticle(16, iNumber) = VERT(2)
            evtParticle(17, iNumber) = VERT(3)
            evtParticle(18, iNumber) = itparent
            evtParticle(19, iNumber) = idparent
            evtParticle(20, iNumber) = porig(1)
            evtParticle(21, iNumber) = porig(2)
            evtParticle(22, iNumber) = porig(3)
            evtParticle(23, iNumber) = vorig(1)
            evtParticle(24, iNumber) = vorig(2)
            evtParticle(25, iNumber) = vorig(3)
            evtParticle(26, iNumber) = itorigin
            evtParticle(27, iNUmber) = idorigin
            evtParticle(28, iNumber) = ITRA
            evtParticle(29, iNumber) = kSearch
            evtParticle(30, iNumber) = mxtot

            storeTrack(iNumber) = ITRA

            if(kPrint.lt.0) then
               kPrint = kPrint + 1
               write(6,11) ITRA, IEVENT
 11            format(/, ' Storing track ', i6,' event ', i5)
            endif               ! diagnostic print
         
         else

            stop ' iNumber too large in muonArmStageOutput'
               
         endif

      endif                     ! store particle information for output

      if(iOption.eq.-1)then     ! IOPTION = -1 when called by the end-of-event g_user routine

c     At this point we can make a decision on which particles to put into the output OSCAR NTUPLE file

         do kNumber = 1, iNumber
            evtParticle(2, kNumber) = iNumber - 1
            call filloscarfile(EvtParticle(1,kNumber))
         enddo
         
         if(iPrint.lt.10)then

            iPrint = iPrint + 1
            write(6,2) IEVENT, iNumber
 2          format(//, ' muonArmStageOutput <I>: Event ',
     +                 i8, ' iNumber ', i5)

         endif                  ! diagnostic output

         lastTrack = -1
         iNumber = 0

      endif                     ! write out the stored particle information

      return
      end
      
