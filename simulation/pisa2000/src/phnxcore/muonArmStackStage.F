      subroutine muonArmStackStage(iOption)
      implicit none


c     Original author: Charles F. Maguire
c     Creation date: January 25, 2007

c     Last revision: C.F. Maguire, January 25, 2007
c                    Original version

c     Purpose: Implement particle acceptance conditions for staging the Muon Arm simulations

c     Calling map
c         Called by gustep at each Z tracking step for every particle when STACK_STAGE command has been given
c         Called by g_user at the end of the event when all tracking has been completed


c     Calling parameter
      integer iOption           ! 1 means check track (gustep call), -1 means do output (g_user call)

c     Global variables

#include "guevgen.inc"
#include "geant321/gcking.inc"
#include "gckine.inc"
#include "gctrak.inc"
#include "secubuf.inc"
#include "gcflag.inc"
#include "gcsets.inc"


c     Local variables

      integer iFirst /1/        ! first time call switch
      save iFirst

      integer kPrint/0/         ! first diagnostic print counter
      save kPrint
      integer kPrintMax
      parameter (kPrintMax=10)

      integer iPrint /0/        ! second diagnostic print counter
      save iPrint

      integer kStage            ! do loop index
      integer kkStage
      real particleCurrentZ
      real particleVertexZ
      real stageZ
      real stageMomentum

      integer kClone            ! do loop index
      integer i                 ! do loop index

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
c                                     "kstage:pxvert:pyvert:pzvert:xvtxanc:yvtxanc:"// 16
c                                     "zvtxanc:itparent:idparent:pxorig:pyorig:pzorig:"// 22
c				      "xorig:yorig:zorig:itorig:idorig:itra:ksearch:mxtot"); // 30

      real evtParticle(30, MAXNUM)
      integer storeTrack(MAXNUM)

      integer gknubuf           ! for gfkine call
      real gkubuf(10)           ! for gfkine call
      integer spart, snvert     ! for gfkine call
      real pvrtx(4), svrtx(3)   ! for gfkine call
      integer origvert          ! for gfkine call
      real porig(4), vorig(3)   ! for gfkine call
      integer ixyz

      integer jTrack            ! temporary track variable
      integer mTrack            ! temporary track variable


c     Begin executable code

      if(iOption.eq.1)then

c     Call by gustep

         if(iFirst.eq.1)then
            iFirst = 0
            write(6,1)nStageStack
 1          format(//,' First call to muonArmStackStage ',
     +             ', Version Jan. 26. 2007 at 14:55',
     +             '; nStageStack = ',i2)

c     Future insert of code to open ROOT output file



         endif ! check on first call

c     Check current Z of particle with respect to all the stage boundary Z values
c     If the particle's current Z is after a given stage boundary and
c     the particle's Z-vertex is before the stage boundary then
c     kill the particle and place it on the stack

         particleCurrentZ = vect(3)
         particleVertexZ = vert(3)
         do kStage = 1, nStageStack
            stageZ = zstageStack(kStage)
            stageMomentum = pstageStack(kStage)
            if(particleCurrentZ.ge.stageZ.and.
     +         particleVertexZ.lt.stageZ.and.
     +         vect(7).ge.stageMomentum.and.
     +         vect(6).gt.0.0)then            ! condition on forward going particle
               if(iset.gt.0)then
                  write(6,9)iset, particleCurrentZ
 9                format(//, ' muonArmStackStage error: clone',
     +                   ' attempted in sensitive detector',
     +                   ', set number',i3, ', z = ', f8.2)
                  stop ' PISA stops due to this error'
               endif
               go to 10         ! break out of stage loop
            endif !check if the particle should be killed and cloned
         enddo  ! loop over stages

c     Particle did not meet kill condition, so just return

         return

c     Branch point for break out of loop over stages

 10      continue
         iOption = 0     !  tells gustep to return without further checking
         kkStage = kStage
         if(kPrint.lt.kPrintMax) then
            kPrint = kPrint + 1
            write(6,11)itra, ievent, kstage, nCloneStack(kStage),
     +                 vect(3), vert(3), vect(6)*vect(7)
 11         format(/, ' Storing track', i5,' event', i4,
     +           ', stage number', i2, ' clones', i2,
     +           ', current Z ', f8.2, ' vertex Z ', f8.2,
     +           ', pz ',f8.2)
         endif  ! diagnostic print 
         call trkstack(itra)  ! save track in PISA ancestry software

c     ISTOP is a GEANT tracking variable, 1 means stop tracking the particle

         iStop = 1              ! kill particle once it has reached the zBoundary


c     default primary particle assignments for track ancestry

         itparent = -ITRA
         idparent = 0
         itorigin = ITRA        ! default for primary particle
         idorigin = IPART       ! default for primary particle
         do ixyz = 1,3
            porig(ixyz) = pvert(ixyz)
            vorig(ixyz) = vert(ixyz)
         enddo                  ! initialize primary particle vertex values

         if(itra.gt.mxtot)then  ! mxtot is the number of primary particles in the event


c     parent particle check

            call gfkine(itra,svrtx,pvrtx,spart,snvert,gkubuf,
     +           gknubuf)
            itparent = gkubuf(1)
            if(itparent.gt.0)then
               call gfkine(itparent,svrtx,pvrtx,spart,snvert,gkubuf,
     +              gknubuf)
               if(itparent.gt.mxtot)then
                  idparent = -spart ! indicate that the parent particle is a secondary
                  itparent = -itparent
               else
                  idparent = spart !
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

            ksearch = 0
5          continue
            kSearch = kSearch + 1
            call gfkine(jTrack,svrtx,pvrtx,spart,snvert,gkubuf,
     +           gknubuf)
            jTrack = gkubuf(1)    ! parent track number
            if(jTrack.gt.mxtot.and.kSearch.lt.MAXSEARCH)then
               go to 5          ! backward jump, limited to 100 times
            endif


c     At this point jTrack will be less than mxtot, or kSearch = MAXSEARCH

            if(jTrack.gt.0)then
               itorigin = jTrack
               call gfkine(jTrack,svrtx,pvrtx,spart,snvert,gkubuf,
     +              gknubuf)
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
c     Tag these particles with a negative stage number

               do kNumber = 1, iNumber
                  if(iabs(itparent).eq.storeTrack(kNumber))then
                     kkStage = -kStage
                     go to 15
                  endif         ! check for track number equality
               enddo            ! check previously stored tracks
               
            endif               ! check if at least one previous store

         endif                  ! check on vertex number greater than 1


 15      continue  ! branch point for breaking out of parent checking loop

         iNumber = iNumber + 1

         if(iNumber.le.MAXNUM)then
            evtParticle(1, iNumber) = IEVENT
            evtParticle(3, iNumber) = IPART
            evtParticle(4, iNumber) = VECT(7)*VECT(4)
            evtParticle(5, iNumber) = VECT(7)*VECT(5)
            evtParticle(6, iNumber) = VECT(7)*VECT(6)
            evtParticle(7, iNumber) = sqrt(VECT(7)*VECT(7) +
     +           AMASS*AMASS)
            evtParticle(8, iNumber) = VECT(1)
            evtParticle(9, iNumber) = VECT(2)
            evtParticle(10, iNumber) = VECT(3)
            evtParticle(11, iNumber) = kkstage
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
 
         endif  ! check on less than maximum number of particles

c     Put clone particles on stack

         do kClone = 1,nCloneStack(kStage)
           ngkine = ngkine + 1
           do i=1,3
              gpos(i,ngkine) = vect(i)
           enddo
           gkin(4,ngkine) = getot
           gkin(1,ngkine) = vect(4)*vect(7)
           gkin(2,ngkine) = vect(5)*vect(7)
           gkin(3,ngkine) = vect(6)*vect(7)
           gkin(5,ngkine) = ipart
           tofd(ngkine) = 0.0
           iflgk(ngkine) = 1
        enddo                   ! cloning loop


c       Put all clones on the GEANT stack, using the same parent information

        nubuf = 1               ! store one user word in KINE
        ubuf(1) = itra          ! track number of the parent is stored
        call gsking(0)

      endif ! check on iOption = 1

      if(iOption.eq.-1)then

c     Call by g_user

c     Insert particles into output ROOT NTUPLE at the end of the event

c     At this point we can make a decision on which particles to put into the output OSCAR NTUPLE file

         do kNumber = 1, iNumber
            evtParticle(2, kNumber) = iNumber - 1
            call filloscarfile(EvtParticle(1,kNumber))
         enddo

        if(iPrint.lt.10)then

           iPrint = iPrint + 1
           write(6,2) IEVENT, iNumber
 2         format(//, ' muonArmStackStage <I>: Event ',
     +          i8, ' iNumber ', i5)

        endif                   ! diagnostic output

        iNumber = 0

      endif

      return
      end
