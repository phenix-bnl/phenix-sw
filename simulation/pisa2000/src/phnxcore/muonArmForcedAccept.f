      subroutine muonArmForcedAccept

c     Original author: Charles F. Maguire
c     Creation date: February 21, 2004

c     Purpose: Implement forced acceptance conditions for the Muon Arm

c     Calling map
c         Called by gustep when ISWIT(7) = 8

c     Method


      implicit none

c     Global common blocks

#include "gckine.inc"
#include "gctrak.inc"
#include "guevgen.inc"


c     No calling arguments



c     Local variables

      integer iFirst /1/
      save iFirst

      integer iReject /0/
      save iReject

      integer iAccept /0/
      save iAccept
      integer jAccept /0/
      save jAccept

      real zTest             ! absolute value of current Z position
      real zAccept /400.0/   ! will accept any particle for which primary has reached 400 cm
      save zAccept

      integer newPrimary /0/
      save newPrimary
 
      integer jTrack
      integer nubuf             ! for gfkine call
      real ubuf(10)             ! for gfkine call
      integer spart, snvert     ! for gfkine call
      real pvrtx(4), svrtx(3)   ! for gfkine call
      integer jKine/0/
      save jKine
      integer kKine/0/
      save kKine


c     Begin execution


      if(iFirst.eq.1) then

         iFirst = 0
         write(6,1)
 1       format(//,' muonArmForcedAccept <I>: Initialization ',//)

      endif                     ! check on first call


c     Kill photons, electrons, positrons, and pizero immediately, assuming they will shower

      if(IPART.le.3 .or. IPART.eq.7) then
         iStop = 1              ! GEANT variable to indicate that the particle has disappeared
         return
      endif                     !  kill photons, electrons, positrons, and photons


c     Check the present Z value of the particle

      zTest = abs(VECT(3))
      if(zTest.gt.zAccept) then

c     Particle has reached acceptance Z boundary

         logAccepted = .TRUE.   ! take the particle

         if(jAccept.eq.1)then
            nrvacc_evt = nrvacc_evt + 1
            jaccept = 0
            if(iAccept.lt.10)then
               iAccept = iAccept + 1
               call gfkine(ITRA,svrtx,pvrtx,spart,snvert,ubuf,nubuf)
               write(6,2)ITRA, IPART, VECT(3), snvert, svrtx(3), 
     +              ubuf(1), iAccept
 2             format(//, ' muonArmForcedAccept <I>: Accepting track ',
     +              i5, ',  particle ID ', i3, ',  Z ', g9.2,
     +              ',  SNVERT ', i4, ', ZVERT ', g9.2, ', UBUF(1) ',
     +              g6.0, ',  iAccept ', i3,//)
            endif               ! diagnostic output

         endif                  ! check if first time this track has been accepted
         
         return

      endif                     ! no more checking if particle has reached the zAccept boundary

      if(IVERT.eq.1) then
         newPrimary = 1         ! switch to indicate that a new primary particle has been started or is continuing
         jAccept = 1
         return
      endif                     ! still the primary particle

      if(IPART.eq.5 .or. IPART.eq.6) then
         return
      endif                     ! keep all muons


c     Check if the particle has the primary as its immediate ancestor

c      call gfkine(ITRA,svrtx,pvrtx,spart,snvert,ubuf,nubuf)
c      jTrack = ubuf(1)
      jTrack = 0
      snvert = ivert
      if(jKine.lt.10) then
         jKine = jKine + 1
         write(6,3)ITRA, IPART, VECT(3), jTrack, snvert
 3       format(//, ' muonArmForcedAccept <I>: Vertex check ', i5,
     +        ',  particle ID ', i3, ',  Z ', g9.2,
     +        ', jTrack ', i5, ',  SNVERT ', i5)
      endif                     ! diagnostic print out
      if(snvert.eq.2.and.(IPART.gt.7.and.IPART.lt.13)) then
         if(kKine.lt.10) then
            kKine = kKine + 1
            write(6,4)ITRA
 4          format('  muonArmForcedAccept <I>: First descendant ', i5) 
         endif                  ! diagnostic print out
         return                 ! particle is an allowed immediate descendant of the primary
      endif                     ! check if particle is an allowed immediate descendant of the primary


c     At this point we know the particle is not a primary, it is not a muon, and it is not a first descendant
c     It has also not reached the Z acceptance boundary
c     So we stop the tracking

      iStop = 1                 ! tell GEANT that the particle has disappeared

      if(iReject.lt.10 .and. newPrimary.eq.1)then

         iReject = iReject + 1
         write(6,5)ITRA, IPART, VECT(3), snvert, iReject
 5       format(//, ' muonArmForcedAccept <I>: Rejecting track ', i5,
     +              ',  particle ID ', i3, ',  Z = ', g9.2,
     +              ',  SNVERT ', i4, ',  iReject ', i3,//)

         newPrimary = 0

      endif                     !  diagnostic output for rejected particles

      return
      end
