      subroutine hijingpisa()
      implicit none
c
c     Event Interface from HIJING to PISA 
c
c     Original Author: Charles F. Maguire, May 29 1996
c
c     Function: transfers HIJING momentum and vertex infomation
c
c     Calls: None
c
c     Called by: HIJING_C
c
c     Revision History
c     C.F. Maguire    March 6, 1999   Add jet particles to output
c                                     Peter Steinberg request
c
c
c     Global variables
c
      include 'hijingpar.inc'
      include 'himain.inc'      
      include 'event.inc'


      real hipr1, hint1
      integer ihpr2, ihnt2
      COMMON/HIPARNT/HIPR1(100),IHPR2(50),HINT1(100),IHNT2(50)
c      common/histrng/nfp(300,15),pp(300,15),nft(300,15),pt(300,15)
c
c     Local variables
c
C      integer i
      integer ipart
      integer ixyz
C      real thetadeg
      real DEGRAD /57.2957795/
      real ptot

      real costh, costhmin, costhmax ! faster, more precise calculation than "theta"

      costhmin = cos(thetamax/DEGRAD)
      costhmax = cos(thetamin/DEGRAD)

      nptls = 0
      bimevt = bimpact
      ktry = nbinary  ! using variable in event.inc originally allocated for HIJET 

      do ipart = 1, Natt

         ptot = sqrt(patt(ipart,1)*patt(ipart,1) +
     +               patt(ipart,2)*patt(ipart,2) +
     +               patt(ipart,3)*patt(ipart,3))

         if ( ptot.ne.0.0 ) then
            costh = patt(ipart,3)/ptot
         else 
            costh = 1.0
         endif

         ! Handle possible precision errors in momenta
         if ( costh.gt.1.0 ) then
            costh = 1.0
         else if ( costh.lt.-1.0 ) then
            costh = -1.0
         endif

C         if(ptot.ne.0.0)then
C            thetadeg = DEGRAD*acos(patt(ipart,3)/ptot)
C         else
C            thetadeg = 0.0
C         endif

C         if(thetadeg.ge.thetamin.and.thetadeg.le.thetamax)then
         if(costh.ge.costhmin.and.costh.le.costhmax)then
            nptls = nptls + 1
            idptl(nptls) = katt(ipart,1)
            do ixyz = 1,4
               p4vec(ixyz,nptls) = patt(ipart,ixyz)
               xyzvert(ixyz,ipart) = vatt(ipart,ixyz) ! DLW: Extract the vertex information
            enddo               !  loop over coordinates
         endif                  ! check on Theta in range
      enddo  ! loop over number of particles
      naccepted = nptls

      if(trigger_jet.eq.1)then
c
c     Add jet four momenta (Peter Steinberg request)
c
         nptls = nptls + 2
         idptl(Natt+1) = 53
         idptl(Natt+2) = 54
         ipart = Natt+1
         do ixyz = 1,4
            p4vec(ixyz,ipart) = hint1(20+ixyz)
         enddo
         ipart = Natt+2
         do ixyz = 1,4
            p4vec(ixyz,ipart) = hint1(30+ixyz)
         enddo
      endif

      return
      end









