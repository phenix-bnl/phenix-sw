c     $Id: hijing_evt.f,v 1.6 2008/05/28 22:08:10 lajoie Exp $
      integer function hijing_evt()
      implicit none

c     Descriptions:
c     Routine to read a complete HIJING event from a file and
c     load GUEVGEN common blocks with full event information

c     MAP:
c     Called by: GUEVGEN
c     Calls:     EVENT_FILTER   Routine to read and filter event from file
 
c     Global declarations:
#include "event.inc"
#include "evntcode.inc"
#include "guevgen.inc"
      
c     Local declarations
c     ==================
 
      Integer  IRETURN
      Integer  J
      Integer  iFirstMom /0/
      Logical  STATUS
      REAL     XNOR

      integer idtemp
      real tran
      Integer iFirstP0toPC /0/
      Integer iFirstK0toKC /0/
      Integer iFirstPitoKaon /0/

      Logical logP0toPC /.FALSE./
      Logical logPCtoP0 /.FALSE./
      Logical logK0toKC /.FALSE./
      Logical logKCtoK0 /.FALSE./
      Logical logPitoKaon /.FALSE./
      Logical logKaontoPi /.FALSE./

      Integer  IEVENT
      Save   IEVENT
      Data IEVENT /0/
 

c     Executable code
c     ===============

c     Set event code number

      event_code = hijing
 

c     read in new event from hijing data file
c     ======================================
      call event_filter(ireturn)
      if (ireturn.eq.-1)then
         status = .true.
      else
         write(*, '(a, i4)') 'hijing_evt - ireturn = ',ireturn
         status = .false.
         hijing_evt = 0
         go to 500
      endif
 

c     set event number
c     ================
      ievent = ievent + 1
      numevt = ievent
 

c     copy hijing arrays into the arrays that guevgen expects (viz. pptot, 
c     idtot)
c     =======================================================================
      mxtot = min (nptls, max_mxtot)
      if (mxtot .lt. nptls) then
          write (6,'(1h ,a,i7,a,a,i7)')
     &        ' hijing_evt - keeping ', mxtot, ' particles',
     &        ' of true hijing event ', numevt
          write (6,'(1x,a,i7,a)')
     &        ' hijing_evt - there were actually ',nptls,
     %        ' particles in the full event.'
      end if

c     hijing does not store the vertex creation point
c     set to zero here, event vertex can be smeared later
c     JGL 5/28/08
      xyz(1) = 0.0
      xyz(2) = 0.0
      xyz(3) = 0.0

      if(iFirstMom.eq.0.and.(ptChange.ne.1.0.or.pzChange.ne.1.0))then
        
         iFirstMom = 1
         write(6,121)ptChange, pzChange
 121     format('hijing_evt - warning, the px and py are ',
     +          'multiplied by a factor ',f8.4,
     +          'and the pz is multiplied by a factor ',f8.4)
                
      endif ! check on momentum change

      if(ifirstp0topc.eq.0.and.p0topch.ne.0.0)then
        
        ! set pi0 to pch or pch to pi0
        ifirstp0topc = 1
        if(p0topch.gt.0)then
          logp0topc = .true.
        else
          logpctop0 = .true.
        endif 
        write(6,122)p0topch
 122    format('hijing_evt - warning, the pizero to ',
     +    'picharge factor is ',f8.4)
         p0topch = abs(p0topch)
      endif 

      ! set k0 to kch or kch to k0
      if(ifirstk0tokc.eq.0.and.k0tokch.ne.0.0)then
        ifirstk0tokc = 1
        if(k0tokch.gt.0)then
          logk0tokc = .true.
        else
          logkctok0 = .true.
        endif 
        write(6,123)k0tokch
 123    format('hijing_evt - warning, the kzero to ',
     +    'kcharge factor is ',f8.4)
         k0tokch = abs(k0tokch)
      endif 

      ! set k0 to kch or kch to k0
      if(ifirstpitokaon.eq.0.and.pitokaon.ne.0.0)then
        ifirstpitokaon = 1
        if(pitokaon.gt.0)then
          logpitokaon = .true.
        else
          logkaontopi = .true.
        endif 
        write(6,124)pitokaon
 124    format('hijing_evt - warning, the pi to ',
     +    'kaon factor is ',f8.4)
        pitokaon = abs(pitokaon)
      endif
      
      do j = 1, mxtot
         idtemp = gtype(j)

         if(idtemp.eq.7.and.logp0topc)then
            call grndm(tran,1)
            if(tran.le.p0topch)then
               call grndm(tran,1)
               if(tran.lt.0.5)then
                  idtemp = 8
               else
                  idtemp = 9
               endif  ! check on change to pi+ or pi-
            endif  ! check if change for this pi0
         endif  ! check if pi0 and a possible change

         if((idtemp.eq.8.or.idtemp.eq.9).and.logPCtoP0)then
            call grndm(tran,1)
            if(tran.le.p0topch)then
               idtemp = 7
            endif  ! check if change for this pi+ or pi-
         endif  ! check if pi+ or pi- and a possible change

         if(idtemp.eq.10.and.logK0toKC)then
            call grndm(tran,1)
            if(tran.le.k0tokch)then
               call grndm(tran,1)
               if(tran.lt.0.5)then
                  idtemp = 11
               else
                  idtemp = 12
               endif  ! check on change to K+ or K-
            endif  ! check if change for this pi0
         endif  ! check if K0 and a possible change

         if((idtemp.eq.11.or.idtemp.eq.12).and.logKCtoK0)then
            call grndm(tran,1)
            if(tran.le.k0tokch)then
               idtemp = 10
            endif  ! check if change for this K+ or K-
         endif  ! check if K+ or K- and a possible change

         if((idtemp.eq.7.or.idtemp.eq.8.or.idtemp.eq.9)
     +       .and.logPitoKaon)then
            call grndm(tran,1)
            if(tran.le.pitokaon)then
               idtemp = idtemp + 3
            endif  ! check if change for this pion
         endif  ! check if pi0, pi+, or p- and a possible change

         if((idtemp.eq.10.or.idtemp.eq.11.or.idtemp.eq.12)
     +       .and.logKaontoPi)then
            call grndm(tran,1)
            if(tran.le.pitokaon)then
               idtemp = idtemp - 3
            endif  ! check if change for this Kaon
         endif  ! check if K0, K+, or K- and a possible change

         idtot(j) = idtemp
         pptot(2,j) = ptchange*p4vec(1,j)
         pptot(3,j) = ptchange*p4vec(2,j)
         pptot(4,j) = pzchange*p4vec(3,j)
         pptot(1,j) = p4vec(4,j)   ! should really change p4 too, but it is not used
         
c     set particle vertices as well
         xyzmv(1,j) = xyz(1); 
         xyzmv(2,j) = xyz(2); 
         xyzmv(3,j) = xyz(3); 

      end do

      hijing_evt = -1
 
 500  continue
 
      if (.not.status) write(6,*) ' hijing_evt - reading encountered ',
     &  'eof or error condition'
      chevt_name = 'hijing source'

      return
      end
      
