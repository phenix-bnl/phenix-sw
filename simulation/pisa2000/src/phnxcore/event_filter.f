      subroutine event_filter(ireturn)
      implicit none

c     Original author: C.F. Maguire
c     Creation date: May 30, 1996

c     Description:
c     ============

c     Routine to read the RHI event generator (HIJET, VENUS, HIJING, etc.)
c     Events are processed through a kinematic filer contained in event.par
c     Based on READ_HIJET routine which had proliferated into READ_VENUS,
c            READ_LUCIAE, etc.
c     Since all the event generator files should have a common format, then
c     a single READ_EVENT routine has been substituted.


c     MAP:
c     ===

c     Called by: HIJET_EVT, VENUS_EVT, HIJING_EVT, RQMD_EVT, LUCIAE_EVT

#include "event.inc"
#include "evtzebra.inc"
#include "evntcode.inc"
#include "u_local.inc"
#include "gckine.inc"
#include "guphnx.inc"
#include "guevgen.inc"
#include "gcunit.inc"
#include "pisa_parts.inc"
#include "kincut.inc"

c     Local variables

      integer*4 egzin, egzinit,egzend
      integer*4 istat,ireturn
      integer*4 i
      logical first /.true./
      integer ievent/0/
      save first
      integer   ii
      real   rtemp, ptot
      real   thsupp
 
      integer kitrtyp   ! local variable for GFPART call
      real kcharge      ! local variable for GFPART call
      real ktlife       ! local variable for GFPART call
      character*20 chpart
      integer bad_id
      integer nwb /0/
      real ub(1)
 
      ! Kinematic cut additions
      integer idtest       ! include/exclude loop variable
      integer id_geant     ! GEANT ID number of a given particle

      ! Namelist input control (variables specified in kincut common )
      integer nskip_evt /0/ ! number of events to skip on input
      real the_min2 /0./ ! minimum angle for primary pi0
      real the_max2 /180./ ! maximum angle for primary pi0
      integer north_south /0/ ! control for both North and South angles

      real the_zdc /0.0/      ! zdc angle cut
      real b_min  /0.0/       ! default minimum impact parameter
      real b_max  /100.0/     ! default maximum impact parameter
      real ptfactor /1.0/     ! default for changing px and py
      real pzfactor /1.0/     ! default for changing pz
      real p0topchfactor /0.0/  ! default for changing pi0 to pi+ or pi-
      real k0tokchfactor /0.0/  ! default for chaning k0 to k+ or k-
      real pitokaonfactor /0.0/ ! default for changing pions to kaons

      namelist /epar/ y_min, y_max, pt_min, pt_max, the_min, the_max,
     +  phi_min, phi_max, vrms, p_min, p_max, iincl, include,
     +  xyz0_input,north_south, the_min2, the_max2, nskip_evt,
     +  t0cent, t0width, b_min, b_max, ptfactor, pzfactor,
     +  p0topchfactor,  k0tokchfactor,  pitokaonfactor

      integer nptlsa
      logical hexist
      logical logexcl /.false./
      logical logincl /.false./
      logical logchn /.false./
      save logexcl, logincl

      real etaval
      integer iqval

c     Executable code
      ireturn=-1
      
      if(first)then
        
        first = .false.
        write (lout,*) 'event_filter - initialization'
        ievent=0
        z_input = .true.

c       Open kinematics filter file
        open(
     +    unit=17,file='event.par',status='old',
     +    form='formatted',err=793)
        go to 794
        
793     continue
        stop '  Unable to open  event.par  kinematics cut file'

794     continue
        t0cent = 0.0   ! initialize for backward compatibility
        t0width = 0.0  ! initialize for backward compatibility
        read(17,nml=epar,err=795,end=795)
        close(unit=17)
        go to 796
        
795     continue
        stop '  Error in reading  event.par '
        
796     continue
        
        ptChange = ptfactor
        pzChange = pzfactor
        p0topch = p0topchfactor
        k0tokch = k0tokchfactor
        pitokaon = pitokaonfactor
        
        write(LOUT,797)y_min, y_max, pt_min,
     +    pt_max, the_min, the_max,
     +    the_min2, the_max2,
     +    phi_min, phi_max, p_min, p_max,
     +    vrms, iincl, xyz0_input,
     +    t0cent, t0width,b_min,b_max, the_zdc,
     +    ptfactor, pzfactor,
     +    p0topchfactor,k0tokchfactor,
     +    pitokaonfactor
797     format('event_filter - ',
     +    'kinematic cuts from the PISA2000 event.par',
     1    ' file',/,3x,
     2    ' y_min = ',g10.3,',   y_max = ',g10.3,/,3x,
     3    ' pt_min = ',g10.3,' GeV/c,   pt_max = ',g10.3,' Gev/c',/,3x,
     4    ' the_min = ',g10.3,' deg.,   the_max = ',g10.3,' deg.',/,3x,
     5    ' the_min2 = ',g10.3,' deg.,  the_max2 =',g10.3,' deg.',/,3x,
     6    ' phi_min = ',g10.3,' deg.,   phi_max = ',g10.3,' deg.',/,3x,
     7    ' p_min = ',g10.3,' GeV/c,   p_max = ',g10.3,' Gev/c',/,3x,
     8    ' vrms = ',3g8.2,' cm,  number of included particle IDs = ',
     9    i4,/,3x,' xyz0_input ',3f6.2,' cm',/,3x,
     +    ' t0cent = ',g10.3,' ns,  t0width = ', g10.3,' ns',/,3x,
     +    ' b_min = ', g10.3,' fm,  b_max = ', g10.3,' fm',/,3x,
     +    ' the_zdc = ', g10.3, ' degrees',/3x,
     +    ' ptfactor = ', g10.3, ',  pzfactor = ', f10.3,/,3x,
     +    ' p0topchfactor = ', g10.3, ' k0tokchfactor = ', g10.3,
     +    ' pitokaonfactor = ', g10.3,/) 

        if(north_south.eq.1)then
          write(6,792)
 792      format(3x,' North_South option selected for Theta cuts',/)
        endif
        
        if(iincl.ne.0)then
          if(iincl.gt.0)then
            logincl = .true.
            if(iincl.gt.20)iincl = 20
            write(LOUT,798)(include(i),i=1,iincl)
798         format(3x,'The following is a list ',
     +        'of INCLUDED particles:',
     1        2(/,3x,10i4),/)
            else
              logexcl = .true.
              iincl = -iincl
              if(iincl.gt.20)iincl = 20
              write(LOUT,799)(include(i),i=1,iincl)
799           format(3x,'The following is a list ',
     +          'of EXCLUDED particles:',
     1          2(/,3x,10i4),/)
            endif     ! check on INCLUDE or EXCLUDE logic
          else
            write(LOUT,'(/,a,/)') '  All particle IDs accepted'
          endif       !  check on IINCL different from 0
          
          if ( y_max .le. y_min ) then
            logchn = .true.
            y_min = -100.0
            y_max =  100.0
          endif
          
          if ( pt_max .le. pt_min ) then
            logchn = .true.
            pt_min = 0.0
            pt_max = 1.0e20
          endif
          
          if ( p_max .le. p_min ) then
            logchn = .true.
            p_min = 0.0
            p_max = 1.0e20
          endif
          
          if ( the_max .le. the_min ) then
            logchn = .true.
            the_min =   0.0
            the_max = 180.0
         endif
        
         if ( phi_max .le. phi_min ) then
            logchn = .true.
            phi_min = -180.0
            phi_max =  180.0
         endif
        
        if(logchn)
     1    write(LOUT,7971)y_min, y_max, pt_min,
     1    pt_max, the_min, the_max,
     1    phi_min, phi_max, p_min, p_max
7971    format('event_filter - revised kinematic cuts',
     1    ' file',/,3x,
     2    ' y_min = ',g10.3,',   y_max = ',g10.3,/,3x,
     3    ' pt_min = ',g10.3,' GeV/c,   pt_max = ',g10.3,' Gev/c',/,3x,
     4    ' the_min = ',g10.3,' deg.,   the_max = ',g10.3,' deg.',/,3x,
     5    ' phi_min = ',g10.3,' deg.,   phi_max = ',g10.3,' deg.',/,3x,
     6    ' p_min = ',g10.3,' GeV/c,   p_max = ',g10.3,' Gev/c',/)
           
        the_min  = the_min / 57.29578
        the_min2  = the_min2 / 57.29578
        the_max  = the_max / 57.29578
        the_max2  = the_max2 / 57.29578
        the_zdc  = the_zdc / 57.29578
        phi_min  = phi_min / 57.29578
        phi_max  = phi_max / 57.29578
         
        if(pkine(6).ne.-2.0)then
          istat = egzinit()
        else
          istat = -1
        endif  ! check if non-ZEBRA events file
        if (istat.ne.-1) goto 9000
        if(pkine(6).ne.-2.0)then
          pkine(6) = 0.0
        endif  ! check if non-ZEBRA events file

c       book the histograms (JPS moved here from inside event loop)

         if(.not.hexist(10))call hbook1(10,'EVENT PART ID BEFORE CUTS',
     1      100,0.,20.,0.)
         if(.not.hexist(11))call hbook1(11,'EVENT PART ID AFTER CUTS',
     1      100,0.,20.,0.)
         if(.not.hexist(12))call hbook1(12,'EVENT  Y  BEFORE CUTS',
     1      100,-7.,7.,0.)
         if(.not.hexist(13))call hbook1(13,'EVENT  Y  AFTER CUTS',
     1      100,-7.,7.,0.)
         if(.not.hexist(14))call hbook1(14,'EVENT THETA BEFORE CUTS',
     1      100, 0.,180.,0.)
         if(.not.hexist(15))call hbook1(15,'EVENT THETA AFTER CUTS',
     1      100, 0.,180.,0.)
         if(.not.hexist(16))call hbook1(16,'EVENT PTOT BEFORE CUTS',
     1      100, 0.,5.0,0.)
         if(.not.hexist(17))call hbook1(17,'EVENT PTOT AFTER CUTS',
     1      100, 0.,5.0,0.)
         if(.not.hexist(18))call hbook1(18,'EVENT PTRAN BEFORE CUTS',
     1      100, 0.,5.0,0.)
         if(.not.hexist(19))call hbook1(19,'EVENT PTRAN AFTER CUTS',
     1      100, 0.,5.0,0.)

C  Histograms 22-23 added by JPSullivan

         if(.not.hexist(22))call hbook1(22,'EVENT  [c]  BEFORE CUTS',
     1      56,-7.,7.,0.)
         if(.not.hexist(23))call hbook1(23,'EVENT  [c]  AFTER CUTS',
     1      56,-7.,7.,0.)
         if(nskip_evt.gt.0)then
            write(6,1)nskip_evt
 1          format(/,3x,'Requested skipping of ',i6,' events',/)
            if(pkine(6).eq.-2.0)then
               write(6,1001)
 1001          format(/,3x,'NSKIP_EVT parameter not implemented',
     +                     ' for non-ZEBRA file event input',/)
               nskip_evt = 0
            endif  ! check on non-ZEBRA event input
         endif  ! check if event skipping requested
      endif   ! initialization on first call

      if(pkine(6).eq.-1.0)then  ! check for forced re-open
         pkine(6) = 0.0
         istat = egzinit()
         ievent = 0
         if (istat.ne.-1) goto 9000
      endif

c     Event loop
c     ==========

 10   continue
      istat = 1
      ievent=ievent+1
      if(nskip_evt.eq.0)then
         write ( lout,98 ) ievent
98    format(/,5x,'Call to egzin at ievent ',i6)
      else
         if(ievent - 10*(ievent/10) .eq. 0)then
            write(lout,98)ievent
         endif  ! give message only every 10 events when event skipping
      endif  ! check on event skipping request

c     This call does the ZEBRA input event-by-event

      if(pkine(6).ne.-2.0)then
         istat = egzin()
      else
         istat = -1
      endif  ! check if non-ZEBRA input file
      if (istat.ne.-1) goto 100
      goto 110

 100  continue   ! error condition

c     Should return here??

      write(*,*)'  Error reading event,',i-1,' events read.'
      if(pkine(6).ne.-2.0)then
         istat=egzend()
      endif
      first=.true.
      ireturn=2
 110  continue   ! no error condition

      if(ievent.le.nskip_evt)then
         go to 10     ! backward GOTO, be careful here
      endif

      if(bimevt.lt.b_min.or.bimevt.gt.b_max)then
         write(6,109)ievent, bimevt
109      format(/,'  EVENT_FILTER <I>: Skipping event ', i4,/,
     +          2x,' impact parameter ', f5.2,' fm is not in cut')
         go to 10     ! backward GOTO, need to be careful
      endif  ! check on impact parameter

      call zdc_fragment ! event COMMON is modified with addition of deuterons

      bad_id = 0       ! count of bad IDs
      nptlsa = 0       ! count of accepted particles
      do i=1,nptls       ! loop over all particles in EVENT event
         ptot = sqrt(p4vec(1,i)**2 + p4vec(2,i)**2 +
     1          p4vec(3,i)**2)
         pt(i) = sqrt(p4vec(1,i)**2 + p4vec(2,i)**2)
         if(p4vec(3,i).ne.0..or.pt(i).ne.0.) then
            theta(i) = atan2(pt(i),p4vec(3,i))
         else
            theta(i) = 0.0001
         endif
         if(pt(i).ne.0.) then
            phi(i) = atan2(p4vec(2,i),p4vec(1,i))
         else
            pt(i) = 0.000001
            phi(i) = 0.001
          endif

c       EVENT output has kinetic energy in p4vec(4,i) spot
c       Change that to the total energy; first get the GEANT mass

          if((gtype(i).gt.0.and.gtype(i).lt.56).or.
     +      (gtype(i).ge.PISA_PART_MIN.and.
     +    gtype(i).le.PISA_PART_MAX))then  
          ! should have pisa_parts limits
          call gfpart(gtype(i),chpart,kitrtyp,mass(i),kcharge,
     1      ktlife,ub,nwb)
          p4vec(4,i) = sqrt(ptot*ptot + mass(i)*mass(i))
          if(i.lt.0)write(LOUT,113)i,gtype(i),mass(i)
113       format(/,2x,'NPTLSA ',i4,' ID ',i4,'  MASS ',g11.4)
        else
          bad_id = bad_id + 1
          write(*,114)i,gtype(i),p4vec(4,i),idptl(i)
114       format(/,3x,'EVENT_FILTER <E>: particle ID unknown to GEANT',
     1      /,4x,'particle # ',i6,'  with ID ',i4,'  has T = ',g11.4,/,
     1      4x,'idptl ',i12)
          if(bad_id.gt.20)then
            stop ' EVENT_FILTER <E>: Too many (>20) bad IDs'
          endif  ! check on too many bad IDs

c         drop the particle

          logchn = .true.
          go to 800
        endif  ! check on particle ID

        if((p4vec(4,i)-p4vec(3,i)).gt.0.) then
          rtemp = (p4vec(4,i)+p4vec(3,i))/(p4vec(4,i)-p4vec(3,i))
        else
          rtemp = -1.0
        endif
        if(rtemp.gt.0.) then
          y(i) = 0.5*alog(rtemp)
        else
          y(i) = 10.0
        endif
        
        if(i.lt.0)then       ! debug printout turned off
        write(LOUT,111)i,(p4vec(ii,i),ii=1,4),
     1    ptot,pt(i),theta(i),
     2    phi(i),y(i),gtype(i)
111     format(/,1x,'i,p4v',i2,1x,4g10.4,' ptot,pt ',2g10.3,/,1x,
     1    ' thet, phi, y ',3g10.3,' GID ',i6)
        endif  ! debug
              
c       fill the pre-cut histograms
        call hfill(10,float(gtype(i)),0.,1.)
        call hfill(12,y(i),0.,1.)
        call hfill(14,57.29578*theta(i),0.,1.)
        call hfill(16,ptot,0.,1.)
        call hfill(18,pt(i),0.,1.)

C       Make a histogram of the number of charged particles vs eta.
C       also count charged particles with eta < 2.
        if((gtype(i).gt.0.and.gtype(i).lt.49).or.
     +    (gtype(i).ge.pisa_part_min.and.
     +    gtype(i).le.pisa_part_max))then
          call gfpart(gtype(i),chpart,kitrtyp,mass(i),kcharge,
     1      ktlife,ub,nwb)
          iqval = nint ( kcharge )
          if ( iqval.ne.0 ) then
            if ( theta(i).lt.0.001 ) then
              etaval = 7.6
            else if ( theta(i).gt.3.1405 ) then
              etaval = -7.6
            else
              etaval = alog(1./(tan(theta(i)/2.)))
            end if
            call hfill ( 22, etaval, 0., 1. )
          end if
        end if

c       now do the kinematic checks
        logchn = .false.
        if(theta(i).lt.the_min.or.theta(i).gt.the_max)then
          if(north_south.eq.0)then
            logchn = .true.
            go to 800
          else
            thsupp = 3.141593 - theta(i)  ! supplementary angle in radians
            if(thsupp.lt.the_min.or.thsupp.gt.the_max)then
              logchn = .true.
              go to 800
            endif ! out-of-limits check
          endif  ! check if the North_South option is being used
        endif
        
        if(pt(i).lt.pt_min.or.pt(i).gt.pt_max)then
          logchn = .true.
          go to 800
        endif
        if(ptot.lt.p_min.or.ptot.gt.p_max)then
          logchn = .true.
          go to 800
        endif
        if(phi(i).lt.phi_min.or.phi(i).gt.phi_max)then
          logchn = .true.
          go to 800
        endif
        
        if(y(i).lt.y_min.or.y(i).gt.y_max)then
          logchn = .true.
          go to 800
        endif
        
        id_geant = gtype(i)
        if(id_geant.eq.7)then

c         Special kinematic cut on pi0

          if(theta(i).lt.the_min2.or.theta(i).gt.the_max2)then
            logchn = .true.
            go to 800
          endif
        endif  ! check particle is a pi0
              
        if(.not.logexcl.and..not.logincl)go to 800

c       Now check the INCLUDED or EXCLUDED list
        if(logincl)then
          do idtest = 1,iincl
            if(id_geant.eq.include(idtest))then
c             particle is on included list, so accept it
              go to 800
            endif   !  check on match
          enddo   ! loop on included IDs

c         particle was not on included list, so reject
          logchn = .true.
          go to 800
        endif
        
        if(logexcl)then
          do idtest = 1,iincl
            if(id_geant.eq.include(idtest))then
c             particle is on excluded list, so reject it
              logchn = .true.
              go to 800
            endif   !  check on match
          enddo   ! loop on include list

c         particle was not on excluded list, so accept it
          go to 800
        endif
        
800     continue    ! branch point for all inclusion/exclusion checks
        
        if(logchn)then
          
c         drop from the list

          if(i.lt.0)then     ! debug printout turned off
          write(LOUT,811)i,(p4vec(ii,i),ii=1,4),gtype(i)
811       format(1x,'Drop to #i,p4v,id',i5,1x,4g10.3,i3)
        endif  ! debug
      else

c       keeping particle
        
        nptlsa = nptlsa + 1
        do ii = 1,4
          p4vec(ii,nptlsa) = p4vec(ii,i)
          gtype(nptlsa) = gtype(i)
        enddo
        
c       Fill "kept particle" histograms
        call hfill(11,float(gtype(i)),0.,1.)
        call hfill(13,y(i),0.,1.)
        call hfill(15,57.29578*theta(i),0.,1.)
        call hfill(17,ptot,0.,1.)
        call hfill(19,pt(i),0.,1.)
        if ( gtype(i).eq.2 ) call hfill(118,pt(i),0.,1.)
        if ( gtype(i).eq.3 ) call hfill(120,pt(i),0.,1.)
        if ( iqval.ne.0 ) then
          call hfill ( 23, etaval, 0., 1. )
        end if

        call zdc_fragment_update(nptlsa, i)  ! "remember particle ID"
              
      endif
      enddo   ! loop over all EVENT particles

c     reset NPTLS to the accepted particle number

      write(LOUT,801)nptlsa,nptls,bimevt
801   format(/,3x,'EVENT_FILTER <I>: accepts',i6,' particles from ',
     1          'original set of',i6,' particles',
     2       /,3x,'with event impact parameter at ',f6.2,' fm',/)

      if(nptlsa.lt.0)then
         write(6,802)nptlsa
802   format(/,' EVENT_FILTER <E>: NEG accepted particles = ',i5) 
         stop '  PISA is exiting'
      endif  ! check on particles

      if(nptlsa.eq.0)then
         write(6,803)nptlsa
803   format(//,' EVENT_FILTER <W>: ZERO accepted particles',  
     +       /,'    PISA is continuing',/)
      endif  ! check on particles

      nptls = nptlsa
      ireturn = -1   ! indicates success
      return

c     Error conditions

 9000 continue
      if(pkine(6).ne.-2.0)then
         istat=egzend()
      endif
      first=.true.
      ireturn=3
      return
      end
