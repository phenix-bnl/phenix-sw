c     $Id: pythia_evt.f,v 1.4 2008/05/21 08:22:12 hpereira Exp $
      subroutine pythia_evt(ievstat)

      implicit none

      ! Global variables
#include "guevgen.inc"
#include "gckine.inc"
#include "event.inc"
#include "evntcode.inc"
#include "pythia_e.inc"

      ! Calling variable
      integer ievstat

      ! Local variables
      integer icall /0/
      integer npart         ! number of particles in event
      integer kpart         ! do loop counter
      real pvec(4)          ! 4-momentum of particle (px, py, pz, et)
      real xvec(3)          ! (x,y,z)
      integer ntot_events   ! total number of events in file
      real rootS            ! c.m. energy
      integer kevent        ! event number on file
      integer idPart        ! particle ID
      integer ireturn       ! return from event filter routine
      integer pythiamode /0/
      integer iback

      ! Begin execution
      if(icall.eq.0)then
        event_code = pythia
        ztarg = 1.0
        atarg = 1.0
        zproj = 1.0
        aproj = 1.0
        chevt_name = 'Pythia events from data file'
        open(unit=24,file='pythia.dat',status='old',
     +    access='sequential',form='unformatted',
     +    err=6099)
        read(24,err=5001,end=5003)rootS, ntot_events
        sqrt_s = rootS
        bmin = 0.0
        bmax = 0.0
        bimevt = 0.0
        N_event = ntot_events
      endif  ! initialization

1001  icall = icall + 1
      if(icall.gt.ntot_events)then
         stop ' No more events in file'
      endif
      numevt = numevt + 1  ! increment Event number 
      read(24,err=6001,end=6003)kevent,npart
      read(24,err=6002,end=6004)pyth_proc_id 
      read(24,err=6002,end=6004)pyth_bjork(1),pyth_bjork(2)
      read(24,err=6002,end=6004)
     +  pyth_partstu(1),pyth_partstu(2),
     +  pyth_partstu(3)
      read(24,err=6002,end=6004)pyth_qsqr
      read(24,err=6002,end=6004)pyth_ptrans

      ! intermediate particles
      do kpart = 1,4
         read(24,err=6006,end=6008)intr_part_id(kpart)
         read(24,err=6009,end=6010)
     +    intr_part_p(1,kpart),
     +    intr_part_p(2,kpart),
     +    intr_part_p(3,kpart),
     +    intr_part_p(4,kpart)
      enddo  ! intermediate particle store
      if(npart.gt.maxptl)then
         stop ' pythia_evt - Too many particle in event'
      endif

      ! test with first particle to determine pythia format mode
      ! original mode : idPart, px,py,pz,E
      ! new      mode : idPart, px,py,pz,E,x,y,z
      if (pythiamode.ne.0) goto 4002 
      read(24)idPart
      read(24,err=4001)pvec(1), pvec(2), pvec(3), pvec(4)
     +                 ,xvec(1) ,xvec(2) ,xvec(3)
      pythiamode = 2
      backspace(24)
      backspace(24)
      goto 4002
4001  pythiamode = 1    
      rewind(24)
      read(24,err=5001,end=5003)rootS, ntot_events
      icall = icall - 1
      numevt = numevt - 1 
      goto 1001

4002  write(*,*) "pythiamode " , pythiamode

      do kpart = 1, npart
        read(24,err=6005,end=6007)idPart
        if (pythiamode.eq.1) then
          read(24,err=6005,end=6007)pvec(1), pvec(2), pvec(3), pvec(4)
          xvec(1) = 0.0
          xvec(2) = 0.0
          xvec(3) = 0.0
        elseif (pythiamode.eq.2) then
          
          ! multi vertex input mode
          read(24,err=6005,end=6007) 
     +      pvec(1), pvec(2), pvec(3),pvec(4), 
     +      xvec(1), xvec(2), xvec(3)
            
        endif
        
        p4vec(1,kpart) = pvec(1)
        p4vec(2,kpart) = pvec(2)
        p4vec(3,kpart) = pvec(3)
        p4vec(4,kpart) = pvec(4)
        xvec(1) = xvec(1)/10.0   ! convert from mm to cm
        xvec(2) = xvec(2)/10.0   ! convert from mm to cm
        xvec(3) = xvec(3)/10.0   ! convert from mm to cm
        xyzvec(1,kpart)= xvec(1)
        xyzvec(2,kpart)= xvec(2)
        xyzvec(3,kpart)= xvec(3)
        
        idptl(kpart) = idPart
        gtype(kpart) = idPart
        id_parent(kpart) = 0
      enddo
      
      ! Call event filter for primary particle cuts
      ! This stores the particles in the GEANT input array
      pkine(6) = -2.0   ! Tells event_filter not to expect a ZEBRA events file
      nptls = npart
      call event_filter(ireturn)
      if (ireturn.ne.-1)then
         write(*, '(A, I4)') 'pythia_evt - ireturn = ',ireturn
         stop " Failure in call to EVENT_FILTER"
      endif      

      ! Copy PYTHIA arrays into the arrays that GUEVGEN expects (viz. PPTOT, IDTOT)
      mxtot = min (nptls, max_mxtot)
      if (mxtot .lt. nptls) then
          write (6,'(1h ,a,i7,a,a,i7)')
     &        ' pythia_evt - keeping ', mxtot, ' particles',
     &        ' of true pythia event ', numevt
          write (6,'(a,i7,a)')
     &        ' pythia_evt - there were actually ',nptls,
     %        ' particles in the full event.'
      end if

      
      do kpart = 1, mxtot
        idtot(kpart) = gtype(kpart)
        pptot(2,kpart) = p4vec(1,kpart)
        pptot(3,kpart) = p4vec(2,kpart)
        pptot(4,kpart) = p4vec(3,kpart)
        pptot(1,kpart) = p4vec(4,kpart)
        xyzmv(1,kpart) = xyzvec(1,kpart)
        xyzmv(2,kpart) = xyzvec(2,kpart)
        xyzmv(3,kpart) = xyzvec(3,kpart)
              
        if( kpart .eq. 1 ) then
          xyz(1) = xyzmv(1,kpart)
          xyz(2) = xyzmv(2,kpart)
          xyz(3) = xyzmv(3,kpart)
        endif
        
      end do

      if (pythiamode.eq.2) multi_xyz = 1
            
      ievstat = -1  ! successful return
      
      return
 5001 continue
 5003 continue
 6001 continue
      stop ' pythia_evt - read error for kevent'
 6002 continue
      stop ' pythia_evt - read error for process ID'
 6003 continue
      stop ' pythia_evt - EOF error for kevent'
 6004 continue
      stop ' pythia_evt - EOF error for process ID'
 6005 continue
      stop ' pythia_evt - read error for GEANT particles'
 6006 continue
      stop ' pythia_evt - read error for intermediate ID'
 6007 continue
      stop ' pythia_evt - EOF error for GEANT particles'
 6008 continue
      stop ' pythia_evt - EOF error for intermediate ID'
 6009 continue
      stop ' pythia_evt - read error for intermediate particles'
 6010 continue
      stop ' pythia_evt - EOF error for intermediate particles'
 6099 continue
      stop ' Error in pythia.dat file open'
      end
