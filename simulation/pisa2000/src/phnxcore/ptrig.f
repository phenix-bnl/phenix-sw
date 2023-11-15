      subroutine ptrig
      implicit none

C     Original Author: Shaheen Tonse (LLNL)
C     Creation Date: April 4, 1993

C     Purpose: Routine to take care of Pisa true event looping, using the
C     KUIP command PISA/GLOBAL/PTRIG. This is so that Pisa does
C     not have to change the gxint package, with the exception of the
C     main routine
 
#include "subevt.inc"
#include "guevgen.inc"
#include "guphnx.inc"
#include "gcflag.inc"

C     local variables
c     integer startflag /0/
      integer ntrig, j
      logical kilcon
      
      write( *,* ) 'ptrig'
      
c       ! initialize output
c       root_output = 0
c       zebra_output = 1

c       if(iswit(1).ge.3.and.iswit(1).le.8)then
c         root_output = 1
c         startflag = 0   ! default as subevent output for root clone objects
c         if(iswit(1).gt.4)then

c           ! change to full event output for root clone objects
c           startflag = 1  

c           ! for phool formatted objects
c           if(iswit(1).eq.7.or.iswit(1).eq.8)startflag = 2

c         endif

c         call rootstart(startflag, pisa_file )
c         if(iswit(1).eq.3.or.iswit(1).eq.5.or.iswit(1).eq.7)then
c           zebra_output = 0
c         endif

c       endif

      
      ! get number of requested triggers 
      call kugeti(ntrig)
      
      nrvacc_evt=0
      
      ! use ISWIT(7) to have forced acceptance of a pair, or single particles
      if(iswit(7).ge.1)then
        logacc = .true.  
      endif

      if(logacc) then

c       Forced accept condition
        write(6,*) 'ptrig - integer variable input was ',ntrig
        write(6,*) 
     +    'ptrig - requesting ', ntrig, ' forced-accept events'
        write(6,*) 'ptrig - initial status is not to accept the event'
        do j = 1,9999999
          if(nrvacc_evt.eq.ntrig) goto 5
          logaccepted = .false.  ! default condition is not to accept the event
 2        CALL kuexec('TRIGGER 1')

C         check here for "kill condition", meaning presence of "endit.dat" file
 3        if(kilcon())then
            write(6,*)
     +        'ptrig - Loop is KILLED: NRVACC_EVT = ', nrvacc_evt
            write(6,*) 'ptrig - Number of event calls = ', j-1
            go to 5
          endif
          if(.not.end_evtflg)go to 2
        end do
        
 5      continue
        if(nrvacc_evt.eq.ntrig)then
          write(6,*) 'ptrig - Loop ends: NRVACC_EVT = ', nrvacc_evt
          write(6,*) 'ptrig - Number of event calls = ', j-1
        endif
      else

c     No forced accept condition
      write(6,*) 'ptrig - integer variable input was ',ntrig
      
      do j = 1,ntrig
        if(kilcon())go to 50
 20     call kuexec('trigger 1')

C       check here for "kill condition", meaning presence of "endit.dat" file
        if(.not.end_evtflg)go to 20
      end do
 50   continue
      endif                    ! check on logacc switch`
      
      write( *,* ) 'ptrig - done.'
      return
      end
