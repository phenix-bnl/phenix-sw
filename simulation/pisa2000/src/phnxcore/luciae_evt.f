c     $Id: luciae_evt.f,v 1.3 2008/05/21 08:22:10 hpereira Exp $
      integer function luciae_evt( )
      implicit none

c     Descriptions:
c     Routine to read a complete LUCIAE event from a file and
c     load GUEVGEN common blocks with full event information

c     MAP:
c     Called by: GUEVGEN
c     Calls:     EVENT_FILTER    Routine to read and filter event from file
 

c     Global declarations:
c     ====================
 
#include "event.inc"
#include "evntcode.inc"
#include "guevgen.inc"
 
c     Local declarations
      integer  ireturn
      integer  j
      logical  status
      real     xnor
 
c     gross kludge-up of event number handling since
c     it appears that read_luciae doesn't return a
c     useful event number
      integer  ievent
      save   ievent
      data ievent /0/
 
c     Executable code
c     Set event code
      event_code = luciae

c     Read in new event from LUCIAE data file
      call event_filter(ireturn)
      if (ireturn.eq.-1)then
        status = .true.
      else
        write(*, '(a, i4)') 'luciae_evt - ireturn = ',ireturn
        status = .false.
        luciae_evt = 0
        Go to 500
      endif
 
c     Set event number
      ievent = ievent + 1
      numevt = ievent
 
c     copy luciae arrays into the arrays that guevgen expects (viz. pptot, 
c     idtot, xyzmv)
      mxtot = min (nptls, max_mxtot)
      if (mxtot .lt. nptls) then
        write (6,'(1h ,a,i7,a,a,i7)')
     +      ' luciae_evt - keeping ', mxtot, ' particles',
     +      ' of true luciae event ', numevt
        write (6,'(1x,a,i7,a)')
     +      ' luciae_evt - there were actually ',nptls,
     +      ' particles in the full event.'
      end if
      
      ! luciae provide single particle vertex
      do j = 1, mxtot
        idtot(j) = gtype(j)
        pptot(2,j) = p4vec(1,j)
        pptot(3,j) = p4vec(2,j)
        pptot(4,j) = p4vec(3,j)
        pptot(1,j) = p4vec(4,j)
        xyzmv(1,j)=xyzvec(1,j)
        xyzmv(2,j)=xyzvec(2,j)
        xyzmv(3,j)=xyzvec(3,j)
              
        ! set the 'global' event vertex to the one 
        ! from the first encountered particle
        if( j .eq. 1 ) then
          xyz(1) = xyzmv(1,j)
          xyz(2) = xyzmv(2,j)
          xyz(3) = xyzmv(3,j)
        endif
        
      end do
      
C     this is some test code to enable me to put together
C     events run in dfferent blocks of runs -- I will use
C     this file to force the same vertex position in each
C     case -- JPS 24 Jan 96.

      write ( 47,501 ) ievent,xyz
  501 format ( 'luciae_evt - ievent,xyz vertex', i5,3f10.4)
      luciae_evt = -1
 
 500  continue
 
      if (.not.status) write(6,*) 'luciae_evt - reading encountered ',
     &  'eof or error condition'
      chevt_name = 'luciae source'
 
      return
      end
