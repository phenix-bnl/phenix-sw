c     $Id: hijet_evt.f,v 1.4 2008/05/21 08:22:10 hpereira Exp $
      integer function hijet_evt ( )
      implicit none

c     Authors S.R. Tonse and C.F. Maguire  (written for original PISA 1.0)

c     Descriptions:
c     Routine to read a complete HIJET event from a file and
c      load GUEVGEN common blocks with full event information

c     MAP:
c     Called by: GUEVGEN
c     Calls:     EVENT_FILTER Routine to read and filter event from file
  

c     Global declarations:
c     ====================
 
#include "event.inc"
#include "evntcode.inc"
#include "guevgen.inc"
 
c     Local declarations
c     ==================
 
      Integer  IRETURN
      Integer  J
      Logical  STATUS

      Integer iFirst /0/
 

c     Gross kludge-up of event number handling since
c     it appears that READ_HIJET doesn't return a
c     useful event number
c     ==============================================
      integer  ievent
      save   ievent
      data ievent /0/
 

c     Executable code
c     ===============
 
c     Set event code
      event_code = hijet

c     Read in new event from HIJET data file
      call event_filter(ireturn)
      if (ireturn.eq.-1)then
        status = .true.
      else
        write (*, '(3x, a, i4/)') 'hijet_evt- ireturn = ', ireturn
        status = .false.
        hijet_evt = 0
        Go to 500
      endif
 
c     set event number
      ievent = ievent + 1
      numevt = ievent
 
c     copy hijet arrays into the arrays that guevgen expects 
c     (viz. pptot, idtot, xyz)
      mxtot = min (nptls, max_mxtot)
      if (mxtot .lt. nptls) then
        write (6,'(1h ,a,i7,a,a,i7)')
     &      ' hijet_evt - keeping ', mxtot, ' particles',
     &      ' of true hijet event ', numevt
        write (6,'(/,1x,a,i7,a)')
     &      ' hijet_evt - there were actually ',nptls,
     %      ' particles in the full event.'
      end if
 
      do j = 1, mxtot
        idtot(j) = gtype(j)
        pptot(2,j) = ptchange*p4vec(1,j)
        pptot(3,j) = ptchange*p4vec(2,j)
        pptot(4,j) = pzchange*p4vec(3,j)
        pptot(1,j) = p4vec(4,j)
      end do
 
      hijet_evt = -1
C     this is some test code to enable me to put together
C     events run in dfferent blocks of runs -- I will use
C     this file to force the same vertex position in each
C     case -- JPS 24 Jan 96.

      write ( 47,501 ) ievent,xyz
 501  format ( 'hijet_evt - ievent,xyz vertex',i5,3f10.4)
 
 500  continue

      if (.not.status) write(6,*) 'hijet_evt - reading encountered ',
     &  'eof or error condition'
      chevt_name = 'hijet source'
      return
      end
