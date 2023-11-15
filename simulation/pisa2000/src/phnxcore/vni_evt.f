c     $Id: vni_evt.f,v 1.3 2008/05/21 08:22:14 hpereira Exp $
      integer function vni_evt()
      implicit none

c     Descriptions:
c     Routine to read a complete VNI event from a file and
c     load GUEVGEN common blocks with full event information

c     MAP:
c     Called by: GUEVGEN
c     Calls:     EVENT_FILTER   Routine to read and filter event from file

c     Author:   Charles F. Maguire
c     Created:  May 30, 1996

c     Global declarations:
#include "event.inc"
#include "evntcode.inc"
#include "guevgen.inc"

c     Local declarations
      integer  ireturn
      integer  j
      logical  status
      real     xnor

      integer  ievent
      save   ievent
      data ievent /0/
 
c     Executable code
c     Set event code number
      event_code = vni
 
c     Read in new event from VNI data file
      call event_filter(ireturn)
      if (ireturn.eq.-1)then
         status = .true.
      else
         write(*, '(a, i4)') 'vni_evt - ireturn = ',ireturn
         status = .false.
         vni_evt = 0
         Go to 500
      endif
 
c     Set event number
      ievent = ievent + 1
      numevt = ievent
 
c     Copy VNI arrays into the arrays that GUEVGEN expects (viz. PPTOT, 
C     IDTOT)
      mxtot = min (nptls, max_mxtot)
      if (mxtot .lt. nptls) then
          write (6,'(1h ,a,i7,a,a,i7)')
     &        ' vni_evt - keeping ', mxtot, ' particles',
     &        ' of true vni event ', numevt
          write (6,'(/,1x,a,i7,a)')
     &        ' vni_evt - there were actually ',nptls,
     %        ' particles in the full event.'
      end if

c     VNI does not store the vertex creation point
      do j = 1, mxtot
         idtot(j) = gtype(j)
         pptot(2,j) = p4vec(1,j)
         pptot(3,j) = p4vec(2,j)
         pptot(4,j) = p4vec(3,j)
         pptot(1,j) = p4vec(4,j)
      end do

C     this is some test code to enable me to put together
C     events run in dfferent blocks of runs -- I will use
C     this file to force the same vertex position in each
C     case -- JPS 24 Jan 96.

      write ( 47,501 ) ievent,xyz
 501  format (' vni_evt - ievent,xyz vertex', i5,3f10.4)
      vni_evt = -1
 
 500  continue
 
      if (.not.status) write(6,*) ' vni_evt: - reading encountered ',
     &  'eof or error condition'
      chevt_name = 'vni source'

      return
      end
