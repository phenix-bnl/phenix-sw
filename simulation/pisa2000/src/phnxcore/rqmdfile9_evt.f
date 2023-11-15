c     $Id: rqmdfile9_evt.f,v 1.4 2008/05/21 08:22:13 hpereira Exp $
      integer function rqmdfile9_evt()
      implicit none

c     Descriptions:
c     Routine to read a complete RQMD event native file9 and
c     load GUEVGEN common blocks with full event information

c     MAP:
c     Called by: GUEVGEN

c     Author:   Charles F. Maguire
c     Created:  November 4, 2000

c     Global declarations:
#include "event.inc"
#include "evntcode.inc"
#include "guevgen.inc"

c     Local declarations
      integer  ievent
      save   ievent
      data ievent /0/

      real xvec(4)
      integer ityp
      integer npidec
      integer lund 
 
      integer iloop, jloop
      integer npart

c     Executable code
c     Set event code number
      event_code = rqmd
 
c     Read in new event from RQMD data file9
      if(ievent.eq.0)then
        open(unit=95, file='file9', status='old',
     +    access='sequential', form='formatted')
      endif ! first time initialization
 
c     set event number
      ievent = ievent + 1
      numevt = ievent

c     read RQMD event header information in file9
      read(95,1,err=91,end=89)  ! OUTPUT
1     format(a4)
      read(95,2,err=91,end=89)atarg
2     format(5x,i4)
      read(95,3,err=91,end=89)ztarg
3     format(6x,i4)
      read(95,2,err=91,end=89)aproj
      read(95,3,err=91,end=89)zproj
      read(95,1,err=91,end=89)  ! REF
      read(95,4,err=91,end=89)sqrt_s
4     format(8x,f10.4)
      read(95,11,err=95,end=93)bimevt
11    format(6x,f10.4)

      read(95,1,err=91,end=89)  ! IYY

      read(95,12,err=87,end=85)npart
12    format(i8)
      nptls = npart + atarg + aproj  
      if(nptls.gt.MAX_MXTOT)then
         stop ' rqmdfile9_evt - sees too many particles in event'
      else
        write(6,5)nptls, bimevt
5       format(' rqmdfile9_evt - particles = ', i6,
     +    ',  impact parameter = ', f10.4,' fm')
      endif

      read(95,1,err=91,end=89)

      do iloop = 1,nptls
        read(95,13,err=83,end=81)ityp, npidec, xvec,
     +    (pptot(jloop,iloop),jloop=1,4)
13      format(i2,i4,1x,f11.3,2f10.3,f11.3,f10.4,2f9.4,f11.4,f7.4)

c       GEANT conversion from Tim Miller's routinec
        call transl(-1,ityp,npidec,lund)
        call lund_to_geant(lund,idtot(iloop))
      enddo

c     =======================================================================
      mxtot = min (nptls, max_mxtot)
      if (mxtot .lt. nptls) then
        write (6,'(1h ,a,i7,a,a,i7)')
     &    ' rqmd_evt - keeping ', mxtot, ' particles',
     &    ' of true rqmd event ', numevt
        write (6,'(a,i7,a)')
     &    ' rqmd_evt - there were actually ',nptls,
     &    ' particles in the full event.'
      end if

C     this is some test code to enable me to put together
C     events run in dfferent blocks of runs -- I will use
C     this file to force the same vertex position in each
C     case -- JPS 24 Jan 96.
      
      write ( 47,501 ) ievent,xyz
  501 format (' rqmdfile9_evt - ievent,xyz vertex', i5,3f10.4)
 
      rqmdfile9_evt = -1
      chevt_name = 'rqmdfile9 source'

      return
      
99    continue
      write(6,98)ievent
98    format(' rqmdfile9_evt - end-of-file ' ,i6,'  events')
      stop ' eof'
97    continue
      stop ' error reading header'
95    continue
      stop ' error reading bimpact'
93    continue
      stop ' eof reading bimpact'
91    continue
      stop ' error reading iyy'
89    continue
      stop ' eof reading iyy'
87    continue
      stop ' error reading npart'
85    continue
      stop ' eof reading npart'
83    continue
      write(6,82)iloop
82    format('  at iloop = ', i5)
      stop ' err reading particle'
81    continue
      stop ' eof reading particle'
      end
