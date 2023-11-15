c     $Id: nexusascii_evt.f,v 1.3 2008/05/21 08:22:11 hpereira Exp $
      integer function nexusascii_evt()
      implicit none

c     Descriptions:
c     Routine to read a complete NEXUS event ASCII format and
c     load GUEVGEN common blocks with full event information

c     MAP:
c     Called by: GUEVGEN

c     Author:   Charles F. Maguire
c     Created:  January 5, 2001

c     Global declarations:
#include "event.inc"
#include "evntcode.inc"
#include "guevgen.inc"

c     External declarations
      integer isa_to_geant

c     Local declarations
      integer  ievent
      save   ievent
      data ievent /0/

      real xvec(4)
      integer ityp
      integer partnexus
      integer evtnexus
 
      integer iloop, jloop

c     Executable code
c     Set event code number
      event_code = nexus
 
c     Read in new event from NEXUS data file9
      if(ievent.eq.0)then
        open(unit=95, file='nexusascii.txt', status='old',
     +    access='sequential', form='formatted', err=95)
        read(95,1,err=91,end=89)chevt_name ! title
 1      format(a40)
        write(6,2)chevt_name
 2      format(' nexusascii_evt - <I>',/,1x,a40,/)
      endif 
 
c     set event number
      ievent = ievent + 1
      numevt = ievent

c     read NEXUS event header information (hardwire Au + Au for now)
      zproj = 79
      aproj = 197
      ztarg = 79
      atarg = 197

c     Above hardwire information shoud be replaced by information from event file
c     Read in event header information for number of particles,
c     impact parameters, and energy  (event.inc)
c     (evtnexus number is ignored)

      read(95,4,err=83,end=85)evtnexus,nptls,bimevt,sqrt_s
4     format(i6,i7,2e13.6)

c     check on number of particles not exceeding maximum MAX_MXTOT (guevgen.inc)
      if(nptls.gt.max_mxtot)then
         stop ' nexusascii_evt - sees too many particles in event'
      else
        write(6,5)nptls, bimevt
5       format(' nexusascii - particles = ', i6,
     +    ',  impact parameter = ', f10.4,' fm',/)
      endif


c     Store 4 momentum in the pptot array (guevgen.inc)
c     The space point xvec and the NEXUS particle number are ignored

      do iloop = 1,nptls
        
        !  This is the GEANT order (et,px,py,pz)
        read(95,13,err=83,end=81)partnexus,ityp, xvec,
     +    (pptot(jloop,iloop),jloop=2,4),
     +    pptot(1,iloop)
13      format(i6,2x,i9,8e13.6)

c       GEANT conversion for ID number
        idtot(iloop) = isa_to_geant(ityp)
        
      enddo  !  transferring particle information to GEANT arrays

c     =======================================================================
      mxtot = min (nptls, max_mxtot)
      if (mxtot .lt. nptls) then
          write (6,'(1h ,a,i7,a,a,i7)')
     &        ' nexus_evt - keeping ', mxtot, ' particles',
     &        ' of true nexus event ', numevt
          write (6,'(a,i7,a)')
     &        ' nexus_evt - there were actually ',nptls,
     %        ' particles in the full event.'
      end if

C     this is some test code to enable me to put together
C     events run in dfferent blocks of runs -- I will use
C     this file to force the same vertex position in each
C     case -- JPS 24 Jan 96.

      write( 47,501 ) ievent,xyz
 501  format( 'nexusascii_evt - ievent,xyz vertex', i5,3f10.4)
      nexusascii_evt = -1  ! this is the correct return flag

      return
95    continue
      stop 'nexusascii_evt - unable to open nexusascii.txt file'
91    continue
      stop 'nexusascii_evt - error in reading title'
89    continue
      stop 'nexusascii_evt - eof in reading title'
87    continue
      stop 'nexusascii_evt - error reading event header'
85    continue
      stop 'nexusascii_evt - eof reading event header'
83    continue
      write(6,82)iloop
82    format('nexusascii_evt - at iloop = ', i5)
      stop 'nexusascii_evt - err reading particle'
81    continue
      stop 'nexusascii_evt - eof reading particle'
      end
