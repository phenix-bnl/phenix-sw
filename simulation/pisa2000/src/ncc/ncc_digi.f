C...  User Routine called at the end of each event
C...  
C...  Author: Vasily Dzhordzhadze 07.20.2004
C...  Author: Sky D. Rolnick 02.28.2007
C...  
      
      SUBROUTINE NCC_digi
      
      IMPLICIT NONE

#include "gconst.inc"
#include "gcflag.inc"
#include "gcvolu.inc"
#include "gcnum.inc"
#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fncclink.inc"
#include "mapfest.inc"

      integer nvdim
      integer nhmax
      integer nhddr
      PARAMETER ( NVDIM=2,NHMAX=3999,NHDDR= 6)

      INTEGER ITRAH(NHMAX), NUMVS(NVDIM), NUMVV(NVDIM, NHMAX)
      REAL  HITD(NHDDR,NHMAX)

      integer ioipd1
      integer ioipd2
      integer ioipd3
      integer ioipd4
      integer ic,ijk
      integer ipoint
      integer jpoint
      integer ih
      integer nhits
      integer ljfst
      integer ksub
      integer jj
 
      CHARACTER*20 CHFORM

      DATA LJFST /0/
      SAVE LJFST, IOIPD1, IOIPD2, IOIPD3, IOIPD4, KSUB
      character*4 NCCSil1/'PDSI'/
      character*4 NCCSil2/'STSI'/

c     begin execution
      CALL VZERO (NUMVS, NVDIM)
      if ( ljfst .eq. 0 ) then
         ljfst = 1
         KSUB = 1
         CHFORM = '1I / 4I 2F'
         CALL MZFORM( NCCSil1, CHFORM, IOIPD1 )
         CALL MZFORM( NCCSil2, CHFORM, IOIPD2 ) 

      endif                     !  end initialization
      
C...  Silicon  #1 

      CALL GFHITS ( 'NCC ', NCCSil1, NVDIM, NHDDR, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITD, NHITS )
!      print*,'NCC_dig.f: <I> North NCC1 nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'ncc_digi.f: <W> NCC nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC1(1,KSUB),  !return bank address
     $        LFNCC1(1,KSUB),  !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        NCCSil1,          !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC1*NHITS + 1, !# of data words
     $        IOIPD1,           !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC1(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system


            IQF( IPOINT + OFINCC1_TR ) = itrah(ih)
     
            call trkstack(itrah(ih))
 
            QF(ipoint + OFINCC1_IEVT)  = hitd(1,ih)
!            QF(ipoint + OFINCC1_VERT1) = hitd(2,ih)
!            QF(ipoint + OFINCC1_VERT2) = hitd(3,ih)
!            QF(ipoint + OFINCC1_VERT3) = hitd(4,ih)
!            QF(ipoint + OFINCC1_ETOT)  = hitd(5,ih)
!            QF(ipoint + OFINCC1_PVERT1) = hitd(6,ih)
!            QF(ipoint + OFINCC1_PVERT2) = hitd(7,ih)
!            QF(ipoint + OFINCC1_PVERT3) = hitd(8,ih)
            QF(ipoint + OFINCC1_INCC)  = hitd(2,ih)
            QF(ipoint + OFINCC1_TWRID) = hitd(3,ih)
            QF(ipoint + OFINCC1_SENID) = hitd(4,ih)
            QF(ipoint + OFINCC1_TOFIN) = hitd(5,ih)
            QF(ipoint + OFINCC1_DEDX1) = hitd(6,ih)

            IPOINT = IPOINT + MFI_NCC1
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC1(1,KSUB), !return bank address
     $        LFNCC1(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        NCCSil1,  !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC1 + 1,    !# of data words
     $        IOIPD1,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC1(1,KSUB) + 1
         JPOINT = IPOINT
        IQF(JPOINT) = 0
      end if



C...  Silicon #2

      CALL GFHITS ( 'NCC ', NCCSil2, NVDIM, NHDDR, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITD, NHITS )
!      print*,'NCC_dig.f: <I> North NCC2 nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'ncc_digi.f: <W> NCC nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC1(2,KSUB), !return bank address
     $        LFNCC1(2,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        NCCSil2, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD2,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC1(2,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitd(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitd(2,ih)
c            QF(ipoint + OFINCC2_TWRID) = hitd(3,ih)
c            QF(ipoint + OFINCC2_SENID) = hitd(4,ih)
c            QF(ipoint + OFINCC2_TOFIN) = hitd(5,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitd(6,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC1(2,KSUB), !return bank address
     $        LFNCC1(2,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        NCCSil2, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD2,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC1(2,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if




      RETURN 
      END

