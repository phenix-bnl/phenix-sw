C...  User Routine called at the end of each event
C...  
C...  Author: Vasily Dzhordzhadze 07.20.2004
C...  Author: Sky D. Rolnick 02.28.2007
C...  Modified for MXPS - JGL 3/28/2011
      
      SUBROUTINE MXPS_digi
      
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
      integer nhddrabs
      integer nhddrent
      PARAMETER ( NVDIM=3,NHMAX=10000,NHDDR=6,NHDDRABS=3,NHDDRENT=6)

      INTEGER ITRAH(NHMAX), NUMVS(NVDIM), NUMVV(NVDIM, NHMAX)
      REAL  HITD(NHDDR,NHMAX)
      REAL  HITDABS(NHDDRABS,NHMAX)
      REAL  HITDENT(NHDDRENT,NHMAX)

      integer ioipd1
      integer ioipd2
      integer ioipd3
      integer ioipd4
      integer ioipd5
      integer ioipd6
      integer ioipd7
      integer ioipd8
      integer ioipd9
      integer ioipd10
      integer ioipd11
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
      SAVE IOIPD5, IOIPD6, IOIPD7, IOIPD8, IOIPD9
      SAVE IOIPD10, IOIPD11
      character*4 NCCSil/'STSI'/
      character*4 MPCEXABS1/'W_DS'/
      character*4 MPCEXABS2/'SCDS'/
      character*4 MPCEXABS3/'SPSS'/
      character*4 MPCEXABS4/'INTS'/
      character*4 MPCEXABS5/'COVS'/

      character*4 MPCEXABS6/'FPLT'/
      character*4 MPCEXABS7/'BPLT'/
      character*4 MPCEXABS8/'SKIN'/
      character*4 MPCEXABS9/'PLAT'/
      character*4 MPCEXABS10/'W_DS'/

c     begin execution
      CALL VZERO (NUMVS, NVDIM)
      if ( ljfst .eq. 0 ) then
         ljfst = 1
         KSUB = 1
         CHFORM = '1I / 4I 2F'
         CALL MZFORM( NCCSil, CHFORM, IOIPD1 ) 
         CHFORM = '1I / 2I 1F'
         CALL MZFORM( MPCEXABS1, CHFORM, IOIPD2 ) 
         CALL MZFORM( MPCEXABS2, CHFORM, IOIPD3 ) 
         CALL MZFORM( MPCEXABS3, CHFORM, IOIPD4 ) 
         CALL MZFORM( MPCEXABS4, CHFORM, IOIPD5 ) 
         CALL MZFORM( MPCEXABS5, CHFORM, IOIPD6 ) 
         CALL MZFORM( MPCEXABS6, CHFORM, IOIPD7 ) 
         CALL MZFORM( MPCEXABS7, CHFORM, IOIPD8 ) 
         CALL MZFORM( MPCEXABS8, CHFORM, IOIPD9 ) 
         CALL MZFORM( MPCEXABS9, CHFORM, IOIPD10 ) 
         CHFORM = '1I / 6F 2I'
         CALL MZFORM( MPCEXABS10, CHFORM, IOIPD11 ) 
      endif                     !  end initialization
      
C...  Silicon Strips

      CALL GFHITS ( 'NCC ', NCCSil, NVDIM, NHDDR, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITD, NHITS )
c      print*,'MXPS_digi.f: <I> STSI nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> STSI nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC1(1,KSUB), !return bank address
     $        LFNCC1(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        NCCSil, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC1*NHITS + 1, !# of data words
     $        IOIPD1,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC1(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC1_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC1_IEVT)  = hitd(1,ih)
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
     $        NCCSil, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC1 + 1,    !# of data words
     $        IOIPD1,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC1(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C...  Absorber hits
C... W_DS
      CALL GFHITS ( 'EXAB', MPCEXABS1, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> W_DS nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> W_DS nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(1,KSUB), !return bank address
     $        LFNCC2(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS1, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD2,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(1,KSUB), !return bank address
     $        LFNCC2(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS1, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD2,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C... SCDS
      CALL GFHITS ( 'EXAB', MPCEXABS2, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> SCDS nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> SCDS nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(2,KSUB), !return bank address
     $        LFNCC2(2,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS2, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD3,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(2,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(2,KSUB), !return bank address
     $        LFNCC2(2,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS2, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD3,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(2,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C... SPSS
      CALL GFHITS ( 'EXAB', MPCEXABS3, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> SPSS nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> SPSS nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(3,KSUB), !return bank address
     $        LFNCC2(3,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS3, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD4,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(3,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(3,KSUB), !return bank address
     $        LFNCC2(3,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS3, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD4,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(3,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C... INTS
      CALL GFHITS ( 'EXAB', MPCEXABS4, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> INTS nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> INTS nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(4,KSUB), !return bank address
     $        LFNCC2(4,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS4, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD5,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(4,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(4,KSUB), !return bank address
     $        LFNCC2(4,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS4, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD5,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(4,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C... COVS
      CALL GFHITS ( 'EXAB', MPCEXABS5, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> COVS nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> COVS nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(5,KSUB), !return bank address
     $        LFNCC2(5,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS5, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD6,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(5,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(5,KSUB), !return bank address
     $        LFNCC2(5,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS5, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD6,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(5,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C... FPLT
      CALL GFHITS ( 'MPAL', MPCEXABS6, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> FPLT nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> FPLT nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(6,KSUB), !return bank address
     $        LFNCC2(6,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS6, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD7,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(6,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(6,KSUB), !return bank address
     $        LFNCC2(6,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS6, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD7,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(6,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C... BPLT
      CALL GFHITS ( 'MPAL', MPCEXABS7, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> BPLT nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> BPLT nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(7,KSUB), !return bank address
     $        LFNCC2(7,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS7, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD8,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(7,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(7,KSUB), !return bank address
     $        LFNCC2(7,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS7, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD8,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(7,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C... SKIN
      CALL GFHITS ( 'MPAL', MPCEXABS8, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> SKIN nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> SKIN nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(8,KSUB), !return bank address
     $        LFNCC2(8,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS8, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD9,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(8,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(8,KSUB), !return bank address
     $        LFNCC2(8,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS8, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD9,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(8,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C... PLAT
      CALL GFHITS ( 'MPAL', MPCEXABS9, NVDIM, NHDDRABS, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDABS, NHITS )
C--      print*,'MXPS_digi.f: <I> SKIN nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> SKIN nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(9,KSUB), !return bank address
     $        LFNCC2(9,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS9, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2*NHITS + 1, !# of data words
     $        IOIPD10,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(9,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC2_IEVT)  = hitdabs(1,ih)
            QF(ipoint + OFINCC2_INCC)  = hitdabs(2,ih)
            QF(ipoint + OFINCC2_DEDX1) = hitdabs(3,ih)

            IPOINT = IPOINT + MFI_NCC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC2(9,KSUB), !return bank address
     $        LFNCC2(9,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS9, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC2 + 1,    !# of data words
     $        IOIPD10,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC2(9,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C...  Entry Particles

      CALL GFHITS ( 'EXNT', MPCEXABS10, NVDIM, NHDDRENT, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITDENT, NHITS )
      if (nhits .gt. nhmax) then
         print*, 'mxps_digi.f: <W> EXNT W_DS nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFNCC3(1,KSUB), !return bank address
     $        LFNCC3(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS10, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC3*NHITS + 1, !# of data words
     $        IOIPD11,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC3(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         
         DO IH = 1, NHITS

*     coordinate information in "local" system

            IQF( IPOINT + OFINCC3_TR ) = itrah(ih)

            call trkstack(itrah(ih))

            QF(ipoint + OFINCC3_VX)  = hitdent(1,ih)
            QF(ipoint + OFINCC3_VY)  = hitdent(2,ih)
            QF(ipoint + OFINCC3_VZ)  = hitdent(3,ih)
            QF(ipoint + OFINCC3_PX)  = hitdent(4,ih)
            QF(ipoint + OFINCC3_PY)  = hitdent(5,ih)
            QF(ipoint + OFINCC3_PZ)  = hitdent(6,ih)

            IPOINT = IPOINT + MFI_NCC3
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFNCC3(1,KSUB), !return bank address
     $        LFNCC3(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCEXABS10, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_NCC3 + 1,    !# of data words
     $        IOIPD11,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFNCC3(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if


      RETURN 
      END

