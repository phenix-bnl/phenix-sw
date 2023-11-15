
C...  User Routine called at the end of each event
C...  
C...  Author: L. A. Linden Levy and M. C. McCain 17.02.2004
C...  
      
      SUBROUTINE RLT_digi
      
      IMPLICIT NONE

#include "gconst.inc"
#include "gcflag.inc"
#include "gcvolu.inc"
#include "gcnum.inc"
#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "frltlink.inc"
#include "mapfest.inc"

      integer nvdim
      integer nhmax
      integer nhddr
      PARAMETER ( NVDIM=4,NHMAX=1999,NHDDR= 13)

      INTEGER ITRAH(NHMAX), NUMVS(NVDIM), NUMVV(NVDIM, NHMAX)
      REAL  HITD(NHDDR,NHMAX)

      real xd(3)
      real xd1(3)
      real xd2(3) 
 
      integer ioipd1
      integer ioipd2
      integer ioipd3
      integer ic
      integer ipoint
      integer jpoint
      integer ih
      integer nhits
      integer ljfst
      integer ksub
      integer jj
 
      CHARACTER*20 CHFORM

      DATA LJFST /0/
      SAVE LJFST, IOIPD1, IOIPD2, IOIPD3, KSUB
   
      character*4 rltRPC1_gas/'rtg1'/
      character*4 rltRPC2_gas/'rtg2'/
      character*4 rltRPC3_gas/'rtg3'/

c     begin execution
      CALL VZERO (NUMVS, NVDIM)
      if ( ljfst .eq. 0 ) then
         ljfst = 1
         KSUB = 1
         CHFORM = '1I / 1I 7F 1I 5F'
         CALL MZFORM( rltRPC1_gas, CHFORM, IOIPD1 )
         CALL MZFORM( rltRPC2_gas, CHFORM, IOIPD2 )
         CALL MZFORM( rltRPC3_gas, CHFORM, IOIPD3 )
      endif                     !  end initialization



      
C...  RPC #1

      CALL GFHITS ( 'RLT ', rltRPC1_gas, NVDIM, NHDDR, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITD, NHITS )
*      print*,'rlt_dig.f: <I> RPC1 nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFrltRPC1(1,KSUB), !return bank address
     $        LFrltRPC1(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        rltRPC1_gas, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_rltRPC1*NHITS + 1, !# of data words
     $        IOIPD1,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFrltRPC1(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         DO IH = 1, NHITS

*     coordinate information in "local" system

            DO JJ = 1,3
               XD1( JJ) = HITD( JJ, IH )
               XD2( JJ) = HITD( JJ+3, IH )
            ENDDO
            IQF( IPOINT + OFIrltRPC1_TR ) = itrah(ih)

            call trkstack(itrah(ih))
            
            QF( IPOINT + OFIrltRPC1_X1 ) = xd1(1)
            QF( IPOINT + OFIrltRPC1_Y1 ) = xd1(2)
            QF( IPOINT + OFIrltRPC1_Z1 ) = xd1(3)
            QF( IPOINT + OFIrltRPC1_X2 ) = xd2(1)
            QF( IPOINT + OFIrltRPC1_Y2 ) = xd2(2)
            QF( IPOINT + OFIrltRPC1_Z2 ) = xd2(3)
            QF( ipoint + OFIrltRPC1_TF) = hitd(7,ih)
            iqf( ipoint + OFIrltRPC1_ID) = hitd(8,ih)
            qf( ipoint + OFIrltRPC1_DE) = hitd(9,ih)
            qf( ipoint + OFIrltRPC1_XG1) = hitd(10,ih)
            qf( ipoint + OFIrltRPC1_YG1) = hitd(11,ih)
            qf( ipoint + OFIrltRPC1_ZG1) = hitd(12,ih)
            qf( ipoint + OFIrltRPC1_PL) = hitd(13,ih)
            IPOINT = IPOINT + MFI_rltRPC1
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFrltRPC1(1,KSUB), !return bank address
     $        LFrltRPC1(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        rltRPC1_gas, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_rltRPC1 + 1,    !# of data words
     $        IOIPD1,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFrltRPC1(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C...  RPC #2

      CALL GFHITS ( 'RLT ', rltRPC2_gas, NVDIM, NHDDR, NHMAX, 
     *     0, NUMVS, ITRAH, NUMVV, HITD, NHITS )
*      print*,'rlt_dig.f: <I> RPC2 nhits=', nhits
      if (nhits .gt. nhmax) then
         print*, 'nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFrltRPC2(1,KSUB), !return bank address
     $        LFrltRPC2(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        rltRPC2_gas, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_rltRPC2*NHITS + 1, !# of data words
     $        IOIPD2,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFrltRPC2(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         
         DO IH = 1, NHITS

*     coordinate information in "local" system

            DO JJ = 1,3
               XD1( JJ) = HITD( JJ, IH )
               XD2( JJ) = HITD( JJ+3, IH )
            ENDDO
            IQF( IPOINT + OFIrltRPC2_TR ) = itrah(ih)

            call trkstack(itrah(ih))
            
            QF( IPOINT + OFIrltRPC2_X1 ) = xd1(1)
            QF( IPOINT + OFIrltRPC2_Y1 ) = xd1(2)
            QF( IPOINT + OFIrltRPC2_Z1 ) = xd1(3)
            QF( IPOINT + OFIrltRPC2_X2 ) = xd2(1)
            QF( IPOINT + OFIrltRPC2_Y2 ) = xd2(2)
            QF( IPOINT + OFIrltRPC2_Z2 ) = xd2(3)
            QF( ipoint + OFIrltRPC2_TF) = hitd(7,ih)
            iqf( ipoint + OFIrltRPC2_ID) = hitd(8,ih)
            qf( ipoint + OFIrltRPC2_DE) = hitd(9,ih)
            qf( ipoint + OFIrltRPC2_XG1) = hitd(10,ih)
            qf( ipoint + OFIrltRPC2_YG1) = hitd(11,ih)
            qf( ipoint + OFIrltRPC2_ZG1) = hitd(12,ih)
            qf( ipoint + OFIrltRPC2_PL) = hitd(13,ih)
            IPOINT = IPOINT + MFI_rltRPC2
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFrltRPC2(1,KSUB), !return bank address
     $        LFrltRPC2(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        rltRPC2_gas, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_rltRPC2 + 1,    !# of data words
     $        IOIPD2,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFrltRPC2(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

C...  RPC #3

      CALL GFHITS ( 'RLT ', rltRPC3_gas, NVDIM, NHDDR, NHMAX, 
     *     0, NUMVS, ITRAH, NUMVV, HITD, NHITS )
*      print*,'rlt_dig.f: <I> RPC3 nhits=', nhits

      if (nhits .gt. nhmax) then
         print*, 'nhits goes beyond limit'
         nhits = nhmax
      end if

      if(nhits.GE.1) then
         call MZBOOK(ixdiv_fe,
     $        LFrltRPC3(1,KSUB), !return bank address
     $        LFrltRPC3(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        rltRPC3_gas, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_rltRPC3*NHITS + 1, !# of data words
     $        IOIPD3,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFrltRPC3(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = NHITS
         
         DO IH = 1, NHITS

*     coordinate information in "local" system

            DO JJ = 1,3
               XD1( JJ) = HITD( JJ, IH )
               XD2( JJ) = HITD( JJ+3, IH )
            ENDDO
            IQF( IPOINT + OFIrltRPC3_TR ) = itrah(ih)

            call trkstack(itrah(ih))
            
            QF( IPOINT + OFIrltRPC3_X1 ) = xd1(1)
            QF( IPOINT + OFIrltRPC3_Y1 ) = xd1(2)
            QF( IPOINT + OFIrltRPC3_Z1 ) = xd1(3)
            QF( IPOINT + OFIrltRPC3_X2 ) = xd2(1)
            QF( IPOINT + OFIrltRPC3_Y2 ) = xd2(2)
            QF( IPOINT + OFIrltRPC3_Z2 ) = xd2(3)
            QF( ipoint + OFIrltRPC3_TF) = hitd(7,ih)
            iqf( ipoint + OFIrltRPC3_ID) = hitd(8,ih)
            qf( ipoint + OFIrltRPC3_DE) = hitd(9,ih)
            qf( ipoint + OFIrltRPC3_XG1) = hitd(10,ih)
            qf( ipoint + OFIrltRPC3_YG1) = hitd(11,ih)
            qf( ipoint + OFIrltRPC3_ZG1) = hitd(12,ih)
            qf( ipoint + OFIrltRPC3_PL) = hitd(13,ih)
            IPOINT = IPOINT + MFI_rltRPC3
         enddo                  !  loop on number of hits
      else
         call MZBOOK(ixdiv_fe,
     $        LFrltRPC3(1,KSUB), !return bank address
     $        LFrltRPC3(1,KSUB), !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        rltRPC3_gas, !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_rltRPC3 + 1,    !# of data words
     $        IOIPD3,     !I/O characteristics
     $        -1)               !do not clear memory contents
         IPOINT = LFrltRPC3(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

      RETURN 
      END
