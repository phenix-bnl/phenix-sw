      SUBROUTINE MuonPad_digi
*                                                        *
*     USER ROUTINE CALLED AT THE END OF EACH EVENT       *
      implicit none

#include "gconst.inc"
#include "gcflag.inc"
#include "gcvolu.inc"
#include "gcnum.inc"
#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fmupclink.inc"
#include "mapfest.inc"

      integer nvdim
      integer nhmax
      integer nhddr
      PARAMETER ( NVDIM=4,NHMAX=1999,NHDDR= 13)
 
      INTEGER ITRAH(NHMAX), NUMVS(NVDIM), NUMVV(NVDIM, NHMAX)
      REAL  HITD(NHDDR,NHMAX)
 
      INTEGER nmchmb
      CHARACTER*4 NAMECH
 
      real xd(3)
      real xd1(3)
      real xd2(3)
 
 
      integer ioipd1(2)
      integer ioipd2(2)
      integer ioipd3(2)
      integer ic
      integer ipoint
      integer jpoint
      integer ih
      integer nhits
      integer ljfst
      integer ksub
      integer jj
 
      CHARACTER*20 CHFORM

      DATA LJFST / 0/
      SAVE LJFST, IOIPD1, KSUB
   
      character*4 utrgPC1_gas(2)
      character*4 utrgPC2_gas(2)
      character*4 utrgPC3_gas(2)
      data utrgPC1_gas/'UG1N', 'UG1S'/          ! changed to uppercase
      data utrgPC2_gas/'UG2N', 'UG2S'/          ! jul 2013 HvH
      data utrgPC3_gas/'UG3N', 'UG3S'/
      integer igas

c     begin execution

      CALL VZERO (NUMVS, NVDIM)
      if ( ljfst .eq. 0 ) then
         write (6,*) 'Calling MuonPad_digi.f (for RPC1, RPC3)'
         ljfst = 1
         KSUB = 1
         CHFORM = '1I / 1I 7F 1I 5F'
         do igas = 1, 2
             CALL MZFORM( utrgPC1_gas(igas), CHFORM, IOIPD1(IC) )
             CALL MZFORM( utrgPC2_gas(igas), CHFORM, IOIPD2(IC) )
             CALL MZFORM( utrgPC3_gas(igas), CHFORM, IOIPD3(IC) )
         end do
      endif   !  end initialization


cc    deal with PC1 ....
      do 133 igas = 1, 2
          CALL GFHITS ( 'MUPC', utrgPC1_gas(igas), NVDIM, NHDDR, NHMAX, 
     *              0, NUMVS, ITRAH, NUMVV, HITD, NHITS )

          if (nhits .gt. nhmax) then
             print*, 'nhits goes beyond limit'
             nhits = nhmax
          end if

          if(nhits.GE.1) then
             call MZBOOK(ixdiv_fe,
     $               LFmupc1(igas,KSUB),      !return bank address
     $               LFmupc1(igas,KSUB),      !supporting link
     $               1,                     !JBIAS=1 ---> top level bank
     $               utrgPC1_gas(igas),      !bank name
     $               0,                     !# of links
     $               0,                     !# of down links
     $               MFI_mupc1*NHITS + 1,     !# of data words
     $               IOIPD1(igas),            !I/O characteristics
     $               -1)                    !do not clear memory contents
             IPOINT = LFmupc1(igas,KSUB) + 1
             JPOINT = IPOINT
             IQF(JPOINT) = NHITS

             DO IH = 1, NHITS

*              coordinate information in "local" system

               DO JJ = 1,3
                  XD1( JJ) = HITD( JJ  , IH )    ! entry point into volume
                  XD2( JJ) = HITD( JJ+3, IH )    ! exit point from volume
               ENDDO
               IQF( IPOINT + OFImupc1_TR ) = itrah(ih)

               call trkstack(itrah(ih))

               QF( IPOINT + OFImupc1_X1 ) = xd1(1)
               QF( IPOINT + OFImupc1_Y1 ) = xd1(2)
               QF( IPOINT + OFImupc1_Z1 ) = xd1(3)
               QF( IPOINT + OFImupc1_X2 ) = xd2(1)
               QF( IPOINT + OFImupc1_Y2 ) = xd2(2)
               QF( IPOINT + OFImupc1_Z2 ) = xd2(3)
               QF( ipoint + OFImupc1_TF) = hitd(7,ih)
               iqf( ipoint + OFImupc1_ID) = hitd(8,ih)
               qf( ipoint + OFImupc1_DE) = hitd(9,ih)
               qf( ipoint + OFImupc1_XG1) = hitd(10,ih)
               qf( ipoint + OFImupc1_YG1) = hitd(11,ih)
               qf( ipoint + OFImupc1_ZG1) = hitd(12,ih)
               qf( ipoint + OFImupc1_PL) = hitd(13,ih)
               IPOINT = IPOINT + MFI_mupc1
             enddo   !  loop on number of hits
           else
             call MZBOOK(ixdiv_fe,
     $               LFmupc1(igas,KSUB),      !return bank address
     $               LFmupc1(igas,KSUB),      !supporting link
     $               1,                     !JBIAS=1 ---> top level bank
     $               utrgPC1_gas(igas),      !bank name
     $               0,                     !# of links
     $               0,                     !# of down links
     $               MFI_mupc1 + 1,     !# of data words
     $               IOIPD1(igas),            !I/O characteristics
     $               -1)                    !do not clear memory contents
             IPOINT = LFmupc1(igas,KSUB) + 1
             JPOINT = IPOINT
             IQF(JPOINT) = 0
           end if
133     continue        ! end PC1 loop

cc    deal with PC2 ....
      do 134 igas = 1, 2
          CALL GFHITS ( 'MUPC', utrgPC2_gas(igas), NVDIM, NHDDR, NHMAX, 
     *              0, NUMVS, ITRAH, NUMVV, HITD, NHITS )

          if (nhits .gt. nhmax) then
             print*, 'nhits goes beyond limit'
             nhits = nhmax
          end if

          if(nhits.GE.1) then
             call MZBOOK(ixdiv_fe,
     $               LFmupc2(igas,KSUB),      !return bank address
     $               LFmupc2(igas,KSUB),      !supporting link
     $               1,                     !JBIAS=1 ---> top level bank
     $               utrgPC2_gas(igas),      !bank name
     $               0,                     !# of links
     $               0,                     !# of down links
     $               MFI_mupc2*NHITS + 1,     !# of data words
     $               IOIPD2(igas),            !I/O characteristics
     $               -1)                    !do not clear memory contents
             IPOINT = LFmupc2(igas,KSUB) + 1
             JPOINT = IPOINT
             IQF(JPOINT) = NHITS

             DO IH = 1, NHITS

*              coordinate information in "local" system

               DO JJ = 1,3
                  XD1( JJ) = HITD( JJ, IH )
                  XD2( JJ) = HITD( JJ+3, IH )
               ENDDO
               IQF( IPOINT + OFImupc2_TR ) = itrah(ih)

               call trkstack(itrah(ih))

               QF( IPOINT + OFImupc2_X1 ) = xd1(1)
               QF( IPOINT + OFImupc2_Y1 ) = xd1(2)
               QF( IPOINT + OFImupc2_Z1 ) = xd1(3)
               QF( IPOINT + OFImupc2_X2 ) = xd2(1)
               QF( IPOINT + OFImupc2_Y2 ) = xd2(2)
               QF( IPOINT + OFImupc2_Z2 ) = xd2(3)
               QF( ipoint + OFImupc2_TF) = hitd(7,ih)
               iqf( ipoint + OFImupc2_ID) = hitd(8,ih)
               qf( ipoint + OFImupc2_DE) = hitd(9,ih)
               qf( ipoint + OFImupc2_XG1) = hitd(10,ih)
               qf( ipoint + OFImupc2_YG1) = hitd(11,ih)
               qf( ipoint + OFImupc2_ZG1) = hitd(12,ih)
               qf( ipoint + OFImupc2_PL) = hitd(13,ih)
               IPOINT = IPOINT + MFI_mupc2
             enddo   !  loop on number of hits
           else
             call MZBOOK(ixdiv_fe,
     $               LFmupc2(igas,KSUB),      !return bank address
     $               LFmupc2(igas,KSUB),      !supporting link
     $               1,                     !JBIAS=1 ---> top level bank
     $               utrgPC2_gas(igas),      !bank name
     $               0,                     !# of links
     $               0,                     !# of down links
     $               MFI_mupc2 + 1,     !# of data words
     $               IOIPD2(igas),            !I/O characteristics
     $               -1)                    !do not clear memory contents
             IPOINT = LFmupc2(igas,KSUB) + 1
             JPOINT = IPOINT
             IQF(JPOINT) = 0
           end if
134     continue    ! end PC2 loop

cc    deal with PC3 ....
      do 135 igas = 1, 2
          CALL GFHITS ( 'MUPC', utrgPC3_gas(igas), NVDIM, NHDDR, NHMAX, 
     *              0, NUMVS, ITRAH, NUMVV, HITD, NHITS )

          if (nhits .gt. nhmax) then
             print*, 'nhits goes beyond limit'
             nhits = nhmax
          end if

          if(nhits.GE.1) then
             call MZBOOK(ixdiv_fe,
     $               LFmupc3(igas,KSUB),      !return bank address
     $               LFmupc3(igas,KSUB),      !supporting link
     $               1,                     !JBIAS=1 ---> top level bank
     $               utrgPC3_gas(igas),      !bank name
     $               0,                     !# of links
     $               0,                     !# of down links
     $               MFI_mupc3*NHITS + 1,     !# of data words
     $               IOIPD3(igas),            !I/O characteristics
     $               -1)                    !do not clear memory contents
             IPOINT = LFmupc3(igas,KSUB) + 1
             JPOINT = IPOINT
             IQF(JPOINT) = NHITS

             DO IH = 1, NHITS

*              coordinate information in "local" system

               DO JJ = 1,3
                  XD1( JJ) = HITD( JJ, IH )
                  XD2( JJ) = HITD( JJ+3, IH )
               ENDDO
               IQF( IPOINT + OFImupc3_TR ) = itrah(ih)

               call trkstack(itrah(ih))

               QF( IPOINT + OFImupc3_X1 ) = xd1(1)
               QF( IPOINT + OFImupc3_Y1 ) = xd1(2)
               QF( IPOINT + OFImupc3_Z1 ) = xd1(3)
               QF( IPOINT + OFImupc3_X2 ) = xd2(1)
               QF( IPOINT + OFImupc3_Y2 ) = xd2(2)
               QF( IPOINT + OFImupc3_Z2 ) = xd2(3)
               QF( ipoint + OFImupc3_TF) = hitd(7,ih)
               iqf( ipoint + OFImupc3_ID) = hitd(8,ih)
               qf( ipoint + OFImupc3_DE) = hitd(9,ih)
               qf( ipoint + OFImupc3_XG1) = hitd(10,ih)
               qf( ipoint + OFImupc3_YG1) = hitd(11,ih)
               qf( ipoint + OFImupc3_ZG1) = hitd(12,ih)
               qf( ipoint + OFImupc3_PL) = hitd(13,ih)
               IPOINT = IPOINT + MFI_mupc3
             enddo   !  loop on number of hits
           else
             call MZBOOK(ixdiv_fe,
     $               LFmupc3(igas,KSUB),      !return bank address
     $               LFmupc3(igas,KSUB),      !supporting link
     $               1,                     !JBIAS=1 ---> top level bank
     $               utrgPC3_gas(igas),      !bank name
     $               0,                     !# of links
     $               0,                     !# of down links
     $               MFI_mupc3 + 1,     !# of data words
     $               IOIPD3(igas),            !I/O characteristics
     $               -1)                    !do not clear memory contents
             IPOINT = LFmupc3(igas,KSUB) + 1
             JPOINT = IPOINT
             IQF(JPOINT) = 0
           end if
135     continue

       return 
       end
