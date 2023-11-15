      SUBROUTINE PC1OUT
**********************************************************
*                                                        *
*     USER ROUTINE CALLED AT THE END OF EACH EVENT       *
*  ( FROM itr_digi ) to prepare and fill PD1 DIGI bank   *
**********************************************************
*  Revised by K. Filimonov on September 6th, 1995.
*  Modified for updated geometry.


c     Revision History
c    Name           Date            Comment
c    C.F. Maguire   Feb. 15, 1998   Add global output and path length

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)



          implicit none

#include "gconst.inc"
#include "gcflag.inc"
#include "gcvolu.inc"
#include "gcnum.inc"
#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpilink.inc"

c     Special variables for field map trajectory NTUPLE

      integer itrkpc1
      integer mtrkpc1
      integer ndetpc1
#include "mapfest.inc"

      integer nvdim
      integer nhmax
      integer nhddr
      PARAMETER ( NVDIM=4,NHMAX=1999,NHDDR= 13)
 
      INTEGER ITRAH(NHMAX), NUMVS(NVDIM), NUMVV(NVDIM, NHMAX)
      REAL  HITD(NHDDR,NHMAX)

c    Detector parameter names as in PD1DET


c     DATA NAMEH1/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ',
c    +     'TOF ', 'PTID', 'DELE' , 'XG1', 'YG1', 'ZG1', 'PL'/

 
      INTEGER nmchmb
      CHARACTER*4 NAMECH
      CHARACTER*4 NMPAD1(16),  NMGAS(16)
 
      real xd(3)
      real xd1(3)
      real xd2(3)
 
 
      integer ioipd1(18)
      integer ic
      integer ipoint
      integer jpoint
      integer ih
      integer nhits
      integer ljfst
      integer ksub
      integer jj
 
      CHARACTER*20 CHFORM

      DATA NMPAD1 /'P101', 'P102', 'P103', 'P104', 'P105', 'P106',
     +'P107', 'P108', 'P109', 'P110', 'P111', 'P112', 'P113', 'P114',
     +'P115', 'P116' /

      DATA NMGAS /'ZZ01', 'ZZ02', 'ZZ03', 'ZZ04', 'ZZ05', 'ZZ06',
     +'ZZ07', 'ZZ08', 'ZZ09', 'ZZ10', 'ZZ11', 'ZZ12', 'ZZ13', 'ZZ14',
     +'ZZ15', 'ZZ16' /

      DATA LJFST / 0/
      SAVE LJFST, IOIPD1, KSUB
 

c     begin execution

      CALL VZERO ( NUMVS, NVDIM)
      nmchmb = iqf( lfi_para + 6)  ! number of PC1 sectors
 
      if ( ljfst .eq. 0 ) then
         ljfst = 1
         ksub = 1
         CHFORM = '1I / 1I 7F 1I 5F'
         DO IC = 1, NMCHMB
            CALL MZFORM( NMGAS(IC), CHFORM, IOIPD1(IC) )
         END DO
      endif   !  end initialization
*  .......................................................

*        FIRST PAD DETECTOR

      npc1trks = 0

c      pre-book all the ZEBRA banks even if some of these will have no hits
c      "runaway" writing possible if not pre-booked (see the simul20.mem)

      do ic = 1,nmchmb
         NAMECH = NMGAS(IC)
         CALL GFHITS ( 'ITR ', NAMECH, NVDIM, NHDDR, NHMAX, 0,
     >                  NUMVS, ITRAH, NUMVV, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> ITR (pc1out.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if

         if(nhits.le.0)nhits = 1
C  book & fill a zebra bank for the data here.
         call MZBOOK(ixdiv_fe,
     $               LFI_PAD(IC,KSUB),      !return bank address
     $               LFI_PAD(IC,KSUB),      !supporting link
     $               1,                     !JBIAS=1 ---> top level bank
     $               NMGAS(IC),             !bank name
     $               0,                     !# of links
     $               0,                     !# of down links
     $               MFI_PD1*NHITS + 1,     !# of data words
     $               IOIPD1(IC),            !I/O characteristics
     $               -1)                    !do not clear memory contents
         IPOINT = LFI_PAD(IC,KSUB) + 1
         JPOINT = IPOINT

C   Initialize with 0 hits to start, will fix below

         IQF(JPOINT) = 0
      enddo

*     The loop on the number of chambers

      DO 133 IC = 1, NMCHMB
         NAMECH = NMGAS(IC)
         CALL GFHITS ( 'ITR ', NAMECH, NVDIM, NHDDR, NHMAX, 0,
     >                 NUMVS, ITRAH, NUMVV, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> ITR (pc1out.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if

         IF ( NHITS .LE. 0 ) GO TO 133
         IPOINT = LFI_PAD(IC,KSUB) + 1
         JPOINT = IPOINT    ! no need to have a separate jpoint variable

C   Initialize with # of hits to start

         IQF(JPOINT) = nhits

*        loop on the number of hits for the chamber fixed at IC

         DO IH = 1, NHITS

*      coordinate information in "local" system

            DO JJ = 1,3
               XD1( JJ) = HITD( JJ, IH )
               XD2( JJ) = HITD( JJ+3, IH )
            ENDDO
            IQF( IPOINT + OFIPD1_TR ) = itrah(ih)

c     Call to put the track on the FKIN data structure stack

            call trkstack(itrah(ih))

c     Call for the trajectory NTUPLE if the MAPF switch is set

            if(cvolu_opt(3,4).eq.'MAPF')then
               itrkpc1 = itrah(ih)
               if(itrkpc1.gt.0)then
                  ndetpc1 = 1
                  call trkxyz(itrkpc1,ndetpc1)
               endif
               if(npc1trks.gt.0)then

c     search over previous tracks

                  do mtrkpc1 = 1,npc1trks
                     if(itrkpc1.eq.kpc1trks(mtrkpc1))then
                        go to 111
                     endif
                  enddo

c     PC1 track not found, so put in the array

                  npc1trks = npc1trks + 1
                  if(npc1trks.le.maxpc1trks)then
                     kpc1trks(npc1trks) = itrkpc1
                  else
                     stop 'PC1 has too many tracks?'
                  endif
               else

c     first PC1 track

                  npc1trks = 1
                  kpc1trks(1) = itrkpc1
               endif
            endif ! check if MAPF option was set
 111        continue
            QF( IPOINT + OFIPD1_X1 ) = xd1(1)
            QF( IPOINT + OFIPD1_Y1 ) = xd1(2)
            QF( IPOINT + OFIPD1_Z1 ) = xd1(3)
            QF( IPOINT + OFIPD1_X2 ) = xd2(1)
            QF( IPOINT + OFIPD1_Y2 ) = xd2(2)
            QF( IPOINT + OFIPD1_Z2 ) = xd2(3)
            qf( ipoint + ofipd1_tf) = hitd(7,ih)
            iqf( ipoint + ofipd1_id) = hitd(8,ih)
            qf( ipoint + ofipd1_de) = hitd(9,ih)
            qf( ipoint + ofipd1_xg1) = hitd(10,ih)
            qf( ipoint + ofipd1_yg1) = hitd(11,ih)
            qf( ipoint + ofipd1_zg1) = hitd(12,ih)
            qf( ipoint + ofipd1_pl) = hitd(13,ih)
            IPOINT = IPOINT + MFI_PD1
         enddo   !  loop on number of hits
  133 CONTINUE   !  loop on number of chambers (also branch point nhits = 0)
      RETURN
      END
