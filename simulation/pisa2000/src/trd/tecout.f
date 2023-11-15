      SUBROUTINE TECOUT

*     USER ROUTINE CALLED AT THE END OF EACH EVENT for TECOUT output


c     Revision history

c     C. Maguire,  Feb. 15, 1998   Add global coordinates to output in order
c                                  to save time in DIO; also raise NHMAX again
c                                  to 10000

c     J. Mitchell, Jan. 9, 1998 -> NHMAX raised to 6000 (6 planes active)

c     C. Maguire December 15, 1995  Change NHMAX from 2000 to 3000
c                                   Old limit crossed for ~5% of HIJET events

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)


      IMPLICIT NONE

c     HIT VARIABLES

      INTEGER  NHMAX, NHDDR, NVDIM
      PARAMETER ( NHMAX=20000,  NHDDR= 13, NVDIM=4 )
      INTEGER  NUMVS( NVDIM), ITRAH(NHMAX)
      REAL HITD(NHDDR,NHMAX)
      INTEGER NUMBV(NVDIM,NHMAX)

c    Hit names from TECDET subroutine

c     DATA NAMEH5/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ',
c    >            'TOF1 ','PTID', 'TOF2', 'DELE',
c    >            'XG1', 'YG1', 'ZG1'/

 
#include "gcflag.inc"
#include "gcbank.inc"
#include "gcnum.inc"
#include "gconst.inc"
#include "gcvolu.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "fptlink.inc"
 
       REAL XM1(3), XM2(3)
 
      integer NMPLANE    ! NUMBER OF PLANES
      parameter (NMPLANE = 6)   ! NUMBER OF PLANES
       INTEGER LJFST, IP, NHITS, IPOINT, JPOINT,
     >    IH, JJ, IOITEC(NMPLANE)
 
      CHARACTER*20 CHFORM
 
 
      CHARACTER*4 NAMECH
      CHARACTER*4 XE(6) /'XEN1','XEN2','XEN3','XEN4','XEN5','XEN6'/
 
      DATA LJFST /0/
      SAVE LJFST, IOITEC

C     BEGIN EXECUTION

      IF( LJFST .EQ. 0 ) THEN
         LJFST = 1
         CHFORM = '1I / 1I 7F 1I 1F 1I 4F'
         DO IP= 1, NMPLANE    ! number of TEC planes
            if(iqf(lft_para + 4 + ip).gt.0)then
               CALL MZFORM ( XE(IP), CHFORM, IOITEC(IP) )
            endif  ! check on active plane
         END DO
      ENDIF

*  ........................................................


*     The loop on the number of planes

      DO 433 IP = 1, NMPLANE

c     Check on active plane

         if(iqf(lft_para + 4 + ip).eq.0)go to 433
         NAMECH = XE(IP)
         CALL VZERO ( NUMVS, NVDIM )
         CALL GFHITS ( 'TRD ', NAMECH, NVDIM, NHDDR, NHMAX, 0,
     >                 NUMVS, ITRAH, NUMBV, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> TRD (tecout.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if



c     need to pre-book the ZEBRA output in case of no hits (see simul20.mem)

         IF ( NHITS .GE. 1 )then
            CALL MZBOOK ( IXDIV_FE,
     >                    LFT_TEC(IP,1),
     >                    LFT_TEC(IP,1),
     >                    1,
     >                    XE(IP),
     >                    0,
     >                    0,
     >                    MFI_TEC*NHITS + 1,
     >                    IOITEC(IP),
     >                   -1 )
            IPOINT = LFT_TEC(IP,1) + 1
            JPOINT = IPOINT
            IQF(JPOINT) = NHITS

c      loop on the number of hits for the this plane

            DO 400 IH = 1, NHITS
               call trkstack(itrah(ih))   ! put track # in PISA stack

*      coordinate information in "local" system

               DO JJ = 1,3

c     The entrance/exit coordinates are in the local frame here

                  XM1( JJ) = HITD( JJ, IH )
                  XM2( JJ) = HITD( JJ+3, IH )
               END DO
               IQF( IPOINT + OFITEC_TR ) = itrah(ih)
               QF( IPOINT + OFITEC_X1 ) = XM1(1)
               QF( IPOINT + OFITEC_Y1 ) = XM1(2)
               QF( IPOINT + OFITEC_Z1 ) = XM1(3)
               QF( IPOINT + OFITEC_X2 ) = XM2(1)
               QF( IPOINT + OFITEC_Y2 ) = XM2(2)
               QF( IPOINT + OFITEC_Z2 ) = XM2(3)
               qf( ipoint + ofiTEC_f1) = hitd(7,ih)
               iqf( ipoint + ofiTEC_id) = ip   ! change to PLANE number
               qf( ipoint + ofiTEC_f2) = hitd(9,ih)   ! TOF
               iqf( ipoint + ofiTEC_ic) = numbv(2, ih)  ! PHI sector number
               qf( ipoint + ofiTEC_de) = hitd(10,ih)   ! energy loss
               qf( ipoint + ofiTEC_xg1) = hitd(11,ih)   ! x1 global (entrance)
               qf( ipoint + ofiTEC_yg1) = hitd(12,ih)   ! y1 global (entrance)
               qf( ipoint + ofiTEC_zg1) = hitd(13,ih)   ! z1 global (entrance)
               IPOINT = IPOINT + MFI_TEC
400         CONTINUE  ! loop on NHITS
         else

c     no hits case for this subsection

            CALL MZBOOK ( IXDIV_FE,
     >                    LFT_TEC(IP,1),
     >                    LFT_TEC(IP,1),
     >                    1,
     >                    XE(IP),
     >                    0,
     >                    0,
     >                    MFI_TEC + 1,
     >                    IOITEC(IP),
     >                   -1 )
            IPOINT = LFT_TEC(IP,1) + 1
            IQF( IPOINT) = 0
         endif   ! check on non-zero hits
433   CONTINUE   ! loop over TEC PLANES
      RETURN
      END
