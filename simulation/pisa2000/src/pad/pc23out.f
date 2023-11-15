      SUBROUTINE PC23OUT

*     USER ROUTINE CALLED AT THE END OF EACH EVENT for PAD2/PAD3 output



c     Revision History
c    Name           Date            Comment
c    N. Smirnov     Jul. 19, 1994   New version

c    K. Filimonov   Sep.  6, 1995   For updated geometry of PC2/PC3.

c    C.F. Maguire   Feb. 11, 1997   Additions for TRKXYZ (field mapping NTUPLE)

c    C.F. Maguire   Feb. 15, 1998   Add global output and path length

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)



      IMPLICIT NONE
      INTEGER  NHMAX, NHDDR, NWDIM
      PARAMETER ( NHMAX=1999,  NHDDR= 13, NWDIM=4 )
      INTEGER  NUMWS( NWDIM), ITRAH(NHMAX)
      REAL HITD(NHDDR,NHMAX)
      INTEGER NUMVW(NWDIM,NHMAX), NAMEPD(NWDIM), NMPD(NWDIM)

c    Hit names for PC23DET subroutine

c     DATA NAMEH5/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ',
c    +            'TOF ','PTID', 'DELE', 'XGL1', 'YGL1', 'ZGL1', 'PTHL'/

 
#include "gcflag.inc"
#include "gcbank.inc"
#include "gcnum.inc"
#include "gconst.inc"
#include "gcvolu.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "fpplink.inc"

c     Variables for the call to the trkxyz routine for trajectory NTUPLE

      integer  mpc1trk
      integer  itrkpc3
      integer  npc3
#include "mapfest.inc"
 
       REAL XD(3), XD1(3), XD2(3)
       INTEGER LJFST, IC, NHITS, IPOINT, JPOINT, JK,
     >    IH, ITRC, JJ, IOIPD2(8), IOIPD3(8)
      CHARACTER*20 CHFORM
      CHARACTER*4 NAMECH
      CHARACTER*4 VPC2SHELL(8), VPC3SHELL(8) 
      CHARACTER*4 VPC2GASGP(8), VPC3GASGP(8)
      CHARACTER*4 MSPD(4), MTPD(4)
      integer nmchm    ! number of chamber subsections (fixed at 8)
      parameter (nmchm = 8)   ! change for UCDR version

      DATA VPC2SHELL /'PD21', 'PD22', 'PD23', 'PD24', 'PD25', 'PD26',
     +'PD27', 'PD28' /
      DATA VPC2GASGP /'AR21', 'AR22', 'AR23', 'AR24', 'AR25', 'AR26',
     +  'AR27', 'AR28' /
      DATA VPC3SHELL /'PD31', 'PD32', 'PD33', 'PD34', 'PD35', 'PD36',
     +'PD37', 'PD38' /
      DATA VPC3GASGP /'AR31', 'AR32', 'AR33', 'AR34', 'AR35', 'AR36',
     +'AR37', 'AR38' / 

      DATA MSPD /'HALL', 'EMCL', '    ', '    '/
      DATA MTPD /'HALL', 'EMCL', '    ', '    '/
      DATA LJFST /0/
      SAVE LJFST, IOIPD2, IOIPD3

      IF( LJFST .EQ. 0 ) THEN
         LJFST = 1
         CHFORM = '1I / 1I 7F 1I 5F'
         DO IC= 1, NMCHM    ! should have 8 chambers only
            CALL MZFORM ( VPC2GASGP(IC), CHFORM, IOIPD2(IC) )
            CALL MZFORM ( VPC3GASGP(IC), CHFORM, IOIPD3(IC) )
         END DO
      ENDIF

*  ........................................................

      CALL VZERO ( NUMWS, NWDIM )

*     The loop on the number of chambers

      DO 433 IC = 1, NMCHM
         MSPD(3) = VPC2SHELL(IC)
         MSPD(4) = VPC2GASGP(IC)
         NAMECH= VPC2GASGP(IC)
          CALL GFHITS ( 'PAD ', NAMECH, NWDIM, NHDDR, NHMAX, 0,
     >                 NUMWS, ITRAH, NUMVW, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> PAD (pc23out.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if

c     need to pre-book the ZEBRA output in case of no hits (see simul20.mem)

         IF ( NHITS .GE. 1 )then
            CALL MZBOOK ( IXDIV_FE,
     >                    LFP_PD2(IC,1),
     >                    LFP_PD2(IC,1),
     >                    1,
     >                    VPC2GASGP(IC),
     >                    0,
     >                    0,
     >                    MFI_PD2*NHITS + 1,
     >                    IOIPD2(IC),
     >                   -1 )
            IPOINT = LFP_PD2(IC,1) + 1
            JPOINT = IPOINT
            IQF( JPOINT) = NHITS

*      loop on the number of hits for the chamber sector

            DO 400 IH = 1, NHITS

*      coordinate information in "local" system

               DO JJ = 1,3
                  XD1( JJ) = HITD( JJ, IH )
                  XD2( JJ) = HITD( JJ+3, IH )
               END DO
               IQF( IPOINT + OFIPD2_TR ) = itrah(ih)
               call trkstack(itrah(ih))  ! put track # in PISA track stacker
               QF( IPOINT + OFIPD2_X1 ) = xd1(1)
               QF( IPOINT + OFIPD2_Y1 ) = xd1(2)
               QF( IPOINT + OFIPD2_Z1 ) = xd1(3)
               QF( IPOINT + OFIPD2_X2 ) = xd2(1)
               QF( IPOINT + OFIPD2_Y2 ) = xd2(2)
               QF( IPOINT + OFIPD2_Z2 ) = xd2(3)
               qf( ipoint + ofipd2_tf) = hitd(7,ih)
               iqf( ipoint + ofipd2_id) = hitd(8,ih)
               qf( ipoint + ofipd2_de) = hitd(9,ih)
               qf( ipoint + ofipd2_xg1) = hitd(10,ih)
               qf( ipoint + ofipd2_yg1) = hitd(11,ih)
               qf( ipoint + ofipd2_zg1) = hitd(12,ih)
               qf( ipoint + ofipd2_pl) = hitd(13,ih)
               IPOINT = IPOINT + MFI_PD2

*    the end loop on number of hits

400         CONTINUE
         else

c     no hits case for this subsection

            CALL MZBOOK ( IXDIV_FE,
     >                    LFP_PD2(IC,1),
     >                    LFP_PD2(IC,1),
     >                    1,
     >                    VPC2GASGP(IC),
     >                    0,
     >                    0,
     >                    MFI_PD2 + 1,
     >                    IOIPD2(IC),
     >                   -1 )
            IPOINT = LFP_PD2(IC,1) + 1
            IQF( IPOINT) = 0
         endif   ! check on non-zero hits
433   CONTINUE   ! loop over chamber subsections  PAD2

*    The loop on the number of chambers in PAD 3

      DO 633 IC = 1, NMCHM
         MTPD(3) = VPC3SHELL(IC)
         MTPD(4) = VPC3GASGP(IC)
         NAMECH= VPC3GASGP(IC)
         CALL GFHITS ( 'PAD ', NAMECH, NWDIM, NHDDR, NHMAX, 0,
     >                NUMWS, ITRAH, NUMVW, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> PAD (pc23out.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if

         IF ( NHITS .GE. 1 )then
            CALL MZBOOK ( IXDIV_FE,
     >                    LFP_PD3(IC,1),
     >                    LFP_PD3(IC,1),
     >                    1,
     >                    VPC3GASGP(IC),
     >                    0,
     >                    0,
     >                    MFI_PD3*NHITS + 1,
     >                    IOIPD3(IC),
     >                    -1 )
            IPOINT = LFP_PD3(IC,1) + 1
            JPOINT = IPOINT
            IQF( JPOINT) = NHITS

*      loop on the number of hits for the chamber sector

            DO 600 IH = 1, NHITS

*      coordinate information in "local" system

               DO JJ = 1,3
                  XD1( JJ) = HITD( JJ, IH )
                  XD2( JJ) = HITD( JJ+3, IH )
               END DO
               IQF( IPOINT + OFIPD3_TR ) = itrah(ih)
               call trkstack(itrah(ih))  ! put track # in PISA track stacker

c    Check if want to store the track in the trajectory NTUPLE

               if(cvolu_opt(3,9).eq.'MAPF')then
                  itrkpc3 = itrah(ih)
                  npc3 = 3

c     search if this PC3 track was also in PC1

                  if(npc1trks.gt.0)then
                     do mpc1trk = 1,npc1trks
                        if(itrkpc3.eq.kpc1trks(mpc1trk))then
                           npc3 = -3
                           go to 111
                        endif
                     enddo
                  endif
 111              continue
                  call trkxyz(itrkpc3,npc3)   !  call from PC3
               endif
               QF( IPOINT + OFIPD3_X1 ) = xd1(1)
               QF( IPOINT + OFIPD3_Y1 ) = xd1(2)
               QF( IPOINT + OFIPD3_Z1 ) = xd1(3)
               QF( IPOINT + OFIPD3_X2 ) = xd2(1)
               QF( IPOINT + OFIPD3_Y2 ) = xd2(2)
               QF( IPOINT + OFIPD3_Z2 ) = xd2(3)
               qf( ipoint + ofipd3_tf) = hitd(7,ih)
               iqf( ipoint + ofipd3_id) = hitd(8,ih)
               qf( ipoint + ofipd3_de) = hitd(9,ih)
               qf( ipoint + ofipd3_xg1) = hitd(10,ih)
               qf( ipoint + ofipd3_yg1) = hitd(11,ih)
               qf( ipoint + ofipd3_zg1) = hitd(12,ih)
               qf( ipoint + ofipd3_pl) = hitd(13,ih)
               IPOINT = IPOINT + MFI_PD3

*    the end loop on number of hits

600   CONTINUE
         else

c     no hits case

            CALL MZBOOK ( IXDIV_FE,
     >                    LFP_PD3(IC,1),
     >                    LFP_PD3(IC,1),
     >                    1,
     >                    VPC3GASGP(IC),
     >                    0,
     >                    0,
     >                    MFI_PD3 + 1,
     >                    IOIPD3(IC),
     >                    -1 )
            IPOINT = LFP_PD3(IC,1) + 1
            IQF( IPOINT) = 0
         endif
633   CONTINUE   ! loop on number of chamber subsections PAD 3
      RETURN
      END
