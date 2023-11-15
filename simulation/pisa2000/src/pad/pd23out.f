*CMZ :  2.04/00 23/11/94  14.07.25  by  Charles F. Maguire
*CMZ :      05/02/93  22.09.13  by  Charles F. Maguire

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)

      SUBROUTINE PD23OUT

*     USER ROUTINE CALLED AT THE END OF EACH EVENT for PAD2/PAD3 output


*     New version July 19, 1994

      IMPLICIT NONE
      INTEGER  NHMAX, NHDDR, NWDIM
      PARAMETER ( NHMAX=1999,  NHDDR= 9, NWDIM=4 )
      INTEGER  NUMWS( NWDIM), ITRAH(NHMAX)
      REAL HITD(NHDDR,NHMAX)
      INTEGER NUMVW(NWDIM,NHMAX), NAMEPD(NWDIM), NMPD(NWDIM)

c    Hit names for PD23DET subroutine

c     DATA NAMEH5/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ',
c    >            'TOF ','PTID', 'DELE'/

 
#include "gcflag.inc"
#include "gcbank.inc"
#include "gcnum.inc"
#include "gconst.inc"
#include "gcvolu.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "fpplink.inc"
*KEND.
 
       REAL XD(3), XD1(3), XD2(3)
       INTEGER LJFST, IC, NHITS, IPOINT, JPOINT, JK,
     >    IH, ITRC, JJ, IOIPD2(8), IOIPD3(8)
      CHARACTER*20 CHFORM
      CHARACTER*4 NAMECH
      CHARACTER*4 NMCVSP(8), NMPADS(8), NMCVTP(8), NMPADT(8)
      CHARACTER*4 MSPD(4), MTPD(4)
      integer nmchm    ! number of chamber subsections (fixed at 8)
      parameter (nmchm = 8)   ! change for UCDR version
 
      DATA NMPADS /'PDS1', 'PDS2', 'PDS3', 'PDS4', 'PDS5', 'PDS6',
     1             'PDS7', 'PDS8'/
      DATA NMPADT /'PDT1', 'PDT2', 'PDT3', 'PDT4', 'PDT5', 'PDT6',
     1             'PDT7', 'PDT8'/
      DATA NMCVSP /'CVS1', 'CVS2', 'CVS3', 'CVS4', 'CVS5', 'CVS6',
     1             'CVS7', 'CVS8'/
      DATA NMCVTP /'CVT1', 'CVT2', 'CVT3', 'CVT4', 'CVT5', 'CVT6',
     1             'CVT7', 'CVT8'/
 
      DATA MSPD /'HALL', 'EMCL', '    ', '    '/
      DATA MTPD /'HALL', 'EMCL', '    ', '    '/
      DATA LJFST /0/
      SAVE LJFST, IOIPD2, IOIPD3

      IF( LJFST .EQ. 0 ) THEN
         LJFST = 1
         CHFORM = '1I / 1I 7F 1I 5F'
         DO IC= 1, NMCHM    ! should have 8 chambers only
            CALL MZFORM ( NMPADS(IC), CHFORM, IOIPD2(IC) )
            CALL MZFORM ( NMPADT(IC), CHFORM, IOIPD3(IC) )
         END DO
      ENDIF

*  ........................................................

      CALL VZERO ( NUMWS, NWDIM )

*     The loop on the number of chambers

      DO 433 IC = 1, NMCHM
         MSPD( 3) = NMCVSP(IC)
         MSPD( 4) = NMPADS(IC)
         NAMECH = NMPADS(IC)
         CALL GFHITS ( 'PAD ', NAMECH, NWDIM, NHDDR, NHMAX, 0,
     >                 NUMWS, ITRAH, NUMVW, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> PAD (pd23out.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if

c     need to pre-book the ZEBRA output in case of no hits (see simul20.mem)

         IF ( NHITS .GE. 1 )then
            CALL MZBOOK ( IXDIV_FE,
     >                    LFP_PD2(IC,1),
     >                    LFP_PD2(IC,1),
     >                    1,
     >                    NMPADS(IC),
     >                    0,
     >                    0,
     >                    MFI_PD2*NHITS + 1,
     >                    IOIPD2(IC),
     >                   -1 )
            IPOINT = LFP_PD2(IC,1) + 1
            JPOINT = IPOINT
            IQF( JPOINT) = NHITS

*      loop on the number of hits for the chamber fixed

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
               IPOINT = IPOINT + MFI_PD2

*    the end loop on number of hits

400         CONTINUE
         else

c     no hits case for this subsection

            CALL MZBOOK ( IXDIV_FE,
     >                    LFP_PD2(IC,1),
     >                    LFP_PD2(IC,1),
     >                    1,
     >                    NMPADS(IC),
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
         MTPD( 3) = NMCVTP(IC)
         MTPD( 4) = NMPADT(IC)
         NAMECH = NMPADT(IC)
         CALL GFHITS ( 'PAD ', NAMECH, NWDIM, NHDDR, NHMAX, 0,
     >                NUMWS, ITRAH, NUMVW, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> PAD (pd23out.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if

         IF ( NHITS .GE. 1 )then
            CALL MZBOOK ( IXDIV_FE,
     >                    LFP_PD3(IC,1),
     >                    LFP_PD3(IC,1),
     >                    1,
     >                    NMPADT(IC),
     >                    0,
     >                    0,
     >                    MFI_PD3*NHITS + 1,
     >                    IOIPD3(IC),
     >                    -1 )
            IPOINT = LFP_PD3(IC,1) + 1
            JPOINT = IPOINT
            IQF( JPOINT) = NHITS

*      loop on the number of hits for the chamber fixed

            DO 600 IH = 1, NHITS

*      coordinate information in "local" system

               DO JJ = 1,3
                  XD1( JJ) = HITD( JJ, IH )
                  XD2( JJ) = HITD( JJ+3, IH )
               END DO
               IQF( IPOINT + OFIPD3_TR ) = itrah(ih)
               call trkstack(itrah(ih))  ! put track # in PISA track stacker
               QF( IPOINT + OFIPD3_X1 ) = xd1(1)
               QF( IPOINT + OFIPD3_Y1 ) = xd1(2)
               QF( IPOINT + OFIPD3_Z1 ) = xd1(3)
               QF( IPOINT + OFIPD3_X2 ) = xd2(1)
               QF( IPOINT + OFIPD3_Y2 ) = xd2(2)
               QF( IPOINT + OFIPD3_Z2 ) = xd2(3)
               qf( ipoint + ofipd3_tf) = hitd(7,ih)
               iqf( ipoint + ofipd3_id) = hitd(8,ih)
               qf( ipoint + ofipd3_de) = hitd(9,ih)
               IPOINT = IPOINT + MFI_PD3

*    the end loop on number of hits

600   CONTINUE
         else

c     no hits case

            CALL MZBOOK ( IXDIV_FE,
     >                    LFP_PD3(IC,1),
     >                    LFP_PD3(IC,1),
     >                    1,
     >                    NMPADT(IC),
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
