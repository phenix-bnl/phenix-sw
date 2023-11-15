c $Id: pd23gem.f,v 1.3 2008/05/21 08:22:03 hpereira Exp $
*-- Author :    Charles F. Maguire   19/07/94
      SUBROUTINE PD23GEM
*       ============================

C         DEFINE USER GEOMETRY SET UP
*    for pad detectors 2,3:  called from TRD
C*************************************************


      IMPLICIT NONE
 
 
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpplink.inc"
#include "fstore.inc"
 

c     local variables

      integer nsec         ! number of azimuthal sections
      parameter (nsec = 8) ! UCDR value
      integer isec         ! do loop counter for number of azimuthal sections
      integer lfp_pd23     ! base pointer for the parameter bank PC2/PC3 values
      integer lfp_temp     ! temporary base pointer
      character*10 chform  ! data format for ZEBRA parameter bank
      integer iod          ! return format from ZEBRA
 

c     NOTE: The variables in the ITP2PR common block should go into a
c           parameter ZEBRA bank for later use in PISORP

      COMMON / ITP2PR/ FPD2PX, FPD2PZ, NCL2MX, NCL2MZ, PDDM2X, PDDM2Z,
     +DRPAD2, pd2sg
      REAL FPD2PX, FPD2PZ, PD2SG
      INTEGER NCL2MX, NCL2MZ

      COMMON / ITP3PR/ FPD3PX, FPD3PZ, NCL3MX, NCL3MZ, PDDM3X, PDDM3Z,
     +DRPAD3, PD3SG

      REAL FPD3PX, FPD3PZ, PD3SG
      INTEGER NCL3MX, NCL3MZ
      COMMON / RSPPAD/ RPSPD1, RPSPD2, RPSPD3, PADDLN( 18, 2, 3),
     +ENDXP1(18,2), ENDYP1(18,2), ENDXPD(8,2,2), ENDYPD(8,2,2), DZPD1,
     +DZPD2, DZPD3
      REAL RPSPD1, RPSPD2, RPSPD3, PADDLN, ENDXP1, ENDYP1, DZPD1,
     +DZPD2, DZPD3, ENDXPD, ENDYPD


      COMMON/PDPLPR/ APD1(18), BPD1(18), CPD1(18), DPD1(18), APD2(8),
     +BPD2(8), CPD2(8), DPD2(8), APD3(8), BPD3(8), CPD3(8), DPD3(8)
      REAL APD1, BPD1, CPD1, DPD1
      REAL APD2, BPD2, CPD2, DPD2
      REAL APD3, BPD3, CPD3, DPD3

      REAL X1, Y1, X2, Y2, Z0


*   .................................................................



      REAL PAR(20), TETGG(8)


      CHARACTER*4 NMCVSP(8), NMPADS(8), NMCVTP(8), NMPADT(8)

      REAL TET12, TANT12, ZCNTR, FICNTR, SNFI, CSFI, DFIPD2, XCNTP2,
     +YCNTP2, fieldm, tmaxfd, dmaxms, DFIPD3, XCNTP3, YCNTP3, deemax,
     +epsil, stmin, T1, T2, T3, F1, F2, F3, XI, YI, XF, YF, ROTPD,
     +wchgas(3), amylar(3), zmylar(3), wmylar(3), achgas(3), zchgas(3)
      INTEGER IST,JPS,J,JRT,JPT,L,JI,LL,IRTTR(8),NMPD23,N, ifld

*   SOME GEOMETRICAL PARAMETERS

*        read TRD, PD2, PD3 parameters through Namelist /TRD_PAR/

      REAL TETG, TACC, RINST2, ROTST2, FOILTH, GAPTH, FLNUMB, FLNMTH,
     +DAMPH, THCHMB, THPCSP, THDRSP, PECMNG, WRDMNS, THWIN1, THWIN2,
     +THSPRC, THSPCR, THRSHL, THRSHH, STTHR1, STTHR2, PDDM2X, PD2SGM,
     +PD3SGM, PDDM2Z, DRPAD2, PDDM3X, PDDM3Z, DRPAD3,
     +TDELPHI1, TDELPHI2, PDELPHI1, PDELPHI2

      INTEGER NMTRAP, NMMODL, NMLPWC, NMLDRC, NWSTRP, JTFORM, LPWCSH,
     +NMPCHM

      NAMELIST/TRD_PAR/ TETG, TACC, RINST2, ROTST2, NMTRAP, NMMODL,
     +FOILTH, GAPTH, FLNUMB, FLNMTH, DAMPH, THCHMB, THPCSP, THDRSP,
     +PECMNG, NMLPWC, NMLDRC, WRDMNS, NWSTRP, THWIN1, THWIN2, THSPRC,
     +THSPCR, THRSHL, THRSHH, STTHR1, STTHR2, JTFORM, LPWCSH, NMPCHM,
     +PDDM2X, PDDM2Z, DRPAD2, PD2SGM, PDDM3X, PDDM3Z, DRPAD3, PD3SGM,
     +TDELPHI1, TDELPHI2, PDELPHI1, PDELPHI2



      DATA TETGG / 33.75, 56.25, 78.75, 101.25, 258.75, 281.25, 303.75,
     +326.25 /

      DATA NMPADS /'PDS1', 'PDS2', 'PDS3', 'PDS4', 'PDS5', 'PDS6',
     +'PDS7', 'PDS8' /
      DATA NMPADT /'PDT1', 'PDT2', 'PDT3', 'PDT4', 'PDT5', 'PDT6',
     +'PDT7', 'PDT8' /
      DATA NMCVSP /'CVS1', 'CVS2', 'CVS3', 'CVS4', 'CVS5', 'CVS6',
     +'CVS7', 'CVS8' /
      DATA NMCVTP /'CVT1', 'CVT2', 'CVT3', 'CVT4', 'CVT5', 'CVT6',
     +'CVT7', 'CVT8' /

      DATA AMYLAR/12.01, 1.01, 15.99/, ZMYLAR/6., 1., 8./, WMYLAR/ 5.,
     +4., 2./
      DATA ACHGAS/ 39.95, 12.01, 1.01/, ZCHGAS/18., 6., 1./, WCHGAS/
     +0.7, 0.075, 0.225/
 
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun
     
c---------------------------------------------------------------------

      CALL GSMIXT(667,' MYLAR$',AMYLAR,ZMYLAR, 1.39, -3,WMYLAR)
      CALL GSMIXT(668,'GAS PAD CHMB$',ACHGAS,ZCHGAS,0.0014552,3,WCHGAS)

C       DEFINE USER TRACKING MEDIA PARAMETERS

      FIELDM = 0.
      IFLD = 0
      TMAXFD = 1.
      DMAXMS = 0.5
      DEEMAX = 0.2
      EPSIL = 0.005
      STMIN = 0.005

      CALL GSTMED(671,' GAS IN PAD CHMB N2 $', 668,  1,  IFLD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(673,' GAS IN PAD CHMB N3 $', 668,  1,  IFLD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(670,'PAD DET2 CAV.,MYLAR $', 667,  0,  IFLD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(672,'PAD DET3 CAV.,MYLAR $', 667,  0,  IFLD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
     


c     Read the geometry file segment
      
      write( *,* ) 'pd23gem - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = trd_par, err = 999 )


c     reset the TETGG array as needed

      if(pdelphi1.ne.0.0)then
         do l = 1,4
            tetgg(l) = tetgg(l) - pdelphi1
         enddo  ! first arm correction loop
      endif ! check on first arm correction
      if(pdelphi2.ne.0.0)then
         do l = 5,8
            tetgg(l) = tetgg(l) - pdelphi2
         enddo  ! second arm correction loop
      endif ! check on second arm correction
 
      PD2SG = PD2SGM
      PD3SG = PD3SGM

      NMPD23 = NMPCHM

      TACC = TACC * DEGRAD

      tetg = tetg * degrad
      tet12 = tetg / 2.
      tant12 = tan ( tet12)

      DFIPD2 = RINST2 * TANT12
      RPSPD2 = RINST2 + DRPAD2 * 0.5 + 1.
      DZPD2 = RPSPD2 * TAN( TACC)
      FPD2PX = -DFIPD2 + PDDM2X / 2.
      FPD2PZ = -DZPD2 - PDDM2Z / 2.
      NCL2MX = 2.* ABS( FPD2PX)/PDDM2X + 1.5
      NCL2MZ = 2.* ABS( FPD2PZ)/PDDM2Z + 1.5


      DFIPD3 = ROTST2 * SIN( TET12)
      RPSPD3 = ROTST2 * COS( TET12) - DRPAD3 * 0.5 - 1.
      DZPD3 = RPSPD3 * TAN( TACC)
      FPD3PX = -DFIPD3 + PDDM3X / 2.
      FPD3PZ = -DZPD3 - PDDM3Z / 2.
      NCL3MX = 2.* ABS( FPD3PX)/PDDM3X + 1.5
      NCL3MZ = 2.* ABS( FPD3PZ)/PDDM3Z + 1.5



*    This part is connected with space boundaries of
*      PD2 chambers:  we will need the coordinates
*     of "beginning" and "end" of each detector

      F1 = 22.5 + 90.
      ROTPD = RPSPD2 / COS( 11.25 * DEGRAD)
      XI = ROTPD * COS ( F1 * DEGRAD)
      YI = ROTPD * SIN ( F1 * DEGRAD)
      XF = ROTPD * COS ( (F1+22.5) * DEGRAD )
      YF = ROTPD * SIN ( (F1+22.5) * DEGRAD )
      ENDXPD( 1, 1, 1) = XI
      ENDYPD( 1, 1, 1) = YI
      ENDXPD( 1, 2, 1) = XF
      ENDYPD( 1, 2, 1) = YF

      DO N = 1, 3
         XI = XF
         YI = YF
         XF = ROTPD * COS ( (F1 + 22.5*(N+1))*DEGRAD)
         YF = ROTPD * SIN ( (F1 + 22.5*(N+1))*DEGRAD)
         ENDXPD(N+ 1, 1, 1) = XI
         ENDYPD(N+ 1, 1, 1) = YI
         ENDXPD(N+ 1, 2, 1) = XF
         ENDYPD(N+ 1, 2, 1) = YF
      END DO

      F1 = 247.5 + 90.
      XI = ROTPD * COS ( F1 * DEGRAD)
      YI = ROTPD * SIN ( F1 * DEGRAD)
      XF = ROTPD * COS ( (F1+22.5) * DEGRAD )
      YF = ROTPD * SIN ( (F1+22.5) * DEGRAD )
      ENDXPD( 5, 1, 1) = XI
      ENDYPD( 5, 1, 1) = YI
      ENDXPD( 5, 2, 1) = XF
      ENDYPD( 5, 2, 1) = YF

      DO N = 1, 3
         XI = XF
         YI = YF
         XF = ROTPD * COS ( (F1 + 22.5*(N+1))*DEGRAD)
         YF = ROTPD * SIN ( (F1 + 22.5*(N+1))*DEGRAD)
         ENDXPD(N+ 5, 1, 1) = XI
         ENDYPD(N+ 5, 1, 1) = YI
         ENDXPD(N+ 5, 2, 1) = XF
         ENDYPD(N+ 5, 2, 1) = YF
      END DO

*    This part is connected with space boundaries of
*      PD3 chambers:  we will need the coordinates
*     of "beginning" and "end" of each detector

      F1 = 22.5 + 90.
      ROTPD = RPSPD3 / COS( 11.25 * DEGRAD)
      XI = ROTPD * COS ( F1 * DEGRAD)
      YI = ROTPD * SIN ( F1 * DEGRAD)
      XF = ROTPD * COS ( (F1+22.5) * DEGRAD )
      YF = ROTPD * SIN ( (F1+22.5) * DEGRAD )
      ENDXPD( 1, 1, 2) = XI
      ENDYPD( 1, 1, 2) = YI
      ENDXPD( 1, 2, 2) = XF
      ENDYPD( 1, 2, 2) = YF

      DO N = 1, 3
         XI = XF
         YI = YF
         XF = ROTPD * COS ( (F1 + 22.5*(N+1))*DEGRAD)
         YF = ROTPD * SIN ( (F1 + 22.5*(N+1))*DEGRAD)
         ENDXPD(N+ 1, 1, 2) = XI
         ENDYPD(N+ 1, 1, 2) = YI
         ENDXPD(N+ 1, 2, 2) = XF
         ENDYPD(N+ 1, 2, 2) = YF
      END DO

      F1 = 247.5 + 90.
      XI = ROTPD * COS ( F1 * DEGRAD)
      YI = ROTPD * SIN ( F1 * DEGRAD)
      XF = ROTPD * COS ( (F1+22.5) * DEGRAD )
      YF = ROTPD * SIN ( (F1+22.5) * DEGRAD )
      ENDXPD( 5, 1, 2) = XI
      ENDYPD( 5, 1, 2) = YI
      ENDXPD( 5, 2, 2) = XF
      ENDYPD( 5, 2, 2) = YF

      DO N = 1, 3
         XI = XF
         YI = YF
         XF = ROTPD * COS ( (F1 + 22.5*(N+1))*DEGRAD)
         YF = ROTPD * SIN ( (F1 + 22.5*(N+1))*DEGRAD)
         ENDXPD(N+ 5, 1, 2) = XI
         ENDYPD(N+ 5, 1, 2) = YI
         ENDXPD(N+ 5, 2, 2) = XF
         ENDYPD(N+ 5, 2, 2) = YF
      END DO


      Z0 = DZPD2
      DO N = 1, 8
         X1 = ENDXPD(N, 1,1)
         Y1 = ENDYPD(N, 1,1)
         X2 = ENDXPD(N, 2,1)
         Y2 = ENDYPD(N, 2,1)
 
         APD2(N) = 2.* Z0 * ( Y1 - Y2)
         BPD2(N) = 2.* Z0 * ( X2 - X1)
         CPD2(N) = 0.
         DPD2(N) = -2.* Z0 * ( X2*Y1 - Y2*X1 )
      END DO


      Z0 = DZPD3
      DO N = 1, 8
         X1 = ENDXPD(N, 1,2)
         Y1 = ENDYPD(N, 1,2)
         X2 = ENDXPD(N, 2,2)
         Y2 = ENDYPD(N, 2,2)
 
         APD3(N) = 2.* Z0 * ( Y1 - Y2)
         BPD3(N) = 2.* Z0 * ( X2 - X1)
         CPD3(N) = 0.
         DPD3(N) = -2.* Z0 * ( X2*Y1 - Y2*X1 )
      END DO

* ................................................
*             ROTATION MATRIX FOR TRAP

      T1 = 90.
      T2 = 90.
      T3 = 0.
      F3 = 0.
      DO 781 JRT = 1, NMPCHM
         F1 = TETGG ( JRT)
         F2 = F1 + 90.
         irot = irot + 1
         irttr( jrt) = irot
         CALL GSROTM ( IRTTR(JRT), T1, F1, T2, F2, T3, F3 )
  781 CONTINUE

*   *************************************************

*  -----------------------------------------------------------


*        SECOND  PAD DETECTOR

      PAR( 1) = DFIPD2
      PAR( 2) = DRPAD2/2.
      PAR( 3) = DZPD2
      DO L = 1, NMPCHM
         CALL GSVOLU (NMCVSP(L),'BOX ',670, PAR, 3, IST )
         CALL GSATT (NMCVSP(L),'SEEN',0 )
      END DO

      DO JI = 1,3
         PAR( JI) = PAR( JI) - 0.072
      END DO

      DO L = 1, NMPCHM
         CALL GSVOLU (NMPADS(L),'BOX ',671, PAR, 3, IST)
         CALL GSATT (NMPADS(L),'SEEN',1 )
         CALL GSATT (NMPADS(L),'COLO',5 )
      END DO


*        THIRD  PAD DETECTOR

      PAR( 1) = DFIPD3
      PAR( 2) = DRPAD3/2.
      PAR( 3) = DZPD3


      DO L = 1, NMPCHM
         CALL GSVOLU (NMCVTP(L),'BOX ',672, PAR, 3, IST )
         CALL GSATT (NMCVTP(L),'SEEN',0 )
      END DO

      DO JI = 1,3
         PAR( JI) = PAR( JI) - 0.143
      END DO

      DO L = 1, NMPCHM
         CALL GSVOLU (NMPADT(L),'BOX ',673, PAR, 3, IST)
         CALL GSATT (NMPADT(L),'SEEN',1 )
         CALL GSATT (NMPADT(L),'COLO',5 )
      END DO


* ............................................................
*      WE FINISHED WITH VOLUMES AND COME TO SPACE AND POSITION
*                 PROBLEM


      ZCNTR = 0.

      DO 815 J = 1, NMPCHM
         JPT = 1
         FICNTR = PIBY2  + TETGG( J) * DEGRAD


         SNFI = SIN ( FICNTR)
         CSFI = COS ( FICNTR)

         XCNTP2 =   RPSPD2  * CSFI
         YCNTP2 =   RPSPD2  * SNFI

         XCNTP3 =   RPSPD3  * CSFI
         YCNTP3 =   RPSPD3  * SNFI



c     Mother volume (for now) is EMCL instead of TRAD

         CALL GSPOS (NMCVSP(J),JPT,'EMCL', XCNTP2, YCNTP2, ZCNTR,
     +   IRTTR(J), 'ONLY' )
         CALL GSPOS (NMPADS(J),JPT,NMCVSP(J),0.,0.,0., 0, 'ONLY' )

         CALL GSPOS (NMCVTP(J),JPT,'EMCL', XCNTP3, YCNTP3, ZCNTR,
     +   IRTTR(J), 'ONLY' )
         CALL GSPOS (NMPADT(J),JPT,NMCVTP(J),0.,0.,0., 0, 'ONLY' )
*  .............................................................
  815 CONTINUE

c    parameter bank for PC2/PC3

c     PD23 geometry takes npd23_para  additional parameters (FPLINK definition)

 
      chform = '3I -F'  ! 3 integers, rest floating  point
      call mzform('PARA', CHFORM, iod)  ! book characteristics
      call mzbook(ixdiv_fr, lfp_para, lfp_para, 1, 'PARA', 0, 0,
     +            1 + npd23_para, iod, 0)

c     now store the PC2/PC3 parameters for later use in PISORP

      lfp_pd23 = lfp_para
      qf(lfp_pd23 + 1) = rpspd2
      qf(lfp_pd23 + 2) = rpspd3
      lfp_temp = lfp_pd23 + 2
      do isec = 1,nsec
         qf(lfp_temp + isec) = endxpd(isec, 1, 1)
      enddo  ! loop over azimuthal sections for XI values in PC2
      lfp_temp = lfp_temp + nsec
      do isec = 1,nsec
         qf(lfp_temp + isec) = endxpd(isec, 2, 1)
      enddo  ! loop over azimuthal sections for XF values in PC2
      lfp_temp = lfp_temp + nsec
      do isec = 1,nsec
         qf(lfp_temp + isec) = endypd(isec, 1, 1)
      enddo  ! loop over azimuthal sections for YF values in PC2
      lfp_temp = lfp_temp + nsec
      do isec = 1,nsec
         qf(lfp_temp + isec) = endypd(isec, 2, 1)
      enddo  ! loop over azimuthal sections for YF values in PC2
      lfp_temp = lfp_temp + nsec
      do isec = 1,nsec
         qf(lfp_temp + isec) = endxpd(isec, 1, 2)
      enddo  ! loop over azimuthal sections for XI values in PC3
      lfp_temp = lfp_temp + nsec
      do isec = 1,nsec
         qf(lfp_temp + isec) = endxpd(isec, 2, 2)
      enddo  ! loop over azimuthal sections for XF values in PC3
      lfp_temp = lfp_temp + nsec
      do isec = 1,nsec
         qf(lfp_temp + isec) = endypd(isec, 1, 2)
      enddo  ! loop over azimuthal sections for YF values in PC3
      lfp_temp = lfp_temp + nsec
      do isec = 1,nsec
         qf(lfp_temp + isec) = endypd(isec, 2, 2)
      enddo  ! loop over azimuthal sections for YF values in PC3
      lfp_temp = lfp_temp + nsec
      qf(lfp_temp + 1) = dzpd2
      qf(lfp_temp + 2) = dzpd3

c     now store the PDPLPR common block variables

      lfp_temp = lfp_temp + 2
      do isec = 1,nsec
         qf(lfp_temp + isec) = apd2(isec)
         qf(lfp_temp + 8 + isec) = bpd2(isec)   ! assumes NSEC = 8 here
         qf(lfp_temp + 16 + isec) = cpd2(isec)  ! assumes NSEC = 8 here
         qf(lfp_temp + 24 + isec) = dpd2(isec)  ! assumes NSEC = 8 here
         qf(lfp_temp + 32 + isec) = apd3(isec)  ! assumes NSEC = 8 here
         qf(lfp_temp + 40 + isec) = bpd3(isec)  ! assumes NSEC = 8 here
         qf(lfp_temp + 48 + isec) = cpd3(isec)  ! assumes NSEC = 8 here
         qf(lfp_temp + 56 + isec) = dpd3(isec)  ! assumes NSEC = 8 here
      enddo  ! loop to store the PDPLPR common block variables
 
      RETURN

  999 continue
      write(6,1000)
 1000  format(/'pd23gem - read error in pad_par segment'/,3x,
     +   '  Namelist mis-match in pad_par segment ?',//,3x,
     +   'The geometry will be re-read to pinpoint the erroneous',
     +  ' line',/,3x,'****This will cause the program to crash.****',//)
        
      rewind( itf_lun )
      read( itf_lun, nml = trd_par )
      stop 'pd23gem - stop because of geometry error.'
      END
