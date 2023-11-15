      SUBROUTINE ZDC_GEOM

#include "gcvolu.inc"
#include "gcflag.inc"
#include "gugeom.inc"

C.. SEQ. /GCONSP/................................................
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO

      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)

c------------------------------------------------
c---  Materials indexis:
c---  H=1
c---  D=2
c---  C=6
c---  N=7
c---  Al=9
c---  Fe=10
c---  Cu=11
c---  W =12
c---  Pb=13
c---  U =14
c---  Air=15
c---  Vacuum=16
c--------------------------------------------------
C.. Data for PMMA:
      DIMENSION PMMA_A(3),PMMA_Z(3),PMMA_W(3),PAR(3)
      DATA PMMA_A/12.,1.,16./
      DATA PMMA_Z/6.,1.,8./
      DATA PMMA_W/5.,8.,2./

c.. Half-size of the MOTHER volume (defined in UGEOM)
      COMMON/XYZ_MOTHER/XYZ_M(3),TILTE_ANG

C........ ZDC geometrical structure:.............................

c    N_MOD   -   number of identical modules in the ZDC. The length of
c                each module is of about 2 interaction length.
c                Structure of the module: (front plate)/
c                (absorber plate)/(gap with fibers)/
c                (absorber plate)/(gap with fibers)/....
c                                         /(back plate)
c    N_LAY   -   number of layers (absorber+gap with fibers)in each module.
c    NUM_FIB -   number of fibers in layer.
c    T_PLA, H_PLA, W_PLA, M_PLA - thickness, height, width and material index
c                                 of the front/back plates.
c    T_ABS, H_ABC, W_ABS, M_ABS -   --""--  absorber plates.
c    T_GAP, H_GAP, W_GAP, M_GAP -   --""--  gap with fibers.
c    D_FIB,H_FIB, M_FIB         -   fiber's diameter, length and material index.
c    TILTE_ANG           -   tilte angle of sandwich structure (degree).
c                            ALL DIMENSIONS IN CM.
c........................................................................


      COMMON/ZDC_DEF/
     +      N_MOD,N_LAY,N_FIB,
     +      N_FIB_FST,N_LAY_FST,N_ABS_FST,N_PLA_FST,
     +      T_PLA,H_PLA,W_PLA,M_PLA,
     +      T_ABS,H_ABS,W_ABS,M_ABS,
     +      D_FIB,H_FIB,W_FIB,M_FIB,
     +      AGAP, T_GAP,H_GAP,W_GAP,M_GAP,
     +      FIB_LEN_LAY(27)

      COMMON /ZDCCOM/ ism41, ism42, zdcl
      INTEGER ism41, ism42
      CHARACTER*4 zdcl


c     C.F. Maguire  February 9, 2000
c     We cannot specify the rotational numbers for PISA geometries.
c     This prevents conflicts in rotation numbers between different detectors.
c     We cannot specify a detector specific range of rotations numbers either.

      INTEGER irot1, irot2, irot3

c      DATA irot1/1338/
c      DATA irot2/1339/


      INTEGER imed1, imed2, imed3, imed4
      DATA imed1/1321/
      DATA imed2/1322/
      DATA imed3/1323/
      DATA imed4/1324/


c     CFM: change from 15 to 11 for the number of hits data elements

      CHARACTER*4 namesh2(11)
      INTEGER     nbitsh2(11)
      REAL        orig2(11),fact2(11)
      INTEGER     iset, idet

      DATA namesh2/'X   ','Y   ','Z   ','DE  ','PX  ','PY  ','PZ  '
     +            ,'TOF ','PID ','N-S ','MODU'/
      DATA nbitsh2/11*32/
      DATA orig2  /3*1.0e4, 0,     3*1.0e3, 0,      3*0/  
      DATA fact2  /3*1.0e2, 1.0e7, 3*1.0e5, 1.0e12, 3*1/

**************************************************************

       N_FIB_FST=0
       N_LAY_FST=1000
       N_ABS_FST=2000
       N_PLA_FST=3000

c.................  Modules definition:

c..Number of Modules in ZDC
       N_MOD=3
c..Number of Layers in Module
       N_LAY=27
c..Tilte angle of layers relative beam direction(degree)
       TILTE_ANG=45.
c..ZDC distance from origo
cscj       ZDC_DIST=1890.325
       ZDC_DIST=1843

c..  Front and back plates definition (material - IRON):
       T_PLA=0.23
       H_PLA=40.
       W_PLA=10.
       M_PLA=10

c..  Asorber plates definition (material - TUNGSTEN):
       T_ABS=0.5
       H_ABS=15.0
       W_ABS=10.
       M_ABS=12

c..  Fibers definition (material - PMMA):
       D_FIB=0.05
       T_FIB=D_FIB*3.14/4.
       H_FIB=40.
       W_FIB=10.
       M_FIB=1326

c..  Gap with fibers definition (material - AIR):
       AGAP=0.02
       T_GAP=D_FIB+AGAP
       H_GAP=H_FIB
       W_GAP=W_FIB
       M_GAP=15

c..   Fill in table with the ribbon lengths:
       AL_MAX=54.0
       PITCH=(T_ABS+T_GAP)/SIN(TILTE_ANG*DEGRAD)
c       CUT_STEP=0.
       CUT_STEP=PITCH*3.3/5.0
       DO J=1,N_LAY
       FIB_LEN_LAY(J)=AL_MAX-H_ABS/2.-CUT_STEP*(J-1)
       ENDDO

C..  Define additional materials (PMMA):

      CALL GSMIXT(M_FIB,'PMMA',PMMA_A,PMMA_Z,1.18,-3,PMMA_W)

C..  Define tracking medium parameters:
      IFIELD=0.
      FIELDM=0.
      TMAXFD=20.
      STEMAX=0.01
      DEEMAX=0.01
      EPSIL=0.01
      STMIN=0.1

C..  Materials setting:
      I_SENS=1
      CALL GSTMED(imed1,'AIR',M_GAP,I_SENS,IFIELD,FIELDM,TMAXFD,
     +             STEMAX,DEEMAX,EPSIL,STMIN,0.,0)
      CALL GSTMED(imed2,'IRON',M_PLA,I_SENS,IFIELD,FIELDM,TMAXFD,
     +             STEMAX,DEEMAX,EPSIL,STMIN,0.,0)
      CALL GSTMED(imed3,'WOLF',M_ABS,I_SENS,IFIELD,FIELDM,TMAXFD,
     +             STEMAX,DEEMAX,EPSIL,STMIN,0.,0)
      I_SENS=1
      CALL GSTMED(imed4,'PMMA',M_FIB,I_SENS,IFIELD,FIELDM,TMAXFD,
     +             STEMAX,DEEMAX,EPSIL,STMIN,0.,0)

c.. POS_FIRST - z-coordinate of first front plate of the first module:

      AL_MOD=2*T_PLA+N_LAY*(T_ABS+T_GAP)
      AL_DET=AL_MOD*N_MOD
      RTT=1./SIN(TILTE_ANG*DEGRAD)
      ALENG_DET=AL_DET*RTT
      POS_FIRST=-ALENG_DET/2.

C..  Set MOTHER volume - box with air:
      Z_MOTHER=ALENG_DET+2.*(H_GAP-H_abs/2.)*COS(TILTE_ANG*DEGRAD)
      ROOM=1.
      XYZ_M(1)=W_ABS/2.+ROOM
      XYZ_M(2)=(H_GAP-H_ABS/2.)*SIN(TILTE_ANG*DEGRAD)+ROOM
      XYZ_M(3)=Z_MOTHER/2.+ROOM 
      CALL GSVOLU('MAI1','BOX ',imed1,XYZ_M,3,IVOLU)
      CALL GSVOLU('MAI2','BOX ',imed1,XYZ_M,3,IVOLU)

C-----Define rotation matrix for fibers inside air plate

      irot = irot + 1
      CALL GSROTM(irot, 90., 0., 90., 90., 180.,00.)
      irot3 = irot

      CALL GSPOS('MAI1',1,zdcl,0.,0., ZDC_DIST,0,'ONLY')
      CALL GSPOS('MAI2',1,zdcl,0.,0.,-ZDC_DIST,irot3,'ONLY')

C..  Set casette's front & back plates volume:
      PAR(1)=W_PLA/2.
      PAR(2)=H_PLA/2.
      PAR(3)=T_PLA/2.
      CALL GSVOLU('I_P1','BOX ',imed2,PAR,3,IVOLU)
      CALL GSVOLU('I_P2','BOX ',imed2,PAR,3,IVOLU)

C.. Set absorber's plate volume:
      PAR(1)=W_ABS/2.
      PAR(2)=H_ABS/2.
      PAR(3)=T_ABS/2.
      CALL GSVOLU('W_P1','BOX ',imed3,PAR,3,IVOLU)
      CALL GSVOLU('W_P2','BOX ',imed3,PAR,3,IVOLU)

C.. Set gap volume (fiber's container):
      PAR(1)=W_GAP/2.
      PAR(2)=H_GAP/2.
      PAR(3)=T_GAP/2.
      CALL GSVOLU('GAP1','BOX ',imed1,PAR,3,IVOLU)
      CALL GSVOLU('GAP2','BOX ',imed1,PAR,3,IVOLU)

C-----Define PMMA cylindric fiber

      PAR(1)=0.
      PAR(2)=D_FIB/2.
      PAR(3)=H_FIB/2.

      CALL GSVOLU('FIB1','TUBE',imed4,PAR,3,IVOLU)
      CALL GSVOLU('FIB2','TUBE',imed4,PAR,3,IVOLU)

C-----Define rotation matrix for fibers inside air plate

      irot = irot + 1
      CALL GSROTM(irot,90.,0.,180.,0.,90.,90.)
      irot2 = irot

C-----Positioning fibers into air plate "GAPF"

      X_POS=-W_FIB/2.
      ST=D_FIB/2.
      N_FIB=W_FIB/D_FIB
      I_VOL_F=N_FIB_FST
      DO I=1,N_FIB
      X_POS=X_POS+ST
      I_VOL_F=I_VOL_F+1
      CALL GSPOS('FIB1',I_VOL_F,'GAP1',X_POS,0.,0.,irot2,'ONLY')
      CALL GSPOS('FIB2',I_VOL_F,'GAP2',X_POS,0.,0.,irot2,'ONLY')
      X_POS=X_POS+ST
      ENDDO

C.. Define rotation matrix for the plates

      irot = irot + 1
      CALL GSROTM(irot,90.,0.,TILTE_ANG,90.,90.-TILTE_ANG,270.)
      irot1 = irot

C.. Positioning of F/B plates, absorber plates and gaps with fibers
C.. inside the MOTHER:
      RTT=1./SIN(TILTE_ANG*DEGRAD)
      STEP_PLA=T_PLA*RTT
      STEP_ABS=T_ABS*RTT
      STEP_GAP=T_GAP*RTT

c.. Start loop on modules and layers:

      POS=POS_FIRST-STEP_PLA/2.
      Y_POS=(H_GAP-H_ABS)/2.*SIN(TILTE_ANG*DEGRAD)
      Z_POS=(H_GAP-H_ABS)/2.*COS(TILTE_ANG*DEGRAD)

      Y_POS_PL=(H_PLA-H_ABS)/2.*SIN(TILTE_ANG*DEGRAD)
      Z_POS_PL=(H_PLA-H_ABS)/2.*COS(TILTE_ANG*DEGRAD)
      I_VOL_PLA=N_PLA_FST
      I_VOL_ABS=N_ABS_FST
      I_VOL_FIB=N_LAY_FST
      DO N=1,N_MOD
c.. Positioning front plate of each module:
       I_VOL_PLA=I_VOL_PLA+1
       POS=POS+STEP_PLA/2.
       CALL GSPOS('I_P1',I_VOL_PLA,'MAI1',0.,Y_POS_PL,
     +            POS+Z_POS_PL,irot1,'ONLY')
       CALL GSPOS('I_P2',I_VOL_PLA,'MAI2',0.,Y_POS_PL,
     +            POS+Z_POS_PL,irot1,'ONLY')
          POS=POS+STEP_PLA/2.

          DO I=1,N_LAY
C.. Positioning absorber plate:
              I_VOL_ABS=I_VOL_ABS+1
              POS=POS+STEP_ABS/2.
              CALL GSPOS('W_P1',I_VOL_ABS,'MAI1',0.,0.,
     +            POS,irot1,'ONLY')
              CALL GSPOS('W_P2',I_VOL_ABS,'MAI2',0.,0.,
     +            POS,irot1,'ONLY')
              POS=POS+STEP_ABS/2.

C.. Positioning gap with fibers volume:
              I_VOL_FIB=I_VOL_FIB+1
              POS=POS+STEP_GAP/2.
              CALL GSPOS('GAP1',I_VOL_FIB,'MAI1',0.,Y_POS,
     +           POS+Z_POS,irot1,'ONLY')
              CALL GSPOS('GAP2',I_VOL_FIB,'MAI2',0.,Y_POS,
     +           POS+Z_POS,irot1,'ONLY')
              POS=POS+STEP_GAP/2.

          ENDDO
C.. Positioning back plate:
          I_VOL_PLA=I_VOL_PLA+1
          POS=POS+STEP_PLA/2.
          CALL GSPOS('I_P1',I_VOL_PLA,'MAI1',0.,Y_POS_PL,
     +                POS+Z_POS_PL,irot1,'ONLY')
          CALL GSPOS('I_P2',I_VOL_PLA,'MAI2',0.,Y_POS_PL,
     +                POS+Z_POS_PL,irot1,'ONLY')
          POS=POS+STEP_PLA/2.
      ENDDO


* Define sensitive detectors

      CALL gsdetv('ZDC ','MAI1',8,12000,12000,iset,idet)
      CALL gsdetv('ZDC ','MAI2',8,12000,12000,iset,idet)

      CALL gsdeth('ZDC ','MAI1',11,namesh2,nbitsh2,orig2,fact2)
      CALL gsdeth('ZDC ','MAI2',11,namesh2,nbitsh2,orig2,fact2)

      RETURN
      END
