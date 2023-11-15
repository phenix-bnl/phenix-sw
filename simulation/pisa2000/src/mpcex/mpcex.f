C=====================================================================
C    FILE NAME mpcex
C.   author : J. Lajoie and A. Timilsina
C    Feb 22, 2011
C.   Muon Piston Calorimeter + Si Preshower modifications
C    Located on both sides from IP: face starts at: |z| ~ 220 cm
C    Emcal is made out of PbWO4 r0 = 8.28 g/cm^3 
C    Size 2.2x2.2x18cm^3 
C    ~200 modules per side
C=====================================================================
      subroutine mpcex
C=====================================================================

C    DESCRIPTION: This routine defines the geometry for the Muon-Piston
C                 Calorimeter. Structure of MPC is embedded in the mother
C                 volume of MUA1 (north) or MUA2 (south).

C     HALL --> MUA1/2 --> PBO4                  ! MPC volume hierarchy

C    MAP:
C           1) CALLED BY; GUGEOM
C           2) CALLS;     GSVOLU,GSATT,GSROTH,GSPOS,GSDET,GSDETH

C=====================================================================


#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"
C#include 'gcsets.inc'
#include "gcvolu.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "fmpclink.inc"
C mutr header, for z_roll()
#include "fpmlink.inc"

C variables for GSTMED
      INTEGER NMAT,ISVOL,IFIELD,NWBUF
      REAL FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,
     1  UBUF(10)

C variables for GSDET
C      character*4 NAMESV1(3),NAMESV2(3)
C      data NAMESV1/'HALL','MUA1','PBO4'/        ! Tower
C      data NAMESV2/'HALL','MUA2','PBO4'/        ! Tower
      character*4 NAMESV
      data NAMESV/'PBO4'/        ! Hit Identified By Tower
      integer NBITSV(1)
      data  NBITSV/16/           ! Tower Number Is 32 Bit
      integer IDTYPE,ISET,IDET
      data  IDTYPE/2023/         ! User Defined Detector Type

C variables for GSDETH
      character*4 namesh(11)
      integer  nbitsh(11)
      real     orig(11),fact(11)

      DATA namesh/'X   ','Y   ','Z   ','TOFG','XE  ','YE  ',
     *            'MOME','PIDE','NUME','TOWR','DEDX'/
      DATA nbitsh/11*32/
      DATA orig  /7*1000., 0.,0.,0.,0./      ! offsets on namesh
      DATA fact  /7*1000., 1.,1.,1.,1000./   ! scale factors on namesh


c     The above gains (offsets and scale factors) give
c              - 10  um position resolution
c              - 1.0 MeV/c incoming momentum resolution
c              - 1.0 MeV energy deposition resoution


C Material definition
      integer imat, imed
      integer PBWO4                    ! Tracking Medium

      common/mpcalorpar/PBWO4

      data imat/2500/
      data imed/2505/
      data PBWO4/2506/         ! defines NUMEu
C        data Tungsten/25007/

C Scratch variables
      integer ivolu
      real tower_par(3)         ! tower center location

C PbWO4 parameters
      real awlf(3),zwlf(3),wwlf(3)
      real denwlf
      DATA awlf/207.2, 183.84, 15.9994/ ! A for Pb,W,O
      DATA zwlf/82.0,   74.0,   8.0/    ! Z for Pb,W,O
      DATA wwlf/1.0,     1.0,   4.0/    ! atomic proportions
      DATA denwlf/8.28/                 ! PbWO4 density

C Material thickneses in X,Y,Z directions (2.2x2.2x18 cm^3)
      real x_wlf,y_wlf,z_wlf,z_pos
      data x_wlf/1.1/
      data y_wlf/1.1/
      data z_wlf/9.0/

C  Size of One Tower
      namelist /mpc_coord/ x_wlf,y_wlf,z_wlf,z_pos
       
C Mix the above
      CALL GSMIXT(imat,'PBWO4$',awlf,zwlf,denwlf,-3,wwlf)

C Parameters include file


C--            DEFINES USER TRACKING MEDIA PARAMETERS

      IFIELD = 1     ! b-field (0 for none)
      FIELDM = 10.   ! 0.
      TMAXFD = 45.0  ! 10.0 (max angle due to field/step)
      DEEMAX =  0.2  ! 0.2  (max fractional energy loss/step)
      
      STEMAX =  0.0001  
      EPSIL  =  0.0001  ! tracking precision in cm
      STMIN  =  0.0001  ! min step due to eloss of mscatter (cm)

      CALL GSTMED(imed+1,'MPC_PWO$', imat,  1 , IFIELD,
     *             FIELDM,TMAXFD,STEMAX,DEEMAX, EPSIL, STMIN, 0 , 0 )

C   Tracking media #2507 - Aluminum skin
      nmat = 9       ! Al
C      isvol = 0      ! not sensitive

      if(ivolu_opt(1,26).ne.0) then 
        isvol = 1   ! sensitive
      else
        isvol = 0
      end if

      ifield = 1     ! magnetic field
      fieldm = 5.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
C      dmaxms = 1.0   ! max disp. due to mulsct. in one step (cm)
C      deemax = 0.2   ! max fractional energy loss in one step
C      epsil = 0.1    ! tracking precision (cm)
C      stmin = 0.1    ! min step due to e loss or mulsct. (cm)
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil = 0.01   ! tracking precision (cm)
      stmin = 0.01   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      nwbuf = 1
      call gstmed(2507,'MPCskin Al $',nmat,isvol,ifield,fieldm,tmaxfd,
     *            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Modified cutoffs 
      call gstpar(2507,'CUTELE',0.001)      ! electron cut
      call gstpar(2507,'CUTGAM',0.001)      ! gamma cut
      call gstpar(2507,'BCUTE',0.001)      ! e brem cut

C   Tracking media #2508 - Steel Vblock
      nmat = 10      ! Fe
      isvol = 0      ! not sensitive sensitive
      ifield = 1     ! magnetic field
      fieldm = 5.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 1.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = 0.1    ! tracking precision (cm)
      stmin = 0.1    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      nwbuf = 1
      call gstmed(2508,'NBpipe V $',nmat,isvol,ifield,fieldm,tmaxfd,
     *            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

C--  DEFINE USER VOLUMES


C  --- PBO4 Volume (One Tower Only)
        tower_par(1) =  x_wlf
        tower_par(2) =  y_wlf
        tower_par(3) =  z_wlf
        CALL GSVOLU('PBO4','BOX ',PBWO4,tower_par,3,ivolu)
        IF(ivolu.lt.0) STOP 'ERROR WITH GSVOLU'   

C  --- Now place the crystals in the MPC configuration

        write( *,* ) 'MPC (MPC-EX) Detector Version - August 2015'
        CALL SMPCEX
        CALL NMPCEX

      IF (CVOLU_OPT(1,24).EQ.'FULL') THEN   ! check volume option
        CALL GSDET('MPC ','PBO4',1,NAMESV,NBITSV,IDTYPE,100,100,
     *  ISET,IDET)

        CALL GSDETH('MPC ','PBO4',11,namesh,nbitsh,orig,fact)

        CALL GPSETS('MPC ','PBO4')
      ENDIF ! check volume option

C     Print out all tracking media
C      CALL GPTMED(0)

c 2005
      RETURN
      END
