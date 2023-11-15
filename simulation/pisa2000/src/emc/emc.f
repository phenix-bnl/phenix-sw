*+PATCH,//EMCFORTR/EMCFORTR
*+DECK,EMC.
**CMZ :          03/03/98  10.17.32  by  Henner Buesching
**CMZ :  2.04/00 24/10/94  14.27.39  by  Charles F. Maguire
**CMZ :  2.03/00 26/05/93  10.13.06  by  H. Kehayias
**CMZ :  2.01/00 19/05/93  22.32.02  by  Charles F. Maguire
*-- Author :
*-- Author :
c*******************************************************************

      subroutine EMC(FULL,NH)
      implicit none

#include "g77trigdef.inc"


c     Put all geometry parameters into namelist and phnx.par
c     eliminated "pbgl_west_arm" option
c     implemented "CTRK" switch for generation of Cherenkov photons
c     in lead glass
c     G. David, August 1999

*HeB  Henner Buesching
*     changed Pbgl geometry and added Pbgl supporting frame
*     fixed small bugs concerning Pbgl

c     C.F. Maguire:
c        5/7/92 Re-Work of EMC Lead-Gas version with F. Obenshain
c        8/1/92 Update to reflect new design of the EMCal group
c        9/20/92 Update to be compatible with PISA Release 2

c        4/6/93 Update to include high resolution crystals, change
c               structure of cells to much more realistic one,
c               tie geometry to distances rather
c               than angles   (G. David)

#include "gcflag.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpelink.inc"
#include "gccuts.inc"
#include "emcbit.inc"


#include "gctrak.inc"

c     guphnx.inc has cvolu_opt in order to access cvolu_opt for tracking thr.

#include "guphnx.inc"

cMM
c     include common block for rotation matrix values
c     in 'emc_pbgl_coord.f'
cMM
#include "emc_coord.inc"
cMM

c     April 25, 1993  G. David

c     Watch for new PHNX.PAR file!

c     Update: December 27, 1993,  G. David

c     Rewritten for new geometry (octants) and better lead glass
c     geometry code: Dec. 93, G. David
c     This code is completely incompatible with the earlier version;
c     also mixing the new geometry with the old DIGI routine may
c     cause crashes.  Comments between lines of code are updated.

c     Overhaul in October 1996, G. David.  In addition to many
c     changes in the geometry the entire EMCal PISA philosophy is changed
c     in order to get in line with the rest of PHENIX.  This involves
c     among other things changes in variables passed in UDETPAR, etc.
c     If you want to go back to the previous versions, use
c     EMCOLD -- EMC_DIGI  (and EMC_GINI)
c     However, this version uses
c     EMC -- EMC_HITS

c     ********************************

c     The PHENIX EMCal consists of up to 8 "walls" (or "octants").
c     Minimum 1, maximum 6 of them is Shish-Kebab; the number of
c     Shish-Kebab walls is determined by the (PHNX.PAR) parameter
c     EMC_WALLS.  Note that you have to define at least one
c     Shish-Kebab wall.

c     >>>>>>> Since 1995 the official word for "wall" is SECTOR ************

c     The last 2 sectors (7 and 8) are lead glass, and they get
c     defined whenever bit 2 of EMC_OPT (see below) is set.
c     That means that you can still have lead glass even if
c     EMC_WALLS = 1, i.e. only one Shish-Kebab is defined

c     EMC_OPT is the EMCal option (subdetectors required) bit pattern:
c     1: Shish-Kebab, 2: PbGl


c     Shish-Kebab:

c     basic unit will be a BOX with a square entrance face
c        given by CELL_SIZE * CELL_SIZE
c     It will be referred to as 'ECn1' (Emcal Cell n 1), where
c     n is the serial number of the sector

c     This box will be of total thickness CELL_EMC_DEPTH,
c     which is the sum of front plastic plate, back steel plate,
c     LEAD_EMC_PLATES converters and LEAD_EMC_PLATES scintillator
c     tiles (plus thin paper on both sides of scint. tiles).
c     In addition there is a light-tight plastic hut in front of
c     all four cells (this covers the fiber loops).

c     12 * 12 of these cells (actually nz_super * nphi_super, obtained
c     from SUPER_NZ_NPHI in PHNX.PAR) are grouped together and called
c     a "supermodule", referred to as 'SCn1' (Super Cell n 1).

c     6 * 3 of these supermodules constitutes a "sector", which covers
c     approximately an area of 400.0 * 200.0 cm2. 

c     LEAD GLASS:

c     Basic unit is a BOX ('MOD ', containing one lead glass block
c     plus non-sensitive components).
c     6 x 4 such modules are grouped together as "supermodules".

c*******************************************************************


c*******************************************************************


c     MAIN GEOMETRY parameter file (phnx.par) segment

      integer emc_walls /8/        ! number of sectors, lately (4 in each arm)


      integer super_emc_color /5/       ! super cell color

c     for some simulation purposes it is assumed that the Pb-Scintillator
c     sub-system replaces the PbGlass azimuthal sector

      real rpos_emc /510./        ! initial radial position
      real rpos_emc_add_pbgl/20./ ! To allow for TOF sector
      real emc_deltar_west/0.0/
      real emc_deltaz_west/0.0/
      real emc_deltar_east/0.0/
      real emc_deltaz_east/0.0/
      real emc_phi_sec1,emc_phi_sec2,emc_phi_sec3,emc_phi_sec4,
     1           emc_phi_sec5,emc_phi_sec6,emc_phi_sec7,emc_phi_sec8

      real cell_size /5.542/       ! 5.539 cm cell size, Oct 1996, AUTOCAD
      integer     emc_opt/3/       ! Define PbSc and PbGl (bit 1 and 2)
      integer     emc_debug/0/

cgd***      integer steel_emc_med/5/          ! Medium of back steel plate
cgd***      integer fplate_emc_med/923/          ! Medium of front plastic
cgd***      integer hut_emc_med/923/          ! Medium of front plastic hut
cgd***      integer vacuum_emc_med/6/         ! Take AIR now
cgd***      integer lead_emc_med/9/           ! Lead...
cgd***      integer paper_emc_med/923/        ! Refl. paper on tiles (take plastic!)
cgd***      integer scint_emc_med/98/         ! Scintillating material
cgd***      integer fiber_emc_med/923/        ! WLS fiber material - plastic for now

c     Updated materials now defined in emc_trk_media

cgd***      integer steel_emc_med/984/          ! Medium of back steel plate
cgd***      integer fplate_emc_med/986/          ! Medium of front plastic
cgd***      integer hut_emc_med/986/          ! Medium of front plastic hut
cgd***      integer vacuum_emc_med/6/         ! Take AIR now
cgd***      integer lead_emc_med/983/           ! Lead...
cgd***      integer paper_emc_med/985/        ! Refl. paper on tiles (take plastic!)
cgd***      integer scint_emc_med/981/         ! Scintillating material
cgd***      integer fiber_emc_med/982/        ! WLS fiber material - plastic for now

c     Material numbers changed to 8xx to comply with PISA standards
c     March 28, 1998, GD

      integer steel_emc_med/884/          ! Medium of back steel plate
      integer fplate_emc_med/886/          ! Medium of front plastic
      integer hut_emc_med/886/          ! Medium of front plastic hut
      integer vacuum_emc_med/6/         ! Take AIR now
      integer lead_emc_med/883/           ! Lead...
      integer paper_emc_med/885/        ! Refl. paper on tiles (take plastic!)
      integer scint_emc_med/881/         ! Scintillating material
      integer fiber_emc_med/882/        ! WLS fiber material - plastic for now

      real    smskin_emc_thck/0.05/     ! Steel around supermodule
      real    clearance_emc_module/0.03/ ! Clearance between modules
      real    clearance_emc_supermodule/0.08/ ! Clearance between modules
      real    skin_emc_thck/0.012/        ! Thickness of tack-welded "skin"
      real    paper_emc_thck/0.012/      ! ... of paper (0.12 mm)
      real    fplate_emc_thck/0.5/     ! ... of plastic in front (5 mm)
      real    hut_emc_thck/0.2/        ! ... of lighttight hut in front
      real    hut_emc_height/6.5/      ! ... of lighttight hut in front
      real    bsteel_emc_thck/0.1/      ! ... of steel behind
      real    scint_emc_thck/0.4/       ! ... of the scintillator
      real    lead_emc_thck/0.15/       ! ... of the lead absorber
      real    cage_emc_thck/1.0/        ! ... of the structure around sectors
      real    fiber_emc_dia/0.1/        ! Fiber diameter
c                                       ! of the risetime
      integer lead_emc_plates/65/       ! Number of lead plates
      integer scint_emc_plates/66/      ! Number of scint. plates

      integer seen_emc_cage/1/
      integer seen_emc_cell/1/          ! Seen and color params in GSATT
      integer seen_emc_hut/1/
      integer seen_emc_layers/0/        ! calls
      integer cage_emc_color/7/
      integer cell_emc_color/2/
      integer hut_emc_color/3/
      integer steel_emc_color/4/
      integer fiber_emc_color/5/
      integer lead_emc_color/6/
      integer paper_emc_color/1/
      integer scint_emc_color/7/

      real    emc_cutgam/0.001/
      real    emc_cutele/0.001/
      real    emc_cutneu/0.001/
      real    emc_cuthad/0.001/
      real    emc_cutmuo/0.001/

      real    emc_response_option/0.0/




c     End of Lead Scintillator (PbSc) geometry parameters

c*******************************************************************

c     When calculating the impact position you have to add the
c     depth if the insensitive material in front (but already
c     inside the detector volume)

      real rpos_emc_plus_pbsc
      real rpos_emc_plus_pbgl



c       Get carriage translations and pass them via udetpar

        INTEGER  arm_shift
        REAL     east_shift(3),west_shift(3)
cgd***updateS
c     PbGl variables for new code of Arne Claussen, Dec. 13, 1993
cgd***+DECK,SUPGEO.
cgd****CMZ :          26/07/93  15.01.28  by  Arne Claussen
cgd****-- Author :    Arne Claussen (01/07/1992)
cgd***            include 'DETVOL.
 
C     define general variables

      INTEGER YS, MODNR, zs
 
C     this are the measures for a supermodule
 
      REAL*4 SUPPR(3),SUPPS(3)
                                              
C     this are the measures for the silicon layer between two supermodules
 
      REAL SILPR(3), SILPS(3)
C     this are the measures for a module
 
      REAL MODPR(3), MODPS(3)

C     measures of the pb glass block, position relative to the module mother
C     volume
 
      REAL PBGPR(3), PBGPS(3)
 
C     measures of the fiber plate
 
      REAL FIBPR(3), FIBPS(3)

C     measures of the PM-housing
 
      REAL PMPR(3), PMPS(3)

C     measures of the mylar layers (vertically and horizontically)
 
      REAL MYHPR(3), MYHPSO(3), MYHPSU(3)
      REAL MYVPR(3), MYVPSR(3), MYVPSL(3)

 
C     measures of the shrink tube material (vertically and horizontically)
 
      REAL SSHPR(3), SSHPSO(3), SSHPSU(3)
      REAL SSVPR(3), SSVPSR(3), SSVPSL(3)

 
C     measures of the epoxid material (vertically and horizontically)
 
      REAL EPHPR(3), EPHPSO(3), EPHPSU(3)
      REAL EPVPR(3), EPVPSR(3), EPVPSL(3)

 
C     measures of the carbon fiber layers between and around the modules
 
      REAL CFHPR(3), CFHPS(3)
      REAL CFVPR(3), CFVPS(3)

 
*HeB C   measures for the aluminium side plates (horizontally and vertically)
*HeB  its not aluminium but steel
 
      REAL SPHPR(3), SPHPS(3)
      REAL SPVPR(3), SPVPS(3)

 
*HeB C   measures of the "Neusilber" back plate
*HeB does not exist any more
 
*HeB      REAL BPPR(3), BPPS(3)

 
C     measures for the hut mother volume
 
      REAL HUTPR(3), HUTPS(3)

 
C     measures for the hut bottom parts (positions relative to the hut
C     mother volume); B = bottom, X/Y = main direction X/Y
 
      REAL HBXPR(3), HBXPSO(3), HBXPSU(3)
      REAL HBYPR(3), HBYPSR(3), HBYPSL(3)

 
C     measures for the hut top part (positions relative to the hut
C     mother volume); T = top
 
      REAL HTPR(3), HTPS(3)

 
C     measures for the hut trapezoid parts (positions relative to the
C     hut mother volume); Z = trapezoid, X/Y = main direction X/Y
 
      REAL HZXPR(4), HZXPSO(3), HZXPSU(3)
      REAL HZYPR(4), HZYPSR(3), HZYPSL(3)


*HeB Supporting Frame for PbGl

      REAL PBFRAMEPR(11)
      REAL PBFRAMEAPR(3)
cgd***      REAL PBFRAMEBPR(3)
      REAL PBFRAMECPR(3)
      REAL PBFRAMEDPR(3)
      REAL PBFRAMEEPR(3)
      REAL PBFRAMEFPR(11)
      REAL PBFRAMEGPR(3)
      REAL PBFRAMEHPR(11)


       

*HeB  INTEGER vid(50)        ! Added, hopefully temporarily, GD Dec13,1993

c     local variables
      REAL ra_thr(10)/10*0.0/

      real x_sens, y_sens, z_sens
      real boxsupr_par(3)
      real x_super, y_super, z_super, z_start, y_start
      real zc_start,yc_start
      real facte(11)
      integer nzsegm, nphisegm, iz, iphi, i
      integer nz_pbgl/16/           ! Supermodules in z
      integer nphi_pbgl/12/         ! Supermodules in phi (y)
      integer itype, iwall
      integer IOPARA, IOPARU        ! new for release 2 ZEBRA

c     save parameters for the DIGI routine

      integer nudetpar
      parameter (nudetpar = 80)
#include "emcvar.inc"
      real udetpar(nudetpar)


c     Defining parameters for cells and supercells for each sector
c     and calorimeter type (max 8 sectors, types: Shish-Kebab, PbGlass)

c        udetpar(1) = float(emc_walls)     ! number of walls from PHNX.PAR
c        udetpar(2) = float(emc_opt)       ! EMCal option from PHNX.PAR
c        udetpar(3) = float(iwall)        ! wall number (1-8)
c        udetpar(4) = float(itype)        ! subdetector type
c                                         ! (Sh-K:1 PbGl:2)
c        udetpar(5) = angle               ! phi angle of wall center
c        udetpar(6) = rpos_emc                ! radial position of wall center
c        udetpar(7) = z_start             ! center of first cell, z coor.
c        udetpar(8) = y_start             ! center of first cell, y coor.
c        udetpar(9) = lsiz                ! long. size of a cell
c        udetpar(10) = tsiz               ! transverse size of a cell
c        udetpar(11) = FLOAT(no_modz)     ! No. of cells in z in a supermod.
c        udetpar(12) = FLOAT(no_mody)     ! No. of cells in y in a supermod.
c        udetpar(13) = FLOAT(no_smodz)    ! No. of supermods. in z / sector
c        udetpar(14) = FLOAT(no_smody)    ! No. of supermods. in y / sector
c        udetpar(15) = 0.0                ! 
c        udetpar(16) = 0.0                !
c        udetpar(17) = 0.0                !
c        udetpar(18) = FLOAT(scint_emc_med)       ! Sensitive medium
c        udetpar(19) = 0.0                !
c        udetpar(20) = 0.0                ! 
c        udetpar(21) = 0.0                ! 
c        udetpar(22) = FLOAT(emc_debug)   ! Debug info option
c        udetpar(23) = gcuts(1)           ! CUTGAM
c        udetpar(24) = gcuts(2)           ! CUTELE
c        udetpar(25) = gcuts(3)           ! CUTNEU
c        udetpar(26) = gcuts(4)           ! CUTHAD
c        udetpar(27) = gcuts(5)           ! CUTMUO
c        udetpar(28) = 0.0                !
c        udetpar(29) = 0.0                !
c        udetpar(30) = emc_r_min_sc       ! bitp lower limit, PbSc
c        udetpar(31) = emc_r_max_sc       ! bitp upper limit, PbSc
c        udetpar(32) = emc_r_step         ! bitp stepsize, PbSc
c        udetpar(33) = emc_z_min          ! bitp lower limit
c        udetpar(34) = emc_z_max          ! bitp upper limit
c        udetpar(35) = emc_z_step         ! bitp stepsize
c        udetpar(36) = emc_x_min_sc       ! bitp lower limit, PbSc
c        udetpar(37) = emc_x_max_sc       ! bitp upper limit, PbSc
c        udetpar(38) = emc_x_step         ! bitp stepsize, PbSc
c        udetpar(39) = 0.0                !
c        udetpar(40) = emc_dele_max_sc    ! bitp dE upper limit, PbSc
c        udetpar(41) = emc_dele_step_sc   ! bitp dE upper limit, PbSc
c        udetpar(42) = emc_tof_min        ! bitp lower limit
c        udetpar(43) = emc_tof_max        ! bitp upper limit
c        udetpar(44) = emc_tof_step       ! bitp stepsize
c        udetpar(45) = 0.0                !
c        udetpar(46) = 0.0                !
c        udetpar(47) = 0.0                !
c        udetpar(48) = 0.0                !
c        udetpar(49) = 0.0                !
c        udetpar(50) = FLOAT(emc_ind1_max_sc)    ! bitp tower ind. 
c        udetpar(51) = FLOAT(emc_ind2_max_sc)    ! bitp tower ind. 
c        udetpar(52) = FLOAT(emc_iwall_max)      ! 
c        udetpar(53) = FLOAT(emc_itype_max)      ! 
c        udetpar(54) = FLOAT(emc_i1_max)         ! 
c        udetpar(55) = 0.0                !
c        udetpar(56) = 0.0                !
c        udetpar(57) = 0.0                !
c        udetpar(58) = 0.0                !
c        udetpar(59) = 0.0                !
c        udetpar(60) = FLOAT(emc_itrack_max)    ! 
c        udetpar(61) = FLOAT(emc_spart_max)     ! 
c        udetpar(62) = FLOAT(emc_ncycle_max)    ! 
c        udetpar(63) = 0.0                !
c        udetpar(64) = 0.0                !
c        udetpar(65) = emc_cutgam         ! 
c        udetpar(66) = emc_cutele         ! 
c        udetpar(67) = emc_cutneu         ! 
c        udetpar(68) = emc_cuthad         ! 
c        udetpar(69) = emc_cutmuo         ! 
c        udetpar(70) = 0.0                !
c        udetpar(71) = ra_thr(1)          ! Spec. thresh. for mat 981
c        udetpar(71) = ra_thr(2)          ! Spec. thresh. for mat 982
c        udetpar(71) = ra_thr(3)          ! Spec. thresh. for mat 983
c        udetpar(71) = ra_thr(4)          ! Spec. thresh. for mat 984
c        udetpar(71) = ra_thr(5)          ! Spec. thresh. for mat 985
c        udetpar(71) = ra_thr(6)          ! Spec. thresh. for mat 986
c        udetpar(77) = 0.0                !
c        udetpar(78) = 0.0                !
c        udetpar(79) = 0.0                !
c        udetpar(80) = 0.0                ! EMC response option

      character*4 v_m_name,v_i_name,set_id,namesv(2)
      character*4 v_s_name
      character*4 v_e_name/'SCON'/
      CHARACTER*4 v_supv_name,v_suph_name  ! Support horizontal, vertical

      integer  nr,mr,npar,ivolu,inull,nv,idtype, nbitsv(2),
     1  nwpa,nwsa,iset,idet
      Data inull/0/

      integer*4 nh                   !set before call in gugeom
      character*4 full         ! set before call in gugeom



c--------------------------------------------------

c     Include new variables 4/6/93 G. David

      character*4 v_x_name

c     Variables connected to setup options

      logical l_shkb/.false./
      logical l_pbgl/.false./
cgd***      logical l_pbgl_west_arm/.false./

c      integer i_smod_wall/1/            ! Get from PHNX.PAR (cell in smod)

c     Variables for Shish-Kebab


c     Variables for lead glass

cgd***      integer pbgl_emc_med/910/         ! Lead glass medium
      integer pbgl_emc_med/810/         ! Lead glass medium
c      integer plastic_emc_med/902/      ! Plastic wrapping medium (old number)
cgd***      integer plastic_emc_med/986/      ! Plastic wrapping medium
      integer plastic_emc_med/886/      ! Plastic wrapping medium
      integer seen_emc_pbgl/1/
      integer pbgl_emc_color/6/
      integer pbgl_nz                   ! No=umber of blocks in z direction


c     All sorts of other stuff


      integer kr,zr
      integer i_rotate,irot_array(8)   ! Rotation matrices for sectors
      integer i_rot_hut_pbgl(4)	!HeB Rotation matrices for pbgl hut
      integer irot_1
      integer nmed
      integer j,k

      real    lsiz,tsiz
      integer no_modz,no_mody,no_smodz,no_smody
      real    posx,posy,posz
      real    em_tsiz
      real    em_cell(3)
      real    em_sheet(3)
      real    cell_emc_depth
      real    r_wally,r_wallx,r_angle(8),angle,r_viewing_angle

      integer super_nz_nphi(2) /12, 12/ ! supermodule arrangement
      integer nz_super, nphi_super
      equivalence (nz_super,super_nz_nphi(1))
      equivalence (nphi_super,super_nz_nphi(2))

cgd***  Add global arm-translation variables May 27, 2002 GD
      real    emc_westarm_deltay/0.0/,emc_westarm_deltaz/0.0/
      real    emc_eastarm_deltay/0.0/,emc_eastarm_deltaz/0.0/

c     End of all declarations
cgd--------------------------------

c     Namelist transferred here, G. David, June 29, 2001

      namelist /hall_par/ arm_shift,east_shift,west_shift

      namelist /emc_par/ emc_opt,emc_walls,
     1            emc_debug,
     2            rpos_emc, rpos_emc_add_pbgl,
     2      emc_deltar_west,emc_deltaz_west,
     2      emc_deltar_east,emc_deltaz_east,
     2      emc_phi_sec1,emc_phi_sec2,emc_phi_sec3,emc_phi_sec4,
     2           emc_phi_sec5,emc_phi_sec6,emc_phi_sec7,emc_phi_sec8,
     3            super_nz_nphi,pbgl_nz,
     4            pbgl_emc_med,scint_emc_med,fiber_emc_med,
     5            cell_size,lead_emc_plates,scint_emc_plates,
     5            smskin_emc_thck,skin_emc_thck,
     5            paper_emc_thck,fplate_emc_thck,hut_emc_thck,
     5            hut_emc_height,bsteel_emc_thck,
     5            scint_emc_thck,lead_emc_thck,cage_emc_thck,
     6            clearance_emc_module,
     6            clearance_emc_supermodule,
     7            seen_emc_cell,seen_emc_layers,seen_emc_hut,
     7            seen_emc_cage,cage_emc_color,
     7            super_emc_color,
     7            cell_emc_color,
     7            seen_emc_pbgl,pbgl_emc_color,
     8            emc_cutgam,emc_cutele,emc_cuthad,emc_cutneu,
     8            emc_cutmuo,
     8            emc_westarm_deltay,emc_westarm_deltaz,
     8            emc_eastarm_deltay,emc_eastarm_deltaz,

c     Stuff for bit-packing

     1		emc_r_min_sc,emc_r_max_sc,emc_r_step,
     1		emc_r_min_gl,emc_r_max_gl,
     1		emc_z_min,emc_z_max,emc_z_step,
     1		emc_x_min_sc,emc_x_max_sc,emc_x_step,
     1		emc_x_min_gl,emc_x_max_gl,
     1		emc_dele_max_sc,emc_dele_step_sc,
     1		emc_dele_max_gl,emc_dele_step_gl,
     1		emc_tof_min,emc_tof_max,emc_tof_step,
     1		emc_iwall_max,emc_itype_max,emc_i1_max,
     1		emc_ind1_max_sc,emc_ind2_max_sc,
     1		emc_ind1_max_gl,emc_ind2_max_gl,
     1		emc_itrack_max,emc_spart_max,emc_ncycle_max



cgd***  ALL DATA statements trasferred here, G. David, June 29, 2001
        DATA     arm_shift/0/,east_shift/3*0.0/,west_shift/3*0.0/



c     These variables used in bit packing (emc_bitp.f) are defined
c     in the INCLUDE file emcbit.inc

c	Radii (min, max)

cgd***	DATA	emc_r_min_sc/510.0/,emc_r_max_sc/560.0/,
cgd***     1     emc_r_step/0.05/
cgd***	DATA	emc_r_min_gl/565.0/,emc_r_max_gl/615.0/
cgd***	DATA	emc_r_min_gl/543.0/,emc_r_max_gl/593.0/
	DATA	emc_r_min_sc/510.0/,emc_r_max_sc/580.0/,
     1     emc_r_step/0.07/
	DATA	emc_r_min_gl/535.0/,emc_r_max_gl/605.0/

c	z limits (min, max)

	DATA	emc_z_min/-200.0/,emc_z_max/200.0/,emc_z_step/0.1/

c	x limits (min, max)

	DATA	emc_x_min_sc/280.0/,emc_x_max_sc/560.0/,
     1       emc_x_step/0.07/
cgd***	DATA	emc_x_min_gl/490.0/,emc_x_max_gl/610.0/
	DATA	emc_x_min_gl/460.0/,emc_x_max_gl/740.0/

c	Delta E (max)

	DATA	emc_dele_max_sc/0.0327/,emc_dele_step_sc/1.0e-6/
	DATA	emc_dele_max_gl/0.256/,emc_dele_step_gl/8.0e-6/

c	TOF (min, max)

	DATA	emc_tof_min/17.0/,emc_tof_max/345.0/,
     1       emc_tof_step/0.010/

c	Sectors, detector types

	DATA emc_iwall_max/8/,emc_itype_max/3/,emc_i1_max/8/

c	Indices within supermodule

	DATA emc_ind1_max_sc/18/,emc_ind2_max_sc/144/
	DATA emc_ind1_max_gl/192/,emc_ind2_max_gl/24/

c	Misc

	DATA emc_itrack_max/32767/,emc_spart_max/62/,
     1       emc_ncycle_max/31/



*HeB      DATA SUPPR / 60.0, 8.247, 12.312/
      DATA SUPPR / 47.04, 8.221, 12.312/
      DATA SUPPS / 0.0, 0.0, 0.0/

      DATA SILPR / 20.0, 0.025, 12.312/
*HeB      DATA SILPS /  -4.4, -8.272, 0.0  /
      DATA SILPS /  -15.04, -8.196, 0.0  /
 
*HeB      DATA MODPR / 25.6,  2.043, 2.052/
      DATA MODPR / 25.6,  2.034, 2.048/
 
      DATA PBGPR / 20.000, 2.000, 2.000/
      DATA PBGPS / -4.4, 0.0, 0.0 /
      DATA FIBPR / 0.600, 1.900, 1.900/
      DATA FIBPS / -25.0, 0.0, 0.0/
 
*HeB      DATA PMPR / 5.0, 1.9, 1.9/
      DATA PMPR / 5.0, 2.0, 2.0/
      DATA PMPS / 20.6, 0.0, 0.0/
 
      DATA MYHPR / 20.00, 0.001, 2.002/
      DATA MYHPSO / -4.4, 2.001, 0.0/
      DATA MYHPSU / -4.4, -2.001, 0.0/
*HeB      DATA MYVPR / 20.00, 2.001, 0.001/
      DATA MYVPR / 20.00, 2.000, 0.001/
      DATA MYVPSR / -4.4, 0.0, 2.001/
      DATA MYVPSL / -4.4, 0.0, -2.001/
*HeB      DATA SSHPR / 20.0, 0.011, 2.024/
*HeB      DATA SSHPSO / -4.4, 2.013, 0.0/
*HeB      DATA SSHPSU / -4.4, -2.013, 0.0/
*HeB      DATA SSVPR / 20.0, 2.013, 0.011/
*HeB      DATA SSVPSR / -4.4, 0.0, 2.013/
*HeB      DATA SSVPSL / -4.4, 0.0, -2.013/
      DATA SSHPR / 20.0, 0.007, 2.016/
      DATA SSHPSO / -4.4, 2.009, 0.0/
      DATA SSHPSU / -4.4, -2.009, 0.0/
      DATA SSVPR / 20.0, 2.002, 0.007/
      DATA SSVPSR / -4.4, 0.0, 2.009/
      DATA SSVPSL / -4.4, 0.0, -2.009/
*HeB      DATA EPHPR / 20.0, 0.0095, 2.052/
*HeB      DATA EPHPSO / -4.4, 2.0335, 0.0/
*HeB      DATA EPHPSU / -4.4, -2.0335, 0.0/
*HeB      DATA EPVPR / 20.0, 2.033, 0.014/
*HeB      DATA EPVPSR / -4.4, 0.0, 2.038/
*HeB      DATA EPVPSL / -4.4, 0.0, -2.038/
      DATA EPHPR / 20.0, 0.009, 2.048/
      DATA EPHPSO / -4.4, 2.025, 0.0/
      DATA EPHPSU / -4.4, -2.025, 0.0/
      DATA EPVPR / 20.0, 2.016, 0.016/
      DATA EPVPSR / -4.4, 0.0, 2.032/
      DATA EPVPSL / -4.4, 0.0, -2.032/
*HeB      DATA CFHPR /20.0,  0.012, 12.282/
*HeB      DATA CFVPR / 20.0, 8.247, 0.012/
      DATA CFHPR /20.0,  0.012, 12.288/
      DATA CFVPR / 20.0, 8.196, 0.012/
*HeB      DATA SPHPR /21.0, 0.04, 12.312/
*HeB      DATA SPVPR / 21.0, 8.207, 0.04/
      DATA SPHPR /19.5, 0.012, 12.312/
      DATA SPVPR / 19.5, 8.172, 0.012/
*HeB      DATA BPPR / 0.04, 8.247, 12.312/
*HeB      DATA BPPS /  57.64, 0.0, 0.0/
*HeB      DATA HUTPR / 6.0, 8.0, 12.0/
*HeB      DATA HUTPS / -31.6, 0.0, 0.0/
      DATA HUTPR / 6.0, 8.196, 12.312/
      DATA HUTPS / -41.04, 0.025, 0.0/
*HeB      DATA HBXPR / 0.12, 0.25, 11.5/
*HeB      DATA HBXPSO / 5.88, 7.75, 0.0/
*HeB      DATA HBXPSU / 5.88,-7.75, 0.0/
*HeB      DATA HBYPR / 0.12, 8.0, 0.25/
*HeB      DATA HBYPSR / 5.88, 0.0, 11.75/
*HeB      DATA HBYPSL /5.88, 0.0, -11.75/

      DATA HBXPR / 0.12, 0.186, 12.086/
      DATA HBXPSO / 5.88, 8.01, 0.0/
      DATA HBXPSU / 5.88,-8.01, 0.0/
      DATA HBYPR / 0.12, 8.196, 0.113/
      DATA HBYPSR / 5.88, 0.0, 12.199/
      DATA HBYPSL /5.88, 0.0, -12.199/
      DATA HTPR / 0.12, 4.30, 4.25/
      DATA HTPS / -5.88, 0.0, 0.0/
*HeB      DATA HZXPR / 4.25, 11.5, 0.12, 5.978/
*HeB      DATA HZXPSO / 0.00, 5.90, 0.00/
*HeB      DATA HZXPSU / 0.00,-5.90, 0.00/
*HeB      DATA HZYPR / 4.30, 7.50, 0.12, 6.709/
*HeB      DATA HZYPSR / 0.0, 0.00, 7.875/
*HeB      DATA HZYPSL /0.0, 0.00, -7.875/

      DATA HZXPR / 4.25, 12.085, 0.12, 6.253/
      DATA HZXPSO / 0.00, 6.062, 0.00/
      DATA HZXPSU / 0.00, -6.062, 0.00/
      DATA HZYPR / 4.30, 7.823, 0.12, 7.166/
      DATA HZYPSR / 0.0, 0.00, -8.167/
      DATA HZYPSL / 0.0, 0.00, 8.167/
      DATA PBFRAMEPR / 206.192,0.,0.,53.,100.2695,120.978,355.85,
     +          53.,100.2695,120.978,355.85/
      DATA PBFRAMEAPR / 1.1, 45., 205.692/
      DATA PBFRAMECPR / 1.5, 53., 196.992/
      DATA PBFRAMEDPR / 1.5,54.87, 196.992/
      DATA PBFRAMEEPR / 17.25,45., 4.35/
      DATA PBFRAMEFPR / 0.5,0.,0.,45.,82.752,
     + 88.2775,3.51,45.,82.752,88.2775,3.51/
      DATA PBFRAMEGPR / 64.402, 45., 1.1/
      DATA PBFRAMEHPR/4.85,0.,0.,45.,20.64,
     + 32.701,352.37,45.,20.64,32.701,352.37/
     
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun


c---------------------------------------------------

*HeB added call for special media

c     Define "new" materials for PbSc as well (Sep 27, 1997, GD)

      CALL EMC_TRK_MEDIA


c     Add option to generate Cherenkov photons in PbGl
c     Aug. 6, 1999  G. David / H. Buesching

      emc_response_option = 0.0     ! Default, depE

cgd***      CALL GSTMED(810,'TF1$',810,1,0,0.,10.,0.1,0.02,0.1,5.E-4,0,0)

cgd***  Had to throw this out; 'CTRK' will work only for 'AUAU' for now.
c       The reason is hit overflow; the technique of setting higher
c       thresholds doesn't seem to work if applied with letting GEANT
c       do the Cherenkov photons
c       Gabor David, Dec. 19, 1999
c     code commented out
cgd***      IF(cvolu_opt(6,8).eq.'CTRK') THEN
cgd***       CALL EMC_CKOVMAT
cgd***       emc_response_option = 1.0
cgd***      ENDIF
c     code replacing it
      IF(cvolu_opt(5,8).eq.'PPLO'
     1      .or.cvolu_opt(5,8).eq.'PPHI'
     1      .or.cvolu_opt(5,8).eq.'PPME') THEN
       cvolu_opt(6,8) = '    '
       CONTINUE
      ELSE
       IF(cvolu_opt(6,8).eq.'CTRK') THEN
        CALL EMC_CKOVMAT

        write(*,*) 'emc.f: emc-ckovmat called'

cMM set special parameters (22.08.2000)

cMM   PbGl parameters

        call gstpar(810,'CUTGAM',0.001)
        call gstpar(810,'CUTELE',0.0001)
        call gstpar(810,'CUTNEU',0.001)
        call gstpar(810,'CUTHAD',0.001)
        call gstpar(810,'CUTMUO',0.001)
        call gstpar(810,'BCUTE',0.001)
        call gstpar(810,'BCUTM',0.001)
        call gstpar(810,'DCUTE',0.001)
        call gstpar(810,'DCUTM',0.001)
        call gstpar(810,'PPCUTM',0.001)

        call gstpar(810,'PFIS',1.)
        call gstpar(810,'MUNU',0.)
        call gstpar(810,'DCAY',1.)
        call gstpar(810,'LABS',1.)

cMM   Mylar parameters

        call gstpar(820,'CUTGAM',0.001)
        call gstpar(820,'CUTELE',0.0001)
        call gstpar(820,'CUTNEU',0.001)
        call gstpar(820,'CUTHAD',0.001)
        call gstpar(820,'CUTMUO',0.001)
        call gstpar(820,'BCUTE',0.001)
        call gstpar(820,'BCUTM',0.001)
        call gstpar(820,'DCUTE',0.001)
        call gstpar(820,'DCUTM',0.001)
        call gstpar(820,'PPCUTM',0.001)

        call gstpar(820,'PFIS',1.)
        call gstpar(820,'MUNU',0.)
        call gstpar(820,'DCAY',1.)
        call gstpar(820,'LABS',1.)

cMM   PVC parameters

        call gstpar(821,'CUTGAM',0.001)
        call gstpar(821,'CUTELE',0.0001)
        call gstpar(821,'CUTNEU',0.001)
        call gstpar(821,'CUTHAD',0.001)
        call gstpar(821,'CUTMUO',0.001)
        call gstpar(821,'BCUTE',0.001)
        call gstpar(821,'BCUTM',0.001)
        call gstpar(821,'DCUTE',0.001)
        call gstpar(821,'DCUTM',0.001)
        call gstpar(821,'PPCUTM',0.001)

        call gstpar(821,'PFIS',1.)
        call gstpar(821,'MUNU',0.)
        call gstpar(821,'DCAY',1.)
        call gstpar(821,'LABS',1.)

        write(*,*) 'emc.f: special parameters set'
cMM

        emc_response_option = 1.0
       ENDIF
      ENDIF
c     End of code manipulation, Dec, 19, 1999


c     This section redefines the tracking thresholds, hopefully
c     w.r.t. the running conditions

c     Write defaults without referring to cvolu_opt(5,8)

      ra_thr(1) = 0.0001    ! Scintillator
      ra_thr(2) = 0.0001    ! WLS
      ra_thr(3) = 0.0001    ! lead
      ra_thr(4) = 0.0001    ! steel
      ra_thr(5) = 0.0001    ! Tyvek
      ra_thr(6) = 0.0001    ! plastic 1

      IF(cvolu_opt(5,8).eq.'AUAU') THEN

c        Thresholds are default, already properly set

          GOTO 896
      ENDIF
      IF(cvolu_opt(5,8).eq.'AUHI') THEN
       DO i = 1,6
        ra_thr(i) = 0.001
       ENDDO
       GOTO 896
      ENDIF
      IF(cvolu_opt(5,8).eq.'PPLO') THEN
       DO i = 1,6
        ra_thr(i) = 0.003
       ENDDO
       GOTO 896
      ENDIF
      IF(cvolu_opt(5,8).eq.'PPME') THEN
       DO i = 1,6
        ra_thr(i) = 0.005
       ENDDO
       GOTO 896
      ENDIF
      IF(cvolu_opt(5,8).eq.'PPHI') THEN
       DO i = 1,6
        ra_thr(i) = 0.01
       ENDDO
      ENDIF

 896  CONTINUE
      DO i = 1,6
cgd***       CALL GSTPAR(980+i,'CUTELE',ra_thr(i))
cgd***       CALL GSTPAR(980+i,'CUTGAM',ra_thr(i))
cgd***       CALL GSTPAR(980+i,'BCUTE',ra_thr(i))
       CALL GSTPAR(880+i,'CUTELE',ra_thr(i))
       CALL GSTPAR(880+i,'CUTGAM',ra_thr(i))
       CALL GSTPAR(880+i,'BCUTE',ra_thr(i))
      ENDDO
cgd***
c     Unfortunately we have to play the same game with PbGl
c     Figure out if this distorts linearity!!!  Dec 17, 1999 G. David
      IF(cvolu_opt(5,8).eq.'PPLO'.or.cvolu_opt(5,8).eq.'PPME'
     1      .or.cvolu_opt(5,8).eq.'PPHI') THEN
c       WRITE(*,*) ra_thr(1)
       CALL GSTPAR(810,'CUTELE',ra_thr(1))
       CALL GSTPAR(810,'CUTGAM',ra_thr(1))
       CALL GSTPAR(810,'BCUTE',ra_thr(1))
      ENDIF


c     Read the geometery file segment

     
      write( *,* ) 'emc - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = emc_par, err = 999 )

c     Insert possibility of translation May 8, 2000 G. David
c     Read the carriage translation geometery file segment

      rewind(itf_lun)
      read( itf_lun, nml = hall_par, err = 999 )

c     Defend yourself against user laziness: if arm_shift is 0,
c     there should be NO translation
      IF(arm_shift.eq.0) THEN
       DO i=1,3
        east_shift(i) = 0.0
        west_shift(i) = 0.0
       ENDDO
      ENDIF

cMM
c     get shift values for east arm
cMM
      emc_rpos = rpos_emc
      emc_dtof = rpos_emc_add_pbgl

      x_shift = east_shift(1)
      y_shift = east_shift(2)
      z_shift = east_shift(3)
cMM

c     Get EMCal option


      l_shkb = btest(emc_opt,0)
      l_pbgl = btest(emc_opt,1)

c     Geometry parameters are read. Now store it in 'EPRA' bank

c     BOOK 'EPRA' bank

      call MZFORM('PARA',     !bank name
     $              '-F',       !all data in the bank is Float
     $              IOPARA)
      call MZBOOK(
     $       ixdiv_fr,          !division
     $       lFE_PARA,          !return address of the bank
     $       lFE_PARA,          !supporting link
     $       1,                 !JB=1 means create top level bank
     $       'PARA',            !bank name
     $       0,                 !NL=0. No links
     $       0,                 !NS=0. No supporting (down) links
     $       EMC_PARA_ND,       !# of data words
     $       IOPARA,            !IO format word
     $       0)                 !NZERO=0 means that whole bank is cleared

c       copy raw geometry parameters into 'EPRA' bank

c       Only a small part of it is filled right now; first zero out
c       everything (05/18/93 GD)

c      DO i = 0,emc_para_nd - 1
      do i = 1,emc_para_nd
         qf(lfe_para + i) = 0.0
      ENDDO

      qf(lFE_PARA + 1 + OFEA_EMC_WALLS)   = float(emc_walls)  ! 1 through 8
      qf(lfe_para + 1 + ofea_emc_opt) = FLOAT(emc_opt)
      qf(lfe_para + 1 + ofea_emc_angle) = 0.0
      qf(lfe_para + 1 + ofea_emc_rpos) = rpos_emc

c-------------------------------------------------------------------

c     now put in geometry into proper mother volume (HALL for now)

      v_m_name = 'EMCL'
      npar = 3       ! BOX section parameter count

c-------------------------------------------------

c     Define building blocks for all three types of electromagnetic
c     calorimeter (if the corresponding bit is set in EMC_OPT)

c-----------------------------

c*********************************************************************

c     Define lead glass blocks if necessary

      IF(.NOT.l_pbgl) GOTO 1912
c     This part of the code changed completely with respect to
c     pisa203/rel2.3 release.  Gabor's original code has been
c     replaced by Arne Claussen's (Muenster) SUPGEO code and
c     the corresponding variables added
c     Some other changes, 12/27/93 G. D.

cgd***+DECK,SUPGEO.
cgd****CMZ :          26/07/93  15.01.28  by  Arne Claussen
cgd****-- Author :    Arne Claussen (01/07/1992)

C*******************************************************************
C*          SUPGEO-Routine for SUPERSIM package
C*******************************************************************

C this routine sets up the geometry for the supermodule and the neightbours

C     first super module

c     Change earlier material 15 to 16 (15 is Cu in PHENIX!)

*HeB  removed all VID(x) => ivolu

      CALL GSVOLU('SUP ','BOX ', 16,SUPPR,3,ivolu)
      CALL GSATT('SUP ','SEEN',0)

c      CALL GSVOLU('MOD ','BOX ', 15,MODPR,3,ivolu)
cgd***      CALL GSVOLU('PBG ','BOX ',910,PBGPR,3,ivolu)
cgd***      CALL GSVOLU('FIB ','BOX ',922,FIBPR,3,ivolu)
cgd***      CALL GSVOLU('PMH ','BOX ',922,PMPR,3,ivolu)
cgd***      CALL GSVOLU('MYH ','BOX ',920,MYHPR,3,ivolu)
cgd***      CALL GSVOLU('MYV ','BOX ',920,MYVPR,3,ivolu)
cgd***      CALL GSVOLU('SSH ','BOX ',921,SSHPR,3,ivolu)
cgd***      CALL GSVOLU('SSV ','BOX ',921,SSVPR,3,ivolu)
cgd***      CALL GSVOLU('EPH ','BOX ',924,EPHPR,3,ivolu)
cgd***      CALL GSVOLU('EPV ','BOX ',924,EPVPR,3,ivolu)

c     Changed material to 8xx - March 28, 1998, GD

      CALL GSVOLU('PBG ','BOX ',810,PBGPR,3,ivolu)
      CALL GSVOLU('FIB ','BOX ',822,FIBPR,3,ivolu)
      CALL GSVOLU('PMH ','BOX ',822,PMPR,3,ivolu)
      CALL GSVOLU('MYH ','BOX ',820,MYHPR,3,ivolu)
      CALL GSVOLU('MYV ','BOX ',820,MYVPR,3,ivolu)
      CALL GSVOLU('SSH ','BOX ',821,SSHPR,3,ivolu)
      CALL GSVOLU('SSV ','BOX ',821,SSVPR,3,ivolu)
      CALL GSVOLU('EPH ','BOX ',824,EPHPR,3,ivolu)
      CALL GSVOLU('EPV ','BOX ',824,EPVPR,3,ivolu)
*HeB changed Material
*      CALL GSVOLU('CFH ','BOX ',  6,CFHPR,3,ivolu)
*      CALL GSVOLU('CFV ','BOX ',  6,CFVPR,3,ivolu)
*      CALL GSVOLU('SPH ','BOX ',  9,SPHPR,3,ivolu)
*      CALL GSVOLU('SPV ','BOX ',  9,SPVPR,3,ivolu)
cgd***      CALL GSVOLU('CFH ','BOX ',911,CFHPR,3,ivolu)
cgd***      CALL GSVOLU('CFV ','BOX ',911,CFVPR,3,ivolu)
cgd***      CALL GSVOLU('SPH ','BOX ',912,SPHPR,3,ivolu)
cgd***      CALL GSVOLU('SPV ','BOX ',912,SPVPR,3,ivolu)
      CALL GSVOLU('CFH ','BOX ',811,CFHPR,3,ivolu)
      CALL GSVOLU('CFV ','BOX ',811,CFVPR,3,ivolu)
      CALL GSVOLU('SPH ','BOX ',812,SPHPR,3,ivolu)
      CALL GSVOLU('SPV ','BOX ',812,SPVPR,3,ivolu)
*HeB      CALL GSVOLU('BP  ','BOX ',925,BPPR,3,ivolu)
 
c      CALL GSATT('MOD ','SEEN',seen_emc_cell)
      CALL GSATT('PBG ','SEEN',seen_emc_layers)
      CALL GSATT('FIB ','SEEN',seen_emc_layers)
      CALL GSATT('PMH ','SEEN',seen_emc_layers)
      CALL GSATT('MYH ','SEEN',seen_emc_layers)
      CALL GSATT('MYV ','SEEN',seen_emc_layers)
      CALL GSATT('MYV ','SEEN',seen_emc_layers)
      CALL GSATT('SSH ','SEEN',seen_emc_layers)
      CALL GSATT('SSV ','SEEN',seen_emc_layers)
      CALL GSATT('EPH ','SEEN',seen_emc_layers)
      CALL GSATT('EPV ','SEEN',seen_emc_layers)
      CALL GSATT('CFH ','SEEN',seen_emc_layers)
      CALL GSATT('CFV ','SEEN',seen_emc_layers)
      CALL GSATT('SPH ','SEEN',seen_emc_layers)
      CALL GSATT('SPV ','SEEN',seen_emc_layers)
*HeB      CALL GSATT('BP  ','SEEN',seen_emc_layers)
 
cMM   (04.09.2000) Medium (!) 6 is AIR without a magnetic field
      CALL GSVOLU('HUT ','BOX ',6,HUTPR,3,ivolu)
cMM      CALL GSVOLU('HUT ','BOX ',16,HUTPR,3,ivolu)
cMM

cgd***      CALL GSVOLU('HBH ','BOX ',923,HBXPR,3,ivolu)
cgd***      CALL GSVOLU('HBV ','BOX ',923,HBYPR,3,ivolu)
cgd***      CALL GSVOLU('HT  ','BOX ',923,HTPR,3,ivolu)
cgd***      CALL GSVOLU('HZX ','TRD1',923,HZXPR,4,ivolu)
cgd***      CALL GSVOLU('HZY ','TRD1',923,HZYPR,4,ivolu)

      CALL GSVOLU('HBH ','BOX ',823,HBXPR,3,ivolu)
      CALL GSVOLU('HBV ','BOX ',823,HBYPR,3,ivolu)
      CALL GSVOLU('HT  ','BOX ',823,HTPR,3,ivolu)
      CALL GSVOLU('HZX ','TRD1',823,HZXPR,4,ivolu)
      CALL GSVOLU('HZY ','TRD1',823,HZYPR,4,ivolu)
 
 
C     silicon layer in between
 
cgd***      CALL GSVOLU('SIL ','BOX ',926,SILPR,3,ivolu)
      CALL GSVOLU('SIL ','BOX ',826,SILPR,3,ivolu)
 
*HeB  supporting frame for PbGl

      CALL GSVOLU('PBFA','BOX ',9,PBFRAMEAPR,3,ivolu)
      CALL GSVOLU('PBFC','BOX ',9,PBFRAMECPR,3,ivolu)
      CALL GSVOLU('PBFD','BOX ',9,PBFRAMEDPR,3,ivolu)
      CALL GSVOLU('PBFE','BOX ',9,PBFRAMEEPR,3,ivolu)
      CALL GSVOLU('PBFF','TRAP ',9,PBFRAMEFPR,11,ivolu)
      CALL GSVOLU('PBFG','BOX ',9,PBFRAMEGPR,3,ivolu)
      CALL GSVOLU('PBFH','TRAP',9,PBFRAMEHPR,11,ivolu)
      CALL GSVOLU('PBF ','TRAP',16,PBFRAMEPR,11,ivolu) 

C     position the volumes
 
      CALL GSPOS('PBFA',1,'PBF ',104.9815,8.,0.,0,'ONLY')
      irot = irot + 1
      CALL GSROTM(irot,90.,352.7,90.,82.7,0.,0.)
      CALL GSPOS('PBFA',2,'PBF ',110.6995,8.365,0.,irot,'ONLY')
      CALL GSPOS('PBFC',1,'PBF ',-94.9225,0.,0.,0,'ONLY')
      irot = irot + 1
      CALL GSROTM(irot,90.,15.,90.,105.,0.,0.)
      CALL GSPOS('PBFD',1,'PBF ',-109.1185,-0.019,0.,irot,'ONLY')
      CALL GSPOS('PBFE',1,'PBF ',86.6315,8.,201.342,0,'ONLY')
      CALL GSPOS('PBFE',2,'PBF ',86.6315,8.,-201.342,0,'ONLY')
      CALL GSPOS('PBFF',1,'PBF ',26.0875,8.,206.192,0,'ONLY')
      CALL GSPOS('PBFF',2,'PBF ',26.0875,8.,-206.192,0,'ONLY')
      CALL GSPOS('PBFG',1,'PBF ',4.9795,8.,198.092,0,'ONLY')
      CALL GSPOS('PBFG',2,'PBF ',4.9795,8.,-198.092,0,'ONLY')
      CALL GSPOS('PBFH',1,'PBF ',-86.095,8.,201.842,0,'ONLY')
      CALL GSPOS('PBFH',2,'PBF ',-86.095,8.,-201.842,0,'ONLY')
  
C     volumes in a single module
 
      DO i = 7,8
        WRITE(v_x_name,'(a2,i1,a1)') 'EC',i,'2'
c        CALL GSVOLU(v_x_name,'BOX ', 16,MODPR,3,ivolu)
        CALL GSVOLU(v_x_name,'BOX ', 813,MODPR,3,ivolu)
c        write(*,*) 'emc - booking ', v_x_name, ' id: ', ivolu

        CALL GSPOS('PBG ',1,v_x_name,PBGPS(1),PBGPS(2),PBGPS(3),
     1    0,'ONLY')
        CALL GSPOS('FIB ',1,v_x_name,FIBPS(1),FIBPS(2),FIBPS(3),
     1    0,'ONLY')
        CALL GSPOS('PMH ',1,v_x_name,PMPS(1),PMPS(2),PMPS(3),
     1    0,'ONLY')
        CALL GSPOS('MYH ',1,v_x_name,MYHPSO(1),MYHPSO(2),MYHPSO(3),
     1    0,'ONLY')
        CALL GSPOS('MYH ',2,v_x_name,MYHPSU(1),MYHPSU(2),MYHPSU(3),
     1    0,'ONLY')
        CALL GSPOS('MYV ',1,v_x_name,MYVPSR(1),MYVPSR(2),MYVPSR(3),
     1    0,'ONLY')
        CALL GSPOS('MYV ',2,v_x_name,MYVPSL(1),MYVPSL(2),MYVPSL(3),
     1    0,'ONLY')
        CALL GSPOS('SSH ',1,v_x_name,SSHPSO(1),SSHPSO(2),SSHPSO(3),
     1    0,'ONLY')
        CALL GSPOS('SSH ',2,v_x_name,SSHPSU(1),SSHPSU(2),SSHPSU(3),
     1    0,'ONLY')
        CALL GSPOS('SSV ',1,v_x_name,SSVPSR(1),SSVPSR(2),SSVPSR(3),
     1    0,'ONLY')
        CALL GSPOS('SSV ',2,v_x_name,SSVPSL(1),SSVPSL(2),SSVPSL(3),
     1    0,'ONLY')
        CALL GSPOS('EPH ',1,v_x_name,EPHPSO(1),EPHPSO(2),EPHPSO(3),
     1    0,'ONLY')
        CALL GSPOS('EPH ',2,v_x_name,EPHPSU(1),EPHPSU(2),EPHPSU(3),
     1    0,'ONLY')
        CALL GSPOS('EPV ',1,v_x_name,EPVPSR(1),EPVPSR(2),EPVPSR(3),
     1    0,'ONLY')
        CALL GSPOS('EPV ',2,v_x_name,EPVPSL(1),EPVPSL(2),EPVPSL(3),
     1    0,'ONLY')
      enddo
      
      nr = 0
      DO i = 7,8
         WRITE(v_s_name,'(a2,i1,a1)') 'EC',i,'2'
         WRITE(v_i_name,'(a2,i1,a1)') 'SC',i,'2'
         CALL GSVOLU(v_i_name,'BOX ', 16,SUPPR,3,ivolu)
C     position of the complete modules
 
         MODPS(1) = -9.44
         DO YS=1,4
*HeB       MODPS(2) = 2.058*(-5. + 2.*YS)
           MODPS(2) = 2.046*(-5. + 2.*YS) + 0.025
           DO zs=1,6
*HeB        MODPS(3) = 2.052*(-7. + 2.*zs)
            MODPS(3) = 2.048*(-7. + 2.*zs)
            MODNR = (YS-1)*6 + zs
            CALL GSPOS(v_s_name,
     1        MODNR,v_i_name,MODPS(1),MODPS(2),MODPS(3)
     1        ,0,'ONLY')
          ENDDO
        ENDDO
 
C     position of the carbon fiber layers
 
         CFHPS(1) = -15.04
         CFHPS(3) = 0.0
         DO YS=1,5
*HeB          CFHPS(2) = (3-YS)*4.116
           CFHPS(2) = (3-YS)*4.092 + 0.025
           CALL GSPOS('CFH ',YS,v_i_name,CFHPS(1),CFHPS(2),CFHPS(3),
     1                0,'ONLY')
         ENDDO
         CFVPS(1) = -15.04
         CFVPS(2) = 0.025
*HeB         CFVPS(3) = -12.297
         CFVPS(3) = -12.3
         CALL GSPOS('CFV ',1,v_i_name,CFVPS(1),CFVPS(2),CFVPS(3),
     1              0,'ONLY')
*HeB         CFVPS(1) =  12.297
         CFVPS(3) =  12.3
         CALL GSPOS('CFV ',2,v_i_name,CFVPS(1),CFVPS(2),CFVPS(3),
     1              0,'ONLY')
 
C     position of the aluminium side plates
 
         SPHPS(1) = 24.46
         SPHPS(3) = 0.0
         SPHPS(2) = -8.184+0.025
         CALL GSPOS('SPH ',1,v_i_name,SPHPS(1),SPHPS(2),SPHPS(3),
     1              0,'ONLY')
         SPHPS(2) =  8.184+0.025
         CALL GSPOS('SPH ',2,v_i_name,SPHPS(1),SPHPS(2),SPHPS(3),
     1              0,'ONLY')
 
         SPVPS(1) = 24.46
         SPVPS(2) = 0.025
*         SPVPS(3) = -12.272
         SPVPS(3) = -12.3
         CALL GSPOS('SPV ',1,v_i_name,SPVPS(1),SPVPS(2),SPVPS(3),
     1              0,'ONLY')
*         SPVPS(1) =  12.272
         SPVPS(3) =  12.3
         CALL GSPOS('SPV ',2,v_i_name,SPVPS(1),SPVPS(2),SPVPS(3),
     1              0,'ONLY')
 
C     position of the "Neusilber" back plate
 
*HeB         CALL GSPOS('BP  ',1,v_i_name,BPPS(1),BPPS(2),BPPS(3),
*HeB     1              0,'ONLY')
 
*HeB     position of the silicon layer
 
           CALL GSPOS('SIL ',1,v_i_name,SILPS(1),SILPS(2),SILPS(3),
     1                0,'ONLY')

C     define the rotation matrix for the rotation of the trapezoids
 
C comment by S K Gupta in GSROTM  call the first argument should not be a numb
C         number instead it should be a variable. The following rotation
C         matrices seem not to be in use.
*HeB  new rot matrix
*HeB C    CALL GSROTM(1,90.,0.,105.22,90.,15.22,90.)
*HeB C    CALL GSROTM(2,90.,0.,74.78,90.,15.22,-90.)
*HeB C    CALL GSROTM(3,90.,-90.,121.14,0.,31.14,0.)
*HeB C    CALL GSROTM(4,90.,-90.,58.86,0.,31.14,180.)
	  irot = irot + 1
	  i_rot_hut_pbgl(1) = irot
          CALL GSROTM(irot,0.,0.,90.,106.366,90.,16.366)
          irot = irot + 1
          i_rot_hut_pbgl(2) = irot
          CALL GSROTM(irot,0.,0.,90.,73.634,90.,-16.366)
          irot = irot + 1
          i_rot_hut_pbgl(3) = irot
          CALL GSROTM(irot,90.,90.,33.144,0.,123.144,0.)
          irot = irot + 1
          i_rot_hut_pbgl(4) = irot
          CALL GSROTM(irot,90.,90.,33.144,180.,56.856,0.)
 
 
C     position the hut volumes
c     Leave out hut trapezoidal parts for now  12/28/93 G. David
*HeB  hut trapezoidal parts included
 
         CALL GSPOS('HUT ',1,v_i_name,HUTPS(1),HUTPS(2),HUTPS(3),
     1              0,'ONLY')
         IF(i.eq.7) THEN
*HeB            CALL GSPOS('HBH ',1,'HUT ',HBXPSO(1),HBXPSO(2),HBXPSO(3),
*HeB     1              0,'ONLY')
*HeB            CALL GSPOS('HBH ',2,'HUT ',HBXPSU(1),HBXPSU(2),HBXPSU(3),
*HeB     1              0,'ONLY')
*HeB            CALL GSPOS('HBV ',1,'HUT ',HBYPSR(1),HBYPSR(2),HBYPSR(3),
*HeB     1              0,'ONLY')
*HeB            CALL GSPOS('HBV ',2,'HUT ',HBYPSL(1),HBYPSL(2),HBYPSL(3),
*HeB     1              0,'ONLY')
									
      CALL GSPOS('HBH ',1,'HUT ',HBXPSO(1),HBXPSO(2),HBXPSO(3),0,'ONLY')
      CALL GSPOS('HBH ',2,'HUT ',HBXPSU(1),HBXPSU(2),HBXPSU(3),0,'ONLY')
      CALL GSPOS('HBV ',1,'HUT ',HBYPSR(1),HBYPSR(2),HBYPSR(3),0,'ONLY')
      CALL GSPOS('HBV ',2,'HUT ',HBYPSL(1),HBYPSL(2),HBYPSL(3),0,'ONLY')

      CALL GSPOS('HZY ',1,'HUT ',HZYPSL(1),HZYPSL(2),
     +  HZYPSL(3),i_rot_hut_pbgl(4),'ONLY')
      CALL GSPOS('HZY ',2,'HUT ',HZYPSR(1),HZYPSR(2),
     + 	HZYPSR(3),i_rot_hut_pbgl(3),'ONLY')
      CALL GSPOS('HZX ',1,'HUT ',HZXPSU(1),HZXPSU(2),	
     +	HZXPSU(3),i_rot_hut_pbgl(2),'ONLY')
      CALL GSPOS('HZX ',2,'HUT ',HZXPSO(1),HZXPSO(2),
     +	HZXPSO(3),i_rot_hut_pbgl(1),'ONLY')
      CALL GSPOS('HT  ',1,'HUT ',HTPS(1),HTPS(2),HTPS(3),0,'ONLY')
          ENDIF
									 
C     position the overall volumes in the mother volume
 
 
      ENDDO              ! Loop over i = 7,8
cgd***

c     End of Arne Claussen's insert (with considerable alterations,
c                                    Dec. 27, 1993, G. David)
cgd***updateE

1912  CONTINUE

c******************************************************************

c     Build blocks for cells for Shish-Kebab (12*12 in a supermodule)
c     First define layers
c     then build a cell (two cells: EC11 through EC61 for the six sectors)

c**************

c     Define everything needed to build Shish-Kebab


c     The "hut", i.e. the plastic cover is nowadays over
c     the entire supermodule (one cover per supermodule
c     instead of one cover per module)

      em_sheet(1) = hut_emc_height / 2.0
      em_sheet(2) = cell_size * super_nz_nphi(2) / 2.0
      em_sheet(3) = cell_size * super_nz_nphi(1) / 2.0
      nmed = hut_emc_med
      CALL GSVOLU('EMHU','BOX ',nmed,em_sheet,3,ivolu)
      CALL GSATT('EMHU','SEEN',seen_emc_hut)
      CALL GSATT('EMHU','COLO',hut_emc_color)
      em_sheet(1) = em_sheet(1) - hut_emc_thck / 2.0
      em_sheet(2) = em_sheet(2) - hut_emc_thck
      em_sheet(3) = em_sheet(3) - hut_emc_thck
      nmed = vacuum_emc_med
      CALL GSVOLU('EMHI','BOX ',nmed,em_sheet,3,ivolu)
      CALL GSATT('EMHI','SEEN',seen_emc_layers)
      CALL GSATT('EMHI','COLO',hut_emc_color+1)


c     Define layers in cell

      em_tsiz = cell_size
     1     - clearance_emc_module - skin_emc_thck   ! Subtract only once, since
c                                              ! four cells are in a module
      em_sheet(2) = em_tsiz / 2.0
      em_sheet(3) = em_tsiz / 2.0

      em_sheet(1) = fplate_emc_thck / 2.0
      nmed = fplate_emc_med
      CALL GSVOLU('EMFP','BOX ',nmed,em_sheet,3,ivolu)  ! Define FrontSteel
      CALL GSATT('EMFP','SEEN',seen_emc_layers)
      CALL GSATT('EMFP','COLO',steel_emc_color)

      em_sheet(1) = scint_emc_thck / 2.0
      nmed = scint_emc_med
      CALL GSVOLU('EMSC','BOX ',nmed,em_sheet,3,ivolu)  ! Define SCintillator
      CALL GSATT('EMSC','SEEN',seen_emc_layers)
      CALL GSATT('EMSC','COLO',scint_emc_color)


      em_sheet(1) = lead_emc_thck / 2.0
      nmed = lead_emc_med
      CALL GSVOLU('EMPB','BOX ',nmed,em_sheet,3,ivolu)  ! Define PB (lead)
      CALL GSATT('EMPB','SEEN',seen_emc_layers)
      CALL GSATT('EMPB','COLO',lead_emc_color)

      em_sheet(1) = paper_emc_thck / 2.0
      nmed = paper_emc_med
      CALL GSVOLU('EMPA','BOX ',nmed,em_sheet,3,ivolu)  ! Define PAper
      CALL GSATT('EMPA','SEEN',seen_emc_layers)
      CALL GSATT('EMPA','COLO',paper_emc_color)

      em_sheet(1) = bsteel_emc_thck / 2.0
      nmed = steel_emc_med
      CALL GSVOLU('EMBS','BOX ',nmed,em_sheet,3,ivolu)  ! Define BackSteel
      CALL GSATT('EMBS','SEEN',seen_emc_layers)
      CALL GSATT('EMBS','COLO',steel_emc_color)



c******************************

      cell_emc_depth = 2.0 * fplate_emc_thck + 2.0 * bsteel_emc_thck +
     1                 scint_emc_thck * scint_emc_plates +
     1                 lead_emc_thck * lead_emc_plates +
     1                 paper_emc_thck * scint_emc_plates * 2.0

      nmed = fiber_emc_med
      em_sheet(1) = 0.0
      em_sheet(2) = fiber_emc_dia / 2.0
      em_sheet(3) = cell_emc_depth / 2.0
      CALL GSVOLU('EMFI','TUBE',nmed,em_sheet,3,ivolu)   ! Define FIber
      CALL GSATT('EMFI','SEEN',seen_emc_layers)
      CALL GSATT('EMFI','COLO',fiber_emc_color)

c     You need to define supermodule matrix within the sector

      nzsegm = 6        ! New supermodule structure (Nov. 1994)
      nphisegm = 3       ! 6 in z, 3 in phi


c     Now build a cell
c     It consists of: 1/ plastic hut
c                     2/ plastic frontplate
c                        + steel frontplate
c                     3/ "lead_emc_plates"*elementary box defined
c                        as (currently 65 elementary boxes)
c                        lead, paper, scint, paper
c                     4/ plastic + steel backplate
c                     5/ longitudinally running fibers


c      irot = 1   ! cannot re-define irot

      irot = irot + 1
      irot_1 = irot
      CALL GSROTM(irot,90.0,90.0,0.0,0.0,90.0,0.0)
      em_cell(1) = cell_emc_depth / 2.0
      em_cell(2) = cell_size / 2.0
      em_cell(3) = cell_size / 2.0
      
      ! medium is "sensitive" vacuum
      nmed = 813
      zr = 0
      do i=1,MIN(6,emc_walls)

c       Define cell

        WRITE(v_i_name,'(a2,i1,a1)') 'EC',i,'1'
        CALL GSVOLU(v_i_name,'BOX ',nmed,em_cell,3,ivolu)  ! Define CEll
        CALL GSATT(v_i_name,'SEEN',seen_emc_cell)
        CALL GSATT(v_i_name,'COLO',cell_emc_color)
c        write(*,*) 'emc - booking ', v_i_name, ' id: ', ivolu
        kr = 1

c        Fill cell with layers

         posx = fplate_emc_thck / 2.0 - em_cell(1)
         CALL GSPOS('EMFP',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')
         kr = kr + 1
         posx = posx + (fplate_emc_thck + bsteel_emc_thck) / 2.0
         CALL GSPOS('EMBS',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')
         kr = kr + 1
         posx = posx + (bsteel_emc_thck + paper_emc_thck) / 2.0
         CALL GSPOS('EMPA',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')
         kr = kr + 1
         posx = posx + (paper_emc_thck + scint_emc_thck) / 2.0
         CALL GSPOS('EMSC',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')   ! Scintillator
         kr = kr + 1
         posx = posx + (scint_emc_thck + paper_emc_thck) / 2.0
         CALL GSPOS('EMPA',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')   ! Paper

         do j = 1,lead_emc_plates                                 ! Elem. boxes
            kr=kr+1
            posx = posx + (paper_emc_thck + lead_emc_thck) / 2.0
            CALL GSPOS('EMPB',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')
            kr = kr + 1
            posx = posx + (lead_emc_thck + paper_emc_thck) / 2.0
            CALL GSPOS('EMPA',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')   ! Paper
            kr = kr + 1
            posx = posx + (paper_emc_thck + scint_emc_thck) / 2.0
            CALL GSPOS('EMSC',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')   ! Scintillator
            kr = kr + 1
            posx = posx + (scint_emc_thck + paper_emc_thck) / 2.0
            CALL GSPOS('EMPA',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')   ! Paper
         enddo                 ! Loop over lead - scintillator layers

         kr = kr + 1
         posx = posx + (paper_emc_thck + fplate_emc_thck) / 2.0        ! Steel backsh.
         CALL GSPOS('EMFP',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')
         kr = kr + 1
         posx = posx + (fplate_emc_thck + bsteel_emc_thck) / 2.0        ! Steel backsh.
         CALL GSPOS('EMBS',kr,v_i_name,posx,0.0,0.0,inull,'ONLY')

         posx = 0.0
         posy = - em_tsiz / 2.0 + em_tsiz / 12.0
         DO j = 1,6
            posz = - em_tsiz / 2.0 + em_tsiz / 12.0
            DO k = 1,6
               kr = kr + 1
               CALL GSPOS('EMFI',kr,v_i_name,posx,posy,posz,
     1                    irot_1,'ONLY')
               posz = posz + em_tsiz / 6.0
            ENDDO
            posy = posy + em_tsiz / 6.0
         ENDDO
      enddo    ! End of loop over i defining Shish-Kebab cells
c--------------

c        At this point you got all the elementary building blocks
c        Shish-Kebab cells - ECn1 - , crystals - EMXn
c        and PbGl supermodules - SUP -.

c------------
 
c-------------------------------------------------

c     Now generate the supermodules.  For uniqueness (GEANT...)
c     every sector has its own supermodules (different names), although
c     physically these are identical (e.g. SC11 through SC61, the
c     six Shish-Kebab supermodules are identical, but they are used
c     later to build sectors 1...6, respectively).

c     Once the supermodules are defined...

c     loop over sectors


c            Generate rotation matrices.  We assume that the sectors
c            on the same side touch each other; the angle (inclination)
c            of the sectors is derived from this
c            The first six guys are for the Shish-Kebab

c     October 31, 1996, G. David
c     Watch out! zero azimuth is actually the WEST arm, and the
c     lead glass resides in the WEST arm, so mirror the entire
c     thing now!
c     November 3, 1997:  Toggle switch l_pbgl_west_arm to move
c     lead glass from west to east and vice versa

      r_wallx = rpos_emc - hut_emc_height
      r_wally = FLOAT(nphi_super * nphisegm) * cell_size / 2.0
     1          + cage_emc_thck
      r_viewing_angle = ATAN2D(r_wally,r_wallx)

cgd***      r_angle(1) = - 2.0 * r_viewing_angle
c      Do everything from phnx.par !
      r_angle(1) = emc_phi_sec1

      irot = irot + 1
cgd***      IF(l_pbgl_west_arm) THEN
cgd***         CALL GSROTM(irot,90.0,180.0-r_angle(1),
cgd***     1     90.0,90.0-r_angle(1),0.0,0.0)
cgd***       ELSE
cgd***         CALL GSROTM(irot,90.0,r_angle(1),90.0,90.0+r_angle(1),0.0,0.0)
      CALL GSROTM(irot,90.0,r_angle(1),90.0,90.0+r_angle(1),0.0,0.0)
cgd***      ENDIF
      irot_array(1) = irot

cgd***      r_angle(2) = 0.0
c      Do everything from phnx.par !
      r_angle(2) = emc_phi_sec2

      irot = irot + 1
cgd***      IF(l_pbgl_west_arm) THEN
cgd***         CALL GSROTM(irot,90.0,180.0-r_angle(2),
cgd***     1     90.0,90.0-r_angle(2),0.0,0.0)
cgd***       ELSE
      CALL GSROTM(irot,90.0,r_angle(2),90.0,90.0+r_angle(2),0.0,0.0)
cgd***      ENDIF
      irot_array(2) = irot

cgd***      r_angle(3) = 2.0 * r_viewing_angle
c      Do everything from phnx.par !
      r_angle(3) = emc_phi_sec3

      irot = irot + 1
cgd***      IF(l_pbgl_west_arm) THEN
cgd***         CALL GSROTM(irot,90.0,180.0-r_angle(3),
cgd***     1     90.0,90.0-r_angle(3),0.0,0.0)
cgd***       ELSE
      CALL GSROTM(irot,90.0,r_angle(3),90.0,90.0+r_angle(3),0.0,0.0)
cgd***      ENDIF
      irot_array(3) = irot

cgd***      r_angle(4) = 4.0 * r_viewing_angle
c      Do everything from phnx.par !
      r_angle(4) = emc_phi_sec4

      irot = irot + 1
cgd***      IF(l_pbgl_west_arm) THEN
cgd***         CALL GSROTM(irot,90.0,180.0-r_angle(4),
cgd***     1     90.0,90.0-r_angle(4),0.0,0.0)
cgd***       ELSE
      CALL GSROTM(irot,90.0,r_angle(4),90.0,90.0+r_angle(4),0.0,0.0)
cgd***      ENDIF
      irot_array(4) = irot

cgd***      r_angle(5) = r_angle(4)
      irot = irot + 1
c      Do everything from phnx.par !
      r_angle(5) = emc_phi_sec5

cgd***      IF(l_pbgl_west_arm) THEN
cgd***         CALL GSROTM(irot,90.0,r_angle(4),
cgd***     1     90.0,90.0+r_angle(4),0.0,0.0)
cgd***       ELSE
cgd***         CALL GSROTM(irot,90.0,180.0-r_angle(4),90.0,90.0-r_angle(4),
cgd***     1            0.0,0.0)
      CALL GSROTM(irot,90.0,180.0-r_angle(5),90.0,90.0-r_angle(5),
     1            0.0,0.0)
cgd***      ENDIF
      irot_array(5) = irot

cgd***      r_angle(6) = r_angle(3)
      irot = irot + 1
c      Do everything from phnx.par !
      r_angle(6) = emc_phi_sec6

cgd***      IF(l_pbgl_west_arm) THEN
cgd***         CALL GSROTM(irot,90.0,r_angle(3),
cgd***     1     90.0,90.0+r_angle(3),0.0,0.0)
cgd***        ELSE
cgd***         CALL GSROTM(irot,90.0,180.0-r_angle(3),90.0,90.0-r_angle(3),
cgd***     1            0.0,0.0)
      CALL GSROTM(irot,90.0,180.0-r_angle(6),90.0,90.0-r_angle(6),
     1            0.0,0.0)
cgd***       ENDIF
      irot_array(6) = irot

c             Now for the lead glass: the distance is different

      r_wallx = rpos_emc + rpos_emc_add_pbgl     ! 20 cm for TOF in front
      r_wally = FLOAT(nphi_pbgl) * suppr(2)      !
     1          + 2.0 * cage_emc_thck
*HeB      r_viewing_angle = ATAN2D(r_wally,r_wallx)
*HeB  calculated angle is too small for real supporting frame    
      r_viewing_angle = 11. 



      irot = irot + 1
cgd***      r_angle(7) = 0.0
c      Do everything from phnx.par !
      r_angle(7) = emc_phi_sec7

cgd***      IF(l_pbgl_west_arm) THEN
cgd***      CALL GSROTM(irot,90.0,r_angle(7),90.0,90.0+r_angle(7),
cgd***     1            0.0,0.0)
cgd***       ELSE
      CALL GSROTM(irot,90.0,180.0-r_angle(7),90.0,90.0-r_angle(7),
     1            0.0,0.0)
cgd***      ENDIF
      irot_array(7) = irot

      irot = irot + 1
cgd***      r_angle(8) = - 2.0 * r_viewing_angle
c      Do everything from phnx.par !
      r_angle(8) = emc_phi_sec8

cgd***      IF(l_pbgl_west_arm) THEN
cgd***      CALL GSROTM(irot,90.0,r_angle(8),90.0,90.0+r_angle(8),
cgd***     1            0.0,0.0)
cgd***       ELSE
      CALL GSROTM(irot,90.0,180.0-r_angle(8),90.0,90.0-r_angle(8),
     1            0.0,0.0)
cgd***      ENDIF
      irot_array(8) = irot



c     This is the main loop to build sectors


      do iwall = 1,MIN(6,emc_walls)

         i_rotate = irot_array(iwall)
         itype = 1                   ! Shish-Kebab
         nr = 0         ! initialize copy number for supermodules
         mr = 0         ! initialize copy number for cells



c     super-cell segments along the PHI direction


c     angle subtended in azimuth for each supercell

         write (v_i_name, '(a2,2i1)') 'EC', iwall, itype

c     Now make the Super-Cell
c     This is a BOX with NZ_SUPER*CELL by NPHI_SUPER*CELL dimension
c        in the Z and the Y directions.
c        (currently 12 * 12)
c     The X dimension is the same CELL_EMC_DEPTH.

          write(v_s_name,'(a2,2i1)') 'SC',iwall,itype
          write(v_supv_name,'(a2,2i1)') 'SV',iwall,itype
          write(v_suph_name,'(a2,2i1)') 'SH',iwall,itype
          boxsupr_par(1) = cell_emc_depth/2.0
          boxsupr_par(2) = float(nphi_super)*cell_size/2.0
          boxsupr_par(3) = float(nz_super)*cell_size/2.0
          npar=3

c     Use non-magnetic air as the medium

          call gsvolu(v_s_name,'BOX ',6,boxsupr_par,npar,ivolu)
cgd***
c          write(6,'(a2,3f8.3)') 'SM',boxsupr_par(1),
c     1          boxsupr_par(2),boxsupr_par(3)

          if(super_emc_color.gt.0)then
            CALL GSATT(v_s_name,'SEEN',1)
            CALL GSATT(v_s_name,'COLO',super_emc_color)
           else
            CALL GSATT(v_s_name,'SEEN',0)
          endif

          em_sheet(1) = cell_emc_depth / 2.0
          em_sheet(2) = cage_emc_thck
          em_sheet(3) = cage_emc_thck +
     1     FLOAT(nzsegm) * FLOAT(nz_super) * cell_size / 2.0
          nmed = steel_emc_med
          CALL GSVOLU(v_suph_name,
     1         'BOX ',nmed,em_sheet,3,ivolu) ! Structure Horizontal
          CALL GSATT(v_suph_name,'SEEN',seen_emc_cage)
          CALL GSATT(v_suph_name,'COLO',cage_emc_color)

          em_sheet(2) = cage_emc_thck +
     1     FLOAT(nphisegm) * FLOAT(nphi_super) * cell_size / 2.0
          em_sheet(3) = cage_emc_thck
          CALL GSVOLU(v_supv_name,
     1         'BOX ',nmed,em_sheet,3,ivolu) ! Structure Vertical
          CALL GSATT(v_supv_name,'SEEN',seen_emc_cage)
          CALL GSATT(v_supv_name,'COLO',cage_emc_color)

c---------------------------

c     now fill the Supermodule for Shish-Kebab

          x_super = 0.0
          z_start = -float(nz_super)*cell_size/2.0 + cell_size/2.0
          do iz = 1,nz_super
             z_super = z_start + float(iz-1)*cell_size
             y_start = -float(nphi_super)*cell_size/2.0 +
     1                    cell_size/2.0
             do iphi = 1,nphi_super
                y_super = y_start + float(iphi-1)*cell_size
                mr = mr+1
                call gspos(v_i_name,mr,v_s_name,x_super,y_super,
     1                       z_super,inull,'ONLY')
cgd***
c          write(6,'(a2,3f8.3)') 'WA',x_super,y_super,z_super

             enddo
          enddo

c         Now put hut on top of supermodule

          x_super = - hut_emc_height / 2.0 - em_cell(1)
          y_super = 0.0
          z_super = 0.0
          mr = mr + 1
          CALL GSPOS('EMHU',mr,v_s_name,x_super,y_super,
     1         z_super,inull,'ONLY')
          x_super = x_super + hut_emc_thck / 2.0
          mr = mr + 1
          CALL GSPOS('EMHI',mr,v_s_name,x_super,y_super,
     1         z_super,inull,'ONLY')

c     put at V_S_NAME at all positions (super cells)


c         boxsupr_par(i) is the HALF-SIZE !!

          z_start = - boxsupr_par(3) * FLOAT(nzsegm - 1)


c         Fill the inside of the support structure
c         i.e. FILL A SECTOR WITH SUPERMODULES

          z_sens = z_start
          do iz = 1,nzsegm


             y_start = - boxsupr_par(2) * FLOAT(nphisegm-1)
             do iphi = 1,nphisegm
                x_sens = rpos_emc + cell_emc_depth / 2.0
                y_sens = y_start + FLOAT(iphi-1) * boxsupr_par(2) * 2.0

                posx = x_sens * COSD(r_angle(iwall))
     1                 - y_sens * SIND(r_angle(iwall))
                posy = x_sens * SIND(r_angle(iwall))
     1                 + y_sens * COSD(r_angle(iwall))
                posz = z_sens

cgd***                IF(l_pbgl_west_arm) THEN
cgd***                   IF(iwall.le.4) posx = - posx
cgd***                 ELSE
                   IF(iwall.ge.5) posx = -posx
cgd***                ENDIF


                nr = nr + 1
                if(iwall.le.4)then

cgd***             Fine-tuning of geometry for Year-2
c                  May 27, 2002  G. David
                   posz = posz + emc_westarm_deltaz
                   posy = posy + emc_westarm_deltay

                   call gspos(v_s_name,nr,wEMC,posx,posy,
     1                        posz,i_rotate,'ONLY')


                else

cgd***             Fine-tuning of geometry for Year-2
c                  May 27, 2002  G. David
                   posz = posz + emc_eastarm_deltaz
                   posy = posy + emc_eastarm_deltay


                  call gspos(v_s_name,nr,eEMC,posx,posy,
     1                        posz,i_rotate,'ONLY')
                endif
cgd***
c          write(6,'(a2,4f8.3)') 'SE',posx,posy,posz,r_angle(iwall)

             enddo    ! store over phi of the Super Cells


c       Increment z position

             z_sens = z_sens + boxsupr_par(3) * 2.0
          enddo ! store over z of the Super Cells
 


c     Put "cage" (support structure) around a sector

          y_start = 0.0
          z_start = - boxsupr_par(3) * FLOAT(nzsegm) -
     1         cage_emc_thck / 2.0
          x_sens = rpos_emc + cell_emc_depth / 2.0
          y_sens = y_start
          z_sens = z_start

          posx = x_sens * COSD(r_angle(iwall))
     1                 - y_sens * SIND(r_angle(iwall))
          posy = x_sens * SIND(r_angle(iwall))
     1                 + y_sens * COSD(r_angle(iwall))
          posz = z_sens

cgd***          IF(l_pbgl_west_arm) THEN
cgd***             IF(iwall.le.4) posx = - posx
cgd***           ELSE
          IF(iwall.ge.5) posx = -posx
cgd***          ENDIF
          nr = nr + 1



          if(iwall.le.4)then

cgd***             Fine-tuning of geometry for Year-2
c                  May 27, 2002  G. David
           posz = posz + emc_westarm_deltaz
           posy = posy + emc_westarm_deltay

           call gspos(v_supv_name,nr,wEMC,posx,posy,
     1                  posz,i_rotate,'ONLY')

          else

cgd***             Fine-tuning of geometry for Year-2
c                  May 27, 2002  G. David
           posz = posz + emc_eastarm_deltaz
           posy = posy + emc_eastarm_deltay

           call gspos(v_supv_name,nr,eEMC,posx,posy,
     1                  posz,i_rotate,'ONLY')
          endif

          posz = posz + boxsupr_par(3) * FLOAT(nzsegm) * 2.0
     1           + cage_emc_thck
          nr = nr + 1
          if(iwall.le.4)then
             call gspos(v_supv_name,nr,wEMC,posx,posy,
     1                  posz,i_rotate,'ONLY')
          else
             call gspos(v_supv_name,nr,eEMC,posx,posy,
     1                  posz,i_rotate,'ONLY')              
          endif
          y_start = - boxsupr_par(2) * FLOAT(nphisegm)
     1           - cage_emc_thck / 2.0
          x_sens = rpos_emc + cell_emc_depth / 2.0
          y_sens = y_start
          z_sens = 0.0
          posx = x_sens * COSD(r_angle(iwall))
     1                 - y_sens * SIND(r_angle(iwall))
          posy = x_sens * SIND(r_angle(iwall))
     1                 + y_sens * COSD(r_angle(iwall))
          posz = z_sens

cgd***          IF(l_pbgl_west_arm) THEN
cgd***             IF(iwall.le.4) posx = - posx
cgd***           ELSE
          IF(iwall.ge.5) posx = -posx
cgd***          ENDIF
          nr = nr + 1
          if(iwall.le.4)then
             call gspos(v_suph_name,nr,wEMC,posx,posy,
     1                  posz,i_rotate,'ONLY')
           else
             call gspos(v_suph_name,nr,eEMC,posx,posy,
     1                  posz,i_rotate,'ONLY')
           endif

           y_sens = y_sens + FLOAT(nphisegm) * boxsupr_par(2) * 2.0
     1           + cage_emc_thck
          posx = x_sens * COSD(r_angle(iwall))
     1                 - y_sens * SIND(r_angle(iwall))
          posy = x_sens * SIND(r_angle(iwall))
     1                 + y_sens * COSD(r_angle(iwall))
          posz = z_sens

cgd***          IF(l_pbgl_west_arm) THEN
cgd***             IF(iwall.le.4) posx = - posx
cgd***           ELSE
          IF(iwall.ge.5) posx = -posx
cgd***          ENDIF
          nr = nr + 1

cgd***

          if(iwall.le.4)then
             call gspos(v_suph_name,nr,wEMC,posx,posy,
     1                  posz,i_rotate,'ONLY')
          else
             call gspos(v_suph_name,nr,eEMC,posx,posy,
     1                  posz,i_rotate,'ONLY')
          endif

c ----------------------------------

c     This is for lead scintillator
c     now make it a sensitive detector

          set_id = 'EMC '         ! put it in a SET
          nv = 2  ! for now
          namesv(1) = v_i_name
          namesv(2) = v_s_name
          nbitsv(1) = 16
          nbitsv(2) = 16
          idtype = 0
          nwpa = 100           ! for now
          nwsa = 100           ! for now
          do i=1,nh
            facte(i) = fact(i)
          enddo
          facte(4) = 1.e+8     ! 0.01 keV resolution on DELE
          call gsdetv(set_id,v_i_name,idtype,nwpa,
     1      nwsa,iset,idet)
          call gsdeth(set_id,v_i_name,nh,namesh,nbitsh,orig,facte)

c------------------------

c     now put the Super Cell parameters in the UDETPAR array
c     for the Shish-Kebab modules

          angle = r_angle(iwall)
cgd***          IF(l_pbgl_west_arm) THEN
cgd***             IF(iwall.ge.5) angle = 180.0 - r_angle(iwall)
cgd***           ELSE
          IF(iwall.le.4) angle = 180.0 - r_angle(iwall)
cgd***          ENDIF
          lsiz = cell_emc_depth
          tsiz = cell_size
          itype = 1
          no_modz = nz_super
          no_mody = nphi_super
          no_smody = nphisegm
          no_smodz = nzsegm



          zc_start = - FLOAT(no_modz * no_smodz - 1)
     1               * tsiz / 2.0


          yc_start = rpos_emc * SIND(angle)
     1               - (FLOAT(no_mody * no_smody - 1)
     1               * tsiz / 2.0) * ABS(COSD(angle))

          rpos_emc_plus_pbsc = hut_emc_height + fplate_emc_thck

          IF(gcuts(1).eq.0.0) gcuts(1) = emc_cutgam
          IF(gcuts(2).eq.0.0) gcuts(1) = emc_cutele
          IF(gcuts(3).eq.0.0) gcuts(1) = emc_cutneu
          IF(gcuts(4).eq.0.0) gcuts(1) = emc_cuthad
          IF(gcuts(5).eq.0.0) gcuts(1) = emc_cutmuo

          udetpar(1) = float(emc_walls)     ! number of walls from PHNX.PAR
          udetpar(2) = float(emc_opt)       ! EMCal option from PHNX.PAR
          udetpar(3) = float(iwall)        ! wall number (1-4)
          udetpar(4) = float(itype)        ! detector type (Sh-K,PbGl)
          udetpar(5) = angle               ! phi angle of wall center
          udetpar(6) = rpos_emc                ! radial position of wall center
          udetpar(7) = zc_start             ! center of first cell, z coor.
          udetpar(8) = yc_start             ! center of first cell, y coor.
          udetpar(9) = lsiz                ! long. size of a cell
          udetpar(10) = tsiz               ! transverse size of a cell
          udetpar(11) = FLOAT(no_modz)     ! No. of cells in z in a supermod.
          udetpar(12) = FLOAT(no_mody)     ! No. of cells in y in a supermod.
          udetpar(13) = FLOAT(no_smodz)    ! No. of supermods. in z / wall
          udetpar(14) = FLOAT(no_smody)    ! No. of supermods. in y / wall

c          udetpar(15) = 0.0                ! 
c          udetpar(16) = 0.0                !
c          udetpar(17) = 0.0                !
c         Use udetpar(15)-(17) to store carriage translation

          IF(iwall.le.4) THEN
           DO i=1,3
            udetpar(14+i) = west_shift(i)
           ENDDO
          ELSE
           DO i=1,3
            udetpar(14+i) = east_shift(i)
           ENDDO
          ENDIF

          udetpar(18) = FLOAT(scint_emc_med)  ! Shish-Kebab scint. medium
          udetpar(19) = 0.0                !
          udetpar(20) = 0.0                !
          udetpar(21) = 0.0
          udetpar(22) = FLOAT(emc_debug)
          udetpar(23) = gcuts(1)
          udetpar(24) = gcuts(2)
          udetpar(25) = gcuts(3)
          udetpar(26) = gcuts(4)
          udetpar(27) = gcuts(5)
          udetpar(28) = 0.0     !
          udetpar(29) = 0.0     !
          udetpar(30) = emc_r_min_sc ! bitp lower limit, PbSc
          udetpar(31) = emc_r_max_sc ! bitp upper limit, PbSc
          udetpar(32) = emc_r_step ! bitp stepsize, PbSc
          udetpar(33) = emc_z_min ! bitp lower limit
          udetpar(34) = emc_z_max ! bitp upper limit
          udetpar(35) = emc_z_step ! bitp stepsize
          udetpar(36) = emc_x_min_sc ! bitp lower limit, PbSc
          udetpar(37) = emc_x_max_sc ! bitp upper limit, PbSc
          udetpar(38) = emc_x_step ! bitp stepsize, PbSc
          udetpar(39) = 0.0     !
          udetpar(40) = emc_dele_max_sc ! bitp dE upper limit, PbSc
          udetpar(41) = emc_dele_step_sc ! bitp dE upper limit, PbSc
          udetpar(42) = emc_tof_min ! bitp lower limit
          udetpar(43) = emc_tof_max ! bitp upper limit
          udetpar(44) = emc_tof_step ! bitp stepsize
          udetpar(45) = 0.0     !
          udetpar(46) = 0.0     !
          udetpar(47) = 0.0     !
          udetpar(48) = 0.0     !
          udetpar(49) = 0.0     !
          udetpar(50) = FLOAT(emc_ind1_max_sc) ! bitp tower ind. 
          udetpar(51) = FLOAT(emc_ind2_max_sc) ! bitp tower ind. 
          udetpar(52) = FLOAT(emc_iwall_max) ! 
          udetpar(53) = FLOAT(emc_itype_max) ! 
          udetpar(54) = FLOAT(emc_i1_max) ! 
          udetpar(55) = 0.0     !
          udetpar(56) = 0.0     !
          udetpar(57) = 0.0     !
          udetpar(58) = 0.0     !
          udetpar(59) = 0.0     !
          udetpar(60) = FLOAT(emc_itrack_max) ! 
          udetpar(61) = FLOAT(emc_spart_max) ! 
          udetpar(62) = FLOAT(emc_ncycle_max) ! 
          udetpar(63) = 0.0     !
          udetpar(64) = 0.0     !
          udetpar(65) = emc_cutgam ! 
          udetpar(66) = emc_cutele ! 
          udetpar(67) = emc_cutneu ! 
          udetpar(68) = emc_cuthad ! 
          udetpar(69) = emc_cutmuo ! 
          udetpar(70) = 0.0     !
          udetpar(71) = ra_thr(1)     !  EM threshold for scint.
          udetpar(72) = ra_thr(2)     !  EM threshold for WLS
          udetpar(73) = ra_thr(3)     !  EM threshold for lead
          udetpar(74) = ra_thr(4)     !  EM threshold for steel
          udetpar(75) = ra_thr(5)     !  EM threshold for tyvek
          udetpar(76) = ra_thr(6)     !  EM threshold for plastic 1
          udetpar(77) = 0.0     !
          udetpar(78) = 0.0     !
          udetpar(79) = 0.0     !
          udetpar(80) = emc_response_option     !

          call gsdetu('EMC ',v_i_name,nudetpar,udetpar)


          if(iwall.eq.1.and.itype.eq.1)then

c BOOK 'EPRU' bank

              call MZFORM('PARU',     !bank name
     $              '-F',       !all data in the bank is Float
     $              IOPARU)
              call MZBOOK(
     $         ixdiv_fr,          !division
     $         lFE_PARU,          !return address of the bank
     $         lFE_PARU,          !supporting link
     $         1,                 !JB=1 means create top level bank
     $         'PARU',            !bank name
     $         0,                 !NL=0. No links
     $         0,                 !NS=0. No supporting (down) links
     $         EMC_PARU_ND,       !# of data words
     $         IOPARU,            !IO format word
     $         0)                 !NZERO=0 means that whole bank is cleared

c copy derived geometry parameters into 'EPRU' bank

c             Booked but not filled and not retrieved right now
c             05/18/93 GD

          endif ! store of user parameters
1514      CONTINUE
      enddo    ! loop over arms

      IF(.NOT.l_pbgl) GOTO 9999    ! No lead glass to define

c         Define lead glass

*HeB supporting frame
cgd***          x_sens = rpos_emc + 20.0 + suppr(1) + 1.96 
          x_sens = rpos_emc + rpos_emc_add_pbgl + suppr(1) + 1.96 
*                  ! Additional 20. cm for TOF and 1.96 cm for framecenter
          y_sens = -5.229
          z_sens = 0.
	  nr = 0	  

      DO iwall = 7,8
	  
                posx = x_sens * COSD(r_angle(iwall))
     1                 - y_sens * SIND(r_angle(iwall))
                posy = x_sens * SIND(r_angle(iwall))
     1                 + y_sens * COSD(r_angle(iwall))
                posz = z_sens

      irot = irot + 1
      nr = nr + 1
cgd***      IF(l_pbgl_west_arm) THEN
cgd***       CALL GSROTM(irot,90.,90.+r_angle(iwall),90.,
cgd***     1	  0.+r_angle(iwall),0.,0.)
cgd***       IF(iwall.le.4) posx = - posx
cgd***      ELSE
      CALL GSROTM(irot,90.,90.-r_angle(iwall),90.,
     1	  180.-r_angle(iwall),0.,0.)
      IF(iwall.ge.5) posx = -posx
cgd***      ENDIF
      CALL GSPOS('PBF ',nr,eEMC,posx,posy,posz,irot,'MANY')

      ENDDO
      DO iwall = 7,8
         i_rotate = irot_array(iwall)
         itype = 2                   ! Lead glass
         nr = 0         ! initialize copy number for supermodules
         mr = 0         ! initialize copy number for cells


         write (v_i_name, '(a2,2i1)') 'EC', iwall, itype


          write(v_s_name,'(a2,2i1)') 'SC',iwall,itype


c     put at V_S_NAME at all positions (super cells)


          z_start = -suppr(3) * FLOAT(nz_pbgl - 1)
          z_sens = z_start
          do iz = 1,nz_pbgl

c            Take care of the case when there are crystals


             y_start = - suppr(2) * FLOAT(nphi_pbgl - 1)
             do iphi = 1,nphi_pbgl
cgd***                x_sens = rpos_emc + 20.0 + suppr(1) ! Additional 20. cm for TOF
                x_sens = rpos_emc + rpos_emc_add_pbgl + suppr(1) 
                y_sens = y_start + FLOAT(iphi-1) * suppr(2) * 2.0





                posx = x_sens * COSD(r_angle(iwall))
     1                 - y_sens * SIND(r_angle(iwall))
                posy = x_sens * SIND(r_angle(iwall))
     1                 + y_sens * COSD(r_angle(iwall))
                posz = z_sens

cgd***                IF(l_pbgl_west_arm) THEN
cgd***                  IF(iwall.le.4) posx = - posx
cgd***                 ELSE
                IF(iwall.ge.5) posx = -posx
cgd***                ENDIF

                nr = nr + 1
                if(iwall.le.4)then
                   call GSPOS(v_s_name,nr,wEMC,posx,posy,
     1                        posz,i_rotate,'ONLY')
                else
                  call GSPOS(v_s_name,nr,eEMC,posx,posy,
     1                        posz,i_rotate,'ONLY')
                endif
             enddo    ! store over phi of the Super Cells


c       Increment z position

             z_sens = z_sens + suppr(3) * 2.0
          enddo ! store over z of the Super Cells


c     now make it a sensitive detector

          set_id = 'EMC '         ! put it in a SET
          nv = 2  ! for now
          namesv(1) = v_i_name
          namesv(2) = v_s_name
          nbitsv(1) = 16
          nbitsv(2) = 16
          idtype = 0
          nwpa = 100           ! for now
          nwsa = 100           ! for now
          do i=1,nh
            facte(i) = fact(i)
          enddo
          facte(4) = 1.e+8     ! 0.01 keV resolution on DELE
          call gsdetv(set_id,v_i_name,idtype,nwpa,
     1                 nwsa,iset,idet)
          call gsdeth(set_id,v_i_name,nh,namesh,nbitsh,orig,facte)


c------------------------
c     now put the Super Cell parameters in the UDETPAR array
c     for the leadg glass modules

          angle = r_angle(iwall)

cgd***          IF(l_pbgl_west_arm) THEN
cgd***             IF(iwall.ge.5) angle = 180.0 - r_angle(iwall)
cgd***           ELSE
          IF(iwall.le.4) angle = 180.0 - r_angle(iwall)
cgd***          ENDIF
          lsiz = 2.0 * modpr(1)
          tsiz = 2.0 * modpr(2)
          itype = 2
          no_modz = 6
          no_mody = 4
          no_smody = nphi_pbgl
          no_smodz = nz_pbgl



          zc_start = - FLOAT(no_modz * no_smodz - 1)
     1               * tsiz / 2.0


cgd***          yc_start = rpos_emc * SIND(angle)
c          yc_start = (rpos_emc + rpos_emc_add_pbgl) * SIND(angle)
c     1               - (FLOAT(no_mody * no_smody - 1)
c     1               * tsiz / 2.0) * ABS(COSD(angle))

cgd***          rpos_emc_plus_pbgl = suppr(1) - pbgpr(1) + pbgps(1) + 20.
          rpos_emc_plus_pbgl = 2.0 * hutpr(1) + rpos_emc_add_pbgl
          yc_start = (rpos_emc + rpos_emc_plus_pbgl) * SIND(angle)
     1               - (FLOAT(no_mody * no_smody - 1)
     1               * tsiz / 2.0) * ABS(COSD(angle))


          udetpar(1) = float(emc_walls)     ! number of walls from PHNX.PAR
          udetpar(2) = float(emc_opt)       ! EMCal option from PHNX.PAR
          udetpar(3) = float(iwall)        ! wall number (1-4)
          udetpar(4) = float(itype)        ! detector type (Sh-K,PbGl)
          udetpar(5) = angle               ! phi angle of wall center
          udetpar(6) = rpos_emc +
     1         rpos_emc_plus_pbgl                 ! radial position of wall center
          udetpar(7) = zc_start             ! center of first cell, z coor.
          udetpar(8) = yc_start             ! center of first cell, y coor.
          udetpar(9) = lsiz                ! long. size of a cell
          udetpar(10) = tsiz               ! transverse size of a cell
          udetpar(11) = FLOAT(no_modz)     ! No. of cells in z in a supermod.
          udetpar(12) = FLOAT(no_mody)     ! No. of cells in y in a supermod.
          udetpar(13) = FLOAT(no_smodz)    ! No. of supermods. in z / wall
          udetpar(14) = FLOAT(no_smody)    ! No. of supermods. in y / wall


c          udetpar(15) = 0.0                ! 
c          udetpar(16) = 0.0                !
c          udetpar(17) = 0.0                !
c         Use udetpar(15)-(17) to store carriage translation
c         PbGl always in east arm
          DO i=1,3
           udetpar(14+i) = east_shift(i)
          ENDDO

          udetpar(18) = FLOAT(pbgl_emc_med)  ! Shish-Kebab scint. medium
          udetpar(19) = 0.0
          udetpar(20) = 0.0
          udetpar(21) = 0.0
          udetpar(22) = FLOAT(emc_debug)
          udetpar(23) = gcuts(1)
          udetpar(24) = gcuts(2)
          udetpar(25) = gcuts(3)
          udetpar(26) = gcuts(4)
          udetpar(27) = gcuts(5)
          udetpar(28) = 0.0     !
          udetpar(29) = 0.0     !
          udetpar(30) = emc_r_min_gl ! bitp lower limit, PbGl
          udetpar(31) = emc_r_max_gl ! bitp upper limit, PbGl
          udetpar(32) = emc_r_step ! bitp stepsize, PbGl
          udetpar(33) = emc_z_min ! bitp lower limit
          udetpar(34) = emc_z_max ! bitp upper limit
          udetpar(35) = emc_z_step ! bitp stepsize
          udetpar(36) = emc_x_min_gl ! bitp lower limit, PbGl
          udetpar(37) = emc_x_max_gl ! bitp upper limit, PbGl
          udetpar(38) = emc_x_step ! bitp stepsize, PbSc
          udetpar(39) = 0.0     !
          udetpar(40) = emc_dele_max_gl ! bitp dE upper limit, PbGl
          udetpar(41) = emc_dele_step_gl ! bitp dE upper limit, PbGl
          udetpar(42) = emc_tof_min ! bitp lower limit
          udetpar(43) = emc_tof_max ! bitp upper limit
          udetpar(44) = emc_tof_step ! bitp stepsize
          udetpar(45) = 0.0     !
          udetpar(46) = 0.0     !
          udetpar(47) = 0.0     !
          udetpar(48) = 0.0     !
          udetpar(49) = 0.0     !
          udetpar(50) = FLOAT(emc_ind1_max_gl) ! bitp tower ind. 
          udetpar(51) = FLOAT(emc_ind2_max_gl) ! bitp tower ind. 
          udetpar(52) = FLOAT(emc_iwall_max) ! 
          udetpar(53) = FLOAT(emc_itype_max) ! 
          udetpar(54) = FLOAT(emc_i1_max) ! 
          udetpar(55) = 0.0     !
          udetpar(56) = 0.0     !
          udetpar(57) = 0.0     !
          udetpar(58) = 0.0     !
          udetpar(59) = 0.0     !
          udetpar(60) = FLOAT(emc_itrack_max) ! 
          udetpar(61) = FLOAT(emc_spart_max) ! 
          udetpar(62) = FLOAT(emc_ncycle_max) ! 
          udetpar(63) = 0.0     !
          udetpar(64) = 0.0     !
          udetpar(65) = emc_cutgam ! 
          udetpar(66) = emc_cutele ! 
          udetpar(67) = emc_cutneu ! 
          udetpar(68) = emc_cuthad ! 
          udetpar(69) = emc_cutmuo ! 
          udetpar(70) = 0.0     !
          udetpar(71) = 0.0     !
          udetpar(72) = 0.0     !
          udetpar(73) = 0.0     !
          udetpar(74) = 0.0     !
          udetpar(75) = 0.0     !
          udetpar(76) = 0.0     !
          udetpar(77) = 0.0     !
          udetpar(78) = 0.0     !
          udetpar(79) = 0.0     !
          udetpar(80) = emc_response_option     !

          call gsdetu('EMC ',v_i_name,nudetpar,udetpar)


      ENDDO                        ! Loop over iwall = 7,8 lead glass

c   End of detector geometry set up

9999  continue

      return

999   continue
      write(6,1000)

1000  format(/'emc - read error in emc_par segment'/,3x,
     1   '  Namelist mis-match in emc_par segment ?',//,3x,
     2   'The geometry will be re-read to pinpoint the erroneous',
     3   ' line',//)
        
      rewind( itf_lun )
      read( itf_lun, nml = emc_par )
      read( itf_lun, nml = hall_par )
      
      stop 'emc -  PISA stopped because of geometry error.'
      end

c**********************************************************************8

