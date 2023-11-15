C$Id: svx.f,v 1.68 2017/02/24 18:59:06 hubert Exp $
C     File name: svx.f      ( was previously inr.f)
C     ------xxx----------
C
C     Original author: Shaheen Tonse (LLNL)
C     Creation date: March 18, 1993
C
C     Purpose: Set up the Silicon Vertex Detector (SVX)
C
C     Revision History:
c     """"""""""""""""
C     C.F. Maguire       June 28, 2001 Convert to use for PHENIX upgrades group
c
c     V. L. Rykov        26-Aug-2003: 
c              1) The vertex detector encloser ("envelope", cooling bag)
c                 is introduced.
c              2) For the barrel VTX, "plain" Si cylinders are replaced with
c                 something more realistic (ladders and sensors).
c              3) Set of hit components extended with the "wish list", but,
c                 for the time being, only 8 the first components are stored.
c              4) The gains for position resolution set for resolution 0.1 mkm.
c     V. L. Rykov        03-Sep-2003:
c              Using the full (extended) set for hit components.
c     V. L. Rykov        19-Feb-2004:
c              inrORIG & inrFACT adjusted for geting rid of the bad diagnostic
c              due to endcap, which still is not splitted into sensors.
c     V. L. Rykov        29-Mar-2004:
c              Fixed the bug in the passive layer definition.
c     V. L. Rykov        16-Apr-2004:
c              SVX cage and barrel parameters along with the sensor rotation
c              matrices and translation vestors are written into the
c              svxPISA.par file.
c     V. L. Rykov        21-Apr-2004:
c              Fixed some strange volume interference of SVX & MUI first
c              reported by Hua Pei on 04/19/2004.
c     Hubert van Hecke, Aug/Sep 2004: replaced endcap geometry with detailed lampshade
c             description from Gerd Kunde, Michael Malik and Jan Boissevain
c             Moved the material/medium numbers into the MVD range
c     Hubert van Hecke, Jan 2005: shrunk middle 4 endcaps, added support lampshadeshade 
c             (SISR) with pipes and cables. Added cooling tubes to the barrel.
c     Hubert: added a 'dummy silicon plane' in the location of South and North Mutr 
c             stations 1. This allows simple cuts on surviving particles. 
c             Switched on/off with new phnx.par variable station1_dummy. Aug 2005
c     Hubert: added option to install only 1/4 of the North endcaps, switched with
c             variable quarter_section in the phenx.par file
c     Hubert: Bug fix: declared SIP(S,M,B) to be 'MANY', since they are overlapping
c     Hubert: Added switch (sili_endcap_type) to make 'flat' endcaps.
c              0=no endcaps, 1=umbrella endcaps, 1=flat endcaps.   06 Jan 2006 (w/ Sasha)
c              3=titlted endcap pixel ('ldrd') pixel planes, 4=flat ldrd planes 
c     Sasha L: Added misalignment for the barrel (07 September 2006)
c     Hubert:  Added global in/out coordinates to the output (18 sep 2006)
c     Sasha L: staggered geometry for barrel strips (layers 3 and 4)
c     Hubert:  removed 'station1' option, replaced by 'stagger' option to give stations
c              small, staggered rotations. (13 Dec 2006). 
c     Hubert:  Fixed S support cone orientation (sisr#2), moved cooling tubes to central
c              up/down sectors.
c     Hubert:  Removed cooling tubes from SISR, 1 of 2 SJCC, SIIC, SBX1,2,3. Enlarged 
c              SISP, thinned SICB,M,S 3->2mm. Enlarged SISP's. SICT 3->4mm. Jan 2007
c              Added support posts SISQ for SISP's, and cooling manifolds SJSP and SJSQ
c              Removed code for sili_endcap_type = 1
c              Introduced material 'honeycomb-1' for support structures. 
c     Hubert:  Enlarged the master volume SIEN to hold the readout wheels at +-Z. Added
c              readout wheel volume SIWH, with pc boards, connectors, cables rohacell.
c     Dave Lee FVTX geometry updated to reflect TDR 9-10-07  Type 1 = FVTX, type
C              2 = LDRD 	
C     Hubert:  Lifted out the FVTX into svx_fvtx.f, and IFVTX into svx_ifvtx.f. This file
c              now constructs the barrels only, plus common support structures, plus 
c              the big readout wheels.
c     Maki Kurosawa constructed the no-tilt layers 3,4, plus new cooling and omega 
c              pieces. March-April 2008 
c     Hubert:  Redefine endcap cage, according to apr 08 drawings.
c     Maki:    21-Jun-2008: Implemented realistic ROC3 material for strips (layer 3 and 4)
c     Maki:    18-Aug-2008: Implemented realistic material for pixels (layer 1 and 2)
c              Modified coolant medium and the shape of omega piece
c     Maki:    29-Jun-2009: Implemented realistic geometry of stave for strips (layer 3 and 4)
c     Maki:    25-Aug-2009: Fixed insensitive area at outer stripixel layers (layer 3 and 4)
c     Maki:    27-Jul-2010: Symmetrize about y-z plane
c     Hubert:  8 Nov 11: move SLCC (FVTX cage) to svx_fvtx.f
c     Hubert:  Jan 2012 Overlap fixes: SJCC-SCMN, SJSQ-SJSP
c     Hubert:  Feb 2014 Added vtx_shiftx,y,z to allow small shifts of the 4 barrels. 
c     Hubert:  Mar 2014 Reorganized the the volumes to encapsulate all ladder elements
c     Hubert:  May 2015 Fixed mass and geometry of SISP,SISQ,SJSP,SJSR,SICC (removed SJCC,SKCC)
c              Note rhicrun is used to add stainless couplings in 2014, 2015
c     Hubert:  Apr 2016: run12 VTX East was removed, added SISO, plastic for ladder clips, 
c              and B1,2 cable connectors SJCC, adjusted SICC size (B1,2 Kapton cables).
c
*=====================================================================================

      SUBROUTINE SVX(FULL,NH)
 
      Implicit none
 
C---  Formal Argument Declarations
C     ----------------------------
c...  Input (?):
      character*4 full            ! set before call in gugeom
C...  Output: number of components of a hit
      integer*4   nh              ! set before call in gugeom
C
C---  External Functions
C     ------------------
C
C     ================================================================
C---  Global Declarationa
C     ================================================================
#include "guphnx.inc"
#include "gclist.inc"
#include "gconst.inc"  
#include "gcflag.inc"
#include "gugeom.inc"
#include "gcvolu.inc"

C  need to access zebra to write parameters to FZOUT file

#include "fstore.inc"
#include "sublink.inc"
#include "fpdlink.inc"

C
C     ================================================================
C---  Local declarations of the input data from phnx.par file
C     ================================================================

*---- NOTE: namelist sili_cg_par appears in svx_fvtx.f AND svx.f ----- make sure it is the same
c---  VTX Envelope/Cage parameters: Volumes SIEN(outer)/SICG(inner)
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Integer sili_cg_npcon  /6/      ! Number of corners for SIEN's PCON
      Real    sili_cg_z(6)   /6*0.0/  ! z-pos. of the Cage corners
      Real    sili_cg_rmn    /2.2/    ! Inner cage radius, cm
      Real    sili_cg_rmx(6) /6*0.0/  ! Outer SIEN radii at the corners, cm
      Real    sili_cg_thck   /0.5/    ! Cage wall thickness, cm
      Real    sili_cg_inthck /0.2/    ! Thickness of the beam pipe ins., cm
      Real    sili_cg_xdisp  /0.0/    ! x-displacement of SIEN in HALL, cm
      Real    sili_cg_ydisp  /0.0/    ! y-displacement of SIEN in HALL, cm
      Real    sili_cg_zdisp  /0.0/    ! z-displacement of SIEN in HALL, cm
      Real    sili_cg_tempc  /0.0/    ! Temperature inside the Cage, deg. C
      real    sili_cg_rinner          ! part of fvtx cage definition
      real    sili_cg_swedge_len      !    ''      note thet these are copied from
      real    sili_cg_bwedge_len      !    ''      the endcap namelist.
      real    sili_cg_support_thk     !    ''      
      real    fcg_z1, fcg_z2, fcg_z3, fcg_r1, fcg_r2, fcg_t, 
     &        fcg_z                   ! forward vertex cage variables
      Integer sili_endcap_config      ! fvtx (1), ifvtx (2) or none (0)

      namelist /sili_cg_par/ sili_cg_npcon,sili_cg_z
     &     ,sili_cg_rmn,sili_cg_rmx,sili_cg_thck,sili_cg_inthck
     &     ,sili_cg_xdisp,sili_cg_ydisp,sili_cg_zdisp
     &     ,sili_cg_tempc, sili_cg_rinner, sili_cg_swedge_len,
     &     sili_cg_bwedge_len, sili_cg_support_thk,
     &     fcg_z1, fcg_z2, fcg_z3, fcg_r1, fcg_r2, fcg_t, fcg_z,
     &     sili_endcap_config
*---  END namelist sili_cg_par -------------------------------------------------------------

cc---  VTX Envelope/Cage parameters: Volumes SIEN(outer)/SICG(inner)
cc     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
c      Real    sili_cg_rmn    /2.2/    ! Inner cage radius, cm
c      Real    sili_cg_thck   /0.5/    ! Cage wall thickness, cm
c      Real    sili_cg_inthck /0.2/    ! Thickness of the beam pipe ins., cm
c      Real    sili_cg_tempc  /0.0/    ! Temperature inside the Cage, deg. C
c      Integer sili_cg_npcon  /6/      ! Number of corners for SIEN's PCON
c      Real    sili_cg_z(6)    /6*0.0/ ! z-pos. of the Cage corners
c      Real    sili_cg_rmx(10)/10*0.0/ ! Outer SIEN radii at the corners, cm
c      Real    sili_cg_xdisp  /0.0/    ! x-displacement of SIEN in HALL, cm
c      Real    sili_cg_ydisp  /0.0/    ! y-displacement of SIEN in HALL, cm
c      Real    sili_cg_zdisp  /0.0/    ! z-displacement of SIEN in HALL, cm
c      real    sili_cg_rinner          ! part of fvtx cage definition
c      real    sili_cg_swedge_len      !    ''      note thet these are copied from
c      real    sili_cg_bwedge_len      !    ''      the endcap namelist.
c      real    sili_cg_support_thk     !    ''      
c      Integer sili_endcap_config      ! fvtx (1), ifvtx (2) or none (0)
c      real    fcg_z1, fcg_z2, fcg_z3, fcg_r1, fcg_r2, fcg_t, fcg_alpha, 
c     &        fcg_sina, fcg_cosa, fcg_tana, fcg_z   ! forward vertex cage variables
c
c      namelist /sili_cg_par/ sili_cg_npcon,sili_cg_z
c     &     ,sili_cg_rmn,sili_cg_rmx,sili_cg_thck,sili_cg_inthck
c     &     ,sili_cg_xdisp,sili_cg_ydisp,sili_cg_zdisp
c     &     ,sili_cg_tempc, sili_cg_rinner, sili_cg_swedge_len,
c     &     sili_cg_bwedge_len, sili_cg_support_thk,
c     &     fcg_z1, fcg_z2, fcg_z3, fcg_r1, fcg_r2, fcg_t, fcg_z,
c     &     sili_endcap_config

c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c---  VTX Gas Enclosure parameters:
c     SISF
      Real sili_ec_sf_z(2) /-19.68, 19.68/ ! z-pos. of support frame, cm
      Real sili_ec_sf_rmn  /21.68/         ! Inner support frame radius
      Real sili_ec_sf_rmx  /21.79/         ! Outer support frame radius
c     SIPL
      Real sili_ec_pl_rmn  /21.81/         ! Outer polyeth radius
      Real sili_ec_pl_thk  /0.01524/        ! Thickness of Polyethylene
c     SIER
      Real sili_ec_rh_thk  /0.3175/        ! Thickness of rohacell
c     SIAM
      Real sili_ec_alm_thk  /0.00508/       ! Thickness of aluminum mylar

      namelist /sili_ec_par/ sili_ec_sf_z,sili_ec_sf_rmn
     &     ,sili_ec_sf_rmx,sili_ec_pl_rmn,sili_ec_pl_thk
     &     ,sili_ec_rh_thk,sili_ec_alm_thk
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c---  VTX Barrel parameters
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Integer sili_br_nlayers /8/                    ! Number of barrel layers

      Real    sili_br_snhalfx(20) /.696,19*1.71555/  ! Si sensor width, cm
      Real    sili_br_snhalfy(20) /0.01,19*0.02/     ! Si sensor thickness, cm
      Real    sili_br_snhalfz(20) /2.836,19*3.2291/  ! Si sensor length, cm
      Real    sili_br_bp_thk      /0.0017/           ! ROC3 bias plane thickness cm
      Real    sili_br_psv_thk     /0.12/             ! ROC3 insulator thickness cm
      Real    sili_br_k3_thk(2)   /0.09,0.33/        ! K3 thickness cm
      Real    sili_br_tb_thk(2)   /0.0381, 0.028/    ! Cooling u-tube thickness for stripixel cm
      Real    sili_br_stv_thk(3)  /0.05, 0.0410, 0.0408/   ! ROC3 stave thickness cm
      Real    sili_br_bus_thk(2)  /0.0178, 0.0054/   ! BUS thickness for stripixel cm
      Real    sili_br_rcc_thk(2)  /0.1484, 0.0027/   ! RCC thickness for stripixel cm
      Real    sili_br_tap_thk(2)  /0.0127, 0.0027/   ! TAPER thickness for stripixel cm
      Real    sili_br_rsn_thk     /0.01/             ! CFC resin thickness cm
      Real    sili_br_x0add(20)   /20*0.01/          ! Passive material thickness
c                                              ! in the ladders added on top
c                                              ! of Si sensor, RadLength X0
      Real    sili_br_snzgap(20)  /20*0./      ! Sensor z-gap (if >=0) or
c                                              ! sensor z-overlap (if <0), cm
      Real    sili_br_tilt(20)    /7.8,7.0,0.0,0.0,16*0./ !  Ladder tilts
      Integer sili_br_nsn(20)     /4,19*5/       ! Number of Si sensors/ladder
      Real    sili_br_r(20)   /2.5,5.,10.,14.,0.3,15*0./ ! Radial positions ...
c                                              ! (read carefully!!!)
c                                              ! of the center line,
c                                              ! x=y=0 (loca Si sensor
c                                              ! coordinates), of the Si
c                                              ! sensors in the layers
c                                              !    The fifth parameter is staggering 
c                                              !    in strip layers 3 and 4.
c                                              !    Set it to 0 to get old geometry
      Real    sili_br_z(20)      /20*0./ ! Z-pos. of the ladder centers in SICG
      Real    sili_br_dphi(20)   /29.40,29.40  ! Azim. spacing of ladders, deg.
     *                           ,22.87,18.71
     *                           ,16*0./
      Real    sili_br_stdphi(5,4) /10*0.
     *                            ,28.20, 18.04, 17.36, 16.06, 30.84
     *                            ,24.86, 12.40, 11.60, 12.06, 22.96/
      Integer sili_br_nsec(20)   /20*2/  ! Number of cont. phi-sections/layer
      Real    sili_br_phic(5,20) /0.,180.,3*0. ! Sect. center azimuth pos.
     *                           ,0.,180.,3*0.
     *                           ,0.,180.,3*0.
     *                           ,0.,180.,3*0.
     *                           ,80*0./
      Integer sili_br_nlad(5,20) /5*5    ! Number of ladders/phi-section
     *                           ,5*5
     *                           ,5*7
     *                           ,5*9
     *                           ,80*1/

      real sili_br_misalignment(8) /8*0./ ! misalignment in X-Y plane (cm), east arm first
      real sili_br_shiftstag

      logical lbarrel /.true./           ! allows the barrel to be switched on/off
      integer sili_wheel_yesno /1/       ! build readout wheels or not
      integer sili_use_align /0/         ! version number for database ladder alignment records

c     ======================================================================
c     The parameters are for support rings at the end of the ladders
      Integer sili_br_nspring     /2/          ! Number of rings
      Real    sili_br_spz(20) /-17.,17.,18*0./ ! Z-positions, cm
      Real    sili_br_sprin(20)  /20*2.4/      ! Inner radii, cm
      Real    sili_br_sprout(20) /20*10./      ! Outer radii, cm
      Real    sili_br_spthck(20) /20*0.25/     ! Half-hickness, cm
      Real    sili_br_manz(20) /-17.,17.,18*0./ ! Z-positions cooling manifold
      Real    sili_br_manr(20)  /20*2.4/       ! mean radius cooling manifold
      Real    sili_br_mandia(20) /20*10./      ! diameter cooling manifold
      real    pixel_hdi_thk/.01/
      real    pixel_hdi_phimin/110./
      real    pixel_hdi_phimax/250./ 
      real siwh_ppcb_half(3)  ! pixel pcb half-thickness = 1/32" 
      real siwh_spcb_half(3)  ! strip pcb half-thickness = 1/32" 
      real siwh_fpcb_thick    ! fvtx pcb half-thickness = 1/32" 
      real siwh_pcb_z(10)     ! pcb center positions     
      real siwh_pcb_rmax      ! pcb max radius        
      real siwh_pcb_connz     ! pcb connector half-z
      real siwh_pcb_suppz(10) ! pcb support/cooling half-z
      real siwh_roha1_thick   ! inner rohacell cover half-thickness
      real sili_br_ct_radius  ! cooling tube radius
      real sili_endcap_z(8)   ! to place barrel cables, endcap roha cage
      character*40 vtx_alignment_file1, vtx_alignment_file2

c     ======================================================================
      namelist /sili_br_par/ sili_br_nlayers,sili_br_r,sili_br_z
     $     ,sili_br_nsn,sili_br_snhalfx,sili_br_snhalfy,sili_br_snhalfz
     $     ,sili_br_bp_thk,sili_br_psv_thk,sili_br_k3_thk,sili_br_tb_thk
     $     ,sili_br_stv_thk,sili_br_bus_thk,sili_br_rcc_thk
     $     ,sili_br_tap_thk,sili_br_rsn_thk,sili_br_x0add,sili_br_snzgap
     $     ,sili_br_nsec,sili_br_nlad,sili_br_phic,sili_br_dphi
     $     ,sili_br_stdphi,sili_br_tilt
     $     ,sili_br_nspring,sili_br_spz,sili_br_sprin
     $     ,sili_br_sprout,sili_br_spthck
     &     ,sili_br_manz,sili_br_manr ,sili_br_mandia
     $     ,sili_br_misalignment,
     &     sili_wheel_yesno,siwh_ppcb_half,siwh_spcb_half,
     &     siwh_fpcb_thick, siwh_pcb_z, siwh_pcb_rmax,
     &     siwh_pcb_connz, siwh_pcb_suppz, siwh_roha1_thick,
     &     sili_br_ct_radius, sili_endcap_z, sili_use_align,
     &     vtx_shiftx, vtx_shifty, vtx_shiftz,
     &     vtx_alignment_file1, vtx_alignment_file2
      real vtx_shiftx, vtx_shifty, vtx_shiftz
C     ================================================================
C---  Local definitions
C     ================================================================
c     Input filename
      character*50 svxpar_file /'svxPISA.par'/    ! Output file for parameters
      character*4  set_id      /'SVX '/           ! Detector/hit set ID
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
c     VTX cage volume names
      character*4 siliEnvelope/'SIEN'/  ! Envelope (outer cage surface)
      character*4 siliCage    /'SICG'/  ! Cage inner surface
      character*4 siliSupport /'SISP'/  ! Barrel support rings
      character*4 siliClips   /'SISO'/  ! Support clips
      character*4 siliEcSppFr /'SISF'/  ! Gas Enclosure - Support frame
      character*4 siliEcPoly  /'SIPL'/  ! Gas Enclosure - Polyethylene
      character*4 siliEcRoha  /'SIER'/  ! Gas Enclosure - Rohacell
      character*4 siliAlMylar /'SIML'/  ! Gas Enclosure - Aluminum Mylar
      character*4 siliPCB     /'SIPB'/  ! PCB on Big Wheel

c     VTX, FVTX layer names (for barrel, names of ladders in a layer)
      character*4 siliNames(20) /'SI01', 'SI02', 'SI03', 'SI04',
     &                           'SI05', 'SI06', 'SI07', 'SI08',
     &                           'SI09', 'SI10', 'SI11', 'SI12',
     &                           'SI13', 'SI14', 'SI15', 'SI16',
     &                           'SI17', 'SI18', 'SI19', 'SI20'/
      character*4 siljNames(04) /'SJ01', 'SJ02', 'SJ03', 'SJ04'/  ! ladder names

      character*4 siliStSupp(4) /'SP01', 'SP02', 'SP03', 'SP04'/  ! only sp03,4 exist
      character*4 siliBrSensor  /'SISN'/  ! Barrel sensor name
      character*4 siliBrRead    /'SIRC'/  ! Barrel readout chip
      character*4 siliBrResin   /'SIRS'/  ! Barrel resin
      character*4 siliBrResinn  /'SIRR'/  ! Barrel resin
      character*4 siliBrK3      /'SIK3'/  ! Barrel k3 side
      character*4 siliBrTube    /'SITB'/  ! Barrel cooling tube
      character*4 siliBrCool    /'SICL'/  ! Barrel cooling tube
      character*4 siliBrBusp    /'SIBS'/  ! Barrel Bus (Pixe)
      character*4 siliBrBus1    /'SIB1'/  ! Barrel Bus (Strip)
      character*4 siliBrBus2    /'SIB2'/  ! Barrel Bus (Strip)
      character*4 siliBrRCC1    /'SIR1'/  ! Barrel RCC
      character*4 siliBrRCC2    /'SIR2'/  ! Barrel RCC
      character*4 siliBrRCC3    /'SIR3'/  ! Barrel RCC
      character*4 siliBrRCC4    /'SIR4'/  ! Barrel RCC
      character*4 siliBrRCC5    /'SIR5'/  ! Barrel RCC
      character*4 siliBrStave   /'SIST'/  ! Barrel stave
      character*4 siliBrBPlane  /'SIBP'/  ! Barrel bias plane
      character*4 siliBrAPlane  /'SIAP'/  ! Barrel analog ground and power plane
      character*4 siliBrDPlane  /'SIDP'/  ! Barrel digital ground plane
      character*4 siliBrPassive /'SIPV'/  ! Barrel strips passive material name
      character*4 siliBrPassivePix /'SIPP'/  ! Barrel pixels passive material name (does not exist?)
      character*4 siliBrVGPlane /'SIVG'/  ! Barrel vdd and digital plane material name
      character*4 siliBrSGPlane /'SISG'/  ! Barrel signal plane material name
      character*4 siliBrOmega   /'SIOM'/  ! Barrel omega shape name
      character*4 siliBrOmegap  /'SIOP'/  ! Barrel omega piece name
      character*4 siliBrSVX4    /'SVX4'/  ! Barrel svx4 name
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Integer     nwpa      /500/                    ! Init. size of HITS banks
      Integer     nwsa      /500/                    ! Init. size of DIGI banks
      Integer     idtype    /2001/                   ! User def. detector type

      Integer     nbrv      /5/                      ! Num. of br. vol. desc.
      Integer     necv      /9/                      ! Num. of endcap vol. desc.
      Integer     nv        /7/                      ! max(nbrv,necv)
      Integer     nbitsv(7) /7*8/                    ! Bits to pack vol. copy #


      Character*4 namesv(7) /'HALL','SIEN','SICG',
     &     'SVxx','SJ0x','SI0x','SISN'/              ! Barrel volume names,  x=1-4

c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c   The following are used in GSDETH
c   """"""""""""""""""""""""""""""""
c   Hit parameters will be global position(3), energy loss, time of flight,
c   particle type and 
c   entry momentum(3), local in & out positions(6),
c
      Integer     nhh         /21/                   ! Number of hit components
      integer*4 inrNBITSH(21) /21*32/                ! Bits for packing the hits
c   Hit component names
      character*4 inrNMSH(21) /'POSX','POSY','POSZ'  ! Global positions
     &     ,'DELE','TOFL'                            ! Energy loss & TOF
     &     ,'P_ID','MOMX', 'MOMY', 'MOMZ'            ! Particle ID & Entry mom.
     &     ,'XILC','YILC','ZILC','XOLC','YOLC','ZOLC'  ! Local entry & exit
     &     ,'XIGL','YIGL','ZIGL','XOGL','YOGL','ZOGL'/ ! global entry & exit

c     Default setting of offsets and gains
      REAL inrORIG(21) /3*1000.,3*0.,3*1000.,6*1000.,6*1000./       ! offsets
      REAL inrFACT(21) /3*100000.,1.E7,1.e12,1.0,3*100000.
     &     ,6*100000.,6*100000./                                    ! gains
c
c     The above gains give
c              - 0.1 keV energy deposition resolution
c              - 0.0001 mm position resolution
c              - 0.01 MeV/c momentum resolution
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c     Rotation matrices
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Integer irot_cage        ! Cage (SIEN) rotation matrix #

C     ================================================================
C---  Local definitions of materials/media
C     ================================================================
      Integer sili_med_silicon /10/     ! Sensitive silicon nmed (nmat=50,Si)
      Integer sili_med_passilicon /11/     ! Passive silicon nmed (nmat=50,Si)
      Integer sili_med_copper  /15/     ! Ladder passive nmed    (nmat=11,Cu)
      Integer sili_med_passive /26/     ! Ladder passive nmed    (nmat=09,Al)
      Integer sili_med_cg      /120/    ! Cage (SIEN-SICG) nmat/nmed (Rohacell)
      Integer sili_med_coldair /121/    ! Gas inside the SICG (cold air)
      Integer sili_med_gfrp    /122/    ! GFRP for the fake support
      integer sili_med_carbon  /123/    ! carbon-carbon composite
      integer sili_med_coolant /124/    ! C5F12 liquid coolant
      integer sili_med_honeycomb /125/  ! 1/4" honeycomb, .5mm c-c skin, Al core
      integer sili_med_m55j    /126/    ! M55J
      integer sili_med_hdi       /108/  !
      integer sili_med_resin   /127/    ! CFC resin
      integer sili_med_cfiber  /128/    ! carbon-fiber
      integer sili_med_kapton  /129/    ! kapton
      integer sili_med_k3      /130/    ! Allcomp K3
      integer sili_med_poly    /131/    ! Polyethylene
      integer sili_med_peek    /60/     ! PEEK

c     Material for the cage, volumes SIEN-SICG
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     Copied from pisa200/src/ver/vermatdef.f, Rev 1.2
C     Rohacell(H11-C8-N1-O12) Mixture Parameters for the vertex detector:
      REAL AROHA(4)/  1.008 , 12.01  , 14.008 , 16.  / ! A for Rohacell
      REAL ZROHA(4)/  1.    ,  6.    ,  7.    ,  8.  / ! Z for Rohacell
      REAL WROHA(4)/ 11.    ,  8.    ,  1.    ,  2.  / ! Weights for Rohacell
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                       ! kapton cable mixture:
      REAL ACABLE(5) / 12.01, 14.008, 16.00, 1.008, 63.54 /                     
      REAL ZCABLE(5) /  6.  ,  7.   ,  8.  , 1.   , 29.   /                     
      REAL WCABLE(5) / .21  ,  .03  , .091 , .013 , .655  /                     
      REAL DENCABLE / 2.74/                                                     
C
C     ================================================================
c---  Local work variables (counters, etc)
C     ================================================================
      CHARACTER*20 NAMTMED
      INTEGER NMAT, ISVOL, IFIELD, NWBUF, LNAM(7) 
      integer LNUM(7) /1,1,1,1,1,1,1/
      REAL FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN
     &     ,WMAT(4),UBUF(1),ANUMB,ZNUMB,DENS,RADL,ABSL
      CHARACTER*80 CHFORM
      character*4  v_m_name,v_i_name, v_j_name, sil_name,
     &      name_ew
      integer iLayer, iPoint, icnt, nr, npar, nmed, ivolu, iset, idet, 
     &   iod, wedges, nladd, iladd, isec, ksec, isen, nsensors, ivol1, 
     &   ierr, icopy, irottop,  irotbot, irot1, irot2, irot3, irotrcc,i,
     &   nsvx, nsvxr, nstvr, nk3r, ntbr, nclr, nbusr, nrccr, irot_3_4,
     &   njtube, nktube, idouble, isign, imanp, imant, nsjsq, nsisq,
     &   fvtx_config, imass

      real dim_sili(40), philadd, dphid, dphir, dphit, sgn,
     & phisupp, dthck, phirotladd, rladd, xladd, yladd, zladd,
     & xladd2, yladd2, rladd2, shift, delphi, rjsupp, 
     & xjsupp, yjsupp, zrcc, phirotladdtmp,zrotladd,
     & ladd_halfx, ladd_halfthck, supp_halfthck,
     & ladd_halfz, pasv_radl, pasv_halfy,
     & b_halfx,agv_halfx,dg_halfx,s_halfx,r_halfx,sig_halfx,vg_halfx,
     & b_halfy,agv_halfy,dg_halfy,s_halfy(3),r_halfy,sig_halfy,vg_halfy,
     & bus_halfx, bus_halfy(2), rcc_halfx, rcc_halfy(2),
     & tap_halfx, tap_halfy(2),
     & k31_halfx, k32_halfx, k33_halfx, k34_halfx,
     & k31_halfy, k32_halfy, k33_halfy, k34_halfy,
     & tb1_halfx, tb2_halfx, tb3_halfx, tb4_halfx,
     & tb1_halfy, tb2_halfy, tb3_halfy, tb4_halfy,
     & sen_y(2), par(26), rcool, a_cool(4), z_cool(4),
     & posx, posy, posz, a_honey(2), z_honey(2), 
     & siwh_rmin, siwh_rmax, bwedge_lowx, bwedge_highx, swedge_lowx,
     & swedge_highx, aangle, bangle, cstep, ssil_lowx, ssil_highx,
     & ssil_len, wedge_thk, back_planthk, hdithk, silthk, routerb,
     & routers, stationzthick, bsil_lowx, bsil_highx, bsil_len, 
     & phictladd(2),dphict(2),rctladd(2),xctladd(2),yctladd(2),
     & phiomladd(4),dphiom(4),romladd(4),xomladd(4),yomladd(4),
     & shiftstag(2),phitmpladd,ctdist,
     & xomega(2), yomega(2), romega(2), a_omega(4), z_omega(4),
     & xomegap(2), yomegap(2), romegap(2), phiompladd(2),dphiomp(2),
     & a_kapton(4), z_kapton(4), a_poly(2), z_poly(2),
     & sir4x, sir4y, sir4z, sir5x, sir5y, sir5z, sj12_y, sj_halfy,
     & rholder(5), x1,y1,z1,x2,y2,z2

      data rholder/2.3, 4.0, 9.0, 14.7, 19.7/ 
      
      real rmyres,rmyrndm,rmydum
      integer rmyrndmi

      integer itf_lun       !   geometry description logical unit   
      common /interface/itf_lun
      real xdel(24), ydel(24), zdel(24)       ! data base values returned here
      common /laddalign/ xdel, ydel, zdel     ! This common block maps onto C struct
      character*7 object                      ! db object name, like 'VTX L1W'

      integer skip_sector, skip_ladder
      integer sec_flag, lad_flag
      logical lfound /.false./
      logical ex1  /.false./
      logical ex2  /.false./
      integer iline
      character*80 line1, line2
      character*4 cvol_opt
*------------------------------------------------------------------------------------
* for the geometry alignment interface:
      real del_layer1(10,4,4)     ! ladder, chip, x-y-z-phi
      real del_layer2(20,4,4)     ! ladder, chip, x-y-z-phi
      real del_layer3(16,5,4)     ! ladder, chip, x-y-z-phi
      real del_layer4(25,5,4)     ! ladder, chip, x-y-z-phi
      integer kdum, klayer, kladder, ksensor
      real delx, dely, delz, delph,deltas(4)
C======================================================================!
C*************************** EXECUTABLE CODE***************************!
C======================================================================!
C
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c BEGIN
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c
c       Read the geometry file segments
c
      write(6,'(//,''SVX - reading VTX parameters from phnx.par'')')

      rewind( unit = itf_lun )
      read( itf_lun, nml = sili_br_par, err = 997 )    ! blocks in alphabetic order
      read( itf_lun, nml = sili_cg_par, err = 998 )    ! unless you do a rewind
      read( itf_lun, nml = sili_ec_par, err = 999 )
      write (6,*)'RHIC run number (from pisa.kumac): ',rhicrun
      if (rhicrun.lt.11) then
        write (6,*)' There was no VTX/FVTX beforre run 11'
        return
      endif
C
C  only book volumes if input parameters are OK
C
      write(*,'(a16,5a5)') 'SVX CVOLU_OPT = ',
     &  CVOLU_OPT(1,3),CVOLU_OPT(2,3),CVOLU_OPT(3,3),
     &  CVOLU_OPT(4,3),CVOLU_OPT(5,3)

       call uhtoc(cvolu_opt(1,3),4,cvol_opt,4)      ! convert Hollerith to Ascii
      IF(cvol_opt.EQ.'BARR') continue
      write(*,*) ' sili_endcap_config = ', sili_endcap_config

      IF(cvol_opt.EQ.'FULL'.OR.
     &   cvol_opt.EQ.'VOLS'.OR.
     &   cvol_opt.EQ.'BARR')    THEN

        NH = nhh             ! Number of hit components to NH output parameter
        nv = max(nbrv, necv) ! Number of volume descriptors

C********+*********+*********+*********+*********+*********+*********+**
C       Define mixture ROHACELL for the SIEN
C       Copied from pisa200/src/ver/vermatdef.f, Rev 1.2
C
        nwbuf   = 1     ! number of user words in GSMATE calls
        NMAT    = sili_med_cg   ! Rohacell
        CALL GSMIXT (NMAT,' ROHACELL$',AROHA,ZROHA,0.075,-4,WROHA)
C
C       Tracking media # sili_cg_wall - Rohacell
C     
        NMED    = sili_med_cg   ! Rohacell
        ISVOL   = 0             ! Not  sensitive
        IFIELD  = 1             ! Magnetic field
        FIELDM  = 10.0          ! max field
        TMAXFD  = 45.0          ! maximum angle due to field (one step) in deg
        DMAXMS  = 0.2           ! max disp. due to mulsct. in one step (cm)
        DEEMAX  = 0.1           ! max fractional energy loss in one step
        EPSIL   = .001          ! tracking precision (cm)
        STMIN   = 0.5           ! min step due to e loss or mulsct. (cm)
        UBUF(1) = 0.            ! tracking stop switch
        CALL GSTMED(NMED,'Rohacell$',NMAT,ISVOL,IFIELD
     1       ,FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)

        NMAT    = sili_med_hdi  ! 108 Kapton cables
        CALL GSMIXT (NMAT,' ROHACELL$',ACABLE,ZCABLE,DENCABLE,5,WCABLE)
C                                                                               
C     Tracking media # 108 -- MVD/VTX/FVTX/IFVTX cables (kapton + copper)                      
C                                                                               
        NMED    = sili_med_HDI   ! HDI                                            
        ISVOL   = 0     ! Not sensitive                                           
        IFIELD  = 1     ! Magnetic field                                          
        FIELDM  = 5.0   ! max field                                               
        TMAXFD  = 45.0  ! maximum angle due to field (one step) in degrees        
        DMAXMS  = 0.2   ! max disp. due to mulsct. in one step (cm)               
        DEEMAX  = 0.1   ! max fractional energy loss in one step                  
        EPSIL   = .001  ! tracking precision (cm)                                 
        STMIN   = 0.5   ! min step due to e loss or mulsct. (cm)                  
        UBUF(1) = 0.    ! tracking stop switch                                    
        CALL GSTMED(NMED,'HDI$',NMAT,ISVOL,IFIELD,FIELDM,TMAXFD,                  
     &             DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)                          

c     Define material 'AIRCOLD' at 0C
        nmat = sili_med_coldair
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
*       write (6,*)' aircold:'
        if (anumb.ne. -1.0)  goto 993               ! if so, abort.
        anumb = 14.61
        znumb = 7.3
        dens  = 1.293e-3*273./(sili_cg_tempc+273.)
        radl  = 0.283e5*(sili_cg_tempc+273.)/273
        absl  = 0.696e5*(sili_cg_tempc+273.)/273.
        ubuf(1) = sili_cg_tempc
        nwbuf = 1
        CALL GSMATE(nmat,'AIRCOLD',anumb,znumb,
     1              dens,radl,absl,ubuf,nwbuf)
        nmed = sili_med_coldair ! Air at temperature sili_cg_tempc C
        isvol = 0               ! Not sensitive
        ifield = 1              ! magnetic field
        fieldm = 10.            ! max field, kGs
        tmaxfd = 0.2            ! max angle due to field (one step) in degrees
        dmaxms = 0.1            ! max disp. due to mulsct. in one step, cm
        deemax = 0.01           ! max fractional energy loss in one step
        epsil  = 0.001          ! tracking precision, cm
        stmin  = 0.1            ! min step due to e-loss or multsct., cm
        ubuf(1) = 0.            ! tracking stop switch
        CALL GSTMED(nmed,'AIRCOLD$',nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Define material/media 'GFRP' (slightly modified G10 from ../itr/pc1gem.f)
        nmat  = sili_med_gfrp                       ! 
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
        if (anumb.ne. -1.0)  goto 993               ! if so, abort.
        anumb = 18.14                               ! NOTE that an unsuccessful
        znumb = 9.065                               ! call to GFMATE can clobber
        dens  = 1.68                                ! the returned variables, so we 
        radl  = 25.                                 ! set them all AFTER calling GFMATE
        absl  = 56.7                                ! Jan 2007 HvH
        ubuf(1) = 0.
        nwbuf   = 1
        CALL GSMATE(nmat,'GFRP',anumb,znumb,dens
     1       ,radl,absl,ubuf,nwbuf)
        nmed   = sili_med_gfrp
        isvol  = 0              ! Not sensitive
        ifield = 1              ! magnetic field
        fieldm = 10.            ! max field, kGs
        tmaxfd = 0.2            ! max angle due to field (one step) in degrees
        dmaxms = 0.1            ! max disp. due to mulsct. in one step, cm
        deemax = 0.01           ! max fractional energy loss in one step
        epsil  = 0.001          ! tracking precision, cm
        stmin  = 0.1            ! min step due to e-loss or multsct., cm
        ubuf(1) = 0.            ! tracking stop switch
        CALL GSTMED(nmed,'GFRP$',nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)


c  Define material/media 'Carbon-carbon', for endcap composite panels
        nmat  = sili_med_carbon ! = 123
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
*       write (6,*)' carbon:',ubuf,nwbuf
        if (anumb.ne. -1.0)  goto 993               ! if so, abort.
        anumb = 12.01           ! from call to gpmate(0)
        znumb =  6.00           !
        dens  =  1.78           ! from Hytec report (default=2.265)
        radl  = 23.9            ! scaled up from density
        absl  = 63.5            ! scaled up from density
        ubuf(1) = 0.
        nwbuf   = 1
        CALL GSMATE(nmat,'Carbon-carbon$',anumb,znumb,dens
     &       ,radl,absl,ubuf,nwbuf)

        nmed   = sili_med_carbon
        isvol  = 0              ! Not sensitive
        ifield = 1              ! magnetic field
        fieldm = 10.            ! max field, kGs
        tmaxfd = 0.2            ! max angle due to field (one step) in degrees
        dmaxms = 0.1            ! max disp. due to mulsct. in one step, cm
        deemax = 0.01           ! max fractional energy loss in one step
        epsil  = 0.001          ! tracking precision, cm
        stmin  = 0.1            ! min step due to e-loss or multsct., cm
        ubuf(1) = 0.            ! tracking stop switch
        CALL GSTMED(nmed,'Carbon-carbon$',nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  Define material/media 'Freon-coolant'       ! H5C6OF9 (NOVEC 7200)
        nmat  = sili_med_coolant! = 124
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
*       write (6,*)' coolant:'
        if (anumb.ne. -1.0)  goto 993               ! if so, abort. If not, define:
        a_cool(1) =  1.008      ! Hydrogen
        z_cool(1) =  1.000      !
        wmat  (1) =  5          !
        a_cool(2) = 12.010      ! Carbon
        z_cool(2) =  6.000      !
        wmat  (2) =  6          !
        a_cool(3) = 15.999      ! Oxygen
        z_cool(3) =  8.000      !
        wmat  (3) =  1          !
        a_cool(4) = 18.998      ! Fluor
        z_cool(4) =  9.000      !
        wmat  (4) =  9          !
        dens  =  1.43           ! NOVEC 7200
        CALL GSMIXT(nmat,'Freon-coolant$', a_cool, z_cool, dens,-2,wmat)
        nmed   = sili_med_coolant                   ! use parameters from carbon-carbon
        CALL GSTMED(nmed,'Freon-coolant$',nmat,isvol,ifield,fieldm
     &               ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

*       write (6,*)' honeycomb:'
        nmat = sili_med_honeycomb   ! = 125
        a_honey(1) = 12.010     ! Carbon
        z_honey(1) =  6.000     !
        wmat   (1) =  0.97      ! 97% by weigh
        a_honey(2) = 26.98      ! Aluminum
        z_honey(2) = 13.000     !
        wmat   (2) = 0.03       ! 3% by weight
        dens       = 0.251      ! scaled from c-c
        CALL GSMIXT(nmat,'Honeycomb-1$', a_honey, z_honey, dens,2,wmat)
        nmed   = sili_med_honeycomb                   ! use parameters from carbon-carbon
        CALL GSTMED(nmed,'Honeycomb-1$',nmat,isvol,ifield,fieldm
     &               ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  Define material/media 'M55J-omega'       ! M55J (from Hytec report)
        nmat  = sili_med_m55j        ! = 126
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
        if (anumb.ne. -1.0)  goto 993               ! if so, abort. If not, define:
        a_omega(1) = 12.010      ! Carbon
        z_omega(1) =  6.000      !
        wmat  (1) =  0.91        !
        a_omega(2) = 14.0067     ! Nitrogen
        z_omega(2) =  7.000      !
        wmat  (2) = 0.01         !
        a_omega(3) = 15.9994     ! Oxygen
        z_omega(3) =  8.000      !
        wmat  (3) = 0.055        !
        a_omega(4) =  1.00794    ! Hydrogen
        z_omega(4) =  1.000      !
        wmat  (4) = 0.025        !
        dens  =  1.63            ! from Hytec report
        CALL GSMIXT(nmat,'M55J-omega$', a_omega, z_omega, dens,4,wmat)
        nmed   = sili_med_m55j
        CALL GSTMED(nmed,'M55J-omega$',nmat,isvol,ifield,fieldm
     &               ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  Define material/media 'CFC resin', for resin between insulator and stave
        nmat  = sili_med_resin ! = 127
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
        if (anumb.ne. -1.0)  goto 993               ! if so, abort.
        anumb = 12.01           ! from call to gpmate(0)
        znumb =  6.00           !
        dens  =  1.2            ! default=2.265
        radl  = 35.5            ! scaled up from density
        absl  = 94.2            ! scaled up from density
        ubuf(1) = 0.
        nwbuf   = 1
        CALL GSMATE(nmat,'Resin$',anumb,znumb,dens
     &       ,radl,absl,ubuf,nwbuf)

        nmed   = sili_med_resin
        isvol  = 0              ! Not sensitive
        ifield = 1              ! magnetic field
        fieldm = 10.            ! max field, kGs
        tmaxfd = 0.2            ! max angle due to field (one step) in degrees
        dmaxms = 0.1            ! max disp. due to mulsct. in one step, cm
        deemax = 0.01           ! max fractional energy loss in one step
        epsil  = 0.001          ! tracking precision, cm
        stmin  = 0.1            ! min step due to e-loss or multsct., cm
        ubuf(1) = 0.            ! tracking stop switch
        CALL GSTMED(nmed,'Resin$',nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  Define material/media 'Carbon-fiber', for endcap composite panels
        nmat  = sili_med_cfiber ! = 128
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
*       write (6,*)' carbon:',ubuf,nwbuf
        if (anumb.ne. -1.0)  goto 993               ! if so, abort.
        anumb = 12.01           ! from call to gpmate(0)
        znumb =  6.00           !
        dens  =  1.60           ! from Hytec report (default=2.265) 2008/08/06
        radl  = 26.6            ! scaled up from density
        absl  = 70.6            ! scaled up from density
        ubuf(1) = 0.
        nwbuf   = 1
        CALL GSMATE(nmat,'Carbon-fiber$',anumb,znumb,dens
     &       ,radl,absl,ubuf,nwbuf)

        nmed   = sili_med_cfiber
        isvol  = 0              ! Not sensitive
        ifield = 1              ! magnetic field
        fieldm = 10.            ! max field, kGs
        tmaxfd = 0.2            ! max angle due to field (one step) in degrees
        dmaxms = 0.1            ! max disp. due to mulsct. in one step, cm
        deemax = 0.01           ! max fractional energy loss in one step
        epsil  = 0.001          ! tracking precision, cm
        stmin  = 0.1            ! min step due to e-loss or multsct., cm
        ubuf(1) = 0.            ! tracking stop switch
        CALL GSTMED(nmed,'Carbon-fiber$',nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  Define material/media 'Kapton'       ! Kapton Polyimide
        nmat  = sili_med_kapton        ! = 129
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
        if (anumb.ne. -1.0)  goto 993               ! if so, abort. If not, define:
        a_kapton(1) =  1.00794    ! Hydrogen
        z_kapton(1) =  1.000      !
        wmat   (1) =  0.026362   !
        a_kapton(2) = 12.010      ! Carbon
        z_kapton(2) =  6.000      !
        wmat   (2) =  0.691133   !
        a_kapton(3) = 14.0067     ! Nitrogen
        z_kapton(3) =  7.000      !
        wmat   (3) =  0.07327    !
        a_kapton(4) = 15.9994     ! Oxygen
        z_kapton(4) =  8.000      !
        wmat   (4) = 0.209235    !
        dens  =  1.43            !
        CALL GSMIXT(nmat,'Kapton$', a_kapton, z_kapton, dens,4,wmat)
        nmed   = sili_med_kapton
        CALL GSTMED(nmed,'Kapton$',nmat,isvol,ifield,fieldm
     &               ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  Define material/media Allcomp K3
        nmat  = sili_med_k3     ! = 130
        CALL GFMATE(nmat,sil_name,anumb,znumb,      ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
        if (anumb.ne. -1.0)  goto 993               ! if so, abort.
        anumb = 12.01           ! from call to gpmate(0)
        znumb =  6.00           !
        dens  =  0.2            !
        radl  = 213             ! scaled up from density
        absl  = 565.2           ! scaled up from density
        ubuf(1) = 0.
        nwbuf   = 1
        CALL GSMATE(nmat,'K3$',anumb,znumb,dens
     &       ,radl,absl,ubuf,nwbuf)

        nmed   = sili_med_k3
        isvol  = 0              ! Not sensitive
        ifield = 1              ! magnetic field
        fieldm = 10.            ! max field, kGs
        tmaxfd = 0.2            ! max angle due to field (one step) in degrees
        dmaxms = 0.1            ! max disp. due to mulsct. in one step, cm
        deemax = 0.01           ! max fractional energy loss in one step
        epsil  = 0.001          ! tracking precision, cm
        stmin  = 0.1            ! min step due to e-loss or multsct., cm
        ubuf(1) = 0.            ! tracking stop switch
        CALL GSTMED(nmed,'K3$',nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  Define material/media Polyethylene
        nmat  = sili_med_poly   ! = 131 C2H4H2
        call gfmate(nmat,sil_name,anumb,znumb,     ! First check if this material
     &              dens,radl,absl,ubuf,nwbuf)      ! number has already been used:
        if (anumb.ne. -1.0)  goto 993               ! if so, abort.
        a_poly(1) =  1.00794   ! Hydrogen
        z_poly(1) =  1.000
        wmat(1)   =  0.75
        a_poly(2) = 12.010     ! Carbon
        z_poly(2) =  6.000
        wmat(2)   =  0.25
        dens      = 0.941      ! g/cm3
        CALL gsmixt(nmat,'Polyethylene$',a_poly,z_poly,dens,2,wmat)
        nmed      = sili_med_poly
        call gstmed(nmed,'Polyethylene$',nmat,isvol,ifield,fieldm
     &                ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  Define material PEEK
        nmat = sili_med_peek  ! mixture defined in mat_mixt_med.f by Mickey
        nmed = sili_med_peek
        call gstmed(nmed,'PEEK$',nmat,isvol,ifield,fieldm
     &                ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

cxx   call gpmate(0)          ! print all materials
cxx   call gptmed(0)          ! print all media

C********+*********+*********+*********+*********+*********+*********+**

C...  Create outer surface of the cage, SIEN 
        dim_sili(1)  =   0.
        dim_sili(2)  = 360.
        dim_sili(3)  =   sili_cg_npcon
        npar = 3
        Do icnt = 1, sili_cg_npcon
          npar = npar + 1
          dim_sili(npar)  = sili_cg_z(icnt)
          npar = npar + 1
          dim_sili(npar)  = sili_cg_rmn
          npar = npar + 1
          dim_sili(npar)  = sili_cg_rmx(icnt)
        Enddo
        
        v_m_name = 'HALL'
        v_i_name = siliEnvelope     ! = SIEN
        call gsvolu(v_i_name,'PCON',sili_med_cg,dim_sili,npar,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gspos(v_i_name,1,v_m_name,
     *       sili_cg_xdisp,sili_cg_ydisp,sili_cg_zdisp,
     *       irotnull,'ONLY')

C...  Create inner volume of the cage SICG, a cylinder with a hole for the beampipe.
        dim_sili(1) = 0.0
        dim_sili(2) = 360.0
        dim_sili(3) = 2

        dim_sili(4) = sili_cg_z(1)   + sili_cg_thck
        dim_sili(5) = sili_cg_rmn
        dim_sili(6) = sili_cg_rmx(3) - sili_cg_thck

        dim_sili(7) = sili_cg_z(6)   - sili_cg_thck
        dim_sili(8) = sili_cg_rmn
        dim_sili(9) = sili_cg_rmx(3) - sili_cg_thck

        nmed = sili_med_coldair ! Air at temperature sili_cg_tempc C
        v_m_name = siliEnvelope
        v_i_name = siliCage
        
        call gsvolu(v_i_name,'PCON',nmed,dim_sili,9,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gspos(v_i_name,1,v_m_name,0.,0.,0.,irotnull,'ONLY')

        ! Make holder volumes for the East VTX and the West VTX
        dim_sili(1) = -81                    ! West holder volume 
        dim_sili(2) = 2*abs(dim_sili(1))     ! phi, dphi
        dim_sili(3) = 10                     ! nz

        ladd_halfz    = sili_br_nsn(4)*sili_br_snhalfz(4)
     *   + 0.5*sili_br_snzgap(4)*(sili_br_nsn(4)-1)      ! layer 4 Length/2

        dim_sili(4) = -ladd_halfz - 0.1      ! z1
        dim_sili(5) = rholder(4) + 0.4       !   rmin
        dim_sili(6) = rholder(5)             !   rmax
        ladd_halfz    = sili_br_nsn(3)*sili_br_snhalfz(3)
     *     + 0.5*sili_br_snzgap(3)*(sili_br_nsn(3)-1) ! ladder 3 length/2
        dim_sili(7) = -ladd_halfz            ! z2
        dim_sili(8) = dim_sili(5)            !
        dim_sili(9) = dim_sili(6)            !
        dim_sili(10) = dim_sili(7)           ! z3 = z2
        dim_sili(11) = rholder(3) + 0.2      !
        dim_sili(12) = rholder(5)            !
        ladd_halfz    = sili_br_nsn(2)*sili_br_snhalfz(2)
     *     + 0.5*sili_br_snzgap(2)*(sili_br_nsn(2)-1) ! ladder 2 length/2
        dim_sili(13) = -ladd_halfz           ! z4
        dim_sili(14) = rholder(3) + 0.2      !
        dim_sili(15) = rholder(5)            !
        dim_sili(16) = dim_sili(13)          ! z5 = z4
        dim_sili(17) = rholder(1) + 0.2      !
        dim_sili(18) = rholder(5)            !
        dim_sili(19) = -dim_sili(16)         ! z6 = -z5
        dim_sili(20) =  dim_sili(17)         !
        dim_sili(21) =  dim_sili(18)         !
        dim_sili(22) = -dim_sili(13)         ! z7 = -z4
        dim_sili(23) =  dim_sili(14)         !
        dim_sili(24) =  dim_sili(15)         !
        dim_sili(25) = -dim_sili(10)         ! z8 = -z3
        dim_sili(26) =  dim_sili(11)         !
        dim_sili(27) =  dim_sili(12)         !
        dim_sili(28) = -dim_sili(07)         ! z9 = -z2
        dim_sili(29) =  dim_sili(08)         !
        dim_sili(30) =  dim_sili(09)         !
        dim_sili(31) = -dim_sili(04)         ! z10 = -z2
        dim_sili(32) =  dim_sili(05)         !
        dim_sili(33) =  dim_sili(06)         !
                                             ! Make shells to hold the ladders
        do isec = 1, 2                       ! 2 sectors: West, East 
          if (isec.eq.1) then                ! 
            name_ew = 'SVXW'
            call gsvolu(name_ew,'PCON', nmed, dim_sili, 33, ivolu)
            call GSATT (name_ew,'SEEN',1)
            call GSATT (name_ew,'COLO',6)    ! 2=red
            call GSPOS (name_ew,1,'SICG',0.,0.,0.,irotnull,'ONLY')
            write (6,'(''made SVX barrel half-shell '',a4)') name_ew
          elseif (rhicrun.ne.13) then        ! in run13, svx east was removed 
            dim_sili(1) = dim_sili(1) + 180.
            name_ew = 'SVXE'
            call gsvolu(name_ew,'PCON', nmed, dim_sili, 33, ivolu)
            call GSATT (name_ew,'SEEN',1)
            call GSATT (name_ew,'COLO',6)    ! 2=red
            call GSPOS (name_ew,1,'SICG',0.,0.,0.,irotnull,'ONLY')
            write (6,'(''made SVX barrel half-shell '',a4)') name_ew
          endif               
        enddo

C...  Create Gas Enclosure
c     Space Frame
        nmed = sili_med_gfrp    ! G-10
        v_m_name = siliEnvelope
        v_i_name = siliEcSppFr      ! SISF

        dim_sili(1) = 0.0
        dim_sili(2) = 360.0
        dim_sili(3) = 2

        dim_sili(4) = sili_ec_sf_z(1)
        dim_sili(5) = sili_ec_sf_rmn
        dim_sili(6) = sili_ec_sf_rmx

        dim_sili(7) = sili_ec_sf_z(2)
        dim_sili(8) = sili_ec_sf_rmn
        dim_sili(9) = sili_ec_sf_rmx

        call gsvolu(v_i_name,'PCON',nmed,dim_sili,9,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gsatt(v_i_name,'COLO',4)
        call gspos(v_i_name,1,v_m_name,0.,0.,0.,irotnull,'ONLY')  ! SISF

c      HDPE
        nmed = sili_med_poly    ! HDPE
        v_m_name = siliEnvelope
        v_i_name = siliEcPoly    ! SIPL

        dim_sili(1) = 0.0
        dim_sili(2) = 360.0
        dim_sili(3) = 2

        dim_sili(4) = sili_ec_sf_z(1)
        dim_sili(5) = sili_ec_pl_rmn
        dim_sili(6) = sili_ec_pl_rmn + sili_ec_pl_thk

        dim_sili(7) = sili_ec_sf_z(2)
        dim_sili(8) = dim_sili(5)
        dim_sili(9) = dim_sili(6)

        call gsvolu(v_i_name,'PCON',nmed,dim_sili,9,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gsatt(v_i_name,'COLO',2)
        call gspos(v_i_name,1,v_m_name,0.,0.,0.,irotnull,'ONLY')  ! SIPL

c     Rohacell
        nmed = sili_med_cg
        v_m_name = siliEnvelope
        v_i_name = siliEcRoha    !   SIER

        dim_sili(1) = 0.0
        dim_sili(2) = 360.0
        dim_sili(3) = 2

        dim_sili(4) = sili_ec_sf_z(1)
        dim_sili(5) = dim_sili(6)
        dim_sili(6) = dim_sili(5) + sili_ec_rh_thk

        dim_sili(7) = sili_ec_sf_z(2)
        dim_sili(8) = dim_sili(5)
        dim_sili(9) = dim_sili(6)

        call gsvolu(v_i_name,'PCON',nmed,dim_sili,9,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gsatt(v_i_name,'COLO',5)
        call gspos(v_i_name,1,v_m_name,0.,0.,0.,irotnull,'ONLY')  ! SIER

c     Aluminum Mylar
        nmed = sili_med_passive
        v_m_name = siliEnvelope
        v_i_name = siliAlMylar    ! SIML 
        dim_sili(1) = 0.0
        dim_sili(2) = 360.0
        dim_sili(3) = 2

        dim_sili(4) = sili_ec_sf_z(1)
        dim_sili(5) = dim_sili(6)
        dim_sili(6) = dim_sili(5) + sili_ec_alm_thk

        dim_sili(7) = sili_ec_sf_z(2)
        dim_sili(8) = dim_sili(5)
        dim_sili(9) = dim_sili(6)

        call gsvolu(v_i_name,'PCON',nmed,dim_sili,9,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gsatt(v_i_name,'COLO',2)
        call gspos(v_i_name,1,v_m_name,0.,0.,0.,irotnull,'ONLY')  ! SIML


C...  Build barrel VTX

C (4b) cooling tube (copied from further down, since copies are used in the barrel)
        call gsvolu( 'SICT', 'TUBS', sili_med_coolant, par, 0, ivol1)

c       Create omega shape behind the passive material
        nmed = sili_med_m55j          ! M55J
        call gsvolu(siliBrOmega,'TUBS',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrOmega,'SEEN',1)
        call GSATT (siliBrOmega,'COLO',4)

c       Create omega shape behind the passive material
        nmed = sili_med_m55j          ! M55J
        call gsvolu(siliBrOmegap,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrOmegap,'SEEN',1)
        call GSATT (siliBrOmegap,'COLO',4)

c       Create a Bus material
        nmed = sili_med_kapton        ! Kapton   SIBS
        call gsvolu(siliBrBusp,'BOX ',nmed,dim_sili,0,ivolu)    
        call GSATT (siliBrBusp,'SEEN',1)
        call GSATT (siliBrBusp,'COLO',37)

c       Create SVX4 behind the passive material
        nmed = sili_med_passilicon    ! Passive Silicon
        call gsvolu(siliBrSVX4,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrSVX4,'SEEN',1)
        call GSATT (siliBrSVX4,'COLO',4)

c       Create a bias plane of ROC3 behind the sensor
        nmed = sili_med_copper        ! Copper
        call gsvolu(siliBrBPlane,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrBPlane,'SEEN',1)
        call GSATT (siliBrBPlane,'COLO',5)

c       Create a analog plane of ROC3 behind the sensor
        nmed = sili_med_copper        ! Copper
        call gsvolu(siliBrAPlane,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrAPlane,'SEEN',1)
        call GSATT (siliBrAPlane,'COLO',4)

c       Create a digital plane of ROC3 behind the sensor
        nmed = sili_med_copper        ! Copper
        call gsvolu(siliBrDPlane,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrDPlane,'SEEN',1)
        call GSATT (siliBrDPlane,'COLO',4)

c       Create a layer of passive material behind the sensor (strips)
        nmed = sili_med_gfrp          ! G10
        call gsvolu(siliBrPassive,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrPassive,'SEEN',1)
        call GSATT (siliBrPassive,'COLO',3)

c       Create a vdd and gnd plane material behind the sensor
        nmed = sili_med_passive     ! SIVG, Al
        call gsvolu(siliBrVGPlane,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrVGPlane,'SEEN',1)
        call GSATT (siliBrVGPlane,'COLO',4)

c       Create a vdd and gnd plane material behind the sensor
        nmed = sili_med_copper      ! SIGS, Coppper
        call gsvolu(siliBrSGPlane,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrSGPlane,'SEEN',1)
        call GSATT (siliBrSGPlane,'COLO',4)

c       Create barrel 3,4 stave core material
        nmed = sili_med_k3                        ! Allcomp K3   SIK3
        if (rhicrun.ge.14) nmed = sili_med_peek   ! PEEK afer 2013
        call gsvolu(siliBrK3,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrK3,'SEEN',1)
        call GSATT (siliBrK3,'COLO',7)

c       Create a Tube material for Stripixel Layer
        nmed = sili_med_passive                     ! Aluminum
        if (rhicrun.ge.14)  nmed = sili_med_peek    ! PEEK
        call gsvolu(siliBrTube,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrTube,'SEEN',1)
        call GSATT (siliBrTube,'COLO',1)

c       Create a Coolant material for Stripixel Layer   SICL
        nmed = sili_med_coolant     ! C5F12 liquid coolant
        call gsvolu(siliBrCool,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrCool,'SEEN',1)
        call GSATT (siliBrCool,'COLO',1)

c       Create a Bus material 1
        nmed = sili_med_kapton        ! Kapton
        call gsvolu(siliBrBus1,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrBus1,'SEEN',1)
        call GSATT (siliBrBus1,'COLO',37)

c       Create a Bus material 2
        nmed = sili_med_copper        ! Copper
        call gsvolu(siliBrBus2,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrBus2,'SEEN',1)
        call GSATT (siliBrBus2,'COLO',37)

c       Create a RCC material 1
        nmed = sili_med_gfrp          ! G10
        call gsvolu(siliBrRCC1,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrRCC1,'SEEN',1)
        call GSATT (siliBrRCC1,'COLO',3)

c       Create a RCC material 2
        nmed = sili_med_copper        ! Copper
        call gsvolu(siliBrRCC2,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrRCC2,'SEEN',1)
        call GSATT (siliBrRCC2,'COLO',3)

c       Create a RCC material 3
        nmed = sili_med_kapton        ! Kapton
        call gsvolu(siliBrRCC3,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrRCC3,'SEEN',1)
        call GSATT (siliBrRCC3,'COLO',3)

c       Create a RCC material 4
        nmed = sili_med_gfrp        ! G10
        call gsvolu(siliBrRCC4,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrRCC4,'SEEN',1)
        call GSATT (siliBrRCC4,'COLO',3)   ! 3-green

c       Create a RCC material 5
        nmed = sili_med_copper        ! Copper
        npar = 0
        call gsvolu(siliBrRCC5,'BOX ',nmed,dim_sili,npar,ivolu)
        call GSATT (siliBrRCC5,'SEEN',1)
        call GSATT (siliBrRCC5,'COLO',6)    ! 6=magenta

c       Create a stave material behind the sensor
        nmed = sili_med_cfiber      ! SIST, Carbon Fiber
        call gsvolu(siliBrStave,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrStave,'SEEN',1)
        call GSATT (siliBrStave,'COLO',4)

c       Create a resin material behind the sensor
        nmed = sili_med_resin       ! SIRS, CFC
        call gsvolu(siliBrResin,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrResin,'SEEN',1)
        call GSATT (siliBrResin,'COLO',5)

c       Create a resin material behind the sensor
        nmed = sili_med_resin       ! SIRR, CFC
        npar = 0
        call gsvolu(siliBrResinn,'BOX ',nmed,dim_sili,npar,ivolu)
        call GSATT (siliBrResinn,'SEEN',1)
        call GSATT (siliBrResinn,'COLO',5)

c       Create a sread out chip material behind the sensor
        nmed = sili_med_passilicon  ! Passive Silicon
        call gsvolu(siliBrRead,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrRead,'SEEN',1)
        call GSATT (siliBrRead,'COLO',2)

c       Create Si sensors SISN
        nmed = sili_med_silicon     ! Silicon
        call gsvolu(siliBrSensor,'BOX ',nmed,dim_sili,0,ivolu)
        call GSATT (siliBrSensor,'SEEN',1)
        call GSATT (siliBrSensor,'COLO',2)
        call GSATT (siliBrSensor,'WORK',1) ! Make volume sensitive

*-------  Make a tube for the run16 B2 place holder             ! This tube is used (twice)
        nmed = sili_med_cfiber                                  ! in run16, taking the place of 
        dim_sili(2)   = 0.476                ! 3/8" OD          ! the layer 3 West ladders, which
        dim_sili(1)   = dim_sili(2)-0.0425   ! .043" wall       ! were removed.
        ilayer = 3
        ladd_halfz    = sili_br_nsn(iLayer)*sili_br_snhalfz(iLayer)
     &   + 0.5*sili_br_snzgap(iLayer)*(sili_br_nsn(iLayer)) ! Length/2
        dim_sili(3)   = ladd_halfz
        call gsvolu('CTUB','TUBE', nmed, dim_sili, 3, ivolu)   ! SJ01-4
        call GSATT('CTUB','SEEN',0)
        call GSATT('CTUB','COLO',2)          ! 2=red


c---  Cycle over the layers    ( 4 barrel layers ) ---------------------------------------

        if(sili_br_r(5).gt.0.) then
           write(*,*) ' SVX BARREL uses Staggerd Geometry: ',
     *     sili_br_r(5)
        endif
        icopy = 0                                               ! for cooling tube SICT

        Do iLayer = 1, sili_br_nlayers                          !  loop over nlayers : 4 layers
c          write (6,*) 'XXX ilayer: ',ilayer
c         Define ladder volume, SInn nn=1-4
          v_m_name = siliNames(iLayer)
c         Define ladder dimensions
          ladd_halfx    = sili_br_snhalfx(iLayer)               ! Width/2
          if((iLayer.eq.1).or.(iLayer.eq.2)) then               ! Pixel Layer
             pasv_halfy = 0.5*0.025                             ! GFRP (BUS) Thick/2
             vg_halfy   = 0.5*0.01                              ! Al (VDD + GND) Thick/2
             sig_halfy  = 0.5*0.0006                            ! Copper Thick/2
             r_halfy    = 0.5*sili_br_rsn_thk                   ! CFC Thick/2
             b_halfy    = 0.5*0.015                             ! Silicon (ROC) Thick/2
             s_halfy(1) = 0.5*sili_br_stv_thk(1)                ! CF Thick/2
             ladd_halfx = sili_br_snhalfx(iLayer)               ! Width/2
             vg_halfx   = ladd_halfx
             sig_halfx  = ladd_halfx
             r_halfx    = ladd_halfx
             b_halfx    = ladd_halfx
             s_halfx    = ladd_halfx

             ladd_halfthck = pasv_halfy + sig_halfy + vg_halfy
     *            + 2*r_halfy + b_halfy
     *            + s_halfy(1) + sili_br_snhalfy(iLayer)        ! Thick/2

          else if((iLayer.eq.3).or.(iLayer.eq.4)) then          ! Strip Layer
             b_halfy    = 0.5*sili_br_bp_thk                    ! Cu Thick/2
c             b_halfy    = 0.5*sili_br_bp_thk/2                 ! Cu Thick/2
             agv_halfy  = 0.5*2*sili_br_bp_thk                  ! Cu Thick/2
             dg_halfy   = 0.5*sili_br_bp_thk                    ! Cu Thick/2
c             dg_halfy   = 0.5*sili_br_bp_thk/2
             pasv_halfy = 0.5*(sili_br_psv_thk - b_halfy        ! ROC Board G10 Thick/2
     *                       - agv_halfy - dg_halfy)
             k31_halfy  = 0.5*sili_br_k3_thk(1)                 ! Carbon Foam K3 Thick/2
             k32_halfy  = 0.5*sili_br_k3_thk(2)                 ! Carbon Foam K3 Thick/2
             k33_halfy  = 0.5*sili_br_k3_thk(2)                 ! Carbon Foam K3 Thick/2
             k34_halfy  = 0.5*sili_br_k3_thk(2)                 ! Carbon Foam K3 Thick/2
             tb1_halfy  = 0.5*sili_br_tb_thk(1)                 ! U-Tube Al Thick/2
             tb2_halfy  = 0.5*sili_br_tb_thk(2)                 ! U-Tube Al Thick/2
             tb3_halfy  = tb1_halfy                             ! U-Tube Al Thick/2
             tb4_halfy  = tb2_halfy                             ! U-Tube Al Thick/2
             s_halfy(2) = 0.5*sili_br_stv_thk(2)                ! Stave Face (TOP) C Thick/2
             s_halfy(3) = 0.5*sili_br_stv_thk(3)                ! Stave Face (BOTTOM) C Thick/2
             bus_halfy(1) = 0.5*sili_br_bus_thk(1)              ! Bus Kapton Thick/2
             bus_halfy(2) = 0.5*sili_br_bus_thk(2)              ! Bus Copper Thick/2
             rcc_halfy(1) = 0.5*sili_br_rcc_thk(1)              ! RCC Board Thick/2
             rcc_halfy(2) = 0.5*sili_br_rcc_thk(2)              ! RCC Board Thick/2
             tap_halfy(1) = 0.5*sili_br_tap_thk(1)              ! RCC Cable Kapton Thick/2
             tap_halfy(2) = 0.5*sili_br_tap_thk(2)              ! RCC Cable Copper Thick/2
             r_halfy    = 0.5*sili_br_rsn_thk                   ! Resin CFC Thick/2
             b_halfx    = ladd_halfx - 0.18
             agv_halfx  = ladd_halfx + 0.1 + 2*0.43625
             dg_halfx   = ladd_halfx + 2.255
             k31_halfx  = 0.5*3.61
             k32_halfx  = 0.5*1.714
             k33_halfx  = 0.5*0.288
             k34_halfx  = 0.5*3.034
             tb1_halfx  = 0.5*(0.635+0.025)
             tb2_halfx  = tb1_halfy
             tb3_halfx  = k32_halfx
             tb4_halfx  = k32_halfx
             s_halfx    = k31_halfx
             bus_halfx  = k31_halfx
             rcc_halfx  = 0.5*1.6742 !!! TO BE CHANGED
             tap_halfx  = 0.5*sqrt(2.)*(dg_halfx - 2*rcc_halfx
     *            - k31_halfx)
             ladd_halfx = dg_halfx
             r_halfx    = dg_halfx

             ladd_halfthck = sili_br_snhalfy(iLayer)
     *            + b_halfy + agv_halfy + dg_halfy + pasv_halfy
             supp_halfthck = s_halfy(2) + k31_halfy + k32_halfy
     *            + s_halfy(3) + bus_halfy(1) + bus_halfy(2)
     *            + rcc_halfy(1) + rcc_halfy(2)
          endif

          If(sili_br_snzgap(iLayer) .LT. 0.)          Then
            ladd_halfthck = 2.*ladd_halfthck
          Endif

c         Create a ladder holder volume SJnn made of cold air (SJ01, SJ02, SJ03, SJ04)
          v_j_name = siljNames(ilayer)
          nmed = sili_med_coldair
          dim_sili(1)   = ladd_halfx
          sj12_y = (sili_br_ct_radius + 0.05)/2.0           ! cooling fluid + wall thickness
          sj_halfy = sj12_y + ladd_halfthck                 ! sj01,2 half thickness
          if (ilayer.le.1.or.ilayer.eq.2) then
            dim_sili(2)   = ladd_halfthck + supp_halfthck + sj12_y   ! xxx
          else
            dim_sili(2)   = ladd_halfthck + supp_halfthck     ! xxx
          endif
          ladd_halfz    = sili_br_nsn(iLayer)*sili_br_snhalfz(iLayer)
     *     + 0.5*sili_br_snzgap(iLayer)*(sili_br_nsn(iLayer)) ! Length/2
          dim_sili(3)   = ladd_halfz
          call gsvolu(v_j_name,'BOX ', nmed, dim_sili, 3, ivolu)   ! SJ01-4
          call GSATT(v_j_name,'SEEN',0)
          call GSATT(v_j_name,'COLO',2)        ! 2=red

*-------  Create a ladder holder volume SInn made of cold air (SI01, SI02, SI03, SI04) ----------------------------------
          npar = 3
          nmed = sili_med_coldair
          dim_sili(1)   = ladd_halfx
          dim_sili(2)   = ladd_halfthck
          ladd_halfz    = sili_br_nsn(iLayer)*sili_br_snhalfz(iLayer)
     *     + 0.5 * sili_br_snzgap(iLayer)*(sili_br_nsn(iLayer)) ! Length/2
          dim_sili(3)   = ladd_halfz
          call gsvolu(v_m_name,'BOX ', nmed, dim_sili, npar, ivolu)
          call GSATT(v_m_name,'SEEN',0)
          call GSATT(v_m_name,'COLO',4)
          if (ilayer.eq.3 .or. ilayer.eq.4) then
            irot = irot+1                        ! flip it upside down
            call gsrotm(irot,90.,180.,90.,270.,0.,0.)
            call gspos(v_m_name,1,v_j_name,   ! place the strip supports SI03-SI04 into SJ03-04
     &                  0.,-supp_halfthck,0.,irot,'ONLY')
c             write(6,*)'placing sensor assembly ',v_m_name,
c     &       ' into ladder ', v_j_name,' (flipped)'
          else                                ! layers 1 an 2
            call gspos(v_m_name,1,v_j_name,   ! place the strip supports SI01-SI02 into SJ01,2
     &                  0.,sj12_y,0.,irotnull,'ONLY')         ! xxx test
c             write (6,*)'placing sensor assembly ',v_m_name,
c     &       ' into ladder ', v_j_name
          endif

*-------- Fill the ladded holder volumes ----------------------------------------------------------------------

          if((iLayer.eq.1).or.(iLayer.eq.2)) then       ! First the Pixel layers

*---------  Cooling tube (coolant) SICT into the holder volume SJ01, Sj02 -------------------------------------
            par(1) = 0.0                              
            par(2) = sili_br_ct_radius
            par(3) = ladd_halfz                         ! this depends on the layer
            par(4) = 0.0
            par(5) = 180.0
            ctdist = 1.0
            irot = irot+1                               ! place the coolant indo SJ01-2
            call gsrotm(irot,90.,180.,90.,270.,0.,0.)   ! flip around z
            call gsposp('SICT', 1, v_j_name,        
     &           0.,-ladd_halfthck-supp_halfthck+sj12_y,0.,
     &           irot, 'ONLY', par, 5)

*---------  Top of the Omega piece SIOM into holder volume SJ01, SJ02 -------------------------------------------          
            dim_sili(1) = sili_br_ct_radius          ! inner diameter
            dim_sili(2) = sili_br_ct_radius + 0.05   ! outer diameter (.5mm thick)
            dim_sili(3) = ladd_halfz
            dim_sili(4) = 20.
            dim_sili(5) = dim_sili(4) + 140.         ! end 
            call gsposp(siliBrOmega, 1, v_j_name, 
     &           0.,-ladd_halfthck-supp_halfthck+sj12_y,0.,
     &           irot, 'ONLY', dim_sili, 5)

*---------- Side omega shape 1  SIOM into holder volume SJ01, SJ02
            xomladd(1) = sili_br_ct_radius*cos(DEGRAD*20) +
     &                  cos(DEGRAD*20)*sili_br_ct_radius*sin(DEGRAD*20)/
     &                  (1. - sin(DEGRAD*20))

            dim_sili(1) = sili_br_ct_radius*sin(DEGRAD*20)/
     *           (1. - sin(DEGRAD*20)) - 0.05
            dim_sili(2) = sili_br_ct_radius*sin(DEGRAD*20)/
     *           (1. - sin(DEGRAD*20))
            dim_sili(3) = ladd_halfz
            dim_sili(4) = 270.
            dim_sili(5) = dim_sili(4) + 70.
            call gsposp(siliBrOmega, 2, v_j_name,
     &           xomladd(1),
     &           -ladd_halfthck-supp_halfthck+sj12_y-dim_sili(2),0.,
     &           irot, 'ONLY', dim_sili, 5)

*---------- Side omega shape 2  SIOM into holder volume SJ01, SJ02
            xomladd(1) = sili_br_ct_radius*cos(DEGRAD*20) +
     &                  cos(DEGRAD*20)*sili_br_ct_radius*sin(DEGRAD*20)/
     &                  (1. - sin(DEGRAD*20))

            dim_sili(1) = sili_br_ct_radius*sin(DEGRAD*20)/
     *           (1. - sin(DEGRAD*20)) - 0.05
            dim_sili(2) = sili_br_ct_radius*sin(DEGRAD*20)/
     *           (1. - sin(DEGRAD*20))
            dim_sili(3) = ladd_halfz
            dim_sili(4) = 90.
            dim_sili(5) = dim_sili(4) + 70.
            irot = irot+1                                  ! flip around z and x
            call gsrotm(irot,90.,180.,90.,90.,180.,0.)
            call gsposp(siliBrOmega, 3, v_j_name,
     &           -xomladd(1),
     &           -ladd_halfthck-supp_halfthck+sj12_y-dim_sili(2),0.,
     &           irot, 'ONLY', dim_sili, 5)

*---------- Flat omega shape SIOP, one on each side, into holder volume SJ01, SJ02
                    dim_sili(1) = (2*sili_br_snhalfx(iLayer)
     *                   - 2*(sili_br_ct_radius*cos(DEGRAD*20)/
     *                   (1. - sin(DEGRAD*20))))/4
                    dim_sili(2) = 0.025
                    dim_sili(3) = ladd_halfz
                    call gsposp(siliBrOmegap, 1, v_j_name,
     &                   xomladd(1)+dim_sili(1),
     &                   -ladd_halfthck-supp_halfthck+sj12_y
     &                   -dim_sili(2),
     &                   0.,
     &                   irotnull, 'ONLY' ,dim_sili, 3)
                    call gsposp(siliBrOmegap, 2, v_j_name,
     &                   -xomladd(1)-dim_sili(1), 
     &                   -ladd_halfthck-supp_halfthck+sj12_y
     &                   -dim_sili(2),
     &                    0.,
     &                   irotnull, 'ONLY' ,dim_sili, 3)

          else if((iLayer.eq.3).or.(iLayer.eq.4)) then ! Next, fill the Strip Layer holder volumes

*---------   Create a support volume SPnn made of cold air (SP03, SP04) -------------------------------------
             v_m_name = siliStSupp(iLayer)                ! (SP03,4 in turn hold lots of pieces)
             dim_sili(1)   = 3.1629-0.5*1.6742
             dim_sili(2)   = supp_halfthck
             ladd_halfz    = sili_br_nsn(iLayer)*sili_br_snhalfz(iLayer)
     *            + 0.5*sili_br_snzgap(iLayer)*(sili_br_nsn(iLayer)-1) ! Length/2
             dim_sili(3)   = ladd_halfz
             call gsvolu(v_m_name,'BOX ', nmed, dim_sili, npar, ivolu)
             call GSATT(v_m_name,'SEEN',0)
             call GSATT(v_m_name,'COLO',2)                 ! and place it in the holder volume:
             call gspos(siliStSupp(iLayer),1,v_j_name,     ! place the strip supports SP03, SP04
     &                  0.,ladd_halfthck,0.,irotnull,'ONLY') ! into SJ03, SJ04

*----------- Add rcc4 (SIR4) to the ladder, 5 or 6 on each side    -------------------------------------------

c            SIDE
c             dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
             dim_sili(1) = rcc_halfx
             dim_sili(2) = rcc_halfy(1)
             dim_sili(3) = 0.5*2.3

             sir4x = -3.1629
             sir4y = rcc_halfy(1) + ladd_halfthck - supp_halfthck
             sir4z =  sili_br_snhalfz(iLayer) - ladd_halfz + vtx_shiftz
c             sir4z = zrcc

             icopy = 0
             Do nrccr = 1, sili_br_nsn(iLayer) ! loop over 5 or 6 sensors/stave
               icopy = icopy+1
               call gsposp(siliBRrcc4,icopy,v_j_name,sir4x,sir4y,
     &              sir4z, irotnull,'ONLY',dim_sili,3)
               icopy = icopy+1
               call gsposp(siliBRrcc4,icopy,v_j_name,-sir4x,sir4y,
     &              sir4z, irotnull,'ONLY',dim_sili,3)
               sir4z = sir4z + 2.*sili_br_snhalfz(iLayer)
     &         + sili_br_snzgap(iLayer)
             enddo

c             zrcc = dim_sili(2) + vtx_shiftz
             zrcc =  sili_br_snhalfz(iLayer) - ladd_halfz + vtx_shiftz
             icopy = 0
             Do nrccr = 1, sili_br_nsn(iLayer) ! loop over 5 or 6 sensors/stave
              dim_sili(1) = rcc_halfx                      ! SIR5
              dim_sili(2) = rcc_halfy(2)
              dim_sili(3) = 0.5*2.3
              sir5x = -3.1629
              sir5y = -supp_halfthck + ladd_halfthck + 
     &              rcc_halfy(2) +2*rcc_halfy(1)
              sir5z = zrcc
              icopy = icopy+1
              call gsposp(siliBrRCC5,icopy,v_j_name, sir5x, sir5y,sir5z,
     &          irotnull, 'ONLY', dim_sili,3)
              icopy = icopy+1
              call gsposp(siliBrRCC5,icopy,v_j_name,-sir5x, sir5y,sir5z,
     &          irotnull, 'ONLY', dim_sili,3)

               zrcc = zrcc + 2.*sili_br_snhalfz(iLayer)
     &         + sili_br_snzgap(iLayer)
             Enddo     ! Do nrccr = 1, sili_br_nsn(iLayer)ladd    line 2383

*----------- Add rcc5 (SIR5) to the ladder, one on each side    -------------------------------------------
c             dim_sili(1) = rcc_halfx
c             dim_sili(2) = rcc_halfy(2)
c             dim_sili(3) = 0.5*2.3
c             sir5x = -3.1629
c             sir5y = -supp_halfthck + ladd_halfthck + 
c     &             rcc_halfy(2) +2*rcc_halfy(1)
c             sir5z = zrcc
c             call gsposp(siliBrRCC5,1,v_j_name, sir5x, sir5y, sir5z,
c     &                irotnull, 'ONLY', dim_sili,3)
c             call gsposp(siliBrRCC5,2,v_j_name,-sir5x, sir5y, sir5z,
c     &                irotnull, 'ONLY', dim_sili,3)

          endif       ! end if layer 3 or 4

c         Position sensors SISN and passive layers in the ladder SInn
          dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
cc-----   Special treatment to invert z-axis
          if((iLayer.eq.1).or.(iLayer.eq.2)) then            ! Pixel Layer
            dim_sili(2) = ladd_halfz 
     &      - sili_br_snhalfz(iLayer) - 0.5*sili_br_snzgap(ilayer)
          endif
cc-----
          npar = 3
          dim_sili(3) = sili_br_snhalfx(iLayer)
          dim_sili(5) = sili_br_snhalfz(iLayer)
          sen_y(1)   = sili_br_snhalfy(iLayer) - ladd_halfthck
          If(sili_br_snzgap(iLayer) .GE. 0.) Then
            sen_y(2) = sen_y(1)
          Else
            sen_y(2) = sen_y(1) +
     *            2.*(sili_br_snhalfy(iLayer) + pasv_halfy)
          Endif

c         Cooling tube parameters:
          par(1) = 0.0
          par(2) = sili_br_ct_radius
          par(3) = ladd_halfz                     ! this depends on the layer
          par(4) = 0.0
          par(5) = 180.0
          ctdist = 1.0

          Do nr = 1, sili_br_nsn(iLayer)          ! loop over sensors/stave
                                                  ! = 4, 4, 5, 6
C##############C
C     Pixel    C
C##############C
            if((iLayer.eq.1).or.(iLayer.eq.2)) then            ! Pixel Layer

C=====================================
C     Passive layer (Pixel)
C=====================================
               v_i_name = siliBrBusp                     ! SIBS                                   
c-TH20110221   dim_sili(1) =  pasv_halfy - ladd_halfthck !  SIBS BUS Kapton
               dim_sili(1) = -pasv_halfy + ladd_halfthck !  BUS Kapton
               dim_sili(4) = pasv_halfy
               dim_sili(5) = sili_br_snhalfz(iLayer)
     *              + 0.5*sili_br_snzgap(iLayer)
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)

C============================
C     signal plane (Pixel)
C============================
               v_i_name = siliBrSGPlane      ! SIGS
c-TH20110221   dim_sili(1) = dim_sili(1) + pasv_halfy + sig_halfy
               dim_sili(1) = dim_sili(1) - pasv_halfy - sig_halfy
               dim_sili(3) = sig_halfx
               dim_sili(4) = sig_halfy
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C==============================
C     VDD + GND plane (Pixel)
C==============================
               v_i_name = siliBrVGPlane          ! SIVG
c-TH20110221   dim_sili(1) = dim_sili(1) + sig_halfy + vg_halfy
               dim_sili(1) = dim_sili(1) - sig_halfy - vg_halfy
               dim_sili(3) = vg_halfx
               dim_sili(4) = vg_halfy
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C========================
C     CFC resin (Pixel)
C========================
               v_i_name = siliBrResin            ! SIRS
c-TH20110221   dim_sili(1) = dim_sili(1) + vg_halfy + r_halfy
               dim_sili(1) = dim_sili(1) - vg_halfy - r_halfy
               dim_sili(3) = r_halfx
               dim_sili(4) = r_halfy
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C=====================
C     Sensor (Pixel)
C=====================
               v_i_name = siliBrSensor
c-TH20110221   dim_sili(1) = dim_sili(1) + r_halfy
c-TH20110221        + sili_br_snhalfy(iLayer)
               dim_sili(1) = dim_sili(1) - r_halfy
     *              - sili_br_snhalfy(iLayer)
               dim_sili(3) = sili_br_snhalfx(iLayer)
               dim_sili(4) = sili_br_snhalfy(iLayer)
               dim_sili(5) = sili_br_snhalfz(iLayer)
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C===========================
C     Readout Chip (Pixel)
C===========================
               v_i_name = siliBrRead
c-TH20110221   dim_sili(1) = dim_sili(1) + sili_br_snhalfy(iLayer)
c-TH20110221        + b_halfy
               dim_sili(1) = dim_sili(1) - sili_br_snhalfy(iLayer)
     *              - b_halfy
               dim_sili(3) = b_halfx
               dim_sili(4) = b_halfy
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C========================
C     CFC resin (Pixel)
C========================
               v_i_name = siliBrResinn          ! SIRR
c-TH20110221   dim_sili(1) = dim_sili(1) + b_halfy + r_halfy
               dim_sili(1) = dim_sili(1) - b_halfy - r_halfy
               dim_sili(3) = r_halfx
               dim_sili(4) = r_halfy
               dim_sili(5) = sili_br_snhalfz(iLayer)
     *              + 0.5*sili_br_snzgap(iLayer)
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C====================
C     Stave (Pixel)
C====================
               v_i_name = siliBrStave      ! SIST
c-TH20110221   dim_sili(1) = dim_sili(1) + r_halfy + s_halfy(1)
               dim_sili(1) = dim_sili(1) - r_halfy - s_halfy(1)
               dim_sili(3) = s_halfx
               dim_sili(4) = s_halfy(1)
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)

C##############C
C     Strip    C
C##############C
            else if((iLayer.eq.3).or.(iLayer.eq.4)) then        ! Strip Layer
C=================
C     Sensor
C=================
               v_i_name = siliBrSensor
c              active area is 3.6 mm narrower than passive layer
               v_m_name = siliNames(iLayer)
c-TH20110222   dim_sili(1) = sen_y(mod(nr-1,2)+1)
               dim_sili(1) = -sen_y(mod(nr-1,2)+1)
               dim_sili(3) = sili_br_snhalfx(iLayer)-0.18
               dim_sili(4) = sili_br_snhalfy(iLayer)
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C=====================
c     Bias Plate
C=====================
               v_i_name = siliBrBPlane
c-TH20110222   dim_sili(1) = sen_y(mod(nr-1,2)+1)
c-TH20110222        + sili_br_snhalfy(iLayer) + b_halfy
               dim_sili(1) = dim_sili(1)
     *              - sili_br_snhalfy(iLayer) - b_halfy
               dim_sili(3) = b_halfx
               dim_sili(4) = b_halfy
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C==================================
C     Analog GND and VDD Plate
C==================================
               v_i_name = siliBrAPlane
c-TH20110222   dim_sili(1) = dim_sili(1) + b_halfy + agv_halfy
               dim_sili(1) = dim_sili(1) - b_halfy - agv_halfy
               dim_sili(3) = agv_halfx
               dim_sili(4) = agv_halfy
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C============================
C     Digital GND Plate
C============================
               v_i_name = siliBrDPlane
c-TH20110222   dim_sili(1) = dim_sili(1) + agv_halfy + dg_halfy
               dim_sili(1) = dim_sili(1) - agv_halfy - dg_halfy
               dim_sili(3) = dg_halfx
               dim_sili(4) = dg_halfy
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C=====================================
C     Passive layer (Insulator)
C=====================================
               v_i_name = siliBrPassive
c-TH20110222   dim_sili(1) = dim_sili(1) + dg_halfy + pasv_halfy
               dim_sili(1) = dim_sili(1) - dg_halfy - pasv_halfy
               dim_sili(3) = sili_br_snhalfx(iLayer) + 2.255
               dim_sili(4) = pasv_halfy
               call gsposp(v_i_name,nr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C==============
C     SVX4
C==============
               Do nsvx = 1, 12
                  nsvxr = (nr-1)*12 + nsvx
                  if(nsvx.lt.7) then
                     dim_sili(6) = sili_br_snhalfx(iLayer)
     *                    + 0.1 + 0.43625
                  else
                     dim_sili(6) = -(sili_br_snhalfx(iLayer)
     *                    + 0.1 + 0.43625)
                  endif
c-TH20110222                  dim_sili(1) = sen_y(mod(nr-1,2)+1)
c-TH20110222     *                 + sili_br_snhalfy(iLayer)
c-TH20110222     *                 + 2*b_halfy - 0.015
                  dim_sili(1) = -sen_y(mod(nr-1,2)+1)
     *                 - sili_br_snhalfy(iLayer)
     *                 - 2*b_halfy + 0.015
                  if((nsvx.eq.1).or.(nsvx.eq.7)) then
                     dim_sili(7) = dim_sili(2) - 2.6027
                  else if((nsvx.eq.2).or.(nsvx.eq.8)) then
                     dim_sili(7) = dim_sili(2) - 1.7527
                  else if((nsvx.eq.3).or.(nsvx.eq.9)) then
                     dim_sili(7) = dim_sili(2) - 0.9027
                  else if((nsvx.eq.4).or.(nsvx.eq.10)) then
                     dim_sili(7) = dim_sili(2) + 0.9027
                  else if((nsvx.eq.5).or.(nsvx.eq.11)) then
                     dim_sili(7) = dim_sili(2) + 1.7527
                  else if((nsvx.eq.6).or.(nsvx.eq.12)) then
                     dim_sili(7) = dim_sili(2) + 2.6027
                  endif
                  dim_sili(8)  = 0.43625
                  dim_sili(9)  = 0.015
                  dim_sili(10) = 0.335

                  v_i_name = siliBrSVX4
                  call gsposp(v_i_name,nsvxr,v_m_name
     *                 ,dim_sili(6),dim_sili(1),dim_sili(7),irotnull,
     *                 'ONLY',dim_sili(8),npar)
               Enddo
C========================
C     Top Stave Face
C========================
               v_m_name = siliStSupp(iLayer)
               v_i_name = siliBrStave        ! SIST
               dim_sili(1) = s_halfy(2) - supp_halfthck
               dim_sili(3) = s_halfx
               dim_sili(4) = s_halfy(2)
               nstvr = nstvr + 1
               call gsposp(v_i_name,nstvr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C====================================
C     Carbon Foam (Allcomp K3)
C====================================
               v_i_name = siliBrK3
               dim_sili(11) = dim_sili(1) + s_halfy(2)
               dim_sili(1) = dim_sili(11) + k32_halfy
               if(nr.eq.1) then
c     CORE
                  dim_sili(2) = dim_sili(2) + 0.5*(0.66 + 0.3)
                  dim_sili(3) = k32_halfx
                  dim_sili(4) = k32_halfy
                  dim_sili(5) = dim_sili(5) - 0.5*(0.66 + 0.3)
                  nk3r = nk3r + 1
                  call gsposp(v_i_name,nk3r,v_m_name
     *                 ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     SIDE
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
                  dim_sili(3) = k33_halfx
                  dim_sili(4) = k33_halfy
                  dim_sili(5) = sili_br_snhalfz(iLayer)
                  nk3r = nk3r + 1
                  call gsposp(v_i_name,nk3r,v_m_name
     *                 ,-1.661,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  nk3r = nk3r + 1
                  call gsposp(v_i_name,nk3r,v_m_name
     *                 ,1.661,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     EDGE
                  dim_sili(2) = 0.5*0.3 - ladd_halfz
                  dim_sili(3) = k34_halfx
                  dim_sili(4) = k34_halfy
                  dim_sili(5) = 0.5*0.3
                  nk3r = nk3r + 1
                  call gsposp(v_i_name,nk3r,v_m_name
     *                 ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
                  dim_sili(5) = sili_br_snhalfz(iLayer)
               else
c     CORE
                  dim_sili(3) = k32_halfx
                  dim_sili(4) = k32_halfy
                  nk3r = nk3r + 1
                  call gsposp(v_i_name,nk3r,v_m_name
     *                 ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     SIDE
                  dim_sili(3) = k33_halfx
                  dim_sili(4) = k33_halfy
                  nk3r = nk3r + 1
                  call gsposp(v_i_name,nk3r,v_m_name
     *                 ,-1.661,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  nk3r = nk3r + 1
                  call gsposp(v_i_name,nk3r,v_m_name
     *                 ,1.661,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
               endif
c     SURFACE
               dim_sili(1) = dim_sili(1) + k32_halfy + k31_halfy
               dim_sili(3) = k31_halfx
               dim_sili(4) = k31_halfy
               nk3r = nk3r + 1
               call gsposp(v_i_name,nk3r,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C================
C     U-TUBE
C================
               v_i_name = siliBrTube
               if(nr.eq.1) then
c     LONG-BOTTOM
                  dim_sili(1) = dim_sili(11) + tb1_halfy
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
     *                 + 0.5*0.3
                  dim_sili(3) = tb1_halfx
                  dim_sili(4) = tb1_halfy
                  dim_sili(5) = dim_sili(5) - 0.5*0.3
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,-1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     SHORT-BOTTOM
                  dim_sili(2) = 0.3 + 0.5*(0.635+0.025) - ladd_halfz
                  dim_sili(3) = tb3_halfx
                  dim_sili(4) = tb3_halfy
                  dim_sili(5) = 0.5*(0.635+0.025)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     OUTER-LONG-SIDE
                  dim_sili(1) = dim_sili(1) + tb1_halfy + tb2_halfy
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
     *                 + 0.5*0.3
                  dim_sili(3) = tb2_halfx
                  dim_sili(4) = tb2_halfy
                  dim_sili(5) = sili_br_snhalfz(iLayer) - 0.5*0.3
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,-1.49795,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,1.49795,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     OUTER-SHORT-SIDE
                  dim_sili(2) = 0.3 + tb1_halfy - ladd_halfz
                  dim_sili(3) = tb4_halfx + 0.635+0.025-2*tb1_halfy
                  dim_sili(4) = tb4_halfy
                  dim_sili(5) = tb1_halfy
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     INNER-LONG-SIDE
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
     *                 + 0.5*(0.3+0.635+0.025)-tb1_halfy
                  dim_sili(3) = tb2_halfx
                  dim_sili(4) = tb2_halfy
                  dim_sili(5) = sili_br_snhalfz(iLayer)
     *                 - 0.5*(0.3+0.635+0.025) + tb1_halfy
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,-0.87605,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,0.87605,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     INNER-SHORT-SIDE
                  dim_sili(2) = 0.3 + 0.635+0.025 - tb1_halfy
     *                 - ladd_halfz
                  dim_sili(3) = tb4_halfx
                  dim_sili(4) = tb4_halfy
                  dim_sili(5) = tb1_halfy
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     LONG-TOP
                  dim_sili(1) = dim_sili(1) + tb2_halfy + tb1_halfy
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
     *                 + 0.5*0.3
                  dim_sili(3) = tb1_halfx
                  dim_sili(4) = tb1_halfy
                  dim_sili(5) = sili_br_snhalfz(iLayer) - 0.5*0.3
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,-1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     SHORT-TOP
                  dim_sili(2) = 0.3 + 0.5*(0.635+0.025) - ladd_halfz
                  dim_sili(3) = tb3_halfx
                  dim_sili(4) = tb3_halfy
                  dim_sili(5) = 0.5*(0.635+0.025)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
                  dim_sili(5) = sili_br_snhalfz(iLayer)
               else
c     LONG-BOTTOM
                  dim_sili(1) = dim_sili(11) + tb1_halfy
                  dim_sili(3) = tb1_halfx
                  dim_sili(4) = tb1_halfy
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,-1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     OUTER-LONG-SIDE
                  dim_sili(1) = dim_sili(1) + tb1_halfy + tb2_halfy
                  dim_sili(3) = tb2_halfx
                  dim_sili(4) = tb2_halfy
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,-1.49795,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,1.49795,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     INNER-LONG-SIDE
                  dim_sili(3) = tb2_halfx
                  dim_sili(4) = tb2_halfy
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,-0.87605,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,0.87605,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     LONG-TOP
                  dim_sili(1) = dim_sili(1) + tb2_halfy + tb1_halfy
                  dim_sili(3) = tb1_halfx
                  dim_sili(4) = tb1_halfy
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,-1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  ntbr = ntbr + 1
                  call gsposp(v_i_name,ntbr,v_m_name
     *                 ,1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
               endif
C=================================
C     Coolant for Stripixel
C=================================
               v_i_name = siliBrCool
               dim_sili(1) = dim_sili(11) + 2*tb1_halfy
     *              + tb2_halfy
               if(nr.eq.1) then
c     LONG
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
     *                 + 0.5*0.3 + tb1_halfy
                  dim_sili(3) = tb1_halfx - 2*tb1_halfy
                  dim_sili(4) = tb2_halfy
                  dim_sili(5) = dim_sili(5) - 0.5*0.4 + 2*tb1_halfy
                  nclr = nclr + 1
                  call gsposp(v_i_name,nclr,v_m_name
     *                 ,-1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  nclr = nclr + 1
                  call gsposp(v_i_name,nclr,v_m_name
     *                 ,1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
c     SHORT
                  dim_sili(2) = 0.3 + 0.5*(0.635+0.025) - ladd_halfz
                  dim_sili(3) = tb3_halfx
                  dim_sili(4) = tb2_halfy
                  dim_sili(5) = 0.5*(0.635+0.025) - 2*tb1_halfy
                  nclr = nclr + 1
                  call gsposp(v_i_name,nclr,v_m_name
     *                 ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
                  dim_sili(5) = sili_br_snhalfz(iLayer)
               else
c     LONG
                  dim_sili(3) = tb1_halfx - 2*tb1_halfy
                  dim_sili(4) = tb2_halfy
                  nclr = nclr + 1
                  call gsposp(v_i_name,nclr,v_m_name
     *                 ,-1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
                  nclr = nclr + 1
                  call gsposp(v_i_name,nclr,v_m_name
     *                 ,1.187,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *                 ,dim_sili(3),npar)
               endif
C============================
C     Bottom Stave Face
C============================
               v_i_name = siliBrStave     ! SIST
c               dim_sili(1) = dim_sili(1) + r_halfy + s_halfy
               dim_sili(1) = dim_sili(1) + 2*k31_halfy
     *              + k32_halfy + s_halfy(3)
               dim_sili(3) = s_halfx
               dim_sili(4) = s_halfy(3)
               nstvr = nstvr + 1
               call gsposp(v_i_name,nstvr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C=============
C     BUS
C=============
               v_i_name = siliBrBus1
               dim_sili(1) = dim_sili(1) + s_halfy(3) + bus_halfy(1)
               dim_sili(3) = bus_halfx
               dim_sili(4) = bus_halfy(1)
               nbusr = nbusr + 1
               call gsposp(v_i_name,nbusr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
               v_i_name = siliBrBus2
               dim_sili(1) = dim_sili(1) + bus_halfy(1) + bus_halfy(2)
               dim_sili(3) = bus_halfx
               dim_sili(4) = bus_halfy(2)
               nbusr = nbusr + 1
               call gsposp(v_i_name,nbusr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
C==================
C     RCC Board
C==================
c     CORE
               v_i_name = siliBrRCC1
               dim_sili(1) = dim_sili(1) + bus_halfy(2) + rcc_halfy(1)
               dim_sili(3) = bus_halfx
               dim_sili(4) = rcc_halfy(1)
               nrccr = nrccr + 1
               call gsposp(v_i_name,nrccr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)
               v_i_name = siliBrRCC2
               dim_sili(1) = dim_sili(1) + rcc_halfy(1) + rcc_halfy(2)
               dim_sili(3) = bus_halfx
               dim_sili(4) = rcc_halfy(2)
               nrccr = nrccr + 1
               call gsposp(v_i_name,nrccr,v_m_name
     *              ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *              ,dim_sili(3),npar)

C===================
C     RCC Cable
C===================
               v_i_name = siliBrRCC3
               irot = irot + 1
               CALL GSROTM(irot,90.,45.,90.
     *              ,45.+90.,0.,0.)
               irotrcc = irot
               dim_sili(1) = dim_sili(11) - 2*s_halfy(2)
     *              + 2*rcc_halfy(1) + tap_halfx/sqrt(2.)
               dim_sili(3) = tap_halfx
               dim_sili(4) = tap_halfy(1)
               nrccr = nrccr + 1
               call gsposp(v_i_name,nrccr,v_m_name
     *              ,-2.0654,dim_sili(1),dim_sili(2),irotrcc,'ONLY'
     *              ,dim_sili(3),npar)
               v_i_name = siliBrRCC2
               dim_sili(1) = dim_sili(1)
     *              + (tap_halfy(1) + tap_halfy(2))/sqrt(2.)
               dim_sili(3) = tap_halfx
               dim_sili(4) = tap_halfy(2)
               nrccr = nrccr + 1
               call gsposp(v_i_name,nrccr,v_m_name
     *              ,-2.07084,dim_sili(1),dim_sili(2),irotrcc,'ONLY'
     *              ,dim_sili(3),npar)
               v_i_name = siliBrRCC3
               irot = irot + 1
               CALL GSROTM(irot,90.,-45.,90.
     *              ,-45.+90.,0.,0.)
               irotrcc = irot
               dim_sili(1) = dim_sili(11) - 2*s_halfy(2)
     *              + 2*rcc_halfy(1) + tap_halfx/sqrt(2.)
               dim_sili(3) = tap_halfx
               dim_sili(4) = tap_halfy(1)
               nrccr = nrccr + 1
               call gsposp(v_i_name,nrccr,v_m_name
     *              ,2.0654,dim_sili(1),dim_sili(2),irotrcc,'ONLY'
     *              ,dim_sili(3),npar)
               v_i_name = siliBrRCC2
               dim_sili(1) = dim_sili(1)
     *              + (tap_halfy(1) + tap_halfy(2))/sqrt(2.)
               dim_sili(3) = tap_halfx
               dim_sili(4) = tap_halfy(2)
               nrccr = nrccr + 1
               call gsposp(v_i_name,nrccr,v_m_name
     *              ,2.07084,dim_sili(1),dim_sili(2),irotrcc,'ONLY'
     *              ,dim_sili(3),npar)
               dim_sili(5) = sili_br_snhalfz(iLayer)
            endif      ! if layer 1,2 line ~1410
c---------
c-TH20110221            dim_sili(2) = dim_sili(2) + 2.*sili_br_snhalfz(iLayer)
c-TH20110221    *           + sili_br_snzgap(iLayer)
ccTH20110221----- Special treatment to invert z-axis
            if((iLayer.eq.1).or.(iLayer.eq.2)) then            ! Pixel Layer
              dim_sili(2) = dim_sili(2) - 2.*sili_br_snhalfz(iLayer)
     *           - sili_br_snzgap(iLayer)
            else
              dim_sili(2) = dim_sili(2) + 2.*sili_br_snhalfz(iLayer)
     *           + sili_br_snzgap(iLayer)
            endif
ccTH20110221-----
          Enddo ! Do nr = 1, sili_br_nsn(iLayer) ~line 1405

c---      Place ladders SI01 - SI04 in the cage SICG
          if((iLayer.eq.3).or.(iLayer.eq.4)) then            ! Strip Layers 3 and 4
             v_m_name = siliNames(iLayer)
             dphit = DEGRAD*sili_br_tilt(iLayer)
             rjsupp = sili_br_r(iLayer)
             xjsupp = ladd_halfthck + supp_halfthck          ! xxx new holder box
     *            - sili_br_snhalfy(iLayer) ! partial thickness
             yjsupp = rjsupp + xjsupp*COS(dphit)
             xjsupp =         xjsupp*SIN(dphit)
             rjsupp = sqrt(xjsupp*xjsupp + yjsupp*yjsupp)
          endif

          dphid = sili_br_dphi(iLayer)
          dphir = DEGRAD*dphid

          v_i_name = v_m_name
          v_m_name = siliCage                          ! Note: we are inside of a loop over 4 layers
          nr       = 1                                 ! nr is incremented at the bottom of the loop line 2697 
          Do isec = 1, sili_br_nsec(iLayer)            ! 2 sectors: East, West to line 2235
            nladd   = sili_br_nlad(isec,iLayer)        ! ladders per sector

*-----------See if alignment files are provided to produce an 'aligned' version of the geometry -----*
            inquire (file=vtx_alignment_file1, exist = ex1)
            inquire (file=vtx_alignment_file2, exist = ex2)

            if (ex1.and.ex2) then
              if (isec.eq.1.and.ilayer.eq.1)
     &        write (6,70) vtx_alignment_file1, vtx_alignment_file2
 70           format ('VTX alignment files: ',a40,/,21x,a40)
              open (unit=22,file=vtx_alignment_file1,err=99)
              open (unit=23,file=vtx_alignment_file2,err=99)
            elseif (isec.eq.1.and.ilayer.eq.1) then
              write (6,*) 'One or both VTX alignment input files not
     & found - running ideal geometry'
            endif

            iline = 0                                             ! ALIGNMENT:
            lfound = .false.                                      ! Find the x,y,z deltas
            do while (ex1.and.ex2)
              read(22,'(a80)',end=99,err=99) line1
              read(23,'(a80)',end=99,err=99) line2
              if (index(line1,'matrices').gt.0) lfound = .true.   ! Our information is past this line.
              if (lfound) then                                    ! info comes in blocks of 5 lines
                iline = iline+1
                if (mod(iline,5).eq.2) then                       ! pick up indices for this block
                  read (line1,*) kdum, klayer, kladder, ksensor
                  ksec = 1
                  if (kladder.gt.sili_br_nlad(isec,ilayer)) then
                    ksec = 2
                    kladder = kladder-sili_br_nlad(ksec,ilayer)
                  endif
                endif
                if (mod(iline,5).eq.3 .and.
     &            ilayer .eq.klayer    .and.
     &            isec   .eq.ksec      .and.
     &            ksensor.eq.1) then     !  pick up x,y,z  
                  read(line1,*) x1,y1,z1
                  read(line2,*) x2,y2,z2
                  xdel(kladder) = x2-x1
                  ydel(kladder) = y2-y1
                  zdel(kladder) = z2-z1
                endif                                              ! match layer, ladder
              endif                                                ! ladder info blocks
            enddo                                                  ! end read both files
 99         continue
            if  (ex1.and.ex2) then
              close (unit=22)
              close (unit=23)
            
              write (6,'(''xdel for ilayer isector'',2i2, 12f8.4)') 
     &        ilayer,isec,(xdel(i),i=1,sili_br_nlad(isec,ilayer))
              write (6,'(''ydel for ilayer isector'',2i2, 12f8.4)') 
     &        ilayer,isec,(ydel(i),i=1,sili_br_nlad(isec,ilayer))
            endif
*---------------------------- end reading svxPISAxxx.par's , if any. ------------------------------db-

c           Remove EAST tower in Run13
            sec_flag = skip_sector(rhicrun,isec)
            if(sec_flag.eq.1) then
               cycle                                           ! go to top of do loop, line 2492
            endif

            if((iLayer.eq.1).or.(iLayer.eq.2)) then            ! Pixel Layers 1 and 2
              if(isec.eq.2) then
                 sili_br_tilt(iLayer) = -sili_br_tilt(iLayer)
              endif
*             xladd = ladd_halfthck - sili_br_snhalfy(iLayer)  ! This is the offset from the center of the 
*     &             - 2*(b_halfy + r_halfy + s_halfy(1))       ! sensitive silicon to the mother volume SI01,2
*(1):
              shift = -sj_halfy + sili_br_snhalfy(iLayer)      ! This is the distance from the center of the 
     &              + 2*(b_halfy + r_halfy + s_halfy(1))       ! sensitive silicon to the mother volume SJ01,2
     &              + sili_br_ct_radius + 0.05                 ! SJ01,2 now also contains cooling tube fluid+wall
*(2):
c             shift = sj_halfy                                 ! (1) and (2) are equivalent. 
c     &             - 2*(pasv_halfy+sig_halfy+vg_halfy+r_halfy)
c     &             -sili_br_snhalfy(iLayer)

              rladd = sili_br_r(iLayer)                        ! Radius of the center of the sensitive silicon
              dphit = DEGRAD*sili_br_tilt(iLayer)              ! Tilt angle 13.00 for layer 1, 13.02 for layer 2
              xladd2 =         shift*SIN(dphit)                ! do the transformation SISN->SJ01,2
              yladd2 = rladd + shift*COS(dphit)                !
              rladd2 = sqrt(xladd2*xladd2 + yladd2*yladd2)     ! Radius of the center of SJ01,2
              delphi = atan2(xladd2,yladd2)                    ! The center of SJ01,2 is now also rotated slightly
                                                               ! from the original r-vector pointing to SISN

c             Cooling tube -----------------------------------------------------------------------------------
              xctladd(1) = 2*ladd_halfthck - sili_br_snhalfy(iLayer)
     *             - 2*(r_halfy+sig_halfy+vg_halfy+pasv_halfy)
              yctladd(1) = sili_br_r(iLayer) + xctladd(1)*COS(dphit)
              xctladd(1) =                     xctladd(1)*SIN(dphit)
              rctladd(1) = sqrt(xctladd(1)*xctladd(1)
     *             + yctladd(1)*yctladd(1))
 
cyyy              dphit = RADDEG*ASIN(xladd/rladd)
              dphict(1) = RADDEG*ASIN(xctladd(1)/rctladd(1))
              dphiom(1) = -RADDEG*ASIN(xomladd(1)/romladd(1))
              dphiom(2) = -RADDEG*ASIN(xomladd(2)/romladd(2))
              dphiomp(1) = -RADDEG*ASIN(xomegap(1)/romegap(1))
              dphiomp(2) = -RADDEG*ASIN(xomegap(2)/romegap(2))

c             Cooling tube
              phictladd(1) = sili_br_phic(isec,iLayer) + dphict(1)
     *             - (0.5*dphid)*(nladd-1)
              phictladd(1) = DEGRAD*phictladd(1)
c             Omega shape
              phiomladd(1) = sili_br_phic(isec,iLayer) + dphiom(1)
     *             - (0.5*dphid)*(nladd-1)
              phiomladd(2) = sili_br_phic(isec,iLayer) + dphiom(2)
     *             - (0.5*dphid)*(nladd-1)
              phiomladd(1) = DEGRAD*phiomladd(1)
              phiomladd(2) = DEGRAD*phiomladd(2)

c             Omega piece
              phiompladd(1) = sili_br_phic(isec,iLayer) + dphiomp(1)
     *             - (0.5*dphid)*(nladd-1)
              phiompladd(2) = sili_br_phic(isec,iLayer) + dphiomp(2)
     *             - (0.5*dphid)*(nladd-1)
              phiompladd(1) = DEGRAD*phiompladd(1)
              phiompladd(2) = DEGRAD*phiompladd(2)
c             End layer 1,2 cooling tube ----------------------------------------------------------------


c             Angle between sensor position and x axis
              phitmpladd = sili_br_phic(isec,iLayer)
     *            - (0.5*dphid)*(nladd-1)

c             Angle between ladder position and x axis
              philadd = sili_br_phic(isec,iLayer) 
     &                - (0.5*dphid)*(nladd-1)
              phirotladd = -90.+sili_br_tilt(iLayer) + philadd

            else if((iLayer.eq.3).or.(iLayer.eq.4)) then ! Strip Layer
c             Angle between ladder position and x axis
              if(isec.eq.1) then
                philadd = sili_br_phic(isec,iLayer) + dphit
     *               -90. + sili_br_stdphi(1,iLayer)
              else if(isec.eq.2) then
                philadd = sili_br_phic(isec,iLayer) + dphit 
     *               -90. + sili_br_stdphi(5,iLayer)
              endif
              phirotladd = -90.+sili_br_tilt(iLayer) + philadd
            endif                 ! 3 or 4
            philadd    = DEGRAD*philadd

            DO iladd = 1, nladd   ! loop over ladders iladd in this sector isec, to line 2662
C
C------------ staggered geometry -----------------------------------------
C
              shiftstag(1) = 0.                            ! This section takes care of the 123 123 123 staggering
              shiftstag(2) = 0.                            ! in radius for the strip ladders.
              if((iLayer.eq.3).or.(iLayer.eq.4)) then      ! Strip Layer
                if(iLayer.eq.3) then
                  if((iladd.eq.2).or.(iladd.eq.5).or.(iladd.eq.8)) then
                    if(isec.eq.1) then
                      sili_br_shiftstag = -(sili_br_r(5) + 0.237)  ! inner ones of layer 3 west
                    else if(isec.eq.2) then
                       sili_br_shiftstag = 0                       ! middle ones of layer 3 east
                    endif
                  else if((iladd.eq.1).or.(iladd.eq.4).or.
     *                   (iladd.eq.7)) then
                    if(isec.eq.1) then
                       sili_br_shiftstag = 0.                      ! middle ones layer 3 west
                    else if(isec.eq.2) then
                       sili_br_shiftstag = -(sili_br_r(5) + 0.237) ! inner ones layer 3 east
                    endif
                  else
                    sili_br_shiftstag = (sili_br_r(5) + 0.237)     ! outer ones layer 3 west and east
                  endif
                else     ! layer 4                 ----------------------------- 4 -------------------
                  if((iladd.eq.3).or.(iladd.eq.6).or.
     *                (iladd.eq.9).or.(iladd.eq.12)) then
                    if(isec.eq.1) then
                      sili_br_shiftstag = sili_br_r(5)
                    else if(isec.eq.2) then
                      sili_br_shiftstag = -sili_br_r(5)
                    endif
                  else if((iladd.eq.2).or.(iladd.eq.5).or.
     *                    (iladd.eq.8).or.(iladd.eq.11)) then
                    sili_br_shiftstag = 0.
                  else
                    if(isec.eq.1) then
                      sili_br_shiftstag = -sili_br_r(5)
                    else if(isec.eq.2) then
                      sili_br_shiftstag = sili_br_r(5)
                    endif
                  endif
                endif        ! layer 3 or layer 4
                shiftstag(1) = sili_br_shiftstag*cos(philadd)
                shiftstag(2) = sili_br_shiftstag*sin(philadd)
              endif           ! if layer 3 or 4

c             <- this indentation level: inside loops over layers(4), sectors(2) and ladders(5,10,8,12)
c------------------------------------------------------------------------------------------------------
c             add misalignment
              rmyres = sili_br_misalignment((isec-1)*4+iLayer)   ! resolution in cm
              if(rmyres.gt.0.) then
                rmydum = 10000.
                call gpoiss(rmydum,rmyrndmi,1)     ! gaussian with mean 10000 and sigma 100
                rmyrndm = rmyrndmi                 ! convert to real
                rmyrndm = (rmyrndm-10000.)/100.    ! gaussian with mean 0 and sigma 1
                rmyrndm = rmyrndm * rmyres         ! random shift in cm, assuming rmyres alignment accuracy
                rmyrndm = rmyrndm / sili_br_r(iLayer)           ! random shift in radians ...
                philadd = philadd + rmyrndm        ! shifted angle for each ladder
                write(*,*) ' SVX BARREL IS MISALLIGNED : ', 
     *            isec, iLayer, iladd, rmyrndm
                phictladd(1) = phictladd (1)+ rmyrndm   ! shifted angle for each ladder
                phictladd(2) = phictladd (2)+ rmyrndm   ! shifted angle for each ladder
                phiomladd(1) = phiomladd (1)+ rmyrndm   ! shifted angle for each ladder
                phiomladd(2) = phiomladd (2)+ rmyrndm   ! shifted angle for each ladder
                phiomladd(3) = phiomladd (3)+ rmyrndm   ! shifted angle for each ladder
                phiomladd(4) = phiomladd (4)+ rmyrndm   ! shifted angle for each ladder
              endif    ! misalignment
c
c-------------------------------------------------------------------------
c
c             delphi = 0
              xladd    = rladd2*cos(philadd+delphi) + shiftstag(1)      ! for placing SJ01,2
              yladd    = rladd2*sin(philadd+delphi) + shiftstag(2)
              xjsupp    = rjsupp*cos(philadd) + shiftstag(1)   ! ladder holder box xxx
              yjsupp    = rjsupp*sin(philadd) + shiftstag(2)
c    rotate sensitive volumes so that local axes point in the
c    same direction as channel counting 
c    for pixels it means turning east arm ladders "upside-down"
c    and changing direction of local z axis to opposite
c    for the first strip layer (layer 3 in Geant) east arm
c    ladders must be turned "upside-down", and ladders in
c    west arm must have direction of local z axis changed to opposite  
c    for the outermost strip layer (layer 4 in Geant) west arm ladders
c    must be turned upside-down, and ladders in east arm must have local z
c    axis direction changed to opposite

c              phirotladdsict = phirotladd ! this variable is never used in this code

c-- 2011.02.22 TH
c-- I found the problem on the ladder rotation and orientation.
c-- The ladder construction in local volume is not same with the reality in y-direction.
c-- I found this should be reversed. I modified the ladder constructure and changed the ladder rotation.

              phirotladdtmp = phirotladd - 180.
              zrotladd = 0.                                  ! barrels 1,2 West
              if((isec.eq.2).and.(iLayer.lt.3)) then         ! barrels 1,2 East
                 zrotladd = 180.
              endif
              irot = irot + 1
              irot1 = irot                                   ! irot1 for the barrel 1,2 ladder rotation
              CALL GSROTM(irot1,90.,phirotladdtmp+zrotladd,90.
     *             ,phirotladdtmp+90.,zrotladd,0.)

              irot = irot+1
              irot_3_4 = irot
              if (isec.eq.1.and.ilayer.eq.3) then            ! barrel 3 west
                CALL GSROTM(irot_3_4,90.,phirotladd+180.0,
     &                               90.,phirotladd+90.0 ,
     &                               180.0, 0.)
              elseif (isec.eq.2.and.ilayer.eq.4) then        ! barrel 4 east
                CALL GSROTM(irot_3_4,90.,phirotladd+180.0,
     &                               90.,phirotladd+90.0 ,
     &                               180.0, 0.)
              else
                CALL GSROTM(irot_3_4,90.,phirotladd,90.
     &               ,phirotladd+90.,0.,0.)
              endif

C=====================
C     Place Ladders
C=====================

              lad_flag = skip_ladder(rhicrun, isec, iLayer, iladd) ! Remove missing ladders in Run12
              if (lbarrel.and.lad_flag.lt.0) then
                if (ilayer.eq.1 .or. ilayer.eq.2) then                  ! Pixel layers 1,2
                  xladd = xladd             + vtx_shiftx + xdel(iladd)
                  yladd = yladd             + vtx_shifty + ydel(iladd)
                  zladd = sili_br_z(iLayer) + vtx_shiftz + zdel(iladd)
                  if (ilayer.eq.1.and.nr.le.5   .or.                    ! west halves of layers 1 and 2
     &                ilayer.eq.2.and.nr.le.10) then
                    call gspos(siljNames(ilayer),nr,'SVXW'              ! place the new holder volume SJ01, SJ02
     *                ,xladd,yladd,zladd,irot1,'MANY')
c                    write (6,'(''Placing '',a4,'' copy '',i2,
c     &                         '' into SVXW'')') siljNames(ilayer), nr
                  else                                                  ! east halves of layers 1 and 2
                    call gspos(siljNames(ilayer),nr,'SVXE'              ! place the new holder volume SJ01, SJ02
     *                ,xladd,yladd,zladd,irot1,'MANY')
c                    write (6,'(''Placing '',a4,'' copy '',i2,
c     &                         '' into SVXE'')') siljNames(ilayer), nr
                  endif
                endif
                if((iLayer.eq.3).or.(iLayer.eq.4)) then                 ! Strip Layers 3, 4
                  xjsupp = xjsupp + vtx_shiftx + xdel(iladd)
                  yjsupp = yjsupp + vtx_shifty + ydel(iladd)
                  if (ilayer.eq.3.and.nr.le.8  .or.
     &                ilayer.eq.4.and.nr.le.12) then
                    call gspos(siljNames(ilayer),nr,'SVXW'              ! place the new holder volumes SJ03, SJ04 xxx
     *                 ,xjsupp,yjsupp,zladd,irot_3_4,'MANY')
c                    write (6,'(''Placing '',a4,'' copy '',i2,
c     &                         '' into SVXW'')') siljNames(ilayer), nr
                  else
                    call gspos(siljNames(ilayer),nr,'SVXE'              ! place the new holder volumes SJ03, SJ04 xxx
     *                 ,xjsupp,yjsupp,zladd,irot_3_4,'MANY')
c                    write (6,'(''Placing '',a4,'' copy '',i2,
c     &                         '' into SVXE'')') siljNames(ilayer), nr
                  endif             ! east / west sectors
                endif               ! barrel 3, 4
              endif                 ! if not sector/barrel removed
              if (lad_flag.eq.2.and.(iladd.eq.1.or.iladd.eq.8)) then  ! in run16, layer 3 w was removed,
                write (6,*)'placing run-16 carbo fiber tube in B2W'
                call gspos('CTUB',mod(iladd,8)+1,'SVXW',              ! and 2 carbon tubes were places as
     &          xjsupp,yjsupp,zladd,irotnull,'ONLY')                  ! spacers.
              endif


              if((iLayer.eq.1).or.(iLayer.eq.2)) then
                phitmpladd = phitmpladd + dphir*RADDEG
                philadd    = philadd + dphir
                phirotladd = phirotladd + dphid
                phictladd(1)    = phictladd(1) + dphir
                phictladd(2)    = phictladd(2) + dphir
                phiomladd(1)    = phiomladd(1) + dphir
                phiomladd(2)    = phiomladd(2) + dphir
                phiomladd(3)    = phiomladd(3) + dphir
                phiomladd(4)    = phiomladd(4) + dphir
                phiompladd(1)   = phiompladd(1) + dphir
                phiompladd(2)   = phiompladd(2) + dphir
              else if((iLayer.eq.3).or.(iLayer.eq.4)) then
                if(isec.eq.1) then
                  philadd = philadd
     *                 + sili_br_stdphi(mod(iladd,3)+2,iLayer)*DEGRAD ! Radian
                  phirotladd = phirotladd
     *                 + sili_br_stdphi(mod(iladd,3)+2,iLayer)        ! Degree
                else if(isec.eq.2) then
                  philadd = philadd
     *            + sili_br_stdphi(mod((nladd-iladd),3)+2,iLayer)*DEGRAD ! Radian
                  phirotladd = phirotladd
     *            + sili_br_stdphi(mod((nladd-iladd),3)+2,iLayer)        ! Degree
                endif
              endif
              nr = nr + 1


c if there is misalignment, correct philadd back so that
c misalignments are not additive
              if(rmyres.gt.0.) then
                philadd = philadd - rmyrndm
                phictladd(1) = phictladd (1) - rmyrndm
                phictladd(2) = phictladd (2) - rmyrndm
                phiomladd(1) = phiomladd (1) - rmyrndm
                phiomladd(2) = phiomladd (2) - rmyrndm
                phiomladd(3) = phiomladd (3) - rmyrndm
                phiomladd(4) = phiomladd (4) - rmyrndm
              endif
            Enddo  ! iladd = 1, nladd                  from line 2315
          Enddo  ! Do isec = 1, sili_br_nsec(iLayer)   from line 2235

c         put SInn in set 'SVX'
c         Note for the barrel v_i_name = SI01,2,3,4
         
          namesv(5) = v_i_name
          namesv(5)(2:2) = 'J'     ! SJ01-SJ04
          namesv(6) = v_i_name     ! SI01-SI04

c          write (6,26) set_id, v_i_name, nv, namesv
c 26       format(2a6,i4,8a6)
          call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa
     &         ,nwsa,iset,idet)
          call gsdeth(set_id,v_i_name,nhh,inrNMSH,inrNBITSH
     &         ,inrORIG,inrFACT)

        Enddo                     ! loop over 4 barrel layers  from line 1296

c======================================================================c
c=========================   Readout wheels code    ===================c
c  See http://p25ext.lanl.gov/~hubert/phenix/silicon/simulations/jan07/wheel.html
c======================================================================c
c mother 
        if (sili_wheel_yesno.eq.1) then

          siwh_rmin = sili_cg_rmx(3)
          siwh_rmax = sili_cg_rmx(1)           ! note Helium bag HEB2 starts at 53.6 cm
          nmed = sili_med_coldair

          dim_sili( 1) = 0           ! Holder volume for the readout wheels.
          dim_sili( 2) = 360         ! this is a hollow cylinder fitting
          dim_sili( 3) = 6           ! just around the Silicon Enclosure SIEN
                                   ! It consists of 2 sections, with a 0-thickness 
          dim_sili( 4) = sili_cg_z(1)! central section (to clear the Helium bag). 
          dim_sili( 5) = siwh_rmin
          dim_sili( 6) = siwh_rmax

          dim_sili( 7) = sili_cg_z(2)       ! note in helium_bag.f, I moved HEB1 in to 26.0cm
          dim_sili( 8) = siwh_rmin
          dim_sili( 9) = siwh_rmax
          
          dim_sili(10) = sili_cg_z(3) 
          dim_sili(11) = siwh_rmin
          dim_sili(12) = siwh_rmin
           
          dim_sili(13) = sili_cg_z(4) 
          dim_sili(14) = siwh_rmin
          dim_sili(15) = siwh_rmin
          
          dim_sili(16) = sili_cg_z(5)
          dim_sili(17) = siwh_rmin
          dim_sili(18) = siwh_rmax
          
          dim_sili(19) = sili_cg_z(6) 
          dim_sili(20) = siwh_rmin
          dim_sili(21) = siwh_rmax

          call gsvolu('SIWH', 'PCON', nmed, dim_sili, 21, ivolu)
          call gsatt ('SIWH', 'SEEN', 1)
          call gspos('SIWH', 1, 'SIEN', 0., 0., 0. ,irotnull,'ONLY')

c     ROC boards for VTX
c     Pixel
c          nmed = sili_med_gfrp  ! G-10
c          v_m_name = 'SIWH'
c          v_i_name = 'SIPB'
c          dim_sili(1) = siwh_ppcb_half(1)
c          dim_sili(2) = siwh_ppcb_half(2)
c          dim_sili(3) = siwh_ppcb_half(3)
c          call gsvolu(v_m_name,'BOX ',nmed,dim_sili,npar,ivolu)
c          call gsatt('SIPB','SEEN',1)
c          call gspos('SIPB',1,'SIWH',0.,0.,0.,irotnull,'ONLY')

c     Strip
c          nmed = sili_med_gfrp
c          v_m_name = 'SIWH'
c          v_i_name = 'SISB'
c          dim_sili(1) = siwh_spcb_half(1)
c          dim_sili(2) = siwh_spcb_half(2)
c          dim_sili(3) = siwh_spcb_half(3)
c          call gsvolu(v_m_name,'BOX ',nmed,dim_sili,npar,ivolu)
c          call gsatt('SISB','SEEN',1)
c          call gspos('SISB',1,'SIWH',0.,0.,0.,irotnull,'ONLY')

c     ROC boards for FVTX
          dim_sili( 1) = 0           ! 'ROC' boards
          dim_sili( 2) = 360         ! 
          dim_sili( 3) = 2           ! 
                                   ! 
          dim_sili( 4) = -siwh_fpcb_thick      !  
          dim_sili( 5) =  siwh_rmin
          dim_sili( 6) =  siwh_pcb_rmax

          dim_sili( 7) = +siwh_fpcb_thick      !  
          dim_sili( 8) =  siwh_rmin
          dim_sili( 9) =  siwh_pcb_rmax

          nmed = sili_med_carbon 
          call gsvolu('SWRB', 'PCON', nmed, dim_sili, 9, ivolu)
          call gsatt ('SWRB', 'SEEN', 1)
          call gsatt ('SWRB', 'COLO', 2)
          do i=1,10
             call gspos('SWRB', i, 'SIWH', 0., 0., siwh_pcb_z(i),
     &                 irotnull,'ONLY')
          enddo
c          call gspos('SWRB', 10, 'SIWH', 0., 0., siwh_pcb_z(10),
c     &                irotnull,'ONLY')

*         connectors on the boards, modeled as solid plastic rings:
          dim_sili( 1) = 0           ! 
          dim_sili( 2) = 360         ! 
          dim_sili( 3) = 2           ! 
                                   ! 
          dim_sili( 4) = -siwh_pcb_connz    !  
          dim_sili( 5) =  siwh_pcb_rmax - 1.0
          dim_sili( 6) =  siwh_pcb_rmax
          
          dim_sili( 7) = +siwh_pcb_connz    !  
          dim_sili( 8) =  siwh_pcb_rmax - 1.0
          dim_sili( 9) =  siwh_pcb_rmax
          
          nmed = 802   ! = plastic
          call gsvolu('SWCN', 'PCON', nmed, dim_sili, 9, ivolu)
          call gsatt ('SWCN', 'SEEN', 1)
          call gsatt ('SWCN', 'COLO', 4)
          do i=1,10
            if (i.eq.1.or.(i.ge.6.and.i.le.9)) then
              call gspos('SWCN', i, 'SIWH', 0., 0., 
     &        siwh_pcb_z(i) + siwh_fpcb_thick + siwh_pcb_connz,
     &                  irotnull,'ONLY')
            else
              call gspos('SWCN', i, 'SIWH', 0., 0., 
     &        siwh_pcb_z(i) - siwh_fpcb_thick - siwh_pcb_connz,
     &                irotnull,'ONLY')
            endif
          enddo

*         support/cooling planes, one behind each pc board: 
          nmed = sili_med_passive    ! Aluminum
          call gsvolu('SWSP', 'PCON', nmed, dim_sili, 0, ivolu)
          call gsatt ('SWSP', 'SEEN', 1)
          call gsatt ('SWSP', 'COLO', 3)
          
          dim_sili( 1) = 0           ! 
          dim_sili( 2) = 360         ! 
          dim_sili( 3) = 2           ! 
                                   ! 
*         dim_sili( 4) = -siwh_pcb_suppz !  defined in each gposp()
          dim_sili( 5) =  siwh_rmin
          dim_sili( 6) =  siwh_pcb_rmax
          
*         dim_sili( 7) = +siwh_pcb_suppz    !  
          dim_sili( 8) =  siwh_rmin
          dim_sili( 9) =  siwh_pcb_rmax
          
          do i=1,10
            dim_sili( 4) = -siwh_pcb_suppz(i)    !  
            dim_sili( 7) = +siwh_pcb_suppz(i)    !  

            if (i.eq.1.or.(i.ge.6.and.1.le.9)) then
              call gsposp('SWSP', i, 'SIWH', 0., 0., 
     &        siwh_pcb_z(i) + siwh_fpcb_thick + siwh_pcb_suppz(i),
     &                  irotnull,'ONLY',dim_sili,9)
            else
              call gsposp('SWSP', i, 'SIWH', 0., 0., 
     &        siwh_pcb_z(i) - siwh_fpcb_thick - siwh_pcb_suppz(i),
     &                  irotnull,'ONLY',dim_sili,9)
            endif
          enddo

*         one rohacell thermal disk as an inner-z cover:
          nmed = sili_med_cg
          call gsvolu('SWRH', 'PCON', nmed, dim_sili, 0, ivolu)
          call gsatt ('SWRH', 'SEEN', 1)
          call gsatt ('SWRH', 'COLO', 7)

          dim_sili( 1) = 0           ! 
          dim_sili( 2) = 360         ! 
          dim_sili( 3) = 2           ! 
                                   ! 
          dim_sili( 4) = -siwh_roha1_thick !  defined in each gposp()
          dim_sili( 5) =  siwh_rmin
          dim_sili( 6) =  siwh_pcb_rmax
          
          dim_sili( 7) = +siwh_roha1_thick   !  
          dim_sili( 8) =  siwh_rmin
          dim_sili( 9) =  siwh_pcb_rmax
          
          call gsposp('SWRH', 1, 'SIWH', 0., 0., 
     &    siwh_pcb_z(5) + siwh_fpcb_thick + 2.*siwh_pcb_suppz(1) + 0.2 
     &         + siwh_roha1_thick, irotnull,'ONLY',dim_sili,9)
          
          call gsposp('SWRH', 2, 'SIWH', 0., 0., 
     &    siwh_pcb_z(6) - siwh_fpcb_thick - 2.*siwh_pcb_suppz(1) - 0.2 
     &         - siwh_roha1_thick, irotnull,'ONLY',dim_sili,9)

*         cables coming off the perimeter:
          dim_sili( 1) = 0           ! 
          dim_sili( 2) = 360         ! 
          dim_sili( 3) = 2           ! 
                                    ! 
          dim_sili( 4) =  -5.75
          dim_sili( 5) =  siwh_pcb_rmax
          dim_sili( 6) =  siwh_rmax
          
          dim_sili( 7) =  +5.75
          dim_sili( 8) =  siwh_pcb_rmax
          dim_sili( 9) =  siwh_pcb_rmax +0.5
          
          nmed = 802   ! = plastic
          call gsvolu('SWCB', 'PCON', nmed, dim_sili, 9, ivolu)
          call gsatt ('SWCB', 'SEEN', 1)
          call gsatt ('SWCB', 'COLO', 6)
          
          call gspos('SWCB', 1, 'SIWH', 0., 0., 
     &            -34.25, irotnull,'ONLY')
          irot = irot+1
          call gsrotm(irot,90.,180., 90.,90., 180.,0.)
          call gspos('SWCB', 2, 'SIWH', 0., 0., 
     &            +34.25, irot,'ONLY')

          nmed = sili_med_coldair      ! reset to be sure
        endif                          ! readout wheels modeled or not

      ENDIF             ! this was a check on volume character from line ~387 above CVOLU_OPT...

c---  Write parameters into the svxPISA.par file (temporary solution)
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      open(unit=15,file=svxpar_file,status='UNKNOWN',err=995)
c     SVX cage parameters
      write(15,*,err=994) 'SVX cage parameters:'
      write(15,*,err=994) sili_cg_rmn
      write(15,*,err=994) sili_cg_thck, sili_cg_inthck
      write(15,*,err=994) sili_cg_tempc
      write(15,*,err=994) sili_cg_npcon
      write(15,*,err=994) (sili_cg_z(iLayer),   iLayer=1,sili_cg_npcon)
      write(15,*,err=994) (sili_cg_rmx(iLayer), iLayer=1,sili_cg_npcon)
      write(15,*,err=994) sili_cg_xdisp, sili_cg_ydisp, sili_cg_zdisp
c     SVX barrel parameters
      write(15,*,err=994) 'SVX barrel parameters:'
      write(15,*,err=994) sili_br_nlayers
      write(15,*,err=994)
     *     (sili_br_r(iLayer),       iLayer=1,sili_br_nlayers+1)
      write(15,*,err=994)
     *     (sili_br_z(iLayer),       iLayer=1,sili_br_nlayers)
      write(15,*,err=994)
     *     (sili_br_nsn(iLayer),     iLayer=1,sili_br_nlayers)
      write(15,*,err=994)
     *     (sili_br_snhalfx(iLayer), iLayer=1,sili_br_nlayers)
      write(15,*,err=994)
     *     (sili_br_snhalfy(iLayer), iLayer=1,sili_br_nlayers)
      write(15,*,err=994)
     *     (sili_br_snhalfz(iLayer), iLayer=1,sili_br_nlayers)
      write(15,*,err=994)
     *     (sili_br_x0add(iLayer),   iLayer=1,sili_br_nlayers)
      write(15,*,err=994)
     *     (sili_br_snzgap(iLayer),  iLayer=1,sili_br_nlayers)
      write(15,*,err=994)
     *     (sili_br_dphi(iLayer),    iLayer=1,sili_br_nlayers)
      write(15,*,err=994)
     *     (sili_br_tilt(iLayer),    iLayer=1,sili_br_nlayers)
      do iLayer = 1, sili_br_nlayers
         write(15,*,err=994) sili_br_nsec(iLayer)
         write(15,*,err=994)
     *        (sili_br_nlad(isec,iLayer),isec=1,sili_br_nsec(iLayer))
         write(15,*,err=994)
     *        (sili_br_phic(isec,iLayer),isec=1,sili_br_nsec(iLayer))
      enddo

c     BARREL: Write sensor rotation matrices and translation vectors into file svxPISA.par (=unit 15)
      write(15,*,err=994)
     *   'SVX barrel sensor rotation matrices and translation vectors'
      call uctoh(namesv(1), LNAM(1), 4, 4)              ! level 1 is HALL
      LNUM(1) = 1                                       ! HALL is unique
      call uctoh(namesv(2), LNAM(2), 4, 4)              ! level 2 is SIEN
      LNUM(2) = 1                                       ! 
      call uctoh(namesv(3), LNAM(3), 4, 4)              ! level 3 is SICG
      LNUM(3) = 1                                       !
      
      do iLayer = 1, sili_br_nlayers                    ! loop over barrels 1-4
        write (namesv(5),'(''SJ0'',i1)') ilayer
        call uctoh(namesv(5), LNAM(5), 4, 4)            ! level 5 is SJ01-4
        write (namesv(6),'(''SI0'',i1)') ilayer
        call uctoh(namesv(6), LNAM(6), 4, 4)            ! level 6 is SI01-4

cxx: for run13, the East barrel is removed, and this is not reflected here. Look in the
cxx: output for error messages about SVXE being empty.

        nladd = 0
        do isec = 1, sili_br_nsec(iLayer)              ! loop over west / east
          sec_flag = skip_sector(rhicrun,isec)         ! remove east barrel in 2012
          if (sec_flag.eq.1) then
            if (iLayer.eq.1) write (6,*)'Removing East barrel in 2013'
          else
            namesv(4)                = 'SVXW'
            if (isec.eq.2) namesv(4) = 'SVXE'
            call uctoh(namesv(4), LNAM(4), 4, 4)        ! level 4 is SVXW / SVXE
            LNUM(4) = 1
          
            nladd = sili_br_nlad(isec,iLayer)
            
            do iladd = 1, nladd                           ! loop over ladders in this layer
              LNUM(5) = iladd + (isec-1)*nladd            ! level 5 is SJ01-4, ladder holder volumes
              LNUM(6) = 1                                 ! level 6 is SI01-4, one per holdercc
               
              do isen = 1, sili_br_nsn(iLayer)            ! 4,4,5,6 Si dets per ladder
                call uctoh(namesv(7), LNAM(7), 4, 4)      ! level 7 is SISN
                LNUM(7) = isen
                write(15,*,err=994) 0, iLayer, lnum(5), isen
                NLEVEL = 0                               ! nlevel is in /GCVOLU/
                
c              write (6,25) namesv(1), lnum(1), namesv(2), lnum(2), 
c     &                     namesv(3), lnum(3), namesv(4), lnum(4), 
c     &                     namesv(5), lnum(5), namesv(6), lnum(6), 
c     &                     namesv(7), lnum(7)
c 25           format(' namesv, lnum 1-7= ',7(a6,i2,'',''))
    
               lad_flag = skip_ladder(rhicrun, isec, iLayer, iladd) ! Remove missing ladders in Run12
               if (lad_flag.lt.0) then
                 call glvolu(7, LNAM, LNUM, ierr)  ! fills /GCVOLU/
               else
                 if (lnum(7).eq.1)
     &           write (6,*)' ladder removed', rhicrun,isec,iLayer,iladd
               endif
c            write (66,'(''namesv:'',7a5,''  lnum:'',7i3)') namesv(1),
c     &        namesv(2),namesv(3),namesv(4),namesv(5),namesv(6),
c     &        namesv(7),LNUM
                write(15,*,err=994) (gtran(i,NLEVEL), i=1,3)  ! in GCVOLU: xyz pos
                write(15,*,err=994) (grmat(i,NLEVEL), i=1,3)  ! in GCVOLU: rotation
                write(15,*,err=994) (grmat(i,NLEVEL), i=4,6)  !            matrix
                write(15,*,err=994) (grmat(i,NLEVEL), i=7,9)  !              ''
              enddo       ! end loop over sensors
            enddo         ! end loop over ladders
          endif           ! remove East half in 2013
        enddo             ! end loop over W/E sectors
      enddo               ! loop over iLayer 1-4
      NLEVEL = 0

c-------------------------! end glvolu calls ---------------

      close(unit=15)     ! file svxPISA.par
c---
c---  Fill 'PARA' zebra-bank
c---
      CHFORM = '5I -F'               ! 4 integer counts, then use all float
      call mzform('PARA',CHFORM,iod) ! book characteristic
c
c     write the parameters to a zebra bank. later they will go to output file
c---  Counting number of parameters

      npar = 1                       ! Number of hit components

c     Contribution from SIEN/SICG
      npar = npar + 8 + 2*sili_cg_npcon

c     Contribution from Barrel
      npar = npar + 2 + 11*sili_br_nlayers
      Do iLayer = 1, sili_br_nlayers
        npar = npar + 2*sili_br_nsec(iLayer)
      Enddo

c     Contribution from Endcap
      npar = npar + 2 + 11*8

      call mzbook(ixdiv_fr, lFD_PARA, lFD_PARA, 1,
     &     'PARA', 0, 0, npar, iod, 0)
c
C  fill the bank
c
c     Two first integers: numbers of layers in Barrel & Endcap
      iPoint = 1
      iqf(lfd_para + iPoint) = sili_br_nlayers
      iPoint = iPoint + 1
      iqf(lfd_para + iPoint) = 16   ! was 8
c     Number of hit components
      iPoint = iPoint + 1
      iqf(lfd_para + iPoint) = nhh
c     Number of barrel & endcap volume descriptors
      iPoint = iPoint + 1
      iqf(lfd_para + iPoint) = nbrv          ! = 5
      iPoint = iPoint + 1
      iqf(lfd_para + iPoint) = necv          ! = 6
c
c     Envelope/Cage parameters
c
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_rmn
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_thck
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_inthck
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_tempc
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = FLOAT(sili_cg_npcon)
      Do icnt = 1, sili_cg_npcon
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_cg_z(icnt)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_cg_rmx(icnt)
      Enddo
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_xdisp
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_ydisp
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_zdisp
C
C     Barrel parameters
C
      Do iLayer = 1, sili_br_nlayers
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_snhalfx(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_snhalfy(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_snhalfz(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_x0add(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_snzgap(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_tilt(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = FLOAT(sili_br_nsn(iLayer))
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_r(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_z(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_dphi(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = FLOAT(sili_br_nsec(iLayer))
        Do isec = 1, sili_br_nsec(iLayer)
          iPoint = iPoint + 1
          qf(lfd_para + iPoint) = sili_br_phic(isec,iLayer)
          iPoint = iPoint + 1
          qf(lfd_para + iPoint) = FLOAT(sili_br_nlad(isec,iLayer))
        Enddo
      Enddo

      write (6,'(''...VTX barrels finished'',/)')

        imass = 0 
        if (sili_endcap_config.ge.10) then
          fvtx_config = sili_endcap_config/10
          imass = mod(sili_endcap_config,10)
        else 
          fvtx_config = sili_endcap_config
        endif
        write (6,*)
     &  '(F)VTX: sili_endcap_config, fvtx_config, imass = ',
     &  sili_endcap_config, fvtx_config, imass

        if (imass.le.1) call svx_massnew
        if (imass.eq.2) call svx_massold

        if     (fvtx_config.eq.1) then  ! If 0, nothing installed in endcap region
          call svx_fvtx                            ! install FVTX 
        elseif (fvtx_config.eq.3) then  ! 
          call svx_fvtx_old                    ! install old fvtx disks (split in R) 
        endif                                  ! 

      return   ! from subroutine svx

 993  stop 'svx - material number already used.'
 994  stop 'svx - svxPISA.par write error.'
 995  stop 'svx - unable to open svxPISA.par.'
 997  stop 'svx - read error in sili_br_par segment.'
 998  stop 'svx - read error in sili_cg_par segment.'
 999  stop 'svx - read error in sili_ec_par.'

      end      ! end of subroutine svx

c=============================================================================c
      integer function skip_sector(rhicrun, silisec)

      implicit none
      integer rhicrun
      integer silisec

      if( rhicrun.eq.13.and.silisec.eq.2 ) then
         skip_sector = 1
      else
         skip_sector = -1
      endif

      return
      end ! end of function skip_sector

c=============================================================================c

      integer function skip_ladder(rhicrun, silisec, sililay, sililad)

      implicit none
      integer rhicrun
      integer silisec
      integer sililay
      integer sililad

      skip_ladder = -1
      if( rhicrun.eq.12.and.silisec.eq.2.and.sililay.eq.2) then
         if( sililad.eq.1.or.sililad.eq.9 ) then ! remove all material of ladder
            skip_ladder = 1
         elseif( sililad.eq.10 ) then ! remove all material of ladder except for stave
            skip_ladder = 0
         else
            skip_ladder = -1
         endif
      elseif (rhicrun.eq.16.and.silisec.eq.1.and.sililay.eq.3) then
         skip_ladder = 2
      endif

      return
      end ! end of function skip_ladder

c=============================================================================c
