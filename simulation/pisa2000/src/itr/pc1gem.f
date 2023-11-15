c $Id: pc1gem.f,v 1.4 2008/05/21 08:21:58 hpereira Exp $
*-- Author :    Charles F. Maguire   19/07/94
      SUBROUTINE PC1GEM ( NMPD1C, JFLBNK)
*   ============================================

C*******************************************************
C         DEFINE USER GEOMETRY SET UP                  *
C    FOR TRACK STATION N 1                             *
C       FIRST PAD. DETECTOR.                           *
C          CALLED FROM ITR                             *
C*******************************************************

c    New version for PISA July 19, 1994
 
C..  Revised by Kirill Filimonov (August 28, 1995) 

C.. "Realistic" Geometry is done for PC1 detector. New parameters are
C..  introduced in the namelist, detector is put in its own envelope
C..  with no dependence on the DCs. This subroutine now called from
C..  ITR.


c     Revision History
c    Name           Date            Comment
c    C.F. Maguire   Feb. 15, 1998   Add global coordinates and path length

c                                   Add date key to lfi_para bank

c    X. Wei         July 10, 1998   Add panel head, wire support, spacer,
c                                   end beam, terminal board, gap sealing,
c				    sealing strip

 
      IMPLICIT NONE
 
#include "gcflag.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"
  
**********************************************************************
*                                                                    *        
*                       P C 1    L A Y O U T                         *    
*                                                                    *         
**********************************************************************

*  THIS IS THE VIEW OF HALF PC1 SECTOR ALONG Z-DIRECTION:                     

*                                   radial distance                           

* |--------------------------------|  252.4466 cm
* |     ELECTRONICS (1.0 CM)       |
* |________________________________   251.4466 cm
* |--------------------------------:  251.4212 cm
*  \
*   \ CHEVRON/PIXEL CATHODE (1.0") |
*    \                             |
*     \____________________________   248.8812 cm
*      \---------------------------:  248.8558 cm
*       \ . . . GAS GAP (6 MM) . .    248.5558 cm  <- wire plane
*        \_________________________|  248.2558 cm
*         \------------------------|  248.2304 cm
*          \
*           \ GROUND CATHODE(0.75"):
*            \_____________________   246.3254 cm  
*             \--------------------|  246.3000 cm  
*              \                  .|
*               \             . `  
*                \        . `      :
*                 \     .`         
*                  \  .` Ang=5.625 |
*                   \.     degrees |
*                    \

**********************************************************************

C.. Description of introduced parameters (K. Filimonov)
*   Following the order of the namelist file, $itr_pc1_par part of phnx.par:

*.. - - - - - -   S t a r t   O f  D e s c r i p t i o n   - - - - - - ..*

* pc1nchmb = 16,      ! Total number of sectors of PC1 per two 90-degree arms
* pc1tetgt = 11.25,   ! Angle covered by each PC1 sector (180 degr/16 sectors)
* pc1dzext = 249.32   ! Maximum Z extent
* pc1rinst = 246.3,   ! Inner inscribe radius
* pc1routs = 252.5,   ! Outer outscribe radius 
* pc1grsht = 0.0254,  ! Thickness of PC1 ground cathode carbon-epoxy sheets 
* pc1grhex = 1.905,   ! Thickness of PC1 ground cathode HEXCELL panel (0.75")
* pc1gasgp = 0.6,     ! Gas gap width of PC1
* pc1ctsht = 0.0254,  ! Thickness of PC1 chevron/pixel cathode board (0.010") 
* pc1cthex = 2.540,   ! Thickness of PC1 chevron/pixel cathode HEXCELL panel
* pc1elect = 1.0,     ! Thickness of PC1 Electronics 
* pc1epoxy = 0.0025   ! Epoxy glue thickness
* pc1wires = 0.01,    ! Wire plane thickness (not in use)
* med_carbon_epoxy = 490,   ! Carbon-fiber/Epoxy
* med_s2_glass_epoxy = 491, ! S2-Glass/Epoxy 
* med_hexcell = 492,        ! HEXCELL 
* med_epoxy_glue = 493,     ! Epoxy glue 
* med_pc_gas = 494,         ! Gas 
* med_pc_board = 495,       ! Chevron/Pixel board (Carbon-fiber/Epoxy+Copper)
* med_ground_cathode = 496, ! Ground cathode (S2-Glass/Epoxy+Conductor?)
* med_mother_board = 497,   ! Electronics (Mother board+Chips+Connectors)
* med_wire_plane = 498,     ! Wire plane (not in use)  
* col_sector = 4,     ! Color of sector's shell
* col_hexcell = 0,    ! Color of hexcell
* col_fiber = 0,      ! Color of carbon-epoxy, S2 fiberglass and epoxy
* col_pd_gas = 0,     ! Color of PC's gas
* col_electr = 0,     ! Color of electronics
* delphi1 = -11.25,   ! Shift of the first arm
* delphi2 = +11.25,   ! Shift of the second arm  

*.. - - - - - - - -  E n d   O f   D e s c r i p t i o n   - - - - - - ..*

C.. Namelist parameters:

      INTEGER PC1NCHMB
      REAL PC1TETGT, PC1DZEXT, PC1RINST, PC1ROUTS
      REAL PC1DZGAS

      REAL PC1GRSHT, PC1GRHEX, PC1GASGP, PC1CTSHT, PC1CTHEX 
      REAL PC1EPOXY, PC1WIRES, PC1ELECT

      INTEGER MED_CARBON_EPOXY, MED_S2_GLASS_EPOXY, MED_HEXCELL
      INTEGER MED_EPOXY_GLUE, MED_PC_GAS,  MED_WIRE_PLANE
      INTEGER MED_PC_BOARD, MED_GROUND_CATHODE, MED_MOTHER_BOARD

      INTEGER COL_SECTOR, COL_HEXCELL, COL_FIBER, COL_PD_GAS, COL_ELECTR

C..  Local Variables:

      INTEGER*4 JFLBNK, NMPD1C

      REAL AGAS(3), ZGAS(3), WGAS(3)
      REAL ACATHODE(2), ZCATHODE(2), WCATHODE(2)
      REAL AGROUND(2), ZGROUND(2), WGROUND(2)
      REAL AWIRES(3), ZWIRES(3), WWIRES(3)
      REAL AELECTR(3), ZELECTR(3), WELECTR(3)
 
      REAL T1, T2, T3, F1, F2, F3
      INTEGER L, JRT, IST, J, JPT, LPT
      INTEGER IRTTR(16)
      integer Rotat180, RotatSealStrp(2),ispacer, no
      real tt1, tt2, tt3, ff1, ff2, ff3
      real SpacerIntvl

      REAL PAR(11)
      REAL DZ, THET, PHI, H1, BL1, TL1, ALP1, H2, BL2, TL2, ALP2
      REAL TANG, DHPC1, COSINE
      REAL DX, DY

      REAL A, Z, DENS, RADL, ABSL, UBUF
      INTEGER IMATE, NWBUF

      INTEGER IFIELD
      REAL FIELDM, TMAXFD, DMAXMS, DEEMAX, EPSIL, STMIN

      REAL TETGT(16)
      REAL FICNTR, SNFI, CSFI, XCNTP1, YCNTP1, ZCNTR
      real XPanelHead,YPanelHead,ZPanelHead
      real Pc1DzPanelHead, Pc1DzEndBeam, Pc1DxPanelHeadCavity, 
     * Pc1DyPanelHeadCavity, Pc1DzPanelHeadCavity

      CHARACTER*4 NMPAD1(16), NMGRSH1(16), NMGRSH2(16), NMGRHEX(16)
      CHARACTER*4 NMCTSH1(16), NMCTSH2(16), NMCTHEX(16), NMGAS(16)
      CHARACTER*4 NMGRGL1(16), NMGRGL2(16), NMCTGL1(16), NMCTGL2(16) 
      CHARACTER*4 NMWIRES(16), NMELEC(16)     
      CHARACTER*4 NmGrPnlHead(16),NmGrPnlHeadCavity(16),NmEndBeam(16), 
     *            NmEndBeamCavity(16),NmTermianlBrd(16),NmSpacer(16),
     *            NmSpacerCavity(16), NmWireSuprt(16), 
     *            NmGapSeal(16),NmCtPnlHead(16),NmCtPnlHeadCavity(16),
     *            NmSealStrp(16)         
      LOGICAL logdbg

      real delphi1 /0.0/   ! Shift of the first arm  (August, 1994 decision)
      real delphi2 /0.0/   ! Shift of the second arm (August, 1994 decision)

      INTEGER NCL1MX

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

C.. Read ITR_PC1 parameters through Namelist /ITR_PC1_PAR/

      NAMELIST /ITR_PC1_PAR/ PC1NCHMB, PC1TETGT, PC1DZEXT, PC1RINST, 
     + PC1ROUTS, PC1GRSHT, PC1GRHEX, PC1GASGP, PC1CTSHT, PC1CTHEX, 
     + PC1ELECT, PC1EPOXY, PC1WIRES, MED_CARBON_EPOXY, 
     + MED_S2_GLASS_EPOXY, MED_HEXCELL, MED_EPOXY_GLUE, MED_PC_GAS, 
     + MED_PC_BOARD, MED_GROUND_CATHODE, MED_MOTHER_BOARD, 
     + MED_WIRE_PLANE, COL_SECTOR, COL_HEXCELL, COL_FIBER, COL_PD_GAS, 
     + COL_ELECTR, delphi1, delphi2

C.. Local "mother (shell)" volumes of PC1 sectors (filled with air):
      DATA NMPAD1 /'P101', 'P102', 'P103', 'P104', 'P105', 'P106',
     +'P107', 'P108', 'P109', 'P110', 'P111', 'P112', 'P113', 'P114',
     +'P115', 'P116' /

C.. Ground cathode's carbon-epoxy sheet (0.010" thick) at inner radius
      DATA NMGRSH1 /'GI01', 'GI02', 'GI03', 'GI04', 'GI05', 'GI06',
     +'GI07', 'GI08', 'GI09', 'GI10', 'GI11', 'GI12', 'GI13', 'GI14',
     +'GI15', 'GI16' /
  
C.. Ground cathode's carbon-epoxy sheet (0.010" thick) at outer radius
      DATA NMGRSH2 /'GO01', 'GO02', 'GO03', 'GO04', 'GO05', 'GO06',
     +'GO07', 'GO08', 'GO09', 'GO10', 'GO11', 'GO12', 'GO13', 'GO14',
     +'GO15', 'GO16' /

C.. Ground cathode's HEXCELL (honeycomb core) sheet (0.75" thick)
      DATA NMGRHEX /'GH01', 'GH02', 'GH03', 'GH04', 'GH05', 'GH06',
     +'GH07', 'GH08', 'GH09', 'GH10', 'GH11', 'GH12', 'GH13', 'GH14',
     +'GH15', 'GH16' /

C.. Ground cathode's layer of epoxy glue at inner radius
      DATA NMGRGL1 /'XI01', 'XI02', 'XI03', 'XI04', 'XI05', 'XI06',
     +'XI07', 'XI08', 'XI09', 'XI10', 'XI11', 'XI12', 'XI13', 'XI14',
     +'XI15', 'XI16' /
  
C.. Ground cathode's layer of epoxy glue at outer radius
      DATA NMGRGL2 /'XO01', 'XO02', 'XO03', 'XO04', 'XO05', 'XO06',
     +'XO07', 'XO08', 'XO09', 'XO10', 'XO11', 'XO12', 'XO13', 'XO14',
     +'XO15', 'XO16' /
c???
c.. cathode panel head 
      data NmGrPnlHead /'CP01','CP02','CP03','CP04','CP05','CP06',
     * 'CP07','CP08','CP09','CP10','CP11','CP12','CP13','CP14',
     * 'CP15','CP16'/

c.. cathode panel head cavity
      data NmGrPnlHeadCavity /'PH01','PH02','PH03','PH04','PH05','PH06',
     * 'PH07','PH08','PH09','PH10','PH11','PH12','PH13','PH14',
     * 'PH15','PH16'/

c.. End Beam
      data NmEndBeam /'EB01','EB02','EB03','EB04','EB05','EB06',
     * 'EB07','EB08','EB09','EB10','EB11','EB12','EB13','EB14',
     * 'EB15','EB16'/

c.. End Beam cavity
      data NmEndBeamCavity /'BC01','BC02','BC03','BC04','BC05','BC06',
     * 'BC07','BC08','BC09','BC10','BC11','BC12','BC13','BC14',
     * 'BC15','BC16'/

c..  terminal board
      data NmTermianlBrd /'TB01','TB02','TB03','TB04','TB05','TB06',
     * 'TB07','TB08','TB09','TB10','TB11','TB12','TB13','TB14',
     * 'TB15','TB16'/

c..  spacer and spacer cavity
      data NmSpacer /'PR01','PR02','PR03','PR04','PR05','PR06',
     * 'PR07','PR08','PR09','PR10','PR11','PR12','PR13','PR14',
     * 'PR15','PR16'/

      data NmSpacerCavity /'NC01','NC02','NC03','NC04','NC05','NC06',
     * 'NC07','NC08','NC09','NC10','NC11','NC12','NC13','NC14',
     * 'NC15','NC16'/

c..  Gap Sealing
      data NmGapSeal /'GS01','GS02','GS03','GS04','GS05','GS06',
     * 'GS07','GS08','GS09','GS10','GS11','GS12','GS13','GS14',
     * 'GS15','GS16'/

c..  pix board panel head
      data NmCtPnlHead /'PP01','PP02','PP03','PP04','PP05','PP06',
     * 'PP07','PP08','PP09','PP10','PP11','PP12','PP13','PP14',
     * 'PP15','PP16'/

c..  pix board panel head cavity
      data NmCtPnlHeadCavity /'CC01','CC02','CC03','CC04','CC05','CC06',
     * 'CC07','CC08','CC09','CC10','CC11','CC12','CC13','CC14',
     * 'CC15','CC16'/

c..  Sealing Strip
      data NmSealStrp /'SS01','SS02','SS03','SS04','SS05','SS06',
     * 'SS07','SS08','SS09','SS10','SS11','SS12','SS13','SS14',
     * 'SS15','SS16'/

c..  wire support
      data NmWireSuprt /'WS01','WS02','WS03','WS04','WS05','WS06',
     * 'WS07','WS08','WS09','WS10','WS11','WS12','WS13','WS14',
     * 'WS15','WS16'/

c???
C.. Chevron/pixel cathode's S2-glass-epoxy sheet (0.010" thick) at in. rad.
      DATA NMCTSH1 /'CI01', 'CI02', 'CI03', 'CI04', 'CI05', 'CI06',
     +'CI07', 'CI08', 'CI09', 'CI10', 'CI11', 'CI12', 'CI13', 'CI14',
     +'CI15', 'CI16' /

C.. Chevron/pixel cathode's S2-glass-epoxy sheet (0.010" thick) at out. rad.  
      DATA NMCTSH2 /'CO01', 'CO02', 'CO03', 'CO04', 'CO05', 'CO06',
     +'CO07', 'CO08', 'CO09', 'CO10', 'CO11', 'CO12', 'CO13', 'CO14',
     +'CO15', 'CO16' /

C.. Chevron/pixel cathode's HEXCELL sheet (1.00" thick) 
      DATA NMCTHEX /'CH01', 'CH02', 'CH03', 'CH04', 'CH05', 'CH06',
     +'CH07', 'CH08', 'CH09', 'CH10', 'CH11', 'CH12', 'CH13', 'CH14',
     +'CH15', 'CH16' /

C.. Chevron/pixel cathode's layer of epoxy glue at inner radius
      DATA NMCTGL1 /'YI01', 'YI02', 'YI03', 'YI04', 'YI05', 'YI06',
     +'YI07', 'YI08', 'YI09', 'YI10', 'YI11', 'YI12', 'YI13', 'YI14',
     +'YI15', 'YI16' /
  
C.. Chevron/pixel cathode's layer of epoxy glue at outer radius
      DATA NMCTGL2 /'YO01', 'YO02', 'YO03', 'YO04', 'YO05', 'YO06',
     +'YO07', 'YO08', 'YO09', 'YO10', 'YO11', 'YO12', 'YO13', 'YO14',
     +'YO15', 'YO16' /

C.. Sensitive volume: Gas (argon-ethane 50-50%) gap 6.0 mm thick
      DATA NMGAS /'ZZ01', 'ZZ02', 'ZZ03', 'ZZ04', 'ZZ05', 'ZZ06',
     +'ZZ07', 'ZZ08', 'ZZ09', 'ZZ10', 'ZZ11', 'ZZ12', 'ZZ13', 'ZZ14',
     +'ZZ15', 'ZZ16' /

C.. Wire plane (60 anode wires: Gold plated tungsteen, 25 micrometers dia;
*               61 field wires: Copper-Berillium, 125 micrometers in diameter)
      DATA NMWIRES /'WW01', 'WW02', 'WW03', 'WW04', 'WW05', 'WW06',
     +'WW07', 'WW08', 'WW09', 'WW10', 'WW11', 'WW12', 'WW13', 'WW14',
     +'WW15', 'WW16' /

C.. Electronics (1.0 cm thick) - effective density to be defined later...
      DATA NMELEC /'EL01', 'EL02', 'EL03', 'EL04', 'EL05', 'EL06',
     +'EL07', 'EL08', 'EL09', 'EL10', 'EL11', 'EL12', 'EL13', 'EL14',
     +'EL15', 'EL16' /

c      DATA TETGT / 33.75, 45.00, 56.25, 67.50, 78.75, 90.00,
c     * 101.25, 112.50, 236.25, 247.50, 258.75, 270.00, 281.25,
c     * 292.50, 303.75, 315.00 /
c--> The angle+5.625 is the angle of the center of a sector
c      DATA TETGT / 39.375, 50.625, 61.875, 73.125, 84.375,
c     *95.625,106.875,118.125, 241.875,253.125,264.375,275.625,
c     * 286.875,298.125,309.375,320.625/

      DATA TETGT /28.125, 39.375, 50.625, 61.875, 73.125, 84.375,
     *95.625,106.875,253.125,264.375,275.625,
     * 286.875,298.125,309.375,320.625,331.875/


      DATA PC1DZGAS /180.83/

c     size for panel head 
      data Pc1DzPanelHead /7.86/   ! in cm
      data Pc1DzEndBeam /3.9/
      data Pc1DxPanelHeadCavity /15.0/
      data Pc1DyPanelHeadCavity /1.74/
      data Pc1DzPanelHeadCavity /2.54/
       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

C.. Read the geometry file segment
      write( *,* ) 'pc1gem - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = itr_pc1_par, err = 999 )
      
      if(pc1nchmb.gt.0)then
         logdbg = .false.
      else
         logdbg = .true.
         pc1nchmb = -pc1nchmb
      endif
      if( idebug.gt.0 ) logdbg = .true.

C.. Reset the TETGT array as needed

      if(delphi1.ne.0.0)then
         do l = 1,8
            tetgt(l) = tetgt(l) - delphi1
         enddo  ! first arm correction loop
      endif ! check on first arm correction
      if(delphi2.ne.0.0)then
         do l = 9,16
            tetgt(l) = tetgt(l) - delphi2
         enddo  ! second arm correction loop
      endif ! check on second arm correction

      NMPD1C = PC1NCHMB

      T1 = 90. ! polar angle for axis 1
      T2 = 90. ! polar angle for axis 2
      T3 = 0.  ! polar angle for axis 3
      F3 = 0.  ! azimuthal angle for axis 3

      DO 781 JRT = 1, PC1NCHMB
         F1 = TETGT ( JRT) ! azimuthal angle for axis 1
         F2 = F1 + 90.     ! azimuthal angle for axis 2

C.. "Flexible" ( floating) rotation index definition  (CMF)

         irot = irot + 1
         irttr( jrt) = irot
         CALL GSROTM ( IRTTR(JRT), T1, F1, T2, F2, T3, F3 )
  781 CONTINUE

c----------------------------------------
C. Define 180 degree rotation matrix, i.e. rotate 180 degree around Y axis
        irot = irot + 1
	Rotat180 = irot 
 	tt1 = 90.  ! polar angle for axis X
	tt2 = 90.  ! polar angle for axis Y
	tt3 = 180. ! polar angle for axis Z 
	ff1 = 180. ! azimuthal angle for axis X
	ff2 = 90.  ! azimuthal angle for axis Y
	ff3 = 0.   ! azimuthal angle for axis Z

	CALL GSROTM (Rotat180, tt1, ff1, tt2, ff2, tt3, ff3)

C.Define rotation matrix of sealing strip
        irot = irot + 1
	RotatSealStrp(1) = irot
        irot = irot + 1
	RotatSealStrp(2) = irot
 	tt1 = 90.  ! polar angle for axis X
	tt2 = 90.  ! polar angle for axis Y
	tt3 = 0. ! polar angle for axis Z 
	ff3 = 0. ! azimuthal angle for axis Z

	ff1 = 84.375  ! azimuthal angle for axis X
	ff2 = ff1+90.   ! azimuthal angle for axis Y
	CALL GSROTM (RotatSealStrp(1), tt1, ff1, tt2, ff2, tt3, ff3)
	ff1 =95.625   ! azimuthal angle for axis X
	ff2 = ff1+90.   ! azimuthal angle for axis Y
	CALL GSROTM (RotatSealStrp(2), tt1, ff1, tt2, ff2, tt3, ff3)
C.. ------------------- DEFINE USER MATERIALS ----------------------

C.. Carbon-fiber/Epoxy : 65% of carbon-fiber and 35% of epoxy
*   Take G10 PLATE for now 

      IMATE = 490
      A = 18.14
      Z = 9.065
      DENS = 1.7
      RADL = 19.4
      ABSL = 56.7
      NWBUF = 1
      CALL GSMATE(IMATE,'G10$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

C.. S2-glass/Epoxy - fiberglass material with properties close to G10
*   Take G10 PLATE for now
 
      IMATE = 491
      A = 18.14
      Z = 9.065
      DENS = 1.7
      RADL = 19.4
      ABSL = 56.7
      NWBUF = 1
      CALL GSMATE(IMATE,'G10$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

C.. HEXCELL - the core material for both the chevron/pixel and ground
*             cathode panels. Its density = 0.024 g/cm*3 and radiation
*             length = 81.7 m are deduced from the data used by GEM with the 
*             assumption that the radiation is emitted from a point source 
*             into a cone of 6 degrees with respect to its central axis, 
*             incident upon the hexcell (facing the holes) of infinite extent.

      IMATE = 492
      A = 12.01 
      Z = 6.
      DENS = 0.024
      RADL = 8170.
      ABSL = 99999.
      NWBUF = 1
      CALL GSMATE(IMATE,'HEXCELL$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

C.. Epoxy glue - basically, one can neglect the glue by adding a bit
*   of thickness to the fiberglass face sheets. Take G10 PLATE with 
*   one tau thickness (25 microns) for now. One has to estimate 
*   whether it's worth it to put the glue in at all, that directly
*   depends on the amount of glue to be used. Keep it for now, until
*   tests with real materials are done ...

      IMATE = 493
      A = 18.14
      Z = 9.065
      DENS = 1.7
      RADL = 19.4
      ABSL = 56.7
      NWBUF = 1
      CALL GSMATE(IMATE,'EPOXY GLUE$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

C.. Chamber gas (Argon-Ethane 50%-50%)

*   Atomic weights of Ar, C, and H:
      AGAS(1) = 39.95           
      AGAS(2) = 12.01           
      AGAS(3) =  1.01           

*   Mass numbers of Ar, C, and H:
      ZGAS(1) = 18.            
      ZGAS(2) = 6.             
      ZGAS(3) = 1.             

*   Proportions: 0.5 Ar + 0.5 C2H6
      WGAS(1) = 0.5            
      WGAS(2) = 0.125
      WGAS(3) = 0.375

      IMATE = 494
      DENS = 0.0010394
      CALL GSMIXT(IMATE,'PAD CHAMBER GAS$',AGAS,ZGAS,DENS,3,WGAS)

C.. Pixel/Chevron cathode board - Carbon-fiber/Epoxy + 1/8 oz/ft*2 of copper.
*   The density of copper is 8.96 g/cm*3, then the thickness of the 
*   copper layer is 4.2 microns. G10 density is 1.7 g/cm*3 and its 
*   thickness is 0.0254 cm (0.010"). Thus the density of composite material 
*   is 1.82 g/cm*3

   
*   Atomic weights of G10 and copper:
      ACATHODE(1) = 18.14
      ACATHODE(2) = 63.54

*   Mass numbers of G10 and copper:
      ZCATHODE(1) = 9.065
      ZCATHODE(2) = 29.

*   Proportions: 0.984 G10 + 0.016 Cu
      WCATHODE(1)=0.984
      WCATHODE(2)=0.016

      IMATE = 495
      DENS = 1.82
      CALL GSMIXT(IMATE,'CATHODE BOARD$',
     +            ACATHODE,ZCATHODE,DENS,2,WCATHODE)

C.. Ground cathode board - S2-glass/Epoxy + 1/8 oz/ft*2 of copper.
*   Take same compound as for cathode board for now.   

*   Atomic weights of G10 and copper:
      AGROUND(1) = 18.14
      AGROUND(2) = 63.54

*   Mass numbers of G10 and copper:
      ZGROUND(1) = 9.065
      ZGROUND(2) = 29.

*   Proportions: 0.984 G10 + 0.016 Cu
      WGROUND(1)=0.984
      WGROUND(2)=0.016

      IMATE = 496
      DENS = 1.82
      CALL GSMIXT(IMATE,'GROUND CATHODE BOARD$',
     +            AGROUND,ZGROUND,DENS,2,WGROUND)

C.. Electronics - mother board, chips & connectors + air? maybe cool nitrogen? 
*   Take 0.05 cm G10 plate + 1/4 oz/ft*2 of copper (0.000836 cm) + air for the
*   rest (~0.95 cm with density of 0.001 g/cm*3). The resultant density is 
*   0.093 g/cm*3

*   Atomic weights of G10, copper, and air:
      AELECTR(1) = 18.14
      AELECTR(2) = 63.54
      AELECTR(3) = 14.61

*   Mass numbers of G10, copper, and air:
      ZELECTR(1) = 9.065
      ZELECTR(2) = 29.
      ZELECTR(3) = 7.3

*   Proportions: 0.05 G10 + 0.000836 Cu + 0.95 Air
      WELECTR(1)=0.05
      WELECTR(2)=0.000836
      WELECTR(3)=0.949164

      IMATE = 497
      DENS = 0.093
      CALL GSMIXT(IMATE,'MOTHER BOARD$',AELECTR,ZELECTR,DENS,3,WELECTR)

C.. Wire plane - chamber gas, gold-tungsteen: 60 anode wires (25 microns 
*   in diameter) per sector, copper-berillium: 60 field wires (125 microns 
*   in diameter) per sector. The sector dimensions are 180 x 50 cm, the
*   thickness of wire plane is taken to be 0.01 cm, which gives the volume 
*   of 90 cm*3. The volume of anode wires is 0.054 cm*3 per sector, of field 
*   wires 25 times bigger - 1.35 cm*3. Thus, the proportions for density
*   are (0.0006 : 0.015 : 0.9844) for tungsteen (19.3 g/cm*3), copper
*   (8.96 g/cm*3) and gas (0.0010394 g/cm*3). The density of composite
*   material is 0.147 g/cm*3 for 100 microns thickness of the layer.     

      
*   Atomic weights of tungsteen, copper, and gas:
      AWIRES(1) = 183.85
      AWIRES(2) = 63.54
      AWIRES(3) = 21.855 

*   Mass numbers of tungsteen, copper, and gas:
      ZWIRES(1) = 74.
      ZWIRES(2) = 29.
      ZWIRES(3) = 10.125

*   Proportions: 0.0006 W + 0.015 Cu + 0.9844 Ar+C2H6
      WWIRES(1) = 0.0006
      WWIRES(2) = 0.015
      WWIRES(3) = 0.9844
    
      IMATE = 498
      DENS = 0.147
      CALL GSMIXT(IMATE,'WIRE PLANE$',AWIRES,ZWIRES,DENS,3,WWIRES)

C.. ----------------- DEFINE USER TRACKING MEDIA -------------------

      IFIELD = 1     ! magnetic field; tracking performed with GRKUTA;
      FIELDM = 20.0  ! max field value (in Kilogauss);
      TMAXFD = 1.0   ! maximum angle due to field in one step (in degrees);
      DMAXMS = 0.5   ! max disp. due to mult. scatt. in one step (in cm);
      DEEMAX = 0.2   ! max fractional energy loss in one step;
      EPSIL = 0.01   ! tracking precision (in cm);
      STMIN = 0.01   ! min step due to energy loss or mult. scatt. (in cm);
 
      CALL GSTMED(490,'CARBON-FIBER/EPOXY   $', 490,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(491,'S2-GLASS/EPOXY       $', 491,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(492,'HEXCELL CORE         $', 492,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(493,'EPOXY GLUE           $', 493,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(494,'PAD CHAMBER GAS      $', 494,  1,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(495,'PAD CATHODE BOARD    $', 495,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(496,'GROUND CATHODE BOARD $', 496,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(497,'MOTHER BOARD         $', 497,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(498,'WIRE PLANE           $', 498,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)

C.. -------------------- DEFINE USER VOLUMES -----------------------

C.. Introducing some local variables:

      TANG  = TAN( PC1TETGT*DEGRAD/2. ) ! tangent of 5.625 degrees
      COSINE = COS( PC1TETGT*DEGRAD/2. ) ! cosine of 5.625 degrees
      DHPC1 = 2.*PC1GRSHT+PC1GRHEX+PC1GASGP+2.*PC1CTSHT+
     +        PC1CTHEX+PC1ELECT  ! Sandwich thickness
    

*.. - - - -  P C 1   M O T H E R    V O L U M E  ( S H E L L )  - - - - ..*  

C.. Create the local "shell" volume for each sector:  all other volumes are 
*   contained within their local mother shell P1(01-16).

      DZ = PC1DZEXT/2. 
      THET = 0.
      PHI = 0.
      H1 = DHPC1/2.
      BL1 = PC1RINST*TANG
      TL1  = (PC1RINST+2*H1)*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = ALP1

      Par( 1) = DZ   ! half length along the Z axis
      Par( 2) = THET ! polar angle
      Par( 3) = PHI  ! azimuthal angle
      Par( 4) = H1   ! half length along Y of the face at -DZ
      Par( 5) = BL1  ! half length along X of the side at -H1
      Par( 6) = TL1  ! half length along X of the side at +H1
      Par( 7) = ALP1 ! angle 1 
      Par( 8) = H2   ! half length along Y of the face at +DZ
      Par( 9) = BL2  ! half length along X of the side at -H2
      Par(10) = TL2  ! half length along X of the side at +H2
      Par(11) = ALP2 ! angle 2

      DO L = 1, PC1NCHMB
* Medium for "shell" volumes is air...
         CALL GSVOLU (NMPAD1(L),'TRAP',16, Par, 11, IST)
         CALL GSATT (NMPAD1(L),'SEEN',1 )
         CALL GSATT (NMPAD1(L),'COLO',COL_SECTOR )
      END DO


*.. - - - - - - - - -  P C 1   S A N D W I C H  - - - - - - - - - ..* 

C.. Create the sandwich of the detector, layer by layer:
*   Starting from the inside (inscribe inner radius of PC's envelope)
*   to outside :

C.. 1) Ground cathode panel: Carbon-Epoxy sheet 0.01" thick at 246.3 cm

      DX = PC1RINST*TANG
      DY = PC1GRSHT/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMGRSH1(L),'BOX ',MED_CARBON_EPOXY, Par, 3, IST)
         CALL GSATT (NMGRSH1(L),'SEEN',1 )
         CALL GSATT (NMGRSH1(L),'COLO',COL_FIBER )
      END DO

C.. 1a) Ground cathode panel: Layer of epoxy glue

      DX = PC1RINST*TANG
      DY = PC1EPOXY/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMGRGL1(L),'BOX ',MED_EPOXY_GLUE, Par, 3, IST)
         CALL GSATT (NMGRGL1(L),'SEEN',1 )
         CALL GSATT (NMGRGL1(L),'COLO',COL_FIBER )
      END DO

C.. 2) Ground cathode panel: HEXCELL sheet 0.75" thick at 247.2779 cm

c???      DZ = PC1DZEXT/2. 
      dz = (PC1DZEXT-2*Pc1DzPanelHead)/2

      THET = 0.
      PHI = 0.
      H1 = PC1GRHEX/2.
      BL1 = (PC1RINST+PC1GRSHT)*TANG
      TL1  = BL1+PC1GRHEX*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = ALP1

      Par( 1) = DZ   ! half length along the Z axis
      Par( 2) = THET ! polar angle
      Par( 3) = PHI  ! azimuthal angle
      Par( 4) = H1   ! half length along Y of the face at -DZ
      Par( 5) = BL1  ! half length along X of the side at -H1
      Par( 6) = TL1  ! half length along X of the side at +H1
      Par( 7) = ALP1 ! angle 1 
      Par( 8) = H2   ! half length along Y of the face at +DZ
      Par( 9) = BL2  ! half length along X of the side at -H2
      Par(10) = TL2  ! half length along X of the side at +H2
      Par(11) = ALP2 ! angle 2

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMGRHEX(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (NMGRHEX(L),'SEEN',1 )
         CALL GSATT (NMGRHEX(L),'COLO',COL_HEXCELL )
      END DO

c???
C.. 2a) Ground cathode panel :  panel head

      dz = Pc1DzPanelHead/2

      THET = 0.
      PHI = 0.
      H1 = PC1GRHEX/2.
      BL1 = (PC1RINST+PC1GRSHT)*TANG
      TL1  = BL1+PC1GRHEX*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = ALP1

      Par( 1) = DZ   ! half length along the Z axis
      Par( 2) = THET ! polar angle
      Par( 3) = PHI  ! azimuthal angle
      Par( 4) = H1   ! half length along Y of the face at -DZ
      Par( 5) = BL1  ! half length along X of the side at -H1
      Par( 6) = TL1  ! half length along X of the side at +H1
      Par( 7) = ALP1 ! angle 1 
      Par( 8) = H2   ! half length along Y of the face at +DZ
      Par( 9) = BL2  ! half length along X of the side at -H2
      Par(10) = TL2  ! half length along X of the side at +H2
      Par(11) = ALP2 ! angle 2

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmGrPnlHead(L),'TRAP',med_carbon_epoxy, 
     *	 Par, 11, IST)
         CALL GSATT (NmGrPnlHead(L),'SEEN',1 )  
         CALL GSATT (NmGrPnlHead(L),'COLO',col_fiber )
      END DO

C.. 2b) Ground cathode panel: cavity inside cathode panel head

      DX = Pc1DxPanelHeadCavity/2.
      DY = Pc1DyPanelHeadCavity/2.
      DZ = Pc1DzPanelHeadCavity/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmGrPnlHeadCavity(L),'BOX ',16, Par, 3, IST)
         CALL GSATT (NmGrPnlHeadCavity(L),'SEEN',1 )
         CALL GSATT (NmGrPnlHeadCavity(L),'COLO',col_pd_gas )
      END DO
c???

C.. 2a) Ground cathode panel: Layer of epoxy glue

      DX = (PC1RINST+PC1GRSHT+PC1GRHEX)*TANG
      DY = PC1EPOXY/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMGRGL2(L),'BOX ',MED_EPOXY_GLUE, PAR, 3, IST)
         CALL GSATT (NMGRGL2(L),'SEEN',1 )
         CALL GSATT (NMGRGL2(L),'COLO',COL_FIBER )
      END DO


C.. 3) Ground cathode panel: Carbon-Epoxy + Conductor 0.01" thick at 
*                            248.2304 cm

      DX = (PC1RINST+PC1GRSHT+PC1GRHEX)*TANG
      DY = PC1GRSHT/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMGRSH2(L),'BOX ',MED_GROUND_CATHODE, PAR, 3, IST)
         CALL GSATT (NMGRSH2(L),'SEEN',1 )
         CALL GSATT (NMGRSH2(L),'COLO',COL_FIBER )
      END DO

C.. 4) Sensitive volume: Gas (Argon-Ethane 50%-50%) 6 mm thick

      DZ = PC1DZGAS/2. 
      THET = 0.
      PHI = 0.
      H1 = PC1GASGP/2.
      BL1 = (PC1RINST+2.*PC1GRSHT+PC1GRHEX)*TANG
      TL1  = BL1+PC1GASGP*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = ALP1

      Par( 1) = DZ   ! half length along the Z axis
      Par( 2) = THET ! polar angle
      Par( 3) = PHI  ! azimuthal angle
      Par( 4) = H1   ! half length along Y of the face at -DZ
      Par( 5) = BL1  ! half length along X of the side at -H1
      Par( 6) = TL1  ! half length along X of the side at +H1
      Par( 7) = ALP1 ! angle 1 
      Par( 8) = H2   ! half length along Y of the face at +DZ
      Par( 9) = BL2  ! half length along X of the side at -H2
      Par(10) = TL2  ! half length along X of the side at +H2
      Par(11) = ALP2 ! angle 2

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMGAS(L),'TRAP',MED_PC_GAS, Par, 11, IST)
         CALL GSATT (NMGAS(L),'SEEN',1 )
         CALL GSATT (NMGAS(L),'COLO',COL_PD_GAS )
      END DO


c.. 4a) end beam inside gas volume. 

      DZ = Pc1DzEndBeam/2. 
      THET = 0.
      PHI = 0.
      H1 = PC1GASGP/4.
      BL1 = (PC1RINST+2.*PC1GRSHT+PC1GRHEX)*TANG
      TL1  = BL1+0.5*PC1GASGP*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = ALP1

      Par( 1) = DZ   ! half length along the Z axis
      Par( 2) = THET ! polar angle
      Par( 3) = PHI  ! azimuthal angle
      Par( 4) = H1   ! half length along Y of the face at -DZ
      Par( 5) = BL1  ! half length along X of the side at -H1
      Par( 6) = TL1  ! half length along X of the side at +H1
      Par( 7) = ALP1 ! angle 1 
      Par( 8) = H2   ! half length along Y of the face at +DZ
      Par( 9) = BL2  ! half length along X of the side at -H2
      Par(10) = TL2  ! half length along X of the side at +H2
      Par(11) = ALP2 ! angle 2

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmEndBeam(L),'TRAP ',med_carbon_epoxy,
     *	 PAR, 11, IST)
         CALL GSATT (NmEndBeam(L),'SEEN',1 )
         CALL GSATT (NmEndBeam(L),'COLO',COL_FIBER )
      END DO

C.. 4b) apply cavity on EndBeam

      DX = 15.5
      DY = pc1gasgp/4.
      DZ = 1.17

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmEndBeamCavity(L),'BOX ',16, PAR, 3, IST)
         CALL GSATT (NmEndBeamCavity(L),'SEEN',1 )
         CALL GSATT (NmEndBeamCavity(L),'COLO',COL_FIBER )
      END DO

c.. 4c) terminal board inside shelf. treat it as a complete trape 

      DZ = Pc1DzPanelHead/2. 
      THET = 0.
      PHI = 0.
      H1 = PC1GASGP/4.
      BL1 = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+0.5*PC1GASGP)*TANG
      TL1  = BL1+0.5*PC1GASGP*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = ALP1

      Par( 1) = DZ   ! half length along the Z axis
      Par( 2) = THET ! polar angle
      Par( 3) = PHI  ! azimuthal angle
      Par( 4) = H1   ! half length along Y of the face at -DZ
      Par( 5) = BL1  ! half length along X of the side at -H1
      Par( 6) = TL1  ! half length along X of the side at +H1
      Par( 7) = ALP1 ! angle 1 
      Par( 8) = H2   ! half length along Y of the face at +DZ
      Par( 9) = BL2  ! half length along X of the side at -H2
      Par(10) = TL2  ! half length along X of the side at +H2
      Par(11) = ALP2 ! angle 2

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmTermianlBrd(L),'TRAP ',
     *	 med_pc_board, PAR, 11, IST)
         CALL GSATT (NmTermianlBrd(L),'SEEN',1 )
         CALL GSATT (NmTermianlBrd(L),'COLO',COL_FIBER )
      END DO

C.. 4d) define spacer, simplified it as a box of 2x3x6 mm

      DX = 0.1
      DY = pc1gasgp/2
      DZ = 0.15

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmSpacer(L),'BOX ',med_carbon_epoxy, PAR, 3, IST)
         CALL GSATT (NmSpacer(L),'SEEN',1 )
         CALL GSATT (NmSpacer(L),'COLO',COL_FIBER )
      END DO

C.. 4e) define spacer cavity

      DX = 0.1
      DY = pc1gasgp/4
      DZ = 0.05

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmSpacerCavity(L),'BOX ',16, PAR, 3, IST)
         CALL GSATT (NmSpacerCavity(L),'SEEN',1 )
         CALL GSATT (NmSpacerCavity(L),'COLO',col_pd_gas)
      END DO

C.. 4f) define wire support, simplified it as 2 boxes on the side of spacer

      DX = (PC1RINST+2.*PC1GRSHT+PC1GRHEX)*TANG
      DY = pc1gasgp/4
      DZ = 0.05

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmWireSuprt(L),'BOX ',
     *	 med_carbon_epoxy, PAR, 3, IST)
         CALL GSATT (NmWireSuprt(L),'SEEN',1 )
         CALL GSATT (NmWireSuprt(L),'COLO',COL_FIBER )
      END DO


C.. 4g) define Gap Sealing, simplified it as a box 1.5x6xPC1DZGAS mm^3

      DZ = PC1DZGAS/2
      DY = pc1gasgp/2.
      DX = 0.075

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmGapSeal(L),'BOX ',med_carbon_epoxy, PAR, 3, IST)
         CALL GSATT (NmGapSeal(L),'SEEN',1 )
         CALL GSATT (NmGapSeal(L),'COLO',COL_FIBER )
      END DO
c???
C.. 4h) Wire plane: Consists of chamber gas, gold-tungsteen (anode wires),
*                   copper-berillium (field wires). The mixture is based
*                   on 60 anode and 60 field wires per sector and the wire
*                   plane is represented as a box of 100 micrometers thick
*                   with density and other parameters defined by the 
*                   weighted average per volume.

C.. Get it out for now...

*      DX = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+PC1GASGP/2.+
*     >      PC1WIRES/2.)*TANG
*      DY = PC1WIRES/2.
*      DZ = PC1DZEXT/2.

*      Par( 1) = DX
*      Par( 2) = DY
*      Par( 3) = DZ

*      DO L = 1, PC1NCHMB
*         CALL GSVOLU (NMWIRES(L),'BOX ',MED_WIRE_PLANE, Par, 3, IST)
*         CALL GSATT (NMWIRES(L),'SEEN',1 )
*         CALL GSATT (NMWIRES(L),'COLO',COL_PD_GAS )
*      END DO
       
C.. 5) Chevron/Pixel cathode panel: S2-glass-epoxy sheet + copper board
*                                   0.010" thick

      DX = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+PC1GASGP)*TANG
      DY = PC1CTSHT/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMCTSH1(L),'BOX ',MED_PC_BOARD, Par, 3, IST)
         CALL GSATT (NMCTSH1(L),'SEEN',1 )
         CALL GSATT (NMCTSH1(L),'COLO',COL_FIBER )
      END DO
      

C.. 5a) Chevron/Pixel cathode panel: Layer of epoxy glue

      DX = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+PC1GASGP)*TANG
      DY = PC1EPOXY/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMCTGL1(L),'BOX ',MED_EPOXY_GLUE, Par, 3, IST)
         CALL GSATT (NMCTGL1(L),'SEEN',1 )
         CALL GSATT (NMCTGL1(L),'COLO',COL_FIBER )
      END DO
      
C.. 6)  Chevron/Pixel cathode panel: HEXCELL sheet 1.0" thick

c???      DZ = PC1DZEXT/2.
      dz = (PC1DZEXT-2*Pc1DzPanelHead)/2

      THET = 0.
      PHI = 0.
      H1 = PC1CTHEX/2.
      BL1 = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+PC1CTSHT)*TANG
      TL1  = BL1+PC1CTHEX*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = ALP1

      Par( 1) = DZ   ! half length along the Z axis
      Par( 2) = THET ! polar angle
      Par( 3) = PHI  ! azimuthal angle
      Par( 4) = H1   ! half length along Y of the face at -DZ
      Par( 5) = BL1  ! half length along X of the side at -H1
      Par( 6) = TL1  ! half length along X of the side at +H1
      Par( 7) = ALP1 ! angle 1 
      Par( 8) = H2   ! half length along Y of the face at +DZ
      Par( 9) = BL2  ! half length along X of the side at -H2
      Par(10) = TL2  ! half length along X of the side at +H2
      Par(11) = ALP2 ! angle 2

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMCTHEX(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (NMCTHEX(L),'SEEN',1 )
         CALL GSATT (NMCTHEX(L),'COLO',COL_HEXCELL )
      END DO

c???
C.. 6)  Chevron/Pixel cathode panel: panel head

      dz = Pc1DzPanelHead/2

      THET = 0.
      PHI = 0.
      H1 = PC1CTHEX/2.
      BL1 = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+PC1CTSHT)*TANG
      TL1  = BL1+PC1CTHEX*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = ALP1

      Par( 1) = DZ   ! half length along the Z axis
      Par( 2) = THET ! polar angle
      Par( 3) = PHI  ! azimuthal angle
      Par( 4) = H1   ! half length along Y of the face at -DZ
      Par( 5) = BL1  ! half length along X of the side at -H1
      Par( 6) = TL1  ! half length along X of the side at +H1
      Par( 7) = ALP1 ! angle 1 
      Par( 8) = H2   ! half length along Y of the face at +DZ
      Par( 9) = BL2  ! half length along X of the side at -H2
      Par(10) = TL2  ! half length along X of the side at +H2
      Par(11) = ALP2 ! angle 2

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmCtPnlHead(L),'TRAP',
     *	 med_carbon_epoxy, Par, 11, IST)
         CALL GSATT (NmCtPnlHead(L),'SEEN',1 )
         CALL GSATT (NmCtPnlHead(L),'COLO',col_fiber )
      END DO

C.. 6a) Chevron/Pixel cathode panel: panel head cavity 310x20.4x18 mm^3

      DX = 15.5 
      DY = 1.02
      DZ = 0.9

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmCtPnlHeadCavity(L),'BOX ',16, Par, 3, IST)
         CALL GSATT (NmCtPnlHeadCavity(L),'SEEN',1 )
         CALL GSATT (NmCtPnlHeadCavity(L),'COLO',col_pd_gas )
      END DO
      
c???

C.. 6b) Chevron/Pixel cathode panel: Layer of epoxy glue

      DX = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+PC1CTSHT+
     +      PC1CTHEX)*TANG
      DY = PC1EPOXY/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMCTGL2(L),'BOX ',MED_EPOXY_GLUE, Par, 3, IST)
         CALL GSATT (NMCTGL2(L),'SEEN',1 )
         CALL GSATT (NMCTGL2(L),'COLO',COL_FIBER )
      END DO
  
C.. 7) Chevron/Pixel cathode panel: S2-glass-epoxy sheet 
*                                   0.010" thick

      DX = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+PC1CTSHT+
     +      PC1CTHEX)*TANG
      DY = PC1CTSHT/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMCTSH2(L),'BOX ',MED_S2_GLASS_EPOXY, Par, 3, IST)
         CALL GSATT (NMCTSH2(L),'SEEN',1 )
         CALL GSATT (NMCTSH2(L),'COLO',COL_FIBER )
      END DO
      
C.. 8) Electronics 1.0 cm thick - mother board, cables, connectors and chips

      DX = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+2.*PC1CTSHT+
     +      PC1CTHEX)*TANG
      DY = PC1ELECT/2.
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NMELEC(L),'BOX ',MED_MOTHER_BOARD, Par, 3, IST)
         CALL GSATT (NMELEC(L),'SEEN',1 )
         CALL GSATT (NMELEC(L),'COLO',COL_ELECTR )
      END DO
           
C.. And finally, last layer:

C.. 9) Sealing Strip

      DX = 2.905
      DY = 0.01
      DZ = PC1DZEXT/2.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC1NCHMB
         CALL GSVOLU (NmSealStrp(L),'BOX ',
     *	 med_carbon_epoxy, Par, 3, IST)
         CALL GSATT (NmSealStrp(L),'SEEN',1 )
         CALL GSATT (NmSealStrp(L),'COLO',COL_FIBER )
      END DO
           

C.. ----------------- POSITION THE VOLUMES ------------------------

********************************************************************
*                                                                  * 
*      WE FINISHED WITH VOLUMES AND COME TO SPACE AND POSITION     * 
*                 PROBLEM                                          *
*                                                                  *
********************************************************************
C.. Positioning the PC's envelope in intermediate tracker's (INTR):

*   At the moment INTR is defined between 100 and 250 cm in r.
*   The current version of engineering design (August 1995) allocates 
*   for PCs a radial space between 246.3 cm (inscribe radius) and 
*   253.7 cm (outscribe radius). Thus, the PCs do not fully fit into
*   provided space. 


      DO 715 J = 1, PC1NCHMB
         JPT = 1
	 LPT = 2
         FICNTR = PIBY2  + TETGT( J)   * DEGRAD

         SNFI = SIN ( FICNTR)
         CSFI = COS ( FICNTR)

         RPSPD1 = PC1RINST+DHPC1/2. ! inner radius + half of the thickness

         XCNTP1 =   RPSPD1            * CSFI
         YCNTP1 =   RPSPD1            * SNFI

C.. Positioning "shell" volumes into INTR-volume:

c     Modified (C.F. Maguire, April 28, 2000) for EAST WEST split


         if(j.le.8)then
            CALL GSPOS (NMPAD1(J), 1,ePC1, XCNTP1, YCNTP1, ZCNTR,
     +                  IRTTR(J), 'ONLY' )
         else
            CALL GSPOS (NMPAD1(J), 1,wPC1, XCNTP1, YCNTP1, ZCNTR,
     +                  IRTTR(J), 'ONLY' )
         endif


C.. Positioning all layers inside their "shell" volume:

C.. Starting from inside to outside, in the same order as volumes were
*   defined above:

         XCNTP1 = 0.
         YCNTP1 = 0. 

C.. 1) 
         YCNTP1 = -DHPC1/2.+PC1GRSHT/2. 

         CALL GSPOS (NMGRSH1(J),JPT,NMPAD1(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

C.. 2)  
         YCNTP1 = -DHPC1/2.+PC1GRSHT+PC1GRHEX/2. 

         CALL GSPOS (NMGRHEX(J),JPT,NMPAD1(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )
c???
C.. 2a)  position cathode panel head first 

	 XPanelHead = 0
         YPanelHead = -DHPC1/2.+PC1GRSHT+PC1GRHEX/2. 
	 ZPanelHead =(PC1DZEXT-Pc1DzPanelHead)/2.

         CALL GSPOS (NmGrPnlHead(J),JPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, ZPanelHead, Rotat180, 'ONLY' )

         CALL GSPOS (NmGrPnlHead(J),LPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, -ZPanelHead, 0, 'ONLY' )

C.. 2b)  position the 2 cavity on each cathode panel head now

	 XPanelHead = 12.23  ! (94.6 + 150)/2 (mm)
         YPanelHead = 0.4    ! (25.4-17.4)/2
	 ZPanelHead = 1.24   ! 25.4/2 +39 -78.6/2 (mm)

         CALL GSPOS (NmGrPnlHeadCavity(J),JPT,NmGrPnlHead(J), 
     *   XPanelHead, YPanelHead, ZPanelHead, 0, 'ONLY' )

         CALL GSPOS (NmGrPnlHeadCavity(J),LPT,NmGrPnlHead(J),
     *	 -XPanelHead, YPanelHead, ZPanelHead, 0, 'ONLY' )

c???

C.. 3)
         YCNTP1 = -DHPC1/2.+PC1GRSHT+PC1GRHEX+PC1GRSHT/2. 

         CALL GSPOS (NMGRSH2(J),JPT,NMPAD1(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

C.. 4)
         YCNTP1 = -DHPC1/2.+2.*PC1GRSHT+PC1GRHEX+PC1GASGP/2. 

         CALL GSPOS (NMGAS(J),JPT,NMPAD1(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

c???
C.. 4a)  position  End Beam inside  shell

	 XPanelHead = 0
         YPanelHead = -DHPC1/2.+2.*PC1GRSHT+PC1GRHEX+PC1GASGP/4.
	 ZPanelHead =(PC1DZEXT-Pc1DzEndBeam)/2.

         CALL GSPOS (NmEndBeam(J),JPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, ZPanelHead, Rotat180, 'ONLY' )

         CALL GSPOS (NmEndBeam(J),LPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, -ZPanelHead, 0, 'ONLY' )

C.. 4b)  position  End Beam cavity

	 XPanelHead = 0
         YPanelHead = 0
	 ZPanelHead = -0.78  ! (39-23.4)/2 mm

         CALL GSPOS (NmEndBeamCavity(J),JPT,NmEndBeam(J),
     *	 XPanelHead, YPanelHead, ZPanelHead, 0, 'ONLY' )

C.. 4c)  position terminal board 

	 XPanelHead = 0
         YPanelHead = -DHPC1/2.+2.*PC1GRSHT+PC1GRHEX+PC1GASGP*3/4.
	 ZPanelHead =(PC1DZEXT-Pc1DzPanelHead)/2.

         CALL GSPOS (NmTermianlBrd(J),JPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, ZPanelHead, 0, 'ONLY' )

         CALL GSPOS (NmTermianlBrd(J),LPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, -ZPanelHead, 0, 'ONLY' )

C.. 4d)  position spacer in gas volume

         YPanelHead = 0
	 ZPanelHead = 0
	 SpacerIntvl = 3.57   ! 71.4/2 mm
	 no=0
	 do ispacer = -5,5,2
	   no=no+1
	   XPanelHead = ispacer*SpacerIntvl
           CALL GSPOS (NmSpacer(J),no,NMGAS(J), XPanelHead, 
     *	   YPanelHead, ZPanelHead, 0, 'ONLY' )
	 end do
	 
C.. 4f)  position spacer cavity inside spacer

	 XPanelHead = 0
         YPanelHead = -PC1GASGP/4
	 ZPanelHead = 0
         CALL GSPOS(NmSpacerCavity(J),JPT,NmSpacer(J), XPanelHead, 
     *	 YPanelHead, ZPanelHead, 0, 'ONLY' )
	 
C.. 4g)  position wire support

	 XPanelHead = 0
         YPanelHead = -PC1GASGP/4
	 ZPanelHead = 0
         CALL GSPOS(NmWireSuprt(J),JPT,NMGAS(J), XPanelHead, 
     *	 YPanelHead, ZPanelHead, 0, 'ONLY' )
	 
c.. 4h)  position gap sealing inside gas volume

	 XPanelHead = (PC1RINST+2.*PC1GRSHT+PC1GRHEX)*TANG
         YPanelHead = 0
	 ZPanelHead = 0

         CALL GSPOS (NmGapSeal(J),JPT,NMGAS(J), XPanelHead, 
     *	 YPanelHead, ZPanelHead, 0, 'ONLY' )

         CALL GSPOS (NmGapSeal(J),LPT,NMGAS(J), -XPanelHead, 
     *	 YPanelHead, ZPanelHead, 0, 'ONLY' )

c???
C.. 5)
         YCNTP1 = -DHPC1/2.+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+PC1CTSHT/2. 

         CALL GSPOS (NMCTSH1(J),JPT,NMPAD1(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

C.. 6)
         YCNTP1 = -DHPC1/2.+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+PC1CTSHT+
     +            PC1CTHEX/2. 

         CALL GSPOS (NMCTHEX(J),JPT,NMPAD1(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

c.. 6a)  position pixel board panel head

	 XPanelHead = 0
         YPanelHead = -DHPC1/2.+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+PC1CTSHT+
     *                PC1CTHEX/2.
	 ZPanelHead = (PC1DZEXT-Pc1DzPanelHead)/2

         CALL GSPOS (NmCtPnlHead(J),JPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, ZPanelHead, Rotat180, 'ONLY' )

         CALL GSPOS (NmCtPnlHead(J),LPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, -ZPanelHead, 0, 'ONLY' )

c.. 6b)  position pixel board panel head cavity inside panel cavity

	 XPanelHead = 0
         YPanelHead = -0.25  ! -(25.4-20.4)/2
	 ZPanelHead = -2.53  ! 78.6/2 - 5 - 18/2

         CALL GSPOS (NmCtPnlHeadCavity(J),JPT,NmCtPnlHead(J),
     *	 XPanelHead, YPanelHead, ZPanelHead, 0, 'ONLY' )

c???

C.. 7)
         YCNTP1 = -DHPC1/2.+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+PC1CTSHT+
     +            PC1CTHEX+PC1CTSHT/2. 

         CALL GSPOS (NMCTSH2(J),JPT,NMPAD1(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

C.. 8)
         YCNTP1 = -DHPC1/2.+2.*PC1GRSHT+PC1GRHEX+PC1GASGP+2.*PC1CTSHT+
     +            PC1CTHEX+PC1ELECT/2. 

         CALL GSPOS (NMELEC(J),JPT,NMPAD1(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

C.. 9) position Sealing strip

	 XPanelHead = (PC1RINST+2.*PC1GRSHT+PC1GRHEX+0.5*PC1GASGP)*TANG
         YPanelHead = 0
	 ZPanelHead = 0

         CALL GSPOS (NmSealStrp(J),JPT,NMPAD1(J), XPanelHead, 
     *	 YPanelHead, ZPanelHead, RotatSealStrp(1), 'ONLY' )

         CALL GSPOS (NmSealStrp(J),LPT,NMPAD1(J), -XPanelHead, 
     *	 YPanelHead, ZPanelHead, RotatSealStrp(2), 'ONLY' )

C.. Finished with first-level volumes. Now come to: 
*   Glue inside HEXCELL, Wires inside GAS. 

C.. 1a) Glue
         YCNTP1 = -PC1GRHEX/2.+PC1EPOXY/2.

         CALL GSPOS (NMGRGL1(J),JPT,NMGRHEX(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

C.. 2a) Glue
         YCNTP1 = PC1GRHEX/2.-PC1EPOXY/2.

         CALL GSPOS (NMGRGL2(J),JPT,NMGRHEX(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

C.. 4a) Wires - get it out for now
*         YCNTP1 = 0.

*         CALL GSPOS (NMWIRES(J),JPT,NMGAS(J), XCNTP1, YCNTP1, ZCNTR,
*     +   0, 'ONLY' )

C.. 5a) Glue
         YCNTP1 = -PC1CTHEX/2.+PC1EPOXY/2. 

         CALL GSPOS (NMCTGL1(J),JPT,NMCTHEX(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

C.. 6a) Glue
         YCNTP1 = PC1CTHEX/2.-PC1EPOXY/2.

         CALL GSPOS (NMCTGL2(J),JPT,NMCTHEX(J), XCNTP1, YCNTP1, ZCNTR,
     +   0, 'ONLY' )

  715 CONTINUE

C.. -------------------- END OF DETECTOR GEOMETRY SETUP ------------------ 



      IF( JFLBNK .GT. 0 ) THEN

*     The bank was created in dcgeom

        ncl1mx = nmpd1c                  ! number of PC1 sectors

        iqf(lfi_para + 4) = 19980215  ! key in the date here for PC (STAF)
        iqf( lfi_para+6) = ncl1mx       

c     CFM: NCL1MZ is a local variable which was never initialized
c          This position is being taken over for PC1NEW logical switch

c       iqf( lfi_para+7) = ncl1mz

        iqf( lfi_para+7) = -1      ! -1 indicates new PC1 geometry in PISORP

c all the rest are no longer in use


      ENDIF

 9999 continue
      return
      
c---------------------------------------------------------------------
  999 continue
      write(6,1000)
 1000  format(/'pc1gem - read error in itr_pc1_par segment'/,3x, 
     +   '  Namelist mis-match in itr_pc1_par segment ?',//,3x, 
     +   'The geometry will be re-read to pinpoint the erroneous',
     +  ' line',/,3x,'****This will cause the program to crash.****',//)
        
      rewind( itf_lun )
      read( itf_lun, nml = itr_pc1_par )
      stop ' pc1gem - stop because of phnx.par file error.'
      end
