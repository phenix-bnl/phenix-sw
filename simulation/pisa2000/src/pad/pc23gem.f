c $Id: pc23gem.f,v 1.4 2008/05/21 08:22:03 hpereira Exp $
      SUBROUTINE PC23GEM(NMPD23)
*       ============================

C         DEFINE USER GEOMETRY SET UP
*    for pad detectors 2,3:  called from PAD
C*************************************************


C..  Revised by Kirill Filimonov (August, 1995) 
c     Revised by C.F. Maguire (December 18, 1995)
c                put in 699 medium gas for PC3 (key for PADSTP routine)
c                use para bank to indicate new PC geometry

 
C.. "Realistic" Geometry is done for PC2/PC3 detectors. New parameters are
C..  introduced in the namelist detector is put in its own envelope
C..  with no dependence on TEC. 


c     Revision History
c    Name           Date            Comment
c    C.F. Maguire   Feb. 15, 1998   Add global coordinates and path length

c                                   Add date key to lfp_para bank

c    A.A. Rose      Mar. 19, 1998   Change Z = 0 frame structure

      IMPLICIT NONE

c     Calling parameter

      integer nmpd23   ! number of PC23 sectors from namelist file


c     Local variables for parameter bank

      integer lfp_pd23     ! base pointer for the parameter bank PC2/PC3 values
      character*10 chform  ! data format for ZEBRA parameter bank
      integer iod          ! return format from ZEBRA

 
#include "gcflag.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpplink.inc"
#include "fstore.inc"
  
**********************************************************************
*                                                                    *
*                       P C 2    L A Y O U T                         *    
*                                                                    * 
**********************************************************************

*  THIS IS THE VIEW OF HALF PC2 SECTOR ALONG Z-DIRECTION: 

*                                     | radial distance     
*                        center bars
*___________________________________\_  423.450 cm
*\-------------------------------||---:  423.399 cm
* \\  \\                         ||  
*  \\  \\ GROUND CATHODE (1.25") ||   |
*   \\  \\                       ||   |
*   /\\__\\______________________||___   420.223 cm
*  /  \-------------------------------:  420.172 cm
* /    \\  \\  . GAS GAP (10 MM). . .    419.672 cm  <- wire plane
* |    /\\__\\________________________|  419.172 cm
* |   /  \-----------------------||---|  419.147 cm
* |   |   \\  \\                 ||
* |   |  / \\  \\ PIXEL/CHEVRON  ||   :
* side bars \\  \\ CATHODE(1.25")||  
* (support)  \\__\\______________||___|  415.972 cm  
*             \-----------------------|  415.947 cm
*              \ ELECTRONICS (~1.0cm) 
*               \---------------------:  415.000 cm
*                \                  . '  
*                 \             . `   |
*                  \        . `       |
*                   \     .`          
*                    \  .` Angle=11.25:
*                     \.     degrees    
*                      \              |

**********************************************************************
*                                                                    * 
*                       P C 3    L A Y O U T                         *    
*                                                                    *
**********************************************************************

*  THIS IS THE VIEW OF HALF PC3 SECTOR ALONG Z-DIRECTION: 

*                                      radial distance

* ------------------------------------|  498.1240 cm
*|      ELECTRONICS (1.0 CM)          |
*|____________________________________   497.1240 cm
*\-------------------------------||---:  497.0732 cm
* \\  \\                         ||  
*  \\  \\ PIXEL/CHEVRON CATHODE  ||   |
*   \\  \\     (1.50")           ||   |
*    \\__\\______________________||___   493.2632 cm
*     \-------------------------------:  493.2124 cm
*      \\  \\  . GAS GAP (12 MM). . .    492.6124 cm  <- wire plane
*       \\__\\________________________|  492.0124 cm
*        \-----------------------||---|  491.9362 cm
*         \\  \\                 ||
*          \\  \\    GROUND      ||   :
*           \\  \\ CATHODE(1.50")||  
*            \\__\\______________||___|  488.1262 cm 
*             \-----------------------|  488.0500 cm
*              \                    . '  
*               \               . `   |
*                \          . `       |
*                 \       .`          
*                  \    .` Angle=11.25:
*                   \  .     degrees    
*                    \.               |

**********************************************************************

C.. Description of introduced parameters (K. Filimonov)
*   Following the order of the namelist file, $pc23_par part of phnx.par:

*.. - - - - - -   S t a r t   O f  D e s c r i p t i o n   - - - - - - ..*

* pc23nchmb = 8,      ! Total number of sectors of PC2/PC3 
* pc23tetgt = 22.5,   ! Angle covered by each PC2/PC3 sector 
* pc2dzext = 333.32,  ! Maximum Z extent of PC2
* pc3dzext = 386.09,  ! Maximum Z extent of PC3
* pc2rinst = 415.00,  ! Inner inscribe radius of PC2
* pc3rinst = 488.05,  ! Inner inscribe radius of PC3
* pc2grsht = 0.0508,  ! Thickness of PC2 ground cathode carbon-epoxy sheets
* pc3grsht = 0.0762,  ! Thickness of PC3 ground cathode carbon-epoxy sheets 
* pc2grhex = 3.175,   ! Thickness of PC2 ground cathode HEXCELL panel (1.25")
* pc3grhex = 3.810,   ! Thickness of PC3 ground cathode HEXCELL panel (1.50")
* pc2gasgp = 1.0,     ! Gas gap width of PC2
* pc3gasgp = 1.2,     ! Gas gap width of PC3
* pc2ctsht = 0.0254,  ! Thickness of PC2 chevron/pixel cathode board (0.010")
* pc3ctsht = 0.0508,  ! Thickness of PC3 chevron/pixel cathode board (0.020") 
* pc2cthex = 3.175,   ! Thickness of PC2 chevron/pixel cathode HEXCELL panel
* pc3cthex = 3.810,   ! Thickness of PC3 chevron/pixel cathode HEXCELL panel
* pc2elect = 0.947,   ! Thickness of PC2 Electronics 
* pc3elect = 1.0,     ! Thickness of PC3 Electronics
* pc23sbar = 1.905,   ! Width of carbon-fiber side bars of PC2/PC3 (0.75")
* pc23sbth = 0.4,     ! Thickness of walls of carbon-fiber side bars 
* pc23cbar = 5.08,    ! Width of carbon-fiber center bars of PC2/PC3 (2") 
* pc23cbth = 0.2,     ! Thickness of walls of carbon-fiber center bars
* pc23epoxy = 0.0025  ! Epoxy glue thickness (not used)
* pc23wires = 0.01,   ! Wire plane thickness (not used)
* med_carbon_epoxy = 690,   ! Carbon-fiber/Epoxy
* med_s2_glass_epoxy = 691, ! S2-Glass/Epoxy 
* med_hexcell = 692,        ! HEXCELL 
* med_epoxy_glue = 693,     ! Epoxy glue (not used)
* med_pc_gas = 694,         ! Gas 
* med_pc_board = 695,       ! Chevron/Pixel board (Carbon-fiber/Epoxy+Copper)
* med_ground_cathode = 696, ! Ground cathode (S2-Glass/Epoxy+Conductor?)
* med_mother_board = 697,   ! Electronics (Mother board+Chips+Connectors)
* med_wire_plane = 698,     ! Wire plane (not used)
* col_sector = 4,     ! Color of sector's shell
* col_hexcell = 0,    ! Color of hexcell
* col_fiber = 0,      ! Color of carbon-epoxy, S2 fiberglass and epoxy
* col_pd_gas = 0,     ! Color of PC's gas
* col_electr = 0,     ! Color of electronics
* delphi1 = -11.25,   ! Shift of the first arm
* delphi2 = +11.25,   ! Shift of the second arm  

*.. - - - - - - - -  E n d   O f   D e s c r i p t i o n   - - - - - - ..*

C.. Namelist parameters:

      INTEGER PC23NCHMB
      REAL PC23TETGT
      REAL PC2DZEXT, PC2RINST
      REAL PC3DZEXT, PC3RINST
      REAL PC2DZGAS, PC3DZGAS

      REAL PC2GRSHT, PC2GRHEX, PC2GASGP, PC2CTSHT, PC2CTHEX, PC2ELECT 
      REAL PC3GRSHT, PC3GRHEX, PC3GASGP, PC3CTSHT, PC3CTHEX, PC3ELECT 

      REAL PC23SBAR, PC23SBTH, PC23CBAR, PC23CBTH
      REAL PC23EPOXY, PC23WIRES

      INTEGER MED_CARBON_EPOXY, MED_S2_GLASS_EPOXY, MED_HEXCELL
      INTEGER MED_EPOXY_GLUE, MED_PC_GAS,  MED_WIRE_PLANE
      INTEGER MED_PC_BOARD, MED_GROUND_CATHODE, MED_MOTHER_BOARD

      INTEGER COL_SECTOR, COL_HEXCELL, COL_FIBER, COL_PD_GAS, COL_ELECTR

C..  Local Variables:

      CHARACTER*50 PAR_FILE

      REAL AGAS(3), ZGAS(3), WGAS(3)
      REAL ACATHODE(2), ZCATHODE(2), WCATHODE(2)
      REAL AGROUND(2), ZGROUND(2), WGROUND(2)
      REAL AELECTR(3), ZELECTR(3), WELECTR(3)
 
      REAL T1, T2, T3, F1, F2, F3
      INTEGER L, JRT, IST, J, JPT, LPT
      INTEGER IRTTR(8)

      REAL PAR(11)
      REAL DZ, THET, PHI, H1, BL1, TL1, ALP1, H2, BL2, TL2, ALP2
      REAL TANG, SINE, COSINE, DHPC2, DHPC3
      REAL DX, DY
      REAL ALPH

      REAL A, Z, DENS, RADL, ABSL, UBUF
      INTEGER IMATE, NWBUF

      INTEGER IFIELD
      REAL FIELDM, TMAXFD, DMAXMS, DEEMAX, EPSIL, STMIN

      REAL TETGT(8)
      REAL RPSPD1, FICNTR, SNFI, CSFI, XCNTP1, YCNTP1, ZCNTR, OZCNTR

      CHARACTER*4 VPC2SHELL(8), VPC2GRSH1(8), VPC2GRSH2(8), VPC2GRHXL(8)
      CHARACTER*4 VPC2GRHXR(8), VPC2CTSH1(8), VPC2CTSH2(8), VPC2CTHXL(8) 
      CHARACTER*4 VPC2CTHXR(8), VPC2GASGP(8), VPC2ELECT(8) 
      CHARACTER*4 VPC2SBCLI(8), VPC2SBCLO(8), VPC2SBGLI(8), VPC2SBGLO(8) 
      CHARACTER*4 VPC2SBCRI(8), VPC2SBCRO(8), VPC2SBGRI(8), VPC2SBGRO(8) 
      CHARACTER*4 VPC2SBWLI(8), VPC2SBWLO(8), VPC2SBWRI(8), VPC2SBWRO(8)
      CHARACTER*4 VPC2CBCTI(8), VPC2CBCTO(8), VPC2CBGRI(8), VPC2CBGRO(8)

      CHARACTER*4 VPC3SHELL(8), VPC3GRSH1(8), VPC3GRSH2(8), VPC3GRHXL(8)
      CHARACTER*4 VPC3GRHXR(8), VPC3CTSH1(8), VPC3CTSH2(8), VPC3CTHXL(8) 
      CHARACTER*4 VPC3CTHXR(8), VPC3GASGP(8), VPC3ELECT(8) 
      CHARACTER*4 VPC3SBCLI(8), VPC3SBCLO(8), VPC3SBGLI(8), VPC3SBGLO(8) 
      CHARACTER*4 VPC3SBCRI(8), VPC3SBCRO(8), VPC3SBGRI(8), VPC3SBGRO(8) 
      CHARACTER*4 VPC3SBWLI(8), VPC3SBWLO(8), VPC3SBWRI(8), VPC3SBWRO(8)
      CHARACTER*4 VPC3CBCTI(8), VPC3CBCTO(8), VPC3CBGRI(8), VPC3CBGRO(8)

      real delphi1 /0.0/   ! Shift of the first arm  (August, 1994 decision)
      real delphi2 /0.0/   ! Shift of the second arm (August, 1994 decision)

CAAR  volume names for central support
      CHARACTER*4 VPC2Z0SP(8), VPC3Z0SP(8)
      real sbardz /10./, sbardy/10./, hollow /.4/
      real enbdz, enbdy, tbar1dy , tbar1dz, tbar2dy, tbar2dz
      real Ibar1dy,Ibar1dz,Ibar2dy,Ibar2dz
      real sbarpz,sbarpx,sbarpy
      real yshift,yshift2,p3yoff
      logical ibeamtf

C     Central support parameters
      real gap /2./, gap2,splitcenter2             !calculated gap width, new
      real splitgas2                     !center-of-volumes
      integer med_air /10/
      integer med_s2 /10/, med_alu /10/
      REAL GZCNTR
CAAR

C.. Read PC23 parameters through Namelist /PC23_PAR/

      NAMELIST / PC23_PAR/ PC23NCHMB, PC23TETGT, PC2DZEXT, PC3DZEXT,
     + PC2RINST, PC3RINST, PC2GRSHT, PC3GRSHT, PC2GRHEX, PC3GRHEX, 
     + PC2GASGP, PC3GASGP, PC2CTSHT, PC3CTSHT, PC2CTHEX, PC3CTHEX, 
     + PC2ELECT, PC3ELECT, PC23SBAR, PC23SBTH, PC23CBAR, PC23CBTH, 
     + PC23EPOXY, PC23WIRES, MED_CARBON_EPOXY, MED_S2_GLASS_EPOXY, 
     + MED_HEXCELL, MED_EPOXY_GLUE, MED_PC_GAS, MED_PC_BOARD, 
     + MED_GROUND_CATHODE, MED_MOTHER_BOARD, MED_WIRE_PLANE, 
     + COL_SECTOR, COL_HEXCELL, COL_FIBER, COL_PD_GAS, COL_ELECTR,
     + delphi1, delphi2, sbardz, sbardy, enbdz,enbdy, tbar1dy,
     + tbar1dz,sbarpz,sbarpx,sbarpy, Ibar1dy,Ibar1dz,Ibar2dy,Ibar2dz,
     + tbar2dz,tbar2dy,yshift, ibeamtf, med_s2, med_alu,yshift2,p3yoff
C.. Detector volumes names:

C.. --------------------------  P C  2  ----------------------------..C

C.. Local "mother (shell)" volumes:
      DATA VPC2SHELL /'PD21', 'PD22', 'PD23', 'PD24', 'PD25', 'PD26',
     +'PD27', 'PD28' /

C.. Ground cathode's carbon-epoxy sheet at inner radius
      DATA VPC2GRSH1 /'IG21', 'IG22', 'IG23', 'IG24', 'IG25', 'IG26',
     +'IG27', 'IG28' /
  
C.. Ground cathode's carbon-epoxy sheet at outer radius
      DATA VPC2GRSH2 /'OG21', 'OG22', 'OG23', 'OG24', 'OG25', 'OG26',
     +'OG27', 'OG28' /

C.. Ground cathode's HEXCELL (honeycomb core) sheet (left side) 
      DATA VPC2GRHXL /'HL21', 'HL22', 'HL23', 'HL24', 'HL25', 'HL26',
     +'HL27', 'HL28' /

C.. Ground cathode's HEXCELL (honeycomb core) sheet (right side) 
      DATA VPC2GRHXR /'HR21', 'HR22', 'HR23', 'HR24', 'HR25', 'HR26',
     +'HR27', 'HR28' /

C.. Chevron/pixel cathode's S2-glass-epoxy sheet at inner radius
      DATA VPC2CTSH1 /'IC21', 'IC22', 'IC23', 'IC24', 'IC25', 'IC26',
     +'IC27', 'IC28' /

C.. Chevron/pixel cathode's S2-glass-epoxy sheet at outer radius  
      DATA VPC2CTSH2 /'OC21', 'OC22', 'OC23', 'OC24', 'OC25', 'OC26',
     +'OC27', 'OC28' /

C.. Chevron/pixel cathode's HEXCELL sheet (left side)
      DATA VPC2CTHXL /'XL21', 'XL22', 'XL23', 'XL24', 'XL25', 'XL26',
     +'XL27', 'XL28' /

C.. Chevron/pixel cathode's HEXCELL sheet (right side)
      DATA VPC2CTHXR /'XR21', 'XR22', 'XR23', 'XR24', 'XR25', 'XR26',
     +'XR27', 'XR28' /

C.. Sensitive volume: Gas (argon-ethane 50-50%) gap
      DATA VPC2GASGP /'AR21', 'AR22', 'AR23', 'AR24', 'AR25', 'AR26',
     +  'AR27', 'AR28' /

C.. Electronics - effective density to be defined later...
      DATA VPC2ELECT /'EL21', 'EL22', 'EL23', 'EL24', 'EL25', 'EL26',
     +'EL27', 'EL28' /

C.. Support: Side Bars, Pixel/Chevron Cathode, left, outside
      DATA VPC2SBCLO /'SL21','SL22', 'SL23', 'SL24', 'SL25', 'SL26',
     +'SL27', 'SL28' /

C.. Support: Side Bars, Pixel/Chevron Cathode, left, inside
      DATA VPC2SBCLI /'SI21','SI22', 'SI23', 'SI24', 'SI25', 'SI26',
     +'SI27', 'SI28' /

C.. Support: Side Bars, Pixel/Chevron Cathode, right, outside
      DATA VPC2SBCRO /'RR21','RR22', 'RR23', 'RR24', 'RR25', 'RR26',
     +'RR27', 'RR28' /

C.. Support: Side Bars, Pixel/Chevron Cathode, right, inside
      DATA VPC2SBCRI /'RI21','RI22', 'RI23', 'RI24', 'RI25', 'RI26',
     +'RI27', 'RI28' /

C.. Support: Side Bars, Ground Cathode, left, outside
      DATA VPC2SBGLO /'GL21','GL22', 'GL23', 'GL24', 'GL25', 'GL26',
     +'GL27', 'GL28' /

C.. Support: Side Bars, Ground Cathode, left, inside
      DATA VPC2SBGLI /'GI21','GI22', 'GI23', 'GI24', 'GI25', 'GI26',
     +'GI27', 'GI28' /

C.. Support: Side Bars, Ground Cathode, right, outside
      DATA VPC2SBGRO /'GO21','GO22', 'GO23', 'GO24', 'GO25', 'GO26',
     +'GO27', 'GO28' /

C.. Support: Side Bars, Ground Cathode, right, inside
      DATA VPC2SBGRI /'GR21','GR22', 'GR23', 'GR24', 'GR25', 'GR26',
     +'GR27', 'GR28' /

C.. Support: Side Bars, Gas Gap, left, outside
      DATA VPC2SBWLO /'WL21','WL22', 'WL23', 'WL24', 'WL25', 'WL26',
     +'WL27', 'WL28' /

C.. Support: Side Bars, Gas Gap, left, inside
      DATA VPC2SBWLI /'IW21','IW22', 'IW23', 'IW24', 'IW25', 'IW26',
     +'IW27', 'IW28' /

C.. Support: Side Bars, Gas Gap, right, outside
      DATA VPC2SBWRO /'WO21','WO22', 'WO23', 'WO24', 'WO25', 'WO26',
     +'WO27', 'WO28' /

C.. Support: Side Bars, Gas Gap, right, inside
      DATA VPC2SBWRI /'WR21','WR22', 'WR23', 'WR24', 'WR25', 'WR26',
     +'WR27', 'WR28' /

C.. Support: Center Bars, Pixel/Chevron Cathode, outside
      DATA VPC2CBCTO /'CO21','CO22', 'CO23', 'CO24', 'CO25', 'CO26',
     +'CO27', 'CO28' /

C.. Support: Center Bars, Pixel/Chevron Cathode, inside
      DATA VPC2CBCTI /'CI21','CI22', 'CI23', 'CI24', 'CI25', 'CI26',
     +'CI27', 'CI28' /

C.. Support: Center Bars, Ground Cathode, outside
      DATA VPC2CBGRO /'YO21','YO22', 'YO23', 'YO24', 'YO25', 'YO26',
     +'YO27', 'YO28' /

C.. Support: Center Bars, Ground Cathode, inside
      DATA VPC2CBGRI /'YI21','YI22', 'YI23', 'YI24', 'YI25', 'YI26',
     +'YI27', 'YI28' /

CAAR Support: z=0 bars
      DATA VPC2Z0SP /'SPI2','SPA2','ENB2','ST12','ST22','CR12',
     +'CR22', '2IPS' /
CAAR


      
C.. --------------------------  P C  3  ----------------------------..C


C.. Local "mother (shell)" volumes:
      DATA VPC3SHELL /'PD31', 'PD32', 'PD33', 'PD34', 'PD35', 'PD36',
     +'PD37', 'PD38' /

C.. Ground cathode's carbon-epoxy sheet at inner radius
      DATA VPC3GRSH1 /'IG31', 'IG32', 'IG33', 'IG34', 'IG35', 'IG36',
     +'IG37', 'IG38' /
  
C.. Ground cathode's carbon-epoxy sheet at outer radius
      DATA VPC3GRSH2 /'OG31', 'OG32', 'OG33', 'OG34', 'OG35', 'OG36',
     +'OG37', 'OG38' /

C.. Ground cathode's HEXCELL (honeycomb core) sheet (left side) 
      DATA VPC3GRHXL /'HL31', 'HL32', 'HL33', 'HL34', 'HL35', 'HL36',
     +'HL37', 'HL38' /

C.. Ground cathode's HEXCELL (honeycomb core) sheet (right side) 
      DATA VPC3GRHXR /'HR31', 'HR32', 'HR33', 'HR34', 'HR35', 'HR36',
     +'HR37', 'HR38' /

C.. Chevron/pixel cathode's S2-glass-epoxy sheet at inner radius
      DATA VPC3CTSH1 /'IC31', 'IC32', 'IC33', 'IC34', 'IC35', 'IC36',
     +'IC37', 'IC38' /

C.. Chevron/pixel cathode's S2-glass-epoxy sheet at outer radius  
      DATA VPC3CTSH2 /'OC31', 'OC32', 'OC33', 'OC34', 'OC35', 'OC36',
     +'OC37', 'OC38' /

C.. Chevron/pixel cathode's HEXCELL sheet (left side)
      DATA VPC3CTHXL /'XL31', 'XL32', 'XL33', 'XL34', 'XL35', 'XL36',
     +'XL37', 'XL38' /

C.. Chevron/pixel cathode's HEXCELL sheet (right side)
      DATA VPC3CTHXR /'XR31', 'XR32', 'XR33', 'XR34', 'XR35', 'XR36',
     +'XR37', 'XR38' /

C.. Sensitive volume: Gas (argon-ethane 50-50%) gap
      DATA VPC3GASGP /'AR31', 'AR32', 'AR33', 'AR34', 'AR35', 'AR36',
     +'AR37', 'AR38' /

C.. Electronics - effective density to be defined later...
      DATA VPC3ELECT /'EL31', 'EL32', 'EL33', 'EL34', 'EL35', 'EL36',
     +'EL37', 'EL38' /

C.. Support: Side Bars, Pixel/Chevron Cathode, left, outside
      DATA VPC3SBCLO /'SL31','SL32', 'SL33', 'SL34', 'SL35', 'SL36',
     +'SL37', 'SL38' /

C.. Support: Side Bars, Pixel/Chevron Cathode, left, inside
      DATA VPC3SBCLI /'SI31','SI32', 'SI33', 'SI34', 'SI35', 'SI36',
     +'SI37', 'SI38' /

C.. Support: Side Bars, Pixel/Chevron Cathode, right, outside
      DATA VPC3SBCRO /'RR31','RR32', 'RR33', 'RR34', 'RR35', 'RR36',
     +'RR37', 'RR38' /

C.. Support: Side Bars, Pixel/Chevron Cathode, right, inside
      DATA VPC3SBCRI /'RI31','RI32', 'RI33', 'RI34', 'RI35', 'RI36',
     +'RI37', 'RI38' /

C.. Support: Side Bars, Ground Cathode, left, outside
      DATA VPC3SBGLO /'GL31','GL32', 'GL33', 'GL34', 'GL35', 'GL36',
     +'GL37', 'GL38' /

C.. Support: Side Bars, Ground Cathode, left, inside
      DATA VPC3SBGLI /'GI31','GI32', 'GI33', 'GI34', 'GI35', 'GI36',
     +'GI37', 'GI38' /

C.. Support: Side Bars, Ground Cathode, right, outside
      DATA VPC3SBGRO /'GO31','GO32', 'GO33', 'GO34', 'GO35', 'GO36',
     +'GO37', 'GO38' /

C.. Support: Side Bars, Ground Cathode, right, inside
      DATA VPC3SBGRI /'GR31','GR32', 'GR33', 'GR34', 'GR35', 'GR36',
     +'GR37', 'GR38' /

C.. Support: Side Bars, Gas Gap, left, outside
      DATA VPC3SBWLO /'WL31','WL32', 'WL33', 'WL34', 'WL35', 'WL36',
     +'WL37', 'WL38' /

C.. Support: Side Bars, Gas Gap, left, inside
      DATA VPC3SBWLI /'IW31','IW32', 'IW33', 'IW34', 'IW35', 'IW36',
     +'IW37', 'IW38' /

C.. Support: Side Bars, Gas Gap, right, outside
      DATA VPC3SBWRO /'WO31','WO32', 'WO33', 'WO34', 'WO35', 'WO36',
     +'WO37', 'WO38' /

C.. Support: Side Bars, Gas Gap, right, inside
      DATA VPC3SBWRI /'WR31','WR32', 'WR33', 'WR34', 'WR35', 'WR36',
     +'WR37', 'WR38' /

C.. Support: Center Bars, Pixel/Chevron Cathode, outside
      DATA VPC3CBCTO /'CO31','CO32', 'CO33', 'CO34', 'CO35', 'CO36',
     +'CO37', 'CO38' /

C.. Support: Center Bars, Pixel/Chevron Cathode, inside
      DATA VPC3CBCTI /'CI31','CI32', 'CI33', 'CI34', 'CI35', 'CI36',
     +'CI37', 'CI38' /

C.. Support: Center Bars, Ground Cathode, outside
      DATA VPC3CBGRO /'YO31','YO32', 'YO33', 'YO34', 'YO35', 'YO36',
     +'YO37', 'YO38' /

C.. Support: Center Bars, Ground Cathode, inside
      DATA VPC3CBGRI /'YI31','YI32', 'YI33', 'YI34', 'YI35', 'YI36',
     +'YI37', 'YI38' /
      
CAAR Support: z=0 bars
      DATA VPC3Z0SP /'SPI3','SPA3','ENB3','ST13','ST23','S3I1',
     +'S3I2', '3IPS'/
CAARC.. ----- E N D   O F   V O L U M E   D E C L A R A T I O N ------ ..C

      DATA TETGT / 33.75, 56.25, 78.75, 101.25, 258.75, 281.25, 303.75,
     +326.25 /

      DATA PC2DZGAS /304.95/
      DATA PC3DZGAS /357.38/

c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

      chform = '3I -F'  ! 3 integers, rest floating  point
      call mzform('PARA', CHFORM, iod)  ! book characteristics
      call mzbook(ixdiv_fr, lfp_para, lfp_para, 1, 'PARA', 0, 0,
     +            1 + npd23_para, iod, 0)

c     now store the PC2/PC3 parameters for later use in PISORP

      lfp_pd23 = lfp_para 
      iqf(lfp_pd23 + 1) = -1              ! key for new PC geometry
      iqf(lfp_pd23 + 2) = 19980215        ! date key for PC23 geometry


C.. Read the geometry file segment
      write( *,* ) 'pc23gem - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = pc23_par, err = 999 )

C.. Reset the TETGT array as needed

      if(delphi1.ne.0.0)then
         do l = 1,4
            tetgt(l) = tetgt(l) - delphi1
         enddo  ! first arm correction loop
      endif ! check on first arm correction
      if(delphi2.ne.0.0)then
         do l = 5,8
            tetgt(l) = tetgt(l) - delphi2
         enddo  ! second arm correction loop
      endif ! check on second arm correction

      NMPD23 = PC23NCHMB

      T1 = 90. ! polar angle for axis 1
      T2 = 90. ! polar angle for axis 2
      T3 = 0.  ! polar angle for axis 3
      F3 = 0.  ! azimuthal angle for axis 3

      DO 781 JRT = 1, NMPD23
         F1 = TETGT ( JRT) ! azimuthal angle for axis 1
         F2 = F1 + 90.     ! azimuthal angle for axis 2

C.. "Flexible" ( floating) rotation index definition  (CMF)

         irot = irot + 1
         irttr( jrt) = irot
         CALL GSROTM ( IRTTR(JRT), T1, F1, T2, F2, T3, F3 )
  781 CONTINUE

C.. ------------------- DEFINE USER MATERIALS ----------------------

C.. Carbon-fiber/Epoxy : 65% of carbon-fiber and 35% of epoxy
*   Take G10 PLATE for now 

      IMATE = 690
      A = 18.14
      Z = 9.065
      DENS = 1.7
      RADL = 19.4
      ABSL = 56.7
      NWBUF = 1
      CALL GSMATE(IMATE,'G10$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

C.. S2-glass/Epoxy - fiberglass material with properties close to G10
*   Take G10 PLATE for now
 
      IMATE = 691
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

      IMATE = 692
      A = 12.01 
      Z = 6.
      DENS = 0.024
      RADL = 8170.
      ABSL = 99999.
      NWBUF = 1
      CALL GSMATE(IMATE,'HEXCELL$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

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

      IMATE = 694     ! for PC2 key in the GUSTEP, etc
      DENS = 0.0010394
      CALL GSMIXT(IMATE,'PAD CHAMBER GAS$',AGAS,ZGAS,DENS,3,WGAS)


C.. Pixel/Chevron cathode board - Carbon-fiber/Epoxy + 1/4 oz/ft*2 of copper.
*   The density of copper is 8.96 g/cm*3, then the thickness of the 
*   copper layer is 8.4 microns. G10 density is 1.7 g/cm*3 and its 
*   thickness is 0.0254 cm (0.010"). Thus the density of composite material 
*   is 1.93 g/cm*3

   
*   Atomic weights of G10 and copper:
      ACATHODE(1) = 18.14
      ACATHODE(2) = 63.54

*   Mass numbers of G10 and copper:
      ZCATHODE(1) = 9.065
      ZCATHODE(2) = 29.

*   Proportions: 0.968 G10 + 0.032 Cu
      WCATHODE(1)=0.984
      WCATHODE(2)=0.032

      IMATE = 695
      DENS = 1.93
      CALL GSMIXT(IMATE,'CATHODE BOARD$',
     +            ACATHODE,ZCATHODE,DENS,2,WCATHODE)

C.. Ground cathode board - S2-glass/Epoxy + 1/4 oz/ft*2 of copper.
*   The board thickness is 0.02" - same proportions as for PC1

*   Atomic weights of G10 and copper:
      AGROUND(1) = 18.14
      AGROUND(2) = 63.54

*   Mass numbers of G10 and copper:
      ZGROUND(1) = 9.065
      ZGROUND(2) = 29.

*   Proportions: 0.984 G10 + 0.016 Cu
      WGROUND(1)=0.984
      WGROUND(2)=0.016

      IMATE = 696
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

      IMATE = 697
      DENS = 0.093
      CALL GSMIXT(IMATE,'MOTHER BOARD$',AELECTR,ZELECTR,DENS,3,WELECTR)


C.. ----------------- DEFINE USER TRACKING MEDIA -------------------

      IFIELD = 1     ! magnetic field; tracking performed with GRKUTA;
      FIELDM = 20.0  ! max field value (in Kilogauss);
      TMAXFD = 1.0   ! maximum angle due to field in one step (in degrees);
      DMAXMS = 0.5   ! max disp. due to mult. scatt. in one step (in cm);
      DEEMAX = 0.2   ! max fractional energy loss in one step;
      EPSIL = 0.01   ! tracking precision (in cm);
      STMIN = 0.01   ! min step due to energy loss or mult. scatt. (in cm);
 
      CALL GSTMED(690,'CARBON-FIBER/EPOXY   $', 690,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(691,'S2-GLASS/EPOXY       $', 691,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(692,'HEXCELL CORE         $', 692,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(694,'PC2 CHAMBER GAS      $', 694,  1,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(695,'PAD CATHODE BOARD    $', 695,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(696,'GROUND CATHODE BOARD $', 696,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(697,'MOTHER BOARD         $', 697,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(699,'PC3 CHAMBER GAS      $', 694,  1,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)

C.. -------------------- DEFINE USER VOLUMES -----------------------

C.. Introducing some local variables:

      TANG  = TAN( PC23TETGT*DEGRAD/2. )  ! tangent of 11.25 degrees
      SINE = SIN( PC23TETGT*DEGRAD/2. )   ! sine of 11.25 degrees
      COSINE = COS( PC23TETGT*DEGRAD/2. ) ! cosine of 11.25 degrees

      DHPC2 = 2.*PC2GRSHT+PC2GRHEX+PC2GASGP+2.*PC2CTSHT+
     +        PC2CTHEX+PC2ELECT  ! PC2 Sandwich thickness
c     +        +.4                 ! Added support struct
      DHPC3 = 2.*PC3GRSHT+PC3GRHEX+PC3GASGP+2.*PC3CTSHT+
     +        PC3CTHEX+PC3ELECT  ! PC3 Sandwich thickness
c     +        +.4                 ! Added support struct
    

*.. - - - -  P C 2   M O T H E R    V O L U M E  ( S H E L L )  - - - - ..*  

C.. Create the local "shell" volume for each sector:  all other volumes are 
*   contained within their local mother shell PC2(1-8).

      DZ = PC2DZEXT/2. 
      THET = 0.
      PHI = 0.
      H1 = DHPC2/2.
      BL1 = PC2RINST*TANG
      TL1  = (PC2RINST+2*H1)*TANG
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

      DO L = 1, PC23NCHMB
* Medium for "shell" volumes is air...
         CALL GSVOLU (VPC2SHELL(L),'TRAP',16, Par, 11, IST)
         CALL GSATT (VPC2SHELL(L),'SEEN',1 )
         CALL GSATT (VPC2SHELL(L),'COLO',COL_SECTOR )
      END DO


*.. - - - -  P C 3   M O T H E R    V O L U M E  ( S H E L L )  - - - - ..*  

C.. Create the local "shell" volume for each sector:  all other volumes are 
*   contained within their local mother shell PC3(1-8).

      DZ = PC3DZEXT/2. 
      THET = 0.
      PHI = 0.
      H1 = DHPC3/2.
      BL1 = PC3RINST*TANG
      TL1  = (PC3RINST+2*H1)*TANG
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

      DO L = 1, PC23NCHMB
* Medium for "shell" volumes is air...
         CALL GSVOLU (VPC3SHELL(L),'TRAP',16, Par, 11, IST)
         CALL GSATT (VPC3SHELL(L),'SEEN',1 )
         CALL GSATT (VPC3SHELL(L),'COLO',COL_SECTOR )
      END DO


*.. - - - - - - - - -  P C 2   S A N D W I C H  - - - - - - - - - ..* 

C.. Create the sandwich of the detector, layer by layer:
*   Starting from the inside (inscribe inner radius of PC's envelope)
*   to outside :

caar Temporary Area
      if(ibeamtf) then
      else
         ibar2dz=0.
         ibar2dy=0.
         ibar1dz=0.
         ibar1dy=0.
      endif


      gap= (tbar2dz+ibar2dz+.1)*4.-.2
      gap2=gap+(sbardz+enbdz)*2.


C.. 1) Electronics ~1.0 cm thick - mother board, cables, connectors and chips

      DX = PC2RINST*TANG
      DY = PC2ELECT/2.
caar      DZ = PC2DZEXT/2.
      DZ = (PC2DZEXT-gap-ibar1dz)/4.
      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2ELECT(L),'BOX ',MED_MOTHER_BOARD, Par, 3, IST)
         CALL GSATT (VPC2ELECT(L),'SEEN',1 )
         CALL GSATT (VPC2ELECT(L),'COLO',COL_ELECTR )
      END DO
           
C.. 2) Chevron/Pixel cathode panel: S2-glass-epoxy sheet
*                                   0.010" thick

      DX = (PC2RINST+PC2ELECT)*TANG
      DY = PC2CTSHT/2.
caar
      DZ = (PC2DZEXT-gap)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2CTSH1(L),'BOX ',MED_S2_GLASS_EPOXY, Par, 3, IST)
       CALL GSATT (VPC2CTSH1(L),'SEEN',1 )
       CALL GSATT (VPC2CTSH1(L),'COLO',COL_FIBER )
      END DO
      
C.. 3) Chevron/Pixel cathode panel: 

C.. 3a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC2CTHEX/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBCLO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC2SBCLO(L),'SEEN',1 )
       CALL GSATT (VPC2SBCLO(L),'COLO',COL_FIBER )
      END DO

C.. 3b) Left side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC2CTHEX-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBCLI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC2SBCLI(L),'SEEN',1 )
       CALL GSATT (VPC2SBCLI(L),'COLO',COL_PD_GAS )
      END DO

      
C.. 3c) HEXCELL sheet 1.25" thick (left side)

cAAR
c      DZ = PC2DZEXT/2. 
cAAR
      DZ= PC2DZEXT/4.-gap2/4.
      THET = 0.
      PHI = 0.
      H1 = PC2CTHEX/2.
      BL1 = ((PC2RINST+PC2ELECT+PC2CTSHT)*TANG-PC23SBAR/COSINE-
     >       PC23CBAR/2.)/2.
      TL1  = BL1+PC2CTHEX*TANG/2.
      ALP1 = -PC23TETGT/4.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = -PC23TETGT/4.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2CTHXL(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (VPC2CTHXL(L),'SEEN',1 )
         CALL GSATT (VPC2CTHXL(L),'COLO',COL_HEXCELL )
      END DO
           
C.. 3d) Central bar (the walls of carbon-fiber square tubing)


      DX = PC23CBAR/2.
      DY = PC2CTHEX/2.
cAAR
c      DZ = PC2DZEXT/2.
cAAR
      DZ = (PC2DZEXT-gap2)/4.
      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2CBCTO(L),'BOX ',MED_CARBON_EPOXY, Par, 3, IST)
       CALL GSATT (VPC2CBCTO(L),'SEEN',1 )
       CALL GSATT (VPC2CBCTO(L),'COLO',COL_FIBER )
      END DO

C.. 3e) Central bar (the inside (air) of carbon-fiber square tubing)      

      DX = (PC23CBAR-2.*PC23CBTH)/2.
      DY = (PC2CTHEX-2.*PC23CBTH)/2.
cAAR
c      DZ = PC2DZEXT/2.
cAAR
      DZ = (PC2DZEXT-gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2CBCTI(L),'BOX ', 16, Par, 3, IST)
       CALL GSATT (VPC2CBCTI(L),'SEEN',1 )
       CALL GSATT (VPC2CBCTI(L),'COLO',COL_PD_GAS )
      END DO

C.. 3f) HEXCELL sheet 1.25" thick (right side) 

cAAR
c      DZ = PC2DZEXT/2. 
cAAR
      DZ = (PC2DZEXT-gap2)/4.


      THET = 0.
      PHI = 0.
      H1 = PC2CTHEX/2.
      BL1 = ((PC2RINST+PC2ELECT+PC2CTSHT)*TANG-PC23SBAR/COSINE-
     >       PC23CBAR/2.)/2.
      TL1  = BL1+PC2CTHEX*TANG/2.
      ALP1 = PC23TETGT/4.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = PC23TETGT/4.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2CTHXR(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (VPC2CTHXR(L),'SEEN',1 )
         CALL GSATT (VPC2CTHXR(L),'COLO',COL_HEXCELL )
      END DO
          
C.. 3g) Right side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC2CTHEX/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBCRO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC2SBCRO(L),'SEEN',1 )
       CALL GSATT (VPC2SBCRO(L),'COLO',COL_FIBER )
      END DO

C.. 3h) Right side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC2CTHEX-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBCRI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC2SBCRI(L),'SEEN',1 )
       CALL GSATT (VPC2SBCRI(L),'COLO',COL_PD_GAS )
      END DO

C.. 4) Chevron/Pixel cathode panel: S2-glass-epoxy sheet + copper board
*                                   0.010" thick

      DX = (PC2RINST+PC2ELECT+PC2CTSHT+PC2CTHEX)*TANG
      DY = PC2CTSHT/2.
caar      DZ = PC2DZEXT/2.
      DZ=(PC2DZEXT-gap)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2CTSH2(L),'BOX ',MED_PC_BOARD, Par, 3, IST)
         CALL GSATT (VPC2CTSH2(L),'SEEN',1 )
         CALL GSATT (VPC2CTSH2(L),'COLO',COL_FIBER )
      END DO
      
C.. 5) Gas Gap2 plane:

C.. 5a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC2GASGP/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBWLO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC2SBWLO(L),'SEEN',1 )
       CALL GSATT (VPC2SBWLO(L),'COLO',COL_FIBER )
      END DO

C.. 5b) Left side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC2GASGP-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBWLI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC2SBWLI(L),'SEEN',1 )
       CALL GSATT (VPC2SBWLI(L),'COLO',COL_PD_GAS )
      END DO

      
C.. 5c) Active volume (Gas 50% Ethane 50% Argon and wires)

caar
c      DZ = PC2DZGAS/2.
caar
      dz=(PC2DZGAS-gap2)/4.0

      THET = 0.
      PHI = 0.
      H1 = PC2GASGP/2.
      BL1 = (PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX)*TANG-
     >       PC23SBAR/COSINE
      TL1  = BL1+PC2GASGP*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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


c     CFM: revision for 699 medium number for PC3

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2GASGP(L),'TRAP',MED_PC_GAS,
     +                Par, 11, IST)
         CALL GSATT (VPC2GASGP(L),'SEEN',1 )
         CALL GSATT (VPC2GASGP(L),'COLO',COL_PD_GAS )
      END DO
           
C.. 5d) Right side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC2GASGP/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBWRO(L), 'PARA', MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC2SBWRO(L),'SEEN',1 )
       CALL GSATT (VPC2SBWRO(L),'COLO',COL_FIBER )
      END DO

C.. 5e) Right side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC2GASGP-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBWRI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC2SBWRI(L),'SEEN',1 )
       CALL GSATT (VPC2SBWRI(L),'COLO',COL_PD_GAS )
      END DO

C.. 6) Ground cathode panel: Carbon-Epoxy + Conductor 0.02" thick

      DX = (PC2RINST+PC2ELECT+2*PC2CTSHT+PC2CTHEX+PC2GASGP)*TANG
      DY = PC2GRSHT/2.
caar      DZ = PC2DZEXT/2.
      DZ= (PC2DZEXT-gap)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2GRSH1(L),'BOX ',MED_GROUND_CATHODE,PAR,3,IST)
         CALL GSATT (VPC2GRSH1(L),'SEEN',1 )
         CALL GSATT (VPC2GRSH1(L),'COLO',COL_FIBER )
      END DO


C.. 7) Ground cathode panel: 

C.. 7a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC2GRHEX/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBGLO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC2SBGLO(L),'SEEN',1 )
       CALL GSATT (VPC2SBGLO(L),'COLO',COL_FIBER )
      END DO

C.. 7b) Left side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC2GRHEX-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBGLI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC2SBGLI(L),'SEEN',1 )
       CALL GSATT (VPC2SBGLI(L),'COLO',COL_PD_GAS )
      END DO

      
C.. 7c) HEXCELL sheet 1.25" thick (left side)
caar
c      DZ = PC2DZEXT/2. 
caar
      dz=(PC2DZEXT-gap2)/4.
      THET = 0.
      PHI = 0.
      H1 = PC2GRHEX/2.
      BL1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)/2.
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1 = -PC23TETGT/4.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = -PC23TETGT/4.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2GRHXL(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (VPC2GRHXL(L),'SEEN',1 )
         CALL GSATT (VPC2GRHXL(L),'COLO',COL_HEXCELL )
      END DO
           
C.. 7d) Central bar (the walls of carbon-fiber square tubing)


      DX = PC23CBAR/2.
      DY = PC2GRHEX/2.
caar
c      DZ = PC2DZEXT/2.
caar
      dz=(PC2DZEXT - gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2CBGRO(L),'BOX ',MED_CARBON_EPOXY, Par, 3, IST)
       CALL GSATT (VPC2CBGRO(L),'SEEN',1 )
       CALL GSATT (VPC2CBGRO(L),'COLO',COL_FIBER )
      END DO

C.. 7e) Central bar (the inside (air) of carbon-fiber square tubing)      

      DX = (PC23CBAR-2.*PC23CBTH)/2.
      DY = (PC2GRHEX-2.*PC23CBTH)/2.

caar
c      DZ = PC2DZEXT/2.
caar
      dz=(PC2DZEXT -gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2CBGRI(L),'BOX ', 16, Par, 3, IST)
       CALL GSATT (VPC2CBGRI(L),'SEEN',1 )
       CALL GSATT (VPC2CBGRI(L),'COLO',COL_PD_GAS )
      END DO

C.. 7f) HEXCELL sheet 1.25" thick (right side) 

caar
c      DZ = PC2DZEXT/2.
caar
      dz=(PC2DZEXT -gap2)/4.
      THET = 0.
      PHI = 0.
      H1 = PC2GRHEX/2.
      BL1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)/2.
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1 = PC23TETGT/4.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = PC23TETGT/4.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2GRHXR(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (VPC2GRHXR(L),'SEEN',1 )
         CALL GSATT (VPC2GRHXR(L),'COLO',COL_HEXCELL )
      END DO
          
C.. 7g) Right side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC2GRHEX/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBGRO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC2SBGRO(L),'SEEN',1 )
       CALL GSATT (VPC2SBGRO(L),'COLO',COL_FIBER )
      END DO

C.. 7h) Right side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC2GRHEX-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC2DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC2SBGRI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC2SBGRI(L),'SEEN',1 )
       CALL GSATT (VPC2SBGRI(L),'COLO',COL_PD_GAS )
      END DO

C.. 8) Ground cathode panel: carbon-epoxy sheet 
*                                  0.020" thick

      DX = (PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT+PC2GRHEX)*TANG
      DY = PC2GRSHT/2.
caar      DZ = PC2DZEXT/2.
      DZ=(PC2DZEXT-gap)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC2GRSH2(L),'BOX ',MED_CARBON_EPOXY, Par, 3, IST)
         CALL GSATT (VPC2GRSH2(L),'SEEN',1 )
         CALL GSATT (VPC2GRSH2(L),'COLO',COL_FIBER )
      END DO



CAAR  9...Central (Z=0) Support
c     5 base pieces, 7 pieces w/ I beam.
C     9a) Hollow Pipes....general trapazoid, with square cross section.
c         parameters come from 

      dz   =sbardz/2.
      THET = 0.
      PHI  = 0.
      H1   = sbardy/2.
      BL1  = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1 = 0.
      H2   = H1
      BL2  = BL1
      TL2  = TL1
      ALP2 = 0.

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

      call gsvolu (VPC2Z0SP(1),'TRAP',med_s2, par, 11 , IST)
      call gsatt (VPC2Z0SP(1),'SEEN',1)
      call gsatt (VPC2Z0SP(1),'COLO',col_hexcell)

      dz   = sbardz/2.
      THET = 0.
      PHI  = 0.
      H1   = sbardy/2.
      BL1  = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1 = 0.
      H2   = H1
      BL2  = BL1
      TL2  = TL1
      ALP2 = 0.

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

      call gsvolu (VPC2Z0SP(8),'TRAP',med_s2, par, 11 , IST)
      call gsatt (VPC2Z0SP(8),'SEEN',1)
      call gsatt (VPC2Z0SP(8),'COLO',col_hexcell)


c     9b) Hollow part of pipes

      dz=sbardz/2-hollow/2
      THET = 0.
      PHI = 0.
      H1  = sbardy/2-hollow
      BL1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE -PC23CBAR/2.)
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC2Z0SP(2),'TRAP',16, par, 11 , IST)
      call gsatt(VPC2Z0SP(2),'SEEN',1)      
      call gsatt(VPC2Z0SP(2),'COLO',col_hexcell)
      

c     9c) End Bar

      dz=enbdz/2
      THET = 0.
      PHI = 0.
      H1  = sbardy/2
      BL1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE -PC23CBAR/2.)
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC2Z0SP(3),'TRAP',med_s2, par, 11 , IST)
      call gsatt(VPC2Z0SP(3),'SEEN',1)      
      call gsatt(VPC2Z0SP(3),'COLO',col_hexcell)      


c     9d) T-bar :  central beam, always present..split by I beam
c                   tall bar
      dz=tbar2dz
      THET = 0.
      PHI = 0.
      H1  = tbar2dy
      BL1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE -PC23CBAR/2.)
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC2Z0SP(5),'TRAP',med_s2, par, 11 , IST)
      call gsatt(VPC2Z0SP(5),'SEEN',1)      
      call gsatt(VPC2Z0SP(5),'COLO',col_hexcell)      

c                 cross bar
      dz=tbar1dz
      THET = 0.
      PHI = 0.
      H1  = Tbar1dy
      BL1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE -PC23CBAR/2.)
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC2Z0SP(4),'TRAP',med_s2, par, 11 , IST)
      call gsatt(VPC2Z0SP(4),'SEEN',1)      
      call gsatt(VPC2Z0SP(4),'COLO',col_hexcell)      


c     9e)  Central I beam
c           'cross' pieces
      dz=ibar1dz
      THET = 0.
      PHI = 0.
      H1  = ibar1dy
      BL1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE -PC23CBAR/2.)
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC2Z0SP(6),'TRAP',med_alu, par, 11 , IST)
      call gsatt(VPC2Z0SP(6),'SEEN',1)
      call gsatt(VPC2Z0SP(6),'COLO',col_hexcell)

c     central dividing beam
      dz=ibar2dz
      THET = 0.
      PHI = 0.
      H1  = ibar2dy
      BL1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >        PC2GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)
      TL1  = BL1+PC2GRHEX*TANG/2.
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC2Z0SP(7),'TRAP',med_alu, par, 11 , IST)
      call gsatt(VPC2Z0SP(7),'SEEN',1)      
      call gsatt(VPC2Z0SP(7),'COLO',col_hexcell)      

*.. - - - - - - - - -  P C 3   S A N D W I C H  - - - - - - - - - ..* 

C.. Create the sandwich of the detector, layer by layer:
*   Starting from the inside (inscribe inner radius of PC's envelope)
*   to outside. 

C.. Ground cathode panel: 

C.. 1) Ground cathode panel: carbon-epoxy sheet 
*                                  0.030" thick

      DX = PC3RINST*TANG
      DY = PC3GRSHT/2.
caar      DZ = PC3DZEXT/2.
      DZ = (PC3DZEXT-gap)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3GRSH1(L),'BOX ',MED_CARBON_EPOXY,Par,3,IST)
         CALL GSATT (VPC3GRSH1(L),'SEEN',1 )
         CALL GSATT (VPC3GRSH1(L),'COLO',4 )
      END DO

C.. 2a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC3GRHEX/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBGLO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC3SBGLO(L),'SEEN',1 )
       CALL GSATT (VPC3SBGLO(L),'COLO',COL_FIBER )
      END DO

C.. 2b) Left side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC3GRHEX-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBGLI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC3SBGLI(L),'SEEN',1 )
       CALL GSATT (VPC3SBGLI(L),'COLO',COL_PD_GAS )
      END DO

C.. 2c) HEXCELL sheet 1.5" thick (left side)

caar      DZ = PC3DZEXT/2. 
      DZ = (PC3DZEXT-gap2)/4. 
      THET = 0.
      PHI = 0.
      H1 = PC3GRHEX/2.
      BL1 = ((PC3RINST+PC3GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)/2.
      TL1  = BL1+PC3GRHEX*TANG/2.
      ALP1 = -PC23TETGT/4.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = -PC23TETGT/4.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3GRHXL(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (VPC3GRHXL(L),'SEEN',1 )
         CALL GSATT (VPC3GRHXL(L),'COLO',COL_HEXCELL )
      END DO
           
C.. 2d) Central bar (the walls of carbon-fiber square tubing)


      DX = PC23CBAR/2.
      DY = PC3GRHEX/2.
caar      DZ = PC3DZEXT/2.
      DZ = (PC3DZEXT-gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3CBGRO(L),'BOX ',MED_CARBON_EPOXY, Par, 3, IST)
       CALL GSATT (VPC3CBGRO(L),'SEEN',1 )
       CALL GSATT (VPC3CBGRO(L),'COLO',COL_FIBER )
      END DO

C.. 2e) Central bar (the inside (air) of carbon-fiber square tubing)      

      DX = (PC23CBAR-2.*PC23CBTH)/2.
      DY = (PC3GRHEX-2.*PC23CBTH)/2.
caar      DZ = PC3DZEXT/2.
      DZ = (PC3DZEXT-gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3CBGRI(L),'BOX ', 16, Par, 3, IST)
       CALL GSATT (VPC3CBGRI(L),'SEEN',1 )
       CALL GSATT (VPC3CBGRI(L),'COLO',COL_PD_GAS )
      END DO

C.. 2f) HEXCELL sheet 1.25" thick (right side) 

caar      DZ = PC3DZEXT/2. 
      DZ = (PC3DZEXT-gap2)/4. 
      THET = 0.
      PHI = 0.
      H1 = PC3GRHEX/2.
      BL1 = ((PC3RINST+PC3GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)/2.
      TL1  = BL1+PC3GRHEX*TANG/2.
      ALP1 = PC23TETGT/4.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = PC23TETGT/4.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3GRHXR(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (VPC3GRHXR(L),'SEEN',1 )
         CALL GSATT (VPC3GRHXR(L),'COLO',COL_HEXCELL )
      END DO
          
C.. 2g) Right side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC3GRHEX/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBGRO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC3SBGRO(L),'SEEN',1 )
       CALL GSATT (VPC3SBGRO(L),'COLO',COL_FIBER )
      END DO

C.. 2h) Right side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC3GRHEX-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBGRI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC3SBGRI(L),'SEEN',1 )
       CALL GSATT (VPC3SBGRI(L),'COLO',COL_PD_GAS )
      END DO

C.. 3) Ground cathode panel: Carbon-Epoxy + Conductor 0.03" thick

      DX = (PC3RINST+PC3GRSHT+PC3GRHEX)*TANG
      DY = PC3GRSHT/2.
caar      DZ = PC3DZEXT/2.
      DZ = (PC3DZEXT-gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3GRSH2(L),'BOX ',MED_GROUND_CATHODE,PAR,3,IST)
         CALL GSATT (VPC3GRSH2(L),'SEEN',1 )
         CALL GSATT (VPC3GRSH2(L),'COLO',COL_FIBER )
      END DO

C.. 4) Gas Gap plane:

C.. 4a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC3GASGP/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBWLO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC3SBWLO(L),'SEEN',1 )
       CALL GSATT (VPC3SBWLO(L),'COLO',COL_FIBER )
      END DO

C.. 4b) Left side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC3GASGP-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBWLI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC3SBWLI(L),'SEEN',1 )
       CALL GSATT (VPC3SBWLI(L),'COLO',COL_PD_GAS )
      END DO

      
C.. 4c) Active volume (Gas 50% Ethane 50% Argon and wires)

caar      DZ = PC3DZGAS/2. 
      DZ = (PC3DZGAS-gap2)/4.0
      THET = 0.
      PHI = 0.
      H1 = PC3GASGP/2.
      BL1 = (PC3RINST+2.*PC2GRSHT+PC3GRHEX)*TANG-
     >       PC23SBAR/COSINE
      TL1  = BL1+PC3GASGP*TANG
      ALP1 = 0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3GASGP(L),'TRAP',MED_PC_GAS + 5,
     +                Par, 11, IST)
         CALL GSATT (VPC3GASGP(L),'SEEN',1 )
         CALL GSATT (VPC3GASGP(L),'COLO',COL_PD_GAS )
      END DO
           
C.. 4d) Right side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC3GASGP/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBWRO(L), 'PARA', MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC3SBWRO(L),'SEEN',1 )
       CALL GSATT (VPC3SBWRO(L),'COLO',COL_FIBER )
      END DO

C.. 4e) Right side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC3GASGP-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBWRI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC3SBWRI(L),'SEEN',1 )
       CALL GSATT (VPC3SBWRI(L),'COLO',COL_PD_GAS )
      END DO

C.. 5) Chevron/Pixel cathode panel: S2-glass-epoxy sheet+ copper
*                                   0.020" thick

      DX = (PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP)*TANG
      DY = PC3CTSHT/2.
      DZ = (PC3DZEXT-gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3CTSH1(L),'BOX ',MED_PC_BOARD, Par, 3, IST)
       CALL GSATT (VPC3CTSH1(L),'SEEN',1 )
       CALL GSATT (VPC3CTSH1(L),'COLO',COL_FIBER )
      END DO
      
C.. 6) Chevron/Pixel cathode panel: 

C.. 6a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC3CTHEX/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBCLO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC3SBCLO(L),'SEEN',1 )
       CALL GSATT (VPC3SBCLO(L),'COLO',COL_FIBER )
      END DO

C.. 6b) Left side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC3CTHEX-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = -PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBCLI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC3SBCLI(L),'SEEN',1 )
       CALL GSATT (VPC3SBCLI(L),'COLO',COL_PD_GAS )
      END DO

C.. 6c) HEXCELL sheet 1.5" thick (left side)

caar      DZ = PC3DZEXT/2. 
          DZ = (PC3DZEXT-gap2)/4. 
      THET = 0.
      PHI = 0.
      H1 = PC3CTHEX/2.
      BL1 = ((PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >       PC3CTSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)/2.
      TL1  = BL1+PC3CTHEX*TANG/2.
      ALP1 = -PC23TETGT/4.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = -PC23TETGT/4.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3CTHXL(L),'TRAP',MED_HEXCELL,Par,11,IST)
         CALL GSATT (VPC3CTHXL(L),'SEEN',1 )
         CALL GSATT (VPC3CTHXL(L),'COLO',COL_HEXCELL )
      END DO
           
C.. 6d) Central bar (the walls of carbon-fiber square tubing)

      DX = PC23CBAR/2.
      DY = PC3CTHEX/2.
caar      DZ = PC3DZEXT/2.
      DZ = (PC3DZEXT-gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3CBCTO(L),'BOX ',MED_CARBON_EPOXY, Par, 3, IST)
       CALL GSATT (VPC3CBCTO(L),'SEEN',1 )
       CALL GSATT (VPC3CBCTO(L),'COLO',COL_FIBER )
      END DO

C.. 6e) Central bar (the inside (air) of carbon-fiber square tubing)      

      DX = (PC23CBAR-2.*PC23CBTH)/2.
      DY = (PC3CTHEX-2.*PC23CBTH)/2.
caar      DZ = PC3DZEXT/2.
      DZ = (PC3DZEXT-gap2)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3CBCTI(L),'BOX ', 16, Par, 3, IST)
       CALL GSATT (VPC3CBCTI(L),'SEEN',1 )
       CALL GSATT (VPC3CBCTI(L),'COLO',COL_PD_GAS )
      END DO

C.. 6f) HEXCELL sheet 1.5" thick (right side) 

      DZ = (PC3DZEXT-gap2)/4. 
caar      DZ = PC3DZEXT/2. 
      THET = 0.
      PHI = 0.
      H1 = PC3CTHEX/2.
      BL1 = ((PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >       PC3CTSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)/2.
      TL1  = BL1+PC3CTHEX*TANG/2.
      ALP1 = PC23TETGT/4.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = PC23TETGT/4.

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

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3CTHXR(L),'TRAP',MED_HEXCELL, Par, 11, IST)
         CALL GSATT (VPC3CTHXR(L),'SEEN',1 )
         CALL GSATT (VPC3CTHXR(L),'COLO',COL_HEXCELL )
      END DO
          
C.. 6g) Right side bar (The walls of carbon-fiber parallelepiped tubing)

      DX = PC23SBAR/(2.*COSINE)
      DY = PC3CTHEX/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBCRO(L),'PARA',MED_CARBON_EPOXY, Par, 6, IST)
       CALL GSATT (VPC3SBCRO(L),'SEEN',1 )
       CALL GSATT (VPC3SBCRO(L),'COLO',COL_FIBER )
      END DO

C.. 6h) Right side bar (air, the inside of the tubing)

      DX = (PC23SBAR-2.*PC23SBTH)/(2.*COSINE)
      DY = (PC3CTHEX-2.*PC23SBTH)/(2.*COSINE)
      DZ = PC3DZEXT/2.
      ALPH = PC23TETGT/2. 
      THET = 0.
      PHI = 0.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ
      Par( 4) = ALPH
      Par( 5) = THET
      Par( 6) = PHI

      DO L = 1, PC23NCHMB
       CALL GSVOLU (VPC3SBCRI(L),'PARA', 16, Par, 6, IST)
       CALL GSATT (VPC3SBCRI(L),'SEEN',1 )
       CALL GSATT (VPC3SBCRI(L),'COLO',COL_PD_GAS )
      END DO

C.. 7) Chevron/Pixel cathode panel: S2-glass-epoxy sheet 
*                                   0.020" thick

      DX = (PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+PC3CTSHT+
     >      PC3CTHEX)*TANG
      DY = PC3CTSHT/2.
caar      DZ = PC3DZEXT/2.
      DZ = (PC3DZEXT-gap)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3CTSH2(L),'BOX ',MED_S2_GLASS_EPOXY,Par,3,IST)
         CALL GSATT (VPC3CTSH2(L),'SEEN',1 )
         CALL GSATT (VPC3CTSH2(L),'COLO',COL_FIBER )
      END DO
      
C.. 8) Electronics ~1.0 cm thick - mother board, cables, connectors and chips

      DX = (PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+2.*PC3CTSHT+
     >      PC3CTHEX)*TANG
      DY = PC3ELECT/2.
caar      DZ = PC3DZEXT/2.
      DZ = (PC3DZEXT-gap)/4.

      Par( 1) = DX
      Par( 2) = DY
      Par( 3) = DZ

      DO L = 1, PC23NCHMB
         CALL GSVOLU (VPC3ELECT(L),'BOX ',MED_MOTHER_BOARD, Par, 3, IST)
         CALL GSATT (VPC3ELECT(L),'SEEN',1 )
         CALL GSATT (VPC3ELECT(L),'COLO',COL_ELECTR )
      END DO

cAAR  9...Central (Z=0) Support
c     5 base pieces, 7 pieces w/ I beam.
C     9a) Hollow Pipes....general trapazoid, with square cross section.
c         parameters come from 

      dz   =sbardz/2.
      THET = 0.
      PHI  = 0.
      H1   = sbardy/2.+.38
c      BL1  = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
c     >        PC2GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)
c      TL1  = BL1+PC2GRHEX*TANG/2.
      BL1  = ((PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >       PC3CTSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)
      TL1  = BL1+PC3CTHEX*TANG/2.
      ALP1 = 0.
      H2   = H1
      BL2  = BL1
      TL2  = TL1
      ALP2 = 0.

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

      call gsvolu (VPC3Z0SP(1),'TRAP',med_s2, par, 11 , IST)
      call gsatt (VPC3Z0SP(1),'SEEN',1)

      dz   =sbardz/2.
      THET = 0.
      PHI  = 0.
      H1   = sbardy/2.+.38
c      BL1  = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
c     >        PC2GRSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)
c      TL1  = BL1+PC2GRHEX*TANG/2.
      BL1  = ((PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >       PC3CTSHT)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)
      TL1  = BL1+PC3CTHEX*TANG/2.
      ALP1 = 0.
      H2   = H1
      BL2  = BL1
      TL2  = TL1
      ALP2 = 0.

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

      call gsvolu (VPC3Z0SP(8),'TRAP',med_s2, par, 11 , IST)
      call gsatt (VPC3Z0SP(8),'SEEN',1)

c     9b) Hollow part of pipes

      dz=sbardz/2-hollow/2.
      THET = 0.
      PHI = 0.
      H1  = sbardy/2-hollow+.38
      BL1 = BL1                !use pre BL1 and TL1
      TL1  = TL1
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC3Z0SP(2),'TRAP',16, par, 11 , IST)
      call gsatt(VPC3Z0SP(2),'SEEN',1)      


c     9c) End Bar

      dz=enbdz/2
      THET = 0.
      PHI = 0.
      H1  = sbardy/2+.38
      BL1 =  BL1                !use pre BL1 and TL1
      TL1  = TL1
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC3Z0SP(3),'TRAP',med_s2, par, 11 , IST)
      call gsatt(VPC3Z0SP(3),'SEEN',1)      


c     9d) T-bar :  central beam, always present..split by I beam
c                   tall bar
      dz=tbar2dz
      THET = 0.
      PHI = 0.
      H1  = tbar2dy+.70
      BL1 = BL1
      TL1  = TL1
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC3Z0SP(5),'TRAP',med_s2, par, 11 , IST)
      call gsatt(VPC3Z0SP(5),'SEEN',1)  

c                 cross bar
      dz=tbar1dz
      THET = 0.
      PHI = 0.
      H1  = Tbar1dy+.10
      BL1 = BL1
      TL1  = TL1
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC3Z0SP(4),'TRAP',med_s2, par, 11 , IST)
      call gsatt(VPC3Z0SP(4),'SEEN',1) 

c     9e)  Central I beam
c           'cross' pieces

      dz=ibar1dz
      THET = 0.
      PHI = 0.
      H1  = ibar1dy
      BL1 = BL1
      TL1  = TL1
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC3Z0SP(6),'TRAP',med_alu, par, 11 , IST)
      call gsatt(VPC3Z0SP(6),'SEEN',1)

c     central dividing beam
 
      dz=ibar2dz 
      THET = 0.
      PHI = 0.
      H1  = ibar2dy+.85
      BL1 = BL1
      TL1  = TL1
      ALP1=0.
      H2 = H1
      BL2 = BL1
      TL2 = TL1
      ALP2 = 0.

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

      call gsvolu(VPC3Z0SP(7),'TRAP',med_alu, par, 11 , IST)
      call gsatt(VPC3Z0SP(7),'SEEN',1)      


C.. --------  P O S I T I O N   T H E   V O L U M E S    --------..C

********************************************************************
*                                                                  * 
*      WE FINISHED WITH VOLUMES AND COME TO SPACE AND POSITION     * 
*                 PROBLEM                                          *
*                                                                  *
********************************************************************


C.. Start positioning all daughter volumes, sector by sector, layer by layer

      DO 715 J = 1, PC23NCHMB
        JPT = 1
caar
        LPT = 2
        FICNTR = PIBY2  + TETGT( J)   * DEGRAD

        SNFI = SIN ( FICNTR)
        CSFI = COS ( FICNTR)

C.. ----------- P O S I T I O N    P C 2   D E T E C T O R ------------ ..C

        RPSPD1 = PC2RINST+DHPC2/2. ! inner radius + half of the thickness
        XCNTP1 =   RPSPD1            * CSFI
        YCNTP1 =   RPSPD1            * SNFI

C.. Positioning PC2 "shell" volumes 

        zcntr = 0.0                   ! CFM addition, since ZCNTR is redefined later

        CALL GSPOS (VPC2SHELL(J),1,'EMCL',XCNTP1,YCNTP1,ZCNTR,
     +IRTTR(J),'ONLY' )

C.. Positioning all layers inside their "shell" volume:

C.. Starting from inside to outside, in the same order as volumes were
*   defined above:

caar  Temporary area
        ZCNTR=(PC2DZEXT+gap)/4.
        OZCNTR=(PC2DZEXT+ibar1dz+gap)/4.
        GZCNTR=0.
        splitcenter2 = (PC2DZEXT+gap2)/4.    ! CFM: PC2 version of splitcenter2
        splitgas2 = (pc2dzgas+gap2)/4.       ! CFM: this is the PC2 version
caar


C.. 1) Electronics ~1.0 cm thick - mother board, cables, connectors and chips

        XCNTP1 = 0.
        YCNTP1 = -DHPC2/2.+PC2ELECT/2. 

       CALL GSPOS (VPC2ELECT(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,OZCNTR,
     +0, 'ONLY' )
       CALL GSPOS (VPC2ELECT(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,-OZCNTR,
     +0, 'ONLY' )

C.. 2) Chevron/Pixel cathode panel: S2-glass-epoxy sheet + copper board
*                                   0.010" thick

        XCNTP1 = 0.
        YCNTP1 = -DHPC2/2.+PC2ELECT+PC2CTSHT/2. 

         CALL GSPOS (VPC2CTSH1(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,ZCNTR,
     +0, 'ONLY' )
         CALL GSPOS (VPC2CTSH1(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,-ZCNTR,
     +0, 'ONLY' )

C.. 3) Chevron/Pixel cathode panel: 


C.. 3a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = -(PC2RINST+PC2ELECT+PC2CTSHT+PC2CTHEX/2.)*TANG+
     >             PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC2/2.+PC2ELECT+PC2CTSHT+PC2CTHEX/2.

         CALL GSPOS (VPC2SBCLO(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 3b) Left side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

         CALL GSPOS (VPC2SBCLI(J),JPT,VPC2SBCLO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )


C.. 3c) HEXCELL sheet 1.25" thick (left side)

        XCNTP1 = -((PC2RINST+PC2ELECT+PC2CTSHT+PC2CTHEX/2.)*TANG-
     >              PC23SBAR/COSINE-PC23CBAR/2.)/2.-PC23CBAR/2.  
        YCNTP1 = -DHPC2/2.+PC2ELECT+PC2CTSHT+PC2CTHEX/2.

         CALL GSPOS (VPC2CTHXL(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
         CALL GSPOS (VPC2CTHXL(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )


C.. 3d) Central bar (the walls of carbon-fiber square tubing)

        XCNTP1 = 0.
        YCNTP1 = -DHPC2/2.+PC2ELECT+PC2CTSHT+PC2CTHEX/2.

         CALL GSPOS (VPC2CBCTO(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
         CALL GSPOS (VPC2CBCTO(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 3e) Central bar (the inside of carbon-fiber square tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

         CALL GSPOS (VPC2CBCTI(J),JPT,VPC2CBCTO(J),XCNTP1,YCNTP1,
     + 0.,0, 'ONLY' )

C.. 3f) HEXCELL sheet 1.25" thick (right side) 

        XCNTP1 = ((PC2RINST+PC2ELECT+PC2CTSHT+PC2CTHEX/2.)*TANG-
     >             PC23SBAR/COSINE-PC23CBAR/2.)/2.+PC23CBAR/2.  
        YCNTP1 = -DHPC2/2.+PC2ELECT+PC2CTSHT+PC2CTHEX/2.

         CALL GSPOS (VPC2CTHXR(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
         CALL GSPOS (VPC2CTHXR(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 3g) Right side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = (PC2RINST+PC2ELECT+PC2CTSHT+PC2CTHEX/2.)*TANG-
     >            PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC2/2.+PC2ELECT+PC2CTSHT+PC2CTHEX/2.

         CALL GSPOS (VPC2SBCRO(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 3h) Right side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

         CALL GSPOS (VPC2SBCRI(J),JPT,VPC2SBCRO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 4) Chevron/Pixel cathode panel: S2-glass-epoxy sheet + copper board
*                                   0.010" thick

        XCNTP1 = 0.
        YCNTP1 = -DHPC2/2.+PC2ELECT+PC2CTSHT+PC2CTHEX+PC2CTSHT/2.

         CALL GSPOS (VPC2CTSH2(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,ZCNTR,
     +0, 'ONLY' )
         CALL GSPOS (VPC2CTSH2(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,-ZCNTR,
     +0, 'ONLY' )

C.. 5) Gas Gap plane:

C.. 5a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = -(PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+
     >             PC2GASGP/2.)*TANG+PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP/2.

         CALL GSPOS (VPC2SBWLO(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 5b) Left side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

         CALL GSPOS (VPC2SBWLI(J),JPT,VPC2SBWLO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )


C.. 5c) Active volume (Gas 50% Ethane 50% Argon and wires)

        XCNTP1 = 0.
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP/2.

        CALL GSPOS (VPC2GASGP(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,
     +splitgas2,0, 'ONLY' )
        CALL GSPOS (VPC2GASGP(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,
     +-splitgas2,0, 'ONLY' )

        
C.. 5d) Right side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = (PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+
     >             PC2GASGP/2.)*TANG-PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP/2.

         CALL GSPOS (VPC2SBWRO(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 5e) Right side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

         CALL GSPOS (VPC2SBWRI(J),JPT,VPC2SBWRO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 6) Ground cathode panel: Carbon-Epoxy + Conductor 0.02" thick

        XCNTP1 = 0.
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >            PC2GRSHT/2.

         CALL GSPOS (VPC2GRSH1(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,ZCNTR,
     +0, 'ONLY' )
         CALL GSPOS (VPC2GRSH1(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,-ZCNTR,
     +0, 'ONLY' )
         
C.. 7) Ground cathode panel:


C.. 7a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = -(PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >             PC2GRSHT+PC2GRHEX/2.)*TANG+PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >            PC2GRSHT+PC2GRHEX/2.

         CALL GSPOS (VPC2SBGLO(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 7b) Left side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

         CALL GSPOS (VPC2SBGLI(J),JPT,VPC2SBGLO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 7c) HEXCELL sheet 1.25" thick (left side)

        XCNTP1 = -((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >              PC2GRSHT+PC2GRHEX/2.)*TANG-PC23SBAR/COSINE-
     >              PC23CBAR/2.)/2.-PC23CBAR/2.  
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >            PC2GRSHT+PC2GRHEX/2.

         CALL GSPOS (VPC2GRHXL(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1, 
     +splitcenter2,0, 'ONLY' )
         CALL GSPOS (VPC2GRHXL(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1, 
     +-splitcenter2,0, 'ONLY' )

C.. 7d) Central bar (the walls of carbon-fiber square tubing)

        XCNTP1 = 0.
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >            PC2GRSHT+PC2GRHEX/2.

         CALL GSPOS (VPC2CBGRO(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1, 
     +splitcenter2,0, 'ONLY' )
         CALL GSPOS (VPC2CBGRO(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1, 
     +-splitcenter2,0, 'ONLY' )


C.. 7e) Central bar (the inside of carbon-fiber square tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

         CALL GSPOS (VPC2CBGRI(J),JPT,VPC2CBGRO(J),XCNTP1,YCNTP1, 
     + 0.,0, 'ONLY' )

C.. 7f) HEXCELL sheet 1.25" thick (right side) 

        XCNTP1 = ((PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >             PC2GRSHT+PC2GRHEX/2.)*TANG-PC23SBAR/COSINE-
     >             PC23CBAR/2.)/2.+PC23CBAR/2.  
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >            PC2GRSHT+PC2GRHEX/2.

         CALL GSPOS (VPC2GRHXR(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1, 
     +splitcenter2,0, 'ONLY' )
         CALL GSPOS (VPC2GRHXR(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1, 
     +-splitcenter2,0, 'ONLY' )

C.. 7g) Right side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = (PC2RINST+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >             PC2GRSHT+PC2GRHEX/2.)*TANG-PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >            PC2GRSHT+PC2GRHEX/2.

         CALL GSPOS (VPC2SBGRO(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 7h) Right side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

         CALL GSPOS (VPC2SBGRI(J),JPT,VPC2SBGRO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 8) Ground cathode panel: carbon-epoxy sheet
*                                   0.020" thick

        XCNTP1 = 0.
        YCNTP1 = -DHPC2/2.+PC2ELECT+2.*PC2CTSHT+PC2CTHEX+PC2GASGP+
     >            PC2GRSHT+PC2GRHEX+PC2GRSHT/2.


         CALL GSPOS (VPC2GRSH2(J),JPT,VPC2SHELL(J),XCNTP1,YCNTP1,ZCNTR,
     +0, 'ONLY' )
         CALL GSPOS (VPC2GRSH2(J),LPT,VPC2SHELL(J),XCNTP1,YCNTP1,-ZCNTR,
     +0, 'ONLY' )


c     9) Central Support


c     9b) air in tubes

         if(j.eq.1) then
         ZCNTR=-.5*hollow

         call gspos (VPC2Z0SP(2),JPT,VPC2Z0SP(1),0.,0.,ZCNTR,
     +  0, 'ONLY')
         call gspos (VPC2Z0SP(2),LPT,VPC2Z0SP(8),0.,0.,-ZCNTR,
     +  0, 'ONLY')
         else
         endif

c     9a) hollow tubes...got to place 4 in each sector

        ZCNTR=sbarpz+ibar2dz/2+tbar2dz
        XCNTP1=sbarpx
        YCNTP1=sbarpy

            call gspos (VPC2Z0SP(1),J,VPC2SHELL(J),XCNTP1,
     +     YCNTP1+yshift,ZCNTR,0, 'ONLY')
            call gspos (VPC2Z0SP(8),J+PC23NCHMB,VPC2SHELL(J),
     +                 XCNTP1,YCNTP1+yshift,-ZCNTR,0, 'ONLY')
            call gspos (VPC2Z0SP(1),J+PC23NCHMB*2,VPC2SHELL(J),
     +                 XCNTP1,-YCNTP1+yshift,ZCNTR,0, 'ONLY')
            call gspos (VPC2Z0SP(8),J+PC23NCHMB*3,VPC2SHELL(J),
     +                 XCNTP1,-YCNTP1+yshift,-ZCNTR,0, 'ONLY')
         
c      9c) End bars
        ZCNTR=sbarpz+sbardz/2.+enbdz/2.+tbar2dz+ibar2dz/2
        XCNTP1=sbarpx
        YCNTP1=sbarpy

            call gspos (VPC2Z0SP(3),J,VPC2SHELL(J),XCNTP1,
     +     YCNTP1+yshift,-ZCNTR,0, 'ONLY')
            call gspos (VPC2Z0SP(3),J+PC23NCHMB,VPC2SHELL(J),
     +     XCNTP1,YCNTP1+yshift,ZCNTR,0, 'ONLY')
            call gspos (VPC2Z0SP(3),J+PC23NCHMB*2,VPC2SHELL(J),
     +     XCNTP1,-YCNTP1+yshift,-ZCNTR,0, 'ONLY')
            call gspos (VPC2Z0SP(3),J+PC23NCHMB*3,VPC2SHELL(J),
     +     XCNTP1,-YCNTP1+yshift,ZCNTR,0, 'ONLY')


c     9d) T beam...cross piece
        
        ZCNTR=tbar2dz*2+tbar1dz+ibar2dz
        XCNTP1=0.
        YCNTP1=yshift
 
           call gspos (VPC2Z0SP(4),J,VPC2SHELL(J),XCNTP1,
     +     YCNTP1,ZCNTR,0, 'ONLY')
           call gspos (VPC2Z0SP(4),J+PC23NCHMB,VPC2SHELL(J),XCNTP1,
     +     YCNTP1,-ZCNTR,0, 'ONLY')

c         T beam...upright
        ZCNTR=tbar2dz+ibar2dz
        XCNTP1=0.
        YCNTP1=yshift
 
           call gspos (VPC2Z0SP(5),J,VPC2SHELL(J),XCNTP1,
     +     YCNTP1,ZCNTR,0, 'ONLY')
           call gspos (VPC2Z0SP(5),J+PC23NCHMB,VPC2SHELL(J),XCNTP1,
     +     YCNTP1,-ZCNTR,0, 'ONLY')


c     9e)  I beam...optional

           if(ibeamtf) then

             ZCNTR=0.
             XCNTP1=0.
             YCNTP1=ibar2dy+ibar1dy
c         top/bottom pieces
           call gspos (VPC2Z0SP(6),J,VPC2SHELL(J),XCNTP1,
     +     YCNTP1+yshift,ZCNTR,0, 'ONLY')
           call gspos (VPC2Z0SP(6),J+PC23NCHMB,VPC2SHELL(J),XCNTP1,
     +     -YCNTP1+yshift,ZCNTR,0, 'ONLY')

c         upright
            ZCNTR=0.
            XCNTP1=0.
            YCNTP1=yshift
            call gspos (VPC2Z0SP(7),J,VPC2SHELL(J),XCNTP1,
     +     YCNTP1,ZCNTR,0, 'ONLY')

           else
           endif
          


C.. ----------- P O S I T I O N    P C 3   D E T E C T O R ------------ ..C


        RPSPD1 = PC3RINST+DHPC3/2. ! inner radius + half of the thickness
        XCNTP1 =   RPSPD1            * CSFI
        YCNTP1 =   RPSPD1            * SNFI

C.. Positioning PC3 "shell" volumes 

        zcntr = 0.0                           ! CFM: zcntr redefined several times

        CALL GSPOS (VPC3SHELL(J),1,'EMCL',XCNTP1,YCNTP1,ZCNTR,
     +IRTTR(J),'ONLY' )
caar  Temporary area
        splitgas2=(pc3dzgas+gap2)/4.           ! CFM: this is the PC3 version
        splitcenter2= (PC3DZEXT+gap2)/4.       ! CFM: this is the PC3 version
        ZCNTR=(PC3DZEXT+gap)/4.                ! CFM: this is the PC3 version
        OZCNTR=(PC3DZEXT+2.*ibar1dz+gap)/4.    !electronics
caar
C.. Positioning all layers inside their "shell" volume:

C.. Starting from inside to outside, in the same order as volumes were
*   defined above:

C.. 1) Ground cathode panel: carbon-epoxy sheet
*                                   0.030" thick

        XCNTP1 = 0.
        YCNTP1 = -DHPC3/2.+PC3GRSHT/2.

        CALL GSPOS (VPC3GRSH1(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,ZCNTR,
     +0, 'ONLY' )
        CALL GSPOS (VPC3GRSH1(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,-ZCNTR,
     +0, 'ONLY' )
        
         
C.. 2) Ground cathode panel:

C.. 2a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = -(PC3RINST+PC3GRSHT+PC3GRHEX/2.)*TANG+
     >            PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC3/2.+PC3GRSHT+PC3GRHEX/2.

        CALL GSPOS (VPC3SBGLO(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 2b) Left side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

        CALL GSPOS (VPC3SBGLI(J),JPT,VPC3SBGLO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 2c) HEXCELL sheet 1.5" thick (left side)

        XCNTP1 = -((PC3RINST+PC3GRSHT+PC3GRHEX/2.)*TANG-
     >            PC23SBAR/COSINE-PC23CBAR/2.)/2.-PC23CBAR/2.  
        YCNTP1 = -DHPC3/2.+PC3GRSHT+PC3GRHEX/2.

        CALL GSPOS (VPC3GRHXL(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
        CALL GSPOS (VPC3GRHXL(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 2d) Central bar (the walls of carbon-fiber square tubing)

        XCNTP1 = 0.
        YCNTP1 = -DHPC3/2.+PC3GRSHT+PC3GRHEX/2.

        CALL GSPOS (VPC3CBGRO(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
        CALL GSPOS (VPC3CBGRO(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 2e) Central bar (the inside of carbon-fiber square tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

        CALL GSPOS (VPC3CBGRI(J),JPT,VPC3CBGRO(J),XCNTP1,YCNTP1,
     + 0.,0, 'ONLY' )

C.. 2f) HEXCELL sheet 1.5" thick (right side) 

        XCNTP1 = ((PC3RINST+PC3GRSHT+PC3GRHEX/2.)*TANG-
     >            PC23SBAR/COSINE-PC23CBAR/2.)/2.+PC23CBAR/2.  
        YCNTP1 = -DHPC3/2.+PC3GRSHT+PC3GRHEX/2.

        CALL GSPOS (VPC3GRHXR(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
        CALL GSPOS (VPC3GRHXR(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 2g) Right side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = (PC3RINST+PC3GRSHT+PC3GRHEX/2.)*TANG-
     >            PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC3/2.+PC3GRSHT+PC3GRHEX/2.

        CALL GSPOS (VPC3SBGRO(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 2h) Right side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

        CALL GSPOS (VPC3SBGRI(J),JPT,VPC3SBGRO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 3) Ground cathode panel: Carbon-Epoxy + Conductor 0.02" thick

        XCNTP1 = 0.
        YCNTP1 = -DHPC3/2.+PC3GRSHT+PC3GRHEX+PC3GRSHT/2.

        CALL GSPOS (VPC3GRSH2(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
        CALL GSPOS (VPC3GRSH2(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 4) Gas Gap plane:

C.. 4a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = -(PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP/2.)*TANG+
     >            PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP/2.

        CALL GSPOS (VPC3SBWLO(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +GZCNTR,0, 'ONLY' )

C.. 4b) Left side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

        CALL GSPOS (VPC3SBWLI(J),JPT,VPC3SBWLO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 4c) Active volume (Gas 50% Ethane 50% Argon and wires)

        XCNTP1 = 0.
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP/2.

        CALL GSPOS (VPC3GASGP(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitgas2,0, 'ONLY' )
        CALL GSPOS (VPC3GASGP(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitgas2,0, 'ONLY' )
        
C.. 4d) Right side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = (PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP/2.)*TANG-
     >            PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP/2.

        CALL GSPOS (VPC3SBWRO(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 4e) Right side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

        CALL GSPOS (VPC3SBWRI(J),JPT,VPC3SBWRO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 5) Chevron/Pixel cathode panel: S2-glass-epoxy sheet + copper board
*                                   0.020" thick

        XCNTP1 = 0.
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+PC3CTSHT/2.

        CALL GSPOS (VPC3CTSH1(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
        CALL GSPOS (VPC3CTSH1(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 6) Chevron/Pixel cathode panel: 

C.. 6a) Left side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = -(PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+PC3CTSHT+
     >             PC3CTHEX/2.)*TANG+PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+PC3CTSHT+
     >             PC3CTHEX/2.

        CALL GSPOS (VPC3SBCLO(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 6b) Left side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

        CALL GSPOS (VPC3SBCLI(J),JPT,VPC3SBCLO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 6c) HEXCELL sheet 1.5" thick (left side)

        XCNTP1 = -((PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+PC3CTSHT+
     >            PC3CTHEX/2.)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)/2.-
     >            PC23CBAR/2.  
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >            PC3CTSHT+PC3CTHEX/2.

       CALL GSPOS (VPC3CTHXL(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )

       CALL GSPOS (VPC3CTHXL(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 6d) Central bar (the walls of carbon-fiber square tubing)

        XCNTP1 = 0.
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >            PC3CTSHT+PC3CTHEX/2.

        CALL GSPOS (VPC3CBCTO(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
        CALL GSPOS (VPC3CBCTO(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 6e) Central bar (the inside of carbon-fiber square tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

        CALL GSPOS (VPC3CBCTI(J),JPT,VPC3CBCTO(J),XCNTP1,YCNTP1,
     + 0.,0, 'ONLY' )

C.. 6f) HEXCELL sheet 1.5" thick (right side) 

        XCNTP1 = ((PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+PC3CTSHT+
     >            PC3CTHEX/2.)*TANG-PC23SBAR/COSINE-PC23CBAR/2.)/2.+
     >            PC23CBAR/2.  
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >            PC3CTSHT+PC3CTHEX/2.

        CALL GSPOS (VPC3CTHXR(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
        CALL GSPOS (VPC3CTHXR(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 6g) Right side bar (The walls of carbon-fiber parallelepiped tubing)

        XCNTP1 = (PC3RINST+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+PC3CTSHT+
     >             PC3CTHEX/2.)*TANG-PC23SBAR/(2.*COSINE)  
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+PC3CTSHT+
     >             PC3CTHEX/2.

        CALL GSPOS (VPC3SBCRO(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 6h) Right side bar (air, the inside of the tubing)

        XCNTP1 = 0.
        YCNTP1 = 0.

        CALL GSPOS (VPC3SBCRI(J),JPT,VPC3SBCRO(J),XCNTP1,YCNTP1,GZCNTR,
     +0, 'ONLY' )

C.. 7) Chevron/Pixel cathode panel: S2-glass-epoxy sheet 
*                                   0.020" thick

        XCNTP1 = 0.
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >           PC3CTSHT+PC3CTHEX+PC3CTSHT/2. 

        CALL GSPOS (VPC3CTSH2(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +splitcenter2,0, 'ONLY' )
        CALL GSPOS (VPC3CTSH2(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,
     +-splitcenter2,0, 'ONLY' )

C.. 8) Electronics ~1.0 cm thick - mother board, cables, connectors and chips

        XCNTP1 = 0.
        YCNTP1 = -DHPC3/2.+2.*PC3GRSHT+PC3GRHEX+PC3GASGP+
     >           2.*PC3CTSHT+PC3CTHEX+PC3ELECT/2. 

        CALL GSPOS (VPC3ELECT(J),JPT,VPC3SHELL(J),XCNTP1,YCNTP1,OZCNTR,
     +0, 'ONLY' )
        CALL GSPOS (VPC3ELECT(J),LPT,VPC3SHELL(J),XCNTP1,YCNTP1,-OZCNTR,
     +0, 'ONLY' )


c     9) Central Support


c     9b) air in tubes

        if(j.eq.1) then
        ZCNTR=-.5*hollow

        call gspos (VPC3Z0SP(2),JPT,VPC3Z0SP(1),0.,0.,ZCNTR,
     +  0, 'ONLY')
        call gspos (VPC3Z0SP(2),LPT,VPC3Z0SP(8),0.,0.,-ZCNTR,
     +  0, 'ONLY')
        else
        endif

c     9a) hollow tubes...got to place 4 in each sector

        ZCNTR=sbarpz+ibar2dz/2+tbar2dz
        XCNTP1=sbarpx
        YCNTP1=sbarpy+p3yoff

           call gspos (VPC3Z0SP(1),J,VPC3SHELL(J),XCNTP1,
     +     YCNTP1+yshift2 ,ZCNTR,0, 'ONLY')
           call gspos (VPC3Z0SP(8),J+PC23NCHMB,VPC3SHELL(J),
     +                 XCNTP1,YCNTP1+yshift2 ,-ZCNTR,0, 'ONLY')
           call gspos (VPC3Z0SP(1),J+PC23NCHMB*2,VPC3SHELL(J),
     +                 XCNTP1,-YCNTP1+yshift2 ,ZCNTR,0, 'ONLY')
           call gspos (VPC3Z0SP(8),J+PC23NCHMB*3,VPC3SHELL(J),
     +                 XCNTP1,-YCNTP1+yshift2 ,-ZCNTR,0, 'ONLY')

c     9c) End bars
        ZCNTR=sbarpz+sbardz/2.+enbdz/2.+tbar2dz+ibar2dz/2
        XCNTP1=sbarpx
        YCNTP1=sbarpy+p3yoff

           call gspos (VPC3Z0SP(3),J,VPC3SHELL(J),XCNTP1,
     +     YCNTP1+yshift2 ,-ZCNTR,0, 'ONLY')
           call gspos (VPC3Z0SP(3),J+PC23NCHMB,VPC3SHELL(J),
     +     XCNTP1,YCNTP1+yshift2 ,ZCNTR,0, 'ONLY')
           call gspos (VPC3Z0SP(3),J+PC23NCHMB*2,VPC3SHELL(J),
     +     XCNTP1,-YCNTP1+yshift2 ,-ZCNTR,0, 'ONLY')
           call gspos (VPC3Z0SP(3),J+PC23NCHMB*3,VPC3SHELL(J),
     +     XCNTP1,-YCNTP1+yshift2 ,ZCNTR,0, 'ONLY')

c     9d) T beam...cross piece
        
        ZCNTR=2*tbar2dz+tbar1dz+ibar2dz
        XCNTP1=0.
        YCNTP1=0.
 
          call gspos (VPC3Z0SP(4),J,VPC3SHELL(J),XCNTP1,
     +     YCNTP1+yshift2,ZCNTR,0, 'ONLY')
          call gspos (VPC3Z0SP(4),J+PC23NCHMB,VPC3SHELL(J),XCNTP1,
     +     YCNTP1+yshift2,-ZCNTR,0, 'ONLY')

c         T beam...upright
        ZCNTR=tbar2dz+ibar2dz
        XCNTP1=0.
        YCNTP1=0.
 
          call gspos (VPC3Z0SP(5),J,VPC3SHELL(J),XCNTP1,
     +     YCNTP1+yshift2,ZCNTR,0, 'ONLY')
          call gspos (VPC3Z0SP(5),J+PC23NCHMB,VPC3SHELL(J),XCNTP1,
     +     YCNTP1+yshift2,-ZCNTR,0, 'ONLY')


c     9e)  I beam...optional

          if(ibeamtf) then

            ZCNTR=0.
            XCNTP1=0.
            YCNTP1=ibar2dy+ibar1dy+.85
c         top/bottom pieces
          call gspos (VPC3Z0SP(6),J,VPC3SHELL(J),XCNTP1,
     +     YCNTP1+yshift2 ,ZCNTR,0, 'ONLY')
          call gspos (VPC3Z0SP(6),J+PC23NCHMB,VPC3SHELL(J),XCNTP1,
     +     -YCNTP1+yshift2 ,ZCNTR,0, 'ONLY')

c         upright
           ZCNTR=0.
           XCNTP1=0.
           YCNTP1=0.
           call gspos (VPC3Z0SP(7),J,VPC3SHELL(J),XCNTP1,
     +     YCNTP1+yshift2,ZCNTR,0, 'ONLY')

          else
          endif


  715 CONTINUE

C.. -------------------- END OF DETECTOR GEOMETRY SETUP ------------------ 

 9999 continue
      write(*,*) '**************************************' 
      write(*,*) 'Modified Pad chamber geometry called.' 
      write(*,*) '**************************************' 
      return

c------------------------------------------------------------------------------
  999 continue
      write(6,1000)
 1000  format(/'pc23gem - read error in pc23_par segment'/,3x,
     +   '  Namelist mis-match in pc23_par segment ?',//,3x,
     +   'The geometry will be re-read to pinpoint the erroneous',
     +   ' line'//)

      rewind( itf_lun )
      read( itf_lun, nml = pc23_par )
      stop 'pc23gem - stop because of phnx.par file error.'
      end
