c $Id: ncc.f,v 1.35 2010/11/17 17:28:31 richi Exp $
C    author : Edward Kistenev
C    author : Sky D. Rolnick 02/28/07
C    modified : Sky D. Rolnick 08/03/08
C    modified : SKy D. Rolnick 1/14/08

      SUBROUTINE NCC
      
      IMPLICIT NONE
C
C   ********************************************************************
C   *                                                                  *
C   *  create the user datacards and sets defaults                     *
C   *                                                                  *
C   *  all lengths in cm                                               *
C   *  all angles in degrees                                           *
C   *  all energies in GeV    
C   *  detector is built from double-sensor assemblies                 *
C   *  the cards has the following format:                             *
C   *                                                                  *
C   *  SWIT                                                            *
C   *       1 = kinematic debug (GUKINE)                               *
C   *       2 = tracking debug  (GUSTEP)                               *
C   *       3 = output debug    (GUOUT)                                *
C   *  
C   *  NCC configuration                                               *
C   *  NCC                                                             *
C   *       1 = Z_POLE  - Z-position of Magnet Pole (NCC ends there)   *
C   *       2 = BW_X    - beam window X-size (subassembly units)       *
C   *       3 = BW_Y    - beam window Y-size (subassembly units)       *
C   *       4 = SKIN    - Skin thickness                               *
C   *       5 = ROWS    - number of brick rows                         *
C   *       6 = RLEN    - maximum raw length                           *
C   *   Same common block includes MAP(rows,rlen) array with flags     *
C   *   set to 1 if dual-crystal assembly is present                   *
C   * 

C-- LONGITUDINAL STRUCTURE OF NCC (thicknesses of components)

C--   WPLT
C   *      1 = W_PL    - Base W plate thickness                       *
 
C--   PSTK
C   *      1 = PGAP   - Gap reserved for pad structured readout
C   *      2 = PADS   - Pad-structured Si crystal
C   *      3 = PCAR   - G10 pad carrier layer (dual sensor)
C   *      4 = PINT   - G10 interconnect layer in pad stacks
C   *      5 = PALS   - Ceramic spacer in pad stacks
C   *      6 = PCUF   - Cu foil in pad stacks
C   *      7 = PBAK   - G10 or Ceramic backing to pad stack

C--   SSTK
C   *      1 = SGAP   - Gap reserved for strip structured readout
C   *      2 = STRP   - Strip-structured Si crystal 
C   *      3 = SCAR   - G10 strip sensor carrier (dual sensor size)
C   *      4 = SROU   - readout gap (components)
C   *      5 = SHYB   - Strip Hybrid (G10)
C   *      6 = SCUF   - Cu foil in strip stacks
C   *      7 = SBAK   - Ceramic backing to strip stack


C-- LATERAL STRUCTURE OF NCC (lateral sizes of components)

C   *  LATR   
C   *       1 = TOL     - Positive tolerance applied to constr. comp.  *
C   *       2 = CRDXY   - Lateral size of a Si crystal                 *
C   *       3 = SUBDX   - X-size of the substrate                      *
C   *       4 = SUBDY   - Y-size of the substrate                      *
C   *       5 = STDX    - stack center displacement (from default)     *
C   *       6 = STDY    - stack center displacement (from default)     *
C   *                                                                  *
C   *  PADS - Brick (pads) structure (format: INT)    
C   *       1 = N_SEGMENTS - number of segnments in calorimeter (max 3)
C   *       2 = N_CELLS(3) - number of cells in each segment 
C   *  STRP - Brick (strips) structure (format: INT)    
C   *       1 = N_STRPS    - number of strip layers in calorimeter (max 6)
C   *       2 = L_STRPS(8) - locations of strip layers in units of cells
C   *                                                                  *
C   *  EVNT
C   *    val1 = evKind     - 0(single track), 1(from pythia)

C   *  TRCK
C   *    val1 = PKIND      - track particle ID (evKind = 0)
C   *    val2 = PMODE      - simulation mode (P or pT)
C   *    val3 = P_MIN      - minimum track P or pT
C   *    val4 = p_MAX      - maximum track P or pT
C   *    val5 = PHI_IMP_MIN - min azimuthal angle (in X/Y plane)       *
C   *    val6 = PHI_IMP_MAX - min azimuthal angle (in X/Y plane)       *
C   *    val7 = THETA_IMP_MIN - min polar angle (or Rap)               *
C   *    val8 = THETA_IMP_MAX - max polar angle (or Rap)               *
C   *    val9 = PKINEMAX      - maximum allowed particle momentum      *
C   *  VERT 
C   *    val1 = X_IMP      - X coordinate of the vertex                *
C   *    val2 = dX_IMP     - spread in X                               *
C   *    val3 = Y_IMP      - Y coordinate                              *
C   *    val4 = dY_IMP     - spread in Y                               *
C   *                                                                  *
C   *  RCVR                                                            *
C   *  file name for recovery storage and the saving frequency         *
C   *                                               (format MIXED)     *
C   *    val1 = SAVE_PERIOD - number of events before next save        *
C   *    val2 = SAVE_FILE   - save file name - up to 40 characters     *
C   *                         no blancs allowed: '........'            *
C   ********************************************************************
C--  VOLUME identifiers (volume_id)
C--  there are some excesses - simply to avoid troubles with detector sets

C--  1 - NCC    - 
C--  2 - S0     - Segment-0 subassembly  (dual sensor)
C--  3 - S1     - Segment-1 subassembly  (dual sensor)
C--  4 - S2     - Segment-2 subassembly  (dual sensor) 

C--  6 - STRP   - Strip layer subassembly (X/Y)                         - dual sensor
C--  7 - PADS   - Pad structured sampling cell subassembly (includes W) - dual sensor


C-- SKINS
C-- 16 - SKN1   - long vertical separator
C-- 17 - SKN2   - short vertical separator
C-- 18 - S0TB   - top/bottom for S0 subassemblies
C-- 19 - S1TB   - top/bottom for S1 subassemblies
C-- 20 - S2TB   - top/bottom for S2 subassemblies
C-- 21 -   
C-- 22 - S0BK   - vertical back wall for S0-subassemblies
C-- 23 - S1BK   - vertical back wall for S1-subassemblies
C-- 24 - S2BK   - vertical back wall for S2-subassemblies
C-- 25 -

C-- DUAL SENSOR sized parts (W plates and carrier boards)
C-- 31 - W_DS   - Base W plate to build PAD sampling cells (dual sensor)
C-- 35 - PCDS   - Carrier to build PAD sampling cells (dual sensor)
C-- 39 - SCDS   - Carrier to build STRP Layer (dual sensor)

C-- PAD SILICON MICROMODULE
C-- 46 - PDSI   - Pad-structured Si crystal 
C-- PAD - STACK
C-- 50 - SPSP   - Ceramic spacer in PAD stacks
C-- 54 - INTP   - Interconnect in PAD stacks
C-- 58 - CUFP   - CuFoil in PAD stacks
C-- 62 - COVP   - Backing in PAD stacks

C-- STRIP SILICON MICROMODULE
C-- 71 - STSI   - Strip-structured Si crystal 
C-- STRIP STACK
C-- 73 - SPSS   - empty space for components  
C-- 75 - INTS   - Strip Hybrid 
C-- 77 - CUFS   - Cu foil in strip stacks
C-- 79 - COVS   - Backing to strip stack


C--
C-- Whenever volume is created its volume number assigned by GEANT is stored
C-- in volume_n(40) (see common LOCATORS) in the same order as listed above
C-- When upper level volumes are packaged with lower level volumes they must 
C-- be assigned volume numbers different in case of multpiple copies
C-- Here is the numerology
C--     
C   ********************************************************************
C--   cell's structure
C--   sumpling cell is assumed consisting of a sequence of absorber plate, 
C--   a substrate (G10), Silicon crystal and Air gap to complement readout
C--   thickness to predefined gap value
C--   Hadronic cell will also have complementary W plate in front of the 4mm
C--   W plate
C--   STRIP readout layer is also created as a sampling cell.  
C--   Any of the components can be missing 
C--  The latter is filled by GENERIC VOLUMES
C--     1 -   volume_id # of the first layer
C--     2 -   
C--     3 -   
C--     4 -   
C--     5 -   
C--     6 -   
C--     7 -   
C--     8 -   volume_id # of the last layer




#include "gugeom.inc"
#include "gcbank.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"
#include "gcsets.inc"
#include "gcvolu.inc"
#include "gcunit.inc"
#include "gcnum.inc"
#include "sublink.inc"
#include "fncclink.inc"
#include "ncc_data.inc"

C
C--            DEFAULTS

C-   calorimeter ends at Z_POLE
      DATA        Z_POLE /60./
      DATA        BW_X/2/, BW_Y/1/
      DATA        SKIN/0.1/, ROWS/7/, RLEN/16/
C--  map of live dual-crystal assemblies. we make all layers equal for now
      DATA        MAP/
     1   0,  0,  0, 00,111,111,111,111,111,111,111,111, 00,  0,  0, 0,
     2   0, 00,111,111,111,111,111,111,111,111,111,111,111,111, 00, 0,
     3   0,111,111,111,111,111,111,111,111,111,111,111,111,111,111, 0,
     4   0,111,111,111,111,111,111, 00, 00,111,111,111,111,111,111, 0,
     3   0,111,111,111,111,111,111,111,111,111,111,111,111,111,111, 0,
     2   0, 00,111,111,111,111,111,111,111,111,111,111,111,111,  0, 0,
     1   0,  0,  0, 00,111,111,111,111,111,111,111,111, 00,  0,  0, 0/

      DATA        W_PL/0.2/
C--  glue layers not counted
      DATA        PADS/0.0535/, 
     &            PCAR/0.02/, PALS/0.09/, PINT/0.02/, PCUF/0.005/,
     &            PBAK/0.02/
      DATA        STRP/0.03/, 
     &            SCAR/0.1/,  SROU/0.1/,  SHYB/0.05/, SCUF/0.005/, 
     &            SBAK/0.04/

      DATA        TOL/0.01/, CRDXY/6.2/, XGAP/0.1/, YGAP/0.1/
     &            STDX/0.03/, STDY/0./

      DATA        N_SEGMENTS/3/,
     &            N_CELLS/8,7,7/
      DATA        N_STRPS/8/,
     &            L_STRPS/1,2,3,4,5,6,7,8/

C--  Nothing but generic volumes is used to build cells

C--  numbers in the structure description are pointers to volume_id
      data            maxvol/11/
      data            SGAP/0.65/, PGAP/0.25/, WGAP/0.0/
C      data            SGAP/0.25/, PGAP/0.25/, WGAP/0.0/
      data            cell_kinds/3/
      data            cell_st/
C--  STRIP LAYER
C--old:
     &  79, 77, 71, 75, 73, 39, 73, 75, 71, 77, 79,
C--  &  79, 77, 71, 75, 71, 75, 77, 79,  0,  0,  0,
C--  PAD structured sampling cell
     &  35, 50, 54, 46, 58, 62,  0,  0,  0,  0,  0,
C--  Tungsten
     &  31,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/

      DATA         N_TWR_XY/4/, N_PXL_XY/128/


C-- end usdcrd.f	 
      
      real    par(3), cellsafety
      integer imat,imed

      integer aluminum,copper,tungsten,lead,air,vac,g10,silicon
    
      real            FIELDM, TMAXFD, DMAXMS, DEEMAX, EPSIL,
     &                STMIN 
      integer         IFIELD
      real            ASIL, ZSIL, DENSIL, RLGSIL, ALGSIL
     

C-- begin base_volumes.f	 
      integer          id_stc, ns_c, comp, nmed, ivolu
      character*2      matid
      real             dim(3)

      integer          vn, ics
      real             z_cell
      integer          sp_copy, ic

      integer          irotSecond
      real             x, y, z, xx, yy, zz, z_em, z_had, x_sub, y_sub
      integer          ihor, iver, elflag, hadflag, rotm_flag

      integer set, i, j, irotfirst, iv0, jv0, ivol

C     Begin executable statements

      
      write(6,1)
 1    format(/,3x,'FOCAL geometry version October 20, 2010',/)

C-- W-plate is given one extra TOL in X-dimension and 2 extra TOL in Y
CC      WDX       = SUBDX+TOL
CC      WDY       = SUBDY+2*TOL


C-- W-plate is given one extra TOL in X-dimension and 2 extra TOL in Y
      WDX       = CRDXY   + XGAP + TOL
      WDY       = CRDXY*2 + YGAP + TOL
C--  W plate starts TOL away ftom Cu skin - ignored for now
      TOTW       = RLEN*WDX + 2.*SKIN
      NCC_RAD    = TOTW*1.1/2.
C--  X0, Y0 for the lower left corner in the local detector coordinate
      X0         = -TOTW/2.
      TOTH       = (WDY + 2.*SKIN)*ROWS
      Y0         = -TOTH/2.
C--
C--  Tower and Pixel sizes
      TWR_DXY    = CRDXY/N_TWR_XY
      PXL_DXY    = CRDXY/N_PXL_XY
C--  By this time - all modifications to defaults are already applied 
C--  we can compute X0/Y0 coordinates for every sensor in the detector 


C      print *, '<UGEOM> Coords', X0,'  ', Y0, '  ', TWR_DXY, '  ', 
C     *   PXL_DXY

C--  Define standard and USER particular materials
      DATA imat/2400/
      DATA imed/2405/

c     The following are tracking media numbers from the mat_mixt_med routine
c     The materials are defined in the src/phnxcore/mat_mixt_med.f routine
c     All of these media include tracking through a magnetic field

      DATA aluminum/34/    ! from tracking material 34 Al Frame !9
      DATA copper/30/      ! from tracking material 30 NCC Cu !11
      DATA tungsten/28/    ! defined from material 28 NCC W !12 
      DATA lead/29/        ! defined from material 29 NCC Pb !13
      DATA air/19/         ! from tracking material 19 Air + EM field !15
      DATA vac/32/         ! from tracking material 32 Vac + EM field !16
      DATA g10/27/         ! from tracking material 27 NCC G10
      DATA silicon/31/     ! from tracking material 31 NCC Si !50

C-- begin ugeom.f
C----------------------------------------------------------------------
C--  All GENERIC VOLUMES are at addresses starting from 21 in volume_id
C--  now the W and Pb plates
C--  Volumes 21-29 are double sensor sized in Y-direction (to simplify building bricks) 
      dim(1) = WDX/2.
      dim(2) = WDY/2.
      dim(3) = W_PL/2.
      volume_id(31) = 'W_DS'
      call gsvolu(volume_id(31), 'BOX ', tungsten, dim, 3, volume_n(31))

C--  Carrier board (PADS)
      dim(3) = PCAR/2.
      volume_id(35) = 'PCDS'
      call gsvolu(volume_id(35), 'BOX ', g10, dim, 3, volume_n(35))
C--  Carrier board (STRIPS)
      dim(3) = SCAR/2.
      volume_id(39) = 'SCDS'
      call gsvolu(volume_id(39), 'BOX ', g10, dim, 3, volume_n(39))

C--         PAD STRUCTURED SAMPLING CELLS
C--     PAD CRYSTAL's
      dim(1) = CRDXY/2.
      dim(2) = CRDXY/2.
      dim(3) = PADS/2.
      volume_id(46) = 'PDSI'
      call gsvolu(volume_id(46), 'BOX ', silicon, dim, 3, volume_n(46))
 
C--  Ceramic spacer
      dim(3) = PALS/2.
      volume_id(50) = 'SPSP'
      call gsvolu(volume_id(50), 'BOX ', aluminum, dim, 3, volume_n(50))
C--  Interconnect
      dim(3) = PINT/2.
      volume_id(54) = 'INTP'
      call gsvolu(volume_id(54), 'BOX ', g10, dim, 3, volume_n(54))
C--  Foil
      dim(3) = PCUF/2.
      volume_id(58) = 'CUFP'
      call gsvolu(volume_id(58), 'BOX ', copper, dim, 3, volume_n(58))
C--  Backing G10 or ceramics
      dim(3) = PBAK/2.
      volume_id(62) = 'COVP'
      call gsvolu(volume_id(62), 'BOX ', g10, dim, 3, volume_n(62))
      
C--         STRIP (PHOTON IDENTIFIER) LAYERS
C--     STRIP CRYSTAL's
      dim(1) = CRDXY/2.
      dim(2) = CRDXY/2.
      dim(3) = STRP/2.
      volume_id(71) = 'STSI'
      call gsvolu(volume_id(71), 'BOX ', silicon, dim, 3, volume_n(71))

C--  Air gap (space for readout components) 
      dim(3) = SROU/2.
      volume_id(73) = 'SPSS'
      call gsvolu(volume_id(73), 'BOX ', air, dim, 3, volume_n(73))
C--  Strip hybrid
      dim(3) = SHYB/2.
      volume_id(75) = 'INTS'
      call gsvolu(volume_id(75), 'BOX ', g10, dim, 3, volume_n(75))
C--  Cu ground foil
      dim(3) = SCUF/2.
      volume_id(77) = 'CUFS'
      call gsvolu(volume_id(77), 'BOX ', copper, dim, 3, volume_n(77))
C-- BACKING
      dim(3) = SBAK/2.
      volume_id(79) = 'COVS'
      call gsvolu(volume_id(79), 'BOX ', aluminum, dim, 3, volume_n(79))

C-- end base_volumes.f


C--          ALL EXTRAS
C--  Compute few extra basic geometrical parameters
C--  All we need to know about sampling cells

      DO I = 1, cell_kinds 
         if(i.eq.1) then
            cell_depth(i) = SGAP 
         else if(i.eq.2) then
            cell_depth(i) = PGAP
         else 
            cell_depth(i) = WGAP
         endif
         cellsafety = cell_depth(i)
C--  add absorbers to default gaps      
         DO J = 1, maxvol
            ivol = cell_st(j,i)
            call GLOOK(volume_id(ivol), iq(jvolum+1), nvolum, iv0)
            if(iv0.gt.0) then
               jv0 = lq(jvolum-iv0)
            else
               jv0 = 0
            endif
            if(jv0.gt.0) then
               if(cell_st(j,i).ge.31.and.cell_st(j,i).le.34) then
                  cell_depth(i) = cell_depth(i) + q(jv0+9)*2.
C                      print *, '<UGEOM> Cell ',i,' layer ',j, q(jv0+9),
C     &                 ' depth ',cell_depth(i)
               else
                  cellsafety = cellsafety - q(jv0+9)*2.
C                      print *, '<UGEOM> Cell ',i,' layer ',j, q(jv0+9),
C     &                 ' depth ',cell_depth(i),' safety ',cellsafety
               endif
            endif
         END DO
C--         print *, '<UGEOM> Cell depth :', i, 
C--     &        cell_depth(i),' safety gap ',cellsafety

      END DO


C--  Find total depth occupied by calorimeter segments
      do i = 1, N_SEGMENTS
C--   This is the tungsten depth
         SEGM_DEPTH(i) = cell_depth(3)*N_CELLS(i)
C--   This is the silicon pads depth
         SEGM_DEPTH(i) = SEGM_DEPTH(i)+cell_depth(2)*N_CELLS(i)
C--   This is the skin depth
         SEGM_DEPTH(i) = SEGM_DEPTH(i)+SKIN
C--   If we are in the second and third segments, add more W
         if(i.gt.1) then
            SEGM_DEPTH(i) = SEGM_DEPTH(i)+cell_depth(3)*N_CELLS(i)
         end if
      end do

C--   Now add in extra depth to account for the strips
C--   Note there is redundancy here to account for the cases
C--   when the number of strips exceeds the number of layers
C--   in the first segment.
      do i = 1, N_STRPS
         if(L_STRPS(i).le.N_CELLS(1)) then
            SEGM_DEPTH(1)  = SEGM_DEPTH(1) + cell_depth(1)
         else if(L_STRPS(i).le.N_CELLS(1)+N_CELLS(2)) then
            SEGM_DEPTH(2)  = SEGM_DEPTH(2) + cell_depth(1)
         else             
            SEGM_DEPTH(3)  = SEGM_DEPTH(3) + cell_depth(1)
         end if
      end do
      NCC_DEPTH  = SEGM_DEPTH(1) + SEGM_DEPTH(2) + SEGM_DEPTH(3)
C--  Entrance face position 
      Z_NCC     = Z_POLE-NCC_DEPTH
C--  create NCC specific volumes (cells, segments)
         print *, '<UGEOM> S0/S1/S2/NCC DEPTH-s '
     +        ,SEGM_DEPTH(1),' ', SEGM_DEPTH(2),' ',SEGM_DEPTH(3),
     &        ' ',NCC_DEPTH  


      call COMPAUND_VOLUMES
C--  
C--  Package compaund volumes
c$$$      print *, '<UGEOM> call to FILL_COMPAUND_VOLUMES'
      CALL FILL_COMPAUND_VOLUMES
C--  Single NCC
C--  Combine electromagnetic and hadronic segments together
c$$$      print *, '<UGEOM> call to BUILD_SINGLE_NCC'
      CALL BUILD_SINGLE_NCC
 



C--  Bring it into the world 

C--  North
cc      call GSPOS ( 'NCC ', 1, 'HALL', 0., 0., Z0+NCC_DEPTH/2, 0,
cc     &  'ONLY')  ! no rotation, OK

      call GSPOS ( 'NCC ', 1, 'HALL', 0., 0., Z_NCC+NCC_DEPTH/2, 0,
     &  'ONLY') 

C--  rotation matrix
      irot = irot + 1    ! standard PISA way of defining a new rotation matrix
      irotFirst = irot   ! standard PISA way of defining a new rotation matrix
      call gsrotm(irotFirst, 90., 180., 90., 90., 180., 0.)

CC      call GSPOS ( 'NCC ', 2, 'HALL', 0., 0., -(Z0+NCC_DEPTH/2),
CC     &  irotFirst, 'ONLY')  ! using first rotation definition

      call GSPOS ( 'NCC ', 2, 'HALL', 0., 0., -(Z_NCC+NCC_DEPTH/2),
     & irotFirst, 'ONLY') 

C      print *, '<UGEOM> call to UDET'

      CALL UDET

C      CALL GPMATE(0)
C      CALL GPMATE(28)
C      CALL GPMATE(10)
C      CALL GPTMED(0)

c      CALL GPRINT('MATE', 0)  ! enable these to print out material numbers
c      CALL GPRINT('TMED', 0)
c      CALL GPRINT('VOLU', 0)

  999 RETURN
 9001 format(a3,i1)

      RETURN
      END


C**********************************************************************  
      SUBROUTINE UDET
*******************************************************************
*                                                                 *
*      USER routine to handle detector basic parameters           *
*      The variables we want are E deposited in various parts     *
*      of the detector and X,Y,Z                                  *
*                                                                 *
*******************************************************************

      IMPLICIT NONE
            
#include "gclist.inc"
#include "gcflag.inc"
#include "ncc_data.inc"

C      character*4 MFPD(5)

      DATA NAMESHNCC/'EVT','INCC','TWR ','SENR',
     &               'TOF ','DEDX'/

      DATA NBITSHNCC/NHITSNCC*32/
C      DATA ORIGNCC/3*1000., 1000., 3*1000., 0., 0./
C      DATA FACTNCC/3*1000., 1000., 3*1000., 1., 1./
      DATA ORIGNCC/6*0./
      DATA FACTNCC/4*1., 2*1000./

      DATA       NBITSV/32,32/

c     The following detector types are used in gustep.F
c     The NCC is the 22nd detector volume defined for PISA
c     So all the detector types start with 2022
c     The detector type number has no consequence in GEANT
c     except for what the user does with the number
c     See GEANT manual page 150 for GSDET discussion

      integer idType1 /20221/
      integer idType2 /20222/

      integer set, iset, det, idet
            
      data       set_names/'NCC ', 'NCC ',
     &                     'ABSP', 'ABSS', 'SKNS'/

C--  no other ways found to combine same volume names pointed by
C--  different path ways into sets. At this time all absorbers are 
C--  grouped into 6 different sets  what is clumsy but ...
C-- Numbering scheme
C--  NCC             1/2      - positive and negative z
C--  Subassembly(1x2 sensors) - iver+100*ihor
C--  Sampling cells: 1-N_CELLS(1)  - S0(preshower)
C--                  etc           - S1 (EMC1)
C--                  etc           - S2 (EMC2)
C--  Silicon:  cell kind + Top/Bottom*10 + cell layer*100 

      data       det_names/
     +    'NCC ', 'PDSI',       

     2    'NCC ', 'STSI',       

     3    'NCC ', 'W_DS',
     3    'NCC ', 'PCDS',
     3    'NCC ', 'SPSP',
     3    'NCC ', 'INTP',
     3    'NCC ', 'CUFP',
     3    'NCC ', 'COVP',


     4    'NCC ', 'SCDS',
     4    'NCC ', 'SPSS',
     4    'NCC ', 'INTS', 
     4    'NCC ', 'CUFS',
     4    'NCC ', 'COVS',

     5    'NCC ', 'S0TB',
     5    'NCC ', 'S1TB', 
     5    'NCC ', 'S2TB',
     5    'NCC ', 'S0BK', 
     5    'NCC ', 'S1BK', 
     5    'NCC ', 'S2BK',
     5    'NCC ', 'SKN1',
     5    'NCC ', 'SKN2'/

      data       n_detset/5/ 
      data       n_det   /1, 1,  6,  5,  8/
      data       det0    /0, 1,  2,  8, 13/
      data       active  /2, 2,  2,  2,  2/
      
C--  we will later add tower/stripixel addresses to cell number

      
C--  Silicon sensors
     
      set = 1
         do det = 1, n_det(set)
             print *,'<UDET> creating set/det ',
     &           set,'/',det,' ',set_names(set),'  ',
     &           det_names(active(set),det0(set)+det)

            CALL GSDET (set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           active(set), det_names(1,det0(set)+det),
     &           NBITSV, idType1, 500, 500, ISET, IDET)

            CALL GSDETH(set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           NHITSNCC,NAMESHNCC, NBITSHNCC,ORIGNCC,FACTNCC)
         end do


       set = 2
         do det = 1, n_det(set)
             print *,'<UDET> creating set/det ',
     &           set,'/',det,' ',set_names(set),'  ',
     &           det_names(active(set),det0(set)+det)

            CALL GSDET (set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           active(set), det_names(1,det0(set)+det),
     &           NBITSV, idType2, 500, 500, ISET, IDET)

            CALL GSDETH(set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           NHITSNCC,NAMESHNCC, NBITSHNCC,ORIGNCC,FACTNCC)
         end do

C      call GPSETS('*','*')
      RETURN
      END


C**********************************************************************
C--  find approximate 
      subroutine sensor_address(x, y, sen_x, sen_y)
      implicit none
      integer  sen_x, sen_y
      real     x, y
#include "ncc_data.inc"
      real     dx, dy
      integer  assemb_x, assemb_y
      dx    = x-X0
      if(x.gt.0) dx = dx - 2*SKIN
      dy    = y-Y0
      sen_x = int(dx/WDX)+1
      sen_y = int(dy/(WDY+2*SKIN))
C--      print *,'<sensor_address>', sen_x,sen_y,(WDY+2*SKIN)*(0.5+sen_y)
      if(dy.gt.(WDY+2*SKIN)*(0.5+sen_y)) then
         sen_y = 2*sen_y + 2
      else
         sen_y = 2*sen_y + 1
      end if
      return
      end

C**********************************************************************
      function isSeen(x, y, sen_x, sen_y)
      implicit none
      logical isSeen
      integer  sen_x, sen_y
      real     x, y
#include "ncc_data.inc"
      logical  answer 
      answer  = .true.
      if(x.lt.sensorX0(sen_x,sen_y).or.
     &     x.gt.sensorX0(sen_x,sen_y)+CRDXY) answer = .false.
      if(y.lt.sensorY0(sen_x,sen_y).or.
     &     y.gt.sensorY0(sen_x,sen_y)+CRDXY) answer = .false.
c$$$      if(.not.answer) print *,'<isSeen> miss ', x, y, sen_x, sen_y, 
c$$$     &     sensorX0(sen_x,sen_y), sensorY0(sen_x,sen_y) 
      isSeen = answer
      return
      end

C**********************************************************************
C--  converts x/y coordinates in the local coordinate system 
C--  (z is orthogonal to detector plane, particles are in pozitive z)
C--  into tower address (counting from X0, Y0)
C--  twr_x=0 or twr_y=0 on return means hit missing Silicon
C--  kind = 0 - tower number within sensor (range 1-4)
C--  kind = 1 - tower number scope of NCC (range 1-84)
      subroutine twr_address(kind, x, y, twr_x, twr_y)
      implicit none
#include "ncc_data.inc"

      integer  kind, sen_x, sen_y, twr_x, twr_y
      real     x, y, dx, dy
      logical  isSeen
      twr_x = 0
      twr_y = 0
C--      print *,'<twr_address> x/y ',x,y
      call sensor_address(x, y, sen_x, sen_y)
C--      print *,'<twr_address> sensor ', sen_x, sen_y,
C--     &     sensorX0(sen_x,sen_y), sensorY0(sen_x,sen_y)
C--      print *,'<twr_address> isSeen ',isSeen(x, y, sen_x, sen_y)
         dx   = x-sensorX0(sen_x,sen_y)
         dy   = y-sensorY0(sen_x,sen_y)
         twr_x = dx/TWR_DXY+1
         if(twr_x.lt.1) twr_x=1
         if(twr_x.gt.4) twr_x=4
         if(kind.ne.0) twr_x = twr_x + (sen_x-1)*N_TWR_XY
         twr_y = dy/TWR_DXY+1
         if(twr_y.lt.1) twr_y=1
         if(twr_y.gt.4) twr_y=4
         if(kind.ne.0) twr_y = twr_y + (sen_y-1)*N_TWR_XY
      return
      end

C**********************************************************************
C--  converts x/y coordinates in the local coordinate system 
C--  (z is orthogonal to detector plane, particles are in pozitive z)
C--  into PIXEL address (counting from X0, Y0)
C--  pxl_x=0 or pxl_y=0 on return means hit missing Silicon
C--  kind = 0 - tower number within sensor (range 1-4)
C--  kind = 1 - tower number scope of NCC (range 1-84)
      subroutine pixel_address(kind, x, y, pxl_x, pxl_y)
      implicit none

#include "ncc_data.inc"

      integer  kind, sen_x, sen_y, pxl_x, pxl_y
      real     x, y, dx, dy


      logical  isSeen
      pxl_x = 0
      pxl_y = 0
C--      print *,'<twr_address> x/y ',x,y
      call sensor_address(x, y, sen_x, sen_y)
C--      print *,'<twr_address> sensor ', sen_x, sen_y,
C--     &     sensorX0(sen_x,sen_y), sensorY0(sen_x,sen_y)
C--      print *,'<twr_address> isSeen ',isSeen(x, y, sen_x, sen_y)
C--      if(isSeen(x, y, sen_x, sen_y)) then
         dx   = x-sensorX0(sen_x,sen_y)
         dy   = y-sensorY0(sen_x,sen_y)
         pxl_x = dx/PXL_DXY+1
         if(pxl_x.lt.1)   pxl_x=1
         if(pxl_x.gt.128) pxl_x=128
         if(kind.ne.0) pxl_x = pxl_x + (sen_x-1)*N_PXL_XY
         pxl_y = dy/PXL_DXY+1
         if(pxl_y.lt.1)   pxl_y=1
         if(pxl_y.gt.128) pxl_y=128
          if(kind.ne.0) pxl_y = pxl_y + (sen_y-1)*N_PXL_XY
C--         print *,'<twr_address> hit in Si. Tower ',kind, twr_x, twr_y
C--      end if
      return
      end
C**********************************************************************

      subroutine strip_address(kind, x, y, stripx, stripy)
C--  converts pixel address into address of "strip" 
C--  oriented along x (kind=1) or along y (kind=2)  
C--  address is defined as strip-number (1-128) 
C--  and sensor position (X/Y location)
      
      implicit none
      
#include "ncc_data.inc"
      
      integer  kind, sen_x, sen_y, stripx, stripy
      real     x, y, dx, dy
      
      stripx = 0
      stripy = 0
      
C     print *,'strip_address is NOW IMPLEMENTED!!'
      call sensor_address(x, y, sen_x, sen_y)
      dx   = x-sensorX0(sen_x,sen_y)
      dy   = y-sensorY0(sen_x,sen_y)
      
      if(kind.eq.1) then
         stripx = dx/PXL_DXY+1
         stripy = 1
      else 
         stripx = 1
         stripy = dy/PXL_DXY+1
      endif
      
C     Check: make sure that the numbers are not out of bounds ...
      if(stripx.lt.1)   stripx=1
      if(stripx.gt.128) stripx=128
      if(stripy.lt.1)   stripy=1
      if(stripy.gt.128) stripy=128
      
C     print *,"Strips: ", kind, stripx, stripy
      
      return
      end

C**********************************************************************  
      SUBROUTINE COMPAUND_VOLUMES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Construct individual NOse Cone Calorimeter
C-                         volumes
C--  All volume Identifiers are stored in volume_id
C     -   It is assumed that module have tube shape (TUBE) of a radius slightly 
C--  in excess of TOTW/TOTH. Module is assembled from EM and HAD segments
C--  with PS and SM detectors being part of EM. Modules is created filled 
C--  with Air,
C-   Sampling cells are first built of absorber plates substrate layers 
C--  and Silicon crystals.  into the modules (dual crystal structure). 
C--  EM and HAD segments are built from sampling cells of a different kind.
C--  NCC object is then built from EM and HAD segments and positioned in 
C--  PHENIX in the North and South locations.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-AUG-2006   Edward Kistenev
C-   Modified 09-APR-2008   Converted to STRIPs in PhotonIdentifier layers
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
#include "ncc_data.inc"

      integer          vn 
      integer          air, copper
      real             dim(3)
C----------------------------------------------------------------------

      DATA air/19/         ! from tracking material 15
      DATA copper/30/      ! from tracking material 11

C-   create NCC
C--  inner radius
      dim(1) = BW_X*CRDXY/2
C--  outer radius
      dim(2) = NCC_RAD
C--  half depth
      dim(3) = NCC_DEPTH/2.
C--  Create NCC
      volume_id(1) = 'NCC '
      call GSVOLU(volume_id(1), 'TUBE', air, dim(1), 3, volume_n(1))
C-   create S0 SEGMENT VOLUME (dual sensor wide)
      dim(1) = WDX/2.
      dim(2) = (WDY+2.*SKIN)/2
      dim(3) = SEGM_DEPTH(1)/2.
      volume_id(2) = 'S0  '
C      print *, '<COMPAUND_VOLUMES> ', volume_id(2), dim(1), dim(2), 
C     &     dim(3)
      call GSVOLU(volume_id(2), 'BOX ', air, dim(1), 3, volume_n(2))
C-   create S1 SEGMENT VOLUME (dual sensor wide)
      dim(1) = WDX/2.
      dim(2) = (WDY+2.*SKIN)/2
      dim(3) = SEGM_DEPTH(2)/2.
      volume_id(3) = 'S1  '
C      print *, '<COMPAUND_VOLUMES> ', volume_id(2), dim(1), dim(2), 
C     &     dim(3)
      call GSVOLU(volume_id(3), 'BOX ', air, dim(1), 3, volume_n(3))
C-   create S2 SEGMENT VOLUME (dual sensor wide)
      dim(1) = WDX/2.
      dim(2) = (WDY+2.*SKIN)/2
      dim(3) = SEGM_DEPTH(3)/2.
      volume_id(4) = 'S2  '
C      print *, '<COMPAUND_VOLUMES> ', volume_id(2), dim(1), dim(2), 
C     &     dim(3)
      call GSVOLU(volume_id(4), 'BOX ', air, dim(1), 3, volume_n(4))
C--  create STRIP SAMPLING CELL (LAYER) volume
      dim(1) = WDX/2.
      dim(2) = WDY/2
      dim(3) = cell_depth(1)/2.
      volume_id(6) = 'STRP'
      call GSVOLU(volume_id(6), 'BOX ', air, dim(1), 3, volume_n(6))
C-   create PAD SAMPLING CELL (LAYER) VOLUME
      dim(1) = WDX/2.
      dim(2) = WDY/2
      dim(3) = cell_depth(2)/2.
      volume_id(7) = 'PADS'
      call GSVOLU(volume_id(7), 'BOX ', air, dim(1), 3, volume_n(7))
C-   create TUNGSTEN VOLUME
      dim(1) = WDX/2.
      dim(2) = WDY/2
      dim(3) = cell_depth(3)/2.
      volume_id(8) = 'TUNG'
      call GSVOLU(volume_id(8), 'BOX ', air, dim(1), 3, volume_n(8))

C--               SKINS
C--  
C--  We have 10 different skin elements in the whole calorimeter (ignoring 
C--  window itself)
C-- 16 - SKN1   - long vertical separator (top & bottom)
C-- 17 - SKN2   - short vertical separator  (window)
C-- 18 - S0TB   - top/bottom for PS subassemblies
C-- 19 - S1TB   - top/bottom for EM1 subassemblies
C-- 20 - S2TB   - top/bottom for EM2 subassemblies
C-- 21 -
C-- 22 - S0BK   - vertical back wall for S0(PS)-subassemblies
C-- 23 - S1BK   - vertical back wall for S1-subassemblies
C-- 24 - S2BK   - vertical back wall for S2-subassemblies
C-- 25 

      do 10 vn  = 1, 10
         if(vn.eq.6.or.vn.eq.10) go to 10
         if(vn.le.2) then
C-- Vertival right-left separators
            dim(1) = SKIN/2.
            if(vn.eq.1) then
               dim(2) = (TOTH - (WDY+2.*SKIN)*BW_Y)/4.
            else
               dim(2) = ((WDY+2.*SKIN)*BW_Y)/2.
            end if
            dim(3) = NCC_DEPTH/2.
            write(unit =volume_id(vn+15), fmt=9001) 'SKN', vn
         else 
            dim(1) = WDX/2.
            if(vn.ge.7) then
C--  W plate equivalent skin to close segment
               dim(2) = WDY/2.
               dim(3) = SKIN/2.
               write(unit =volume_id(vn+15), fmt=9002) 'S', 
     +              vn-7,'BK'
            else 
C--  Top-Bottom brick skins
               dim(2) = SKIN/2.
               if(vn.eq.3) then
                  dim(3) = SEGM_DEPTH(1)/2. 
               elseif(vn.eq.4) then 
                  dim(3) = SEGM_DEPTH(2)/2. 
               elseif(vn.eq.5) then 
                  dim(3) = SEGM_DEPTH(3)/2. 
               end if
               write(unit =volume_id(vn+15), fmt=9002) 'S', 
     +          vn-3, 'TB'
            end if
         end if

C--  SKIN's are copper
C         print *,'<COMPAUND VOLUMES> Skin ', vn, dim(1), dim(2),
C     &        dim(3),' ', volume_id(vn+15)
         call gsvolu(volume_id(vn+15), 'BOX ', copper, dim(1), 3, 
     &        volume_n(vn+15))
 10   continue
c$$$      print *, '<COMPAUND_VOLUMES>: return'
  999 RETURN
 9001 format(a3,i1)
 9002 format(a1,i1,a2)
      END


C**********************************************************************  
      SUBROUTINE FILL_COMPAUND_VOLUMES
C**********************************************************************  
      IMPLICIT NONE
#include "ncc_data.inc"

      integer          isc, i

      do i = 1, 100
         volume_n(i) = 0
      end do
C--  SAMPLING CELLs: may have up to maxvol layers (see cell_st)
      do isc = 1, CELL_KINDS
         call build_cell(isc, 5+isc)
      end do

C--  Calorimeter Subsaaemblies (two-towers wide)
      do i = 1, N_SEGMENTS
         if(SEGM_DEPTH(i).gt.0.) call build_subassembly(i)
C         print *,'Subassembly ',i,'  built '
      end do
  999 RETURN
      END



C**********************************************************************  
      SUBROUTINE BUILD_SINGLE_NCC
C**********************************************************************  
      IMPLICIT NONE
#include "gugeom.inc"
#include "gcflag.inc" 
#include "ncc_data.inc"  

      real             x, y, z, z_ps, z_S(3), x_sub, y_sub
      integer          ihor, iver, segm, flag(3), rotm_flag
      integer          irotSecond

C-- we start from inserting vertical right-left separators into NCC
      x  = -SKIN/2.
      y  = (WDY+2.*SKIN)*BW_Y/2. + (TOTH - (WDY+2.*SKIN)*BW_Y)/4.
      z  = 0.
      call gspos(volume_id(16), 3, 
     &              volume_id(1), x, y, z, 0, 'ONLY')
      y  = -y
      call gspos(volume_id(16), 1, 
     &              volume_id(1), x, y, z, 0, 'ONLY')
      x  = -x
      call gspos(volume_id(16), 2, 
     &              volume_id(1), x, y, z, 0, 'ONLY')
      y  = -y
      call gspos(volume_id(16), 4, 
     &              volume_id(1), x, y, z, 0, 'ONLY')
C-- add vertical walls to the window section (no hor. walls for now)
      x  = (WDX*BW_X)/2+SKIN/2.
      y  = 0
      call gspos(volume_id(17), 1, 
     &              volume_id(1), x,  y, z, 0, 'ONLY')
      call gspos(volume_id(17), 2, 
     &              volume_id(1), -x, y, z, 0, 'ONLY')
C--  sassemblies with x<0 should be rotated 180 degrees
C--  so we will need rotation matrix to position those.  The reason 
C--  is trivial - 
C--  we do not want different designs for left/right and there are few
C--  things which are asymmetric (like connectors). 
C--  Rotation matrix also takes care of the fact that sensors are displaced 
C--  respective to the "cental" positions on a substrate
C--  

       irot = irot + 1
       irotSecond = irot

       print *, '<BUILD_SINGLE_NCC> create RM 2'
       call gsrotm(irotSecond, 90., 180., 90., 270., 0., 0.)
       
       print *, '<BUILD_SINGLE_NCC> RM 2 created'
       call GPROTM(2)
     

C--
C-- we will now use MAP to position subassemblies
C-- we still keep old MAP structure: lower bit controls PS+EM0, upper EM1 and HAD 
C-- additional source of controls is the segment depth - when zero - segment ignored
      z_S(1)  = -NCC_DEPTH/2. + SEGM_DEPTH(1)/2.
      z_S(2)  = -NCC_DEPTH/2. + SEGM_DEPTH(1) + SEGM_DEPTH(2)/2.
      z_S(3)  = -NCC_DEPTH/2. + SEGM_DEPTH(1) + SEGM_DEPTH(2) + 
     &         SEGM_DEPTH(3)/2.
      x_sub = X0
      rotm_flag = irotSecond
      do ihor = 1, rlen
         x_sub = x_sub + WDX/2.
         y_sub = Y0
         do iver = 1, rows
            y_sub = y_sub + WDY/2.+SKIN
C            print *, 
C     &           '<BUILD_SINGLE_NCC> h/v ',ihor,iver,' x/y-sub ',x_sub,
C     &           y_sub
            sensorX0(ihor,2*iver)   = 0.
            sensorX0(ihor,2*iver-1) = 0.
            sensorY0(ihor,2*iver)   = 0.
            sensorY0(ihor,2*iver-1) = 0.
C--   if(MAP(ihor,iver).ne.0) then
            flag(1)  = MAP(ihor,iver)/100
            flag(2)  = mod(MAP(ihor,iver)/10,10)
            flag(3)  = mod(MAP(ihor,iver),10)
C--   print *, '<BUILD_SINGLE_NCC> subassembly: ',
C--   &              ihor, iver, MAP(ihor,iver), elflag, hadflag
C--   fill in the X0's/Y0's for the sensors
            sensorX0(ihor,2*iver)   = x_sub-CRDXY/2.+ 
     &           STDX*sign(1.,x_sub)
            sensorX0(ihor,2*iver-1) = sensorX0(ihor,2*iver) 
            sensorY0(ihor,2*iver)   = y_sub      +TOL/2+STDY
            sensorY0(ihor,2*iver-1) = y_sub-CRDXY-TOL/2-STDY
            do segm = 1, N_SEGMENTS
C--   first S0
               if(flag(segm).eq.1)  then
C                  print *, 
C     &                 '<BUILD_SINGLE_NCC> Segment/h/v ',segm,ihor,iver,
C     &                 ' x/y/z ',x_sub,y_sub,z_S(segm),
C     &                 '/ sX0 ', sensorX0(ihor,2*iver),' sY0-1 ',
C     &                 sensorY0(ihor,2*iver-1),' sY0-2 ',  
C     &                 sensorY0(ihor,2*iver) 
                  call gspos(volume_id(segm+1), 
     &                 (iver+100*ihor)*10+segm, volume_id(1), 
     &                 x_sub, y_sub, z_S(segm), rotm_flag, 'ONLY')
               end if
            end do
            y_sub = y_sub + WDY/2.+SKIN            
         end do
         x_sub = x_sub + WDX/2.
         if(ihor.eq.rlen/2) then
            x_sub = x_sub+2.*SKIN
            rotm_flag = 0
         end if
      end do


  999 RETURN
      END


C********************************************************************** 
      SUBROUTINE BUILD_SUBASSEMBLY(segment)
C********************************************************************** 
#include "gcbank.inc"
#include "gcnum.inc"
#include "ncc_data.inc"
C--      IMPLICIT NONE

      integer       segment
      DIMENSION IQ(1),Q(1),LQ(8000)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN)

      real             z_cell
      integer          sp_copy, ic
      z_cell  = -SEGM_DEPTH(segment)/2.
      do ic =1, N_CELLS(segment)
C--   (richi) Let's first place in some tungsten (not attached to silicon)
         z_cell = z_cell + cell_depth(3)/2
         volume_n(8) = volume_n(8)+1
         call gspos(volume_id(8), volume_n(8),  
     &        volume_id(segment+1), 0., 0., z_cell, 0, 'ONLY')
         print *,'TUNG',z_cell,ic
         z_cell = z_cell + cell_depth(3)/2
         if(segment.gt.1) then
            z_cell = z_cell + cell_depth(3)/2
            volume_n(8) = volume_n(8)+1
            call gspos(volume_id(8), volume_n(8),  
     &           volume_id(segment+1), 0., 0., z_cell, 0, 'ONLY')
         print *,'TUNG',z_cell,ic
            z_cell = z_cell + cell_depth(3)/2
         end if

C--   this is the pads identifier counter, needed to check whether a strip is needed
         volume_n(7) = volume_n(7)+1

C--   (richi) Next, let's add in the strips, in the first segment only
         do ist = 1, N_STRPS
            if(L_STRPS(ist).eq.volume_n(7).and.segment.eq.1) then
               z_cell = z_cell + cell_depth(1)/2.
               volume_n(6) = volume_n(6)+1
               call gspos(volume_id(6), volume_n(6),  
     &              volume_id(segment+1), 0., 0., z_cell, 0, 'ONLY')
         print *,'STRIP',z_cell,ic,ist
               z_cell = z_cell + cell_depth(1)/2.         

C                  print *, 
C     &                 '<BUILD_SINGLE_NCC> Strips ',ic,' ',
C     &                 ist,' ', z_cell ,' ', volume_n(6)
            endif
         END DO

C-- (richi) Finally, let's place some pads, which are now after the strips.
C--  add one more pad-structured cell (kind ==1)
         z_cell = z_cell + cell_depth(2)/2.
C-- moved earlier         volume_n(7) = volume_n(7)+1
         call gspos(volume_id(7), volume_n(7), volume_id(segment+1), 
     &        0., 0., z_cell, 0, 'ONLY')
         print *,'PADS',z_cell

         z_cell = z_cell + cell_depth(2)/2.
C                  print *, 
C     &                 '<BUILD_SINGLE_NCC> Pads ',ic,' ',
C     &                 ist,' ', z_cell ,' ', volume_n(6)
      end do
C--  close segment with BASE Cu plate
      z_cell = z_cell+SKIN/2.
C--  this is a back skin   -  copy # 1
      volume_n(21+segment) = volume_n(21+segment)+1
      call gspos(volume_id(21+segment), volume_n(21+segment), 
     &     volume_id(1+segment), 0., 0., z_cell, 0, 'ONLY')
C--  add top and bottom Cu plates
      volume_n(17+segment) = volume_n(17+segment)+1
      call gspos(volume_id(17+segment), volume_n(17+segment),   
     &     volume_id(1+segment), 0.,  (WDY+SKIN)/2, 0., 0, 'ONLY')
      volume_n(17+segment) = volume_n(17+segment)+1
      call gspos(volume_id(17+segment), volume_n(17+segment),  
     &     volume_id(1+segment), 0., -(WDY+SKIN)/2, 0., 0, 'ONLY')
      return
      end

