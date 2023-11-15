C-
C   *       1 = Z_NCC   - Z-position of NCC Front Plate (computed)     *
C   *       2 = Z_POLE  - Z-position of Magnet Pole  (given)           *
C   *       3 = BW_X    - beam window X-size (double sensor units)     *
C   *       4 = BW_Y    - beam window Y-size (double sensor units)     *
C   *       5 = SKIN    - Skin thickness                               *
C   *       6 = ROWS    - number of brick rows                         *
C   *       7 = RLEN    - maximum raw length                           *

      COMMON/NCCDET/      Z_POLE, Z_NCC, BW_X,  BW_Y, SKIN, ROWS, RLEN, 
     &	               TOTW, TOTH, S0_DEPTH, S1_DEPTH,   S2_DEPTH, 
     &                 S3_DEPTH, NCC_DEPTH, NCC_RAD, SEGM_DEPTH(4), 
     &                 MAP(16,7), X0, Y0
      REAL             Z_POLE, Z_NCC, BW_X,  BW_Y, SKIN, TOTW, TOTH, 
     &		       S0_DEPTH, S1_DEPTH, S2_DEPTH, S3_DEPTH, 
     &                 NCC_DEPTH, NCC_RAD,   SEGM_DEPTH, X0, Y0
     &                  
      INTEGER          ROWS, RLEN, MAP


C-- LONGITUDINAL STRUCTURE OF NCC

C-- W PLATES FOR DUAL SENSOR MODULE
C   *      1 = W_PL    - Base W plate thickness                             *

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

      COMMON/LONG/     W_PL,
     +	               PGAP, PADS, PCAR, PINT, PALS, PCUF, PBAK,
     +                 SGAP, STRP, SCAR, SROU, SHYB, SCUF, SBAK,
     +                 WGAP
                      
      REAL             W_PL,
     +	               PGAP, PADS, PCAR, PINT, PALS, PCUF, PBAK,
     +                 SGAP, STRP, SCAR, SROU, SHYB, SCUF, SBAK,
     +                 WGAP
                      
 
C-- LATERAL STRUCTURE OF FOCal

C   *       1 = TOL     - Positive tolerance applied to W substrate          *
C   *       2 = CRDXY   - Lateral size of a Si crystal                       *
C   *       3 = XGAP    - Total free X-space left by crystals on W           *
C   *       4 = YGAP    - Total free Y-space left by double crystals on W    *
C   *       5 = STDX    - stack center displacement (from default center)    *
C   *       6 = STDY    - stack center displacement (from default center)    *
C   *       9 = WDX     - X-size of the W plate (dual subassembly substrate) *
C   *      10 = WDY     - Y-size of the W plate (dual subassembly substrate) *
C   *      11 = TWR_DXY - Lateral size of the pad tower                      *
C   *      11 = PXL_DXY - Lateral size of the pixel "tower"                  *

      COMMON/LATR/     TOL, CRDXY, XGAP, YGAP, STDX, STDY, 
     &                 WDX, WDY, TWR_DXY, PXL_DXY
      REAL             TOL, CRDXY, XGAP, YGAP, STDX, STDY, 
     &                 WDX, WDY, TWR_DXY, PXL_DXY

C   *       1 = N_TW_X   - towers per crystal (along X)
C   *       1 = N_TW_Y   - towers per crystal (along Y)
C   *       1 = N_PXL_X  - pixels per crystal (along X)
C   *       1 = N_PXL_X  - pixels per crystal (along Y)
      COMMON/GRANUL/   N_TWR_XY, N_PXL_XY
      INTEGER          N_TWR_XY, N_PXL_XY
	

C   *  LONG - Brick structure (format: INT)    
C   *       1 = N_SEGMENTS - number of segnments in calorimeter (max 3)
C   *       2 = N_CELLS(3) - number of cells in each segment 
C   *       3 = N_STRPS    - number of strip layers in calorimeter (max 8)
C   *       4 = L_STRPS(8) - locations of strip layers in units of cells
      COMMON/LONG/     N_SEGMENTS, N_CELLS(3), N_STRPS, L_STRPS(8),
     &                 S_CELLS, STRPS
      INTEGER          N_SEGMENTS, N_CELLS   , N_STRPS, L_STRPS,
     &                 S_CELLS, STRPS
C-- We keep two cells only: pad structured which include tungsten and strip structured
      COMMON/SCELLS/   maxvol, cell_st(11,3), cell_depth(3), cell_kinds
      integer          maxvol, cell_st, cell_kinds
      real             cell_depth

C--  *  relatively complex geometry of the detector makes it impractical 
C--  *  lengthy transformations from real coordinates into addresses
C--  *  we store some of the values for future use as LOCATORS 
      COMMON/LOCATORS/ ivPHNX, volume_id(100), volume_n(100),
     +	               sensorX0(16,16), sensorY0(16,16)
      integer          ivPHNX, volume_n
      character*4      volume_id
      real	       sensorX0, sensorY0

      common/basefn/   restore, saveperiod, baseFName
      common/fnames/   dataDir, hitsfile, recovfile, eventsfile 
      integer          saveperiod, restore
      character*200    baseFName, dataDir
      character*200    hitsfile, recovfile, eventsfile
C-
C--  normally unused (in a single track event mode) otherwise
C--  indicates how to get data for event and keeps this data for GUOUT
C--  evKind      - 0(single track), 1(from pythia)
C--  evHits      - number of particles (tracks from input stream)
C--  decay       - flag set to zero in GUKINE and reset to 1 if GEANT
C--                decayed primary particle (evKind=0) in flight (mostly for pi0s)
C--  evData(1)   - identity (0-fragmenting parton, 1-jet, 2-direct lepton)
C--  evData(2)   - PDG code
C--  evData(3-6) - 4-vector, lab system
C--  evData(7-9) - impact point
C--                       

        
      integer NHITSNCC
      parameter(NHITSNCC=6)
      common/GHNAMES/  NAMESHNCC(NHITSNCC),NBITSHNCC(NHITSNCC),
     &                 ORIGNCC(NHITSNCC),FACTNCC(NHITSNCC), 
     &	               NBITSV(2),  n_detset, SET_NAMES(5),n_det(5),  
     &                 det0(5),  active(5),  det_names(2,21)
      real             ORIGNCC,    FACTNCC
      character*4      SET_NAMES,  DET_NAMES,  NAMESHNCC
      integer          nbitshncc,    nbitsv,  n_detset, n_det, det0,
     +                 active
