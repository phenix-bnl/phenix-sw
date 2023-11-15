
      SUBROUTINE MPCEX_PS
      
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
C   *  MXPS configuration                                              *
C   *  MXPS                                                            *
C   *       1 = Z_POLE  - Z-position of Magnet Pole (NCC ends there)   *
C   *       2 = BW_X    - beam window X-size (subassembly units)       *
C   *       3 = BW_Y    - beam window Y-size (subassembly units)       *
C   *       4 = SKIN    - Skin thickness (UNUSED FOR NOW)              *
C   *       5 = ROWS    - number of brick rows                         *
C   *       6 = RLEN    - maximum raw length                           *
C   *   Same common block includes MAP(rows,rlen) array with flags     *
C   *   set to 1 if dual-crystal assembly is present                   *
C   * 

C-- LONGITUDINAL STRUCTURE OF PRESHOWER (thicknesses of components)

C--   WPLT
C   *      1 = W_PL    - Base W plate thickness                       *

C--   SSTK
C   *      1 = SGAP   - Gap reserved for strip structured readout
C   *      2 = STRP   - Strip-structured Si crystal 
C   *      3 = SCAR   - G10 strip sensor carrier (dual sensor size)
C   *      4 = SROU   - readout gap (components)
C   *      5 = SHYB   - Strip Hybrid (G10)
C   *      6 = SCUF   - Cu foil in strip stacks
C   *      7 = SBAK   - Ceramic backing to strip stack


C   *  STRP - Brick (strips) structure (format: INT)    
C   *       1 = N_STRPS    - number of strip layers in calorimeter (max 8)
C   *       2 = L_STRPS(8) - locations of strip layers in units of cells

C   ********************************************************************
C--  VOLUME identifiers (volume_id)
C--  there are some excesses - simply to avoid troubles with detector sets

C--  1 - NCC    - 
C--  2 - S0     - Segment-0 subassembly
C--  3 - S0X    - Segment-0 subassembly w/o first layer (Run-14)
C--  4 - NCCX   - Special NCC volume for Run-14 MPC-EX S

C--  6 - STRP   - Strip layer subassembly (X/Y)                         

C-- DUAL SENSOR sized parts (W plates and carrier boards)
C-- 31 - W_DS   - Base W plate to build PAD sampling cells
C-- 39 - SCDS   - Carrier to build STRP Layer

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
#include "mpcex_ps_data.inc"
#include "guphnx.inc"

C
C--            DEFAULTS

C-   calorimeter ends at Z_POLE- MPC starts at ~220 cm
C-   This location is matched to the end of the MPC front plate
C-   Locations separated for S/N MPC-EX, south location set for Run-14
C-   Z_POLE is the *back* face of the MPC-EX
C-   JGL 08/26/2013
C    The gap between the MPC-EX front Al plate and the back of the MPC-EX
C    is 10.216cm. 

      DATA        Z_POLE_S /208.887/
      DATA        Z_POLE_N /208.887/
      DATA        BW_X/2/, BW_Y/2/

C-   Rows- number of brick rows and Rlen- raw length
      DATA        SKIN/0.0/, ROWS/6/, RLEN/6/

C--  map of live assemblies for preshower
C--  need to account for center layers offset so this looks wrong,
C--  but it is OK. 
 
      DATA        MAP/
     1     000,100,100,100,000,000,
     2     100,100,100,100,100,000,
     3     100,100,000,000,100,100,
     4     100,100,000,000,100,100,
     5     100,100,100,100,100,000,
     6     000,100,100,100,000,000/

C- Thickness of tungesten layer
      DATA        W_PL/0.2/

      DATA        STRP/0.05/, 
     &            SCAR/0.108/,  SROU/0.09/,  SHYB/0.051/, SCUF/0.0/, 
     &            SBAK/0.04/

      DATA        TOL/0.0/, CRDXY/6.2/, XGAP/0.10/, YGAP/0.10/
     &            STDX/0.0/, STDY/0.0/, SDEAD/0.10/, SBOND/0.022/

C NO DEAD AREAS
C      DATA        TOL/0.0/, CRDXY/6.2/, XGAP/0.0/, YGAP/0.0/
C     &            STDX/0.0/, STDY/0.0/, SDEAD/0.0/, SBOND/0.0/

      DATA        N_SEGMENTS/1/,
     &            N_CELLS/8,0,0/
c--  L_STRPS UNUSED now
      DATA        N_STRPS/8/,
     &            L_STRPS/1,2,3,4,5,6,7,8/

C--  Nothing but generic volumes is used to build cells

C--  numbers in the structure description are pointers to volume_id
      data            maxvol/11/
      data            SGAP/0.454/
      data            cell_kinds/1/
      data            cell_st/

C--  MINIPAD LAYER
     &  31, 39, 73, 75, 79, 71, 79, 0, 0, 0, 0,
C--  PAD structured sampling cell (UNUSED)
     &  0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,
C--  Just Tungsten (UNUSED) 
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/

c--  ~2mm minipads 
      DATA         N_TWR_XY/4/, N_PXL_XY/32/
      
      real    par(3)
      integer imat,imed

      integer aluminum,copper,tungsten,lead,air,vac,g10,silicon
    
      real            FIELDM, TMAXFD, DMAXMS, DEEMAX, EPSIL,
     &                STMIN 
      integer         IFIELD
      real            ASIL, ZSIL, DENSIL, RLGSIL, ALGSIL
      
      integer          id_stc, ns_c, comp, nmed
      character*2      matid
      real             dim(3)

      integer          vn, ics
      real             z_cell
      integer          sp_copy, ic

      real             x, y, z, xx, yy, zz, z_em, z_had, x_sub, y_sub
      integer          ihor, iver, elflag, hadflag, rotm_flag

      integer set, i, j, irotfirst, iv0, jv0

      integer sen_x, sen_y 

      integer xy, idepth, iarm

      integer ixp, iyp, iyndx, ixndx, roc_y_order(4)
C      DATA roc_y_order/1,2,4,3/
      DATA roc_y_order/3,4,2,1/

C     Begin executable statements
 
      write(6,1)
 1    format(/,3x,'MPC-EX preshower geometry version December 2013',/)

C-- W-plate is wider by space between micromodules
      WDX       = CRDXY   + XGAP
      WDY       = CRDXY   + YGAP
      TOTW       = RLEN*WDX

C-- adjusted a little wider to avoid overlaps 
      NCC_RAD    = TOTW/2.0 + 2.5; 

C--  X0, Y0 for the lower left corner in the local detector coordinate
      X0         = -TOTW/2.
      TOTH       =  WDY*ROWS
      Y0         = -TOTH/2.

C--  Tower and Pixel sizes
      TWR_DXY    = (CRDXY-2.*SDEAD)/N_TWR_XY
      PXL_DXY    = (CRDXY-2.*SDEAD)/N_PXL_XY

C--  By this time - all modifications to defaults are already applied 
C--  we can compute X0/Y0 coordinates for every sensor in the detector 

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
C      DATA air/19/         ! from tracking material 19 Air + EM field !15
      DATA air/119/        ! from tracking material 119 Air + EM field !15
      DATA vac/32/         ! from tracking material 32 Vac + EM field !16
      DATA g10/27/         ! from tracking material 27 NCC G10
      DATA silicon/31/     ! from tracking material 31 NCC Si !50

C----------------------------------------------------------------------
C--  All GENERIC VOLUMES are at addresses starting from 21 in volume_id
C--  now the W plates
      dim(1) = WDX/2.
      dim(2) = WDY/2.
      dim(3) = W_PL/2.
      volume_id(31) = 'W_DS'
      call gsvolu(volume_id(31), 'BOX ', tungsten, dim, 3, volume_n(31))

C--  Carrier board (STRIPS)
      dim(3) = SCAR/2.
      volume_id(39) = 'SCDS'
      call gsvolu(volume_id(39), 'BOX ', g10, dim, 3, volume_n(39))
      
C--   STRIP (PHOTON IDENTIFIER) LAYERS
C--   STRIP SENSORS
      dim(1) = CRDXY/2.
      dim(2) = CRDXY/2.
      dim(3) = STRP/2.
      volume_id(71) = 'STSI'
      call gsvolu(volume_id(71), 'BOX ', silicon, dim, 3, volume_n(71))

C--  Air gap (space for readout components) 
      dim(3) = SROU/2.
      volume_id(73) = 'SPSS'
      call gsvolu(volume_id(73), 'BOX ', air, dim, 3, volume_n(73))
C--  Strip ROC
      dim(3) = SHYB/2.
      volume_id(75) = 'INTS'
      call gsvolu(volume_id(75), 'BOX ', g10, dim, 3, volume_n(75))
C--  Ceramic spacer (using Al material)
      dim(3) = SBAK/2.
      volume_id(79) = 'COVS'
      call gsvolu(volume_id(79), 'BOX ', aluminum, dim, 3, volume_n(79))

C--  Compute few extra basic geometrical parameters
C--  All we need to know about sampling cells
     
      cell_depth(1) = SGAP + W_PL

C--  Find total depth occupied by calorimeter segments

C--  Now add up for each occupied layer

      SEGM_DEPTH(1) = 0.0
      do i = 1, N_STRPS
        SEGM_DEPTH(1)  = SEGM_DEPTH(1) + cell_depth(1)
      end do

C--  Explicitly zero out the segment depths for the 2,3 segments
C--  These segments are not part of the MPC-EX preshower
      
      SEGM_DEPTH(2) = 0.0
      SEGM_DEPTH(3) = 0.0

      NCC_DEPTH  = SEGM_DEPTH(1)

C--  Entrance face position 
      Z_NCC_S     = Z_POLE_S-NCC_DEPTH
      Z_NCC_N     = Z_POLE_N-NCC_DEPTH

C--  create MXPS specific volumes (cells, segments)
         print *, '<MPCEX_PS> S0/S1/S2/MXPS DEPTH(S) '
     +        ,SEGM_DEPTH(1),' ', SEGM_DEPTH(2),' ',SEGM_DEPTH(3),
     &        ' ',NCC_DEPTH  

      CALL MXPS_COMPOUND_VOLUMES

C--  Package compound volumes

      CALL FILL_MXPS_COMPOUND_VOLUMES

C--  Single MXPS

      CALL BUILD_SINGLE_MXPS
 
C--  Bring it into the world 

C--  North (positive z)
C-- For MXPS position in MUA1
      call GSPOS ( 'NCC ', 1, 'MUA1', 0., 0., (Z_NCC_N+NCC_DEPTH/2), 0,
     &  'ONLY') 
      
      print *,'Z_POLE_N = ',Z_POLE_N
      print *,'MXPS placed in MUA1 at z = ',(Z_NCC_N+NCC_DEPTH/2) 

C--  rotation matrix
      irot = irot + 1    ! standard PISA way of defining a new rotation matrix
      irotFirst = irot   ! standard PISA way of defining a new rotation matrix
      call gsrotm(irotFirst, 90., 180., 90., 90., 180., 0.)

C--  South (negative z)
C-- For MXPS position in MUA2

      if(RHICRUN.le.14) then
        call GSPOS ( 'NCCX', 2, 'MUA2', 0., 0., -(Z_NCC_S+NCC_DEPTH/2.),
     &    irotFirst, 'ONLY') 
      else
        call GSPOS ( 'NCC ', 2, 'MUA2', 0., 0., -(Z_NCC_S+NCC_DEPTH/2.),
     &    irotFirst, 'ONLY') 
      end if

      print *,'Z_POLE_S = ',Z_POLE_S
      print *,'MXPS placed in MUA2 at z = ',-(Z_NCC_S+NCC_DEPTH/2.) 

      CALL MXPS_UDET

C-- Dump sensor geometry information

      print *,'MPC-EX Sensor geometry information' 

      do iver = 1, rows
        do ihor = 1, rlen

           if( (MAP(ihor,iver)/100) .eq. 1) then
              call mxps_sensor_address(sensorX0(ihor,iver)+(WDX/2.),
     &             sensorY0(ihor,iver)+(WDY/2.), sen_x, sen_y)
              print *,'Sensor at x,y = ',sensorX0(ihor,iver),
     &             sensorY0(ihor,iver),sen_x,sen_y
           endif

        end do
      end do

C-- Output geometry in format for MpcExGeometry
C-- requires sensor centers

      OPEN(10,file='mpcexgeometry.dat', action='write',form='formatted')

C-- South/North arm z-layers

      write(10,*)"South arm z positions by layer:"

      do idepth = 1, N_STRPS
        write(10,*) idepth-1, 
     &        -(Z_NCC_S + (NCC_DEPTH/2.0) + layerZ(idepth))
      end do

      write(10,*)"North arm z positions by layer:"

      do idepth = 1, N_STRPS
        write(10,*) idepth-1, 
     &        (Z_NCC_N + (NCC_DEPTH/2.0) + layerZ(idepth))
      end do

C Write out the x,y location of each pixel for each sensor

      write(10,*)"Minipad x,y positions by sensor ix,iy"
      write(10,*)"Minipads listed in ROC BOND order"
      write(10,*)"NOTE: YOU MUST CONVERT x->-x for NORTH ARM"

      do ihor = 1, rlen
        do iver = 1, rows
          write(10,*) (ihor-1),(iver-1)
          if( (sensorY0(ihor,iver).eq.-9999.0) .or. 
     &          (sensorX0(ihor,iver).eq.-9999.0) ) then  
            write(10,*) -9999.0
          else
            
C           TOP CARRIER BOARD

            if(iver>=4) then  

C Output order is sensor ROC bond:

              write(10,*) "x-layer (TOP CARRIER BOARD):"
              do ixp = 1,N_PXL_XY
                do iyndx = 1,N_TWR_XY
                  iyp = roc_y_order(iyndx)

                  write(10,*) (ixp-1)*N_TWR_XY + (iyndx-1), 
     &             sensorX0(ihor,iver) + WDX - SDEAD -
     &             (ixp-0.5)*((CRDXY - 2.0*SDEAD)/N_PXL_XY), 
     &             sensorY0(ihor,iver) + SDEAD + 
     &             (iyp-0.5)*((CRDXY - 2.0*SDEAD - SBOND)/N_TWR_XY)
                
                end do 
              end do

              write(10,*) "y-layer (TOP CARRIER BOARD):"
              do iyp = 1,N_PXL_XY
                do ixndx = 1,N_TWR_XY
                  ixp = roc_y_order(ixndx)

                  write(10,*) (iyp-1)*N_TWR_XY + (ixndx-1),
     &             sensorX0(ihor,iver) + WDY - SDEAD -
     &             (ixp-0.5)*((CRDXY - 2.0*SDEAD - SBOND)/N_TWR_XY),
     &             sensorY0(ihor,iver) + WDX - SDEAD - 
     &             (iyp-0.5)*((CRDXY - 2.0*SDEAD)/N_PXL_XY) 
                
                end do 
              end do

            else

C             BOTTOM CARRIER BOARD  

              write(10,*) "x-layer (BOTTOM CARRIER BOARD):"
              do ixp = 1,N_PXL_XY
                do iyndx = 1,N_TWR_XY
                  iyp = roc_y_order(iyndx)

                  write(10,*) (ixp-1)*N_TWR_XY + (iyndx-1),
     &             sensorX0(ihor,iver) + SDEAD +
     &             (ixp-0.5)*((CRDXY - 2.0*SDEAD)/N_PXL_XY), 
     &             sensorY0(ihor,iver) + WDY - SDEAD -
     &             (iyp-0.5)*((CRDXY - 2.0*SDEAD - SBOND)/N_TWR_XY)
               
                end do 
              end do

              write(10,*) "y-layer (BOTTOM CARRIER BOARD):"
              do iyp = 1,N_PXL_XY
                do ixndx = 1,N_TWR_XY
                  ixp = roc_y_order(ixndx)

                  write(10,*) (iyp-1)*N_TWR_XY + (ixndx-1),
     &             sensorX0(ihor,iver) + SDEAD +
     &             (ixp-0.5)*((CRDXY - 2.0*SDEAD - SBOND)/N_TWR_XY),
     &             sensorY0(ihor,iver) + SDEAD + 
     &             (iyp-0.5)*((CRDXY - 2.0*SDEAD)/N_PXL_XY) 
                
                end do 
              end do

            end if

          end if
        end do
      end do

      write(10,*) "Calculated Minipad Dimensions:"
      write(10,*) (CRDXY - 2.0*SDEAD)/N_PXL_XY
      write(10,*) (CRDXY - 2.0*SDEAD - SBOND)/N_TWR_XY

      close(10) 


      RETURN
 
      END


C**********************************************************************  
      SUBROUTINE MXPS_UDET

      IMPLICIT NONE
            
#include "gclist.inc"
#include "gcflag.inc"
#include "mpcex_ps_data.inc"
#include "guphnx.inc"

      DATA NAMESHNCC/'EVT','INCC','TWR ','SENR',
     &               'TOF ','DEDX'/

      DATA NBITSHNCC/NHITSNCC*32/
      DATA ORIGNCC/6*0./
      DATA FACTNCC/4*1., 2*1000./

      DATA NAMESHNCCABS/'EVT','INCC','DEDX'/

      DATA NBITSHNCCABS/NHITSNCCABS*32/
      DATA ORIGNCCABS/3*0./
      DATA FACTNCCABS/2*1., 1*10.0/

      DATA NAMESHNCCENT/'VX','VY','VZ','PX','PY','PZ'/

      DATA NBITSHNCCENT/NHITSNCCENT*32/
      DATA ORIGNCCENT/6*400./
      DATA FACTNCCENT/6*1000./

      DATA       NBITSV/32,32,32/

c     The following detector types are used in gustep.F
c     The NCC is the 22nd detector volume defined for PISA
c     So all the detector types start with 2022
c     The detector type number has no consequence in GEANT
c     except for what the user does with the number
c     See GEANT manual page 150 for GSDET discussion

c     MXPS deliberately keeps the same detector types
c     as the NCC so that all downstream software is the 
c     same for the NCC/FOCAL/MXPS

      integer idType2 /20222/
      integer idType3 /20223/
      integer idType4 /20224/
      integer idType5 /20225/

      integer set, iset, det, idet
            
      data       set_names/'NCC ', 'EXAB', 'MPAL', 'EXNT'/

      data       det_names/

     1    'NCC ', 'STSI',   
    
     2    'NCC ', 'W_DS',
     2    'NCC ', 'SCDS',
     2    'NCC ', 'SPSS',
     2    'NCC ', 'INTS',
     2    'NCC ', 'COVS',
     3    'MPC ', 'FPLT',
     3    'MPC ', 'BPLT',
     3    'MPC ', 'SKIN',
     3    'MPC ', 'PLAT',

     4    'NCC ', 'W_DS'/

      data       n_detset/4/ 
      data       n_det   /1,  5,  4, 1/
      data       det0    /0,  1,  6, 10/
      data       active  /2,  2,  2, 2/
            
C--  Si Sensors as one detector set

       set = 1
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
  
       IF (CVOLU_OPT(1,26).EQ.'FULL') THEN 

C-- All absorber material combined as second set

         set = 2
         do det = 1, n_det(set)
             print *,'<UDET> creating set/det ',
     &           set,'/',det,' ',set_names(set),'  ',
     &           det_names(active(set),det0(set)+det)

            CALL GSDET (set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           active(set), det_names(1,det0(set)+det),
     &           NBITSV, idType3, 500, 500, ISET, IDET)

            CALL GSDETH(set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           NHITSNCCABS,NAMESHNCCABS, NBITSHNCCABS,ORIGNCCABS,
     &           FACTNCCABS)
         end do

C-- MPC Al absorber material as third set

         set = 3
         do det = 1, n_det(set)
             print *,'<UDET> creating set/det ',
     &           set,'/',det,' ',set_names(set),'  ',
     &           det_names(active(set),det0(set)+det)

            CALL GSDET (set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           active(set), det_names(1,det0(set)+det),
     &           NBITSV, idType4, 500, 500, ISET, IDET)

            CALL GSDETH(set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           NHITSNCCABS,NAMESHNCCABS, NBITSHNCCABS,ORIGNCCABS,
     &           FACTNCCABS)
         end do

       endif  


       IF (CVOLU_OPT(1,28).EQ.'FULL') THEN 
          
C-- Particles entering the MPC-EX as the fourth set

         set = 4
         do det = 1, n_det(set)
             print *,'<UDET> creating set/det ',
     &           set,'/',det,' ',set_names(set),'  ',
     &           det_names(active(set),det0(set)+det)

            CALL GSDET (set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           active(set), det_names(1,det0(set)+det),
     &           NBITSV, idType5, 500, 500, ISET, IDET)

            CALL GSDETH(set_names(set),
     &           det_names(active(set),det0(set)+det),
     &           NHITSNCCENT,NAMESHNCCENT, NBITSHNCCENT,ORIGNCCENT,
     &           FACTNCCENT)
         end do

       endif


C--     call GPSETS('*','*')
      RETURN
      END

C**********************************************************************  
      SUBROUTINE MXPS_COMPOUND_VOLUMES

      IMPLICIT NONE
#include "mpcex_ps_data.inc"
#include "guphnx.inc"

      integer          vn 
      integer          air, copper
      real             dim(3)

      DATA air/19/         ! from tracking material 15
      DATA copper/30/      ! from tracking material 11

C-   create MXPS
C--  inner radius
      dim(1) = (BW_X*WDX)/2.
C--  outer radius
      dim(2) = NCC_RAD
C--  half depth
      dim(3) = NCC_DEPTH/2.
C--  Create NCC
      volume_id(1) = 'NCC '
      call GSVOLU(volume_id(1), 'TUBE', air, dim(1), 3, volume_n(1))
     
      print *,'Outer radius = ', NCC_RAD

      if(RHICRUN.le.14) then
        volume_id(4) = 'NCCX'
        call GSVOLU(volume_id(4), 'TUBE', air, dim(1), 3, volume_n(4))
      end if
         
C-   create S0 SEGMENT VOLUME
      dim(1) = WDX/2.
      dim(2) = WDY/2.
      dim(3) = SEGM_DEPTH(1)/2.
      volume_id(2) = 'S0  '
      call GSVOLU(volume_id(2), 'BOX ', air, dim(1), 3, volume_n(2))

      if(RHICRUN.le.14) then
        volume_id(3) = 'S0X '
        call GSVOLU(volume_id(3), 'BOX ', air, dim(1), 3, volume_n(3))
      end if
     
C--  create STRIP SAMPLING CELL (LAYER) volume
      dim(1) = WDX/2.
      dim(2) = WDY/2
      dim(3) = cell_depth(1)/2.
      volume_id(6) = 'STRP'
      call GSVOLU(volume_id(6), 'BOX ', air, dim(1), 3, volume_n(6))

  999 RETURN
 9001 format(a3,i1)
 9002 format(a1,i1,a2)

      END

C**********************************************************************  
      SUBROUTINE FILL_MXPS_COMPOUND_VOLUMES

      IMPLICIT NONE
#include "mpcex_ps_data.inc"

      integer          isc, i

      do i = 1, 100
         volume_n(i) = 0
      end do

      call mpcex_build_ps_cell(1,6) 
      call build_mxps_subassembly(1)

      RETURN
      END

C**********************************************************************  
      SUBROUTINE BUILD_SINGLE_MXPS

      IMPLICIT NONE

#include "gugeom.inc"
#include "gcflag.inc" 
#include "mpcex_ps_data.inc"  
#include "guphnx.inc"

      real             x, y, z, z_ps, z_S(3), x_sub, y_sub
      integer          ihor, iver, segm, flag(3)
     
C-- we will now use MAP to position subassemblies
C-- we still keep old MAP structure: hundreds bit controls PS

      z_S(1)  = -NCC_DEPTH/2. + SEGM_DEPTH(1)/2.
      y_sub = Y0 + WDY/2.

      do iver = 1, rows

         if((iver.lt.3).or.(iver.ge.5)) then
           x_sub = X0 + WDX
         else
           x_sub = X0 + WDX/2.
         end if

         do ihor = 1, rlen

            flag(1)  = MAP(ihor,iver)/100

c           lower left-hand corner of volume
            sensorX0(ihor,iver)   = x_sub - WDX/2.
            sensorY0(ihor,iver)   = y_sub - WDY/2.
            
c            print *,ihor,iver,sensorX0(ihor,iver),
c     &           sensorY0(ihor,iver) 

            do segm = 1, N_SEGMENTS

C-- Only place S0

               if(flag(segm).eq.1)  then

                 call gspos(volume_id(segm+1), 
     &                 (iver+100*ihor)*10+segm, volume_id(1), 
     &                 x_sub, y_sub, z_S(segm), 0, 'ONLY')

                 if(RHICRUN.le.14) then

C--                 special NCCX volume for MPC-EX S in Run-14

                    if(iver.le.3) then

C--                    special volume w/o first layer

                       call gspos(volume_id(segm+2), 
     &                      (iver+100*ihor)*10+segm, volume_id(4), 
     &                      x_sub, y_sub, z_S(segm), 0, 'ONLY')

                    else

                       call gspos(volume_id(segm+1), 
     &                      (iver+100*ihor)*10+segm, volume_id(4), 
     &                      x_sub, y_sub, z_S(segm), 0, 'ONLY')

                    end if

                  end if

               else

                  sensorX0(ihor,iver) = -9999.0; 
                  sensorY0(ihor,iver) = -9999.0; 

               end if

            end do

            x_sub = x_sub + WDX            

         end do

         y_sub = y_sub + WDY

       end do


      RETURN
      END


C********************************************************************** 
      SUBROUTINE BUILD_MXPS_SUBASSEMBLY(segment)
C********************************************************************** 

      IMPLICIT NONE 

#include "gcbank.inc"
#include "gcnum.inc"
#include "mpcex_ps_data.inc"
#include "guphnx.inc"

      integer       segment, ist
      DIMENSION IQ(1),Q(1),LQ(8000)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN)

      real             z_cell

C--   Only valid for building the minipad segment
C--   The other segments are unused
      if(segment.ne.1) return 

      z_cell  = -SEGM_DEPTH(segment)/2.

      do ist = 1, N_STRPS

        z_cell = z_cell + cell_depth(1)/2.
 
C--     location of Si layer in each cell 

        layerZ(ist) = z_cell + depth_to_Si; 

        volume_n(6) = volume_n(6) + 1
                 
        call gspos(volume_id(6), volume_n(6),  
     &              volume_id(segment+1), 0., 0., z_cell, 0, 'ONLY')
               
c--     Fill the special volume for Run-14 configuration
 
        if((RHICRUN.le.14).and.(ist.ne.1)) then

          call gspos(volume_id(6), volume_n(6),  
     &                volume_id(segment+2), 0., 0., z_cell, 0, 'ONLY')

        end if


        print *,'STRIP CELL',z_cell,ist

        z_cell = z_cell + cell_depth(1)/2.         

      end do

      return
      end


C**********************************************************************
C DETECTOR ADDRESS ROUTINES
C**********************************************************************


C**********************************************************************
C--  find approximate 
      subroutine mxps_sensor_address(x, y, sen_x, sen_y)
      implicit none
      integer  sen_x, sen_y
      real     x, y
#include "mpcex_ps_data.inc"
      real     dx, dy
      integer  assemb_x, assemb_y
      dx    = x - X0
      dy    = y - Y0
      
      sen_y = int(dy/WDY)+1

c-- account for additional offset of center MXPS layers
c-- JGL 3/26/2011

      if( (sen_y.le.2).or.(sen_y.ge.5) ) then 
        sen_x = int((dx-(WDX/2.))/WDX)+1
      else
        sen_x = int(dx/WDX)+1
      end if

      return
      end

C**********************************************************************

      subroutine mxps_strip_address(kind, x, y, stripx, stripy)

C--  converts hit into address of "strip" 
C--  oriented along x (kind=1) or along y (kind=2)  
C--  address is defined as strip-number (1-128) 
C--  and sensor position (X/Y location)
      
c--  implemented strip dead areas JGL 12/10/2013
c--  added bonding pad dead area JGL 8/14/2015
      implicit none
      
#include "mpcex_ps_data.inc"
      
      integer  kind, sen_x, sen_y, stripx, stripy
      real     x, y, dx, dy
      
      stripx = 0
      stripy = 0
      
      call mxps_sensor_address(x, y, sen_x, sen_y)
      dx   = x - (sensorX0(sen_x,sen_y) + XGAP/2.0) - SDEAD
      dy   = y - (sensorY0(sen_x,sen_y) + YGAP/2.0) - SDEAD
      
      if(kind.eq.1) then

C--     Bonding pad dead area depends on top/bot
C--     carrier board orientation

        if(sen_y<4) then 
          dy = dy - SBOND;  
        end if

        if((dx.lt.0.).or.(dy.lt.0.).or.
     &     (dx.gt.(WDX - XGAP - 2.*SDEAD)).or.
     &     (dy.gt.(WDY - YGAP - 2.*SDEAD - SBOND))) then
         
C--        hit is in dead area
           return 
        end if

      else

C--     Bonding pad dead area depends on top/bot
C--     carrier board orientation

        if(sen_y>=4) then 
          dx = dx - SBOND;  
        end if

        if((dx.lt.0.).or.(dy.lt.0.).or.
     &     (dx.gt.(WDY - YGAP - 2.*SDEAD - SBOND)).or.
     &     (dy.gt.(WDX - XGAP - 2.*SDEAD))) then
         
C--        hit is in dead area
           return
        end if
 
      end if

C-- Calculate the hit strip number

      if(kind.eq.1) then
         stripx = int(dy/(TWR_DXY-SBOND))*N_PXL_XY + int(dx/PXL_DXY) + 1
         stripy = 1
      else 
         stripx = 1
         stripy = int(dx/(TWR_DXY-SBOND))*N_PXL_XY + int(dy/PXL_DXY) + 1
      end if
      
      if((stripx.lt.1).or.(stripy.lt.1).or.
     &   (stripx.gt.(N_TWR_XY*N_PXL_XY)).or.
     &   (stripy.gt.(N_TWR_XY*N_PXL_XY))) then
         
         stripx = 0
         stripy = 0

      end if
           
      return
      end

