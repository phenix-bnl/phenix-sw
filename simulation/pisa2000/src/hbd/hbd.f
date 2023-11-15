C $Id: hbd.f,v 1.34 2009/08/20 03:56:10 hpereira Exp $
C===============================================================================
C    File name: hbd.f
C    ---------
C    Authors: C.F. Maguire, Christine Aidala, Nikolai Smirnov (STAR)
C    Modified by Ilia Ravinovich for HBD stand alone in December 2003

C    Purpose: Set up the HBD geometry

C    Structure is a cylindrical volume made of air, called HBDG, 
C    placed in the HALL.  This outer volume serves as the main mother 
C    volume for the rest of the HBD.  

C    HALL (PHENIX HALL, for all subsystems in PISA)
C    |
C    HBDG (Outer air cylinder, gas safety volume, "HBD Mother")
C    |
C    TISO (Gas cylinder covering HBD region) 
C    |
C===============================================================================
      SUBROUTINE HBD(FULL,NH)
      Implicit none
C===============================================================================
C    Global Declarations
C===============================================================================
#include "guphnx.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"
#include "gugeom.inc"
C===============================================================================
C    Need to access zebra to write parameters to FZOUT file
C===============================================================================
#include "fstore.inc"
#include "sublink.inc"
#include "fphlink.inc"
C===============================================================================
C    TPC/HBD-specific include files
C===============================================================================
#include "hbd_geompar.inc"
#include "hbd_mednmb.inc"
C===============================================================================
C    Local Declarations
C===============================================================================
C    Main geometry parameter file (phnx.par) segment
C===============================================================================
      namelist/hbd_par/hbdg_rin, hbdg_rout, hbdg_dz,
     $     shade_in, shade_height, shade_spacing_x, shade_spacing_y,
     $     mylar_th, g10_th, g10_fr_x, g10_fr_y, g10_fr_z,
     $     g10_fr_1, g10_fr_2, g10_fr_3, g10_fr_4,
     $     g10_fr_5, g10_fr_6, g10_fr_7, g10_fr_8, g10_fr_9,
     $     g10_fr_10,
     &  
     $     hex_th, mylar_w_th, nitr_th,
     $     g10_gem_fr, g10_electr, gem_size, gem_spacer,
     $     copper_th, gem_dist, capton_th, gem_box,
     $     irot_hbd, hbdg_gas,
     $     hbd_westarm, hbd_eastarm, hbd_electronics, hbd_cf4gas,
     &     hbd_covers, hbd_hvpanels, hbd_walls, hbd_window,
     &     hbd_window_support, hbd_gem_t, hbd_gem_m, hbd_gem_b,
     &     hbd_gem_frames, hbd_csi, hbd_apanels
      
      integer      hbd_westarm
      integer      hbd_eastarm
      integer      hbd_electronics
      integer      hbd_cf4gas
      integer      hbd_covers
      integer      hbd_hvpanels
      integer      hbd_walls
      integer      hbd_window
      integer      hbd_window_support
      integer      hbd_gem_t
      integer      hbd_gem_m
      integer      hbd_gem_b
      integer      hbd_gem_frames
      integer      hbd_csi
      integer      hbd_apanels
      
      integer      startarm  ! to be used for West/East arm on/off
      integer      endarm  ! to be used for West/East arm on/off

      integer*4    nh                   ! set before call in gugeom (not used here)
      character*4  full                 ! set before call in gugeom
      character*3  hbdg_gas     ! radiator gas for HBD, set in phnx.par
      character*4  v_i_name     ! Volume name
      integer      nvol                  ! Internal volume # assigned by GEANT
C===============================================================================
C    The following three variables for ZEBRA usage
C===============================================================================
      integer      ioq, iPoint
      character*10 CHFORM
C===============================================================================
C    The following 16 variables for calls to GSDET and/or GSDETH:
C===============================================================================
      character*4  set_id, namesv(5)
      integer      nvhbd /7/     ! HBD gas and CsI are both 7th-level nested
      integer      nbitsv(7)
      integer      idtype, nwpa, nwsa, iset, idet
      character*4  hitnames(13)  
      integer      hitbits(13)
      real         origin(13), factor(13)
      integer      nshade_x, nshade_y
C===============================================================================
C    Variables for creating and positioning volumes
C===============================================================================
      integer nsect
      real par(15), pos(3)
C===============================================================================
C    Variables for assigning volume names
C===============================================================================
      real hbdg_wall_length
      real hbdg_wall_width1
      real hbdg_wall_width2
      real hbdg_wall_thickness

      real hbdg_cover1_length
      real hbdg_cover1_width1
      real hbdg_cover1_width2
      real hbdg_cover1_thickness
      
      real hbdg_cover2_length
      real hbdg_cover2_width1
      real hbdg_cover2_width2
      real hbdg_cover2_thickness
      
      real hbdg_cover3_length
      real hbdg_cover3_width1
      real hbdg_cover3_width2
      real hbdg_cover3_thickness
      
      real hbdg_cover4_length
      real hbdg_cover4_width1
      real hbdg_cover4_width2
      real hbdg_cover4_thickness

      real hbdg_hvpanel_length
      real hbdg_hvpanel_width1
      real hbdg_hvpanel_width2
      real hbdg_hvpanel_thickness
      real hbdg_pcbcu_thickness

      real hbdg_apanel_length
      real hbdg_apanel_width1
      real hbdg_apanel_width2
      real hbdg_apanel_thickness

      character*1 volid(12)
      character*1 volid2(12)
      character*1 numshade(26)
      character*4 Hgsm, Hgas, Hcsi, Hdet, name, name2
      character*1 nmsect(8)
      character*1 sectnum(6)
      character*1 armnum(2)
      character*1 attitudenum(2)
      real        ap_xyz(12,3)
      real        el_xyz(12,3)
      real        g_xyz(12,3)
      real        ap_rot(12,6)
      real        hp_xyz(4,3)
      real        hp_rot(4,6)
      integer     i,j,k,sect,cn
      character*2 detset(4)
      integer dettype(4)
      common /volumenum/ nvol
      common /pieceID/ sectnum,armnum,attitudenum 
      common /panelXYZ/ ap_xyz,ap_rot,hp_xyz,hp_rot
C===============================================================================
C    The following data statements are for assigning volume names 
      Data nmsect /'1','2','3','4','5','6','7','8'/   ! 8 sectors
      Data sectnum /'0','1','2','3','4','5'/  !6 sectors - final detector
      Data armnum /'0','1'/  !EAST - 0; WEST - 1
      Data attitudenum /'B','T'/ !TOP - 1, BOTTOM - 0
      Data volid /'A','B','C','D','E','F','G','H','I','J','K','L'/
      Data volid2 /'A','B','C','D','E','F','G','H','I','J','K','L'/
      Data numshade /'A','B','C','D','E','F','G','H',
     > 'I','J','K','L','M','N','O','P',
     > 'Q','R','S','T','U','V','W','X',
     > 'Y','Z'/
      Data dettype /11,8,11,8/
      Data detset /'PN','QN','PS','QS'/
      
      Data  hp_xyz(1,1)/-12.0579/,hp_xyz(1,2)/-57.4/,hp_xyz(1,3)/0.0/!EA BOT
      Data  hp_xyz(2,1)/-12.0579/,hp_xyz(2,2)/57.4/,hp_xyz(2,3)/0.0/ !EA TOP
      Data  hp_xyz(3,1)/12.0579/,hp_xyz(3,2)/-57.4/,hp_xyz(3,3)/0.0/ !WE BOT
      Data  hp_xyz(4,1)/12.0579/,hp_xyz(4,2)/57.4/,hp_xyz(4,3)/0.0/  !WE TOP   
      
!      Data (hp_rot(1,i),i=1,6) /90.0,168.113,90.0,90.0,0.0,180.0/  !EAST BOT
!      Data (hp_rot(2,i),i=1,6) /270.0,11.887,90.0,270.0,0.0,180.0/ !EAST TOP   
!      Data (hp_rot(3,i),i=1,6) /90.0,11.887,90.0,90.0,0.0,180.0/    !WEST BOT
!      Data (hp_rot(4,i),i=1,6) /270.0,168.113,90.0,270.0,0.0,180.0/ !WEST TOP   

      Data (hp_rot(1,i),i=1,6) /90.0,168.113,90.0,78.113,180.0,309.0565/ !EAST BOT
      Data (hp_rot(2,i),i=1,6) /90.0,191.887,90.0,281.887,0.0,296.0718/  !EAST TOP   
      Data (hp_rot(3,i),i=1,6) /90.0,11.887,90.0,101.887,0.0,50.9435/    !WEST BOT
      Data (hp_rot(4,i),i=1,6) /90.0,348.113,90.0,258.113,180.0,63.9882/ !WEST TOP   
      
      Data  ap_xyz(1,1)/-32.5189/,ap_xyz(1,2)/-48.668/,ap_xyz(1,3)/0.0/ !EA_0
      Data  ap_xyz(2,1)/-48.668/,ap_xyz(2,2)/-32.5189/,ap_xyz(2,3)/0.0/ !EA_1
      Data  ap_xyz(3,1)/-57.4078/,ap_xyz(3,2)/-11.419/,ap_xyz(3,3)/0.0/ !EA_2
      Data  ap_xyz(4,1)/-57.4078/,ap_xyz(4,2)/11.419/,ap_xyz(4,3)/0.0/ !EA_3
      Data  ap_xyz(5,1)/-48.668/,ap_xyz(5,2)/32.5189/,ap_xyz(5,3)/0.0/ !EA_4
      Data  ap_xyz(6,1)/-32.5189/,ap_xyz(6,2)/48.668/,ap_xyz(6,3)/0.0/ !EA_5

      Data  ap_xyz(7,1)/32.5189/,ap_xyz(7,2)/-48.668/,ap_xyz(7,3)/0.0/ !WE_0
      Data  ap_xyz(8,1)/48.668/,ap_xyz(8,2)/-32.5189/,ap_xyz(8,3)/0.0/ !WE_1
      Data  ap_xyz(9,1)/57.4078/,ap_xyz(9,2)/-11.419/,ap_xyz(9,3)/0.0/ !WE_2
      Data  ap_xyz(10,1)/57.4078/,ap_xyz(10,2)/11.419/,ap_xyz(10,3)/0.0/ !WE_3
      Data  ap_xyz(11,1)/48.668/,ap_xyz(11,2)/32.5189/,ap_xyz(11,3)/0.0/ !WE_4
      Data  ap_xyz(12,1)/32.5189/,ap_xyz(12,2)/48.668/,ap_xyz(12,3)/0.0/ !WE_5

      Data  g_xyz(1,1)/-31.6425/,g_xyz(1,2)/-47.3564/,g_xyz(1,3)/0.0/ !EA_0
      Data  g_xyz(2,1)/-47.3564/,g_xyz(2,2)/-31.6425/,g_xyz(2,3)/0.0/ !EA_1
      Data  g_xyz(3,1)/-55.8617/,g_xyz(3,2)/-11.1112/,g_xyz(3,3)/0.0/ !EA_2
      Data  g_xyz(4,1)/-55.8617/,g_xyz(4,2)/11.1112/,g_xyz(4,3)/0.0/ !EA_3
      Data  g_xyz(5,1)/-47.3564/,g_xyz(5,2)/31.6425/,g_xyz(5,3)/0.0/ !EA_4
      Data  g_xyz(6,1)/-31.6425/,g_xyz(6,2)/47.3564/,g_xyz(6,3)/0.0/ !EA_5
      
      Data  g_xyz(7,1)/31.6425/,g_xyz(7,2)/-47.3564/,g_xyz(7,3)/0.0/ !WE_0
      Data  g_xyz(8,1)/47.3564/,g_xyz(8,2)/-31.6425/,g_xyz(8,3)/0.0/ !WE_1
      Data  g_xyz(9,1)/55.8617/,g_xyz(9,2)/-11.1112/,g_xyz(9,3)/0.0/ !WE_2
      Data  g_xyz(10,1)/55.8617/,g_xyz(10,2)/11.1112/,g_xyz(10,3)/0.0/ !WE_3
      Data  g_xyz(11,1)/47.3564/,g_xyz(11,2)/31.6425/,g_xyz(11,3)/0.0/ !WE_4
      Data  g_xyz(12,1)/31.6425/,g_xyz(12,2)/47.3564/,g_xyz(12,3)/0.0/ !WE_5

      Data el_xyz(1,1)/-33.2412/,el_xyz(1,2)/-49.7489/,el_xyz(1,3)/0.0/
      Data el_xyz(2,1)/-49.7489/,el_xyz(2,2)/-33.2412/,el_xyz(2,3)/0.0/
      Data el_xyz(3,1)/-58.682/,el_xyz(3,2)/-11.6727/,el_xyz(3,3)/0.0/
      Data el_xyz(4,1)/-58.682/,el_xyz(4,2)/11.6727/,el_xyz(4,3)/0.0/
      Data el_xyz(5,1)/-49.7489/,el_xyz(5,2)/33.2412/,el_xyz(5,3)/0.0/
      Data el_xyz(6,1)/-33.2412/,el_xyz(6,2)/49.7489/,el_xyz(6,3)/0.0/

      Data el_xyz(7,1)/33.2412/,el_xyz(7,2)/-49.7489/,el_xyz(7,3)/0.0/
      Data el_xyz(8,1)/49.7489/,el_xyz(8,2)/-33.2412/,el_xyz(8,3)/0.0/
      Data el_xyz(9,1)/58.682/,el_xyz(9,2)/-11.6727/,el_xyz(9,3)/0.0/
      Data el_xyz(10,1)/58.682/,el_xyz(10,2)/11.6727/,el_xyz(10,3)/0.0/
      Data el_xyz(11,1)/49.7489/,el_xyz(11,2)/33.2412/,el_xyz(11,3)/0.0/
      Data el_xyz(12,1)/33.2412/,el_xyz(12,2)/49.7489/,el_xyz(12,3)/0.0/

      Data (ap_rot(1,i),i=1,6) /90.0,326.25,0.0,326.25,90.0,236.25/ !EAST_0
      Data (ap_rot(2,i),i=1,6) /90.0,303.75,0.0,303.75,90.0,213.75/ !EAST_1
      Data (ap_rot(3,i),i=1,6) /90.0,281.25,0.0,281.25,90.0,191.25/ !EAST_2
      Data (ap_rot(4,i),i=1,6) /90.0,258.75,0.0,258.75,90.0,168.75/ !EAST_3
      Data (ap_rot(5,i),i=1,6) /90.0,236.25,0.0,236.25,90.0,146.25/ !EAST_4
      Data (ap_rot(6,i),i=1,6) /90.0,213.75,0.0,213.75,90.0,123.75/ !EAST_5

      Data (ap_rot(7,i),i=1,6) /90.0,33.75,0.0,33.75,90.0,303.75/ !WEST_0
      Data (ap_rot(8,i),i=1,6) /90.0,56.25,0.0,56.25,90.0,326.25/ !WEST_1
      Data (ap_rot(9,i),i=1,6) /90.0,78.75,0.0,78.75,90.0,348.75/ !WEST_2
      Data (ap_rot(10,i),i=1,6) /90.0,101.25,0.0,101.25,90.0,11.25/ !WEST_3
      Data (ap_rot(11,i),i=1,6) /90.0,123.75,0.0,123.75,90.0,33.75/ !WEST_4
      Data (ap_rot(12,i),i=1,6) /90.0,146.25,0.0,146.25,90.0,56.25/ !WEST_5

C===============================================================================
C    The following data statements fill variables used by GSDET and/or GSDETH
C===============================================================================
      Data namesv /'HALL', 'HBDG', 'TISO', '    ', '    '/
      Data nbitsv /7*4/             ! Only 8 sectors for HBD sensitive gas + HBD CsI, so 4 bits is plenty.
      Data hitnames / 'X1  ', 'Y1  ', 'Z1  ', 'PX  ','PY  ', 'PZ  ',
     >   'TOF ', 'P_ID', 'X2  ', 'Y2  ', 'Z2  ', 'DELE', 'DSTP' /     
      Data hitbits/13*32/          ! Allow 32 bits for storing each hit value
      Data origin /3*1000., 3*25., 0., 0., 3*1000., 2*0. /
      Data factor /3*1000., 3*1.e4, 100., 1., 3*1000.,1.e7, 1000. /

c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

      write( *,* ) 'hbd - initialising geometry'

C===============================================================================
C    Variables for phnx.par--default values (in cm)
C===============================================================================
      shade_in = 1
      shade_height = 2.5
      shade_spacing_y = 2.5
      shade_spacing_x = 2.5
      hbdg_rin =4.5           ! input radius of the HBD
      hbdg_rout = 66          ! output radius of the HBD
      hbdg_dz =32.82          ! Z dimension of the HBD
      mylar_th = 0.000001     ! mylar thickness
      g10_th = 0.0250         ! G10 thickness, sandwich
      g10_fr_x = 1.905        ! G10 frame X thickness
      g10_fr_y = 1.905        ! G10 frame Y thickness
      g10_fr_z = 0.7          ! G10 frame Z thickness
      g10_fr_1 = 46.945       ! G10 frame # 1 + 0.0 mm
      g10_fr_2 = 16.5         ! G10 frame # 2 + 0.0 mm
      g10_fr_3 = 24.8         ! G10 frame # 3
      g10_fr_4 = 24.3         ! G10 frame # 4
      g10_fr_5 = 24.3         ! G10 frame # 5
      g10_fr_6 = 24.3         ! G10 frame # 6
      g10_fr_7 = 13.0         ! G10 frame # 7
      g10_fr_8 = 27.745       ! G10 frame # 8
      g10_fr_9 = 42.0         ! G10 frame # 9
      g10_fr_10 = 57.59       ! G10 frame # 10
      hex_th = 1.905          ! Honeycomb thickness
      mylar_w_th = 0.01       ! Mylar window thickness
      nitr_th = 1.805         ! Nitrogen window thickness: 3/4 of inch minus 1 mm
      g10_gem_fr = 1.0        ! G10 GEM frame
      g10_electr = 5.0        ! Place for the electronics
      gem_size = 25.0         ! GEM active area
      gem_spacer = 0.6        ! Sum of the 4 spacers
      copper_th = 0.0005      ! Copper thickness
      gem_dist = 0.15         ! Distance between GEM layers
      capton_th = 0.005       ! Capton thickness
      gem_box = 0.6           ! GEM box thickness
      hbdg_gas = 'CF4'        ! Drift/radiator gas choice
      hbd_westarm = 1         ! Indicate if west arm is existing
      hbd_eastarm = 1         ! Indicate if east arm is existing
      hbd_electronics = 1     ! Indicate if electronics is existing
      hbd_cf4gas = 1          ! Indicate if gas is cf4 (cf4->1,air->0)
      hbd_covers = 1          ! Indicate if covers are existing
      hbd_hvpanels = 1        ! Indicate if HV panels are existing
      hbd_walls = 1           ! Indicate if walls are existing
      hbd_window = 1          ! Indicate if window is existing
      hbd_window_support =1   ! Indicate if window support is existing
      hbd_gem_t = 1
      hbd_gem_m = 1
      hbd_gem_b = 1
      hbd_gem_frames = 1
      hbd_csi = 1
      hbd_apanels = 1
      
C===============================================================================
C                             Read the geometry file segment
C===============================================================================

      write( *,* ) 'hbd - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = hbd_par, err = 999 )

C===============================================================================
C    ARM Selection (It happened during Run7. West was taken out in April, 2007)
C     by T. Sakaguchi. Oct 4, 2007.
C===============================================================================

      if(hbd_westarm.eq.1) then
         endarm= 1
      else
         endarm= 0
      endif

      if(hbd_eastarm.eq.1) then
         startarm= 0
      else
         startarm= 1
      endif

C===============================================================================
C    Move all materials definitions to a separate routine, hbd_trk_media, and call it here.
C    The radiator gas is read in from 'phnx.par'.  The two choices currently implemented 
C    are 'CH4' and 'CF4'.
C===============================================================================
      call hbd_trk_media(hbdg_gas)
C===============================================================================
C    Turn on absorption of Cerenkov photons in the gas, CsI, G10, and mylar
C===============================================================================
      call gstpar(ngas,'LABS',1.)
      call gstpar(ngas_act,'LABS',1.)
      call gstpar(ngas_csi,'LABS',1.)
      call gstpar(nfee_csi,'LABS',1.)
      call gstpar(ngas_act_csi,'LABS',1.)
      call gstpar(ncsi,'LABS',1.)
      call gstpar(nmylar,'LABS',1.)
C===============================================================================
C    Create zebra bank storage for phnx.par parameters:
C===============================================================================
      CHFORM = '3F'                              ! 3 phnx.par parameters, all floating point
      call mzform('PARA',CHFORM,ioq) ! book characteristic
C===============================================================================
C    Write the parameters to a zebra bank. Later they will go to output file.
C    3 phnx.par parameters
C===============================================================================
      call mzbook(ixdiv_fr, lfh_PARA, lfh_PARA, 1,
     &            'PARA', 0, 0, 10, ioq, 0)
C===============================================================================
C    Fill the bank
C===============================================================================
      qf(lfh_para + 1) = hbdg_rin
      iPoint = 1
      qf(lfh_para + iPoint + 1) = hbdg_rout
      qf(lfh_para + iPoint + 2) = hbdg_dz
      iPoint = iPoint + 2
C===============================================================================
C    End ZEBRA storage
C    Only book volumes if input parameters are OK
C    Need to keep this general 'if' statement to check HBD line in pisa.kumac.
C===============================================================================
      IF(CVOLU_OPT(1,15).EQ.'FULL')THEN

         set_id = 'HBD '        ! put it in a SET
         NH = 8
         idtype = 2015          ! this number is used in gustep
         nwpa = 990             ! initial size of HITS banks
         nwsa = 2990            ! initial size of DIGI banks
C===============================================================================
C    Creation and positioning of volumes
C    Block HBDG is just a mylar tube for General HBD volume
C===============================================================================
         par(1) = hbdg_rin       !  Rmin
         par(2) = hbdg_rout      !  Rmax
         par(3) = hbdg_dz+0.1    !  Dz

         call gsvolu('HBDG', 'TUBE', nair, par, 3, nvol )
         CALL gsatt('HBDG', 'SEEN', 1)
         CALL gsatt('HBDG', 'COLO', 1)

         pos(1) = 0.0
         pos(2) = 0.0
         pos(3) = 0.0

         call gspos('HBDG', 1, 'HALL', pos(1), pos(2),
     &     pos(3), 0, 'ONLY' )

C===============================================================================
C    GAS VOLUME
C===============================================================================

!  Don't place East arm CF4 if disabled by a switch in phnx.par
!    Switch is "hbd_eastarm"

         if(hbd_eastarm.eq.1) then
           par(1) = 5.0
           par(2) = 59.5325 !58.5325+1.0
           par(3) = 31.5
           par(4) = 90.0-0*180
           par(5) = 270.0-0*180

           if(hbd_cf4gas.eq.1) then
             call gsvolu('GAZ0', 'TUBS', ngas, par, 5, nvol )
           else
             call gsvolu('GAZ0', 'TUBS', nair, par, 5, nvol )
           endif
           
           call gsatt ('GAZ0', 'SEEN', 1 )
           call gsatt ('GAZ0', 'COLO', 2 )

           pos(1) = 0.0
           pos(2) = 0.0
           pos(3) = 0.0
           
           call gspos('GAZ0', 1, 'HBDG', pos(1), pos(2),
     &       pos(3), 0, 'ONLY' )
         endif
         
!  Don't place West arm CF4 if disabled by a switch in phnx.par
!    Switch is "hbd_westarm"

         if(hbd_westarm.eq.1) then
           par(1) = 5.0
           par(2) = 59.5325 !58.5325+1.0
           par(3) = 31.5
           par(4) = 90.0-1*180
           par(5) = 270.0-1*180
           
           if(hbd_cf4gas.eq.1) then
             call gsvolu('GAZ1', 'TUBS', ngas, par, 5, nvol )
           else
             call gsvolu('GAZ1', 'TUBS', nair, par, 5, nvol )
           endif
           
           call gsatt ('GAZ1', 'SEEN', 1 )
           call gsatt ('GAZ1', 'COLO', 2 )
           
           pos(1) = 0.0
           pos(2) = 0.0
           pos(3) = 0.0
           
           call gspos('GAZ1', 1, 'HBDG', pos(1), pos(2),
     &       pos(3), 0, 'ONLY' )
         endif
         
C===============================================================================
C    Rotations
C===============================================================================         
         irot_hbd=irot+1
         irot=irot+50  !reserve 50 rotations for hbd
         
         call gsrotm(irot_hbd,90.0,90.0,0.0,90.0,90.0,0.0)       !1
         call gsrotm(irot_hbd+21,90.0,90.0,0.0,90.0,90.0,180.0)  !2
         call gsrotm(irot_hbd+35,0.0,0.0,90.0,0.0,90.0,90.0)     !36

         do i = 0, 1 !arm
           do j = 0, 1 
             sect=i*2.+j+1
             call gsrotm(irot_hbd+sect, 90.+180.*j, 90.,           !6
     &         90.,180.-i*180., 0., 90.) !wall
           enddo
           do j = 0, 1 !hvpanel
             sect=i*2.+j+1
             call gsrotm(irot_hbd+4+sect,hp_rot(sect,1),           !10
     &         hp_rot(sect,2),hp_rot(sect,3),hp_rot(sect,4),
     &         hp_rot(sect,5),hp_rot(sect,6)) !hvpanel
           enddo
           do j = 0, 5 !apanel
             sect=6*i+j+1                                                   
             call gsrotm(irot_hbd+8+sect,ap_rot(sect,1),           !22
     &         ap_rot(sect,2),ap_rot(sect,3),ap_rot(sect,4),
     &         ap_rot(sect,5),ap_rot(sect,6)) !apanel
             call gsrotm(irot_hbd+21+sect,180.,
     &         ap_rot(sect,2)+90.,90.0,ap_rot(sect,2)+90.0,
     &         90.0,ap_rot(sect,2))
           enddo
         enddo

         hbdg_apanel_length = 63./2.
         hbdg_apanel_width1 = 22.9/2.
         hbdg_apanel_width2 = 23.67/2.
         hbdg_apanel_thickness = 1.955/2.
         hbdg_pcbcu_thickness=0.0005
         
         hbdg_hvpanel_length = 63./2.
         hbdg_hvpanel_width1 = 20.6/2.
         hbdg_hvpanel_width2 = 23.37/2.
         hbdg_hvpanel_thickness = 1.955/2     

         hbdg_wall_length = 63./2.
         hbdg_wall_width1 = 51.68/2.
         hbdg_wall_width2 = 54.81/2.
         hbdg_wall_thickness = 1.955/2.
             
C===============================================================================
C    Covers: G10 frame, honeycomb, 2 G10 sheets, 5mkm Cu on the outer side
C===============================================================================
         if(hbd_covers.eq.1) then
           do i = startarm, endarm !arm
             do j = 0, 1 !attitude
               par(1) = 10.001/2.
               par(2) = 59.0
               par(3) = 1.32/2.
               par(4) = 90.0-i*180
               par(5) = 270.0-i*180
               
               name='CG'//armnum(i+1)//armnum(j+1)
               call gsvolu(name, 'TUBS', nfee_csi, par, 5, nvol )
               call gsatt (name, 'SEEN', 1 )
               call gsatt (name, 'COLO', 4 )
               
               pos(1) = -0.3+0.6*i
               pos(2) = 0.0
               pos(3) = (63/2.+1.32/2.)-2*(63/2.+1.32/2.)*j

               call gspos(name, 1, 'HBDG', pos(1), pos(2),
     &           pos(3), 0, 'ONLY' )
               
               par(1) = 10.00/2.+hbdg_apanel_thickness
               par(2) = 59.0
               par(3) = 1.32/2.-g10_th
               par(4) = 90.0-i*180
               par(5) = 270.0-i*180
               
               name2='CU'//armnum(i+1)//armnum(j+1)
               call gsvolu(name2, 'TUBS', nhexcell, par, 5, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 3 )
               
               pos(1)=0.0
               pos(2)=0.0
               pos(3)=0.0

               call gspos(name2, 1,name, pos(1), pos(2),
     &           pos(3), 0,'ONLY')
               
               par(1) = 10.001/2.
               par(2) = 59.0
               par(3) = hbdg_pcbcu_thickness/2.
               par(4) = 90.0-i*180
               par(5) = 270.0-i*180
               
               name2='TC'//armnum(i+1)//armnum(j+1)
               call gsvolu(name2, 'TUBS', ncopper, par, 5, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 5 )
               
               pos(1) = 0.0
               pos(2) = 0.0
               pos(3) = (+1.32/2.+hbdg_pcbcu_thickness/2.)-2.0*j*
     &           (+1.32/2.+hbdg_pcbcu_thickness/2.)
               sect=i*2.+j+1

               call gspos(name2, 1 ,name, pos(1), pos(2),
     &           pos(3), 0, 'ONLY' )
             enddo
           enddo
             
           do i = startarm, endarm !arm
             do k = 0, 1 !side
               do j = 0, 5 !apanel         
                 sect=6*i+j+1
                 name='C'//attitudenum(k+1)//armnum(i+1)//sectnum(j+1)          
                 
                 par(1) = hbdg_apanel_width1
                 par(2) = hbdg_apanel_width2
                 par(3) = 1.32/2.
                 par(4) = hbdg_apanel_thickness
                 
                 pos(1) = ap_xyz(sect,1)
                 pos(2) = ap_xyz(sect,2)
                 pos(3) = (63/2.+1.32/2.)-2*(63/2.+1.32/2.)*k
                 
                 call gsvolu(name, 'TRD1', nfee_csi, par, 4, nvol )
                 call gsatt (name, 'SEEN', 1)
                 call gsatt (name, 'COLO', 2)

                 call gspos(name, 1, 'HBDG', pos(1), pos(2),
     &             pos(3),irot_hbd+8+sect, 'ONLY')
               enddo
             enddo
           enddo
           
           do i = startarm, endarm !arm
             do k = 0, 1 !side 
               do j = 0, 1 !attitude
                 name='V'//attitudenum(k+1)//attitudenum(j
     &            +1)//armnum(i+1)
                 sect=i*2.+j+1
                                  
                 par(1) = 1.32/2.
                 par(2) = 0.
                 par(3) = 0.
                 par(4) = hbdg_hvpanel_thickness
                 par(5) = hbdg_hvpanel_width2
                 par(6) = hbdg_hvpanel_width1
                 par(7) = 26.5
                 par(8) = hbdg_hvpanel_thickness
                 par(9) = hbdg_hvpanel_width2
                 par(10) = hbdg_hvpanel_width1
                 par(11) = 26.5
                 
                 call gsvolu(name, 'TRAP', nfee_csi, par, 11, nvol )
                 call gsatt (name, 'SEEN', 1 )
                 call gsatt (name, 'COLO', 2 )
                 
                 pos(1) = hp_xyz(sect,1)
                 pos(2) = hp_xyz(sect,2)
                 pos(3) = (63/2.+1.32/2.)-2*(63/2.+1.32/2.)*k

                 call gspos(name, 1, 'HBDG', pos(1), pos(2),
     &             pos(3), irot_hbd+4+sect, 'ONLY' )

                 name2='CG'//armnum(i+1)//armnum(k+1)
                 name='W'//attitudenum(k+1)//attitudenum(j+1)//armnum(i
     &            +1)  
                 sect =2*i+j+1
                 
                 par(1) = 1.32/2.
                 par(2) = 0.
                 par(3) = 0.
                 par(4) = hbdg_wall_thickness
                 par(5) = hbdg_wall_width2
                 par(6) = hbdg_wall_width1
                 par(7) = 22.63
                 par(8) = hbdg_wall_thickness
                 par(9) = hbdg_wall_width2
                 par(10) = hbdg_wall_width1
                 par(11) = 22.63

                 call gsvolu(name, 'TRAP', nfee_csi, par, 11, nvol )
                 call gsatt (name, 'SEEN', 1 )
                 call gsatt (name, 'COLO', 2 )
                 
                 pos(1) = -hbdg_wall_thickness+2.*i*
     &             hbdg_wall_thickness
                 pos(2) = -33.1+33.1*2.*j
                 pos(3) = 0.0

                 call gspos(name, 1,name2, pos(1), pos(2),
     &             pos(3), irot_hbd+sect, 'ONLY' )
                 
               enddo
             enddo
           enddo
         endif
C===============================================================================
C    HV panel
C===============================================================================
           do i = startarm, endarm !arm
             do j = 0, 1 !attitude
               name='HV'//attitudenum(j+1)//armnum(i+1)
               name2='GAZ'//armnum(i+1)
               sect=i*2.+j+1
               
               par(1) = hbdg_hvpanel_length
               par(2) = 0.
               par(3) = 0.
               par(4) = hbdg_hvpanel_thickness
               par(5) = hbdg_hvpanel_width2
               par(6) = hbdg_hvpanel_width1
               par(7) = 26.5
               par(8) = hbdg_hvpanel_thickness
               par(9) = hbdg_hvpanel_width2
               par(10) = hbdg_hvpanel_width1
               par(11) = 26.5
               
               call gsvolu(name, 'TRAP', nfee_csi, par, 11, nvol )
               call gsatt (name, 'SEEN', 1 )
               call gsatt (name, 'COLO', 4 )
               
               pos(1) = hp_xyz(sect,1)
               pos(2) = hp_xyz(sect,2)
               pos(3) = hp_xyz(sect,3)

               if(hbd_hvpanels.eq.1) then
                 call gspos(name, 1, name2, pos(1), pos(2),
     &             pos(3), irot_hbd+4+sect, 'ONLY' )
               endif
               
               par(1) = hbdg_hvpanel_length-g10_fr_z
               par(2) = 0.
               par(3) = 0.
               par(4) = hbdg_hvpanel_thickness-g10_th
               par(5) = hbdg_hvpanel_width2-g10_th
               par(6) = hbdg_hvpanel_width1-g10_th
               par(7) = 26.5
               par(8) = hbdg_hvpanel_thickness-g10_th
               par(9) = hbdg_hvpanel_width2-g10_th
               par(10) = hbdg_hvpanel_width1-g10_th
               par(11) = 26.5
               
               name2='HO'//attitudenum(j+1)//armnum(i+1)
               
               call gsvolu(name2, 'TRAP', nhexcell, par, 11, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 1 )
               
               pos(1) = 0.0
               pos(2) = 0.0
               pos(3) = 0.0
               
               if(hbd_hvpanels.eq.1) then
                 call gspos(name2,sect,name,pos(1),pos(2),
     &             pos(3),0,'ONLY')                
               endif    
             enddo
           enddo
           
           
C===============================================================================
C    Window G10 part
C===============================================================================
                  
           do i = startarm, endarm !arm  
             par(1) = 10.001/2.
             par(2) = 13.81/2.
             par(3) = 63./2.
             par(4) = 90.0-i*180 
             par(5) = 270.0-i*180
           
             name='WGT'//armnum(i+1)
             name2='GAZ'//armnum(i+1)
             call gsvolu(name, 'TUBS', nfee_csi, par, 5, nvol )
             call gsatt (name, 'SEEN', 1 )
             call gsatt (name, 'COLO', 3 )
             
             pos(1) = -0.3+0.6*i
             pos(2) = 0.0
             pos(3) = 0.0

             if(hbd_window_support.eq.1) then
               call gspos(name, 1, name2, pos(1), pos(2),
     &           pos(3), 0, 'ONLY' )
             endif
             
C===============================================================================
C    "Window" (air) of G10 window
C===============================================================================
             par(1) = 10.00/2.
             par(2) = 5.59998
             par(3) = 58.2/2.
             par(4) = 113.0-i*180
             par(5) = 247.0-i*180
             
             name2='WGT'//armnum(i+1)
             name='WJN'//armnum(i+1)
             call gsvolu(name, 'TUBS', nair, par, 5, nvol )
             
             call gsatt (name, 'SEEN', 1 )
             call gsatt (name, 'COLO', 1 )
             
             pos(1) = 0.0
             pos(2) = 0.0
             pos(3) = 0.0

             if(hbd_window_support.eq.1) then
               call gspos(name, 1,name2, pos(1), pos(2),
     &           pos(3), 0, 'ONLY' )
             endif
             
C===============================================================================
C    "Window" (cf4) of G10 window
C===============================================================================
             par(1) = 5.6128
             par(2) = 13.81/2.+0.1
             par(3) = 58.2/2.
             par(4) = 113.0-i*180
             par(5) = 247.0-i*180
             
             name2='WGT'//armnum(i+1)
             name='WIN'//armnum(i+1)
             
             if(hbd_cf4gas.eq.1) then
               call gsvolu(name, 'TUBS', ngas, par, 5, nvol )
             else
               call gsvolu(name, 'TUBS', nair, par, 5, nvol )
             endif
             
             call gsatt (name, 'SEEN', 1 )
             call gsatt (name, 'COLO', 1 )
             
             pos(1) = 0.0
             pos(2) = 0.0
             pos(3) = 0.0

             if(hbd_window_support.eq.1) then
               call gspos(name, 1,name2, pos(1), pos(2),
     &           pos(3), 0, 'ONLY' )
             endif
             
C===============================================================================
C    Mylar Window 
C===============================================================================
             
             if(hbd_window.eq.1) then             
               par(1) = 5.6
               par(2) = 5.6127
               par(3) = 63./2.
               par(4) = 90.0-i*180
               par(5) = 270.0-i*180
               
               name2='GAZ'//armnum(i+1)
               name='WMY'//armnum(i+1)	
               call gsvolu(name, 'TUBS', nmylar, par, 5, nvol )
               call gsatt (name, 'SEEN', 1 )
               call gsatt (name, 'COLO', 4 )
               
               pos(1) = -0.3+0.6*i
               pos(2) = 0.0
               pos(3) = 0.0

               call gspos(name, 1, name2, pos(1), pos(2),
     &           pos(3), 0, 'ONLY' )
               
C===============================================================================
C    Mylar Window Aluminium 
C===============================================================================
               par(1) = 5.59999
               par(2) = 5.6
               par(3) = 63./2.
               par(4) = 90.0-i*180
               par(5) = 270.0-i*180
               
               name='WAL'//armnum(i+1)	
               call gsvolu(name, 'TUBS', naluminium, par, 5, nvol )
               call gsatt (name, 'SEEN', 1 )
               call gsatt (name, 'COLO', 6 )
               
               pos(1) = -0.3+0.6*i
               pos(2) = 0.0
               pos(3) = 0.0

               call gspos(name, 1, name2, pos(1), pos(2),
     &           pos(3), 0, 'ONLY' )
             endif
           enddo        
           
*********************************************************************
*   GSM 
*********************************************************************
         do i = startarm, endarm !arm
           do j = 0, 5 !apanel         
             sect=6*i+j+1
             
             par ( 1) = 26.74+0.5
             par ( 2) = 0.6
             par ( 3) = hbdg_apanel_width2

             name='GAZ'//armnum(i+1)
             name2='XY'//armnum(i+1)//sectnum(j+1)

             if(hbd_cf4gas.eq.1) then
               call gsvolu(name2, 'BOX ', ngas_csi, par, 3, nvol )
             else
               call gsvolu(name2, 'BOX ', nair, par, 3, nvol )
             endif
               
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 5 )
             
             pos(1) = g_xyz(sect,1)
             pos(2) = g_xyz(sect,2)
             pos(3) = g_xyz(sect,3)

             call gspos(name2,1,name,pos(1),pos(2),
     &         pos(3),irot_hbd+21+sect,'ONLY')
             
C===============================================================================     
C     ACTIVE GAS LAYER
C===============================================================================
             par ( 1) = 26.74/2.
             par ( 2) = 0.015/2.
             par ( 3) = 22.0/2.
             
             name='XY'//armnum(i+1)//sectnum(j+1)
             name2='PN'//armnum(i+1)//sectnum(j+1)

             if(hbd_cf4gas.eq.1) then
               call gsvolu(name2, 'BOX ', ngas_act_csi, par, 3, nvol )
             else
               call gsvolu(name2, 'BOX ', nair, par, 3, nvol )
             endif
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 4 )
             
             pos(1) = 13.695
             pos(2) = -(-hbdg_apanel_thickness-capton_th-
     &         hbdg_pcbcu_thickness-0.45-capton_th-
     &         2*copper_th-0.015/2.)+0.001*2-
     &         0.6-hbdg_apanel_thickness
             pos(3) = 0.0

             cn=2*sect
             call gspos(name2,cn,name,pos(1),pos(2),pos(3),0,'ONLY')
             
             par ( 1) = 26.74/2.
             par ( 2) = 0.015/2.
             par ( 3) = 22.0/2.
             
             name2='PS'//armnum(i+1)//sectnum(j+1)

             if(hbd_cf4gas.eq.1) then
               call gsvolu(name2, 'BOX ', ngas_act_csi, par, 3, nvol )
             else
               call gsvolu(name2, 'BOX ', nair, par, 3, nvol )
             endif
               
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 4 )
             
             pos(1) = -13.695
             pos(2) = -(-hbdg_apanel_thickness-capton_th-
     &         hbdg_pcbcu_thickness-0.45-capton_th-
     &         2*copper_th-0.015/2.)+ 0.001*2-
     &         0.6-hbdg_apanel_thickness
             pos(3) = 0.0

             cn=2*sect+1
             call gspos(name2,cn,name,pos(1),pos(2),pos(3),0,'ONLY')
            
C===============================================================================     
C     CSI 
C     At the moment thickness of CsI layer 2 mkm instead of 200 nm
C===============================================================================             
             par ( 1) = 26.74/2.
             par (2) = 0.001
!            par ( 2) = 0.00001!200 nm /2.
             par ( 3) = 22.0/2.

             name='XY'//armnum(i+1)//sectnum(j+1)
             name2='QN'//armnum(i+1)//sectnum(j+1)

             if(hbd_csi.eq.1) then
               call gsvolu(name2, 'BOX ', ncsi, par, 3, nvol )
             else
               if(hbd_cf4gas.eq.1) then
                 call gsvolu(name2, 'BOX ', ngas_act_csi, par, 3, nvol )
               endif
               if(hbd_cf4gas.eq.0) then
                 call gsvolu(name2, 'BOX ', nair, par, 3, nvol )
               endif
             endif
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 6 )
             
             pos(1) = 13.695
             pos(2) = -(-hbdg_apanel_thickness-capton_th-
     &         hbdg_pcbcu_thickness-0.45-capton_th-
     &         2*copper_th)+0.0001-
     &         0.6-hbdg_apanel_thickness
             pos(3) = 0.0
             cn=2*sect

             call gspos(name2,cn,name,pos(1),pos(2),pos(3),0,'ONLY')
             
             par ( 1) = 26.74/2.
             par (2) = 0.001
!            par ( 2) = 0.00001!200 nm /2.
             par ( 3) = 22.0/2.
             
             name2='QS'//armnum(i+1)//sectnum(j+1)

             if(hbd_csi.eq.1) then
               call gsvolu(name2, 'BOX ', ncsi, par, 3, nvol )
             else
               if(hbd_cf4gas.eq.1) then
                 call gsvolu(name2, 'BOX ', ngas_act_csi, par, 3, nvol )
               endif
               if(hbd_cf4gas.eq.0) then
                 call gsvolu(name2, 'BOX ', nair, par, 3, nvol )
               endif
             endif
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 6 )
             
             pos(1) = -13.695
             pos(2) = -(-hbdg_apanel_thickness-capton_th-
     &         hbdg_pcbcu_thickness-0.45-capton_th-
     &         2*copper_th)+0.0001-
     &         0.6-hbdg_apanel_thickness
             pos(3) = 0.0

             cn=2*sect+1
!23/09/07
             call gspos(name2,cn,name,pos(1),pos(2),pos(3),0,'ONLY')
           
**********************************************************************
*  apanel
**********************************************************************          
             if(hbd_apanels.eq.1) then
               name='AP'//armnum(i+1)//sectnum(j+1)             
               
               par(1) = hbdg_apanel_width1
               par(2) = hbdg_apanel_width2
               par(3) = hbdg_apanel_length
               par(4) = hbdg_apanel_thickness
               
               pos(1) = ap_xyz(sect,1)
               pos(2) = ap_xyz(sect,2)
               pos(3) = ap_xyz(sect,3)
               
               call gsvolu(name, 'TRD1', nfee_csi, par, 4, nvol )
               call gsatt (name, 'SEEN', 1)
               call gsatt (name, 'COLO', 4)

               name2='GAZ'//armnum(i+1)
               call gspos(name, 1, name2, pos(1), pos(2),
     &           pos(3),irot_hbd+8+sect, 'ONLY')
               
               name2='HO'//armnum(i+1)//sectnum(j+1)
               
               par(1) = hbdg_apanel_width1-g10_th
               par(2) = hbdg_apanel_width2-g10_th
               par(3) = hbdg_apanel_length-g10_fr_z 
               par(4) = hbdg_apanel_thickness-g10_th
               
               call gsvolu(name2,'TRD1',nhexcell,par,4,nvol )
               call gsatt (name2,'SEEN',1)
               call gsatt (name2,'COLO',1)
               
               pos(1) = 0.0
               pos(2) = 0.0
               pos(3) = 0.0
!23/09/07
               call gspos(name2,sect,name,pos(1),pos(2),pos(3),0,'ONLY')
               
C===============================================================================
C     PCB capton
C===============================================================================
               par ( 1) = hbdg_apanel_length
               par ( 2) = capton_th/2.
               par ( 3) = hbdg_apanel_width1
               
               name2='CA'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = 0.0
               pos(2) = 0.0
               pos(3) = -hbdg_apanel_thickness-capton_th/2.
!23/09/07             
               call gspos(name2,sect,name,pos(1),pos(2),
     &           pos(3),irot_hbd,'ONLY')
               
C===============================================================================
C     PCB Copper
C===============================================================================
               par ( 1) = 26.74/2.
               par ( 2) = hbdg_pcbcu_thickness/2.
               par ( 3) = 22.0/2.
               
               name='AP'//armnum(i+1)//sectnum(j+1)             
               name2='CN'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = 0.0
               pos(2) = 13.695
               pos(3) = -hbdg_apanel_thickness-capton_th-
     &           hbdg_pcbcu_thickness/2.
               
               cn=2*sect
!23/09/07
               call gspos (name2,cn,name,pos(1),pos(2),pos(3),
     &           irot_hbd,'ONLY')
               
               par ( 1) = 26.74/2.
               par ( 2) = hbdg_pcbcu_thickness/2.
               par ( 3) = 22.0/2.
               
               name2='CS'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = 0.0
               pos(2) = -13.695
               pos(3) = -hbdg_apanel_thickness-capton_th-
     &           hbdg_pcbcu_thickness/2.
               
               cn=2*sect+1
!23/09/07
               call gspos (name2,cn,name,pos(1),pos(2),pos(3),
     &           irot_hbd,'ONLY')
             endif
C===============================================================================
C      MOTHERBOARD & PREAMPS
C===============================================================================            
             par ( 1) =(1.+26.74)/2.
             par ( 2) = 0.36/2.
             par ( 3) = (1.+22.0)/2.
             
             name='HBDG'
             name2='IN'//armnum(i+1)//sectnum(j+1)

             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )

             pos(1) = el_xyz(sect,1)
             pos(2) = el_xyz(sect,2)
             pos(3) = el_xyz(sect,3)+13.695
             
             cn=2*sect+1
             
             if(hbd_electronics.eq.1) then
               call gspos(name2,cn,name,pos(1),pos(2),pos(3),
     &           irot_hbd+21+sect,'ONLY' )
             endif
             
             par ( 1) = (1.+26.74)/2.
             par ( 2) = 0.36/2.
             par ( 3) = (1.+22.0)/2.

             name2='IS'//armnum(i+1)//sectnum(j+1)
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             pos(1) = el_xyz(sect,1)
             pos(2) = el_xyz(sect,2)
             pos(3) = el_xyz(sect,3)-13.695

             cn=2*sect+1
             if(hbd_electronics.eq.1) then
               call gspos(name2,cn,name,pos(1),pos(2),pos(3),
     &           irot_hbd+21+sect,'ONLY' )
             endif
                          
C===============================================================================
C     GEM BOTTOM KAPTON
C===============================================================================              
             if(hbd_gem_b.eq.1) then
               par ( 1) = 26.74/2.
               par ( 2) = capton_th/2.
               par ( 3) = 22.0/2.
               
               name='XY'//armnum(i+1)//sectnum(j+1)
               name2='XN'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 3 )
               
               pos(1) = 13.695
               pos(2) = -0.45
               pos(3) = 0.0
               
!23/09/07             
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY')
               
               par ( 1) = 26.74/2.
               par ( 2) = capton_th/2.
               par ( 3) = 22.0/2.
               
               name2='XS'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 3 )
               
               pos(1) = -13.695
               pos(2) = -0.45
               pos(3) = 0.0
               
!23/09/07             
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY')
               
C===============================================================================
C     GEM BOTTOM COPPER
C===============================================================================             
               par ( 1) = 26.74/2.
               par ( 2) = copper_th
               par ( 3) = 22.0/2.
               
               name2='KN'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = 13.695
               pos(2) = -0.45+copper_th+capton_th/2.
               pos(3) = 0.0
!23/09/07
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY' )
               
               par ( 1) = 26.74/2.
               par ( 2) = copper_th
               par ( 3) = 22.0/2.
               
               name2='KS'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = -13.695
               pos(2) = -0.45+copper_th+capton_th/2.
               pos(3) = 0.0
!23/09/07             
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY' )
             endif
C===============================================================================
C     GEM MIDDLE KAPTON
C===============================================================================             
             if(hbd_gem_m.eq.1) then
               par ( 1) = 26.74/2.
               par ( 2) = capton_th/2.
               par ( 3) = 22.0/2.
               
               name='XY'//armnum(i+1)//sectnum(j+1)
               name2='YN'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 4 )
               
               pos(1) = 13.695
               pos(2) = -0.3
               pos(3) = 0.0
!23/09/07             
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY' )
               
               par ( 1) = 26.74/2.
               par ( 2) = capton_th/2.
               par ( 3) = 22.0/2.
               
               name2='YS'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 4 )
               
               pos(1) = -13.695
               pos(2) = -0.3
               pos(3) = 0.0
               
!23/09/07             
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY' )
               
C===============================================================================
C     GEM MIDDLE COPPER
C===============================================================================             
               par ( 1) = 26.74/2.
               par ( 2) = copper_th
               par ( 3) = 22.0/2.
               
               name2='LN'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 6 )
               
               pos(1) = 13.695
               pos(2) = -0.3+copper_th+capton_th/2.
               pos(3) = 0.0
!23/09/07
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY' )
               
               par ( 1) = 26.74/2.
               par ( 2) = copper_th
               par ( 3) = 22.0/2.
               
               name2='LS'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 6 )
               
               pos(1) = -13.695
               pos(2) = -0.3+copper_th+capton_th/2.
               pos(3) = 0.0
!23/09/07
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY' )
             endif
C===============================================================================
C     GEM TOP KAPTON
C===============================================================================             
             if(hbd_gem_t.eq.1) then
               par ( 1) = 26.74/2.
               par ( 2) = capton_th/2.
               par ( 3) = 22.0/2.
               
               name='XY'//armnum(i+1)//sectnum(j+1)
               name2='ZN'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = 13.695
               pos(2) = -0.16
               pos(3) = 0.0
!23/09/07             
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY' )
               
               par ( 1) = 26.74/2.
               par ( 2) = capton_th/2.
               par ( 3) = 22.0/2.
               
               name2='ZS'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = -13.695
               pos(2) = -0.16
               pos(3) = 0.0
!23/09/07             
               call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &           0,'ONLY' )             
               
C===============================================================================
C     GEM TOP COPPER
C===============================================================================             
               par ( 1) = 26.74/2.
               par ( 2) = copper_th
               par ( 3) = 22.0/2.
               
               name2='FN'//armnum(i+1)//sectnum(j+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = 13.695
               pos(2) = -0.16+copper_th+capton_th/2.
               pos(3) = 0.0
               
             call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &         0,'ONLY' )
             
             par ( 1) = 26.74/2.
             par ( 2) = copper_th
             par ( 3) = 22.0/2.
             
             name2='FS'//armnum(i+1)//sectnum(j+1)
             
             call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 2 )
             
             pos(1) = -13.695
             pos(2) = -0.16+copper_th+capton_th/2.
             pos(3) = 0.0

             call gspos(name2,1,name,pos(1),pos(2),pos(3),
     &         0,'ONLY' )
           endif
C===============================================================================
C     G10 GEM frames
C===============================================================================             
           if(hbd_gem_frames.eq.1) then
             par ( 1) = 27.34/2.
             par ( 2) = 0.6/2.
             par ( 3) = 0.5/2.
             
             name='XY'//armnum(i+1)//sectnum(j+1)
             name2='LN1'//volid(sect)
             
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             
             pos(1) = 13.695
             pos(2) = -0.3
             pos(3) = 11.05
             
             call gspos(name2,1,name, pos(1),pos(2),pos(3),
     &         0,'ONLY' )
             
             name2='LN2'//volid(sect)
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             
             pos(1) = 13.695
             pos(2) = -0.3
             pos(3) = -11.05
             
             call gspos(name2,2,name, pos(1),pos(2),pos(3),
     &         0,'ONLY' )
             
             name2='LS1'//volid(sect)
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             
             pos(1) = -13.695
             pos(2) = -0.3
             pos(3) = 11.05

             call gspos(name2,1,name, pos(1),pos(2),pos(3),
     &         0,'ONLY' )
             
             name2='LS2'//volid(sect)
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             
             pos(1) = -13.695
             pos(2) = -0.3
             pos(3) = -11.05

             call gspos(name2,1,name, pos(1),pos(2),pos(3),
     &         0,'ONLY' )
             
             par ( 1) = 22.6/2.
             par ( 2) = 0.5/2.
             par ( 3) = 0.6/2.
             
             name2='TN1'//volid(sect)
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             
             pos(1) = 0.25
             pos(2) = -0.3
             pos(3) = 0.0

             call gspos(name2,1,name, pos(1),pos(2),pos(3),
     &         irot_hbd+35,'ONLY' )
             
             name2='TN2'//volid(sect)
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             
             pos(1) = 27.34-0.25
             pos(2) = -0.3
             pos(3) = 0.0

             call gspos(name2,1,name, pos(1),pos(2),pos(3),
     &         irot_hbd+35,'ONLY' )
             
             name2='TS1'//volid(sect)
             
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             
             pos(1) = -0.25
             pos(2) = -0.3
             pos(3) = 0.0
             
             call gspos(name2,1,name, pos(1),pos(2),pos(3),
     &         irot_hbd+35,'ONLY' )
             
             name2='TS2'//volid(sect)
             call gsvolu(name2, 'BOX ', nfee_csi, par, 3, nvol )
             call gsatt (name2, 'SEEN', 1 )
             call gsatt (name2, 'COLO', 1 )
             
             pos(1) = -27.34+0.25
             pos(2) = -0.3
             pos(3) = 0.0

             call gspos(name2,1,name, pos(1),pos(2),pos(3),
     &         irot_hbd+35,'ONLY' )
           endif             
         enddo
       enddo
       
C===============================================================================
C    Walls 
C===============================================================================
         if(hbd_walls.eq.1) then
           do i = startarm, endarm !arm
             do j = 0, 1 !attitude
               name='WP'//attitudenum(j+1)//armnum(i+1)
               name2='GAZ'//armnum(i+1)
               sect =2*i+j+1
               
               par(1) = hbdg_wall_length
               par(2) = 0.
               par(3) = 0.
               par(4) = hbdg_wall_thickness
               par(5) = hbdg_wall_width2
               par(6) = hbdg_wall_width1
               par(7) = 22.63
               par(8) = hbdg_wall_thickness
               par(9) = hbdg_wall_width2
               par(10) = hbdg_wall_width1
               par(11) = 22.63
               
               call gsvolu(name, 'TRAP', nfee_csi, par, 11, nvol )
               call gsatt (name, 'SEEN', 1 )
               call gsatt (name, 'COLO', 4 )
               
               pos(1) = -hbdg_wall_thickness-0.3+2.*i*
     &           (hbdg_wall_thickness+0.3)
               pos(2) = -33.1+33.1*2.*j
               pos(3) = 0.0
               
               call gspos(name, 1, name2, pos(1), pos(2),
     &           pos(3), irot_hbd+sect, 'ONLY' )
               
               par(1) = hbdg_wall_length-g10_fr_z
               par(2) = 0.
               par(3) = 0.
               par(4) = hbdg_wall_thickness-g10_th
               par(5) = hbdg_wall_width2-g10_th
               par(6) = hbdg_wall_width1-g10_th
               par(7) = 22.63
               par(8) = hbdg_wall_thickness-g10_th
               par(9) = hbdg_wall_width2-g10_th
               par(10) = hbdg_wall_width1-g10_th
               par(11) = 22.63
               
               name2='HW'//attitudenum(j+1)//armnum(i+1)
               
               call gsvolu(name2, 'TRAP', nhexcell, par, 11, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 4 )
               
               pos(1) = 0.0
               pos(2) = 0.0
               pos(3) = 0.0
!23/09/07
               call gspos(name2,sect,name,pos(1),pos(2),pos(3),0,'ONLY')
               
!000000000000000000000000000000000000000000000000000000000000000000000000
!    5 mkm copper layer on the outer side of the vertical walls
!0000000000000000000000000000000000000000000000000000000000000000000000000
               par ( 1) = hbdg_wall_width2
               par ( 2) = hbdg_pcbcu_thickness/2.
               par ( 3) = hbdg_wall_length
               
               name2='HC'//attitudenum(j+1)//armnum(i+1)
               
               call gsvolu(name2, 'BOX ', ncopper, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 2 )
               
               pos(1) = -0.4
               pos(2) = -hbdg_wall_thickness-
     &           hbdg_pcbcu_thickness/2.
               pos(3) = 0.0
               
!23/09/07
               call gspos (name2,sect,name,pos(1),pos(2),pos(3),
     &           0,'ONLY')
             enddo
           enddo
         endif
CTS==============================================================
CTS= Shade is now in. by T. Sakaguchi 2007/07/09
CTS==============================================================
         if( shade_in.ne.1 ) go to 1118

         do i = startarm, endarm !arm
           do j = 0, 5 !apanel         
             sect=6*i+j+1
             
             name='XY'//armnum(i+1)//sectnum(j+1)

             nshade_x = 20.00/shade_spacing_x
             nshade_y = 20.00/shade_spacing_y

             do k = 1, nshade_y ! shading aligning in y-direction

               par ( 1) = 0.01/2.
               par ( 2) = shade_height/2.
               par ( 3) = 20.00/2.
             
               name2='S1'//volid2(sect)//numshade(k)
               call gsvolu(name2, 'BOX ', nmylar, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 1 )

               name2='S2'//volid2(sect)//numshade(k)
               call gsvolu(name2, 'BOX ', nmylar, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 1 )

               pos ( 3) = 0.0
               pos ( 2) = 0.2+shade_height/2.
               pos ( 1) = -13.295-20.00/2.+k*shade_spacing_y

               name2='S1'//volid2(sect)//numshade(k)
!23/09/07
!               call gspos(name2,1,name,pos(1),pos(2),pos(3),0,'MANY' )

               pos ( 3) = 0.0
               pos ( 2) = 0.2+shade_height/2.
               pos ( 1) = +13.295-20.00/2.+k*shade_spacing_y

               name2='S2'//volid2(sect)//numshade(k)
!23/09/07
!               call gspos(name2,1,name,pos(1),pos(2),pos(3),0,'MANY' )
             enddo

             do k = 1, nshade_x ! shading aligning in x-direction
               par ( 1) = 20.00/2.
               par ( 2) = shade_height/2.
               par ( 3) = 0.01/2.
            
               name2='S3'//volid2(sect)//numshade(k)
               call gsvolu(name2, 'BOX ', nmylar, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 1 )

               name2='S4'//volid2(sect)//numshade(k)
               call gsvolu(name2, 'BOX ', nmylar, par, 3, nvol )
               call gsatt (name2, 'SEEN', 1 )
               call gsatt (name2, 'COLO', 1 )

               pos ( 3) = -10.00+k*shade_spacing_x
               pos ( 2) = 0.2+shade_height/2.
               pos ( 1) = -13.295

               name2='S3'//volid2(sect)//numshade(k)
!23/09/07
!               call gspos(name2,1,name,pos(1),pos(2),pos(3),0,'MANY' )

               pos ( 3) = -10.00+k*shade_spacing_x
               pos ( 2) = 0.2+shade_height/2.
               pos ( 1) = 13.295

               name2='S4'//volid2(sect)//numshade(k)
!23/09/07
!               call gspos(name2,1,name,pos(1),pos(2),pos(3),0,'MANY' )

             enddo
           enddo
         enddo

CTS==============================================================
CTS= End of shade.  by T. Sakaguchi 2007/07/09
CTS==============================================================

1118     do k = 0, 3 !det
!           do i = 0, 1 !arm
           do i = startarm, endarm !arm
             do j = 0, 5 !sector
               Hgsm = 'XY'//armnum(i+1)//sectnum(j+1)
               Hdet = detset(k+1)//armnum(i+1)//sectnum(j+1)
               namesv(4) = Hgsm
               namesv(5) = Hdet
               call GSDET(set_id,Hdet,nvhbd,namesv,nbitsv,idtype,
     &           nwpa, nwsa, iset, idet )
               call GSDETH(set_id,Hdet,dettype(k+1),hitnames,
     &           hitbits,origin,factor)
             enddo
           enddo
         enddo
         
        write( *,* ) 'hbd - done'
         
C===============================================================================
       ENDIF
C===============================================================================
 9999 continue
      return

 999  continue
      write(6,1000)
 1000 format(/'hbd - read error in hbd_par segment')
      stop 'hbd - namelist mis-match in hbd_par segment ?'
      end
C===============================================================================
      
