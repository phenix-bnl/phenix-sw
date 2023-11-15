*CMZ :          29/06/95  15.08.32  by  Melynda Brooks
*-- Author :    Charles F. Maguire   27/10/94
c
c    Revised March 18, 1996 to fix IBM linker problem
c	New IBM linker confuses FMLINK common block with fmlink subroutine
c       which is the libpacklib CERN library.  No other UNIX platform (nor
c       the previous IBM linker) has this difficulty.
c       Name of FMLINK common block changed to FPMLINK.
C    Revised Aug. 19, 1996 to add muon magnet namelist variables
c
c    Revised Sept/October '96 to add retaining ring parameters
c    Revised Dec. 16, 1997 to have rounded piston geometry
C    Revised Jan 19, 2001 to include specific station one geometry variables
c    Jan'10 HvH: added dyoke_arc, removed cyoke_flag.
C
C StationOneFrame   = InnerRadius 	= 20.684
C                     OuterRadius 	= 147.317
C                     HoleInnerRadius 	= 17.017
C                     HoleOuterRadius 	= 131.885
C                     Thickness 	= 0.635
C                     HoleXOffset 	= 6.504
C                     HoleYOffset 	= 3.810
C                     FrameOffset 	= 2.537
C StationOnePanel   = InnerRadius 	= 20.380
C                     OuterRadius 	= 142.636
C                     HoneyInnerRadius 	= 19.991
C                     HoneyOuterRadius 	= 139.590
C                     Thickness 	= 1.402
C                     HoneyThickness 	= 1.168
C                     CopperThickness 	= 0.005
C                     PanelOffset 	= 3.536
C StationOneAnode   = HoleInnerRadius 	= 18.406
C                     HoleOuterRadius 	= 134.551
C                     Thickness 	= 0.644
C                     HoleOffset 	= 2.483
C StationOneRead    = AnodeWidth 	= 2.983
C                     AnodeLength 	= 110.429
C                     CathodeThickness 	= 0.107
C                     AnodeXOffset 	= 81.425
C                     AnodeYOffset 	= 2.034
C                     CathodeInnerRadius= 142.635
C                     CathodeOuterRadius= 144.928
C                     CathodeOffset 	= 3.535
C StationOneRib     = 22RibWidth 	= 0.635
C                     22RibLength 	= 107.798
C                     22RibXOffset 	= 79.430
C                     22RibYOffset 	= 32.740
C                     45RibWidth 	= 1.499
C                     45RibLength 	= 107.798
C                     45RibOffset 	= 60.453
C StationOneGas     = SmallInnerRadius	= 15.579
C                     SmallOuterRadius	= 117.577
C                     SmallXOffset 	= 15.354
C                     SmallYOffset 	= 6.018
C                     LargeInnerRadius 	= 29.070
C                     LargeOuterRadius 	= 131.071
C                     LargeXOffset 	= 2.397
C                     LargeYOffset 	= 1.339
C StationOneMount   = Thickness 	= 1.270
C                     ZOffset 		= 2.540
C                     InnerRadius 	= 23.500
C                     OuterRadius 	= 152.300
C                     HoleInnerRadius	= 25.501
C                     HoleOuterRadius 	= 135.328
C                     HoleOffset 	= 5.080
C StationOneFee     = Thickness 	= 1.000
C                     InnerRadius 	= 22.500
C                     OuterRadius 	= 162.500
C                     AnnulusOuterRadius= 32.500
C                     HoleLength 	= 87.345
C                     HoleWidth 	= 7.938
C                     HoleXOffset 	= 77.752
C                     BoxWidth 		= 22.300
C                     BoxLength 	= 26.848
C                     BoxHeight 	= 13.401
C StationTwoFFrame  = InnerRadius 	= 64.000	65.595
C                     OuterRadius 	= 265.949	227.379
C                     HoleInnerRadius 	= 52.900	53.950
C                     HoleOuterRadius 	= 239.205	199.899
C                     Thickness 	= 2.841		2.841
C                     FrameXOffset 	= -9.174	-9.174
C                     FrameYOffset 	= -3.800	-3.800
C StationTwoBFrame  = OuterRadius 	= 275.379	236.473
C                     HoleOuterRadius 	= 248.021	208.993
C StationTwoFBar    = BarInnerRadius    = 152.190	138.959
C                     BarOuterRadius    = 154.092	141.181
C StationTwoBBar    = BarInnerRadius    = 162.164	143.959
C                     BarOuterRadius    = 164.069	146.181
C StationTwoAnode   = Thickness 	= .15875	.15875
C StationTwoFoilThickness               = .005
C StationTwoAlFoilThickness               = .005
C StationTwoFRib    = RibWidth 		= 0.500		0.500
C                     RibLength 	= 188.119	147.654
C                     RibXOffset 	= 135.435	118.051
C                     RibYOffset 	= 56.099	48.898
C StationTwoBRib    = RibWidth 		= 0.500		0.500
C                     RibLength 	= 196.513	156.748
C                     RibXOffset 	= 139.433	122.251
C                     RibYOffset 	= 57.755	50.638
C StationTwoFGas    = InnerRadius 	= 53.522	54.992
C                     OuterRadius 	= 228.910	193.000
C                     X1Offset 		= 9.827		9.827
C                     Y1Offset 		= 3.800		3.800
C                     X2Offset 		= 9.636		9.636
C                     Y2Offset 		= 4.262		4.262
C StationTwoBGas    = OuterRadius 	= 239.022	201.999
C StationThreeFrame = InnerRadius 	= 98.472	88.093
C                     OuterRadius 	= 427.472	321.093
C                     HoleInnerRadius 	= 94.502	84.383
C                     HoleOuterRadius 	= 418.422	311.183
C                     Thickness 	= 0.635		0.635
C                     HoleXOffset 	= 6.133		6.133
C                     HoleYOffset 	= 2.540		2.540
C                     FrameXOffset 	= 6.035		6.035
C                     FrameYOffset 	= 2.500		2.500
C StationThreePanel = HoneyInnerRadius 	= 96.487	86.108
C                     HoneyOuterRadius 	= 422.947	316.568
C                     Thickness 	= 2.165		2.165
C                     HoneyThickness 	= 1.968		1.968
C                     CopperThickness 	= 0.005		0.005
C                     HoneyXOffset 	= 3.067		3.067
C                     HoneyYOffset 	= 1.270		1.270
C StationThreeAnode = HoleInnerRadius	= 89.483	75.848
C 		      HoleOuterRadius	= 412.283	302.648
C 		      Thickness 	= 0.635		0.635
C 		      HoleXOffset 	= 11.106	11.106
C                     HoleYOffset 	= 4.600		4.600
C StationThreeRib   = RibWidth 		= 2.540		2.540
C                     RibLength 	= 327.255	228.355
C                     RibXOffset 	= 232.510	180.360 
C                     RibYOffset 	= 97.683	74.709
C StationThreeGas   = InnerRadius 	= 87.423	76.548
C                     OuterRadius 	= 405.430	298.049
C                     X1Offset 		= 3.318		3.318
C                     Y1Offset 		= 0.000		0.000
C                     X2Offset 		= 2.348 	2.348 
C                     Y2Offset 		= 2.348 	2.348
C
C
C --- FPMLINK.inc ---------------------------------------------------------
C
c       channel count tracking stations
        integer           mum_arms_max
        parameter       ( mum_arms_max = 2 )
        integer           mum_stations_max
        parameter       ( mum_stations_max = 3 )
        integer           mum_planes_max
        parameter       ( mum_planes_max = 7 )
        integer           maxplanes
        parameter       (maxplanes=MUM_PLANES_MAX*MUM_STATIONS_MAX)

c       muon detector tracking parameters

        integer mumtrflg
        integer mum_arms, mum_stations, mum_channels
        integer mum_color
        integer ArmMedium
        integer PlanesPerStation(  mum_stations_max,mum_arms_max )
        integer StationMedium
        integer HoneyMedium
        integer FEEFlag    
        real    StationOneFrame(8) 
        real    StationOnePanel(8) 
        real    StationOneAnode(4) 
        real    StationOneRead(8)  
        real    StationOneRib(7)   
        real    StationOneGas(8)   
        real    StationOneMount(7) 
        real    StationOneFee(10)  
        real    StationOneAngles(6,4,mum_arms_max)
        real    StationOneOffsets(3,4,mum_arms_max)
        real    StationTwoFFrame(7,mum_arms_max)
        real    StationTwoBFrame(2,mum_arms_max)
        real    StationTwoFBar(2,mum_arms_max) 
        real    StationTwoBBar(2,mum_arms_max) 
        real    StationTwoAnode(mum_arms_max)
        real    StationTwoFoilThickness
        real    StationTwoAlFoilThickness
        real    StationTwoFRib(4,mum_arms_max)
        real    StationTwoBRib(4,mum_arms_max)
        real    StationTwoFGas(6,mum_arms_max)
        real    StationTwoBGas(mum_arms_max)
        real    StationTwoAngles(6,8,mum_arms_max)
        real    StationTwoOffsets(3,8,mum_arms_max)
        real    StationThreeFrame(9,mum_arms_max)
        real    StationThreePanel(7,mum_arms_max)
        real    StationThreeAnode(5,mum_arms_max)
        real    StationThreeRib(4,mum_arms_max)
        real    StationThreeGas(6,mum_arms_max)
        real    StationThreeAngles(6,8,mum_arms_max)
        real    StationThreeOffsets(3,8,mum_arms_max)
        integer FrameMedium(    mum_stations_max,mum_arms_max)
        real    StationNominalZpos( mum_stations_max+1,mum_arms_max )
        integer    SpokeMedium     !Spoke medium

        integer           mum_par_words
        parameter       ( mum_par_words = 572)

        namelist        / mum_par /
     &         mumtrflg,
     &         mum_arms, 
     &         mum_stations, 
     &         mum_channels,
     &         mum_color,
     &         ArmMedium,
     &         PlanesPerStation,
     &         StationMedium,
     &         HoneyMedium,
     &         FEEFlag,    
     &         StationOneFrame,
     &         StationOnePanel,
     &         StationOneAnode,
     &         StationOneRead,
     &         StationOneRib,
     &         StationOneGas,
     &         StationOneMount,
     &         StationOneFee,
     &         StationOneAngles,
     &         StationOneOffsets,
     &         StationTwoFFrame,
     &         StationTwoBFrame,
     &         StationTwoFBar,
     &         StationTwoBBar,
     &         StationTwoAnode,
     &         StationTwoFoilThickness,
     &         StationTwoAlFoilThickness,
     &         StationTwoFRib,
     &         StationTwoBRib,
     &         StationTwoFGas,
     &         StationTwoBGas,
     &         StationTwoAngles,
     &         StationTwoOffsets,
     &         StationThreeFrame,
     &         StationThreePanel,
     &         StationThreeAnode,
     &         StationThreeRib,
     &         StationThreeGas,
     &         StationThreeAngles,
     &         StationThreeOffsets,
     &         FrameMedium,
     &         StationNominalZpos,
     &         SpokeMedium

        common          / mum_common_1 /
     &         mumtrflg,
     &         mum_arms,
     &         mum_stations,
     &         mum_channels,
     &         mum_color,
     &         ArmMedium,
     &         PlanesPerStation,
     &         StationMedium,
     &         HoneyMedium,
     &         FEEFlag,    
     &         StationOneFrame,
     &         StationOnePanel,
     &         StationOneAnode,
     &         StationOneRead,
     &         StationOneRib,
     &         StationOneGas,
     &         StationOneMount,
     &         StationOneFee,
     &         StationOneAngles,
     &         StationOneOffsets,
     &         StationTwoFFrame,
     &         StationTwoBFrame,
     &         StationTwoFBar,
     &         StationTwoBBar,
     &         StationTwoAnode,
     &         StationTwoFoilThickness,
     &         StationTwoAlFoilThickness,
     &         StationTwoFRib,
     &         StationTwoBRib,
     &         StationTwoFGas,
     &         StationTwoBGas,
     &         StationTwoAngles,
     &         StationTwoOffsets,
     &         StationThreeFrame,
     &         StationThreePanel,
     &         StationThreeAnode,
     &         StationThreeRib,
     &         StationThreeGas,
     &         StationThreeAngles,
     &         StationThreeOffsets,
     &         FrameMedium,
     &         StationNominalZpos,
     &         SpokeMedium
C
C  Magnet namelist variables:
C
      real z_roll(2)       ! Roll magnet this distance in z
      integer ncent_max    ! max anticipated number of z-planes
      parameter (ncent_max=16)   ! 16 as of Feb. 27, 1996
      real z_cent(ncent_max) /ncent_max*0.0/     ! z-plane positions
      real rmin_cent(ncent_max) /ncent_max*0.0/   ! rmin values at given z
      real rmax_cent(ncent_max) /ncent_max*0.0/    ! rmax values at given z
      real pcon_par(3*ncent_max+3)  ! parameter array for the PCON call
      integer npl_cent /0/    ! actual number of z-planes
      integer nmed_cent /0/      ! medium number for the yoke
      integer color_cent /4/     ! color (visible/invisible) for central
      integer nend_max
      parameter (nend_max = 25)
      real z_end(nend_max,2) /nend_max*0.0,nend_max*0.0/ ! z-plane positions
      real rmin_end(nend_max,2) /nend_max*0.0,nend_max*0.0/ ! rmin values at a given z
      real rmax_end(nend_max,2) /nend_max*0.0,nend_max*0.0/ ! rmax values at a given z
      integer npl_end(2) /2*10/
      integer muo_arms /2/
      integer pist_med /5/
      integer coil_med /15/
      real coil_irad1(2) /45.2,45.2/
      real coil_irad2(2) /45.2,45.2/
      real coil_orad1(2) /45.2,45.2/
      real coil_orad2(2) /45.2,45.2/
      real coil_z12(4,2) /4*0.0,4*0.0/
      real mega_irad1(2)
      real mega_orad1(2) /2*30.7/
      real mega_irad2(2)
      real mega_orad2(2) /2*89.6/
      real mega_z12(2,2)
      integer mega_med /5/
      integer color_endc /4/     ! color (visible/invisible) for endcap
      integer color_coil /3/     ! color (visible/invisible) for coils
      real*4 mega_thick
      real*4 depth_notch/17.0/, coil_thick/5.15/
      real*4 pist_thet,shad_thet,pist2_thet
      real*4 zpist_core(2,2),pcore_rad(4,2), pcdelr(2,2)
      real*4 zcent_core(11),ccore_rmin(11),ccore_rmax(11)
      integer iflag_pcore,nmed_pcore,iflag_ccore,nmed_ccore
      integer fin_flag,buss_flag,buss_buss_flag,wtr_flag
      integer rib_flag,fin_med,buss_med,flag_med,rib_med
      integer cutt_med,wtr_med
      integer npc_core(2)
       real*4 cyoke_dz /242.0/    ! half-length in Z
       real*4 cyoke_dx /79.9275/  ! half-width in X
       real*4 cyoke_dy /50.0/     ! half-width in Y
       real*4 cyoke_yp /345.0/    ! mid-point in Y (X and Z mid-points = 0)            
       real*4 dyoke_dz /53.5/     ! half-length in Z
       real*4 dyoke_dx /79.9275/  ! half-width in X
       real*4 dyoke_dy /39.0/     ! half-width in Y
       real*4 dyoke_yp /256.0/    ! mid-point in Y (X mid-point = 0)
       real*4 dyoke_zp /188.5/    ! mid-point in Y (X mid-point = 0)
       real*4 dyoke_arc /13.5/    ! height of overlap arc with AMGU and AMGD
       integer cencoil_med(2)     !coil media (copper)
       integer cencoil_color(2)   !coil color
       real cencoil_rin(2)        !inner radius each coil
       real cencoil_rout(2)       !outer radius each coil
       real cencoil_z(2,2)        !z endpoints (North) each coil
       real fin_thick,fin_z1(2),fin_z2(2),corn_ang(2)
       real buss_z(2),buss_thick,buss_wide
       real flag_par(3),flag_x,flag_y,flag_z(2),flag_r,flag_phi
       real rib_z1(2,2),rib_z2(2,2),wtr_par(5)
c
c
c added Sept-12-96 for retaining ring around coil
c added Oct   7-96 default values for retaining ring parameters
c 
      real ret_ring_par(12) /0.,360.,3,-3.75,159.2,159.3,
     +                       -.65,159.2,174.2,3.75,159.2,174.2/ 
      real ret_ring_pos(3) /0.0, 0.0, 95.0/
      real ret_ring_rot(6) /90.,0.,90.,90.,180.,90./
      integer retmed /5/    !  retaining ring medium (magnetic iron)
c
c
      namelist /mag_par/ npl_cent, nmed_cent, z_cent, rmin_cent,
     1                  rmax_cent, color_cent, color_coil,
     2         npl_end, pist_med, z_end, rmin_end, rmax_end,
     3         coil_med, coil_irad1, coil_irad2, coil_orad1,
     4         coil_orad2, coil_z12, 
     5         mega_irad1, mega_orad1, mega_irad2,
     6         mega_orad2, mega_z12, mega_med, color_endc, muo_arms,
     7         pist_thet, shad_thet, mega_thick, depth_notch,coil_thick,
     8         iflag_pcore, zpist_core, pcore_rad, nmed_pcore,
     9         iflag_ccore, npc_core, zcent_core, ccore_rmin,
     9         ccore_rmax, nmed_ccore,pcdelr,
     x         cyoke_dz, cyoke_dy, cyoke_dx, cyoke_yp,
     y         dyoke_dz, dyoke_dx, dyoke_dy, dyoke_zp, dyoke_yp,
     &         dyoke_arc,
     z         cencoil_med,cencoil_color,
     1         cencoil_z,cencoil_rin,cencoil_rout
     2         ,pist2_thet,fin_thick,fin_z1,fin_z2,corn_ang,buss_z,
     3         buss_thick,buss_wide,flag_par,wtr_par,rib_z1,rib_z2,
     4         flag_z,fin_flag,buss_flag,buss_buss_flag,wtr_flag,
     5         rib_flag,fin_med,buss_med,flag_med,rib_med,cutt_med,
     6         wtr_med,ret_ring_par,ret_ring_pos,ret_ring_rot,retmed,
     7         z_roll
     
       common /ugeom_muon1/mega_irad1,mega_irad2,mega_z12,mega_thick
       
c-sps--------------------------------------------------------------------------
      integer   lfm_link, lfm_lref, lfm_last,
     &          lfm_sparelinks, lfm_sparerefs,
     &          lfm_map, lfm_cal, lfm_para, lfm_paru, lfm_digi,
     &          lfm_cclus
        common  / fpmlink /
     &          lfm_link,               ! start of structural links
     &          lfm_map,                ! Mapped (contiguous) raw data
     &          lfm_cal(maxsub),        ! Calibrated data, subevents
     &          lfm_para,
     &          lfm_paru,
     &          lfm_digi,               ! digitization bank link
     &          lfm_cclus,              ! bank to hold CSC clusters
     &          lfm_sparelinks(5),
     &          lfm_lref,               ! start of reference links
     &          lfm_sparerefs(5),
     &          lfm_last                ! last link ptr.
c       data zebra banks

c       mapped raw data (not active)
        integer           sfmm_map
        integer           ofmm_par1
        integer           ofmm_par2
        parameter       ( sfmm_map      = 2 )   ! Size of 1 entry
        parameter       ( ofmm_par1     = 0 )   ! Offset of par 1
        parameter       ( ofmm_par2     = 2 )   ! Offset of par 2

C       bank of calibrated data
        integer           sfmc_cal
        integer           ofmc_track
        integer           ofmc_plane
        integer           ofmc_pid
        integer           ofmc_ptof
        integer           ofmc_e
        integer           ofmc_x
        integer           ofmc_y
        integer           ofmc_z
        integer           ofmc_px
        integer           ofmc_py
        integer           ofmc_pz
        parameter       ( sfmc_cal      = 11 )
        parameter       ( ofmc_track    = 0 )   ! track number  (fict)
        parameter       ( ofmc_plane    = 1 )   ! plane number  (real)
        parameter       ( ofmc_pid      = 2 )   ! particle id   (real)
        parameter       ( ofmc_ptof     = 3 )   ! particle tof  (real)
        parameter       ( ofmc_e        = 4 )   ! energy        (real)
        parameter       ( ofmc_x        = 5 )   ! x position    (real)
        parameter       ( ofmc_y        = 6 )   ! y position    (real)
        parameter       ( ofmc_z        = 7 )   ! z Position    (real)
        parameter       ( ofmc_px       = 8 )   ! x Momentum    (fict)
        parameter       ( ofmc_py       = 9 )   ! y Momentum    (fict)
        parameter       ( ofmc_pz       =10 )   ! z Momentum    (fict)

C
C --- end of FPMLINK.inc --------------------------------------------------
C
