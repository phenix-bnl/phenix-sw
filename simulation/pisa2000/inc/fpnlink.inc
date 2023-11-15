C
C --- FPNLINK.inc ---------------------------------------------------------
c     CFM proposition for PHNX  MUON   ZEBRA banks April 7, 1992
C     NEW FOR RELEASE 2 BY GUS PETITT, SEPTEMBER 24, 1992
c     Channel count Muon ID
c
      integer mfn_planes
      parameter (mfn_planes=20)  ! maximum anticipated
      integer mfn_copies
      parameter (mfn_copies=2)   ! 2 copies per plane (just for now)
      integer mfn_hits
      parameter (mfn_hits = 100)
      integer mfn_alldets

c
      integer muid_flg,max_muid_planes,max_abs_planes
      parameter (max_muid_planes = 6)
      ! integer nr_muon_arms,nr_muid_planes,ndet_per_pl,nr_det_planes
      integer muid_shield
      integer mu_floor_flg
      integer mu_hi_color,mu_lo_color,color_muid,color_hole
      integer color_dont,color_floor,color_strd,color_yoke
      integer nmed_mu_ps,nmed_mu_gas,nmed_mu_cs,nmed_mu_sh,nmed_mu_sd
      integer nmed_mudn,nmed_muhl,nmed_mufl,nmed_yoke
      integer mu_lo_med(max_muid_planes),mu_hi_med(max_muid_planes)

      real    muid_zmin,muid_zmax,muid_ang,beam_height,mu_top_height,
     +        mu_floor_height,mu_strp_thick,mu_gas_thick,mu_plas_thick,
     +        mu_yoke_thick(2),mu_donut_thick(2,2),mu_floor_thick
      real*4  rmax1_donut(2,2), rmax2_donut(2,2)
      real    mu_abs_thick(max_muid_planes),mu_hi_abs(max_muid_planes)
      real*4  mu_abs_x,mu_abs_y
      real*4  mu_abs_z(max_muid_planes)

      real*4  zmax_muarm,dzhole,dzshld,rmax_1
      real    rykmin1(2),rykmin2(2),rmin_donut(2),zyoke(2),
     +        zgap_yoke_abs,
     +        zgap_labs_ldet,muid_delx,str_xstag
      real    z_muhl_shld(2),thick_muhl_shld
c
      integer muhl_shld_flag,nmed_muhl_shld,color_muhl_shld
      integer muhl_config_flag,muabs_config_flag,npl_muhl
      integer max_muhp
      parameter (max_muhp = 6 )
      real*4  rmin_muhl(max_muhp),rmax_muhl(max_muhp),z_muhl(max_muhp)
      real*4  crack_size,crack_shift
      integer*4 mu_flag_cracks, nmed_cracks, num_cracks,crack_color

        namelist /mun_par/
     +        mu_floor_flg,
     +        muid_shield,
     +        muid_zmin, muid_zmax, muid_ang, beam_height,
     +        mu_top_height, mu_floor_height, mu_strp_thick,
     +        mu_gas_thick, mu_plas_thick, mu_yoke_thick,
     +        mu_donut_thick,mu_floor_thick, mu_abs_thick, mu_hi_abs,
     +        rmax1_donut, rmax2_donut,
     +        mu_hi_med, mu_lo_med, nmed_mu_ps, nmed_mu_gas,
     +        nmed_mu_cs, nmed_mu_sh, nmed_mu_sd, nmed_mudn, nmed_muhl,
     +        nmed_mufl, nmed_yoke, mu_hi_color, mu_lo_color,
     +        color_muid, color_hole, color_dont, color_floor,
     +        color_strd, color_yoke, rykmin1, rykmin2, rmin_donut,
     +        zyoke,zgap_yoke_abs,zgap_labs_ldet,muid_delx,str_xstag,
     +        muid_flg,muhl_shld_flag,nmed_muhl_shld,color_muhl_shld,
     +        z_muhl_shld,thick_muhl_shld,muhl_config_flag,z_muhl,
     +        muabs_config_flag,npl_muhl,rmin_muhl,rmax_muhl,
     +        mu_abs_x,mu_abs_y,mu_abs_z,mu_flag_cracks,nmed_cracks,
     +        crack_size, num_cracks, crack_shift,crack_color

C    WARNING to Shaheen and Charlie:  The following is a patch to make
C    things compile.  I don't understand it:
      parameter (mfn_alldets=mfn_planes*mfn_copies*mfn_hits)
c
c
      integer   lfn_link, lfn_lref, lfn_last,
     &          lfn_sparelinks, lfn_sparerefs,
     &          lfn_map, lfn_cal, lfn_para, lfn_paru, lfn_digi
        common  / fnlink /
     &          lfn_link,               ! start of structural links
     &          lfn_map,                ! Mapped (contiguous) raw data
     &          lfn_cal(maxsub),        ! Calibrated data, subevents
     &          lfn_para,
     &          lfn_paru,
     &          lfn_digi,               ! digitization bank link
     &          lfn_sparelinks(6),
     &          lfn_lref,               ! start of reference links
     &          lfn_sparerefs(5),
     &          lfn_last                ! last link ptr.
c       data zebra banks

c       mapped raw data (not active)
        integer           sfnm_map
        integer           ofnm_par1
        integer           ofnm_par2
        parameter       ( sfnm_map      = 2 )   ! Size of 1 entry
        parameter       ( ofnm_par1     = 0 )   ! Offset of par 1
        parameter       ( ofnm_par2     = 2 )   ! Offset of par 2

C       bank of calibrated data
        integer           sfnc_cal
        integer           ofnc_track
        integer           ofnc_plane
        integer           ofnc_pid
        integer           ofnc_ptof
        integer           ofnc_e
        integer           ofnc_x
        integer           ofnc_y
        integer           ofnc_z
        integer           ofnc_px
        integer           ofnc_py
        integer           ofnc_pz
        parameter       ( sfnc_cal      = 11 )
        parameter       ( ofnc_track    = 0 )   ! track number  (fict)
        parameter       ( ofnc_plane    = 1 )   ! plane number  (real)
        parameter       ( ofnc_pid      = 2 )   ! particle id   (real)
        parameter       ( ofnc_ptof     = 3 )   ! particle tof  (real)
        parameter       ( ofnc_e        = 4 )   ! energy        (real)
        parameter       ( ofnc_x        = 5 )   ! x position    (real)
        parameter       ( ofnc_y        = 6 )   ! y position    (real)
        parameter       ( ofnc_z        = 7 )   ! z Position    (real)
        parameter       ( ofnc_px       = 8 )   ! x Momentum    (fict)
        parameter       ( ofnc_py       = 9 )   ! y Momentum    (fict)
        parameter       ( ofnc_pz       =10 )   ! z Momentum    (fict)

C
C --- end of FPNLINK.inc --------------------------------------------------
C
