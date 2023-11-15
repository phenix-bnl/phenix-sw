c
c --- FPSLINK.inc ---------------------------------------------------------
c for Reaction Plane
c
c
c    Name           Date               Comment
c    C.F. Maguire   August 25, 2006    Template version (copy of TFW template)
c
c -------------------------------------------------------------------------
      integer  lfs_link, lfs_lref, lfs_last, lfs_rxn(48),
     &         lfs_cal, lfs_para, lfs_paru

      common   / fslink /
     &         lfs_link,
     &         lfs_rxn,      
     &         lfs_para,
     &         lfs_paru,
     &         lfs_lref,
     &         lfs_last
c data zebra banks
c mapped raw data (not active)
c bank of calibrated data
        integer           mfd_rxn
        integer           ofsc_track
        integer           ofsc_arm
        integer           ofsc_xlocal1
        integer           ofsc_ylocal1
        integer           ofsc_zlocal1
        integer           ofsc_xlocal2
        integer           ofsc_ylocal2
        integer           ofsc_zlocal2
        integer           ofsc_tof
        integer           ofsc_pathLength
        integer           ofsc_pid
        integer           ofsc_xglobal
        integer           ofsc_yglobal
        integer           ofsc_zglobal
        integer           ofsc_dele
	integer           ofsc_pmomx
        integer           ofsc_pmomy
        integer           ofsc_pmomz
        parameter       ( mfd_rxn      = 18 )   ! number in zebra bank
        parameter       ( ofsc_track    = 0 )   ! track number
        parameter       ( ofsc_arm   = 1 )   ! arm number
        parameter       ( ofsc_xlocal1  = 2 )   ! local x value at entrance
        parameter       ( ofsc_ylocal1  = 3 )   ! local y value at entrance
        parameter       ( ofsc_zlocal1  = 4 )   ! local z value at entrance	
        parameter       ( ofsc_xlocal2  = 5 )   ! local x value at entrance
        parameter       ( ofsc_ylocal2  = 6 )   ! local y value at entrance
        parameter       ( ofsc_zlocal2  = 7 )   ! local z value at entrance
        parameter       ( ofsc_tof      = 8 )   ! particle tof
        parameter       ( ofsc_pathLength = 9 ) ! pathlength
        parameter       ( ofsc_pid      = 10 )  ! particle ID    
        parameter       ( ofsc_xglobal  = 11 )  ! global x position at entrance
        parameter       ( ofsc_yglobal  = 12 )  ! global y position at entrance
        parameter       ( ofsc_zglobal  = 13 )  ! global z position at entrance 
        parameter       ( ofsc_dele  = 14 )     ! energy loss 	
        parameter       ( ofsc_pmomx  = 15 )    ! x component of momentum
        parameter       ( ofsc_pmomy  = 16 )    ! y component of momentum
	parameter       ( ofsc_pmomz  = 17 )    ! z component of momentum
C
C --- end of FPSLINK.inc --------------------------------------------------
C


