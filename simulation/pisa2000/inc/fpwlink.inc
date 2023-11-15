c
c --- FPWLINK.inc ---------------------------------------------------------
c for TOF West
c
c
c    Name           Date               Comment
c    C.F. Maguire   August 16, 2006    Template version
c
c -------------------------------------------------------------------------
      integer  lfw_link, lfw_lref, lfw_last, lfw_tfw(2),
     &         lfw_cal, lfw_para, lfw_paru

      common   / fwlink /
     &         lfw_link,
     &         lfw_tfw,      
     &         lfw_para,
     &         lfw_paru,
     &         lfw_lref,
     &         lfw_last
c data zebra banks
c mapped raw data (not active)
c bank of calibrated data
        integer           mfd_tfw
        integer           ofwc_track
        integer           ofwc_panel
        integer           ofwc_xlocal1
        integer           ofwc_ylocal1
        integer           ofwc_zlocal1
        integer           ofwc_xlocal2
        integer           ofwc_ylocal2
        integer           ofwc_zlocal2
        integer           ofwc_tof
        integer           ofwc_pathLength
        integer           ofwc_pid
        integer           ofwc_xglobal
        integer           ofwc_yglobal
        integer           ofwc_zglobal
        integer           ofwc_dele
        parameter       ( mfd_tfw      = 15 )   ! number in zebra bank
        parameter       ( ofwc_track    = 0 )   ! track number
        parameter       ( ofwc_panel   = 1 )    ! panel number
        parameter       ( ofwc_xlocal1  = 2 )   ! local x value at entrance
        parameter       ( ofwc_ylocal1  = 3 )   ! local y value at entrance
        parameter       ( ofwc_zlocal1  = 4 )   ! local z value at entrance	
        parameter       ( ofwc_xlocal2  = 5 )   ! local x value at entrance
        parameter       ( ofwc_ylocal2  = 6 )   ! local y value at entrance
        parameter       ( ofwc_zlocal2  = 7 )   ! local z value at entrance
        parameter       ( ofwc_tof      = 8 )   ! particle tof
        parameter       ( ofwc_pathLength = 9 ) ! pathlength
        parameter       ( ofwc_pid      = 10 )  ! particle ID    
        parameter       ( ofwc_xglobal  = 11 )  ! global x position at entrance
        parameter       ( ofwc_yglobal  = 12 )  ! global y position at entrance
        parameter       ( ofwc_zglobal  = 13 )  ! global z position at entrance 
        parameter       ( ofwc_dele  = 14 )     ! energy loss 	
C
C --- end of FPWLINK.inc --------------------------------------------------
C
