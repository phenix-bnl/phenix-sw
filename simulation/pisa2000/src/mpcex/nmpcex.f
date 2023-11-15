C=====================================================================
      subroutine nmpcex
C=====================================================================
C
C    DESCRIPTION: This routine defines the geometry for the original
C                 version of the South Muon-Piston Calorimeter.
C
C    MAP:
C           1) CALLED BY MPCEX.F
C
C=====================================================================


C  Volume descriptors
      real  tstep                ! step size between towers
      real  tstep2               ! 1/2 step size between towers
      real  tskin                ! thickness of Al skin of modules
      real  n_toffset            ! x offset of middle segments
      data  tstep/2.26/          ! avg crystal thickness, including wrap
      data  tstep2/1.13/
      data  tskin/0.15875/
      data  n_toffset/0.6807/

      real x_pos,y_pos
      real n_z_pos

      data n_z_pos/229.9475/
      integer itower


C ** North Bottom Row, Left
        itower = 330
        y_pos = -9.*tstep + tstep2 - 2*tskin
        x_pos = -3.*tstep + tstep2 - tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Bottom Row, Right
        y_pos = -9.*tstep + tstep2 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Second Row from Bottom, Left
        itower = 346
        y_pos = -9.*tstep + tstep2 + tstep*1.0 - 2*tskin
        x_pos = -5.*tstep + tstep2 - tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Second Row from Bottom, Right
        itower = 351
        y_pos = -9.*tstep + tstep2 + tstep*1.0 -2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Third Row from Bottom, Left
        itower = 363 
        y_pos = -9.*tstep + tstep2 + tstep*2.0 - 2*tskin
        x_pos = -6.*tstep + tstep2 - tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Third Row from Bottom, Right
        itower = 369
        y_pos = -9.*tstep + tstep2 + tstep*2.0 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Fourth Row from Bottom, Left
        itower = 380
        y_pos = -9.*tstep + tstep2 + tstep*3.0 - 2*tskin
        x_pos = -7.*tstep + tstep2 - tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Fourth Row from Bottom, Right
        itower = 387
        y_pos = -9.*tstep + tstep2 + tstep*3.0 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Fifth Row from Bottom, Left
        itower = 397
        y_pos = -9.*tstep + tstep2 + tstep*4.0 - 2*tskin
        x_pos = -8.*tstep + tstep2 - tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Fifth Row from Bottom, Right
        itower = 405
        y_pos = -9.*tstep + tstep2 + tstep*4.0 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Sixth Row from Bottom, Left
        itower = 415
        y_pos = -9.*tstep + tstep2 + tstep*5.0 - 2*tskin
        x_pos = -8.*tstep + tstep2 - tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Sixth Row from Bottom, Right
        itower = 423
        y_pos = -9.*tstep + tstep2 + tstep*5.0 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Seventh Row from Bottom, Left
        itower = 433
        y_pos = -9.*tstep + tstep2 + tstep*6.0
        x_pos = -8.*tstep + tstep2 - tskin - n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Eigth Row from Bottom, Right
        itower = 443
        y_pos = -9.*tstep + tstep2 + tstep*6.0
        x_pos = 2.*tstep + tstep2 + tskin + n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Eigth Row from Bottom, Left
        itower = 450
        y_pos = -9.*tstep + tstep2 + tstep*7.0
        x_pos = -9.*tstep + tstep2 - tskin - n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Eigth Row from Bottom, Right
        itower = 462
        y_pos = -9.*tstep + tstep2 + tstep*7.0
        x_pos = 3.*tstep + tstep2 + tskin + n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Ninth Row from Bottom, Left
        itower = 468
        y_pos = -9.*tstep + tstep2 + tstep*8.0
        x_pos = -9.*tstep + tstep2 - tskin - n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Ninth Row from Bottom, Right
        itower = 480
        y_pos = -9.*tstep + tstep2 + tstep*8.0
        x_pos = 3.*tstep + tstep2 + tskin + n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Tenth Row from Bottom, Left
        itower = 486
        y_pos = -9.*tstep + tstep2 + tstep*9.0
        x_pos = -9.*tstep + tstep2 - tskin - n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Tenth Row from Bottom, Right
        itower = 498
        y_pos = -9.*tstep + tstep2 + tstep*9.0
        x_pos = 3.*tstep + tstep2 + tskin + n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Eleventh Row from Bottom, Left
        itower = 504
        y_pos = -9.*tstep + tstep2 + tstep*10.0
        x_pos = -9.*tstep + tstep2 - tskin - n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Eleventh Row from Bottom, Right
        itower = 516
        y_pos = -9.*tstep + tstep2 + tstep*10.0
        x_pos = 3.*tstep + tstep2 + tskin + n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Twelth Row from Bottom, Left
        itower = 523
        y_pos = -9.*tstep + tstep2 + tstep*11.0
        x_pos = -8.*tstep + tstep2 - tskin - n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Twelth Row from Bottom, Right
        itower = 533
        y_pos = -9.*tstep + tstep2 + tstep*11.0
        x_pos = 2.*tstep + tstep2 + tskin + n_toffset
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Thirteenth Row from Bottom, Left
        itower = 541
        y_pos = -9.*tstep + tstep2 + tstep*12.0 + 2*tskin
        x_pos = -8.*tstep + tstep2 - tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Thirteenth Row from Bottom, Right
        itower = 549
        y_pos = -9.*tstep + tstep2 + tstep*12.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Fourteenth Row from Bottom, Left
        itower = 559
        y_pos = -9.*tstep + tstep2 + tstep*13.0 + 2*tskin
        x_pos = -8.*tstep + tstep2 - tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Fourteenth Row from Bottom, Right
        itower = 567
        y_pos = -9.*tstep + tstep2 + tstep*13.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Fifteenth Row from Bottom, Left
        itower = 578
        y_pos = -9.*tstep + tstep2 + tstep*14.0 + 2*tskin
        x_pos = -7.*tstep + tstep2 - tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Fifteenth Row from Bottom, Right
        itower = 585
        y_pos = -9.*tstep + tstep2 + tstep*14.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Sixteenth Row from Bottom, Left
        itower = 597
        y_pos = -9.*tstep + tstep2 + tstep*15.0 + 2*tskin
        x_pos = -6.*tstep + tstep2 - tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Sixteenth Row from Bottom, Right
        itower = 603
        y_pos = -9.*tstep + tstep2 + tstep*15.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Seventeenth Row from Bottom, Left
        itower = 616
        y_pos = -9.*tstep + tstep2 + tstep*16.0 + 2*tskin
        x_pos = -5.*tstep + tstep2 - tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Seventeenth Row from Bottom, Right
        itower = 621
        y_pos = -9.*tstep + tstep2 + tstep*16.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Eighteenth Row from Bottom, Left
        itower = 636
        y_pos = -9.*tstep + tstep2 + tstep*17.0 + 2*tskin
        x_pos = -3.*tstep + tstep2 - tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** North Eighteenth Row from Bottom, Right
        itower = 639
        y_pos = -9.*tstep + tstep2 + tstep*17.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA1',x_pos,y_pos,n_z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

        call run08_nmpcex_container

        return
        END


C=====================================================================
C    SUBROUTINE NAME run08_nmpc_container
C.   author : Mickey Chiu
C=====================================================================
      subroutine run08_nmpcex_container
C=====================================================================
C
C    DESCRIPTION: This routine defines the container (skin,ribs) for 
C                 the North Muon-Piston Calorimeter, Run07 configuration. 
C
C=====================================================================

      integer ivolu
      integer skin_nmed
      real    skin_par(3)         ! skin parameters
      real    skin_loc(3)         ! skin location
      real    plate_par(3)        ! skin parameters
      real    IN2CM               ! conversion from in 2 cm
      real    x
      real    y
      real    z
      real    z_pos               ! z midpoint of crystal
      real    tstep               ! crystal size
      real    tskin               ! thickness of Al skin of modules
      real    skin_rmin           ! inner radius of outer skin
      real    skin_rmax           ! outer radius of outer skin
      real    z_frontplate        ! z midpoint of front plate
      real    z_backplate         ! z midpoint of back plate

      data skin_nmed/9/           ! Aluminum
      data skin_par/3*0./
      data skin_loc/3*0./
      data plate_par/3*0./
      data IN2CM/2.54/
      data tstep/2.26/
      data tskin/0.15875/
      data z_pos/229.9475/
      data z_frontplate/219.103/
      data z_backplate/239.424/   ! z_pos + 0.5*crystal_len + 3/16 inch

      skin_rmax = 22.5-(1.0/16.0)*IN2CM
      skin_rmin = skin_rmax-tskin

C This is already defined in run08_smpc.f
C      call gsvolu('SKIN','BOX ',skin_nmed,skin_par,0,ivolu)
C      call gsatt('SKIN','SEEN',1)
C      call gsatt('SKIN','COLO',4)

C SKIN1 (upper and lower vertical midpoint pieces)
      skin_par(1) = tskin - 0.00001
      skin_par(2) = (skin_rmin - (3.0*tstep+2.0*tskin))/2.0 - 0.00001
      skin_par(3) = 9.0

      x = 0.
      y = 3.0*tstep + 2.0*tskin + skin_par(2) + 0.00001/2.0
      z = z_pos
      call gsposp('SKIN',11,'MUA1',x,y,z,0,'ONLY',skin_par,3)

      y = -y
      call gsposp('SKIN',12,'MUA1',x,y,z,0,'ONLY',skin_par,3)

C SKIN2 (upper and lower horizontal pieces)
      skin_par(1) = 8.0*tstep + 2.8
      skin_par(2) = tskin - 0.00001

      x = 0.
      y = 3.0*tstep + tskin
      call gsposp('SKIN',13,'MUA1',x,y,z,0,'ONLY',skin_par,3)

      y = -y
      call gsposp('SKIN',14,'MUA1',x,y,z,0,'ONLY',skin_par,3)

C Front and back plate 

      x = 0.
      y = 0.
      z = z_frontplate
      plate_par(1) = 3.0*tstep 
      plate_par(2) = skin_rmax - 0.00001
      plate_par(3) = (3.0/16.0)*IN2CM

      call gsposp('FPLT',15,'MUA1',x,y,z,0,'ONLY',plate_par,3)

      z = z_backplate
      call gsposp('BPLT',16,'MUA1',x,y,z,0,'ONLY',plate_par,3)

C Outer skin
      plate_par(1) = skin_rmin
      plate_par(2) = skin_rmax
      plate_par(3) = ((z_backplate-z_frontplate)-0.375*IN2CM)/2.0 - 0.00001
      z = (z_frontplate+z_backplate)/2.0      ! midway between front and back plates
      call gsposp('PLAT',17,'MUA1',x,y,z,0,'ONLY',plate_par,3)


      END

