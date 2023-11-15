c $Id: run08_smpc.f,v 1.8 2010/10/21 01:32:04 chiu Exp $

C=====================================================================
C    FILE NAME run08_smpc
C.   author : Mickey Chiu
C    July 16, 2007
C.   South Muon Piston Calorimeter installed in Run08
C=====================================================================
      subroutine run08_smpc
C=====================================================================
C
C    DESCRIPTION: This routine defines the geometry for the run08
C                 version of the South Muon-Piston Calorimeter.
C
C    MAP:
C           1) CALLED BY MPC.F
C
C=====================================================================


C  Volume descriptors
      real  tstep                ! step size between towers
      real  tstep2               ! 1/2 step size between towers
      real  tskin                ! thickness of Al skin of modules
      real  toffset              ! x offset of middle segments
C      data  tstep/2.235/
C      data  tstep2/1.1175/
      data  tstep/2.26/
      data  tstep2/1.13/
      data  tskin/0.15875/
      data  toffset/0.3055/

      real x_pos,y_pos,z_pos
      data z_pos/229.9475/
      integer itower


C ** Bottom Row, Left
        itower = 6
        y_pos = -9.*tstep + tstep2 - 2*tskin
        x_pos = -3.*tstep + tstep2 - tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Bottom Row, Right
        y_pos = -9.*tstep + tstep2 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Second Row from Bottom, Left
        itower = 22 
        y_pos = -9.*tstep + tstep2 + tstep*1.0 - 2*tskin
        x_pos = -5.*tstep + tstep2 - tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Second Row from Bottom, Right
        itower = 27 
        y_pos = -9.*tstep + tstep2 + tstep*1.0 -2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Third Row from Bottom, Left
        itower = 39 
        y_pos = -9.*tstep + tstep2 + tstep*2.0 - 2*tskin
        x_pos = -6.*tstep + tstep2 - tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Third Row from Bottom, Right
        itower = 45 
        y_pos = -9.*tstep + tstep2 + tstep*2.0 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fourth Row from Bottom, Left
        itower = 56 
        y_pos = -9.*tstep + tstep2 + tstep*3.0 - 2*tskin
        x_pos = -7.*tstep + tstep2 - tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fourth Row from Bottom, Right
        itower = 63 
        y_pos = -9.*tstep + tstep2 + tstep*3.0 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fifth Row from Bottom, Left
        itower = 73 
        y_pos = -9.*tstep + tstep2 + tstep*4.0 - 2*tskin
        x_pos = -8.*tstep + tstep2 - tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fifth Row from Bottom, Right
        itower = 81 
        y_pos = -9.*tstep + tstep2 + tstep*4.0 - 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Sixth Row from Bottom, Left
        itower = 91 
        y_pos = -9.*tstep + tstep2 + tstep*5.0
        x_pos = -8.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Sixth Row from Bottom, Right
        itower = 102 
        y_pos = -9.*tstep + tstep2 + tstep*5.0
        x_pos = 3.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Seventh Row from Bottom, Left
        itower = 108
        y_pos = -9.*tstep + tstep2 + tstep*6.0
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eigth Row from Bottom, Right
        itower = 121
        y_pos = -9.*tstep + tstep2 + tstep*6.0
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eigth Row from Bottom, Left
        itower = 126
        y_pos = -9.*tstep + tstep2 + tstep*7.0
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eigth Row from Bottom, Right
        itower = 139
        y_pos = -9.*tstep + tstep2 + tstep*7.0
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Ninth Row from Bottom, Left
        itower = 144
        y_pos = -9.*tstep + tstep2 + tstep*8.0
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Ninth Row from Bottom, Right
        itower = 157
        y_pos = -9.*tstep + tstep2 + tstep*8.0
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Tenth Row from Bottom, Left
        itower = 162
        y_pos = -9.*tstep + tstep2 + tstep*9.0
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Tenth Row from Bottom, Right
        itower = 175
        y_pos = -9.*tstep + tstep2 + tstep*9.0
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eleventh Row from Bottom, Left
        itower = 180
        y_pos = -9.*tstep + tstep2 + tstep*10.0
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eleventh Row from Bottom, Right
        itower = 193
        y_pos = -9.*tstep + tstep2 + tstep*10.0
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Twelth Row from Bottom, Left
        itower = 198
        y_pos = -9.*tstep + tstep2 + tstep*11.0
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Twelth Row from Bottom, Right
        itower = 211
        y_pos = -9.*tstep + tstep2 + tstep*11.0
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Thirteenth Row from Bottom, Left
        itower = 217
        y_pos = -9.*tstep + tstep2 + tstep*12.0
        x_pos = -8.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Thirteenth Row from Bottom, Right
        itower = 228
        y_pos = -9.*tstep + tstep2 + tstep*12.0
        x_pos = 3.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fourteenth Row from Bottom, Left
        itower = 235
        y_pos = -9.*tstep + tstep2 + tstep*13.0 + 2*tskin
        x_pos = -8.*tstep + tstep2 - tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fourteenth Row from Bottom, Right
        itower = 243
        y_pos = -9.*tstep + tstep2 + tstep*13.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fifteenth Row from Bottom, Left
        itower = 254
        y_pos = -9.*tstep + tstep2 + tstep*14.0 + 2*tskin
        x_pos = -7.*tstep + tstep2 - tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fifteenth Row from Bottom, Right
        itower = 261
        y_pos = -9.*tstep + tstep2 + tstep*14.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Sixteenth Row from Bottom, Left
        itower = 273
        y_pos = -9.*tstep + tstep2 + tstep*15.0 + 2*tskin
        x_pos = -6.*tstep + tstep2 - tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Sixteenth Row from Bottom, Right
        itower = 279
        y_pos = -9.*tstep + tstep2 + tstep*15.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Seventeenth Row from Bottom, Left
        itower = 292
        y_pos = -9.*tstep + tstep2 + tstep*16.0 + 2*tskin
        x_pos = -5.*tstep + tstep2 - tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Seventeenth Row from Bottom, Right
        itower = 297
        y_pos = -9.*tstep + tstep2 + tstep*16.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eighteenth Row from Bottom, Left
        itower = 312
        y_pos = -9.*tstep + tstep2 + tstep*17.0 + 2*tskin
        x_pos = -3.*tstep + tstep2 - tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eighteenth Row from Bottom, Right
        itower = 315
        y_pos = -9.*tstep + tstep2 + tstep*17.0 + 2*tskin
        x_pos = tstep2 + tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

        CALL RUN08_SMPC_CONTAINER

        return
        END

C=====================================================================
C    SUBROUTINE NAME run08_smpc_container
C.   author : Mickey Chiu
C=====================================================================
      subroutine run08_smpc_container
C=====================================================================
C
C    DESCRIPTION: This routine defines the container (skin,ribs) for 
C                 the South Muon-Piston Calorimeter, Run08 configuration. 
C
C=====================================================================

      integer ivolu
      integer skin_nmed
      real    skin_par(3)         ! skin parameters
      real    skin_loc(3)         ! skin location
      real    plate_par(3)        ! front/back plate parameters
      real    IN2CM               ! conversion from inch 2 cm
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

      data skin_nmed/2507/        ! Aluminum
      data skin_par/3*0./
      data skin_loc/3*0./
      data plate_par/3*0./
      data IN2CM/2.54/
      data tstep/2.26/
      data tskin/0.15875/
      data z_pos/-229.9475/
      data z_frontplate/-219.103/
      data z_backplate/-239.424/

      skin_rmax = 22.5-(1.0/16.0)*IN2CM
      skin_rmin = skin_rmax-tskin

      call gsvolu('SKIN','BOX ',skin_nmed,skin_par,0,ivolu)
      call gsatt('SKIN','SEEN',1)
      call gsatt('SKIN','COLO',4)

C SKIN1 (upper and lower vertical midpoint pieces)
      skin_par(1) = tskin - 0.00001
      skin_par(2) = (skin_rmin - (4.0*tstep+2.0*tskin))/2.0 - 0.00001
      skin_par(3) = 9.0

      x = 0.
      y = 4.0*tstep + 2.0*tskin + skin_par(2) + 0.00001/2.0
      z = z_pos
      call gsposp('SKIN',1,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      y = -y
      call gsposp('SKIN',2,'MUA2',x,y,z,0,'ONLY',skin_par,3)

C SKIN2 (upper and lower horizontal pieces)
      skin_par(1) = 8.0*tstep + 1.8
      skin_par(2) = tskin - 0.00001

      x = 0.
      y = 4.0*tstep + tskin
      call gsposp('SKIN',3,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      y = -y
      call gsposp('SKIN',4,'MUA2',x,y,z,0,'ONLY',skin_par,3)

C Front and back plate (use tube for now, even though inner region is square)

      call gsvolu('PLAT','TUBE',skin_nmed,plate_par,0,ivolu)
      call gsatt('PLAT','SEEN',1)
      call gsatt('PLAT','COLO',2)

      x = 0.
      y = 0.
      z = z_frontplate
      plate_par(1) = 4.0*tstep 
      plate_par(2) = skin_rmax - 0.00001
      plate_par(3) = (3.0/16.0)*IN2CM
      call gsposp('PLAT',5,'MUA2',x,y,z,0,'ONLY',plate_par,3)

      z = z_backplate
      call gsposp('PLAT',6,'MUA2',x,y,z,0,'ONLY',plate_par,3)

C Outer skin
      plate_par(1) = skin_rmin
      plate_par(2) = skin_rmax
      plate_par(3) = ((z_frontplate-z_backplate)-0.375*IN2CM)/2.0 - 0.00001
      z = (z_frontplate+z_backplate)/2.0      ! midway between front/back plates
      call gsposp('PLAT',7,'MUA2',x,y,z,0,'ONLY',plate_par,3)

      END

