c $Id: run06_smpc.f,v 1.4 2010/01/06 22:37:02 chiu Exp $

C=====================================================================
C    FILE NAME run06_mpc
C.   author : Mickey Chiu
C    July 16, 2007
C.   Run06 Muon Piston Calorimeter, consists of original south MPC only.
C=====================================================================
      subroutine run06_smpc
C=====================================================================

C    DESCRIPTION: This routine defines the geometry for the original
C                 version of the South Muon-Piston Calorimeter.

C    MAP:
C           1) CALLED BY MPC.F

C=====================================================================


C      include 'gclist.inc'
C      include 'gconst.inc'   
C      include 'gcflag.inc'   
C      include 'gcvolu.inc'
C      include 'guphnx.inc'
C      include 'sublink.inc'
C      include 'fmpclink.inc'
C      include 'fpmlink.inc'       ! mutr header, for z_roll()

C  Volume descriptors
      real  tstep                ! step size between towers
      real  tstep2               ! 1/2 step size between towers
      real  tskin                ! thickness of Al skin of modules
      real  toffset              ! x offset of middle segments
      integer ntowers            ! total number of towers in arm
C      data  tstep/2.235/
C      data  tstep2/1.1175/
      data  tstep/2.26/
      data  tstep2/1.13/
      data  tskin/0.15875/
      data  toffset/0.3055/
      data  ntowers/240/

      real x_pos,y_pos,z_pos
      data z_pos/231.083/
      integer itower


C ** Bottom Row, Left
        itower = 6
        y_pos = -9.*tstep + tstep2 - 3*tskin
        x_pos = -3.*tstep + tstep2 - tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Bottom Row, Right
        y_pos = -9.*tstep + tstep2 - 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Second Row from Bottom, Left
        itower = 22 
        y_pos = -9.*tstep + tstep2 + tstep*1.0 - 3*tskin
        x_pos = -5.*tstep + tstep2 - tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Second Row from Bottom, Right
        itower = 27 
        y_pos = -9.*tstep + tstep2 + tstep*1.0 -3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Third Row from Bottom, Left
        itower = 39 
        y_pos = -9.*tstep + tstep2 + tstep*2.0 - 3*tskin
        x_pos = -6.*tstep + tstep2 - tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Third Row from Bottom, Right
        itower = 45 
        y_pos = -9.*tstep + tstep2 + tstep*2.0 - 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fourth Row from Bottom, Left
        itower = 56 
        y_pos = -9.*tstep + tstep2 + tstep*3.0 - 3*tskin
        x_pos = -7.*tstep + tstep2 - tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fourth Row from Bottom, Right
        itower = 63 
        y_pos = -9.*tstep + tstep2 + tstep*3.0 - 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fifth Row from Bottom, Left
        itower = 73 
        y_pos = -9.*tstep + tstep2 + tstep*4.0 - 3*tskin
        x_pos = -8.*tstep + tstep2 - tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fifth Row from Bottom, Right
        itower = 81 
        y_pos = -9.*tstep + tstep2 + tstep*4.0 - 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Sixth Row from Bottom, Left
        itower = 91 
        y_pos = -9.*tstep + tstep2 + tstep*5.0 - tskin
        x_pos = -8.*tstep + tstep2 - tskin - toffset
        DO I = 1,4
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Sixth Row from Bottom, Right
        itower = 103 
        y_pos = -9.*tstep + tstep2 + tstep*5.0 - tskin
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,4
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Seventh Row from Bottom, Left
        itower = 108
        y_pos = -9.*tstep + tstep2 + tstep*6.0 - tskin
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eigth Row from Bottom, Right
        itower = 121
        y_pos = -9.*tstep + tstep2 + tstep*6.0 - tskin
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eigth Row from Bottom, Left
        itower = 126
        y_pos = -9.*tstep + tstep2 + tstep*7.0 - tskin
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eigth Row from Bottom, Right
        itower = 139
        y_pos = -9.*tstep + tstep2 + tstep*7.0 - tskin
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Ninth Row from Bottom, Left
        itower = 144
        y_pos = -9.*tstep + tstep2 + tstep*8.0 - tskin
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Ninth Row from Bottom, Right
        itower = 157
        y_pos = -9.*tstep + tstep2 + tstep*8.0 - tskin
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Tenth Row from Bottom, Left
        itower = 162
        y_pos = -9.*tstep + tstep2 + tstep*9.0 + tskin
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Tenth Row from Bottom, Right
        itower = 175
        y_pos = -9.*tstep + tstep2 + tstep*9.0 + tskin
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eleventh Row from Bottom, Left
        itower = 180
        y_pos = -9.*tstep + tstep2 + tstep*10.0 + tskin
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eleventh Row from Bottom, Right
        itower = 193
        y_pos = -9.*tstep + tstep2 + tstep*10.0 + tskin
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Twelth Row from Bottom, Left
        itower = 198
        y_pos = -9.*tstep + tstep2 + tstep*11.0 + tskin
        x_pos = -9.*tstep + tstep2 - tskin - toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Twelth Row from Bottom, Right
        itower = 211
        y_pos = -9.*tstep + tstep2 + tstep*11.0 + tskin
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Thirteenth Row from Bottom, Left
        itower = 217
        y_pos = -9.*tstep + tstep2 + tstep*12.0 + tskin
        x_pos = -8.*tstep + tstep2 - tskin - toffset
        DO I = 1,4
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Thirteenth Row from Bottom, Right
        itower = 229
        y_pos = -9.*tstep + tstep2 + tstep*12.0 + tskin
        x_pos = 4.*tstep + tstep2 + tskin + toffset
        DO I = 1,4
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fourteenth Row from Bottom, Left
        itower = 235
        y_pos = -9.*tstep + tstep2 + tstep*13.0 + 3*tskin
        x_pos = -8.*tstep + tstep2 - tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fourteenth Row from Bottom, Right
        itower = 243
        y_pos = -9.*tstep + tstep2 + tstep*13.0 + 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,8
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fifteenth Row from Bottom, Left
        itower = 254
        y_pos = -9.*tstep + tstep2 + tstep*14.0 + 3*tskin
        x_pos = -7.*tstep + tstep2 - tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Fifteenth Row from Bottom, Right
        itower = 261
        y_pos = -9.*tstep + tstep2 + tstep*14.0 + 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,7
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Sixteenth Row from Bottom, Left
        itower = 273
        y_pos = -9.*tstep + tstep2 + tstep*15.0 + 3*tskin
        x_pos = -6.*tstep + tstep2 - tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Sixteenth Row from Bottom, Right
        itower = 279
        y_pos = -9.*tstep + tstep2 + tstep*15.0 + 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,6
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Seventeenth Row from Bottom, Left
        itower = 292
        y_pos = -9.*tstep + tstep2 + tstep*16.0 + 3*tskin
        x_pos = -5.*tstep + tstep2 - tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Seventeenth Row from Bottom, Right
        itower = 297
        y_pos = -9.*tstep + tstep2 + tstep*16.0 + 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,5
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eighteenth Row from Bottom, Left
        itower = 312
        y_pos = -9.*tstep + tstep2 + tstep*17.0 + 3*tskin
        x_pos = -3.*tstep + tstep2 - tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

C ** Eighteenth Row from Bottom, Right
        itower = 315
        y_pos = -9.*tstep + tstep2 + tstep*17.0 + 3*tskin
        x_pos = tstep2 + tskin
        DO I = 1,3
          CALL GSPOS('PBO4',itower,'MUA2',x_pos,y_pos,-z_pos,0,'ONLY')
C          print *, 'impc ', itower, ' ', x_pos, ' ', y_pos
          x_pos = x_pos + tstep
          itower = itower + 1
        ENDDO 

        CALL RUN06_SMPC_CONTAINER
        return
        END

C=====================================================================
C    FILE NAME mpc_container
C.   author : Mickey Chiu
C    Nov 10, 2005
C.   Muon Piston Calorimeter.
C=====================================================================
      subroutine run06_smpc_container
C=====================================================================

C    DESCRIPTION: This routine defines the container (skin,ribs) for 
C                 the Muon-Piston Calorimeter. 

C=====================================================================

      integer ivolu
      integer skin_nmed
      real    skin_par(3)         ! skin parameters
      real    skin_loc(3)         ! skin location
      real    plate_par(3)         ! skin parameters
      real    IN2CM               ! conversion from in 2 cm
      real    x
      real    y
      real    z

      data skin_nmed/2507/           ! Tracking Medium, Al
      data skin_par/3*0./
      data skin_loc/3*0./
      data plate_par/3*0./
      data IN2CM/2.54/
      

      call gsvolu('SKIN','BOX ',skin_nmed,skin_par,0,ivolu)
      call gsatt('SKIN','SEEN',1)
      call gsatt('SKIN','COLO',4)

C SKIN1
      skin_par(1) = ((5.032-0.0625)/2.)*IN2CM
      skin_par(2) = 0.0625*IN2CM - 0.001
      skin_par(3) = (7.60/2.)*IN2CM    ! dist bet. front/back plates

      x = ((7.28/2.) + 0.0625)*IN2CM + skin_par(1)
      y = 0.
C      z = -231.665
      z = -230.394
      call gsposp('SKIN',1,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      x = -x
      call gsposp('SKIN',2,'MUA2',x,y,z,0,'ONLY',skin_par,3)

C SKIN2
      skin_par(1) = ((4.188-0.0625)/2.)*IN2CM
      skin_par(2) = (0.0625/2.)*IN2CM - 0.001

      x = ((7.28/2.) + 0.0625)*IN2CM + skin_par(1)
      y = 2.235*4 + 0.0625*1.5*IN2CM
      call gsposp('SKIN',3,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      x = -x
      call gsposp('SKIN',4,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      y = -y
      call gsposp('SKIN',5,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      x = -x
      call gsposp('SKIN',6,'MUA2',x,y,z,0,'ONLY',skin_par,3)

C SKIN3
      skin_par(1) = 7.825*IN2CM
      skin_par(2) = (0.0625/2.)*IN2CM - 0.001

      x = 0.
      y = 2.235*4 + 0.0625*2.5*IN2CM
      call gsposp('SKIN',7,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      y = -y
      call gsposp('SKIN',8,'MUA2',x,y,z,0,'ONLY',skin_par,3)

C SKIN4
      skin_par(1) = (0.0625/2.)*IN2CM - 0.001
      skin_par(2) = 2*0.0625*IN2CM + 4*2.235

      x = ((7.280 + 0.0625)/2.)*IN2CM
      y = 0.
      call gsposp('SKIN',9,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      x = -x
      call gsposp('SKIN',10,'MUA2',x,y,z,0,'ONLY',skin_par,3)

C SKIN5
      skin_par(1) = 0.0625*IN2CM - 0.001
      skin_par(2) = (4.9635/2.)*IN2CM

      x = 0.
      y = (7.280/2. + 0.0625)*IN2CM + skin_par(2)
      call gsposp('SKIN',11,'MUA2',x,y,z,0,'ONLY',skin_par,3)

      y = -y
      call gsposp('SKIN',12,'MUA2',x,y,z,0,'ONLY',skin_par,3)

C Front and back plate (use tube, even though inner region is square)
      plate_par(1) = (7.280/2.)*IN2CM
      plate_par(2) = 8.609*IN2CM
      plate_par(3) = (0.375/2.)*IN2CM

      call gsvolu('PLAT','TUBE',skin_nmed,plate_par,3,ivolu)
      call gsatt('PLAT','SEEN',1)
      call gsatt('PLAT','COLO',4)

      x = 0
      y = 0
C      z = -221.509
      z = -220.238
      call gspos('PLAT',13,'MUA2',x,y,z,0,'ONLY')

C      z = -241.83
      z = -240.559
      call gspos('PLAT',14,'MUA2',x,y,z,0,'ONLY')

      END
