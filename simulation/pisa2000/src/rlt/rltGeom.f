c $Id: rltGeom.f,v 1.4 2013/05/29 15:27:44 hubert Exp $
C...  Relative Luminosity Telescope Geometry
C...  for Longitudonal Double Spin Asymmetry
C...  Required file(s): RLT_RPC_coords.txt --- RPC locations and 
C...  size in phenix coordinate space.
C...
C.... Authors: M. C. McCain and L. A. Linden Levy 16.02.2004
C...  include beer.inc
C...  
C...  ============================================================
C...  CHANGELOG:
C...  04.03.04 Alex: Moved RLT settings into the phnx.par 
C...                 file with help from Harshil.
*     May 2013 HvH moved the _pos.f code into the _vol.f code.
C...  ============================================================


      SUBROUTINE RLTGeom

      IMPLICIT NONE

      character*50 par_file

      real rltRPC1xcent, rltRPC1ycent, rltRPC1zcent,
     *     rltRPC2xcent, rltRPC2ycent, rltRPC2zcent,
     *     rltRPC3xcent, rltRPC3ycent, rltRPC3zcent

      common /rltRPCpos/rltRPC1xcent, rltRPC1ycent, rltRPC1zcent,
     *               rltRPC2xcent, rltRPC2ycent, rltRPC2zcent,
     *               rltRPC3xcent, rltRPC3ycent, rltRPC3zcent

      real rltRPC1dx, rltRPC1dy, rltRPC1dz,
     *     rltRPC2dx, rltRPC2dy, rltRPC2dz,
     *     rltRPC3dx, rltRPC3dy, rltRPC3dz

      common /rltRPCsize/rltRPC1dx, rltRPC1dy, rltRPC1dz,
     *                rltRPC2dx, rltRPC2dy, rltRPC2dz,
     *                rltRPC3dx, rltRPC3dy, rltRPC3dz
      
      namelist /rlt_coord/ rltRPC1xcent,rltRPC1ycent,
     *         rltRPC1zcent,
     *     rltRPC2xcent, rltRPC2ycent,rltRPC2zcent,
     *     rltRPC3xcent,rltRPC3ycent,rltRPC3zcent,
     *     rltRPC1dx,rltRPC1dy,rltRPC1dz,
     *     rltRPC2dx,rltRPC2dy,rltRPC2dz,
     *     rltRPC3dx,rltRPC3dy,rltRPC3dz
       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

c ------------------------------------------------------------------------------


*     retrieve geometry
      write( *,* ) 'rlt - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = rlt_coord, err = 2004 )


      CALL RLTGeom_material     !... Material Definitions
      CALL RLT_RPC1_vol         !... Inner most RPC volume etc. in file rltGeom_vol.f
      CALL RLT_RPC2_vol         !... Called from rltGeom_vol.f  in file rltGeom_vol.f
      CALL RLT_RPC3_vol     
cxxx  CALL RLTGeom_pos          !... Position Detector in PHENIX (moved into *_vol.f)
      CALL RLT_det
      
2004  print*, 'RPC1: rltRPC1xcent=', rltRPC1xcent, 
     *        ', rltRPC1ycent=', rltRPC1ycent, 
     *        ', rltRPC1zcent=', rltRPC1zcent
      print *,'      rltRPC1dx=', rltRPC1dx,
     *        ', rltRPC1dy=', rltRPC1dy,
     *        ', rltRPC1dz=', rltRPC1dz

      print*, 'RPC2: rltRPC2xcent=', rltRPC2xcent, 
     *        ', rltRPC2ycent=', rltRPC2ycent, 
     *        ', rltRPC2zcent=', rltRPC2zcent
      print*,'      rltRPC2dx=', rltRPC2dx,
     *        ', rltRPC2dy=', rltRPC2dy,
     *        ', rltRPC2dz=', rltRPC2dz

      print*, 'RPC3: rltRPC3xcent=', rltRPC3xcent, 
     *        ', rltRPC3ycent=', rltRPC3ycent, 
     *        ', rltRPC3zcent=', rltRPC3zcent
      print*,'      rltRPC3dx=', rltRPC3dx,
     *        ', rltRPC3dy=', rltRPC3dy,
     *        ', rltRPC3dz=', rltRPC3dz

      RETURN
      END
