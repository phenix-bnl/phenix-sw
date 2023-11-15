* position utrPC into muon arm  
*-- Author :    Wei Xie  09/25/2003
* May 2013 HvH - Since there were only 3 executable statements in here, I moved
*                them into MuonPadGeom_vol.f
*                *** THIS CODE IS NO LONGER CALLED ***

      SUBROUTINE MuonPadGeom_pos
 
      IMPLICIT NONE
#include "gcflag.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      real utrgPC1_north, utrgPC1_south, utrgPC2_north, utrgPC2_south,
     *     utrgPC3_north, utrgPC3_south

      common /uPCpos/utrgPC1_north, utrgPC1_south,
     *               utrgPC2_north, utrgPC2_south,
     *               utrgPC3_north, utrgPC3_south

      character*4 utrgPC1_north_name/'UP1N'/
      character*4 utrgPC2_north_name/'UP2N'/
      character*4 utrgPC3_north_name/'UP3N'/
      character*4 utrgPC1_south_name/'UP1S'/
      character*4 utrgPC2_south_name/'UP2S'/
      character*4 utrgPC3_south_name/'UP3S'/
 
c      CALL GSPOS(utrgPC1_north_name, 1, 'MUA1', 0, 0, utrgPC1_north, 
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC1_south_name, 2, 'MUA2', 0, 0, utrgPC1_south, 
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC2_north_name, 1, 'HALL', 0, 0, utrgPC2_north, 
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC2_south_name, 2, 'HALL', 0, 0, utrgPC2_south, 
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC3_north_name, 1, 'HALL', 0, 0, utrgPC3_north, 
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC3_south_name, 2, 'HALL', 0, 0, utrgPC3_south, 
c     *           1, 'ONLY')
c     also when placing the muPC outside of the MUA1/2, one need to position
c     it inside HALL insdead of MUA1/2, otherwise there'll be not hits output
c     when placing muPC in the HALL, one just do the following. No need to 
c     change the other code. Note: make the chamber position overlap with 
c     e.g. MUA1, otherwise there'll be no hits output

cxx      CALL GSPOS(utrgPC1_north_name, 1, 'HALL', 0, 0, utrgPC1_north,
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC1_south_name, 1, 'HALL', 0, 0, utrgPC1_south,
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC2_north_name, 1, 'HALL', 0, 0, utrgPC2_north,
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC2_south_name, 1, 'HALL', 0, 0, utrgPC2_south,
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC3_north_name, 1, 'HALL', 0, 0, utrgPC3_north,
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC3_south_name, 1, 'HALL', 0, 0, utrgPC3_south,
cxx     *           1, 'ONLY')

      return 
      end
