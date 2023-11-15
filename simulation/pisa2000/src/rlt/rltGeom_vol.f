c... Relative Luminosity Telescope
c... Definitions for Volumes of RPCs
c...  
c... Authors:  M. C. McCain and L. A. Linden Levy 16.02.2004
c...
c... Order of RPC locations
c... RPC1 --> closest to beam pipe
c... RPC3 --> farthest from beam pipe
c...
c... Change Log:
c... 16.02.2004 -- simple volumes for primary testing
c...               3 rectangular prisms placed above the 
c...               interaction region
c    June 2013 HvH: rearranged the code: call gspos here, 
c                   change subroutines to entry
c
c...------------------------------------------------------------
c...  RPC1
c...  ___________ gas sandwiched between two glass plates
c..  NOTE:  Currently, RPCs only consist of gas volumes, no glass!
      
      SUBROUTINE RLT_RPC1_vol
      
      IMPLICIT NONE

#include "gcflag.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      real rltRPC1dx, rltRPC1dy, rltRPC1dz,
     *                rltRPC2dx, rltRPC2dy, rltRPC2dz,
     *                rltRPC3dx, rltRPC3dy, rltRPC3dz
      
      common /rltRPCsize/rltRPC1dx, rltRPC1dy, rltRPC1dz,
     *                rltRPC2dx, rltRPC2dy, rltRPC2dz,
     *                rltRPC3dx, rltRPC3dy, rltRPC3dz

      real rltRPC1xcent, rltRPC1ycent, rltRPC1zcent,
     *     rltRPC2xcent, rltRPC2ycent, rltRPC2zcent,
     *     rltRPC3xcent, rltRPC3ycent, rltRPC3zcent

      common /rltRPCpos/rltRPC1xcent, rltRPC1ycent, rltRPC1zcent,
     *               rltRPC2xcent, rltRPC2ycent, rltRPC2zcent,
     *               rltRPC3xcent, rltRPC3ycent, rltRPC3zcent

      real Par(3)
      character*4 rltRPC1shell/'RTS1'/           !-- Parent Volume Name (note upper case - HvH 2013)
      character*4 rltRPC1topPlane/'RTT1'/        !-- Top of Sandwich
      character*4 rltRPC1bottomPlane/'RTB1'/     !-- Bottom of Sandwich
      character*4 rltRPC1gasVolume/'RTG1'/       !-- Inner Gas Volume

      character*4 rltRPC2shell/'RTS2'/           !-- Parent Volume Name
      character*4 rltRPC2topPlane/'RTT2'/        !-- Top of Sandwich
      character*4 rltRPC2bottomPlane/'RTB2'/     !-- Bottom of Sandwich
      character*4 rltRPC2gasVolume/'RTG2'/       !-- Inner Gas Volume

      character*4 rltRPC3shell/'RTS3'/           !-- Parent Volume Name
      character*4 rltRPC3topPlane/'RTT3'/        !-- Top of Sandwich
      character*4 rltRPC3bottomPlane/'RTB3'/     !-- Bottom of Sandwich
      character*4 rltRPC3gasVolume/'RTG3'/       !-- Inner Gas Volume

      integer IVOLU
      integer rltRPCglass, rltRPC1gas, rltRPC2gas, rltRPC3gas
      common  /rltRPCmaterial/rltRPCglass, rltRPC1gas, 
     *                        rltRPC2gas, rltRPC3gas

c.. Shell Volume (Parent Container)
      
      Par(1)=rltRPC1dx
      Par(2)=rltRPC1dy
      Par(3)=rltRPC1dz
      CALL GSVOLU (rltRPC1shell,'BOX ',16, Par, 3, IVOLU)
      CALL GSATT (rltRPC1shell,'SEEN',1 )
      CALL GSATT (rltRPC1shell,'COLO', 2 )

      write (*,*) 'rts1 properties: ',rltrpc1shell, par

      CALL GSPOS(rltRPC1shell, 1, 'HALL', rltRPC1xcent, rltRPC1ycent,
     *           rltRPC1zcent, irotnull, 'ONLY')

      write (*,*) 'rts1 placement:', rltRPC1shell, irotnull,
     &       rltRPC1xcent, rltRPC1ycent,rltRPC1zcent


c.. 1) Top Plane Description
      
c      Par(1)= 
c      Par(2)= 
c      Par(3)= 
      
c      CALL GSVOLU (rltRPC1topPlane,'BOX ',rltRPCglass, Par, 3, IVOLU)
c      CALL GSATT (rltRPC1topPlane,'SEEN',1 )
c      CALL GSATT (rltRPC1topPlane,'COLO',4 )

c.. 2) Bottom Plane Description

c      Par(1)=
c      Par(2)=
c      Par(3)=

c      CALL GSVOLU (rltRPC1bottomPlane,'BOX ',rltRPCglass, Par, 3, IVOLU)
c      CALL GSATT (rltRPC1bottomPlane,'SEEN',1 )
c      CALL GSATT (rltRPC1bottomPlane,'COLO',4 )

c.. 3) Sensitive volume: Gas
c..    Currently, the gas volume is equal to the shell volume
      
      Par(1)=rltRPC1dx
      Par(2)=rltRPC1dy
      Par(3)=rltRPC1dz
      
      CALL GSVOLU (rltRPC1gasVolume,'BOX ',rltRPC1gas, Par, 3, IVOLU)
      CALL GSATT (rltRPC1gasVolume,'SEEN',1 )
      CALL GSATT (rltRPC1gasVolume,'COLO',3 )

c.. Put separate volumes into shell
c.. Currently, top/bottom planes not placed in shell
      
c      CALL GSPOS(rltRPC1topPlane, 1, rltRPC1shell, 0, 0,-uPC1_DZ, 
c     *           1, 'ONLY')
      CALL GSPOS(rltRPC1gasVolume, 1, rltRPC1shell, 0, 0, 0, 
     *           0, 'ONLY')
c      CALL GSPOS(rltRPC1bottomPlane, 1, rltRPC1shell, 
c     *           0, 0, 0.0, 1, 'ONLY')

      return 

c...------------------------------------------------------------
c...  RPC2
c...  ___________ gas sandwiched between two glass plates
c..  NOTE:  Currently, RPCs only consist of gas volumes, no glass!
      
      entry RLT_RPC2_vol
      
c.. Shell Volume (Parent Container)
      
      Par(1)=rltRPC2dx
      Par(2)=rltRPC2dy
      Par(3)=rltRPC2dz
      CALL GSVOLU (rltRPC2shell,'BOX ',16, Par, 3, IVOLU)
      CALL GSATT (rltRPC2shell,'SEEN',1 )
      CALL GSATT (rltRPC2shell,'COLO', 2 )

      CALL GSPOS(rltRPC2shell, 1, 'HALL', rltRPC2xcent, rltRPC2ycent,
     *           rltRPC2zcent, 0, 'ONLY')

c.. 1) Top Plane Description
c.. 2) Bottom Plane Description
c.. 3) Sensitive volume: Gas
c..    Currently, the gas volume is equal to the shell volume
      
      Par(1)=rltRPC2dx
      Par(2)=rltRPC2dy
      Par(3)=rltRPC2dz
      
      CALL GSVOLU (rltRPC2gasVolume,'BOX ',rltRPC2gas, Par, 3, IVOLU)
      CALL GSATT (rltRPC2gasVolume,'SEEN',1 )
      CALL GSATT (rltRPC2gasVolume,'COLO',3 )

c.. Put separate volumes into shell
c.. Currently, top/bottom planes not placed in shell
      
c      CALL GSPOS(rltRPC2topPlane, 1, rltRPC2shell, 0, 0,-uPC1_DZ, 
c     *           1, 'ONLY')
      CALL GSPOS(rltRPC2gasVolume, 1, rltRPC2shell, 0, 0, 0, 
     *           0, 'ONLY')
c      CALL GSPOS(rltRPC2bottomPlane, 1, rltRPC2shell, 
c     *           0, 0, 0.0, 1, 'ONLY')
      return 

c...------------------------------------------------------------
c...  RPC3
c...  ___________ gas sandwiched between two glass plates
c..  NOTE:  Currently, RPCs only consist of gas volumes, no glass!
      
      entry RLT_RPC3_vol
      
c.. Shell Volume (Parent Container)
      
      Par(1)=rltRPC3dx
      Par(2)=rltRPC3dy
      Par(3)=rltRPC3dz
      CALL GSVOLU (rltRPC3shell,'BOX ',16, Par, 3, IVOLU)
      CALL GSATT (rltRPC3shell,'SEEN',1 )
      CALL GSATT (rltRPC3shell,'COLO', 2 )

c.. 1) Top Plane Description
c.. 2) Bottom Plane Description
c.. 3) Sensitive volume: Gas
c..    Currently, the gas volume is equal to the shell volume
      
      Par(1)=rltRPC3dx
      Par(2)=rltRPC3dy
      Par(3)=rltRPC3dz
      
      CALL GSVOLU (rltRPC3gasVolume,'BOX ', rltRPC3gas, Par, 3, IVOLU)
      CALL GSATT (rltRPC3gasVolume,'SEEN',1 )
      CALL GSATT (rltRPC3gasVolume,'COLO',3 )

      CALL GSPOS(rltRPC3shell, 1, 'HALL', rltRPC3xcent, rltRPC3ycent,
     *           rltRPC3zcent, 0, 'ONLY')

c.. Put separate volumes into shell
c.. Currently, top/bottom planes not placed in shell
      
c      CALL GSPOS(rltRPC3topPlane, 1, rltRPC3shell, 0, 0,-uPC1_DZ, 
c     *           1, 'ONLY')
      CALL GSPOS(rltRPC3gasVolume, 1, rltRPC3shell, 0, 0, 0, 
     *           0, 'ONLY')
c      CALL GSPOS(rltRPC3bottomPlane, 1, rltRPC3shell, 
c     *           0, 0, 0.0, 1, 'ONLY')
      return 
      end
