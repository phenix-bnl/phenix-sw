* volum of pad chamber for Muon trigger upgrade
*-- Author :    Wei Xie  09/25/2003
c----------------------------------------------------------
c     volume definition for utrgPC1
c     make it simple now. Just two S2 sheet + gas volume
c     wires and cathod strip will be dealt with in response code
c May 2013 HvH: disabled pc2
c Nov 2014 HvH: fixed zero_thichness sheets UST1
 
      SUBROUTINE utrgPC1_vol

      IMPLICIT NONE
#include "gcflag.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      real uPC1_rin, uPC1_rout, uPC1_DZ, 
     *     uPC2N_rin, uPC2N_rout, uPC2N_DZ,
     *     uPC2S_rin, uPC2S_rout, uPC2S_DZ,
     *     uPC3N_rin, uPC3N_rout, uPC3N_DZ,
     *     uPC3S_rin, uPC3S_rout, uPC3S_DZ

      common /size/uPC1_rin, uPC1_rout, uPC1_DZ, 
     *             uPC2N_rin, uPC2N_rout, uPC2N_DZ, 
     *             uPC2S_rin, uPC2S_rout, uPC2S_DZ,
     *             uPC3N_rin, uPC3N_rout, uPC3N_DZ, 
     *             uPC3S_rin, uPC3S_rout, uPC3S_DZ

      integer S2_glass, IST
      integer MuPC1Gas, MuPC3Gas    ! mupc2gas removed
      common  /material_utrPC/S2_glass, MuPC1Gas, MuPC3Gas ! mupc2gas removed

      real tt1, tt2, tt3, ff1, ff2, ff3, sheet_halfthk
      real Par(10)
      character*4 utrgPC1_sheet/'UST1'/
      character*4 utrgPC1_north_gas/'UG1N'/
      character*4 utrgPC1_south_gas/'UG1S'/
      character*4 utrgPC1_north_name/'UP1N'/ 
      character*4 utrgPC1_south_name/'UP1S'/ 

c      character*4 utrgPC2_sheet/'UST2'/
c      character*4 utrgPC2_north_gas/'UG2N'/
c      character*4 utrgPC2_north_name/'UP2N'/
c      character*4 utrgPC2_south_gas/'UG2S'/
c      character*4 utrgPC2_south_name/'UP2S'/

      character*4 utrgPC3_sheet/'UST3'/
      character*4 utrgPC3_north_gas/'UG3N'/
      character*4 utrgPC3_north_name/'UP3N'/
      character*4 utrgPC3_south_gas/'UG3S'/
      character*4 utrgPC3_south_name/'UP3S'/

      real utrgPC1_north, utrgPC1_south,
     &     utrgPC2_north, utrgPC2_south,
     *     utrgPC3_north, utrgPC3_south

      common /uPCpos/utrgPC1_north, utrgPC1_south,
     *               utrgPC2_north, utrgPC2_south,
     *               utrgPC3_north, utrgPC3_south

*----------------------------------------------------------------------------------------*

C     the local "shell" volume with medium vacuum
      Par(1)=-22.5
      Par(2)=360.
      Par(3)=8.
      Par(4)=2.
      Par(5)=-uPC1_DZ
      Par(6)=uPC1_rin
      Par(7)=uPC1_rout
      Par(8)=uPC1_DZ
      Par(9)=Par(6)
      Par(10)=Par(7)
      CALL GSVOLU (utrgPC1_north_name,'PGON',16, Par, 10, IST)
      CALL GSATT (utrgPC1_north_name,'SEEN',1 )
      CALL GSATT (utrgPC1_north_name,'COLO', 2 )
      CALL GSVOLU (utrgPC1_south_name,'PGON',16, Par, 10, IST)
      CALL GSATT (utrgPC1_south_name,'SEEN',1 )
      CALL GSATT (utrgPC1_south_name,'COLO', 2 )

C.. 1) Ground cathode panel: S2-glass-epoxy sheet UST1
      sheet_halfthk = 0.005
      Par(5) = -sheet_halfthk                    ! was +0.05 
      Par(8) =  sheet_halfthk                    ! was +0.05 07nov14 HvH
      CALL GSVOLU (utrgPC1_sheet,'PGON ',S2_glass, Par, 10, IST)
      CALL GSATT (utrgPC1_sheet,'SEEN',1 )
      CALL GSATT (utrgPC1_sheet,'COLO',4 )

C.. 2) Sensitive volume: Gas (Argon-Ethane 50%-50%) 6 mm thick
      Par(5) =-(uPC1_DZ - 0.05*2)
      Par(8) = - par(5)
      CALL GSVOLU (utrgPC1_north_gas,'PGON ',MuPC1Gas, Par, 10, IST)
      CALL GSATT (utrgPC1_north_gas,'SEEN',1 )
      CALL GSATT (utrgPC1_north_gas,'COLO',4 )
      CALL GSVOLU (utrgPC1_south_gas,'PGON ',MuPC1Gas, Par, 10, IST)
      CALL GSATT (utrgPC1_south_gas,'SEEN',1 )
      CALL GSATT (utrgPC1_south_gas,'COLO',4 )

c   now put the sandwich into local shell volume
      
      CALL GSPOS(utrgPC1_sheet, 1, utrgPC1_north_name, 0, 0,
     &  -uPC1_DZ + sheet_halfthk,  1, 'ONLY')   ! UP1S
      CALL GSPOS(utrgPC1_sheet, 2, utrgPC1_north_name, 0, 0,
     &   uPC1_DZ - sheet_halfthk,  1, 'ONLY')
      CALL GSPOS(utrgPC1_north_gas, 1, utrgPC1_north_name, 
     *           0, 0, 0.0, 1, 'ONLY')

      CALL GSPOS(utrgPC1_sheet, 3, utrgPC1_south_name, 0, 0,
     &  -uPC1_DZ + sheet_halfthk,   1, 'ONLY')
      CALL GSPOS(utrgPC1_sheet, 4, utrgPC1_south_name, 0, 0, 
     &   uPC1_DZ - sheet_halfthk,   1, 'ONLY')
      CALL GSPOS(utrgPC1_south_gas, 1, utrgPC1_south_name, 
     *           0, 0, 0.0, 1, 'ONLY')

      return 

c----------------------------------------------------------
*     **NOTE** this section (RPC2) is no longer called 
     
c----------------------------------------------------------
c     volume definition for RPC3, utrgPC3
c     make it simple now. Just two S2 sheet + gas volume
c     wires and cathod strip will be dealt with in response code 

      entry utrgPC3_vol

C     the local "shell" volume with medium vacuum
      Par(1)=-22.5
      Par(2)=360.
      Par(3)=8.
      Par(4)=2.
      Par(5)=-uPC3N_DZ
      Par(6)=uPC3N_rin
      Par(7)=uPC3N_rout
      Par(8)=uPC3N_DZ
      Par(9)=Par(6)
      Par(10)=Par(7)
      CALL GSVOLU (utrgPC3_north_name,'PGON',16, Par, 10, IST)
      CALL GSATT (utrgPC3_north_name,'SEEN',1 )
      CALL GSATT (utrgPC3_north_name,'COLO', 2 )
      Par(1)=-22.5
      Par(2)=360.
      Par(3)=8.
      Par(4)=2.
      Par(5)=-uPC3S_DZ
      Par(6)=uPC3S_rin
      Par(7)=uPC3S_rout
      Par(8)=uPC3S_DZ
      Par(9)=Par(6)
      Par(10)=Par(7)
      CALL GSVOLU (utrgPC3_south_name,'PGON',16, Par, 10, IST)
      CALL GSATT (utrgPC3_south_name,'SEEN',1 )
      CALL GSATT (utrgPC3_south_name,'COLO', 2 )

C.. 1) Ground cathode panel: S2-glass-epoxy sheet
      Par(5) = -sheet_halfthk   ! was 0.05
      Par(8) =  sheet_halfthk   ! was 0.05 07nov14 HvH
      CALL GSVOLU (utrgPC3_sheet,'PGON ',S2_glass, Par, 10, IST)
      CALL GSATT (utrgPC3_sheet,'SEEN',1 )
      CALL GSATT (utrgPC3_sheet,'COLO',4 )

C.. 2) Sensitive volume: Gas (Argon-Ethane 50%-50%) 6 mm thick
      Par(5) =-(uPC3N_DZ - 0.05*2)
      Par(8) = - par(5)
      CALL GSVOLU (utrgPC3_north_gas,'PGON ',MuPC3Gas, Par, 10, IST)
      CALL GSATT (utrgPC3_north_gas,'SEEN',1 )
      CALL GSATT (utrgPC3_north_gas,'COLO',4 )
      Par(5) =-(uPC3S_DZ - 0.05*2)
      Par(8) =uPC3S_DZ - 0.05*2
      CALL GSVOLU (utrgPC3_south_gas,'PGON ',MuPC3Gas, Par, 10, IST)
      CALL GSATT (utrgPC3_south_gas,'SEEN',1 )
      CALL GSATT (utrgPC3_south_gas,'COLO',4 )

c   now put the sandwich into local shell volume
      
      CALL GSPOS(utrgPC3_sheet, 1, utrgPC3_north_name, 0, 0,
     &          -uPC3N_DZ + sheet_halfthk, 1, 'ONLY')
      CALL GSPOS(utrgPC3_sheet, 2, utrgPC3_north_name, 0, 0,
     &           uPC3N_DZ - sheet_halfthk, 1, 'ONLY')
      CALL GSPOS(utrgPC3_north_gas, 1, utrgPC3_north_name, 
     *           0, 0, 0.0, 1, 'ONLY')

      CALL GSPOS(utrgPC3_sheet, 3, utrgPC3_south_name, 0, 0,
     &   -uPC3S_DZ + sheet_halfthk, 1, 'ONLY')
      CALL GSPOS(utrgPC3_sheet, 4, utrgPC3_south_name, 0, 0, 
     &    uPC3S_DZ - sheet_halfthk, 1, 'ONLY')
      CALL GSPOS(utrgPC3_south_gas, 1, utrgPC3_south_name, 
     *           0, 0, 0.0, 1, 'ONLY')



      CALL GSPOS(utrgPC1_north_name, 1, 'HALL', 0, 0, utrgPC1_north,
     *           1, 'ONLY')
      CALL GSPOS(utrgPC1_south_name, 1, 'HALL', 0, 0, utrgPC1_south,
     *           1, 'ONLY')
c      CALL GSPOS(utrgPC2_north_name, 1, 'HALL', 0, 0, utrgPC2_north,
c     *           1, 'ONLY')
c      CALL GSPOS(utrgPC2_south_name, 1, 'HALL', 0, 0, utrgPC2_south,
c     *           1, 'ONLY')
      CALL GSPOS(utrgPC3_north_name, 1, 'HALL', 0, 0, utrgPC3_north,
     *           1, 'ONLY')
      CALL GSPOS(utrgPC3_south_name, 1, 'HALL', 0, 0, utrgPC3_south,
     *           1, 'ONLY')

      return 
      end
