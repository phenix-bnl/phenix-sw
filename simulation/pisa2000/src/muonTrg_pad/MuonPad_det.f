      SUBROUTINE MuonPad_det 
*
*     Mar 2015 HvH: remove RPC2
*
      IMPLICIT NONE

#include "gugeom.inc"

      INTEGER IS, ID, arm, ich
      CHARACTER*4 NAMEH1(13), MFPD(4)
      CHARACTER*4 NAMECH
      character*4 utrgPC1_north_name/'UP1N'/   ! changed to uppercase
      character*4 utrgPC2_north_name/'UP2N'/   ! jul 2013 HvH
      character*4 utrgPC3_north_name/'UP3N'/
      character*4 utrgPC1_north_gas/'UG1N'/
      character*4 utrgPC2_north_gas/'UG2N'/
      character*4 utrgPC3_north_gas/'UG3N'/
      character*4 utrgPC1_south_name/'UP1S'/
      character*4 utrgPC2_south_name/'UP2S'/
      character*4 utrgPC3_south_name/'UP3S'/
      character*4 utrgPC1_south_gas/'UG1S'/
      character*4 utrgPC2_south_gas/'UG2S'/
      character*4 utrgPC3_south_gas/'UG3S'/

      integer idtype/4321/
      INTEGER NBITS1(4), NBITH1(13)
      REAL FACT1(13), ORIG1(13)

      DATA MFPD /'HALL', '    ', '    ', '    '/

      DATA NBITS1/ 4* 4/
      DATA NAMEH1/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ',
     >     'TOF ', 'PTID', 'DELE' , 'XG1', 'YG1', 'ZG1', 'PL' /

      DATA NBITH1/ 13* 32 /
      DATA ORIG1/ 6*1000., 3* 0., 3*1000., 0./
      DATA FACT1/ 7*1000., 1., 5*1000./

      do arm = 1, 2 
c        do ich = 1, 3
         do ich = 1, 3, 2      ! skip over RPC2 - no longer exists. Mar 2015 HvH
            if(arm.eq.1) then
               MFPD(2) = 'MUA1'
               if(ich.eq.1) then
                   MFPD(3) = utrgPC1_north_name
                   MFPD(4) = utrgPC1_north_gas
                   NAMECH = utrgPC1_north_gas
               else if(ich.eq.2) then
                   MFPD(3) = utrgPC2_north_name
                   MFPD(4) = utrgPC2_north_gas
                   NAMECH = utrgPC2_north_gas
               else 
                   MFPD(3) = utrgPC3_north_name
                   MFPD(4) = utrgPC3_north_gas
                   NAMECH = utrgPC3_north_gas
               end if
            else
               MFPD(2) = 'MUA2'
               if(ich.eq.1) then
                   MFPD(3) = utrgPC1_south_name
                   MFPD(4) = utrgPC1_south_gas
                   NAMECH = utrgPC1_south_gas
               else if(ich.eq.2) then
                   MFPD(3) = utrgPC2_south_name
                   MFPD(4) = utrgPC2_south_gas
                   NAMECH = utrgPC2_south_gas
               else 
                   MFPD(3) = utrgPC3_south_name
                   MFPD(4) = utrgPC3_south_gas
                   NAMECH = utrgPC3_south_gas
               end if
            end if
            CALL GSDET('MUPC', NAMECH, 4, MFPD,  NBITS1, 
     *                 idtype,1990,3990, IS, ID )
            CALL GSDETH('MUPC',NAMECH,13,NAMEH1,NBITH1,ORIG1,FACT1)
         end do
      end do 

      RETURN

      END
