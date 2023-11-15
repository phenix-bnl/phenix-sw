C...  Define Detector regions from active volumes
C...  Author: L. A. Linden Levy & M. C. McCain 17.02.2004

      SUBROUTINE RLT_det 

      IMPLICIT NONE

#include "gugeom.inc"
      
      INTEGER rpc, IS, ID
      INTEGER NBITS1(4), NBITH1(13)
      INTEGER idtype/5432/

      REAL FACT1(13), ORIG1(13)

      character*4 rltRPC1shell/'rts1'/
      character*4 rltRPC2shell/'rts2'/
      character*4 rltRPC3shell/'rts3'/
      character*4 rltRPC1gasVolume/'rtg1'/
      character*4 rltRPC2gasVolume/'rtg2'/
      character*4 rltRPC3gasVolume/'rtg3'/

      character*4 NAMECH
      character*4 MFPD(4), NAMEH1(13)

      DATA MFPD /'HALL','    ','    ','    '/
      DATA NBITS1/ 4* 4/
      DATA NBITH1/ 13* 32/
      DATA NAMEH1/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ',
     >     'TOF ', 'PTID', 'DELE' , 'XG1', 'YG1', 'ZG1', 'PL' /
      DATA ORIG1/ 6*1000., 3* 0., 3*1000., 0./
      DATA FACT1/ 7*1000., 1., 5*1000./

      do rpc = 1, 3
         if (rpc.eq.1) then
            MFPD(2) = 'RPC1'
            MFPD(3) = rltRPC1shell
            MFPD(4) = rltRPC1gasVolume
            NAMECH =  rltRPC1gasVolume
         else if (rpc.eq.2) then
            MFPD(2) = 'RPC2'
            MFPD(3) = rltRPC2shell
            MFPD(4) = rltRPC2gasVolume
            NAMECH =  rltRPC2gasVolume
         else if (rpc.eq.3) then
            MFPD(2) = 'RPC3'
            MFPD(3) = rltRPC3shell
            MFPD(4) = rltRPC3gasVolume
            NAMECH =  rltRPC3gasVolume
         end if

         CALL GSDET('RLT ', NAMECH, 4, MFPD, NBITS1, 
     *        idtype,1990,3990, IS, ID)
         print*,'IS=',IS,'ID=',ID
         CALL GSDETH('RLT ',NAMECH, 13,NAMEH1,NBITH1,ORIG1,FACT1)
     *        
*         CALL GPSETS('RLT ',NAMECH)
      end do

      RETURN
      END
