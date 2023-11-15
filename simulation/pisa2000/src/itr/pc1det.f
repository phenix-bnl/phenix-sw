      SUBROUTINE PC1DET ( NMCHMB)
*       ============================

*****************************************************************
*                                                               *
*    THE ROUTINE TO DESCRIBE DTECTORS AND HIT PARAMETERS        *
*       CALLED FROM ITR                                         *
*     ITS PLACE TO DEFINE DIGITIZATION PARAMETERS               *
*     Called by ITR                                             *
*****************************************************************


c  Revised by K. Filimonov on September 6th, 1995.
c  Modified for a updated geometry. Now called from itr.

c  Revised by C.F. Mguire February 15, 1998 to add global and path length 



c     Revision History
c    Name           Date            Comment
c    C.F. Maguire   Feb. 15, 1998   Add global coordinates and path length


      IMPLICIT NONE


#include "gugeom.inc"

      INTEGER NMCHMB, IS, ID, J
      CHARACTER*4 NAMEH1(13), MFPD(4)
      CHARACTER*4 NAMECH
      CHARACTER*4 NMPAD1(16),  NMGAS(16)

      INTEGER NBITS1(4), NBITH1(13)
      REAL FACT1(13), ORIG1(13)

      DATA MFPD /'HALL', 'INTR', '    ', '    '/

      DATA NMPAD1 /'P101', 'P102', 'P103', 'P104', 'P105', 'P106',
     +'P107', 'P108', 'P109', 'P110', 'P111', 'P112', 'P113', 'P114',
     +'P115', 'P116' /

      DATA NMGAS /'ZZ01', 'ZZ02', 'ZZ03', 'ZZ04', 'ZZ05', 'ZZ06',
     +'ZZ07', 'ZZ08', 'ZZ09', 'ZZ10', 'ZZ11', 'ZZ12', 'ZZ13', 'ZZ14',
     +'ZZ15', 'ZZ16' /

      DATA NBITS1/ 4* 4/
      DATA NAMEH1/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ',
     >     'TOF ', 'PTID', 'DELE' , 'XG1', 'YG1', 'ZG1', 'PL' /

      DATA NBITH1/ 13* 32 /
      DATA ORIG1/ 6*1000., 3* 0., 3*1000., 0./
      DATA FACT1/ 7*1000., 1., 5*1000./


C     DETECTORS HIT DESCRIPTION
C     FIRST PAD. DETECTOR

      DO 15 J=1, NMCHMB
         if(j.le.8)then
            MFPD(2) = ePC1
         else
            MFPD(2) = wPC1
         endif
         MFPD(3) = NMPAD1(J)
         MFPD(4) = NMGAS(J)
         NAMECH = NMGAS(J)
         CALL GSDET('ITR ', NAMECH, 4, MFPD,  NBITS1, 4,1990,3990,
     +      IS, ID )
         CALL GSDETH('ITR ',NAMECH,13,NAMEH1,NBITH1,ORIG1,FACT1)
  15  CONTINUE
      RETURN
      END
