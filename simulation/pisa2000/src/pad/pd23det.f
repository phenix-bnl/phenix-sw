c $Id: pd23det.f,v 1.3 2008/05/21 08:22:03 hpereira Exp $
*-- Author :    Charles F. Maguire   19/07/94
      SUBROUTINE PD23DET( NMCHM)
*    ==================================


*    THE ROUTINE TO DISCRIBE DTECTORS AND HIT PARAMETERS
*       CALLED FROM PAD

********************************************************************


      IMPLICIT NONE
 

c    Calling variable NMCHM  number of PHI sectors  (INPUT off geometry file)

 

*KEEP,GCONST.
#include "gconst.inc"
*KEND.

      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)

      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM


      CHARACTER*4 NAMECH


      INTEGER J, IST, IDT, NMCHM


      CHARACTER*4 NAMEH2( 9), MSPD(4)
      CHARACTER*4 NAMEH5( 9), MTPD(4)

      CHARACTER*4 NMCVSP(8), NMPADS(8), NMCVTP(8), NMPADT(8)

      REAL FACT2( 9), ORIG2( 9)
      REAL FACT5( 9), ORIG5( 9)
      INTEGER NBITS2(4), NBITH2( 9)
      INTEGER NBITS5(4), NBITH5( 9)

      DATA NMPADS /'PDS1', 'PDS2', 'PDS3', 'PDS4', 'PDS5', 'PDS6',
     +'PDS7', 'PDS8' /
      DATA NMPADT /'PDT1', 'PDT2', 'PDT3', 'PDT4', 'PDT5', 'PDT6',
     +'PDT7', 'PDT8' /
      DATA NMCVSP /'CVS1', 'CVS2', 'CVS3', 'CVS4', 'CVS5', 'CVS6',
     +'CVS7', 'CVS8' /
      DATA NMCVTP /'CVT1', 'CVT2', 'CVT3', 'CVT4', 'CVT5', 'CVT6',
     +'CVT7', 'CVT8' /


      DATA MSPD /'HALL', 'EMCL', '    ', '    '/
      DATA MTPD /'HALL', 'EMCL', '    ', '    '/


      DATA NBITS2/ 4*4 /
      DATA NBITS5/ 4*4 /
      DATA NAMEH2/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ', 'TOF ',
     +'PTID', 'DELE'/
      DATA NAMEH5/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ', 'TOF ',
     +'PTID', 'DELE'/

      DATA NBITH2/ 9*32 /
      DATA NBITH5/ 9*32 /
      DATA ORIG2/ 6*1000., 3*0./
      DATA ORIG5/ 6*1000., 3*0./
      DATA FACT2/ 7*1000., 1., 1000./
      DATA FACT5/ 7*1000., 1., 1000./


C   .....   PAD    DETECTORS HIT DESCRIPTION  .............

      DO 16 J=1, NMCHM
         MSPD(3) = NMCVSP(J)
         MSPD(4) = NMPADS(J)
         NAMECH= NMPADS(J)
         CALL GSDET('PAD ', NAMECH, 4, MSPD,  NBITS2,15,1990,3990,
     +        IST, IDT )

         CALL GSDETH('PAD ',NAMECH, 9,NAMEH2,NBITH2,ORIG2,FACT2)

   16 CONTINUE

      DO 17 J=1, NMCHM
         MTPD(3) = NMCVTP(J)
         MTPD(4) = NMPADT(J)
         NAMECH= NMPADT(J)
         CALL GSDET('PAD ', NAMECH, 4, MTPD,  NBITS5,17,1990,3990,
     +        IST, IDT )

         CALL GSDETH('PAD ',NAMECH, 9,NAMEH5,NBITH5,ORIG5,FACT5)

   17 CONTINUE

*  ...........................................................

      RETURN
      END
