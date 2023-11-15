c $Id: pc23det.f,v 1.4 2008/05/21 08:22:03 hpereira Exp $
      SUBROUTINE PC23DET( NMCHM)
*    ==================================


*    THE ROUTINE TO DISCRIBE DTECTORS AND HIT PARAMETERS

*  Revision History

*  Name                  Date           Comment
* Kirill Filimonov     Sep. 13, 1995    New PC2/PC3 independent geometry

* C.F. Maguire         Feb. 15, 1998    Add global and path length output

********************************************************************


      IMPLICIT NONE
 

c    Calling variable NMCHM  number of PHI sectors  (from global geometry)

 

#include "gugeom.inc"
#include "gconst.inc"

      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)

      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM


      CHARACTER*4 NAMECH


      INTEGER J, IST, IDT, NMCHM


      CHARACTER*4 NAMEH2( 13), MSPD(4)
      CHARACTER*4 NAMEH5( 13), MTPD(4)

      CHARACTER*4 VPC2SHELL(8), VPC3SHELL(8) 
      CHARACTER*4 VPC2GASGP(8), VPC3GASGP(8)

      REAL FACT2( 13), ORIG2( 13)
      REAL FACT5( 13), ORIG5( 13)
      INTEGER NBITS2(4), NBITH2( 13)
      INTEGER NBITS5(4), NBITH5( 13)

      DATA VPC2SHELL /'PD21', 'PD22', 'PD23', 'PD24', 'PD25', 'PD26',
     +'PD27', 'PD28' /
      DATA VPC2GASGP /'AR21', 'AR22', 'AR23', 'AR24', 'AR25', 'AR26',
     +  'AR27', 'AR28' /
      DATA VPC3SHELL /'PD31', 'PD32', 'PD33', 'PD34', 'PD35', 'PD36',
     +'PD37', 'PD38' /
      DATA VPC3GASGP /'AR31', 'AR32', 'AR33', 'AR34', 'AR35', 'AR36',
     +'AR37', 'AR38' /


      DATA MSPD /'HALL', 'EMCL', '    ', '    '/
      DATA MTPD /'HALL', 'EMCL', '    ', '    '/


      DATA NBITS2/ 4*4 /
      DATA NBITS5/ 4*4 /
      DATA NAMEH2/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ', 'TOF ',
     +'PTID', 'DELE', 'X1GL', 'Y1GL', 'Z1GL', 'PTHL' /
      DATA NAMEH5/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ', 'TOF ',
     +'PTID', 'DELE','X1GL', 'Y1GL', 'Z1GL', 'PTHL' /

      DATA NBITH2/ 13*32 /
      DATA NBITH5/ 13*32 /
      DATA ORIG2/ 6*1000., 3*0., 3*1000., 0. /
      DATA ORIG5/ 6*1000., 3*0., 3*1000., 0. /
      DATA FACT2/ 7*1000., 1., 5*1000./
      DATA FACT5/ 7*1000., 1., 5*1000./


C   .....   PAD    DETECTORS HIT DESCRIPTION  .............

      DO 16 J=1, NMCHM
         if(j.le.4)then
            MSPD(2) = ePC2
         else
            MSPD(2) = wPC2
         endif
         MSPD(3) = VPC2SHELL(J)
         MSPD(4) = VPC2GASGP(J)
         NAMECH= VPC2GASGP(J)
         CALL GSDET('PAD ', NAMECH, 4, MSPD,  NBITS2,15,1990,3990,
     +        IST, IDT )

         CALL GSDETH('PAD ',NAMECH, 13,NAMEH2,NBITH2,ORIG2,FACT2)

   16 CONTINUE

      DO 17 J=1, NMCHM
         if(j.le.4)then
            MTPD(2) = ePC3
         else
            MTPD(2) = wPC3
         endif
         MTPD(3) = VPC3SHELL(J)
         MTPD(4) = VPC3GASGP(J)
         NAMECH= VPC3GASGP(J)
         CALL GSDET('PAD ', NAMECH, 4, MTPD,  NBITS5,17,1990,3990,
     +        IST, IDT )

         CALL GSDETH('PAD ',NAMECH, 13,NAMEH5,NBITH5,ORIG5,FACT5)

   17 CONTINUE

*  ...........................................................

      RETURN
      END
