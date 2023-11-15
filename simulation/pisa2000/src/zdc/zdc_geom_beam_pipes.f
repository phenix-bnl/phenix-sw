      SUBROUTINE zdc_geom_beam_pipes

************************************************************************
*  GEANT definitions for ZDC beam pipes not defined elsewhere.
*  Author: Andras Ster
*  Creation date: April 20, 1999
************************************************************************

      IMPLICIT NONE

#include "gugeom.inc"

      COMMON /ZDCCOM/ ism41, ism42, zdcl
      INTEGER         ism41, ism42
      CHARACTER*4     zdcl
      DATA            zdcl/'HALL'/

      REAL par(5)
      INTEGER ivolu

*  Tracking material numbers

      INTEGER iron, ivac, iber
      DATA iron/1310/
      DATA ivac/1316/
      DATA iber/1305/

*  Rotation matrix numbers


c     C.F. Maguire  February 10, 2000
c     We cannot specify the rotational numbers for PISA geometries.
c     This prevents conflicts in rotation numbers between different detectors.
c     We cannot specify a detector specific range of rotations numbers either.


      INTEGER iro1, iro2, ir11, ir12
c      DATA iro1/1381/
c      DATA iro2/1382/
c      DATA ir11/1391/
c      DATA ir12/1392/


*  Tracking medium definitions:
*  # Name Mat Is If Bmax  Tmax MSmx  DEmx   Eps  StMn  U Nu

*     CALL gstmed(iron,'IRON',    10,0,1,5.0, 0.3, 0.5,0.2,0.1, 0.1,0,0)
*     CALL gstmed(ivac,'VACUUM',  16,0,0,0.0,45.0,30.0,1.0,1.0,20.0,0,0)
      CALL gstmed(iron,'IRON',    10,0,1,30.0,5.0,0.5,0.2,0.001,0.4,0,0)
      CALL gstmed(ivac,'VACUUM',  16,0,1,30.0,5.0,0.5,0.2,0.001,0.4,0,0)


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*IF center beam pipes already defined elsewhere these volumes are not positioned!

      ivolu = 0

      IF(ivolu .NE. 0) THEN

* COORDINATE SYSTEM AND NOMINAL VERTEX
* The master volume zdcl is a virtual volume defining the master
* coordinate system and the limits of tracking.The master reference system
* is centered at the nominal interaction point, with Z along the
* beam line and the centers of coils at positive Z, Y up, and X horizontal
* completing a right-handed system.

      CALL gstmed(iber,'BERYLL',   5,0,1, 5.0,0.3,0.5,0.2,0.01,0.01,0,0)
 
* Volume definitions:  name  type  mat. lx/2   ly/2   lz/2    # par

      par(1) = 0.0
      par(2) = 0.1
      par(3) = 0.7
      CALL gsvolu('ORIG', 'TUBE', ivac, par, 3, ivolu)  ! Origin of MARS

* Rotation definitions: no th_x phi_x th_y phi_y th_z phi_z  volum rot.

      irot = irot + 1
      CALL gsrotm(irot, 90.0, 0.0, 180.0, 0.0, 90.0, 90.0)       ! orig
      iro1 = irot

      irot = irot + 1
      CALL gsrotm(irot,  0.0, 0.0,  90.0,90.0, 90.0,180.0)       ! orig
      iro2 = irot

* Position definitions: daug. cp  moth.  x_c  y_c  z_c  rot.

      CALL gspos('ORIG', 1, zdcl, 0.0, 0.0, 0.0, 0, 'ONLY')
      CALL gspos('ORIG', 2, zdcl, 0.0, 0.0, 0.0, iro1, 'ONLY')
      CALL gspos('ORIG', 3, zdcl, 0.0, 0.0, 0.0, iro2, 'ONLY')
 
 
*  BEAM PIPE + STEEL SECTIONS
*  Beam pipe: vol. definitions: name  shape mat. r_i   r_o  lz/2  # par

      par(1) = 3.71
      par(2) = 3.81
      par(3) = 600.0000
      CALL gsvolu('BEAM', 'TUBE', iber,  par, 3, ivolu)
      par(1) = 0.0
      par(2) = 3.71
      par(3) = 600.0000
      CALL gsvolu('BEAV', 'TUBE', ivac,  par, 3, ivolu)
      par(1) = 3.81
      par(2) = 5.85
      par(3) = 2.00
      CALL gsvolu('FLB1', 'TUBE', iber,  par, 3, ivolu)
      CALL gsvolu('FLB2', 'TUBE', iber,  par, 3, ivolu)
      CALL gsvolu('FLS1', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('FLS2', 'TUBE', iron,  par, 3, ivolu)
      par(1) = 3.54
      par(2) = 3.81
      par(3) = 2.00
      CALL gsvolu('SPL1', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('SPL2', 'TUBE', iron,  par, 3, ivolu)
      par(1) = 0.00
      par(2) = 3.54
      par(3) = 2.00
      CALL gsvolu('SM11', 'TUBE', ivac,  par, 3, ivolu)
      CALL gsvolu('SM12', 'TUBE', ivac,  par, 3, ivolu)
      par(1) = 12.5
      par(2) = 3.54
      par(3) = 3.81
      par(4) = 6.08
      par(5) = 6.35
      CALL gsvolu('CN11', 'CONE', iron,  par, 5, ivolu)
      par(1) = 12.5
      par(2) = 6.08
      par(3) = 6.35
      par(4) = 3.54
      par(5) = 3.81
      CALL gsvolu('CN12', 'CONE', iron,  par, 5, ivolu)
      par(1) = 12.5
      par(2) = 0.00
      par(3) = 3.54
      par(4) = 0.00
      par(5) = 6.08
      CALL gsvolu('CM11', 'CONE', ivac,  par, 5, ivolu)
      par(1) = 12.5
      par(2) = 0.00
      par(3) = 6.08
      par(4) = 0.00
      par(5) = 3.54
      CALL gsvolu('CM12', 'CONE', ivac,  par, 5, ivolu)
      par(1) = 6.35
      par(2) = 8.39
      par(3) = 2.00
      CALL gsvolu('FLS3', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('FLS4', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('FLS5', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('FLS6', 'TUBE', iron,  par, 3, ivolu)
      par(1) = 6.08
      par(2) = 6.35
      par(3) = 58.5
      CALL gsvolu('SPU1', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('SPU2', 'TUBE', iron,  par, 3, ivolu)
      par(1) = 0.00
      par(2) = 6.08
      par(3) = 58.5
      CALL gsvolu('SM21', 'TUBE', ivac,  par, 3, ivolu)
      CALL gsvolu('SM22', 'TUBE', ivac,  par, 3, ivolu)
      par(1) = 6.35
      par(2) = 7.35
      par(3) = 0.20
      CALL gsvolu('RB11', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB12', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB13', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB14', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB15', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB16', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB17', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB18', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB21', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB22', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB23', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB24', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB25', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB26', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB27', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('RB28', 'TUBE', iron,  par, 3, ivolu)

*  Position definitions: daug. cp  moth.  x_c  y_c  z_c   rot. flag

      CALL gspos('BEAM', 1, zdcl, 0.0000, 0.0000, 0.0000, 0, 'ONLY')
      CALL gspos('BEAV', 1, zdcl, 0.0000, 0.0000, 0.0000, 0, 'ONLY')
      CALL gspos('FLB1', 1, zdcl, 0.0000, 0.0000, 200.00, 0, 'ONLY')
      CALL gspos('FLB2', 1, zdcl, 0.0000, 0.0000,-200.00, 0, 'ONLY')
      CALL gspos('FLS1', 1, zdcl, 0.0000, 0.0000, 600.00, 0, 'ONLY')
      CALL gspos('FLS2', 1, zdcl, 0.0000, 0.0000,-600.00, 0, 'ONLY')
      CALL gspos('SM11', 1, zdcl, 0.0000, 0.0000, 602.00, 0, 'ONLY')
      CALL gspos('SM12', 1, zdcl, 0.0000, 0.0000,-602.00, 0, 'ONLY')
      CALL gspos('SPL1', 1, zdcl, 0.0000, 0.0000, 602.00, 0, 'ONLY')
      CALL gspos('SPL2', 1, zdcl, 0.0000, 0.0000,-602.00, 0, 'ONLY')
      CALL gspos('CM11', 1, zdcl, 0.0000, 0.0000, 616.50, 0, 'ONLY')
      CALL gspos('CM12', 1, zdcl, 0.0000, 0.0000,-616.50, 0, 'ONLY')
      CALL gspos('CN11', 1, zdcl, 0.0000, 0.0000, 616.50, 0, 'ONLY')
      CALL gspos('CN12', 1, zdcl, 0.0000, 0.0000,-616.50, 0, 'ONLY')
      CALL gspos('FLS3', 1, zdcl, 0.0000, 0.0000, 629.00, 0, 'ONLY')
      CALL gspos('FLS4', 1, zdcl, 0.0000, 0.0000,-629.00, 0, 'ONLY')
      CALL gspos('FLS5', 1, zdcl, 0.0000, 0.0000, 746.00, 0, 'ONLY')
      CALL gspos('FLS6', 1, zdcl, 0.0000, 0.0000,-746.00, 0, 'ONLY')
      CALL gspos('SM21', 1, zdcl, 0.0000, 0.0000, 687.50, 0, 'ONLY')
      CALL gspos('SM22', 1, zdcl, 0.0000, 0.0000,-687.50, 0, 'ONLY')
      CALL gspos('SPU1', 1, zdcl, 0.0000, 0.0000, 687.50, 0, 'ONLY')
      CALL gspos('SPU2', 1, zdcl, 0.0000, 0.0000,-687.50, 0, 'ONLY')
      CALL gspos('RB11', 1, zdcl, 0.0000, 0.0000, 679.80, 0, 'ONLY')
      CALL gspos('RB12', 1, zdcl, 0.0000, 0.0000, 681.95, 0, 'ONLY')
      CALL gspos('RB13', 1, zdcl, 0.0000, 0.0000, 684.10, 0, 'ONLY')
      CALL gspos('RB14', 1, zdcl, 0.0000, 0.0000, 686.25, 0, 'ONLY')
      CALL gspos('RB15', 1, zdcl, 0.0000, 0.0000, 688.40, 0, 'ONLY')
      CALL gspos('RB16', 1, zdcl, 0.0000, 0.0000, 690.55, 0, 'ONLY')
      CALL gspos('RB17', 1, zdcl, 0.0000, 0.0000, 692.70, 0, 'ONLY')
      CALL gspos('RB18', 1, zdcl, 0.0000, 0.0000, 694.85, 0, 'ONLY')
      CALL gspos('RB21', 1, zdcl, 0.0000, 0.0000,-679.80, 0, 'ONLY')
      CALL gspos('RB22', 1, zdcl, 0.0000, 0.0000,-681.95, 0, 'ONLY')
      CALL gspos('RB23', 1, zdcl, 0.0000, 0.0000,-684.10, 0, 'ONLY')
      CALL gspos('RB24', 1, zdcl, 0.0000, 0.0000,-686.25, 0, 'ONLY')
      CALL gspos('RB25', 1, zdcl, 0.0000, 0.0000,-688.40, 0, 'ONLY')
      CALL gspos('RB26', 1, zdcl, 0.0000, 0.0000,-690.55, 0, 'ONLY')
      CALL gspos('RB27', 1, zdcl, 0.0000, 0.0000,-692.70, 0, 'ONLY')
      CALL gspos('RB28', 1, zdcl, 0.0000, 0.0000,-694.85, 0, 'ONLY')

      ENDIF 

* These volumes are assumed to be positioned by now

*  UPSTREAM AREA

      par(1) = 6.08
      par(2) = 6.35
      par(3) = 103.94
      CALL gsvolu('SP31', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('SP32', 'TUBE', iron,  par, 3, ivolu)
      par(1) = 0.00
      par(2) = 6.08
      par(3) = 103.94
      CALL gsvolu('SM31', 'TUBE', ivac,  par, 3, ivolu)
      CALL gsvolu('SM32', 'TUBE', ivac,  par, 3, ivolu)
      par(1) = 6.99
      par(2) = 7.14
      par(3) = 210.92
      CALL gsvolu('SP41', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('SP42', 'TUBE', iron,  par, 3, ivolu)

cscj Here is a small update to the geometry.  The magnetic field
cscj only has a lengthof 3.7889 meters according to the CAD fellas
cscj so I'll split this geometry into 3 pieces:      
c      par(1) = 0.00
c      par(2) = 6.99
c     par(3) = 210.92
c      CALL gsvolu('SM41', 'TUBE', ivac,  par, 3, ism41)
c      CALL gsvolu('SM42', 'TUBE', ivac,  par, 3, ism42)

      par(1) = 0.00
      par(2) = 6.99
      par(3) = 378.89/2.0
      CALL gsvolu('SM41', 'TUBE', ivac,  par, 3, ism41)
      CALL gsvolu('SM42', 'TUBE', ivac,  par, 3, ism42)

      par(1) = 0.00
      par(2) = 6.99
      par(3) = 210.92 - (378.89/2.0)
      CALL gsvolu('SM43', 'TUBE', ivac,  par, 3, ivolu)
      CALL gsvolu('SM44', 'TUBE', ivac,  par, 3, ivolu)
      CALL gsvolu('SM45', 'TUBE', ivac,  par, 3, ivolu)
      CALL gsvolu('SM46', 'TUBE', ivac,  par, 3, ivolu)

      par(1) = 9.53
      par(2) = 10.16
      par(3) = 210.92
      CALL gsvolu('SP51', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('SP52', 'TUBE', iron,  par, 3, ivolu)
      par(1) = 7.14
      par(2) = 9.53
      par(3) = 210.92
      CALL gsvolu('SM51', 'TUBE', ivac,  par, 3, ivolu)
      CALL gsvolu('SM52', 'TUBE', ivac,  par, 3, ivolu)
      par(1) = 15.34
      par(2) = 37.00
      par(3) = 210.92
      CALL gsvolu('SP61', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('SP62', 'TUBE', iron,  par, 3, ivolu) 
      par(1) = 10.16
      par(2) = 15.34
      par(3) = 210.92
      CALL gsvolu('SM61', 'TUBE', ivac,  par, 3, ivolu) 
      CALL gsvolu('SM62', 'TUBE', ivac,  par, 3, ivolu) 
      par(1) = 20.87
      par(2) = 7.14
      par(3) = 7.77
      par(4) = 14.60
      par(5) = 15.24
      CALL gsvolu('CN21', 'CONE', iron,  par, 5, ivolu)
      par(1) = 20.87
      par(2) = 14.60
      par(3) = 15.24
      par(4) = 7.14
      par(5) = 7.77
      CALL gsvolu('CN22', 'CONE', iron,  par, 5, ivolu)
      par(1) = 20.87
      par(2) = 0.00
      par(3) = 7.14
      par(4) = 00.00
      par(5) = 14.60
      CALL gsvolu('CM21', 'CONE', ivac,  par, 5, ivolu)
      par(1) = 20.87
      par(2) = 0.00
      par(3) = 14.60
      par(4) = 00.00
      par(5) = 7.14
      CALL gsvolu('CM22', 'CONE', ivac,  par, 5, ivolu)
      par(1) = 14.60
      par(2) = 15.24
      par(3) = 57.895
      CALL gsvolu('SP71', 'TUBE', iron,  par, 3, ivolu)
      CALL gsvolu('SP72', 'TUBE', iron,  par, 3, ivolu) 
      par(1) = 0.00
      par(2) = 14.60
      par(3) = 57.895
      CALL gsvolu('SM71', 'TUBE', ivac,  par, 3, ivolu)
      CALL gsvolu('SM72', 'TUBE', ivac,  par, 3, ivolu)

* Position definitions: daug. cp  moth.  x_c   y_c   z_c   rot. flag

      CALL gspos('SM31', 1, zdcl, 0.0000, 0.0000,  850.14, 0, 'ONLY')
      CALL gspos('SM32', 1, zdcl, 0.0000, 0.0000, -850.14, 0, 'ONLY')
      CALL gspos('SP31', 1, zdcl, 0.0000, 0.0000,  850.14, 0, 'ONLY')
      CALL gspos('SP32', 1, zdcl, 0.0000, 0.0000, -850.14, 0, 'ONLY')
      CALL gspos('SM42', 1, zdcl, 0.0000, 0.0000, 1165.00, 0, 'ONLY')
      CALL gspos('SM41', 1, zdcl, 0.0000, 0.0000,-1165.00, 0, 'ONLY')
      CALL gspos('SP42', 1, zdcl, 0.0000, 0.0000, 1165.00, 0, 'ONLY')
      CALL gspos('SP41', 1, zdcl, 0.0000, 0.0000,-1165.00, 0, 'ONLY')

cscj Now add the small volumes to correct the magnetic field change
cscj discussed above
      CALL gspos('SM43', 1, zdcl, 0.0000, 0.0000, 1375.92, 0, 'ONLY')
      CALL gspos('SM44', 1, zdcl, 0.0000, 0.0000,-1375.92, 0, 'ONLY')
      CALL gspos('SM45', 1, zdcl, 0.0000, 0.0000,  945.08, 0, 'ONLY')
      CALL gspos('SM46', 1, zdcl, 0.0000, 0.0000, -945.08, 0, 'ONLY')


      CALL gspos('SM52', 1, zdcl, 0.0000, 0.0000, 1165.00, 0, 'ONLY')
      CALL gspos('SM51', 1, zdcl, 0.0000, 0.0000,-1165.00, 0, 'ONLY')
      CALL gspos('SP52', 1, zdcl, 0.0000, 0.0000, 1165.00, 0, 'ONLY')
      CALL gspos('SP51', 1, zdcl, 0.0000, 0.0000,-1165.00, 0, 'ONLY')
      CALL gspos('SM62', 1, zdcl, 0.0000, 0.0000, 1165.00, 0, 'ONLY')
      CALL gspos('SM61', 1, zdcl, 0.0000, 0.0000,-1165.00, 0, 'ONLY')
      CALL gspos('SP62', 1, zdcl, 0.0000, 0.0000, 1165.00, 0, 'ONLY')
      CALL gspos('SP61', 1, zdcl, 0.0000, 0.0000,-1165.00, 0, 'ONLY')
      CALL gspos('CM21', 1, zdcl, 0.0000, 0.0000, 1394.25, 0, 'ONLY')
      CALL gspos('CM22', 1, zdcl, 0.0000, 0.0000,-1394.25, 0, 'ONLY')
      CALL gspos('CN21', 1, zdcl, 0.0000, 0.0000, 1394.25, 0, 'ONLY')
      CALL gspos('CN22', 1, zdcl, 0.0000, 0.0000,-1394.25, 0, 'ONLY')
      CALL gspos('SM72', 1, zdcl, 0.0000, 0.0000, 1475.935, 0, 'ONLY')
      CALL gspos('SM71', 1, zdcl, 0.0000, 0.0000,-1475.935, 0, 'ONLY')
      CALL gspos('SP72', 1, zdcl, 0.0000, 0.0000, 1475.935, 0, 'ONLY')
      CALL gspos('SP71', 1, zdcl, 0.0000, 0.0000,-1475.935, 0, 'ONLY')


*  ZCAL AREA
 
      par(1) = 20.0
      par(2) = 20.96
      par(3) = 108.58
      CALL gsvolu('SP81', 'TUBE',  iron, par, 3, ivolu)
      CALL gsvolu('SP82', 'TUBE',  iron, par, 3, ivolu) 
      par(1) = 0.0
      par(2) = 20.0 
      par(3) = 108.58
      CALL gsvolu('SM81', 'TUBE',  ivac,  par, 3, ivolu)
      CALL gsvolu('SM82', 'TUBE',  ivac,  par, 3, ivolu)
      par(1) = 0.0
      par(2) = 20.96
      par(3) = 0.47
      CALL gsvolu('SP91', 'TUBE',  iron,  par, 3, ivolu)
      CALL gsvolu('SP92', 'TUBE',  iron,  par, 3, ivolu) 
      par(1) = 0.0
      par(2) = 6.35
      par(3) = 0.47
      CALL gsvolu('SHL1', 'TUBE',  ivac,  par, 3, ivolu)
      CALL gsvolu('SHL2', 'TUBE',  ivac,  par, 3, ivolu) 
      CALL gsvolu('SHU1', 'TUBE',  ivac,  par, 3, ivolu)
      CALL gsvolu('SHU2', 'TUBE',  ivac,  par, 3, ivolu) 
      par(1) = 6.07
      par(2) = 6.35
      par(3) = 91.5
      CALL gsvolu('S101', 'TUBE',  iron,  par, 3, ivolu)
      CALL gsvolu('S102', 'TUBE',  iron,  par, 3, ivolu) 
      par(1) = 0.0
      par(2) = 6.07
      par(3) = 91.5
      CALL gsvolu('M101', 'TUBE',  ivac,  par, 3, ivolu)
      CALL gsvolu('M102', 'TUBE',  ivac,  par, 3, ivolu)
      par(1) = 6.07
      par(2) = 6.35
      par(3) = 91.5
      CALL gsvolu('S111', 'TUBE',  iron,  par, 3, ivolu)
      CALL gsvolu('S112', 'TUBE',  iron,  par, 3, ivolu) 
      par(1) = 0.0
      par(2) = 6.07
      par(3) = 91.5
      CALL gsvolu('M111', 'TUBE',  ivac,  par, 3, ivolu)
      CALL gsvolu('M112', 'TUBE',  ivac,  par, 3, ivolu)

      irot = irot + 1
      CALL gsrotm(irot,  91.074, 0.0, 90.0, 90.0, 1.074, 0.0)
      ir11 = irot

      irot = irot + 1
      CALL gsrotm(irot, -91.074, 0.0, 90.0, 90.0,-1.074, 0.0)  
      ir12 = irot

      CALL gspos('SM81', 1, zdcl, 0.0000, 0.00, 1642.4,  0, 'ONLY')
      CALL gspos('SM82', 1, zdcl, 0.0000, 0.00,-1642.4,  0, 'ONLY')
      CALL gspos('SP81', 1, zdcl, 0.0000, 0.00, 1642.4,  0, 'ONLY')
      CALL gspos('SP82', 1, zdcl, 0.0000, 0.00,-1642.4,  0, 'ONLY')

      CALL gspos('SP91', 1, zdcl, 0.0000, 0.00, 1750.99,  0, 'ONLY')
      CALL gspos('SP92', 1, zdcl, 0.0000, 0.00,-1750.99,  0, 'ONLY')

      CALL gspos('M101', 1, zdcl, 12.82,  0.00, 1843.20, ir11, 'ONLY')
      CALL gspos('S101', 1, zdcl, 12.82,  0.00, 1843.20, ir11, 'ONLY')
      CALL gspos('M102', 1, zdcl, 12.82,  0.00,-1843.20, ir12, 'ONLY')
      CALL gspos('S102', 1, zdcl, 12.82,  0.00,-1843.20, ir12, 'ONLY')
      CALL gspos('M111', 1, zdcl,-12.82,  0.00, 1843.20, ir12, 'ONLY')
      CALL gspos('S111', 1, zdcl,-12.82,  0.00, 1843.20, ir12, 'ONLY')
      CALL gspos('M112', 1, zdcl,-12.82,  0.00,-1843.20, ir11, 'ONLY')
      CALL gspos('S112', 1, zdcl,-12.82,  0.00,-1843.20, ir11, 'ONLY')

      CALL gspos('SHL1', 1, 'SP91', 11.02,  0.00, 0.0,  0, 'ONLY')
      CALL gspos('SHL2', 1, 'SP92', 11.02,  0.00, 0.0,  0, 'ONLY')
      CALL gspos('SHU1', 1, 'SP91',-11.02,  0.00, 0.0,  0, 'ONLY') 
      CALL gspos('SHU2', 1, 'SP92',-11.02,  0.00, 0.0,  0, 'ONLY')

      RETURN
      END
