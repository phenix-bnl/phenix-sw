/* --------------------------------------------------------------

 utiLocaltoGlobal - Generic local-to-global coordinate translation

 DESCRIPTION: Performs a generic rotation of a coordinate system
              by an angle theta followed by a translation by
	      the polar coordinates (Xtranslate,Ytranslate)
             
 INPUT VARIABLES: XYZloc = local coordinates
                  XYZTranslate = origin of the local coordinate
		                 system axis in global coordinates
		  Theta = rotation angle in degrees

 OUTPUT VARIABLES XYZglo = global coordinates

 AUTHOR:
 J. T. Mitchell - 6/18/97 - Original
                                                  
--------------------------------------------------------------- */

//INCLUDECHECKER: Removed this line: #include <stdio.h>
#include <math.h>
//INCLUDECHECKER: Removed this line: #include <stdlib.h>

void utiLocaltoGlobal(float XYZloc[],float XYZtranslate[],
		      float Theta, float *Xglo, float *Yglo,
		      float *Zglo)
{

  float XYZprime[3];   /* intermediate rotated coordinates */
  float ThetaRad;      /* the rotation angle in radians */

  ThetaRad = 0.017453*Theta;

  /* The coordinate transformation will be a rotation about the
     original (local) axis, followed by a translation. The rotated
     coordinates will be labelled with the suffix "prime". */

  /* Perform the rotation operation */
  XYZprime[0] = XYZloc[0]*cos(ThetaRad) + XYZloc[1]*sin(ThetaRad);
  XYZprime[1] = -XYZloc[0]*sin(ThetaRad) + XYZloc[1]*cos(ThetaRad);
  XYZprime[2] = XYZloc[2];

  /* Perform the translation operation */
  *Xglo = XYZprime[0] + XYZtranslate[0];
  *Yglo = XYZprime[1] + XYZtranslate[1];
  *Zglo = XYZprime[2] + XYZtranslate[2];

}   /* end utiLocaltoGlobal */
