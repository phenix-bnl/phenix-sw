/****************************************************************************
 ****************************************************************************

 utiXYtoRPhi
 -----------

 DESCRIPTION: Converts cartesian coordinates to polar coordinates using
              the standard PHENIX coordinate system.


 AUTHOR/CONTACT: J.T. Mitchell, BNL

 REVISIONS:
       Date            Author          Description

       1/19/96          Mitchell        Original

 INPUT VARIABLES: x,y = input cartesian coordinates (cm)

 OUTPUT VARIABLES: r,phi = output polar coordinates (r in cm, phi in degrees)

 ***************************************************************************
 ***************************************************************************/

//INCLUDECHECKER: Removed this line: #include <stdio.h>
#include <math.h>
//INCLUDECHECKER: Removed this line: #include <stdlib.h>

void utiXYtoRPhi(float *x,float *y,float *r,float *phi)

{

  float x1,y1;

  x1 = *x;
  y1 = *y;

  *r = sqrt(x1*x1+y1*y1);
  
  if (x1 != 0.0)
    {
      *phi = atan(y1/x1)*57.2957795;   /* output phi is in degrees */
    }
  else
    {
      *phi = 90.0;
    }
  
  if (x1<0) *phi += 180.0;        /* only done for arm 2 (east) */
  
}   /* end utiXYtoRPhi */
