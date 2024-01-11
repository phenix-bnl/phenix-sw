/****************************************************************************
 ****************************************************************************

 utiRPhitoXY
 -----------

 DESCRIPTION: Converts polar coordinates to cartesian coordinates using
              the standard PHENIX coordinate system.


 AUTHOR/CONTACT: J.T. Mitchell, BNL

 REVISIONS:
       Date            Author          Description

       1/19/96          Mitchell        Original

 INPUT VARIABLES: r,phi = input polar coordinates (r in cm, phi in degrees)

 OUTPUT VARIABLES: x,y = output cartesian coordinates

 ***************************************************************************
 ***************************************************************************/

//INCLUDECHECKER: Removed this line: #include <stdio.h>
#include <math.h>
//INCLUDECHECKER: Removed this line: #include <stdlib.h>

void utiRPhitoXY(float *r,float *phi,float *x,float *y)

{

  float r1,phi1;
    
  r1 = *r;
  phi1 = *phi;

  phi1 *= 0.0174533;   /* convert to radians */
  
  *x = fabs(r1*cos(phi1));
  *y = fabs(r1*sin(phi1));

  /* x and y are positive now.  Need to assign the proper signs. */

  phi1 = *phi;
  
  if (phi1<0.0) *y = -(*y);   /* can only occur in arm 1 */
  
  if (phi1>90.0 && phi1<=180.0) *x = -(*x);   /* upper part of arm 2 */
  
  if (phi1>180.0 && phi1<=270.0)    /* bottom part of arm 2 */
    {
      *x = -(*x);
      *y = -(*y);
    }

}   /* end utiRPhitoXY */
