/************************************************************************

  utiLineCircle
  -------------

  DESCRIPTION: Given the slope and y-intercept of a line in cartesian
               coordinates, and the radius of a circle, returns the
	       intersection points of the line with the circle.

  INPUT: a = slope of the line in x-y
         b = y=0 intercept of the line in x-y
	 r = radius of the circle in x-y centered at the origin

  OUTPUT: x1 = x-coordinate of the first intersection point
          y1 = y-coordinate of the first intersection point
	  x2 = x-coordinate of the second intersection point
	  y2 = y-coordinate of the second intersection point

  RETURNS: Number of intersection points

  **********************************************************************/

//INCLUDECHECKER: Removed this line: #include <stdio.h>
#include <math.h>
//INCLUDECHECKER: Removed this line: #include <stdlib.h>

int utiLineCircle(float a, float b, float r,
		  float *x1, float *y1, float *x2, float *y2)
{

  int qsign,npoints;
  float aquad,bquad,cquad,dquad,qquad;

  *x1 = 0.0;
  *x2 = 0.0;
  *y1 = 0.0;
  *y2 = 0.0;

  aquad = a*a + 1.0;
  bquad = 2.0*a*b;
  cquad = b*b - r*r;
  qsign = 0;
  if (bquad > 0) qsign = 1;
  if (bquad < 0) qsign = -1;
  dquad = bquad*bquad - 4.0*aquad*cquad;
  if (dquad >= 0.0)
    {
      dquad = sqrt(dquad);
      qquad = -0.5*(bquad + qsign*dquad);
      npoints = 2;
      if (aquad != 0.0) *x1 = qquad/aquad;
      if (qquad != 0.0) *x2 = cquad/qquad;
      *y1 = a*(*x1) + b;
      *y2 = a*(*x2) + b;
    }
  else
    {
      npoints = 0;
    }

  return(npoints);

}   /* end utiLineCircle */
