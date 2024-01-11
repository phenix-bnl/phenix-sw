/****************************************************************************
 ****************************************************************************

 utiGaussian
 -----------

 DESCRIPTION: For a given mean and sigma, returns a random displacement
     from the mean after sampling the Gaussian distribution.
     For example, for a given input u, this will return u+dx or u-dx, where
     dx is the displacement, xgauss.

 AUTHOR/CONTACT: J.T. Mitchell, BNL

 REVISIONS:
       Date            Author          Description

       1/16/97         S.C. Johnson    Small fix
       1/3/96          Mitchell        Converted to C
       7/24/95         J.T. Mitchell   Original

 INPUT VARIABLES: mean = mean of the Gaussian
                  sigma = sigma of the Gaussian to spread
		  randseed = current random number seed

 OUTPUT VARIABLES: xgauss = Final position after Gaussian spread applied

 ***************************************************************************
 ***************************************************************************/

//INCLUDECHECKER: Removed this line: #include <stdio.h>
#include <math.h>
//INCLUDECHECKER: Removed this line: #include <stdlib.h>

extern float utiRandom(long *ldum);

void
utiGaussian(float *mean,float *sigma, long *randseed, 
	    float *xgauss)

{
  long rseed;
  float f, v1, v2, rsq;
  /* Executable Statements */

  rseed = *randseed;   /* for internal use */

  do {
    v1 = 2.0*utiRandom(&rseed)-1.0;
    v2 = 2.0*utiRandom(&rseed)-1.0;
    rsq = v1*v1 + v2*v2;
  } while (rsq >=1.0 || rsq == 0);
  f = sqrt(-2.0*log(rsq)/rsq);

  *xgauss = v2*f*(*sigma) + *mean;
  *randseed = rseed;   /* return the modified seed */
  
}   /* end utiGaussian */
