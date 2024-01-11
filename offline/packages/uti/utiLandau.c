/****************************************************************************
 ****************************************************************************

 utiLandau
 -----------

 DESCRIPTION: 	Generates random value according to Landau distribution.
		Needs uniform random number generator (utiRandom).
		
 ALGORITHM:  	Integral of the Landau distribution is
 		equal to the integral of a Gaussian distribution
		after following substitution: x = exp(-lambda/2)
		Thus, it is enough to generate Gaussian random value x,
		and Landau distributed random number will be
		lambda = -2.0*log((fabs)(x))

 AUTHOR/CONTACT:  Sasha Lebedev, ISU, lebedev@iastate.edu

 REVISIONS:
       Date            Author          Description

       04/13/98        Sasha Lebedev   Original

 INPUT VARIABLES: randseed = Current random number seed (long)

 OUTPUT VARIABLES: xlandau =  Output random value (float)

 ***************************************************************************
 ***************************************************************************/

//INCLUDECHECKER: Removed this line: #include <stdio.h>
#include <math.h>
//INCLUDECHECKER: Removed this line: #include <stdlib.h>

extern float utiRandom(long *randseed);

void utiLandau(long *randseed, float *xlandau)
{

  long rseed;
  float lambda,xgauss = 0.0,X,Y;
  float S = 0.449871;
  float T = -0.386595;
  float A = 0.19600;
  float B = 0.25472;
  float R2 = 0.27846;
  float Q = 1.0;
  float U = 0.1;
  float V = 0.0;
    
  /* EXECUTABLE STATEMENTS */

  rseed = *randseed;   /* copy random seed for internal use */

/* Generate Gaussian random value. RNORML cernlib function,  
   algorithm by J.L. Leva, ACM Trans. Math. Softw., v.18(1992) p. 454 */

  while((Q > R2) || (V*V > -4.0*log(U)*U*U)) 
  {
    U = utiRandom(&rseed);
    V = utiRandom(&rseed);
      V = 1.7156 * (V - 0.5);
        X = U - S;
        Y = (fabs)(V) - T;
          Q = X*X + Y*(A*Y - B*X);

            xgauss = V/U;
  }

/* Generate Landau distributed random number */

  lambda = -2.0*log((fabs)(xgauss)); 

  *xlandau = lambda;   /* return Landau distributed random value */

  *randseed = rseed;   /* return the modified seed */
  
}   /* end utiLandau */

