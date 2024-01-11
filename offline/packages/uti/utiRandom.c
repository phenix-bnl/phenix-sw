/* --------------------------------------------------------------

 utiRandom - Generic random number generator

 DESCRIPTION: Generates a random number between 0 and 1 (exclusive of 0.0
              and 1.0).  Taken from p. 280 of Numerical Methods in C.
             
 INPUT VARIABLES: idum = random number seed

 OUTPUT VARIABLES returns the random number

 AUTHOR:
 J. T. Mitchell - 8/12/97 - Originally typed in.
                                                  
--------------------------------------------------------------- */

//INCLUDECHECKER: Removed this line: #include <stdio.h>
//INCLUDECHECKER: Removed this line: #include <math.h>
//INCLUDECHECKER: Removed this line: #include <stdlib.h>

#define UTI_IA 16807
#define UTI_IM 2147483647
#define UTI_AM (1.0/UTI_IM)
#define UTI_IQ 127773
#define UTI_IR 2836
#define UTI_NTAB 32
#define UTI_NDIV (1+(UTI_IM-1)/UTI_NTAB)
#define UTI_EPS 1.2e-7
#define UTI_RNMX (1.0-UTI_EPS)

float utiRandom(long *idum)
{

  int j;
  long k;
  static long iy=0;
  static long iv[UTI_NTAB];
  float temp;

  if (*idum<=0 || !iy)
    {
      if (-(*idum) < 1) 
	{
	  *idum=1;
	}
      else
	{
	  *idum = -(*idum);
	}
      for (j=UTI_NTAB+7; j>=0; j--)
	{
	  k = (*idum)/UTI_IQ;
	  *idum = UTI_IA*(*idum-k*UTI_IQ)-UTI_IR*k;
	  if (*idum < 0) *idum += UTI_IM;
	  if (j < UTI_NTAB) iv[j] = *idum;
	}
      iy = iv[0];
    }   /* if *idum<=0 || !iy */

  k = (*idum)/UTI_IQ;
  *idum = UTI_IA*(*idum-k*UTI_IQ) - UTI_IR*k;
  if (*idum < 0) *idum += UTI_IM;
  j = iy/UTI_NDIV;
  iy = iv[j];
  iv[j] = *idum;
  if ((temp=UTI_AM*iy) > UTI_RNMX) 
    {
      return UTI_RNMX;
    }
  else
    {
      return temp;
    }

}   /* end utiRandom */
