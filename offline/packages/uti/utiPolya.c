/****************************************************************************
 ****************************************************************************

 utiPolya
 -----------

 DESCRIPTION: 	Generates random number according to Polya distribution
	        with Polya parameter = 1.5

 AUTHOR/CONTACT:  Sasha Lebedev, ISU

 REVISIONS:
       Date            Author          Description

       04/23/98        Sasha Lebedev   Fast version
       02/03/98        Sasha Lebedev   Original

 INPUT VARIABLES: randseed = current random number seed

		  param = Polya distribution parameter

 OUTPUT VARIABLES: xpolya = output random number

 ***************************************************************************
 ***************************************************************************/

//INCLUDECHECKER: Removed this line: #include <stdio.h>
#include <math.h>
//INCLUDECHECKER: Removed this line: #include <stdlib.h>

extern float utiRandom(long *ldum);

void utiPolya(long *randseed, float *xpolya)

{

  static short firstcall = 0;
  static float integral[1000];

  int check,i;
  long rseed;
  float param,f,buff[1000],lambda,step,shift,pran;
    
  /* EXECUTABLE STATEMENTS */

  step = 0.01; 
  shift = 0.005; 
  param=1.5;

  if(firstcall==0) /* initialize integral array */
  {
    for(i=0; i<1000; i++)
    {
      lambda=i*step+shift;
      buff[i]=param/0.8862*sqrt(param*lambda)*exp(-param*lambda); 
/* 0.8862 = Gamma(1.5) */
        if(i>0)
        {
          integral[i]=integral[i-1]+buff[i];
        }
        else
        {
          integral[i]=buff[i];
        }
    }
  firstcall = 1;
  }

  check = 0;
  rseed = *randseed;   /* for internal use */
  pran=1.0;
  while (check == 0) 
    {
      f = utiRandom(&rseed);
      if (f>0.0001 && f<0.9999) check = 1;
    }
   
   i=0;
   while(i<1000)
   {
     if(f<integral[i]/integral[999])
     {
     pran=i*step+shift; 
     break;
     }
   i++;
   }
   
  *xpolya = pran;   /* return random number */

  *randseed = rseed;   /* return the modified seed */
  
}   /* end utiPolya */

