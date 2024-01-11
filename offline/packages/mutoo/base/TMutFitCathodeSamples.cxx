// $Id: TMutFitCathodeSamples.cxx,v 1.2 2017/07/11 03:53:12 phnxbld Exp $
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_min.h>

#include <cmath>
#include <string.h>

#include "TMutFitCathodeSamples.h"

using namespace std;

/*!

	\file    Logbook.cc
	\author  Melynda Brooks, LANL, mbrooks@lanl.gov
	\version $Revision: 1.2 $
	\date    $Date: 2017/07/11 03:53:12 $

	Routine to use GSL routines to fit 4 ADC samples to a pulse shape and return
	the peak value and t0 of pulse. The pulse shape is represented by:                                    

	V = -V0*(1 - exp[-(t-t0)/trise]) * exp[-(t-t0)/tfall]           
	ADC = gain * V + pedestal                                       
	
	and times should be in nanoseconds.                                   

	f_wood contains the function that is to be minimized (the chi-square  
	value for the above fit function minus the measured values)         
	f_wood_df calculates the derivative of the above function w.r.t. the
	fit parameters.                                                 

	parameters:                                                                  

	IN:  
	stime[4] - time of 4 samples                                     
	scharge[4] - charge of 4 samples                                 
	sigma[4] - error on charge of each sample                        
	nsamples - the number of samples we are fitting                  
	trise = rise time of pulse                                       
	tfall = fall time of pulse                                       

	FitType = 
	"exp" - Fit pulse to double exponential                
	"samp1" - Return sample #1 as peak                     	
	"samp2" - Return sample #2 as peak                     
	"samp3" - Return sample #3 as peak                     
	"samp4" - Return sample #4 as peak                     
	"avg"   - Return average of samples 2-4                

	OUT:    
	V0 - Fitted v0 of pulse shape                                    
	t0 - Fitted t0 of pulse shape                                    
	vpeak - Fitted peak of pulse                                     
	tpeak - Fitted time of pulse peak                                

*/

//____________________________________________________
struct xy_params{
  double x[4];
  double y[4];
  double sigma[4];
  double trise;
  double tfall;
  int npoints;
  int nparams;
  int FitRise;
  int FitFall;
};

//____________________________________________________
/* stopping parameters for line search */
const double EPSABS_LINE = 0.00001 ;
const double EPSREL_LINE = 0.00001 ;
const double EPSABS = 0.001;
const unsigned int MAX_ITERATIONS_LINE = 10;

//____________________________________________________
double f_wood(const gsl_vector * x, void * params)
{
  int i ;
  double chi;
  float calccharge[4];
  float trise, tfall, v0f, t0f;
  float time[4];
  struct xy_params* p = (struct xy_params*)params;

  double v0 = gsl_vector_get(x,0);
  double t0 = gsl_vector_get(x,1);

  chi = 0;
  if (p->FitRise){
    if (p->FitFall)trise = (float)gsl_vector_get(x,3);
    else trise = (float)gsl_vector_get(x,2);
  }
  else{
    trise = (float)p->trise;
  }

  if (p->FitFall){
    tfall = (float)gsl_vector_get(x,2);
  }
  else{
    tfall = (float)p->tfall;
  }

  v0f = (float)v0;
  t0f = (float)t0;
  for (i=0; i<p->npoints; i++)time[i] = (float)p->x[i];
  MUTOO::TMutCalcADCSamples(&trise, &tfall, &v0f, &t0f, &p->npoints, time, calccharge, 0);
  for (i=0; i < p->npoints; i++){
    chi+= pow((p->y[i] - (double)calccharge[i])/(double)(p->sigma[i]), 2);
  }
 
//  printf("%f: chi =",chi);
//  printf ("%f,%f,%f,%f: charge =",calccharge[0],calccharge[1],calccharge[2],calccharge[3]);
  return chi;

}

//____________________________________________________
void f_wood_df(const gsl_vector * x, void * params,gsl_vector * df)
{
  int i;
  double delta;
  double yfit;
  double param[4], aj;
  double deriv[4];
  static double deltaa[4] = {0.01, 0.01, 0.01, 0.01};

  struct xy_params* p = (struct xy_params*)params;
  gsl_vector* y = (gsl_vector*)x;

  param[0] = gsl_vector_get(x,0);
  param[1] = gsl_vector_get(x,1);
  if (p->FitRise || p->FitFall) param[2] = gsl_vector_get(x,2);
  if (p->FitRise && p->FitFall) param[3] = gsl_vector_get(x,3);
 
  for (i=0; i<p->nparams; i++){
    aj = param[i];

    if (aj != 0.0) delta = deltaa[i]*aj;
    else delta = deltaa[i];

    param[i] = aj + delta;
    gsl_vector_set(y,i,param[i]);

    yfit = f_wood(y, params);
    param[i] = aj - delta;
    gsl_vector_set(y,i,param[i]);

    if (delta != 0.0) deriv[i] = (yfit - f_wood(y, params))/(2.0*delta);
    else deriv[i] = 0.0; 

    param[i] = aj;
    gsl_vector_set(y,i,param[i]);

  }


  gsl_vector_set(df,0, deriv[0] );
  gsl_vector_set(df,1, deriv[1] );
  if (p->FitRise || p->FitFall) gsl_vector_set(df,2, deriv[2] );
  if (p->FitRise && p->FitFall) gsl_vector_set(df,3, deriv[3] );
}

//____________________________________________________
void f_wood_fdf(const gsl_vector * x, void * params,double *f,gsl_vector * df)
{
  f_wood_df(x,params,df);
  *f=f_wood(x,params);
}

//____________________________________________________
int MUTOO::TMutCalcADCSamples(
  const float* trise,
  const float* tfall,
  const float* vpeak,
  const float* t0, 
  const int* nsamples, 
  const float tsamples[4], 
  float charge[4],
  const int* fitv0)

{

  int isample;
  float tpeak;     //time of pulse peak
  float v0;        //coefficient of pulse-shape equation

	/*  
		Calculate what the peak time should be (maximum of pulse), put this
  	into equation for pulse shape so that V0 can be determined for this
	  value of vpeak, then calculate what V would be for the nsamples.
	*/
	
  if (fitv0){
    tpeak = *trise*log(1 + *tfall/(*trise)) + *t0;
    v0 = (*vpeak)/((1 - exp(-tpeak/(*trise)))*exp(-tpeak/(*tfall)));
  } else v0 = *vpeak;

  for (isample=0; isample<*nsamples; isample++){
    charge[isample] = 
				-v0*(1 - exp((*t0 - tsamples[isample])/(*trise))) *
				exp((*t0 - tsamples[isample])/(*tfall));
  }

  return 1;

}

//____________________________________________________
//! fit cathode samples using signal line shape
int MUTOO::TMutFitCathodeSamples (
		const float stime[4], 
  	const float scharge[4],
  	const float sigma[4], 
  	const int *nsamples, 
  	float *trise, 
  	float *tfall,
  	float *v0f, 
  	float *t0f, 
  	float *vpeak, 
  	float *tpeak,
  	float *chisquare,
  	char* FitType,
  	const int *FitRise,
  	const int *FitFall)
{

    float calccharge[4];
    float time[4];
    const gsl_multimin_fdfminimizer_type *T;

    *chisquare = -999;

    if ( !strncmp(FitType, "samp1", 5)){
      *v0f = 0.0;
      *t0f = ((stime[2] - stime[0])/(scharge[2] - scharge[0]))*
              (-scharge[1]) + stime[1];
      *tpeak = stime[0];
      *vpeak = -scharge[0];
    }        
    else if ( !strncmp(FitType, "samp2", 5)){
      *v0f = 0.0;
      *t0f = ((stime[2] - stime[0])/(scharge[2] - scharge[0]))*
              (-scharge[1]) + stime[1];
      *tpeak = stime[1];
      *vpeak = -scharge[1];
    }
    else if ( !strncmp(FitType, "samp3", 5)) {
      *v0f = 0.0;
      *t0f = ((stime[2] - stime[0])/(scharge[2] - scharge[0]))*
              (-scharge[1]) + stime[1];
      *tpeak = stime[2];
      *vpeak = -scharge[2];
    }
    else if ( !strncmp(FitType, "samp4", 5)) {
      *v0f = 0.0;
      *t0f = ((stime[2] - stime[0])/(scharge[2] - scharge[0]))*
              (-scharge[1]) + stime[1];
      *tpeak = stime[3];
      *vpeak = -scharge[3];
    }
    else if ( !strncmp(FitType, "avg", 5)) {
      *v0f = 0.0;
      *t0f = 0.0;
      *tpeak = (stime[3] + stime[2] + stime[1])/3.0;
      *vpeak = -(scharge[3] + scharge[2] + scharge[1])/3.0;
      *t0f = ((*tpeak - stime[0])/(-(*vpeak) - scharge[0]))*
              (*vpeak) + *tpeak;
    }
    else if ( !strncmp(FitType, "exp", 3)){

      gsl_multimin_function_fdf f;
      gsl_multimin_fdfminimizer *s;
      gsl_vector * x;
      size_t iterations = 0;
      
			int status;
      struct xy_params params;

      int NPARAMS = 2;
      if (*FitRise) NPARAMS++;
      if (*FitFall) NPARAMS++;

      for( int i=0; i<4; i++ )
			{
        params.sigma[i] = (double)sigma[i];
        params.x[i] = (double)stime[i];
        params.y[i] = (double)scharge[i];
      }
      params.npoints = *nsamples;
      params.nparams = NPARAMS;
      params.trise = (double)*trise;
      params.tfall = (double)*tfall;
      params.FitRise = *FitRise;
      params.FitFall = *FitFall;
      f.params = &params; 

      f.f=f_wood;
      f.df=f_wood_df;
      f.fdf=f_wood_fdf;
      f.n=NPARAMS;

      x=gsl_vector_calloc(f.n);
      gsl_vector_set(x,0,-2.0*scharge[3]);
      gsl_vector_set(x,1,1.0);

      if (*FitFall) gsl_vector_set(x,2,(double)(*tfall));

      if (*FitRise) {
        if (*FitFall) gsl_vector_set(x,3,(double)(*trise));
        else gsl_vector_set(x,2,(double)(*trise));
      }
      T = gsl_multimin_fdfminimizer_steepest_descent;
      s = gsl_multimin_fdfminimizer_alloc(T, NPARAMS);
      gsl_multimin_fdfminimizer_set(s, &f, x, 10.0, EPSABS);

      do 
        {
          iterations++;
          status = gsl_multimin_fdfminimizer_iterate(s);
          if (status) 
	    {
	      break;
	    }
	  status = gsl_multimin_test_gradient(s->gradient, EPSABS);
	}
      while (status == GSL_CONTINUE && iterations < 10);
	
      // Return pulse shape parameters and the time of the peak
      // maximum and the peak maximum.  Make sure to return vpeak as a
      // _positive_ value.

      *v0f = (float)gsl_vector_get(s->x,0);
      *t0f = (float)gsl_vector_get(s->x,1);
      if (*FitFall) *tfall = (float)gsl_vector_get(s->x,2);
			if (*FitRise) *trise = (*FitFall) ? (float)gsl_vector_get(s->x,3):(float)gsl_vector_get(s->x,2);

      *tpeak = (*trise)*log(1+*tfall/(*trise)) + *t0f;
      *vpeak = (*v0f)*(1 - exp((*t0f - *tpeak)/(*trise)))*exp((*t0f - *tpeak)/(*tfall));
  
      *chisquare = 0;
      for( int i=0; i<params.npoints; i++) time[i] = (float)params.x[i];
			
      MUTOO::TMutCalcADCSamples(trise, tfall, v0f, t0f, &params.npoints, time, calccharge, 0);
      
			for( int i=0; i < params.npoints; i++ )
      *chisquare+= pow((params.y[i] - (double)calccharge[i])/(double)(params.sigma[i]), 2);

      gsl_vector_free(x);
      gsl_multimin_fdfminimizer_free(s);

 
  }   // if fitting exponentials
  return 1;
}
