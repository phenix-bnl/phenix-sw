// $Id: TMutFitCathodeSamples.h,v 1.1 2006/04/22 01:53:01 hpereira Exp $

#ifndef TMutFitCathodeSamples_h
#define TMutFitCathodeSamples_h

namespace MUTOO
{

	//! calculate ADC samples from signal function
	int TMutCalcADCSamples(const float *trise,
  	const float* tfall,
  	const float* vpeak,
  	const float* t0,
  	const int* nsamples,
  	const float tsamples[4],
  	float charge[4],
  	const int* fitv0);
	
	//! fit cathode samples using signal line shape
	int TMutFitCathodeSamples (
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
  	char *FitType,
  	const int *FitRise,
  	const int *FitFall);
};

#endif
