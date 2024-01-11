#ifndef __MPCRANDOM_H__
#define __MPCRANDOM_H__

#include <PHObject.h>
#include <TRandom3.h>
//#include <gsl/gsl_rng.h>
//#include <gsl/gsl_randist.h>

class MpcRandom
{
public:
  MpcRandom();
  virtual ~MpcRandom();
  static MpcRandom *instance();

  TRandom3& GetRandom3() { return random3; }
  Double_t Gaus(const Double_t mean, const Double_t sigma) {
    return random3.Gaus(mean,sigma);
    //return mean + gsl_ran_gaussian(gsl_rand_gen,sigma);
  }
  Double_t Uniform(const Double_t x1, const Double_t x2) {
    return random3.Uniform(x1,x2);
    //return mean + gsl_ran_gaussian(gsl_rand_gen,sigma);
  }

private:

  static MpcRandom *__instance;
  TRandom3 random3;
  //const gsl_rng_type *gsl_rand_type;
  //gsl_rng *gsl_rand_gen;
};

#endif	// __MPCRANDOM_H__

