#include <mpcNoiseContainer.h>
#include <MpcRandom.h>

using namespace std;

mpcNoiseContainer *mpcNoiseContainer::__instance = NULL;


mpcNoiseContainer *mpcNoiseContainer::instance(float term_calib)
{
  if (__instance)
    {
      return __instance;
    }

  // instantiate new MpcMap on first call
  __instance = new mpcNoiseContainer(term_calib);

  return __instance;

}

mpcNoiseContainer::mpcNoiseContainer(float term_calib)
{
  MpcRandom* mpcrand = MpcRandom::instance();
  
  float term_stochastic = 0.02626128;
  float term_noise = 0.045;
 
  for(int ich=0;ich<576;ich++){
    sigma_stochastic[ich] = term_stochastic;
    sigma_noise[ich] = term_noise;
    value_calib[ich] = mpcrand->Gaus(0,term_calib);
  }
  is_reset = 0;
}

void mpcNoiseContainer::reset_calib(float term_calib)
{
  MpcRandom* mpcrand = MpcRandom::instance();
  for(int ich=0;ich<576;ich++){
    value_calib[ich] = mpcrand->Gaus(0,term_calib);
  }
  is_reset = 1;
  return;
}

mpcNoiseContainer::~mpcNoiseContainer()
{
}
