#include <MpcRandom.h>
#include <fstream>
#include <iostream>

using namespace std;

MpcRandom *MpcRandom::__instance = NULL;

MpcRandom *MpcRandom::instance()
{
  if (__instance)
    {
      return __instance;
    }

  // instantiate new MpcRandom on first call
  __instance = new MpcRandom();

  return __instance;
}

MpcRandom::MpcRandom()
{
  UInt_t seed;
  ifstream devrandom;
  devrandom.open("/dev/random",ios::binary);
  devrandom.read((char*)&seed,sizeof(seed) );
  devrandom.close();

  random3.SetSeed( seed );
  cout << "MpcRandom::MpcRandom, Setting seed to " << seed << endl;

/*
  gsl_rng_env_setup();
  gsl_rand_type = gsl_rng_default;
  gsl_rand_gen = gsl_rng_alloc( gsl_rand_type );
*/

}

MpcRandom::~MpcRandom()
{
//  gsl_rng_free( gsl_rand_gen );
}

