// $Id: PHGslRng.cxx,v 1.3 2007/11/18 12:36:13 hpereira Exp $

//////////////////////////////////////////////////////////////////
/*! 
  \file PHGslRng.cxx
  \brief wrapper around gsl randome generator initialization
  \author	Hugo Pereira
  \version $Revision: 1.3 $
  \date		$Date: 2007/11/18 12:36:13 $
*/
//////////////////////////////////////////////////////////////////

#include "PHGslRng.h"
#include "MUTOO.h"

//______________________________________________________________________
PHGslRng::PHGslRng( unsigned long int seed, bool verbose )
{
    
  gsl_rng_env_setup();
  const gsl_rng_type *T = gsl_rng_default;
  _rng = gsl_rng_alloc(T);
  
  // assign seed
  if( seed ) gsl_rng_set( _rng, seed );
  
  if( verbose )
  {
    std::cout << "PHGslRng::PHGslRng - Initialializing RNG from env (GSL_RNG_TYPE, GSL_RNG_SEED)" << std::endl;
    std::cout << "PHGslRng::PHGslRng - random number generator name: " << std::string(gsl_rng_name(_rng)) << std::endl;
    std::cout << "PHGslRng::PHGslRng - random number generator seed: " << gsl_rng_default_seed << std::endl;
    std::cout << "PHGslRng::PHGslRng - random number generator first value: " << gsl_rng_get(_rng) << std::endl;
  }
  
}

//______________________________________________________________________
PHGslRng::~PHGslRng( void )
{ gsl_rng_free( _rng ); }
