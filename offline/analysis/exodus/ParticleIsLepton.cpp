//-----------------------------------------------------------------------------
//
//  Check whether a perticle is a lepton or not
//
//-----------------------------------------------------------------------------

#include <cstdlib>
#include "Particle.h"
#include "ParticleIsLepton.h"

bool ParticleIsLepton(const Particle& PParticle)
{
  const int pid = PParticle.GetID();

  if ( std::abs(pid)==11 || std::abs(pid)==13 )
  {
    return(true);
  }
  else
  {
    return(false);
  }
}
