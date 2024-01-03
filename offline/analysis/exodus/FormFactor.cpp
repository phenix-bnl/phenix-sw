//-----------------------------------------------------------------------------
//
// Calculate the form factor for Dalitz decays according to the
//
//               Lepton-G parametrization
//
//-----------------------------------------------------------------------------

#include <cmath>
#include "FormFactor.h"
#include "Particle.h"
#include "ParticleProperty.h"

double FormFactor(const double q2, const ParticleProperty& PParent)
{
  const int ParentID = PParent.GetID();

  switch( ParentID )
  {
    case 111:
      return std::pow(1.0/(1.0-5.5*q2), 2);
    case 221:
      return std::pow(1.0/(1.0-1.9*q2), 2);
    case 331:
      return std::pow(0.764, 4) /
        (std::pow(std::pow(0.764, 2)-q2, 2) + std::pow(0.1020*0.764, 2));
    case 223:
      return std::pow(0.6519, 2) /
        (std::pow(std::pow(0.6519, 2)-q2, 2) + std::pow(0.04198*0.6519, 2));
    default:
      return 1;
  }
}
