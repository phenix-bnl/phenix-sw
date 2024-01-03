//-----------------------------------------------------------------------------
//
//  Apply CERES resolution to a four momentum
//
//-----------------------------------------------------------------------------

#include <TRandom.h>
#include <cmath>
#include "ApplyCERESResolution.h"
#include "Momentum.h"

void ApplyCERESResolution(Mom4& mom4)
{
  double mass  = mom4.Mom4::Abs();
  double E     = mom4.GetE();
  double p     = E>=mass ? std::sqrt(E*E-mass*mass) : 0;
  double theta = mom4.Theta();
  double phi   = mom4.Phi();

  double sigma = p*std::sqrt(0.053*p*0.053*p+0.041*0.041);
  p     = gRandom->Gaus(p,sigma);
  sigma = 0.6e-03;
  theta = gRandom->Gaus(theta,sigma);
  sigma = 3.0e-03;
  phi   = gRandom->Gaus(phi,sigma);

  E         = std::sqrt(p*p+mass*mass);
  double px = p*std::sin(theta)*std::cos(phi);
  double py = p*std::sin(theta)*std::sin(phi);
  double pz = p*std::cos(theta);

  mom4.Set(E, px, py, pz);
}
