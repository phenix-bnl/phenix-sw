//-----------------------------------------------------------------------------
//
//  Apply PHENIX resolution to a four momentum
//
//-----------------------------------------------------------------------------

#include <TRandom.h>
#include <cmath>
#include "ApplyPHENIXResolution.h"
#include "Momentum.h"

void ApplyPHENIXResolution(Mom4& mom4, const double A, const double B)
{
  double p = mom4.Mom3::Abs();

  const double mass = std::max(mom4.Mom4::Abs(), 0.51099906e-3);

  const double theta = mom4.Theta();
  const double phi   = mom4.Phi();

  const double sigma = p*std::sqrt(p*p*B*B + A*A);
  p     = std::max(gRandom->Gaus(p,sigma), 0.);

  const double E  = std::max(hypot(p, mass), p);
  const double px = p*std::sin(theta)*std::cos(phi);
  const double py = p*std::sin(theta)*std::sin(phi);
  const double pz = p*std::cos(theta);
  mom4.Set(E, px, py, pz);
}
