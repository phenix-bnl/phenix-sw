//-----------------------------------------------------------------------------
//
//  Apply PHENIX resolution to a four momentum
//
//-----------------------------------------------------------------------------

#include <TRandom.h>
#include <cmath>
#include "ApplyPHENIXResolutionRun10.h"
#include "Momentum.h"

namespace {
  // Private helper functions
  //
  // The goal here was to group functionality into one-purpose units to keep
  // everything readable The hope is that each of them is small enough to allow
  // complete inlining. Extra conditional branches were eliminated to reduce
  // branch mispredictions.

  double sigma_pol(double x, double a, double b, double c, int n=1) {
    // calculate polynomial of the form
    //
    //    f(x) = a + b*x +c/x^n

    return a + b*x + c/std::pow(x, n);
  }

  double smear_with_xdep_sigma(double x, double a, double b, double c) {
    // smear with a Gaussian function where sigma is a function of x
    const double sigma = sigma_pol(x, a, b, c, 1);
    return gRandom->Gaus(x, sigma);
  }

  double mom_peak(double p, double a, double b, double c, double d) {
    const double ac = a-c;
    const double a_safe = std::max(0., p-a);
    const double c_safe = std::max(0., std::max(ac, p-c));
    return d + 2*b*ac*a_safe + b*std::pow(c_safe, 2);
  }

  double mom_sigma(double p, double a, double b, double c) {
    return sigma_pol(p, a, b, c, 2);
  }

  double mom_slope(double p, double a, double c) {
    return sigma_pol(p, a, 0, c, 2);
  }

  double mom_ratio(double p, double a, double b, double c) {
    const double p_safe = std::max(0., p - b);
    return c*pow(p_safe,2) + a;
  }

  double smeared_mom(double p, double p3, double p2, double p1, double p0, double s_c, double s_b, double s_a, double sl_c, double sl_a, double r0, double r1, double r2) {
    const double p_safe = std::max(p, 0.15);
    const double peak   = mom_peak(p_safe, p3, p2, p1, p0);
    const double sigma  = mom_sigma(p_safe, s_a, s_b, s_c);

    const double smear_mom =
      gRandom->Rndm() < mom_ratio(p, r0, r1, r2)
      ? gRandom->Gaus(peak, sigma)
      : -1.*gRandom->Exp(1./mom_slope(p, sl_a, sl_c)) - peak - 2.0 * sigma;

    return std::max(0., p + smear_mom);
  }
}

void ApplyPHENIXResolutionRun10(Mom4& mom4,
    double p0     , double p1     , double p2      , double p3,
    double s_c    , double s_b    , double s_a     ,
    double sl_c   , double sl_a   ,
    double r0     , double r1     , double r2      ,
    double phi_a  , double phi_b  , double phi_c   ,
    double theta_a, double theta_b, double theta_c)
{
  const double p = mom4.Mom3::Abs();
  const double mass = std::max(mom4.Mom4::Abs(), 0.51099906e-3);
  const double theta = mom4.Theta();
  const double phi   = mom4.Phi();

  const double new_phi = smear_with_xdep_sigma(phi, phi_a, phi_b, phi_c);
  const double new_theta= smear_with_xdep_sigma(theta, theta_a, theta_b, theta_c);
  const double new_p = smeared_mom(p, p3, p2, p1, p0, s_c, s_b, s_a, sl_c, sl_a, r0, r1, r2);

  const double E  = hypot(new_p, mass);
  const double px = new_p*std::sin(new_theta)*std::cos(new_phi);
  const double py = new_p*std::sin(new_theta)*std::sin(new_phi);
  const double pz = new_p*std::cos(new_theta);
  mom4.Set(E, px, py, pz);
}
