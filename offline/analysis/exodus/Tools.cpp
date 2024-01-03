//-----------------------------------------------------------------------------
//
//  Implementation of some useful tools acting on momenta and particles
//
//-----------------------------------------------------------------------------

#include <gsl/gsl_math.h>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <limits>
#include "Momentum.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

Mom3 invert(const Mom3& input)
{
  return Mom3(-input.Getpx(), -input.Getpy(), -input.Getpz());
}

Mom4 boost_vector(const Mom4& pinit, const Mom4& pframe)
{
  double massofparent = pframe.Abs();
  Mom3 eta = pframe*(-1./massofparent);
  double gamma = pframe.GetE()/massofparent;
  Mom3 pl;
  if ( eta*eta < std::numeric_limits<double>::epsilon() )
    pl.Set(0.,0.,0.);
  else
    pl = eta*((eta*pinit)/(eta*eta));
  const double E = (gamma*pinit.GetE()-eta*pinit);
  const Mom3 p(pl*(gamma-1)+pinit-eta*pinit.GetE());
  return Mom4(E, p);
}

Mom3 Rotate(const Mom3& pold, const double costheta, const double sintheta,
    const double cosphi, const double sinphi)
{
  const double px = pold.Getpx()*costheta*cosphi
    -pold.Getpy()*sinphi
    +pold.Getpz()*sintheta*cosphi;
  const double py = pold.Getpx()*costheta*sinphi
    +pold.Getpy()*cosphi
    +pold.Getpz()*sintheta*sinphi;
  const double pz = -pold.Getpx()*sintheta
    +pold.Getpz()*costheta;
  return Mom3(px, py, pz);
}

double GetMass(const int id, const ParticlePropertyList& Index)
{
  return Index.GetByID(id)->GetMass();
}

double GetWidth(const int id, const ParticlePropertyList& Index)
{
  return Index.GetByID(id)->GetWidth();
}

double phiPHENIX(const double phi) {
  if (phi > 1.5*M_PI)
    return phi - 2*M_PI;
  else if (phi < -0.5*M_PI)
    return phi + 2*M_PI;
  else
    return phi;
}

std::string getDataFileName(const std::string& basename) {
  const std::string prefix = std::getenv("OFFLINE_MAIN");
  const std::string in_data_dir_name = prefix + "/share/exodus/" + basename;
  if (std::ifstream(basename.c_str())) {
    return basename;
  } else if (std::ifstream((in_data_dir_name).c_str())) {
    return in_data_dir_name;
  } else {
    std::cerr << "ERROR: Required file " << basename << " not found -- ABORTING" << std::endl;
    abort();
  }
}

