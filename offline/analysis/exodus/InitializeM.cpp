//-----------------------------------------------------------------------------
//
// Generate mass distributions
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <cmath>
#include <limits>
#include <string>
#include "InitializeM.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

TH1* InitializeM(const int ParticleID, double mass_min,
    double mass_max, const ParticlePropertyList& PPList)
{
  const double pimass = 0.13956995;
  const double emass  = 0.00051099906;

  ParticleProperty * PParticle = PPList.GetByID(ParticleID);
  double vmass  = PParticle->GetMass();
  double vwidth = PParticle->GetWidth();

  const int nbins = 10000;
  if ( mass_min < std::numeric_limits<float>::epsilon() && mass_max < std::numeric_limits<float>::epsilon() )
  {
    mass_min  = 2.0*pimass;
    mass_max  = 5.0;
  }
  const double binwidth  = (mass_max-mass_min)/nbins;
  TH1F * mhistogram = new TH1F(createString("mass",ParticleID).c_str(),
      "mass",nbins,mass_min,mass_max);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    const double mass_bin = mass_min+(ibin-1)*binwidth+binwidth/2.0;
    double weight;
    if ( ParticleID==113 || ParticleID==223 || ParticleID==333 )
    {
      weight = GounarisSakurai(mass_bin,vmass,vwidth,emass);
      if ( ParticleID==113 )
      {
        weight = weight*(1-4*pimass*pimass/mass_bin/mass_bin);
        weight = weight* exp(-mass_bin/.17);
      }
    }
    else
    {
      weight = Lorentz(mass_bin,vmass,vwidth);
    }
    mhistogram->AddBinContent(ibin,weight);
  }

  return mhistogram;
}

double GounarisSakurai(const double mass, const double vmass,
    const double vwidth, const double emass)
{
  const double pimass = 0.13956995;

  const double corr = vwidth*(vmass/mass) *
    std::exp(1.5*std::log((mass*mass/4.0-pimass*pimass)/
          (vmass*vmass/4.0-pimass*pimass)));
  const double epsilon = (emass/mass)*(emass/mass);

  if ( 1.0-4.0*epsilon>=0.0 )
  {
    return std::sqrt(1.0-4.0*epsilon)*(1.0+2.0*epsilon)/
      ((vmass*vmass-mass*mass)*(vmass*vmass-mass*mass)+
       (vmass*corr)*(vmass*corr));
  }

  return 0;
}

double Lorentz(const double mass, const double vmass, const double vwidth)
{
  double weight = (vwidth*vwidth/4.0)/(vwidth*vwidth/4.0+(vmass-mass)*(vmass-mass));

  return weight;
}
