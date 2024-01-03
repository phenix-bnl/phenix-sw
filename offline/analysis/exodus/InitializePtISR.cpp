//-----------------------------------------------------------------------------
//
// Generate transverse-momentum distributions for the ISR generator
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <cmath>
#include <string>
#include "InitializePtISR.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

TH1* InitializePtISR(const int ParticleID, const ParticlePropertyList& PPList)
{
  ParticleProperty * PParticle = PPList.GetByID(ParticleID);
  const double mass = PParticle->GetMass();

  const double a1 = 1.0;
  const double a2 = 0.139;
  const double a3 = 0.107;
  const double T1 = 0.1;
  const double T2 = 0.23;
  const double T3 = 0.102;

  const double T  = 0.175+0.115*mass;

  const int nbins = 1000;
  const double ptmin = 0.0;
  const double ptmax = 4.0;
  const double binwidth = (ptmax-ptmin)/nbins;
  TH1F * pthistogram = new TH1F(createString("pt",ParticleID).c_str(),
      "pt",nbins,ptmin,ptmax);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    double pt = ptmin+(ibin-1)*binwidth+binwidth/2.0;
    double mt = std::sqrt(pt*pt+mass*mass);
    double weight;
    if ( ParticleID==111 )
    {
      weight = pt*(a1*std::exp(-1.0*mt/T1)+a2*std::exp(-1.0*mt/T2)+a3*std::exp(-1.0*mt/T3));
    }
    else
    {
      weight = pt*std::exp(-1.0*mt/T);
    }
    pthistogram->AddBinContent(ibin,weight);
  }

  return pthistogram;
}
