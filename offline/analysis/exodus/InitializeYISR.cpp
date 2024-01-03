//-----------------------------------------------------------------------------
//
// Generate rapidity distributions for the ISR generator
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <cmath>
#include <string>
#include "InitializeYISR.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

TH1* InitializeYISR(const int ParticleID, const ParticlePropertyList& PPList)
{
  const double mnuc  = 0.938;
  const double ebeam = 450.0;
  const double m_pi0 = 0.1349743;

  double sqrts        = std::sqrt(2.0*mnuc*ebeam+2.0*mnuc*mnuc);
  double y0           = std::log(2.0*ebeam/mnuc)/2.0;
  double gamma        = sqrts/(2.0*mnuc);
  double sigma_landau = std::sqrt(log(gamma));
  double ymax_pi0     = std::log(sqrts/m_pi0);

  const ParticleProperty* PParticle = PPList.GetByID(ParticleID);
  double mass      = PParticle->GetMass();
  double ymax_part = std::log(sqrts/mass);
  double ymin_part = -1.0*ymax_part;
  double sig       = sigma_landau*(ymax_part/ymax_pi0);

  ymax_part = ymax_part*y0;
  ymin_part = ymin_part*y0;

  double ymean = y0;

  const int nbins = 1000;
  const double ymin  = -2.0;
  const double ymax  = 8.0;
  const double binwidth = (ymax-ymin)/nbins;
  TH1F * yhistogram = new TH1F(createString("y",ParticleID).c_str(),
      "y",nbins,ymin,ymax);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    double y = ymin+(ibin-1)*binwidth+binwidth/2.0;
    double sigma;
    if ( y<=ymean )
    {
      sigma = sig*(1.0-((y0-ymean)*(y0-ymean)/(ymax_part-ymin_part)));
    }
    else
    {
      sigma = sig*(1.0+((y0-ymean)*(y0-ymean)/(ymax_part-ymin_part)));
    }
    double weight = std::exp(0.75*(y-ymean)/sigma)+std::exp(-0.75*(y-ymean)/sigma);
    weight = 1.0/(weight*weight);
    yhistogram->AddBinContent(ibin,weight);
  }

  return yhistogram;
}
