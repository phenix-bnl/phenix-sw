//-----------------------------------------------------------------------------
//
// Generate rapidity distributions for the CERES generator
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <TMath.h>
#include <cmath>
#include <string>
#include "InitializeYCERES.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

double y_shift(const double, const double, const double, const double);

TH1* InitializeYCERES(const int ParticleID, const ParticlePropertyList& PPList)
{
  const double mnuc  = 0.938;
  const double ebeam = 450.0;
  const double Aproj = 1.0;
  const double Atarg = 9.0;
  const double bpar  = 0.0;
  const double m_pi0 = 0.1349743;

  double sqrts        = std::sqrt(2.0*mnuc*ebeam+2.0*mnuc*mnuc);
  double y0           = std::log(2.0*ebeam/mnuc)/2.0;
  double gamma        = sqrts/(2.0*mnuc);
  double sigma_landau = std::sqrt(std::log(gamma));
  double ymax_pi0     = std::log(sqrts/m_pi0);

  ParticleProperty * PParticle = PPList.GetByID(ParticleID);
  double mass      = PParticle->GetMass();
  double ymax_part = std::log(sqrts/mass);
  double ymin_part = -1.0*ymax_part;
  double sig       = sigma_landau*(ymax_part/ymax_pi0);

  ymax_part = ymax_part*y0;
  ymin_part = ymin_part*y0;

  double ymean = y0-y_shift(Aproj,Atarg,bpar);

  const int nbins = 1000;
  const double ymin  = -2.0;
  const double ymax  = 8.0;
  const double binwidth = (ymax-ymin)/nbins;
  TH1F * yhistogram = new TH1F(createString("y",ParticleID).c_str(),
      "y",nbins,ymin,ymax);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    const double y  = ymin+(ibin-1)*binwidth+binwidth/2.0;
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

double y_shift(const double Aproj, const double Atarg,
    const double bpar)
{
  const double density = 0.168;

  if ( Aproj<2.0 )
  {
    if ( Atarg<12.0 ) return 0.0;
    return 1.0;
  }
  else
  {
    if ( sqrt((Atarg/Aproj-1.0)*(Atarg/Aproj-1.0))<0.1 ) return 0.0;
  }

  double rmin = std::exp((1.0/3.0)*std::log(0.75*Aproj/TMath::Pi()/density));
  double rmax = std::exp((1.0/3.0)*std::log(0.75*Atarg/TMath::Pi()/density));
  if ( bpar>(rmin+rmax) ) return 0.0;

  double hmax = 2.0*std::sqrt(rmax*rmax-rmin*rmin);

  double t_part, p_part;
  if ( bpar<(rmax-rmin) )
  {
    double rcyl   = rmin;
    double vmax   = 2.0*TMath::Pi()*rcyl*rcyl*hmax;
    double hmic   = rmax-std::sqrt(rmax*rmax-rmin*rmin);
    double rapb   = (rmax-hmic)/rmax;
    double vmin   = 2.0/3.0*TMath::Pi()*rmax*rmax*rmax*(1.0+0.5*rapb*rapb*rapb-1.5*rapb);
    t_part = density*(2.0*vmin+vmax);
    p_part = Aproj;
  }
  else
  {
    double rcyl   = 0.5*(rmax+rmin-bpar);
    double vmax   = 2.0*TMath::Pi()*rcyl*rcyl*hmax;
    double hmic   = rmax-std::sqrt(rmax*rmax-rmin*rmin);
    double rapc   = (rmin-2.0*rcyl)/rmin;
    t_part = density*(2.0/3.0*TMath::Pi()*rcyl*rcyl*hmic+vmax);
    p_part = density*2.0/3.0*TMath::Pi()*rmin*rmin*rmin*
      (1.0+0.5*rapc*rapc*rapc-1.5*rapc);
  }

  double shift = 0.0055*(t_part-p_part);
  return shift;
}
