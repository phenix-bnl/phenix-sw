//-----------------------------------------------------------------------------
//
// Generate transverse-momentum distributions for the PHENIX generator
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <gsl/gsl_math.h>
#include <cmath>
#include <iostream>
#include <limits>
#include <string>
#include "InitializePtPHENIX.h"
#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "Tools.h"

TH1* InitializePtPHENIX(const int setup, const int ParticleID,
    const double f_c, const double f_p0, const double f_a,
    const double f_b, const double f_n,
    const double t, const double w, const double A, const double p0,
    const double m, const double B, const double n,
    const ParticlePropertyList& PPList)
{
  const ParticleProperty* PParticle = PPList.GetByID(ParticleID);
  const double mass = PParticle->GetMass();
  const double pimass = GetMass(111,PPList);

  int nbins;
  double ptmin, ptmax;
  if ( ParticleID == -111 ) {
    nbins = 20000;
    ptmin = 0.0;
    ptmax = 20.0;
  } else {
    nbins = 15000;
    ptmin = 0.0;
    ptmax = 15.0;
  }

  const double binwidth = (ptmax-ptmin)/nbins;
  TH1F * pthistogram = new TH1F(createString("pt",ParticleID).c_str(),"pt",nbins,ptmin,ptmax);

  double weight = 1.0;
  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    double pt = ptmin+(ibin-1)*binwidth+binwidth/2.0;
    double mt = std::sqrt(pt*pt+mass*mass);
    double t_fo = 0.1;
    double beta = 0.0;

    switch(setup)
    {
      case 1:  if ( ibin==1 )
                 std::cout << "Initializing power law for particle "
                   << ParticleID << std::endl;

               weight = pt*std::exp(9.99*std::log(1.219/
                     (1.219+std::sqrt(pt*pt+mass*mass-pimass*pimass))));

               break;

      case 2:  if ( ibin==1 )
                 std::cout << "Initializing exponential with flow for particle "
                   << ParticleID << std::endl;

               // t_fo = 0.157; // PID==111
               // beta = 0.40;
               // t_fo = 0.179; // other
               // beta = 0.0;
               t_fo = 0.157;
               beta = 0.40;
               weight = pt*std::exp(-1.0*mt/(t_fo+beta*beta*mass));

               break;

      case 3:  if ( ibin==1 )
                 std::cout << "AuAu: Initializing most realistic parameterization "
                   << "for particle " << ParticleID << std::endl;

               weight = pt * f_c /
                 std::pow(std::exp(-f_a*std::sqrt(mt*mt-pimass*pimass)
                       -f_b*std::sqrt(mt*mt-pimass*pimass)*
                       std::sqrt(mt*mt-pimass*pimass)) +
                     std::sqrt(mt*mt-pimass*pimass)/f_p0,f_n);

               // direct photons
               if ( ParticleID == -111 ) {
                 weight = pt/std::pow(std::exp(0.1*pt)+pt/0.34,5.89);
               }

               // Ke3
               if ( ParticleID == 21 ) {
                 weight = pt/std::pow(1.8+pt,17.0);
               }

               break;

      case 4:  if ( ibin==1 )
                 std::cout << "pp: Initializing most realistic parameterization "
                   << "for particle " << ParticleID << std::endl;

               weight = pt * f_c /
                 std::pow(std::exp(-f_a*std::sqrt(mt*mt-pimass*pimass)
                       -f_b*std::sqrt(mt*mt-pimass*pimass)*
                       std::sqrt(mt*mt-pimass*pimass)) +
                     std::sqrt(mt*mt-pimass*pimass)/f_p0,f_n);

               // direct photons
               if ( ParticleID == -111 ) {
                 weight = pt/std::pow(std::exp(0.1*pt)+pt/0.34,5.89);
               }

               // Ke3
               if ( ParticleID == 21 ) {
                 weight = pt*std::exp(-1.0*pt/0.453)/std::pow(0.408+pt,7.249);
               }

               break;

       case 5: if (ibin == 1)
               std::cout << "dAu: Initializing the best parameterization "
	                  << "for particle " << ParticleID << std::endl;

               if (ParticleID==111 or // Pion
                   ParticleID==221 or // Eta
                   ParticleID==331 or // Etaprime
                   ParticleID==113 or // Rho
                   ParticleID==223 or // Omega
                   ParticleID==333) // Phi
                 weight = 2*M_PI * pt *
                   (((1/(1+std::exp((std::sqrt(mt*mt-pimass*pimass)-t)/w)))*
                     (A/std::pow(1.0+(std::sqrt(mt*mt-pimass*pimass)/p0),m))) +
                    ((1-(1/(1+std::exp((std::sqrt(mt*mt-pimass*pimass)-t)/w))))*
                     (B/std::pow(0.2+(std::sqrt(mt*mt-pimass*pimass)),n))));

               // J/Psi and Psiprime:
               // no good guess here
               else if (ParticleID == 443 or ParticleID==444)
                 weight = 0;

               else
                 weight = 0;

               break;

      default: if ( ibin==1 )
               {
                 std::cerr << "Error: pt setup " << setup
                   << " not predefined" << std::endl;
                 std::cerr << "using flat pt distribution" << std::endl;
               }

               weight = 1.0;

               break;
    }

    pthistogram->AddBinContent(ibin,weight);
  }

  if (pthistogram->Integral() > std::numeric_limits<double>::epsilon()) {
    pthistogram->Scale(nbins/pthistogram->Integral());
  }

  return pthistogram;
}
