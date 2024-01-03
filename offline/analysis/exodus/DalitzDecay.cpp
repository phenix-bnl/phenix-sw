//-----------------------------------------------------------------------------
//
//  Generate Dalitz decay
//
//-----------------------------------------------------------------------------



#include <TH1.h>
#include <TRandom.h>
#include <gsl/gsl_math.h>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>
#include "DalitzDecay.h"
#include "DecayList.h"
#include "Momentum.h"
#include "Particle.h"
#include "Tools.h"

void DalitzDecay(Particle& PParent,
    Particle& PLepton1,
    Particle& PLepton2,
    Particle& POther,
    const ParticlePropertyList& PPList,
    const Decay& PDecay)
{
  double pmass=PParent.GetMass();
  double lmass=0;
  double omass=0;

  for (int ibody=1; ibody<=3; ibody++) {
    switch( PDecay.GetChildID(ibody) ) {
      case -11:   PLepton1.SetID(-11); break;
      case  11:   PLepton2.SetID(11); break;
      case -13:   PLepton1.SetID(-13); break;
      case  13:   PLepton2.SetID(13); break;
      default :   POther.SetID(PDecay.GetChildID(ibody));
    }
  }

  for( int ibody=1; ibody<=3; ibody++)
  {
    int ID = PDecay.GetChildID(ibody);
    if ( std::abs(ID)==11 || std::abs(ID)==13 )
      lmass = GetMass(PDecay.GetChildID(ibody), PPList);
    else
      omass = GetMass(PDecay.GetChildID(ibody), PPList);
  }

  if (2*lmass+omass>pmass) {
    std::cerr << "Decay kinematically impossible!" << std::endl;
    PParent.SetValid(0);
    PLepton1.SetValid(0);
    PLepton2.SetValid(0);
    POther.SetValid(0);
    return;
  }

  TH1F* LeptonPairMass = PDecay.GetHistogram();

  double lpmass;
  while (true)
  {
    lpmass = LeptonPairMass->GetRandom();
    if ( pmass-omass>lpmass && lpmass/2.>lmass ) break;
  }

  double E1          = lpmass/2.;
  double p1          = std::sqrt((E1+lmass)*(E1-lmass));
  double beta_square = 1.0 - 4.0*(lmass*lmass)/(lpmass*lpmass);
  double lambda      = beta_square/(2.0-beta_square);
  double costheta, sintheta, cosphi, sinphi, phi;
  if ( omass<0.01 )
  {
    do
    {
      costheta = (2.0*gRandom->Rndm())-1.;
    }
    while ( (1.0+lambda*costheta*costheta)<(2.0*gRandom->Rndm()) );
  }
  else
  {
    costheta = (2.0*gRandom->Rndm())-1.;
  }
  sintheta = std::sqrt((1.+costheta)*(1.-costheta));
  phi      = 2.0*M_PI*gRandom->Rndm();
  sinphi   = std::sin(phi);
  cosphi   = std::cos(phi);

  int new_generation = PParent.GetGeneration()+1;
  PLepton1.SetGeneration(new_generation);
  PLepton2.SetGeneration(new_generation);
  POther.SetGeneration(new_generation);

  PLepton1.SetDecaysum(0.0);
  PLepton2.SetDecaysum(0.0);
  POther.SetDecaysum(0.0);

  PLepton1.Set4mom(E1,
      p1*sintheta*cosphi,
      p1*sintheta*sinphi,
      p1*costheta);
  PLepton2.Set4mom(E1,invert(PLepton1.Get4mom()));

  double E3       = (std::pow(pmass, 2) + std::pow(omass, 2) - std::pow(lpmass, 2))/(2.*pmass);
  double p3       = std::sqrt((E3+omass)*(E3-omass));
  costheta = (2.0*gRandom->Rndm())-1.;
  sintheta = std::sqrt((1.+costheta)*(1.-costheta));
  phi      = 2.0*M_PI*gRandom->Rndm();
  sinphi   = std::sin(phi);
  cosphi   = std::cos(phi);

  POther.Set4mom(E3,
      p3*sintheta*cosphi,
      p3*sintheta*sinphi,
      p3*costheta);

  PLepton1.Set4mom(E1,
      Rotate(PLepton1.Get4mom(),costheta,-sintheta,-cosphi,-sinphi));
  PLepton2.Set4mom(E1,
      Rotate(PLepton2.Get4mom(),costheta,-sintheta,-cosphi,-sinphi));

  Mom4 lp_boost(std::sqrt(std::pow(p3, 2) + std::pow(lpmass, 2)),invert(POther.Get4mom()));
  PLepton1.Set4mom(boost_vector(PLepton1.Get4mom(),lp_boost));
  PLepton2.Set4mom(boost_vector(PLepton2.Get4mom(),lp_boost));

  PLepton1.Set4mom(boost_vector(PLepton1.Get4mom(),PParent.Get4mom()));
  PLepton2.Set4mom(boost_vector(PLepton2.Get4mom(),PParent.Get4mom()));
  POther.Set4mom(boost_vector(POther.Get4mom(),PParent.Get4mom()));

  PLepton1.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
  PLepton2.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
  POther.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
}
