//-----------------------------------------------------------------------------
//
//  Generate double Dalitz decay
//  (unsure about correctness of pair mass distributions
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <TRandom.h>
#include <gsl/gsl_math.h>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>
#include "DecayList.h"
#include "DoubleDalitzDecay.h"
#include "Momentum.h"
#include "Particle.h"
#include "Tools.h"

void DoubleDalitzDecay(Particle& PParent,
    Particle& PLepton1,
    Particle& PLepton2,
    Particle& PLepton3,
    Particle& PLepton4,
    const ParticlePropertyList& PPList,
    const Decay& PDecay)
{
  // we do not want to use this file in it's current state
  Particle PPair1, PPair2;

  PLepton1.SetID(PDecay.GetChildID(1));
  PLepton2.SetID(PDecay.GetChildID(2));
  PLepton3.SetID(PDecay.GetChildID(3));
  PLepton4.SetID(PDecay.GetChildID(4));

  double pmass=PParent.GetMass();
  double lmass=0;
  for ( int ibody=1; ibody<=4; ibody++)
  {
    int ID = PDecay.GetChildID(ibody);
    if ( std::abs(ID)==11 )
      lmass = GetMass(ID, PPList);
  }

  if (4*lmass>pmass) {
    std::cerr << "Decay kinematically impossible!" << std::endl;
    PParent.SetValid(0);
    PLepton1.SetValid(0);
    PLepton2.SetValid(0);
    PLepton3.SetValid(0);
    PLepton4.SetValid(0);
    return;
  }

  TH1F* LeptonPairMass = PDecay.GetHistogram();

  // sample the two lepton pair masses
  double lp1mass, lp2mass;
  while (true)
  {
    lp1mass = LeptonPairMass->GetRandom();
    lp2mass = LeptonPairMass->GetRandom();
    if ( pmass>(lp1mass+lp2mass)
        && lp1mass/2.>lmass
        && lp2mass/2.>lmass ) break;
  }

  // first decay the parent into the two lepton pairs
  double Epair1 = (std::pow(pmass, 2) + std::pow(lp1mass, 2) - std::pow(lp2mass, 2))/(2.*pmass);
  double Epair2 = pmass-Epair1;
  double ppair1 = std::sqrt((Epair1+lp1mass)*(Epair1-lp1mass));
  double costheta = (2.0*gRandom->Rndm())-1.0;
  double sintheta = std::sqrt((1.+costheta)*(1.-costheta));
  double phi      = 2.0*M_PI*gRandom->Rndm();
  PPair1.Set4mom(Epair1,
      ppair1*sintheta*std::cos(phi),
      ppair1*sintheta*std::sin(phi),
      ppair1*costheta);
  PPair2.Set4mom(Epair2,invert(PPair1.Get4mom()));
  double theta = PParent.Get4mom().Theta();
  phi          = PParent.Get4mom().Phi();
  costheta = std::cos(theta);
  sintheta = std::sin(theta);
  double cosphi   = std::cos(phi);
  double sinphi   = std::sin(phi);
  PPair1.Set4mom(Epair1,
      Rotate(PPair1.Get4mom(),costheta,sintheta,cosphi,sinphi));
  PPair2.Set4mom(Epair2,
      Rotate(PPair2.Get4mom(),costheta,sintheta,cosphi,sinphi));
  PPair1.Set4mom(boost_vector(PPair1.Get4mom(),PParent.Get4mom()));
  PPair2.Set4mom(boost_vector(PPair2.Get4mom(),PParent.Get4mom()));

  // then decay the first pair into two leptons
  double Elepton1 = (std::pow(lp1mass, 2) + std::pow(lmass, 2) - std::pow(lmass, 2))/(2.*lp1mass);
  double Elepton2 = lp1mass-Elepton1;
  double plepton1 = std::sqrt((Elepton1+lmass)*(Elepton1-lmass));
  costheta = (2.0*gRandom->Rndm())-1.0;
  sintheta = std::sqrt((1.+costheta)*(1.-costheta));
  phi      = 2.0*M_PI*gRandom->Rndm();
  PLepton1.Set4mom(Elepton1,
      plepton1*sintheta*std::cos(phi),
      plepton1*sintheta*std::sin(phi),
      plepton1*costheta);
  PLepton2.Set4mom(Elepton2,invert(PLepton1.Get4mom()));
  theta    = PPair1.Get4mom().Theta();
  phi      = PPair1.Get4mom().Phi();
  costheta = std::cos(theta);
  sintheta = std::sin(theta);
  cosphi   = std::cos(phi);
  sinphi   = std::sin(phi);
  PLepton1.Set4mom(Elepton1,
      Rotate(PLepton1.Get4mom(),costheta,sintheta,cosphi,sinphi));
  PLepton2.Set4mom(Elepton2,
      Rotate(PLepton2.Get4mom(),costheta,sintheta,cosphi,sinphi));
  PLepton1.Set4mom(boost_vector(PLepton1.Get4mom(),PPair1.Get4mom()));
  PLepton2.Set4mom(boost_vector(PLepton2.Get4mom(),PPair1.Get4mom()));

  // and then decay the second pair
  Elepton1 = (std::pow(lp2mass, 2) + std::pow(lmass, 2) - std::pow(lmass, 2))/(2.*lp2mass);
  Elepton2 = lp2mass-Elepton1;
  plepton1 = std::sqrt((Elepton1+lmass)*(Elepton1-lmass));
  costheta = (2.0*gRandom->Rndm())-1.0;
  sintheta = std::sqrt((1.+costheta)*(1.-costheta));
  phi      = 2.0*M_PI*gRandom->Rndm();
  PLepton3.Set4mom(Elepton1,
      plepton1*sintheta*std::cos(phi),
      plepton1*sintheta*std::sin(phi),
      plepton1*costheta);
  PLepton4.Set4mom(Elepton2,invert(PLepton3.Get4mom()));
  theta    = PPair2.Get4mom().Theta();
  phi      = PPair2.Get4mom().Phi();
  costheta = std::cos(theta);
  sintheta = std::sin(theta);
  cosphi   = std::cos(phi);
  sinphi   = std::sin(phi);
  PLepton3.Set4mom(Elepton1,
      Rotate(PLepton3.Get4mom(),costheta,sintheta,cosphi,sinphi));
  PLepton4.Set4mom(Elepton2,
      Rotate(PLepton4.Get4mom(),costheta,sintheta,cosphi,sinphi));
  PLepton3.Set4mom(boost_vector(PLepton3.Get4mom(),PPair2.Get4mom()));
  PLepton4.Set4mom(boost_vector(PLepton4.Get4mom(),PPair2.Get4mom()));

  int new_generation = PParent.GetGeneration()+1;
  PLepton1.SetGeneration(new_generation);
  PLepton2.SetGeneration(new_generation);
  PLepton3.SetGeneration(new_generation);
  PLepton4.SetGeneration(new_generation);

  PLepton1.SetDecaysum(0.0);
  PLepton2.SetDecaysum(0.0);
  PLepton3.SetDecaysum(0.0);
  PLepton4.SetDecaysum(0.0);

  PLepton1.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
  PLepton2.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
  PLepton3.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
  PLepton4.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
}
