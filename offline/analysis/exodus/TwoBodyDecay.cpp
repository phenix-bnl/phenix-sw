//-----------------------------------------------------------------------------
//
//  Generate two-body decay
//
//-----------------------------------------------------------------------------

#include <TMath.h>
#include <TRandom.h>
#include <cmath>
#include <iostream>
#include <string>
#include "Bremsstrahlung.h"
#include "DecayList.h"
#include "Momentum.h"
#include "Particle.h"
#include "Tools.h"
#include "TwoBodyDecay.h"

class ParticlePropertyList;

void TwoBodyDecay(Particle& PParent,
    Particle& PChild1,
    Particle& PChild2,
    const ParticlePropertyList& PPList,
    const Decay& PDecay)
{
  PChild1.SetID(PDecay.GetChildID(1));
  PChild2.SetID(PDecay.GetChildID(2));

  const double mp = PParent.GetMass();
  const double md1 = GetMass(PDecay.GetChildID(1),PPList);
  const double md2 = GetMass(PDecay.GetChildID(2),PPList);

  if ( mp<md1+md2 )
  {
    std::cout << "Decay kinematically impossible!" << std::endl;
    PParent.SetValid(0);
    PChild1.SetValid(0);
    PChild2.SetValid(0);
    return;
  }

  const double Ed1 = (std::pow(mp, 2) + std::pow(md1, 2) - std::pow(md2, 2)) / (2.*mp);
  const double Ed2 = mp-Ed1;
  const double pd1 = std::sqrt((Ed1+md1)*(Ed1-md1));

  double costheta = (2.0*gRandom->Rndm())-1.0;
  double sintheta = std::sqrt((1.+costheta)*(1.-costheta));
  double phi      = 2.0*TMath::Pi()*gRandom->Rndm();

  const int new_generation = PParent.GetGeneration()+1;
  PChild1.SetGeneration(new_generation);
  PChild2.SetGeneration(new_generation);

  PChild1.SetDecaysum(0.0);
  PChild2.SetDecaysum(0.0);

  PChild1.Set4mom(Ed1,
      pd1*sintheta*std::cos(phi),
      pd1*sintheta*std::sin(phi),
      pd1*costheta);
  PChild2.Set4mom(Ed2,invert(PChild1.Get4mom()));

  const double theta  = PParent.Get4mom().Theta();
  phi           = PParent.Get4mom().Phi();
  costheta      = std::cos(theta);
  sintheta      = std::sin(theta);
  const double cosphi = std::cos(phi);
  const double sinphi = std::sin(phi);

  PChild1.Set4mom(Ed1,
      Rotate(PChild1.Get4mom(),costheta,sintheta,cosphi,sinphi));
  PChild2.Set4mom(Ed2,
      Rotate(PChild2.Get4mom(),costheta,sintheta,cosphi,sinphi));

  PChild1.Set4mom(boost_vector(PChild1.Get4mom(),PParent.Get4mom()));
  PChild2.Set4mom(boost_vector(PChild2.Get4mom(),PParent.Get4mom()));

  PChild1.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
  PChild2.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());

  // add internal radiation for J/Psi or Psiprime or phi or omega or rho or eta or Upsilon(1S,2S,3S)
  if (PParent.GetID()==443 or
      PParent.GetID()==444 or
      PParent.GetID()==333 or
      PParent.GetID()==223 or
      PParent.GetID()==113 or
      PParent.GetID()==221 or
      PParent.GetID()==553 or
      PParent.GetID()==100553 or
      PParent.GetID()==200553)
    Bremsstrahlung::InternalBremsstrahlung(PChild1, PChild2);
}
