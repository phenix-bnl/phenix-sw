//-----------------------------------------------------------------------------
//
//  Generate three-body decay (except Dalitz decay)
//
//-----------------------------------------------------------------------------

#include <TMath.h>
#include <TRandom.h>
#include <cmath>
#include <iostream>
#include <string>
#include "DecayList.h"
#include "Momentum.h"
#include "Particle.h"
#include "ThreeBodyDecay.h"
#include "Tools.h"

void ThreeBodyDecay(Particle& PParent,
    Particle& PChild1,
    Particle& PChild2,
    Particle& PChild3,
    const ParticlePropertyList& PPList,
    const Decay& PDecay)
{
  PChild1.SetID(PDecay.GetChildID(1));
  PChild2.SetID(PDecay.GetChildID(2));
  PChild3.SetID(PDecay.GetChildID(3));

  double mp = PParent.GetMass();
  double md[3];
  for (int i=1; i<=3; i++)
  {
    md[i-1] = GetMass(PDecay.GetChildID(i), PPList);
  }

  if ( mp < md[0]+md[1]+md[2] )
  {
    std::cerr << "Decay kinematically impossible!" << std::endl;
    PParent.SetValid(0);
    PChild1.SetValid(0);
    PChild2.SetValid(0);
    PChild3.SetValid(0);
    return;
  }

  double m12lo = std::pow(md[0]+md[1], 2);
  double m12hi = std::pow(mp-md[2], 2);
  double m13lo = std::pow(md[0]+md[2], 2);
  double m13hi = std::pow(mp-md[1], 2);

  double m12, m13, t1, e1s, e3s, m13min, m13max;
  while (true)
  {
    m12 = std::sqrt(m12lo+(m12hi-m12lo)*gRandom->Rndm());
    m13 = std::sqrt(m13lo+(m13hi-m13lo)*gRandom->Rndm());
    e1s = (m12*m12+md[0]*md[0]-md[1]*md[1])/(2.0*m12);
    e3s = (mp*mp-m12*m12-md[2]*md[2])/(2.0*m12);

    t1 = e1s*e1s-md[0]*md[0];
    if ( t1<0 ) continue;

    m13max = std::sqrt(std::pow(e1s+e3s, 2) - std::pow(std::sqrt(t1)-std::sqrt(std::pow(e3s, 2)-std::pow(md[2], 2)), 2));
    m13min = std::sqrt(std::pow(e1s+e3s, 2) - std::pow(std::sqrt(t1)+std::sqrt(std::pow(e3s, 2)-std::pow(md[2], 2)), 2));

    if( m13<=m13max && m13>=m13min ) break;
  }

  double Ed3 = (std::pow(mp, 2) + std::pow(md[2], 2) - std::pow(m12, 2)) / (2.0*mp);
  double pd3 = std::sqrt((Ed3+md[2])*(Ed3-md[2]));

  double costheta = 2.0*gRandom->Rndm()-1.;
  double sintheta = std::sqrt((1.+costheta)*(1.-costheta));
  double phi      = 2.0*TMath::Pi()*gRandom->Rndm();
  double sinphi   = std::sin(phi);
  double cosphi   = std::cos(phi);

  int new_generation = PParent.GetGeneration()+1;
  PChild1.SetGeneration(new_generation);
  PChild2.SetGeneration(new_generation);
  PChild3.SetGeneration(new_generation);

  PChild1.SetDecaysum(0.0);
  PChild2.SetDecaysum(0.0);
  PChild3.SetDecaysum(0.0);

  PChild3.Set4mom(Ed3,pd3*sintheta*cosphi,pd3*sintheta*sinphi,pd3*costheta);

  double Ed1      = (m12*m12+md[0]*md[0]-md[1]*md[1])/(2.*m12);
  double pd1      = std::sqrt((Ed1+md[0])*(Ed1-md[0]));
  costheta = 2.0*gRandom->Rndm()-1.0;
  sintheta = std::sqrt((1.-costheta)*(1.+costheta));
  phi      = 2.0*TMath::Pi()*gRandom->Rndm();

  PChild1.Set4mom(Ed1,
      pd1*sintheta*std::cos(phi),
      pd1*sintheta*std::sin(phi),
      pd1*costheta);
  PChild2.Set4mom(std::sqrt(PChild1.Get4mom().Mom3::Abs()+std::pow(md[1], 2)),
      invert(PChild1.Get4mom()));

  double E12 = std::sqrt(std::pow(pd3, 2) + std::pow(m12, 2));
  Mom4 p4boost(E12,invert(PChild3.Get4mom()));

  PChild1.Set4mom(boost_vector(PChild1.Get4mom(),p4boost));
  PChild2.Set4mom(boost_vector(PChild2.Get4mom(),p4boost));

  PChild1.Set4mom(boost_vector(PChild1.Get4mom(),PParent.Get4mom()));
  PChild2.Set4mom(boost_vector(PChild2.Get4mom(),PParent.Get4mom()));
  PChild3.Set4mom(boost_vector(PChild3.Get4mom(),PParent.Get4mom()));

  PChild1.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
  PChild2.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());
  PChild3.SetVertex(PParent.GetxVertex(),
      PParent.GetyVertex(),
      PParent.GetzVertex());

}
