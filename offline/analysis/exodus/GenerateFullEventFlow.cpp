#include <TF1.h>
#include <TF2.h>
#include <TH1.h>
#include <TRandom.h>
#include <gsl/gsl_math.h>
#include <cmath>
#include <cstdlib>
#include "GenerateFullEventFlow.h"
#include "Particle.h"
#include "ParticleGenerator.h"
#include "ParticleGeneratorList.h"
#include "ParticleList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"

double v2phi(const double pt, const TF1& f_v2, TF1& f_dNdphi)
{
  double v2;
  if (pt <= 2.92)  v2 = f_v2.Eval(pt,0.0,0.0);
  else v2 = f_v2.Eval(2.92,0.0,0.0);
  f_dNdphi.SetParameters(&v2);
  double phi = f_dNdphi.GetRandom();
  return phi;
}

void GenerateFullEventFlow(ParticleList& PList, const int dnch_dy, const double zVTXmax,
    const ParticleGeneratorList& PGList,
    const ParticlePropertyList& PPList,
    TF1& f_dNdphi, const TF1& f_v2)
{
  const int generation = 1;

  PLNode* Current = PList.GetHeadNode();

  int iGenerator = 1;
  ParticleGenerator* Generator = PGList.Get(iGenerator);

  double zVTX = 2.0*zVTXmax*(gRandom->Rndm()-0.5);
  double phiR = 2.0*M_PI*gRandom->Rndm();

  while (Generator)
  {
    int GeneratorID       = Generator->GetID();
    ParticleProperty* PProperty = PPList.GetByID(GeneratorID);
    double mass                 = PProperty->GetMass();
    double GeneratorWeight      = Generator->GetWeight();
    TF2* GeneratorPtYFunc       = Generator->GetPtYFunction();
    TH1* GeneratorPtHist        = Generator->GetPtHistogram();
    TH1* GeneratorYHist         = Generator->GetYHistogram();
    TH1* GeneratorMHist         = Generator->GetMHistogram();
    int multiplicity = gRandom->Poisson(2*dnch_dy*GeneratorWeight);

    if ( std::abs(GeneratorID)==211 ||
        std::abs(GeneratorID)==321 ||
        std::abs(GeneratorID)==2212 ||
        GeneratorID==111 ||
        GeneratorID==221 ||
        GeneratorID==331 )
    {
      GeneratorWeight = 1.0;
    }

    for ( int ievent=1; ievent<=multiplicity; ievent++ )
    {
      double pt, y;
      if ( !GeneratorPtYFunc )
      {
        pt = GeneratorPtHist->GetRandom();
        y  = GeneratorYHist->GetRandom();
      }
      else
      {
        GeneratorPtYFunc->GetRandom2(pt,y);
      }
      if ( GeneratorMHist!=0 ) mass = GeneratorMHist->GetRandom();
      double phi = v2phi(pt, f_v2,f_dNdphi) + phiR;
      double mt  = std::sqrt(pt*pt+mass*mass);
      double E        = mt*std::cosh(y);
      double px       = pt*std::cos(phi);
      double py       = pt*std::sin(phi);
      double pz       = mt*std::sinh(y);
      double decaysum = 0.0;
      Particle* PParticle = new Particle;
      PParticle->SetID(GeneratorID);
      PParticle->Set4mom(E,px,py,pz);
      PParticle->SetVertex(0.0,0.0,zVTX);
      PParticle->SetDecaysum(decaysum);
      PParticle->SetGeneration(generation);
      PParticle->SetWeight(GeneratorWeight);
      PList.InsertAfter(Current, PParticle);
      Current = Current->GetNextNode();
    }
    iGenerator++;
    Generator = PGList.Get(iGenerator);
  }
}
