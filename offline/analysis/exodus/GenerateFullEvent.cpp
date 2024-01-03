#include <TF2.h>
#include <TH1.h>
#include <TMath.h>
#include <TRandom.h>
#include <cmath>
#include "GenerateFullEvent.h"
#include "Particle.h"
#include "ParticleGenerator.h"
#include "ParticleGeneratorList.h"
#include "ParticleList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"

void GenerateFullEvent(ParticleList *PList, const int dnch_dy, const double zVTXmax,
    ParticleGeneratorList *PGList,
    ParticlePropertyList *PPList)
{
  double pt, y, mass, phi;
  double mt, E,px,py, pz, decaysum;
  double sample_pt     = 0.0;
  double sample_y      = 0.0;
  const int generation = 1;
  int    multiplicity  = 1;
  int    GeneratorID;
  double GeneratorWeight;
  TF2*   GeneratorPtYFunc;
  TH1*   GeneratorPtHist;
  TH1*   GeneratorYHist;
  TH1*   GeneratorMHist;

  Particle*         PParticle;
  ParticleProperty* PProperty;
  PLNode*           Current = PList->GetHeadNode();

  int iGenerator = 1;
  ParticleGenerator* Generator  = PGList->Get(iGenerator);

  const double zVTX = 2.0*zVTXmax*(gRandom->Rndm()-0.5);

  while (Generator)
  {
    GeneratorID      = Generator->GetID();
    PProperty        = PPList->GetByID(GeneratorID);
    mass             = PProperty->GetMass();
    GeneratorWeight  = Generator->GetWeight();
    GeneratorPtYFunc = Generator->GetPtYFunction();
    GeneratorPtHist  = Generator->GetPtHistogram();
    GeneratorYHist   = Generator->GetYHistogram();
    GeneratorMHist   = Generator->GetMHistogram();
    multiplicity     = gRandom->Poisson(2*dnch_dy*GeneratorWeight);

    for ( int ievent=1, itotal=0; ievent<=multiplicity; ievent++, itotal++ )
    {
      itotal++;
      if ( !GeneratorPtYFunc )
      {
        pt = GeneratorPtHist->GetRandom();
        y  = GeneratorYHist->GetRandom();
      }
      else
      {
        GeneratorPtYFunc->GetRandom2(sample_pt,sample_y);
        pt = sample_pt;
        y  = sample_y;
      }
      if ( GeneratorMHist!=0 ) mass = GeneratorMHist->GetRandom();
      phi      = 2.0*TMath::Pi()*gRandom->Rndm();
      mt       = std::sqrt(pt*pt+mass*mass);
      E        = mt*std::cosh(y);
      px       = pt*std::cos(phi);
      py       = pt*std::sin(phi);
      pz       = mt*std::sinh(y);
      decaysum = 0.0;
      PParticle = new Particle;
      PParticle->SetID(GeneratorID);
      PParticle->Set4mom(E,px,py,pz);
      PParticle->SetVertex(0.0,0.0,zVTX);
      PParticle->SetDecaysum(decaysum);
      PParticle->SetGeneration(generation);
      PParticle->SetWeight(1.0);
      PList->InsertAfter(Current, PParticle);
      Current = Current->GetNextNode();
    }
    iGenerator++;
    Generator = PGList->Get(iGenerator);
  }
}
