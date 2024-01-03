#include <TF2.h>
#include <TH1.h>
#include <TMath.h>
#include <TRandom.h>
#include <cmath>
#include <iostream>
#include <string>
#include "GenerateParticles.h"
#include "Particle.h"
#include "ParticleGenerator.h"
#include "ParticleGeneratorList.h"
#include "ParticleList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"

void GenerateParticles(ParticleList *PList, const int events,
    const ParticleGeneratorList* PGList,
    const ParticlePropertyList* PPList)
{
  double pt, y;
  const int generation = 1;

  PLNode           * Current   = PList->GetHeadNode();

  int iGenerator = 1;
  ParticleGenerator* Generator  = PGList->Get(iGenerator);

  int itotal = 0;
  while (Generator)
  {
    int GeneratorID  = Generator->GetID();
    ParticleProperty* PProperty = PPList->GetByID(GeneratorID);
    double mass           = PProperty->GetMass();
    TF2* GeneratorPtYFunc = Generator->GetPtYFunction();
    TH1* GeneratorPtHist  = Generator->GetPtHistogram();
    TH1* GeneratorYHist   = Generator->GetYHistogram();
    TH1* GeneratorMHist   = Generator->GetMHistogram();
    for ( int ievent=1; ievent<=events; ievent++)
    {
      itotal++;
      if ( itotal % 10000 == 0 )
        std::cout << itotal << " particles generated" << std::endl;
      if ( !GeneratorPtYFunc )
      {
        if ( GeneratorID==-111 ) {
          pt = 20.0*gRandom->Rndm();
        } else {
          pt = 15.0*gRandom->Rndm();
        }
        y  = GeneratorYHist->GetRandom();
      }
      else
      {
        GeneratorPtYFunc->GetRandom2(pt,y);
      }
      if ( GeneratorMHist!=0 ) mass = GeneratorMHist->GetRandom();
      double phi = 2.0*TMath::Pi()*gRandom->Rndm();
      double mt  = std::sqrt(pt*pt+mass*mass);
      double E   = mt*std::cosh(y);
      double px  = pt*std::cos(phi);
      double py  = pt*std::sin(phi);
      double pz  = mt*std::sinh(y);
      int ptbin = static_cast<int>((1000.0*pt) + 1);
      double weight   = Generator->GetWeight()*GeneratorPtHist->GetBinContent(ptbin);
      double decaysum = 0.0;
      Particle* PParticle = new Particle;
      PParticle->SetID(GeneratorID);
      PParticle->Set4mom(E,px,py,pz);
      PParticle->SetVertex(0.,0.,0.);
      PParticle->SetDecaysum(decaysum);
      PParticle->SetGeneration(generation);
      PParticle->SetWeight(weight);
      PList->InsertAfter(Current, PParticle);
      Current = Current->GetNextNode();
    }
    iGenerator++;
    Generator = PGList->Get(iGenerator);
  }
}
