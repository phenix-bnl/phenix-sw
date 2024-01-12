#include <iostream>

#include <TSingleParticleCustomGenerator.h>
#include <TClonesArray.h>
#include <Hepevt.h>
#include <TMCParticle.h>
#include <TLorentzVector.h>
#include <TDatabasePDG.h>

using namespace std;

ClassImp(TSingleParticleCustomGenerator);              

TSingleParticleCustomGenerator::TSingleParticleCustomGenerator(const char* name, const char* title) :
  TSingleParticleGenerator(name,title)
{
}


void
TSingleParticleCustomGenerator::GenerateVertex()
{
  _vtx[0] = 0.1;	// x
  _vtx[1] = 0.2;	// y
  _vtx[2] = 0.3;	// z
}

void
TSingleParticleCustomGenerator::GenerateMomentum()
{
  _p[0] = 1.;	// px
  _p[1] = 2.;	// py
  _p[2] = 3.;	// pz
  _p[3] = sqrt(_mass*_mass + _p[0]*_p[0] + _p[1]*_p[1] + _p[2]*_p[2]);	// e
}

