//-----------------------------------------------------------------------------
//
//  Read a stream of particles from an ASCII input file and insert
//  them into a ParticleList
//
//-----------------------------------------------------------------------------

#include <fstream>
#include <memory>
#include "GetEventFromInputStream.h"
#include "Particle.h"
#include "ParticleList.h"

ParticleList* GetEventFromInputStream(std::ifstream* input_file)
{
  char character;
  if (not input_file->get(character))
    return 0;

  int pid       = 0;
  double E      = 0.0;
  double px     = 0.0;
  double py     = 0.0;
  double pz     = 0.0;
  double weight = 0.0;

  *input_file >> pid;
  *input_file >> E;
  *input_file >> px;
  *input_file >> py;
  *input_file >> pz;
  *input_file >> weight;
  std::auto_ptr<Particle> pParticle(new Particle);
  pParticle->Set4mom(E,px,py,pz);
  pParticle->SetID(pid);
  pParticle->SetWeight(weight);
  pParticle->SetDecaysum(0.0);
  pParticle->SetGeneration(1);
  if ( E>0.0 ) {
    std::auto_ptr<ParticleList> ReadParticleList(new ParticleList);
    PLNode* CurrentNode = ReadParticleList->GetHeadNode();
    ReadParticleList->InsertAfter(CurrentNode, pParticle.release());
    return ReadParticleList.release();
  } else {
    return 0;
  }
}
