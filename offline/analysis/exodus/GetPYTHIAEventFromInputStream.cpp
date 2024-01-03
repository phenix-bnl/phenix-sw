//-----------------------------------------------------------------------------
//
//  Read a stream of particles from an ASCII input file and insert
//  them into a ParticleList
//
//-----------------------------------------------------------------------------

#include <fstream>
#include <limits>
#include <memory>
#include "GetPYTHIAEventFromInputStream.h"
#include "Particle.h"
#include "ParticleIsLepton.h"
#include "ParticleList.h"

ParticleList* GetPYTHIAEventFromInputStream(std::ifstream * input_file, const int generator)
{
  std::auto_ptr<ParticleList> ReadParticleList(new ParticleList);
  PLNode* CurrentNode = ReadParticleList->GetHeadNode();

  char character;
  if (not input_file->get(character))
    return ReadParticleList.release();

  int pid       = 0;
  double E      = 0.0;
  double px     = 0.0;
  double py     = 0.0;
  double pz     = 0.0;
  double weight = 0.0;

  for (int icount=0; icount<generator; icount++)
  {
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
    if ( ParticleIsLepton(*pParticle) )
    {
      pParticle->SetGeneration(2);
    }
    else
    {
      pParticle->SetGeneration(1);
    }
    if ( E>0.0 )
    {
      ReadParticleList->InsertAfter(CurrentNode, pParticle.release());
      CurrentNode = CurrentNode->GetNextNode();
    }
  }
  if (E < std::numeric_limits<double>::epsilon())
    return 0;
  return ReadParticleList.release();
}
