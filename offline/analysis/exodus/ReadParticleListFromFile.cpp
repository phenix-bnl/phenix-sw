//-----------------------------------------------------------------------------
//
//  Read a stream of particles from an ASCII input file and insert
//  them into a ParticleList
//
//-----------------------------------------------------------------------------

#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include "Particle.h"
#include "ParticleList.h"
#include "ReadParticleListFromFile.h"

ParticleList * ReadParticleListFromFile(const char * file)
{
  int itotal = 0;
  std::auto_ptr<ParticleList> ReadParticleList(new ParticleList);
  PLNode* CurrentNode            = ReadParticleList->GetHeadNode();

  std::cout << "Reading particle list from file: "
    << file << std::endl;
  std::cout << std::endl;

  std::ifstream fin(file);
  char   character;
  while (fin.get(character))
  {
    int pid       = 0;
    double E      = NAN;
    double px     = NAN;
    double py     = NAN;
    double pz     = NAN;
    double weight = NAN;
    fin >> pid;
    fin >> E;
    fin >> px;
    fin >> py;
    fin >> pz;
    fin >> weight;
    std::auto_ptr<Particle> pParticle(new Particle);
    pParticle->Set4mom(E,px,py,pz);
    pParticle->SetID(pid);
    pParticle->SetWeight(weight);
    pParticle->SetDecaysum(0.0);
    pParticle->SetGeneration(1);
    if ( E>0.0 )
    {
      itotal++;
      if ( itotal % 10000 == 0 )
        std::cout << itotal << " particles read from file" << std::endl;
      ReadParticleList->InsertAfter(CurrentNode, pParticle.release());
      CurrentNode = CurrentNode->GetNextNode();
    }
  }

  fin.close();

  std::cout << std::endl;

  return ReadParticleList.release();
}
