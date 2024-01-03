//-----------------------------------------------------------------------------
//
//  Write a stream of particles in an ASCII file
//
//-----------------------------------------------------------------------------

#include <fstream>
#include <iostream>
#include <string>
#include "Momentum.h"
#include "Particle.h"
#include "ParticleList.h"
#include "WriteParticleListToFile.h"

void WriteParticleListToFile(const char* file, const ParticleList& PList)
{
  PLNode   * CurrentNode     = PList.GetHeadNode();

  std::cout << "Writing particle list to file: "
    << file << std::endl;

  std::ofstream fout(file);

  fout.precision(6);
  fout << std::endl;
  for ( int i=1; i<=PList.GetLength(); i++)
  {
    CurrentNode               = CurrentNode->GetNextNode();
    Particle* CurrentParticle = CurrentNode->Get(0);
    int pid                   = CurrentParticle->GetID();
    Mom4 four_momentum        = CurrentParticle->Get4mom();
    double E                  = four_momentum.GetE();
    double px                 = four_momentum.Getpx();
    double py                 = four_momentum.Getpy();
    double pz                 = four_momentum.Getpz();
    double weight             = CurrentParticle->GetWeight();
    fout << pid    << " ";
    fout.precision(12);
    fout << E      << " ";
    fout << px     << " ";
    fout << py     << " ";
    fout << pz     << " ";
    fout.precision(6);
    fout << weight << " ";
    fout << std::endl;
  }

  fout.close();

  return;
}
