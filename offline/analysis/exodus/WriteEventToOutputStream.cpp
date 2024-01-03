//-----------------------------------------------------------------------------
//
//  Write a stream of particles in an ASCII file
//
//-----------------------------------------------------------------------------

#include <fstream>
#include <string>
#include "Momentum.h"
#include "Particle.h"
#include "ParticleList.h"
#include "WriteEventToOutputStream.h"

void WriteEventToOutputStream(std::ofstream * output_file, const ParticleList& PList)
{
  PLNode   * CurrentNode     = PList.GetHeadNode();

  output_file->precision(6);

  for ( int i=1; i<=PList.GetLength(); i++)
  {
    CurrentNode     = CurrentNode->GetNextNode();
    Particle* CurrentParticle = CurrentNode->Get(0);
    Mom4 four_momentum   = CurrentParticle->Get4mom();
    double E  = four_momentum.GetE();
    double px = four_momentum.Getpx();
    double py = four_momentum.Getpy();
    double pz = four_momentum.Getpz();
    output_file->precision(10);
    *output_file << E      << " ";
    *output_file << px     << " ";
    *output_file << py     << " ";
    *output_file << pz     << " ";
  }
  *output_file << std::endl;

  return;
}
