//-----------------------------------------------------------------------------
//
//  Write a stream of particles in an OSCAR compliant ASCII file
//
//-----------------------------------------------------------------------------

#include <cmath>
#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include "Momentum.h"
#include "Particle.h"
#include "ParticleList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "WriteParticleListToOscarFile.h"

void WriteParticleListToOscarFile(const char* file, const ParticleList& PList,
    const ParticlePropertyList& PPList)
{
  PLNode   * CurrentNode     = PList.GetHeadNode();

  std::cout << "Writing particle list to OSCAR compliant file: " << file << std::endl;

  std::ofstream fout(file);

  fout << "# OSC1999A" << std::endl;
  fout << "# final_id_p_x" << std::endl;
  fout << "# EXODUS event generator in single particle mode" << std::endl;
  fout << "#" << std::endl;

  fout.precision(6);
  for ( int i=1; i<=PList.GetLength(); i++)
  {
    CurrentNode = CurrentNode->GetNextNode();
    Particle *CurrentParticle( CurrentNode->Get(0) );
    int pid = CurrentParticle->GetID();
    ParticleProperty *Species = PPList.GetByID(pid);
    Mom4 four_momentum = CurrentParticle->Get4mom();
    double E  = four_momentum.GetE();
    double px = four_momentum.Getpx();
    double py = four_momentum.Getpy();
    double pz = four_momentum.Getpz();
    double mass;
    if ( Species->GetWidth() < std::numeric_limits<double>::epsilon() ) {
      mass = Species->GetMass();
    } else {
      mass = E*E-(px*px+py*py+pz*pz);
      if ( mass>0. )
        mass = std::sqrt(mass);
      else
        mass = 0.;
    }
    double xvtx = CurrentParticle->GetxVertex()*1.0e+13;
    double yvtx = CurrentParticle->GetyVertex()*1.0e+13;
    double zvtx = CurrentParticle->GetzVertex()*1.0e+13;

    fout << "0 1" << std::endl;
    fout << "0 " << pid << " 0 ";
    fout << px   << " ";
    fout << py   << " ";
    fout << pz   << " ";
    fout << E    << " ";
    fout << mass << " ";
    fout << xvtx << " ";
    fout << yvtx << " ";
    fout << zvtx << " ";
    fout << "0"  << std::endl;
    fout << "0 0" << std::endl;

  }

  fout.close();

  return;
}
