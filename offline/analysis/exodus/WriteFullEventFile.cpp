//-----------------------------------------------------------------------------
//
//  Write a stream of particles in an OSCAR compliant ASCII file
//
//-----------------------------------------------------------------------------

#include <fstream>
#include <iomanip>
#include <string>
#include "Momentum.h"
#include "Particle.h"
#include "ParticleList.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"
#include "WriteFullEventFile.h"

void WriteFullEventFile(std::ofstream& fout, const int eventID,
    const ParticleList& PList,
    const ParticlePropertyList& PPList)
{
  PLNode*   CurrentNode = PList.GetHeadNode();

  CurrentNode     = CurrentNode->GetNextNode();
  Particle* CurrentParticle = CurrentNode->Get(0);
  PLNode* NextNode = 0;
  Particle* NextParticle = 0;
  if (PList.GetLength()>1) {
    NextNode       = CurrentNode->GetNextNode();
    NextParticle = NextNode->Get(0);
  }

  int final_state_particles = 0;
  for ( int i=1; i<=PList.GetLength(); i++)
  {
    bool final_state = false;
    if ( i != PList.GetLength() ) {
      if ( CurrentParticle->GetGeneration() >= NextParticle->GetGeneration() )
        final_state = true;
    } else {
      final_state = true;
    }
    if (CurrentParticle->GetWeight() == 0)
      final_state = false;
    if ( final_state ) final_state_particles++;
    CurrentNode     = NextNode;
    CurrentParticle = NextParticle;
    if ( i<PList.GetLength()-1 )
    {
      NextNode = CurrentNode->GetNextNode();
      NextParticle    = NextNode->Get(0);
    }
  }
  if (not final_state_particles) return;

  CurrentNode     = PList.GetHeadNode();
  NextNode        = 0;
  NextParticle    = 0;

  CurrentNode     = CurrentNode->GetNextNode();
  CurrentParticle = CurrentNode->Get(0);
  if ( PList.GetLength()>1 )
  {
    NextNode        = CurrentNode->GetNextNode();
    NextParticle    = NextNode->Get(0);
  }

  fout << "0 " << final_state_particles << '\n';

  int particle_in_file = 0;
  for ( int i=1; i<=PList.GetLength(); i++)
  {
    bool final_state = false;
    if ( i != PList.GetLength() ) {
      if ( CurrentParticle->GetGeneration() >= NextParticle->GetGeneration() )
        final_state = true;
    } else {
      final_state = true;
    }
    if (CurrentParticle->GetWeight() == 0)
      final_state = false;
    if ( final_state )
    {
      const int pid             = CurrentParticle->GetID();
      ParticleProperty* Species = PPList.GetByID(pid);
      double mass               = Species->GetMass();
      Mom4 four_momentum        = CurrentParticle->Get4mom();
      double E                  = four_momentum.GetE();
      double px                 = four_momentum.Getpx();
      double py                 = four_momentum.Getpy();
      double pz                 = four_momentum.Getpz();
      double xvtx               = CurrentParticle->GetxVertex()*1.0e+13;
      double yvtx               = CurrentParticle->GetyVertex()*1.0e+13;
      double zvtx               = CurrentParticle->GetzVertex()*1.0e+13;
      particle_in_file++;
      fout << std::setw(2)  << particle_in_file << " ";
      fout << std::setw(4)  << pid              << " ";
      fout << std::setw(1)  << eventID          << " ";
      fout << std::setw(12) << std::scientific << px   << " ";
      fout << std::setw(12) << std::scientific << py   << " ";
      fout << std::setw(12) << std::scientific << pz   << " ";
      fout << std::setw(12) << std::scientific << E    << " ";
      fout << std::setw(12) << std::scientific << mass << " ";
      fout << std::setw(12) << std::scientific << xvtx << " ";
      fout << std::setw(12) << std::scientific << yvtx << " ";
      fout << std::setw(12) << std::scientific << zvtx << " ";
      fout << std::setw(1) << "0"              << '\n';
    }
    CurrentNode     = NextNode;
    CurrentParticle = NextParticle;
    if ( i<PList.GetLength()-1 )
    {
      NextNode = CurrentNode->GetNextNode();
      NextParticle    = NextNode->Get(0);
    }
  }
  fout << "0 0 " << '\n';

  return;

}
