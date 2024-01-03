//-----------------------------------------------------------------------------
//
//  Adjust decay sum of particles in a particle list according to the
//  decays in a decay list
//
//-----------------------------------------------------------------------------

#include "AdjustDecaySum.h"
#include "DecayList.h"
#include "Particle.h"
#include "ParticleList.h"

void AdjustDecaySum(ParticleList& PList, const DecayList&  DList)
{
  PLNode* CurrentNode = PList.GetHeadNode();
  Particle * CurrentParticle;

  for ( int i=1; i<=PList.GetLength(); i++ )
  {
    CurrentNode = CurrentNode->GetNextNode();
    CurrentParticle = CurrentNode->Get(0);
    int pid = CurrentParticle->GetID();
    for ( int j=1; j<=DList.GetSize(); j++ )
    {
      Decay * CurrentDecay = DList.Get(j);
      if ( pid==CurrentDecay->GetParentID() )
      {
        CurrentParticle->SetDecaysum(CurrentDecay->GetBRSum());
        break;
      }
    }
  }
}

void AdjustDecaySum(Particle& PParticle, const DecayList& DList)
{
  int pid = PParticle.GetID();

  for ( int j=1; j<=DList.GetSize(); j++ )
  {
    Decay * CurrentDecay = DList.Get(j);
    if ( pid==CurrentDecay->GetParentID() )
    {
      PParticle.SetDecaysum(CurrentDecay->GetBRSum());
      break;
    }
  }
}
