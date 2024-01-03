//-----------------------------------------------------------------------------
//
//  Main steering routine of the decay machine
//
//-----------------------------------------------------------------------------



#include <TRandom.h>
#include <cassert>
#include <cstdlib>
#include <cstddef>
#include <functional>
#include <iostream>
#include <string>
#include <vector>
#include "AdjustDecaySum.h"
#include "DalitzDecay.h"
#include "DecayList.h"
#include "DoAllDecays.h"
#include "DoubleDalitzDecay.h"
#include "Particle.h"
#include "ParticleList.h"
#include "ThreeBodyDecay.h"
#include "TwoBodyDecay.h"

void DoAllDecays(ParticleList& PList,
    const ParticlePropertyList& PPList,
    const DecayList& DList)
{
  PLNode* CurrentNode = PList.GetHeadNode();

  int particle_counter = 0;
  int itotal = 0;
  while (true)
  {
    particle_counter++;
    if ( particle_counter>PList.GetLength() ) break;

    CurrentNode = CurrentNode->GetNextNode();
    Particle* PParent = CurrentNode->Get(0);

    if ( PParent->GetGeneration()==1 )
    {
      itotal++;
      if ( itotal % 10000 == 0 )
        std::cout << itotal << " primary particles done" << std::endl;
    }

    if (std::equal_to<double>()(PParent->GetDecaysum(),0.))
      continue;

    int possible_decays = 0;
    int selected_decay = 0;
    Decay* PDecay;
    double decay_matrix[5][2];
    for ( int decay_counter=1; decay_counter<=DList.GetSize(); decay_counter++ )
    {
      PDecay = DList.Get(decay_counter);
      if ( (PParent->GetID())!=(PDecay->GetParentID()) )
        continue;
      decay_matrix[possible_decays][0] = decay_counter;
      if ( possible_decays == 0 )
        decay_matrix[possible_decays][1]
          = PDecay->GetBranchingRatio()/PDecay->GetBRSum();
      else
        decay_matrix[possible_decays][1]
          = decay_matrix[possible_decays-1][1]
          + PDecay->GetBranchingRatio()/PDecay->GetBRSum();
      possible_decays++;
    }

    if (not possible_decays)
      continue;

    double rndm = gRandom->Rndm();
    for ( int decay_counter=1; decay_counter<=possible_decays;
        decay_counter++ )
    {
      if ( !selected_decay && rndm<=decay_matrix[decay_counter-1][1] )
        selected_decay = static_cast<int>(decay_matrix[decay_counter-1][0]);
    }
    PDecay = DList.Get(selected_decay);

    std::vector<Particle*> PChildren;
    for (int i=0; i<PDecay->GetNBody(); ++i) PChildren.push_back(new Particle);

    double new_weight = -1;
    if ( PDecay->GetNBody()==2 )
    {
      TwoBodyDecay(*PParent, *PChildren[0], *PChildren[1], PPList, *PDecay);
      new_weight = PParent->GetWeight()*PParent->GetDecaysum();
    }
    else if ( PDecay->GetNBody()==3 )
    {
      unsigned nlep=0;
      for( int nb=1;nb<=3;nb++ )
      {
        if ( std::abs(PDecay->GetChildID(nb))==11 )
          nlep++;
      }
      if ( nlep==2 )
        DalitzDecay(*PParent, *PChildren[0], *PChildren[1], *PChildren[2], PPList, *PDecay);
      else
        ThreeBodyDecay(*PParent, *PChildren[0], *PChildren[1], *PChildren[2], PPList, *PDecay);
      new_weight = PParent->GetWeight()*PParent->GetDecaysum();
    }
    else if ( PDecay->GetNBody()==4 )
    {
      unsigned nlep=0;
      for( int nb=1;nb<=4;nb++ )
      {
        if ( std::abs(PDecay->GetChildID(nb))==11 )
          nlep++;
      }
      if ( nlep==4 )
      {
        DoubleDalitzDecay(*PParent, *PChildren[0], *PChildren[1], *PChildren[2], *PChildren[3], PPList, *PDecay);
        new_weight = PParent->GetWeight()*PParent->GetDecaysum();
      }
    }
    else
      std::cout << "Decays with more than 3 decay products are currently "
        << "not implemented" << std::endl;

    for (size_t i=0; i<PChildren.size(); ++i) {
      if (!PDecay->GetChildrenStable())
        AdjustDecaySum(*PChildren[i], DList);

      // Make a hard check here since above ways new_weight gets assigned
      // aren't really simple and we don't want the default to propagate.
      assert(new_weight>=0);
      PChildren[i]->SetWeight(new_weight);
      PList.InsertAfter(CurrentNode,PChildren[i]);
    }
  }
}
