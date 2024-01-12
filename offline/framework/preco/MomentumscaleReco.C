#include "PHCompositeNode.h"
#include "MomentumscaleReco.h"

#include "phool.h"

#include "PHObject.h"
#include "DchTrack.h"
#include "PHIODataNode.h"

#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;


MomentumscaleReco::MomentumscaleReco(float MSF)
{
  ScaleFactor = MSF;
  cout << "MomentumScaleReco::  Applying a momentum scale factor of " << ScaleFactor << endl;

  ThisName = "MOMENTUMSCALE";
  return ;
}


int MomentumscaleReco::process_event(PHCompositeNode *topNode)
{
  // Find the DC Node...
  DchTrack *d_dctrk = 0;
  PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
  DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
  if(DCTRACK) d_dctrk  = DCTRACK->getData ();
  if (!d_dctrk) 
    {
      cout << PHWHERE << "MomentumscaleReco:: dctrk not in Node Tree" << endl;
      return -1;
    }

  // Loop over all tracks and scale momentum...
  for (unsigned int itrk=0; itrk < d_dctrk->get_DchNTrack()  ; itrk++)
    {
      float mom = d_dctrk->get_momentum(itrk) * ScaleFactor;
      d_dctrk->set_momentum(itrk, mom);
    }

  return 0;
}

