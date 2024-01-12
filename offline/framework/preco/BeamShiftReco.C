#include "PHCompositeNode.h"
#include "BeamShiftReco.h"

#include "phool.h"

#include "PHObject.h"
#include "DchTrack.h"
#include "PHIODataNode.h"

#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;


BeamShiftReco::BeamShiftReco(float dx, float dy)
{
  XOffset = dx;
  YOffset = dy;
  cout << "BeamShiftReco::  Applying a shift in beam position by dx = " << XOffset
       << " and dy = " << YOffset << endl;

  ThisName = "BeamShiftReco";
  return ;
}


int BeamShiftReco::process_event(PHCompositeNode *topNode)
{
  // Find the DC Node...
  DchTrack *d_dctrk = 0;
  PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
  DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
  if(DCTRACK) d_dctrk  = DCTRACK->getData ();
  if (!d_dctrk) 
    {
      cout << PHWHERE << "BeamShiftReco:: dctrk not in Node Tree" << endl;
      return -1;
    }

  // Loop over all tracks and change momentum, alpha, phi0 ...
  for (unsigned int itrk=0; itrk < d_dctrk->get_DchNTrack()  ; itrk++)
    {
      float alpha = d_dctrk->get_alpha(itrk);
      float mom = d_dctrk->get_momentum(itrk);
      float pt = mom;
      float the0 = d_dctrk->get_theta0(itrk);
      if (the0 > -999)
	{
	  pt = mom * sin(the0);
	}
      float phi = d_dctrk->get_phi(itrk);
      float phi0 = d_dctrk->get_phi0(itrk);

      float newalpha = new_alpha(alpha,phi);
      float del_alpha = newalpha - alpha;
      float newpt = (newalpha==0.0) ? -999. : pt*fabs(alpha/newalpha);
      float newphi0 = delta_phi0(del_alpha) + phi0;
      if (alpha > -999)
	  d_dctrk->set_alpha(itrk,newalpha);
      if (phi0 > -999)
	d_dctrk->set_phi0(itrk, newphi0);
      if (the0 > -999)
	{
	  mom = newpt / sin(the0);
	  d_dctrk->set_momentum(itrk, mom);
	}
    }

  return 0;
}

float
BeamShiftReco::new_alpha(float alpha, float phi)
{

  float xp = sin(phi);
  float yp = cos(phi);

  float AlphaOffset = (XOffset * xp / 220. + YOffset * yp / 220.);

  return (alpha - AlphaOffset);

}

float
BeamShiftReco::delta_phi0(float del_alpha)
{

  return 2.0195*del_alpha;

}

