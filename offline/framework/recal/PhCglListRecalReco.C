#include "PhCglListRecalReco.h"

#include <PhCglSngl.h>
#include <PhCglList.h>
#include <PhCglSnglv2.h>
#include <PhCglListv2.h>
#include <PhCglSnglv3.h>
#include <PhCglListv3.h>
#include <PhCglSnglv4_Run7a.h>
#include <PhCglListv4_Run7a.h>
#include <PhPhotonSngl.h>
#include <PhPhotonList.h>

#include <PHGlobal.h>
#include <RunHeader.h>
#include <PHCentralTrack.h>
#include <PHCentralTrackv21.h>
#include <PHCentralTrackv22.h>
#include <PHCentralTrackv23.h>


#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>
#include <recoConsts.h>

#include <iostream>
#include <cmath>

using namespace std;
using namespace findNode;

typedef PHIODataNode<PHObject> PHObjectNode_t;

PhCglListRecalReco::PhCglListRecalReco(const string &name):
  Recalibrator(name),
  recal_cgl(NULL),
  phcgllist_version(0),
  phcgllist_active(false)
{
  baseclasses.insert("PhCglListv4_Run7a");
  return ;
}

int
PhCglListRecalReco::Init(PHCompositeNode *topNode)
{

  PHCentralTrack *phcgllist_existing = findNode::getClass<PHCentralTrackv23>(topNode, "PHCentralTrack");

  if(!phcgllist_existing) // If it doesn't exist, create it and set this recalibrator to be active (in the CreateNodeTree).
    {
      CreateNodeTree(topNode);
    }
  return 0;
}


int
PhCglListRecalReco::isValidRun(const int runno) const
{
  // hard code
  // This is redundancy of InitRun, you have to change both
  if ( BEGIN_OF_RUN7 < runno  && BEGIN_OF_RUN8 > runno)
    {
      return 1;
    }
  return 0;
}


int
PhCglListRecalReco::process_event(PHCompositeNode *topNode)
{

  /////////////////////////////////
  //  Only work if it's active

  if(!phcgllist_active) return EVENT_OK;

  //////////////////////////////
  //  Combine work depends on the object type of input source

  int combine_result;

  combine_result = combine_v4_Run7a(topNode);
  return combine_result;
}

int
PhCglListRecalReco::combine_v4_Run7a(PHCompositeNode *topNode)
{
  // In this function we fix the Run7 production case: two nodes of CWG nanoDST into one node in memory

  PhCglListv4_Run7a *cglnode = findNode::getClass<PhCglListv4_Run7a>(topNode, "PhCglList");
  PhCglListv4_Run7a *hadroncglnode = findNode::getClass<PhCglListv4_Run7a>(topNode, "HadronPhCglList");

  if((!cglnode) && (!hadroncglnode))
    cglnode = findNode::getClass<PhCglListv4_Run7a>(topNode, "HardPhCglList");

  if((!cglnode) && (!hadroncglnode))
    {
      cout << "PhCglListRecalReco::process_event ERROR: not any known PhCglList node in original input!" << endl;
      return ABORTEVENT;
    }

  if(cglnode)
    {
      int npart = cglnode->get_npart();
      for (int itrk = 0; itrk < npart; itrk++)
	{

	  PHSnglCentralTrack *trk = cglnode->get_track(itrk);
	  recal_cgl -> AddPHParticle(*trk);
	}

    } // if cglnode exists


  if(hadroncglnode)
    {
      int npart = hadroncglnode->get_npart();
      for (int itrk = 0; itrk < npart; itrk++)
	{

	  PHSnglCentralTrack *trk = hadroncglnode->get_track(itrk);
	  recal_cgl -> AddPHParticle(*trk);
	}

    } // if hadroncglnode exists
  return EVENT_OK;
}

bool
PhCglListRecalReco::CreateNodeTree(PHCompositeNode *topNode)
{
  // Find the DST node so we can put objects there...
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  recal_cgl = new PHCentralTrackv23();

  // Make nodes containing the objects...

  PHObjectNode_t *recal_cgl_node = new PHObjectNode_t(recal_cgl, "PHCentralTrack", "PHObject");

  // Put the nodes in the tree below "DST"...
  dstNode->addNode(recal_cgl_node);

  phcgllist_active = true;

  return true;
}

