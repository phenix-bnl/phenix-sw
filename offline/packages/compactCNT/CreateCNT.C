#include <CreateCNT.h>
#include <TrackProjectionMap.h>

#include <PHCentralTrack.h>
#include <PHCentralTrackv24.h>
#include <PHSnglCentralTrack.h>
#include <PHSnglCentralTrackv24.h>

#include <Fun4AllReturnCodes.h>

#include <PHAngle.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <cstdlib>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

CreateCNT::CreateCNT(const std::string &name): SubsysReco(name)
{
  phcnt = 0;
  return;
}

int CreateCNT::InitRun(PHCompositeNode *topNode)
{
  TrackProjectionMap *trkproj = findNode::getClass<TrackProjectionMap>(topNode, "TrackProjection_comp");
  if(!trkproj)
    {
      // No track projections were found, this is not a compactDST file, we do not need PHCentralTrack
      return EVENT_OK;
    }


  phcnt = findNode::getClass<PHCentralTrackv24>(topNode, "PHCentralTrack");

  if(!phcnt)
    {

      PHNodeIterator iter(topNode);
      PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
      
      phcnt = new PHCentralTrackv24();
      
      // Make nodes containing the objects...
      
      PHObjectNode_t *recal_cgl_node = new PHObjectNode_t(phcnt, "PHCentralTrack", "PHObject");
      
      // Put the nodes in the tree below "DST"...
      dstNode->addNode(recal_cgl_node);
    }
  else
    {
      cout << PHWHERE << "   PHCentralTrack already on node tree - good! " << endl;
    }

  return EVENT_OK;
}

int CreateCNT::process_event(PHCompositeNode *topNode)
{
  TrackProjectionMap *trkproj = findNode::getClass<TrackProjectionMap>(topNode, "TrackProjection_comp");
  if(trkproj)
    {
      PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
      if(!cnt)
	{
	  cout << PHWHERE << "Could not find PHCentralTrack node, quit!" << endl;
	  exit(1);
	}
      
      int NumTracks = trkproj->Entries();
      
      // This just guards against exceeding the initial TClonesArray size allocation
      phcnt->set_TClonesArraySize(NumTracks);
      
      for (unsigned int i=0; i< (unsigned int) NumTracks;i++)
	{
	  phcnt->AddPHParticle(i);
	}
      
      for (unsigned int i = 0; i < phcnt->get_npart(); i++)
	{
	  PHSnglCentralTrack *sngl = phcnt->get_track(i);
	  sngl->set_isPi(-9999.);
	  sngl->set_isK(-9999.);
	  sngl->set_isP(-9999.);
	  sngl->set_mcid(-9999);
	}
    }

  return EVENT_OK;
}

