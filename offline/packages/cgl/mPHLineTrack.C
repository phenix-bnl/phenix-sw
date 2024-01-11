// Written by Jane Burward-Hoy
// Module that calls PHLineTrack for each track entry in dDchTracks
// What this module does in mPHLineTrackModel::event(PHCompositeNode *)
//   o searches for dDchTracks and cglDetectorGeo nodes in node tree
//   o constructs a list of PHLineTrack for each dDchTracks entry
//     and cglDetectorGeo pointer
//   o projects the track to the detectors and vertex
//   o writes out PHLineTrack list to a temporary node in the node tree.

#include "PHLineTrack.hh"
#include "mPHLineTrack.hh"
#include "cglDetectorGeo.hh"

#include <DchAnaPar.h>
#include <dDchTracksWrapper.h>
#include <DchTrack.h>


#include <getClass.h>

#include <iostream>
#include <limits>

using namespace std;

mPHLineTrack::mPHLineTrack()
{
  tracks = 0;
}

PHBoolean 
mPHLineTrack::event(PHCompositeNode *topNode)
{
  PHNodeIterator nodeIter(topNode);
  PHcglDetGeo = findNode::getClass<cglDetectorGeo>(topNode,"cglDetectorGeo");
  if (!PHcglDetGeo)
    {
      PHMessage("mPHLineTrack::event", PHWarning, "Did not find cglDetectorGeo in node tree: model cannot project");
      return 1;
    }

  trkWrapper = findNode::getClass<dDchTracksWrapper>(topNode,"dDchTracks");
  if (!trkWrapper)
    {
      PHMessage("mPHLineTrack::event", PHError, "Did not find dDchTracksWrapper in node tree: model cannot proceed");
      return 1;
    }

  PHCompositeNode *trackNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "PHTRACK"));
  if (!trackNode)
    {
      trackNode = new PHCompositeNode("PHTRACK");
      topNode->addNode(trackNode);
    }
  PHDataNode<PHPointerList<PHLineTrack> >* trackListNode = (PHDataNode<PHPointerList<PHLineTrack> >* )nodeIter.findFirst("PHDataNode", "PHLineTrackList");
  if (!trackListNode)
    {
      tracks = new PHPointerList<PHLineTrack>(trkWrapper->MaxRowCount());
      trackListNode = new PHDataNode<PHPointerList<PHLineTrack> >(tracks, "PHLineTrackList");
      trackNode->addNode(trackListNode);
    }
  else
    {
      tracks = trackListNode->getData();
    }
  tracks->clearAndDestroy();
  callLineTrack(topNode);

  return 1;
}

void
mPHLineTrack::callLineTrack(PHCompositeNode *topNode)
{
  PHLineTrack *dchtrack;
  DchTrack *dchtrk = findNode::getClass<DchTrack>(topNode,"DchTrack");
  if (!dchtrk)
    {
      cerr << PHWHERE << " Could not find DchTrack Node" << endl;
    }
  if (trkWrapper)
    {
      for (unsigned long i = 0; i < trkWrapper->RowCount() ; i++)
        {
          trkWrapper->set_phi0(i, trkWrapper->get_phi(i));
          if (trkWrapper->get_zed(i) > -9999)
            {
              trkWrapper->set_theta0(i, trkWrapper->get_beta(i));
            }
          else
            {
              trkWrapper->set_theta0(i, -std::numeric_limits<double>::infinity());
            }
          trkWrapper->set_momentum(i, -std::numeric_limits<double>::infinity());
	  if (dchtrk)
	    {
	      dchtrk->set_phi0(i,trkWrapper->get_phi0(i));
	      dchtrk->set_theta0(i,trkWrapper->get_theta0(i));
	      dchtrk->set_momentum(i,trkWrapper->get_momentum(i));
	    }
          dchtrack = new PHLineTrack(i, trkWrapper, *PHcglDetGeo);
          dchtrack->callProjections();
          if (!tracks->append(dchtrack))
            {
              PHMessage("mPHLineTrack::fillTrackTableList",
                        PHError, "cannot append PHLineTrack to tracks list!");
            }
        }
    }
  else
    {
      PHMessage("mPHLineTrack::fillTrackTableList", PHError, "dDchTracksWrapper node is null!");
    }
}

