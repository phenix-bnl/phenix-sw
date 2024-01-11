#include <FillCNT_TrackPathLengths.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <Fun4AllReturnCodes.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <TrackPathLengthMap.h>
#include <TrackPathLengthMapEntry.h>


#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <cstdlib>

using namespace std;

FillCNT_TrackPathLengths::FillCNT_TrackPathLengths(const std::string &name): SubsysReco(name)
{
  return;
}

int
FillCNT_TrackPathLengths::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_TrackPathLengths::process_event(PHCompositeNode *topNode)
{
  TrackPathLengthMap *trkpath = findNode::getClass<TrackPathLengthMap>(topNode, "TrackPathLength_comp");
  if (!trkpath)
    {
      return EVENT_OK;
    }

  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  for (unsigned int i = 0; i < cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      sngl->set_plemc(-1000000.);
      sngl->set_pltofe(-1000000.);
      sngl->set_pltofw(-1000000.);
    }
  for (unsigned int i = 0; i < cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      TrackPathLengthMapEntry *sngltrkpath = trkpath->GetTrack(i);
      if (!sngltrkpath)
        {
          cout << "could not find track " << i << " in path map" << endl;
          exit(1);
        }
      map<short int, float > *alldets = sngltrkpath->getMap();
      map<short int, float >::const_iterator iter;
      for (iter = (*alldets).begin(); iter != (*alldets).end(); iter++)
        {
          switch (iter->first)
            {
            case id_detector::id_tofe:
              sngl->set_pltofe(iter->second);
              break;
            case id_detector::id_tofw:
              sngl->set_pltofw(iter->second);
              break;
            case id_detector::id_emc:
              sngl->set_plemc(iter->second);
              break;
            }
        }

    }
  return EVENT_OK;
}

