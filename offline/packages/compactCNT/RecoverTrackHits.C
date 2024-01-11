#include <RecoverTrackHits.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <TrackHitsMapEntry.h>
#include <TrackHitsMap.h>
#include <vararray/VariableArray.h>


#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <cstdlib>
#include <fstream>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};


RecoverTrackHits::RecoverTrackHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover[0].open("/phenix/scratch/frawley/recovertrackhits.dump");
  dumprecover[1].open("/phenix/scratch/frawley/recovertrackbackhits.dump");
#endif

  inputnodename[0] = "CglTrackHits_VarArray";
  inputnodename[1] = "CglTrackBackHits_VarArray";
  outputnodename[0] = "CglTrackHits_comp";
  outputnodename[1] = "CglTrackBackHits_comp";

  return;
}

int
RecoverTrackHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  VariableArray *trkproj;
  TrackHitsMap *trkmap;
  for (int k = 0; k < 2;k++)
    {
      trkproj = findNode::getClass<VariableArray>(topNode, inputnodename[k]);
      if (trkproj)
        {
          trkmap  = findNode::getClass<TrackHitsMap>(topNode, outputnodename[k]);
          if (!trkmap)
            {
              trkmap = new TrackHitsMap();
              PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(trkmap, outputnodename[k].c_str() , "PHObject");
              dstNode->addNode(PHObjectNode);
            }
        }
    }
  return EVENT_OK;
}

int
RecoverTrackHits::process_event(PHCompositeNode *topNode)
{
  for (int k = 0; k < 2;k++)
    {

      VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, inputnodename[k]);
      if (hitarray)
        {
          TrackHitsMap *trkmap  = findNode::getClass<TrackHitsMap>(topNode, outputnodename[k]);
          if (!trkmap)
            {
              cout << PHWHERE << "Fatal: Cannot locate " <<  outputnodename[k] << endl;
              exit(1);
            }
          unsigned int size = hitarray->get_array_size();
          const short int *array = hitarray->get_array();
          while (size > 0)
            {
              TrackHitsMapEntry trkentry;
              short int id = *array++;
              size--;
              short int nnentry = *array++;
              size--;
              while (nnentry > 0)
                {
                  short int id_det = *array++;
                  size--;
                  short int val = *array++;
                  size--;
                  nnentry--;
                  trkentry.AddHit(id_det, val);
                }
              trkmap->AddHits(id, trkentry);
            }
#ifdef DUMP
          trkmap->identify(dumprecover[k]);
#endif
        }
    }

  return EVENT_OK;
}

int
RecoverTrackHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  for (int k = 0;k < 2;k++)
    {
      dumprecover[k].close();
    }
#endif

  return 0;
}
