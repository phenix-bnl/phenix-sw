#include <RecoverTrackPathLengths.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <TrackPathLengthMapEntry.h>
#include <TrackPathLengthMap.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>


#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <useInt.h>

#include <cstdlib>
#include <sstream>
#include <fstream>

using namespace std;

RecoverTrackPathLengths::RecoverTrackPathLengths(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recovertrackpathlength.dump");
#endif

  return;
}

int
RecoverTrackPathLengths::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  ostringstream tmpstream;

#ifdef useIntflag
  VariableArrayInt *trkproj = findNode::getClass<VariableArrayInt>(topNode, "TrackPathLength_VarArray");
#else
  VariableArray *trkproj = findNode::getClass<VariableArray>(topNode, "TrackPathLength_VarArray");
#endif

  TrackPathLengthMap *trkmap;
  if (trkproj)
    {
      trkmap  = findNode::getClass<TrackPathLengthMap>(topNode, "TrackPathLength_comp");
      if (!trkmap)
        {
          trkmap = new TrackPathLengthMap();
          PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(trkmap, "TrackPathLength_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }
  return EVENT_OK;
}

int
RecoverTrackPathLengths::process_event(PHCompositeNode *topNode)
{

#ifdef useIntflag
  VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "TrackPathLength_VarArray");
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "TrackPathLength_VarArray");
#endif

  if (hitarray)
    {
      TrackPathLengthMap *trkmap  = findNode::getClass<TrackPathLengthMap>(topNode, "TrackPathLength_comp");
      if (!trkmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate " <<  "TrackPathLength_comp" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();
#ifdef useIntflag
      const int *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();
#endif

      while (size > 0)
        {
          TrackPathLengthMapEntry trkentry;
          short int id = *array++;
          size--;
          short int nnentry = *array++;
          size--;
          while (nnentry > 0)
            {
              short int id_det = *array++;
              size--;
              float val = useInt::GetFloat(*array++);
	      size--;
	      nnentry--;
	      trkentry.AddPathLength(id_det,val);
            }
	  trkmap->AddPathLength(id,trkentry);
        }
#ifdef DUMP
      trkmap->identify(dumprecover);
#endif
    }
  return EVENT_OK;
}

int
RecoverTrackPathLengths::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}
