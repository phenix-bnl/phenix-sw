#include <RecoverTrackProjections.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <TrackProjectionMapEntry.h>
#include <TrackProjectionMap.h>
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

RecoverTrackProjections::RecoverTrackProjections(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recoverprojections.dump");
#endif

  return;
}

int
RecoverTrackProjections::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  ostringstream tmpstream;
#ifdef useIntflag
  VariableArrayInt *trkproj = findNode::getClass<VariableArrayInt>(topNode, "TrackProjection_VarArray");
#else
  VariableArray *trkproj = findNode::getClass<VariableArray>(topNode, "TrackProjection_VarArray");
#endif

  TrackProjectionMap *trkmap;
  if (trkproj)
    {
      trkmap  = findNode::getClass<TrackProjectionMap>(topNode, "TrackProjection_comp");
      if (!trkmap)
        {
          trkmap = new TrackProjectionMap();
          PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(trkmap, "TrackProjection_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }
  return EVENT_OK;
}

int
RecoverTrackProjections::process_event(PHCompositeNode *topNode)
{
#ifdef useIntflag
  VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "TrackProjection_VarArray");
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "TrackProjection_VarArray");
#endif

  if (hitarray)
    {
      TrackProjectionMap *trkmap  = findNode::getClass<TrackProjectionMap>(topNode, "TrackProjection_comp");
      if (!trkmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate " <<  "TrackProjection_comp" << endl;
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
          TrackProjectionMapEntry trkentry;
          short int id = *array++;    // track id
          size--;
          short int nnentry = *array++;
          size--;
          while (nnentry > 0)
            {
              short int id_det = *array++;   // detector id
              size--;
	      float val[3];
              for (int k = 0; k < 3;k++)
                {
                  val[k] = useInt::GetFloat(*array++);
                  size--;
                }
              nnentry--;
              trkentry.AddProjection(id_det, val);
            }
	  
          trkmap->AddProjection(id, trkentry);
        }
#ifdef DUMP
      trkmap->identify(dumprecover);
#endif
    }
  return EVENT_OK;
}

int
RecoverTrackProjections::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}

