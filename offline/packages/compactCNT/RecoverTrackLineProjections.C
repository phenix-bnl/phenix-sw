#include "RecoverTrackLineProjections.h"
#include "id_detector.h"
#include "useInt.h"
#include "VariableArrayInt.h"
#include "setIntflag.h"

#include "TrackLineProjectionMapEntry.h"
#include "TrackLineProjectionMap.h"
#include <vararray/VariableArray.h>


#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>


#include <cstdlib>
#include <sstream>
#include <fstream>

using namespace std;

RecoverTrackLineProjections::RecoverTrackLineProjections(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recoverlineprojections.dump");
#endif

  return;
}

int
RecoverTrackLineProjections::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  ostringstream tmpstream;
#ifdef useIntflag_lineproj
  VariableArrayInt *trkproj;
  trkproj = findNode::getClass<VariableArrayInt>(topNode, "TrackLineProjection_VarArray");
#else
  VariableArray *trkproj;
  trkproj = findNode::getClass<VariableArray>(topNode, "TrackLineProjection_VarArray");
  gaga=0;
#endif
  TrackLineProjectionMap *trkmap;
  if (trkproj)
    {
      trkmap  = findNode::getClass<TrackLineProjectionMap>(topNode, "TrackLineProjection_comp");
      if (!trkmap)
        {
          trkmap = new TrackLineProjectionMap();
          PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(trkmap, "TrackLineProjection_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }

  return EVENT_OK;
}

int
RecoverTrackLineProjections::process_event(PHCompositeNode *topNode)
{
#ifdef useIntflag_lineproj
  VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "TrackLineProjection_VarArray");
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "TrackLineProjection_VarArray");
#endif
  if (hitarray)
    {
      TrackLineProjectionMap *trkmap  = findNode::getClass<TrackLineProjectionMap>(topNode, "TrackLineProjection_comp");
      if (!trkmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate " <<  "TrackLineProjection_comp" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();
#ifdef useIntflag_lineproj
      const int *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();
#endif

      while (size > 0)
        {
          TrackLineProjectionMapEntry trkentry;
          short int id = *array++;                      // track id 
          size--;
          short int nnentry = *array++;
          size--;

          while (nnentry > 0)
            {
              short int id_det = *array++;              // detector id
              size--;
              vector<float> val;
              val.resize(6);
              for (int k = 0; k < 6;k++)
                {
                  val[k] = useInt::GetFloat(*array++);          // the six positions defining the line
                  size--;
                }
              nnentry--;
              trkentry.AddProjection(id_det, val);      // creates entry for straight or swapped track
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
RecoverTrackLineProjections::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}

