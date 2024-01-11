#include <FillTrackPathLengths.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>
#include <PHTrackOut.h>

#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <half/half.h>
#include <useInt.h>

#include <set>
#include <sstream>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

FillTrackPathLengths::FillTrackPathLengths(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/filltrackpathlength.dump");
#endif

  return;
}

int
FillTrackPathLengths::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
#ifdef useIntflag
  VariableArrayInt *trkproj = new VariableArrayInt(10003);
#else
  VariableArray *trkproj = new VariableArray(10003);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(trkproj, "TrackPathLength_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  return EVENT_OK;
}

int
FillTrackPathLengths::process_event(PHCompositeNode *topNode)
{
  PHTrackOut *cglproj = findNode::getClass<PHTrackOut>(topNode, "PHTrackOut");

#ifdef useIntflag
  VariableArrayInt *trkproj = findNode::getClass<VariableArrayInt>(topNode, "TrackPathLength_VarArray");
  vector<int> savethis;
  map<int, int> map_one_path;
  map<int, int>::iterator iter;
#else
  VariableArray *trkproj = findNode::getClass<VariableArray>(topNode, "TrackPathLength_VarArray");
  vector<short int> savethis;
  map<int, short int> map_one_path;
  map<int, short int>::iterator iter;
#endif

  for (unsigned int i = 0; i < cglproj->get_PHNTrack(); i++)
    {
      map_one_path.clear();
      savethis.push_back(i);
      
#ifdef DUMP
      dumpfile << "Track: " << i << endl;
#endif
      
      if (cglproj->get_projectionTof(i, 0) > -800)
        {
	  map_one_path[id_detector::id_tofe] = FloatToInt(cglproj->get_tofPathLength(i));
        }
      if (cglproj->get_projectionTofw(i, 0) > -800)
        {
          map_one_path[id_detector::id_tofw] = FloatToInt(cglproj->get_tofwPathLength(i));
        }
      if (cglproj->get_projectionEmc(i, 0) > -800)
        {
          map_one_path[id_detector::id_emc] = FloatToInt(cglproj->get_emcPathLength(i));
        }
      
      savethis.push_back(map_one_path.size());
      for (iter = map_one_path.begin(); iter != map_one_path.end(); iter++)
        {
          savethis.push_back(iter->first);
          savethis.push_back(iter->second);
#ifdef DUMP
          dumpfile << "id: " << iter->first << ", pl: " <<  iter->second << endl;
#endif
        }
    }
  
  trkproj->set_val(savethis);

  return EVENT_OK;
}

int
FillTrackPathLengths::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag
int FillTrackPathLengths::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int FillTrackPathLengths::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
