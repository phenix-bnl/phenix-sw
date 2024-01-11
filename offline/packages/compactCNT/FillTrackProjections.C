#include "FillTrackProjections.h"
#include "id_detector.h"
#include "VariableArrayInt.h"
#include "setIntflag.h"

#include <vararray/VariableArray.h>
#include <PHTrackOut.h>
#include <PHTrackOutv6.h>

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

FillTrackProjections::FillTrackProjections(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillprojections.dump");
#endif

  return;
}

int
FillTrackProjections::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
#ifdef useIntflag
  VariableArrayInt *trkproj = new VariableArrayInt(10000);
#else
  VariableArray *trkproj = new VariableArray(10000);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(trkproj, "TrackProjection_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  return EVENT_OK;
}

int
FillTrackProjections::process_event(PHCompositeNode *topNode)
{
#ifdef DUMP
  string coo[3] = {",x: ", ",y: ",",z: "};
#endif

  PHTrackOut *cglproj = findNode::getClass<PHTrackOut>(topNode, "PHTrackOut");


#ifdef useIntflag
  VariableArrayInt *trkproj = findNode::getClass<VariableArrayInt>(topNode, "TrackProjection_VarArray");
  vector<int> savethis;
  map<int,int *> map_one_proj;
  map<int,int *>::iterator iter;
  int *one_proj = 0;
#else
  VariableArray *trkproj = findNode::getClass<VariableArray>(topNode, "TrackProjection_VarArray");
  vector<short int> savethis;
  map<int,short int *> map_one_proj;
  map<int,short int *>::iterator iter;
  short int *one_proj = 0;
#endif
  for (unsigned int i = 0; i < cglproj->get_PHNTrack(); i++)
    {
      map_one_proj.clear();
      savethis.push_back(i);

#ifdef DUMP
      dumpfile << "Track: " << i << endl;
#endif
      
      if (cglproj->get_projectionEmc(i, 0) > -800)
	{
#ifdef useIntflag
	  one_proj = new int[3];
#else
	  one_proj = new short int[3];
#endif
	  map_one_proj[id_detector::id_emc] = one_proj;
	  for (int j = 0; j < 3;j++)
	    {
	      *one_proj++ = FloatToInt(cglproj->get_projectionEmc(i, j));
	    }
	}
      if (cglproj->get_projectionPc1(i, 0) > -800)
	{
#ifdef useIntflag
	  one_proj = new int[3];
#else
	  one_proj = new short int[3];
#endif
	  map_one_proj[id_detector::id_pc1] = one_proj;
	  for (int j = 0; j < 3;j++)
	    {
	      *one_proj++ = FloatToInt(cglproj->get_projectionPc1(i, j));
	    }
	}
      if (cglproj->get_projectionPc2(i, 0) > -800)
	{
#ifdef useIntflag
	  one_proj = new int[3];
#else
	  one_proj = new short int[3];
#endif
	  map_one_proj[id_detector::id_pc2] = one_proj;
	  for (int j = 0; j < 3;j++)
	    {
	      *one_proj++ = FloatToInt(cglproj->get_projectionPc2(i, j));
	    }
	}
      if (cglproj->get_projectionPc3(i, 0) > -800)
	{
#ifdef useIntflag
	  one_proj = new int[3];
#else
	  one_proj = new short int[3];
#endif
	  map_one_proj[id_detector::id_pc3] = one_proj;
	  for (int j = 0; j < 3;j++)
	    {
	      *one_proj++ = FloatToInt(cglproj->get_projectionPc3(i, j));
	    }
	}
      
      if (cglproj->get_projectionTof(i, 0) > -800)
	{
#ifdef useIntflag
	  one_proj = new int[3];
#else
	  one_proj = new short int[3];
#endif
	  map_one_proj[id_detector::id_tofe] = one_proj;
	  for (int j = 0; j < 3;j++)
	    {
	      *one_proj++ = FloatToInt(cglproj->get_projectionTof(i, j));
	    }
	}
      if (cglproj->get_projectionTofw(i, 0) > -800)
	{
#ifdef useIntflag
	  one_proj = new int[3];
#else
	  one_proj = new short int[3];
#endif
	  map_one_proj[id_detector::id_tofw] = one_proj;
	  for (int j = 0; j < 3;j++)
	    {
	      *one_proj++ = FloatToInt(cglproj->get_projectionTofw(i, j));
	    }
	}

      //
      // Adding HBD projections
      //
      if (cglproj->get_projectionHbd(i, 0) > -800)
	{
#ifdef useIntflag
	  one_proj = new int[3];
#else
	  one_proj = new short int[3];
#endif
	  map_one_proj[id_detector::id_hbd] = one_proj;
	  for (int j = 0; j < 3;j++)
	    {
	      *one_proj++ = FloatToInt(cglproj->get_projectionHbd(i, j));
	    }
	}
      
      //
      // Adding SVX projections for 4 layers
      //

      // This should only be done if cglproj is a PHTrackOutv6 object, otherwise the call to get_projectionSvx() generates 
      // a virtual method warning every time! The dynamic cast returns a null pointer if it is not a v6 object. 
      PHTrackOutv6 *tmp = dynamic_cast <PHTrackOutv6 *> (cglproj);
      if(tmp)
	{
	  if (cglproj->get_projectionSvx(i, 0, 0) > -800)
	    {
#ifdef useIntflag
	      one_proj = new int[3];
#else
	      one_proj = new short int[3];
#endif
	      map_one_proj[id_detector::id_svx0] = one_proj;
	      for (int j = 0; j < 3;j++) { *one_proj++ = FloatToInt(cglproj->get_projectionSvx(i, 0, j)); }
	    }
	  if (cglproj->get_projectionSvx(i, 1, 0) > -800)
	    {
#ifdef useIntflag
	      one_proj = new int[3];
#else
	      one_proj = new short int[3];
#endif
	      map_one_proj[id_detector::id_svx1] = one_proj;
	      for (int j = 0; j < 3;j++) { *one_proj++ = FloatToInt(cglproj->get_projectionSvx(i, 1, j)); }
	    }
	  if (cglproj->get_projectionSvx(i, 2, 0) > -800)
	    {
#ifdef useIntflag
	      one_proj = new int[3];
#else
	      one_proj = new short int[3];
#endif
	      map_one_proj[id_detector::id_svx2] = one_proj;
	      for (int j = 0; j < 3;j++) { *one_proj++ = FloatToInt(cglproj->get_projectionSvx(i, 2, j)); }
	    }
	  if (cglproj->get_projectionSvx(i, 3, 0) > -800)
	    {
#ifdef useIntflag
	      one_proj = new int[3];
#else
	      one_proj = new short int[3];
#endif
	      map_one_proj[id_detector::id_svx3] = one_proj;
	      for (int j = 0; j < 3;j++) { *one_proj++ = FloatToInt(cglproj->get_projectionSvx(i, 3, j)); }
	    }
	}

      // The track projections to the RICH and TEC are not saved here because the track association does not use them.
      // They are saved in FillTrackLineProjections instead.

      savethis.push_back(map_one_proj.size());
      for (iter = map_one_proj.begin(); iter != map_one_proj.end(); iter++)
	{
	  savethis.push_back(iter->first);
#ifdef DUMP
          dumpfile << "id: " << iter->first << endl;
#endif
	  for (int j=0;j<3;j++)
	    {
	      savethis.push_back((iter->second)[j]);
#ifdef DUMP
	      dumpfile << coo[i] << (iter->second)[j];
#endif
	    }
#ifdef DUMP
	  dumpfile << endl;
#endif
	  
	  delete [] iter->second;
	}
    }

  trkproj->set_val(savethis);

  return EVENT_OK;
}

int
FillTrackProjections::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag
int
FillTrackProjections::FloatToInt(const float rval) const
{
   floatint fi;
   fi.f32 = rval;
   return fi.i32;
}
#else
short int
FillTrackProjections::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
