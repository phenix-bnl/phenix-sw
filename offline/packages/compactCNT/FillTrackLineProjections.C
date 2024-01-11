#include <FillTrackLineProjections.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>
#include <PHTrackOut.h>
#include <CglTrack.h>
#include <CrkProjv1.h>
#include <TecProjv1.hh>

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

FillTrackLineProjections::FillTrackLineProjections(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/filllineprojections.dump");
#endif

  return;
}

int
FillTrackLineProjections::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
#ifdef useIntflag_lineproj
  VariableArrayInt *trkproj = new VariableArrayInt(10000);
#else
  VariableArray *trkproj = new VariableArray(10000);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(trkproj, "TrackLineProjection_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  return EVENT_OK;
}

int
FillTrackLineProjections::process_event(PHCompositeNode *topNode)
{
  // This code stores the line projections needed for reassociation of tracks with the RICH and the TEC

#ifdef DUMP
  string coo[6] = {",startx: ", ",starty: ",",startz: ", ",endx: ", ",endy: ",",endz: "};
#endif

  PHTrackOut *cglproj = findNode::getClass<PHTrackOut>(topNode, "PHTrackOut");
  CglTrack *cgl = findNode::getClass<CglTrack>(topNode, "CglTrack");
  CglTrack *cglBG = findNode::getClass<CglTrack>(topNode, "CglTrackBack");

  CrkProj *crkproj = findNode::getClass<CrkProj>(topNode, "CrkProj");
  CrkProj *crkprojBG = findNode::getClass<CrkProj>(topNode, "CrkProjBG");

  // The TEC might not be there! If so, TecProj will be missing

  bool TecOn = True;
  TecProj *tecproj = 0;
  TecProj *tecprojBG = 0;

  tecproj = findNode::getClass<TecProj>(topNode, "TecProj");
  tecprojBG = findNode::getClass<TecProj>(topNode, "TecProjBack");
  if (!tecproj || !tecprojBG)
    {
      TecOn = False;
    }


#ifdef DUMP
  dumpfile << "    PHTrackOut has " << cglproj->get_PHNTrack() << " tracks " << endl;
  dumpfile << "    CrkProj has " << crkproj->get_CrkNProj() << " projections " << endl;
  dumpfile << "    CrkProjBack has " << crkprojBG->get_CrkNProj() << " projections " << endl;
  if(TecOn)
    {
      dumpfile << "    TecProj has " << tecproj->get_TecNProj() << " projections " << endl;
      dumpfile << "    TecProjBack has " << tecprojBG->get_TecNProj() << " projections " << endl;
    }
#endif

#ifdef useIntflag_lineproj
  VariableArrayInt *trkproj = findNode::getClass<VariableArrayInt>(topNode, "TrackLineProjection_VarArray");
  vector<int> savethis;
  map<int,int *> map_one_proj;
  map<int,int *>::iterator iter;
  int *one_proj = 0;
#else
  VariableArray *trkproj = findNode::getClass<VariableArray>(topNode, "TrackLineProjection_VarArray");
  vector<short int> savethis;
  map<int,short int *> map_one_proj;
  map<int,short int *>::iterator iter;
  short int *one_proj = 0;
#endif

  // Loop over all tracks. We want a projection entry for every track, even if it is empty

  for (unsigned int i = 0; i < cglproj->get_PHNTrack(); i++)
    {
      bool crk_stored = false;

      map_one_proj.clear();
      savethis.push_back(i);   // There is an entry for every track. Size is zero if there are no projections

      // First the RICH associated track line projections

      if (cgl->get_richringid(i) >= 0 || cglBG->get_richringid(i) >= 0)
	{
	  // There is a ring associated with this track - find it:

	  for(unsigned int j = 0; j < crkproj->get_CrkNProj(); j++)
	    {
	      if (crkproj->get_cgltrackid(j) == (int) i)
		{
		  // This track is associated with a RICH ring, we want to save it's projection
#ifdef useIntflag_lineproj
		  one_proj = new int[6];
#else
		  one_proj = new short int[6];
#endif
		  map_one_proj[id_detector::id_crk] = one_proj;
#ifdef DUMP
		  dumpfile << "detector id: " << id_detector::id_crk << endl;
#endif
		  
		  *one_proj++ = FloatToInt(crkproj->get_pstartx(j));
		  *one_proj++ = FloatToInt(crkproj->get_pstarty(j));
		  *one_proj++ = FloatToInt(crkproj->get_pstartz(j));
		  *one_proj++ = FloatToInt(crkproj->get_pendx(j));
		  *one_proj++ = FloatToInt(crkproj->get_pendy(j));
		  *one_proj++ = FloatToInt(crkproj->get_pendz(j));
		  
#ifdef DUMP
		  dumpfile << "Straight CRK track:" << endl;
		  dumpfile << coo[0] << crkproj->get_pstartx(j);
		  dumpfile << coo[1] << crkproj->get_pstarty(j);
		  dumpfile << coo[2] << crkproj->get_pstartz(j);
		  dumpfile << coo[3] << crkproj->get_pendx(j);
		  dumpfile << coo[4] << crkproj->get_pendy(j);
		  dumpfile << coo[5] << crkproj->get_pendz(j);
		  dumpfile << endl;
#endif

		  // We have stored the projection for this track now for the
		  // RICH. Therefore we do not need to check to see if the swapped
		  // track is associated with the RICH.

		  crk_stored = true;
		}

	    }

	  if(!crk_stored)
	    {
	      for(unsigned int j = 0; j < crkprojBG->get_CrkNProj(); j++)
		{
		  if( crkprojBG->get_cgltrackid(j) == (int) i)
		    {
		      // This swapped track is associated with a RICH ring, we want to save it's projection
#ifdef useIntflag_lineproj
		      one_proj = new int[6];
#else
		      one_proj = new short int[6];
#endif
		      map_one_proj[id_detector::id_crk] = one_proj;
#ifdef DUMP
		      dumpfile << "detector id: " << id_detector::id_crk << endl;
#endif		  
		      *one_proj++ = FloatToInt(crkprojBG->get_pstartx(j));
		      *one_proj++ = FloatToInt(crkprojBG->get_pstarty(j));
		      *one_proj++ = FloatToInt(crkprojBG->get_pstartz(j));
		      *one_proj++ = FloatToInt(crkprojBG->get_pendx(j));
		      *one_proj++ = FloatToInt(crkprojBG->get_pendy(j));
		      *one_proj++ = FloatToInt(crkprojBG->get_pendz(j));
		      
#ifdef DUMP
		      dumpfile << "Swapped CRK track:" << endl;
		      dumpfile << coo[0] << crkprojBG->get_pstartx(j);
		      dumpfile << coo[1] << crkprojBG->get_pstarty(j);
		      dumpfile << coo[2] << crkprojBG->get_pstartz(j);
		      dumpfile << coo[3] << crkprojBG->get_pendx(j);
		      dumpfile << coo[4] << crkprojBG->get_pendy(j);
		      dumpfile << coo[5] << crkprojBG->get_pendz(j);
		      dumpfile << endl;
#endif
		    }
		}
	    }
	}

      // Only if the TEC is on

      if(TecOn)
	{      
	  // Now the TEC associated track line projections

	  bool tec_stored = false;
	  
	  for(unsigned int j = 0; j < tecproj->get_TecNProj(); j++)
	    {
	      // We store a track line projection only once, even it is associated as both straight and swapped  
	      
	      if (tecproj->get_cgltrackid(j) == (int) i )
		{
		  // This track is associated with a TEC hit, store the projection 
#ifdef useIntflag_lineproj
		  one_proj = new int[6];
#else
		  one_proj = new short int[6];
#endif
		  map_one_proj[id_detector::id_tec] = one_proj;
#ifdef DUMP
		  dumpfile << "detector id: " << id_detector::id_tec << endl;
#endif
		  *one_proj++ = FloatToInt(tecproj->get_pstartx(j));
		  *one_proj++ = FloatToInt(tecproj->get_pstarty(j));
		  *one_proj++ = FloatToInt(tecproj->get_pstartz(j));
		  *one_proj++ = FloatToInt(tecproj->get_pendx(j));
		  *one_proj++ = FloatToInt(tecproj->get_pendy(j));
		  *one_proj++ = FloatToInt(tecproj->get_pendz(j));
		  
#ifdef DUMP
		  dumpfile << "Straight TEC track:" << endl;	      
		  dumpfile << coo[0] << tecproj->get_pstartx(j);
		  dumpfile << coo[1] << tecproj->get_pstarty(j);
		  dumpfile << coo[2] << tecproj->get_pstartz(j);
		  dumpfile << coo[3] << tecproj->get_pendx(j);
		  dumpfile << coo[4] << tecproj->get_pendy(j);
		  dumpfile << coo[5] << tecproj->get_pendz(j);
		  dumpfile << endl;
#endif
		  
		  // We have stored the projection for this track now for the
		  // TEC. Therefore we do not need to check to see if the swapped
		  // track is associated with the TEC.
		  
		  tec_stored = true;
		}
	    }

	  if(!tec_stored)
	    {
	      for(unsigned int j = 0; j < tecprojBG->get_TecNProj(); j++)
		{
		  if(tecprojBG->get_cgltrackid(j) == (int) i)
		    {
		      // This swapped track is associated with a TEC hit, store the projection 
#ifdef useIntflag_lineproj
		      one_proj = new int[6];
#else
		      one_proj = new short int[6];
#endif
		      map_one_proj[id_detector::id_tec] = one_proj;
#ifdef DUMP
		      dumpfile << "detector id: " << id_detector::id_tec << endl;
#endif
		      *one_proj++ = FloatToInt(tecprojBG->get_pstartx(j));
		      *one_proj++ = FloatToInt(tecprojBG->get_pstarty(j));
		      *one_proj++ = FloatToInt(tecprojBG->get_pstartz(j));
		      *one_proj++ = FloatToInt(tecprojBG->get_pendx(j));
		      *one_proj++ = FloatToInt(tecprojBG->get_pendy(j));
		      *one_proj++ = FloatToInt(tecprojBG->get_pendz(j));
		      
#ifdef DUMP
		      dumpfile << "Swapped TEC track:" << endl;	      
		      dumpfile << coo[0] << tecprojBG->get_pstartx(j);
		      dumpfile << coo[1] << tecprojBG->get_pstarty(j);
		      dumpfile << coo[2] << tecprojBG->get_pstartz(j);
		      dumpfile << coo[3] << tecprojBG->get_pendx(j);
		      dumpfile << coo[4] << tecprojBG->get_pendy(j);
		      dumpfile << coo[5] << tecprojBG->get_pendz(j);
		      dumpfile << endl;
#endif
		    }
		}
	    }
	}

      savethis.push_back(map_one_proj.size());
      for (iter = map_one_proj.begin(); iter != map_one_proj.end(); iter++)
	{
	  savethis.push_back(iter->first);
	  for (int i=0;i<6;i++)
	    {
	      savethis.push_back((iter->second)[i]);
	    }
	  delete [] iter->second;
	}
    }
  
  trkproj->set_val(savethis);
  
  return EVENT_OK;
}

int
FillTrackLineProjections::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag_lineproj
int
FillTrackLineProjections::FloatToInt(const float rval) const
{
   floatint fi;
   fi.f32 = rval;
   return fi.i32;
}
#else
short int
FillTrackLineProjections::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
