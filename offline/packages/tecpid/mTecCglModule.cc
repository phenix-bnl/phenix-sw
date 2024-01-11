//#define DEBUG
#include "mTecCglModule.h"
#include "TecCluster.hh"
#include "TecClusterContainer.hh"
#include "TecProj.hh"
#include "TecProjv1.hh"
#include "TecGeometryObject.hh"
#include "mTecUtilities.h"
#include "DchTrack.h"
#include "CglTrack.h"
#include "PHTrackOut.h"
#include "getClass.h"

#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHGeometry.h"

#include <cstdlib>
#include <iostream>

using namespace PHGeometry;
using namespace TecUtilities;
using namespace std;

mTecCglModule::mTecCglModule()
{
  min_distance_cut = 4;
}

mTecCglModule::~mTecCglModule()
{
}

// MAIN
PHBoolean mTecCglModule::event(PHCompositeNode *topNode)
{
  //------ Get pointers to the Tables ------------------------------------

  DchTrack * dch = findNode::getClass<DchTrack>(topNode, "DchTrack");
  if (!dch)
    {
       cerr << PHWHERE << " ERROR: Can not find DchTrack." << endl;
       //       return False;      
    }

  CglTrack * cgl[2];
  cgl[0] = findNode::getClass<CglTrack>(topNode, "CglTrack");
  if (!cgl[0])
    {
       cerr << PHWHERE << " ERROR: Can not find CglTrack." << endl;
       return False;      
    }
  cgl[1] = findNode::getClass<CglTrack>(topNode, "CglTrackBack");
  if (!cgl[1])
    {
       cerr << PHWHERE << " ERROR: Can not find CglTrackBack." << endl;
       return False;      
    }

  TecClusterContainer* tec = findNode::getClass<TecClusterContainer>(topNode,"TecClusterContainer");
  if (!tec)
    {
      cerr << PHWHERE << " ERROR: Can not find TecClusterContainer." << endl;
       return False;      
    }

  TecGeometryObject * TGO = findNode::getClass<TecGeometryObject>(topNode, "TecGeometry");
  if (!TGO)
    {
       cerr << PHWHERE << " ERROR: Can not find TecGeometry." << endl;
       return False;      
    }

  TecCalibrationObject * TCO = findNode::getClass<TecCalibrationObject>(topNode, "TecCalibration");
  if (!TCO)
    {
       cerr << PHWHERE << " ERROR: Can not find TecCalibration." << endl;
       return False;      
    }

  TecProj* tecproj[2];
  tecproj[0] = findNode::getClass<TecProj>(topNode, "TecProj");
  if (!tecproj[0])
    {
      cerr << PHWHERE << " TecProj Object not found" << endl;
      return False;
    }
  tecproj[0]->Reset();

  tecproj[1] = findNode::getClass<TecProj>(topNode, "TecProjBack");
  if (!tecproj[1])
    {
      cerr << PHWHERE << " TecProjBack Object not found" << endl;
      return False;
    }
  tecproj[1]->Reset();

  unsigned int keepNTecProj[2];

  for (unsigned int icgl=0; icgl<2; icgl++)
    for (unsigned int iicgl=0; iicgl<cgl[0]->get_CglNTrack(); iicgl++)
      {
	keepNTecProj[icgl] = tecproj[icgl]->get_TecNProj();

	int itrk = cgl[0]->get_dctracksid(iicgl);
	if (itrk<0) continue;
	PHPoint dcpoint = dch->get_point(itrk);
	PHVector dcvect = dch->get_direction(itrk);
	PHLine dcline = PHLine(dcpoint, dcvect);

	bool is_swapped = false;
	if (icgl==1)
	  is_swapped = true;

	if (!Associate(iicgl, dcline, tec, TCO, TGO, tecproj[icgl], is_swapped, min_distance_cut))
	  continue;

	// set tectrackid in cgl if there is a match
	if( tecproj[icgl]->get_TecNProj() > keepNTecProj[icgl])
	  {
	    cgl[icgl]->set_tectrackid(iicgl,1);
	    keepNTecProj[icgl] = tecproj[icgl]->get_TecNProj();
	  }
      }

#ifdef DEBUG
  //////// DEBUG /////////////////
  cout << "Number of clusters = " << tec->getNClusters() << " "
       << "Number of associated clusters = " << tecproj[0]->get_TecNProj() << endl;
  for (unsigned int i=0; i<tecproj[0]->get_TecNProj(); i++)
    {
      for (int iplane=0; iplane<6; iplane++)
	{
	  int iclus = tecproj[0]->get_teclusterid(i,iplane);
	  if (iclus<0 || iclus>=tec->getNClusters())
	    continue;
	  TecCluster *tecclus = tec->getTecCluster(iclus);
	  if (tecclus)
	    cout << "(" << tecclus->get_xyz_global(TCO,TGO,0) << ", "
		 << tecclus->get_xyz_global(TCO,TGO,1) << ", "
		 << tecclus->get_xyz_global(TCO,TGO,2) << ")";
	}
      cout << endl;
    }
  ///////////////////////////////
#endif

  return true;
}

