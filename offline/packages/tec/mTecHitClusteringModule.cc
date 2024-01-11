#ifndef __MTECHITCLUSTERINGMODULE_CC__
#define __MTECHITCLUSTERINGMODULE_CC__
//#define DEBUG
#include <mTecHitClusteringModule.h>
#include <TecOutV1.hh>
#include <TecClusterV1.hh>
#include <TecClusterContainer.hh>
#include <TecClusterContainerV1.hh>
#include "Fun4AllReturnCodes.h"
#include "mTecUtilities.h"
#include "TecCalibrationObject.hh"

#include <PHNode.h>
#include <PHCompositeNode.h>
#include <PHObject.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <getClass.h>

#include <cmath>

using namespace std;
using namespace TecUtilities;

mTecHitClusteringModule::mTecHitClusteringModule()
{
  teccluster = new TecClusterV1();
  min_ntimebins = 4;
}

mTecHitClusteringModule::~mTecHitClusteringModule(){}

int
mTecHitClusteringModule::event(PHCompositeNode *topNode)
{
  // here I fill up the Hit output node TecHitOut.
  // That was done in mTecHoughTransform to preserve only hits belonging to tracks
  // since now we don't run mTecHoughTransform in production the TecHit Output is filled in here 
    copyTecHitOut(topNode, 0);

  bool hit_calibrated = true;
  TecCalibrationObject* TCO = NULL;
  int hotdeadlist[1000];
  TecOut *tecout;
 // version with calibrated charge
  tecout = findNode::getClass<TecOut>(topNode, "TecOutV1");
  int nhotdead  = 0;
  if (!tecout)
    {
      // version not calibrated
      hit_calibrated = false;
      tecout = findNode::getClass<TecOut>(topNode, "TecHitOut");
      TCO = findNode::getClass<TecCalibrationObject>(topNode, "TecCalibration");
      if (TCO)
	nhotdead = TCO->getHotDeadList(hotdeadlist);
    }
  if (!tecout)
    {
      cout << PHWHERE << "TecHitOut Node missing, aborting" << endl;
      return ABORTEVENT;
    }

  float timetot[48][600];
  float chargetot[48][600];
  int  nhittot[48][600];

  for (int i=0; i<48; i++)
    for (int j=0; j<600; j++)
      {
	timetot[i][j] = 0.0;
	chargetot[i][j] = 0.0;
	nhittot[i][j] = 0;
      }
  
  for(int i1=0; i1<tecout->getNHits(); i1++)
    {
      int index = tecout->getHitIndex(i1); 
      int wire = tecout->getHitWire(i1); 
      float charge = tecout->getHitCharge(i1); 
      if (!hit_calibrated)
	{
	  charge = Ampl2Charge(tecout->getHitADC(i1));
	  if (TCO)
	    charge *= TCO->getAbsoluteGain(index)*TCO->getRelativeGain(index,wire);
	}
      int glindex = index*1000+wire;
      for(int j2=0; j2<nhotdead; j2++)
	if(hotdeadlist[j2]==glindex)
	  {
	    charge=0.;
	    break;
	  }
      if (charge==0) continue;
      int timebin = tecout->getHitTimeBin(i1);
      timetot[index][wire] += (float)timebin;
      chargetot[index][wire] += charge;
      nhittot[index][wire] ++;
    }

  TecClusterContainerV1 *clustercontainer = findNode::getClass<TecClusterContainerV1>(topNode,"TecClusterContainer");
  if (!clustercontainer)
    {
      cout << PHWHERE << "TecClusterContainer Node missing, aborting" << endl;
      return ABORTEVENT;      
    }
  
  for (short index=0; index<48; index++)
    for (short wire=0; wire<600; wire++)
      {
	if (nhittot[index][wire] < min_ntimebins) continue;
	//	cout << index << " " << wire << " ";
	teccluster->set_index(index);
	teccluster->set_wire(wire);
	teccluster->set_ntimebins((short)nhittot[index][wire]);
	teccluster->set_avgtime((short)(timetot[index][wire]/nhittot[index][wire]));
	teccluster->set_charge(chargetot[index][wire]);
	clustercontainer->AddTecCluster(*teccluster);
      }

  /*  cout << "Number of Clusters=" << clustercontainer->getNClusters() << endl;
  for (int i=0; i<clustercontainer->getNClusters() && i<10; i++)
    {
      cout << clustercontainer->getTecCluster(i)->get_index() << " "
	   << clustercontainer->getTecCluster(i)->get_wire() << " "
	   << clustercontainer->getTecCluster(i)->get_ntimebins() << " "
	   << clustercontainer->getTecCluster(i)->get_avgtime() << " "
	   << clustercontainer->getTecCluster(i)->get_charge() << endl;
    }
  */
  return EVENT_OK;
}

#endif /* __MTECHITCLUSTERINGMODULE_CC__ */
