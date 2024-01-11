#include "emcPatch.h"
#include "emcNodeHelper.h"
#include "PHCompositeNode.h"
#include <iostream>
#include <cmath>
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "dEmcCalibTowerWrapper.h"
#include "emcRawDataAccessor.h"
#include "emcRawDataObject.h"
#include "emcCalibratedDataObject.h"
#include "EmcIndexer.h"
#include <cassert>
#include <Fun4AllReturnCodes.h>

using namespace std;

//_____________________________________________________________________________
emcPatch::emcPatch(): SubsysReco("emcPatch")
{

}

//_____________________________________________________________________________
emcPatch::~emcPatch()
{

}

//_____________________________________________________________________________
int
emcPatch::process_event(PHCompositeNode* topNode)
{
  emcNodeHelper nh;

  PHCompositeNode* emc2 = nh.findCompositeNode(topNode,"EMC2");

  if ( !emc2 ) 
    {
      cerr << "emcPatch::event : did not find EMC2 node !"
	   << endl;
      return ABORTRUN;
    }

  dEmcCalibTowerWrapper* calibTower = 
    nh.getTable<dEmcCalibTowerWrapper>("dEmcCalibTower",topNode);
  assert(calibTower!=0);

  emcTowerContainer* tc = 
    nh.getObject<emcTowerContainer>("emcTowerContainer",topNode);
  if (!tc)
    {
      cerr << "<E> Could not find emcTowerContainer node"
	   << endl;
      topNode->print();
      return ABORTRUN;
    }

  tc->Reset();

  emcRawDataAccessor* rda = emcRawDataAccessor::GetInstance();
  assert(rda!=0);
  emcRawDataObject* rdo = rda->GetRawDataObject();
  assert(rdo!=0);

  int out=0;

  for ( size_t i = 0; i < calibTower->RowCount(); ++i ) 
    {
      int softkey = calibTower->get_swkey(i);
      int towerID = EmcIndexer::TowerID(softkey);
      int index = rdo->GetIndexByTowerId(towerID);
      assert(index>=0);
      int amutac,amupre,amupost;
      float tac,hgpost,lgpost,hgpre,lgpre;
      int derr;
      rdo->Get(index,tac,hgpost,lgpost,hgpre,lgpre,amupre,amupost,amutac,derr);
      emcTowerContent* tower = tc->addTower(out);
      ++out;
      int fem,channel;
      EmcIndexer::PXPXSM144CH(towerID,fem,channel);
      tower->SetID(fem,channel);
      tower->SetRaw((int)hgpost,(int)hgpre,
		    (int)lgpost,(int)lgpre,
		    (int)tac,
		    amupre,amupost,amutac);
      tower->SetDataError(rdo->GetDataError(index));
      tower->SetADCTDC(static_cast<int>(rint(calibTower->get_adc(i))),
		       static_cast<int>(rint(calibTower->get_tac(i))));
      tower->SetCalibrated(calibTower->get_ecal(i),
			   calibTower->get_tof(i));
      tower->SetNeighbours(calibTower->get_deadmap(i),
			   calibTower->get_warnmap(i));
    }


  return EVENT_OK;
}
