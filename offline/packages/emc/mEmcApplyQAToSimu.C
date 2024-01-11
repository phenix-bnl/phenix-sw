#include "mEmcApplyQAToSimu.h"

#include "emcBadModules.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "emcNodeHelper.h"
#include "EmcIndexer.h"
#include <Fun4AllReturnCodes.h>

#include <iostream>
#include <cassert>

using namespace std;

//_____________________________________________________________________________
mEmcApplyQAToSimu::mEmcApplyQAToSimu(): SubsysReco("mEmcApplyQAToSimu")
{
}

//_____________________________________________________________________________
mEmcApplyQAToSimu::~mEmcApplyQAToSimu()
{
}

//_____________________________________________________________________________
int
mEmcApplyQAToSimu::process_event(PHCompositeNode* topNode)
{
  PHCompositeNode* emcNode = emcNodeHelper::findCompositeNode(topNode,"EMC");

  if ( !emcNode ) 
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " Cannot get node EMC !"
	   << endl;
      return ABORTRUN;
    }

  emcBadModules* bad = 
    emcNodeHelper::getObject<emcBadModules>("emcBadModules",emcNode);

  if ( !bad ) 
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " Cannot get emcBadModules object !"
	   << endl;
      return ABORTRUN;
    }

  PHCompositeNode* dstNode = emcNodeHelper::findCompositeNode(topNode,"DST");

  if ( !dstNode ) 
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " Cannot get node DST !"
	   << endl;
      return ABORTRUN;
    }

  emcTowerContainer* towers = 
    emcNodeHelper::getObject<emcTowerContainer>("emcTowerContainer",dstNode);

  if ( !towers ) 
   {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " Cannot get emcTowerContainer object !"
	   << endl;
      return ABORTRUN;
    }

  size_t dead_count = 0;

  for ( size_t j = 0; j < towers->size(); ++j )
    {
      emcTowerContent* t_j = towers->getTower(j);
      assert(t_j!=0);

      // FIXME : test that t_j isSimulated == true.

      unsigned int dead = bad->Deadmap(t_j->TowerID());

      if (dead & 0x400) 
	{
	  // set ecal and tof to zero 
          // In the case of real data (in MDO -> RDO):
          // a) in compressed mode: the tower is simply dropped 
          // b) in non-compressed mode: ADC and TDC are set to zero
	  // We adopt b) here.  When merging with a real event,
	  // the tower will be there (i.e. observable by the evaluator) but empty
	  t_j->SetCalibrated(0.0,0.0);
          dead_count++;
        }
      t_j->SetNeighbours(dead,0);
    }
  
  if (dead_count) 
    {
      cout << " Total num. of EMCal dead channels: " << dead_count << endl ;
    }
  return EVENT_OK;
}
