#include <Fun4AllReturnCodes.h>
#include <mEmcMaskDeadTowers.h>
#include <PHCompositeNode.h>

#include <emcBadModules.h>
#include <emcTowerContainer.h>
#include <emcTowerContent.h>
#include <emcNodeHelper.h>
#include <EmcIndexer.h>
#include <recoConsts.h>
#include <getClass.h>

#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

//_____________________________________________________________________________
mEmcMaskDeadTowers::mEmcMaskDeadTowers(): SubsysReco("mEmcMaskDeadTowers")
{
  recoConsts *rc = recoConsts::instance();

  unsigned int err, warn;
  unsigned int towerid;

  fErrorRaw.resize(NCHANNELS, 0);
  fWarnRaw.resize(NCHANNELS, 0);

  fErrorRaw.resize(NCHANNELS, 0);
  fWarnRaw.resize(NCHANNELS, 0);

  fErrorMap.resize(fErrorRaw.size() , 0 );

  ifstream fin(rc->get_CharFlag("EMCDEADRECALDATASOURCE"));
  while (fin >> towerid >> err >> warn)
    {
      cout << towerid << " " << err << endl;
      fErrorRaw[towerid] = err;
      fWarnRaw[towerid] = warn;
    }
  fin.close();
}

//_____________________________________________________________________________
int
mEmcMaskDeadTowers::process_event(PHCompositeNode* topNode)
{
  emcTowerContainer* towers =
    findNode::getClass<emcTowerContainer>(topNode, "emcTowerContainer");
  
  if (!towers)
    return ABORTRUN;

  for ( size_t i = 0; i < towers->size(); ++i )
    {
      emcTowerContent* twr = towers->getTower(i);
      if (twr == 0)
	{
	  cout << PHWHERE << " SEVERE ERROR: Got NULL pointer for tower, contact the emc group with this"
	       << endl;
	  exit(1);
	}
      
      int towerid = twr->TowerID();
      unsigned int cdead = ErrorFast(towerid);
      if (cdead & 0x400)
	{
	  //	      twr->SetCalibrated(0.0,0.0);
	  //	      cout << towerid << "  " << cdead << endl;
	  twr->Zero();
	  twr->SetNeighbours(cdead,0);
	}
    }
  
  return EVENT_OK; // was missing return value
}

//_____________________________________________________________________________
unsigned int
mEmcMaskDeadTowers::ErrorFast(const int towerID) const
{
  return fErrorRaw[towerID];
}

//_____________________________________________________________________________
unsigned int
mEmcMaskDeadTowers::DeadmapFast(const int towerID) const
{
  return fErrorMap[towerID];
}

//_____________________________________________________________________________
unsigned int
mEmcMaskDeadTowers::WarningFast(const int towerID) const
{
  return fWarnRaw[towerID];
}
