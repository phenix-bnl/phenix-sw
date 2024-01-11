#include <Fun4AllReturnCodes.h>
#include "mEmcCalibratorModulev2.h"

#include "emcCalibrationDataHelper.h"
#include "emcCalibrationData.h"
#include "emcDataProcessorRun4.h"
#include "EmcIndexer.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "emcNodeHelper.h"
#include "emcBadModulesv1.h"
#include "emcDataError.h"

#include "PHNodeIterator.h"
#include "PHDataNode.h"
#include "PHCompositeNode.h"
#include "PHTimeStamp.h"
#include "Event.h"

#include <memory>
#include <cassert>

using namespace std;

//_____________________________________________________________________________
namespace
{
  Event* getEvent(PHCompositeNode* topNode)
  {
    PHNodeIterator it(topNode);
    PHDataNode<Event>* eventNode =  
      static_cast<PHDataNode<Event>*>(it.findFirst("PHDataNode","PRDF"));
    if (eventNode)
      {
	return eventNode->getData();
      }
    return 0;
  }
}

//_____________________________________________________________________________
mEmcCalibratorModulev2::mEmcCalibratorModulev2(int runnumber,
					       const PHTimeStamp& ts,
					       bool constantGains,
					       const emcDataStorageMap& store,
					       const char* sectors):
  SubsysReco("mEmcCalibratorModulev2")
{
  fSectors = sectors;
  emcDataProcessorRun4* dp = new emcDataProcessorRun4(runnumber,
						      ts,
						      true,
						      store.storage(),
						      fSectors.c_str());
  emcCalibrationDataHelper* cdh = dp->getCalibrationDataHelper();
  fDataProcessor = dp;

  if ( !store.empty() )
    {
      // We're instructed to change some data sources
      // WARNING: this is really an expert mode of operation !
      // Do not complain if things do not work then...
      std::vector<std::string> ks = store.knownStorages();
      for ( size_t i = 0; i < ks.size(); ++i )
	{
	  cdh->setSource(ks[i],store.storage(ks[i]));
	}
    }

  fTofSectorOffsets.resize(8);

  for ( size_t is = 0; is < fTofSectorOffsets.size(); ++is )
    {
      const emcCalibrationData* so = 
	cdh->getCalibrationData("TofSectorOffset",is);
      fTofSectorOffsets[is] = - so->GetValue(0,2) + so->GetValue(0,3);
      std::cout << "Tof Sector Offset for " << EmcIndexer::EmcSectorId(is)
		<< "=" << fTofSectorOffsets[is] << " ns"
		<< std::endl;
    }

  fTimeStamp = new PHTimeStamp(ts);
  fBadModules = new emcBadModulesv1(*fTimeStamp,
				    emcBadModules::kAll,
				    store.storage(),
				    true,
				    fSectors.c_str());
  fConstantGains = constantGains;
  fRunNumber = runnumber;
}

//_____________________________________________________________________________

mEmcCalibratorModulev2::~mEmcCalibratorModulev2()
{
  delete fDataProcessor;
  delete fTimeStamp;
  delete fBadModules;
}

//_____________________________________________________________________________
void
mEmcCalibratorModulev2::changeToF(emcTowerContainer& towers)
{
  for ( size_t i = 0; i < towers.size(); ++i )
    {
      emcTowerContent* t = towers.getTower(i);
      int is, dummy;
      EmcIndexer::decodeTowerId(t->towerid(),is,dummy,dummy);
      t->SetCalibrated(t->Energy(),t->ToF() + fTofSectorOffsets[is]);
    }
}

//_____________________________________________________________________________
int
mEmcCalibratorModulev2::process_event(PHCompositeNode* topNode)
{
  Event* event = getEvent(topNode);

  if ( !event ) 
    {
      cerr << Name() << "::event : cannot get Event node/object" << endl;
      return ABORTRUN;
    }

  emcTowerContainer* tc = emcNodeHelper::getObject<emcTowerContainer>
    ("emcTowerContainer",topNode);
  
  tc->Reset();

  auto_ptr<emcTowerContainer> pbsc(tc->create());
  auto_ptr<emcTowerContainer> pbgl(tc->create());

  bool ok = fDataProcessor->decode(*event,pbsc.get(),pbgl.get());

  assert(fBadModules!=0);

  if (ok)
    {
      ok = fDataProcessor->toADCandTDC(pbsc.get(),pbgl.get(),*fBadModules);
    }

  if (ok)
    {
      time_t thetime;
      if ( fConstantGains ) 
	{
	  thetime = fTimeStamp->getTics();
	}
      else
	{
	  thetime = event->getTime();
	}
      ok = fDataProcessor->calibrate(pbsc.get(),pbgl.get(),thetime);
    }

  if (ok)
    {
      // use a filter here if you wish
      int out=0;
      for ( size_t i = 0; i < pbsc->size(); ++i ) 
	{
	  emcTowerContent* tin = pbsc->getTower(i);
	  if ( tin->ADC() > 10 && 
	       !(tin->DataError() & emcDataError::CHANNEL_DISABLED()) )
	    {
	      emcTowerContent* tout = tc->addTower(out,*tin);
	      assert(tout!=0);
	      ++out;
	    }
	}
      pbsc->Reset();
      // filter might be different for both subsystem... or not.
      for ( size_t i = 0; i < pbgl->size(); ++i ) 
	{
	  emcTowerContent* tin = pbgl->getTower(i);
	  if ( tin->ADC() > 10 && 
	       !(tin->DataError() & emcDataError::CHANNEL_DISABLED()) )
	    {
	      emcTowerContent* tout = tc->addTower(out,*tin);
	      assert(tout!=0);
	      ++out;
	    }
	}
      pbgl->Reset();
    }
  else
    {
      tc->Reset();
      return ABORTRUN;
    }

  changeToF(*tc);
  return EVENT_OK;
}
