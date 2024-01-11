#include <Fun4AllReturnCodes.h>
#include "mEmcCalibratorModulev1.h"

#include "emcDataProcessorv2.h"
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
mEmcCalibratorModulev1::mEmcCalibratorModulev1(int runnumber,
					       const PHTimeStamp& ts,
					       bool constantGains,
					       emcManageable::EStorage source,
					       const char* sectors):
  SubsysReco("mEmcCalibratorModulev1")
{
  fSectors = sectors;
  fDataProcessor = new emcDataProcessorv2(runnumber,ts,true,
					  source,fSectors.c_str());
  fTimeStamp = new PHTimeStamp(ts);
  fBadModules = new emcBadModulesv1(*fTimeStamp,
				    emcBadModules::kAll,
				    source,
				    true,
				    fSectors.c_str());
  fConstantGains = constantGains;
  fRunNumber = runnumber;
}

//_____________________________________________________________________________
mEmcCalibratorModulev1::~mEmcCalibratorModulev1()
{
  delete fDataProcessor;
  delete fTimeStamp;
  delete fBadModules;
}

//_____________________________________________________________________________
int
mEmcCalibratorModulev1::process_event(PHCompositeNode* topNode)
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

  auto_ptr<emcTowerContainer> pbsc(tc->clone());
  auto_ptr<emcTowerContainer> pbgl(tc->clone());

  bool ok = fDataProcessor->decode(*event,pbsc.get(),pbgl.get());

  if ( !fBadModules)
    {
      fBadModules = new emcBadModulesv1(*fTimeStamp);
    }

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
      return EVENT_OK;
    }
  else
    {
      tc->Reset();
      return ABORTRUN;
    }
}
