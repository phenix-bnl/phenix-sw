#include <Fun4AllReturnCodes.h>
#include "mEmcReCalibratorModulev1.h"

#include "emcDataProcessorv2.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "emcNodeHelper.h"
#include "emcBadModulesv1.h"
#include "emcDataError.h"
#include "EmcIndexer.h"

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
mEmcReCalibratorModulev1::mEmcReCalibratorModulev1(int runnumber,
						   const PHTimeStamp& ts,
						   bool constantGains):
  SubsysReco("mEmcReCalibratorModulev1")
{
  fDataProcessor = new emcDataProcessorv2(runnumber,ts);
  fTimeStamp = new PHTimeStamp(ts);
  fBadModules = new emcBadModulesv1(*fTimeStamp);
  fConstantGains = constantGains;
  fRunNumber = runnumber;
}

//_____________________________________________________________________________
mEmcReCalibratorModulev1::~mEmcReCalibratorModulev1()
{
  delete fDataProcessor;
  delete fTimeStamp;
  delete fBadModules;
}

//_____________________________________________________________________________
int
mEmcReCalibratorModulev1::process_event(PHCompositeNode* topNode)
{
  emcTowerContainer* tc = emcNodeHelper::getObject<emcTowerContainer>
    ("emcTowerContainer",topNode);

  auto_ptr<emcTowerContainer> pbsc(tc->clone());
  auto_ptr<emcTowerContainer> pbgl(tc->clone());
  pbsc->Reset();
  pbgl->Reset();
  for ( size_t i = 0; i < tc->size(); ++i ) 
    {
      emcTowerContent* tin = tc->getTower(i);
      int towerid = tin->towerid();
      if ( EmcIndexer::isPbSc(towerid) )
	{
	  emcTowerContent* tout = pbsc->addTower(pbsc->size(),*tin);
	  assert(tout);
	}
      else if ( EmcIndexer::isPbGl(towerid) )
	{
	  emcTowerContent* tout = pbgl->addTower(pbgl->size(),*tin);
	  assert(tout);
	}
    }
  assert( pbsc->size() + pbgl->size() == tc->size() );
  tc->Reset();

  bool ok = true;

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
      thetime = fTimeStamp->getTics();
      //      if ( fConstantGains ) 
      //	{
      //	  thetime = fTimeStamp->getTics();
      //	}
      //      else
      //	{
      //	  std::auto_ptr<PHTimeStamp> ts(event->getTime());
      //	  thetime = ts->getTics();
      //	}
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
