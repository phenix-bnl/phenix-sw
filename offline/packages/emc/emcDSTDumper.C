#include "ezdst.h"
#include <iostream>
#include "emcClusterContainer.h"
#include "emcTowerContainer.h"
#include "emcObjectFillerManager.h"

//_____________________________________________________________________________
int process_event(DstContent* dst)
{
  static bool first = true;

  emcTowerContainer* tc = static_cast<emcTowerContainer*>
    (dst->getClass("emcTowerContainer"));

  if (tc)
    {
      if ( first ) 
	{
	  tc->identify();
	}
      tc->print(std::cout,1);
    }

  emcClusterContainer* cc = static_cast<emcClusterContainer*>
    (dst->getClass("emcClusterContainer"));

  if (cc)
    {
      if ( first ) 
	{
	  cc->identify();
	}
      cc->print();
    }

  first = false;

  return 0;
}
