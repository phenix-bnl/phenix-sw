#include "NoiseEvents.h"

using namespace std;

NoiseEvents::NoiseEvents(const string &name):Recalibrator(name)
{
  baseclasses.insert("PreviousEvent");

  prevevt=0;
  return ;
}

int NoiseEvents::isValidRun(const int runno) const
{
  if(runno<BEGIN_OF_RUN6)
    {
      // noise events are a problem only since Run6
      return 0;
    }

  return 1;
}

int NoiseEvents::process_event(PHCompositeNode *topNode)
{
  prevevt = findNode::getClass<PreviousEvent>(topNode,"PreviousEvent");

  if(prevevt)
    {
      int clockticks0 = prevevt->get_clockticks(0);
      int clockticks1 = prevevt->get_clockticks(1);
      int clockticks2 = prevevt->get_clockticks(2);
      
      if (clockticks0>112 && clockticks0<124)
	{
	  return ABORTEVENT;
	}
      if ((clockticks1>112 && clockticks1<124) ||
	  (clockticks1>269 && clockticks1<280))
	{
	  return ABORTEVENT;
	}
      if ((clockticks2>112 && clockticks2<124) ||
	  (clockticks2>269 && clockticks2<280) ||
	  (clockticks2>427 && clockticks2<437))
	{
	  return ABORTEVENT;
	}
    }

  return EVENT_OK;
}

