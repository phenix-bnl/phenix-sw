#include "emcOMTimeStamp.h"
#include "RunToTime.hh"
#include "emcTimeStamp.h"
#include <memory>

emcOMTimeStamp gemcOMTimeStamp("PostgreSQL:emcOMTimeStamp","Wrap reading of RunToTimePg");

//_____________________________________________________________________________
emcOMTimeStamp::emcOMTimeStamp(const char* name, const char* title)
  : emcObjectManager(name,title)
{
}

//_____________________________________________________________________________
emcOMTimeStamp::~emcOMTimeStamp()
{
  Reset();
}

//_____________________________________________________________________________
bool
emcOMTimeStamp::CanRead(const emcManageable& object) const
{
  const emcManageable* pobject = &object;
  const emcTimeStamp* test = dynamic_cast<const emcTimeStamp*>(pobject);

  if ( test && object.GetSource() == emcManageable::kDB_Pg ) 
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
emcOMTimeStamp::Read(emcManageable& object, int runnumber)
{
  emcTimeStamp& ts = static_cast<emcTimeStamp&>(object);

  std::map<int,PHTimeStamp*>::const_iterator it = fTSMap.find(runnumber);

  if ( it != fTSMap.end() )
    {
      ts.setTimeStamp(*(it->second));
      return true;
    }
  else
    {
      RunToTime *runTime = RunToTime::instance();
      PHTimeStamp* rts = runTime->getBeginTime(runnumber);
      
      if ( rts != 0 )
	{
	  ts.setTimeStamp(*rts);
	  fTSMap[runnumber]=rts;
	  return true;
	}
      else
	{
	  return false;
	}
    }
}

//_____________________________________________________________________________
void
emcOMTimeStamp::Reset()
{
  std::map<int,PHTimeStamp*>::iterator it;
  for ( it = fTSMap.begin(); it != fTSMap.end(); ++it ) 
    {
      delete it->second;
    }
  fTSMap.clear();
}
