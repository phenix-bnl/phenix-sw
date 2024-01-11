#include "emcTimeStamp.h"

//_____________________________________________________________________________
emcTimeStamp::emcTimeStamp() : emcManageable("TimeStamp","emcTimeStamp wrapper","emcTimeStamp")
{
  fTimeStamp.setTics(0);
}

//_____________________________________________________________________________
emcTimeStamp::~emcTimeStamp()
{
}
