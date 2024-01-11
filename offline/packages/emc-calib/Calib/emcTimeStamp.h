#ifndef __EMCTIMESTAMP_H__
#define __EMCTIMESTAMP_H__

#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif

/** Adapter class.
Makes a PHTimeStamp being an emcManageable.
@ingroup calibration
*/

class emcTimeStamp : public emcManageable
{
public:
  emcTimeStamp();
  virtual ~emcTimeStamp();

  PHTimeStamp getTimeStamp(void) const { return fTimeStamp; }

  void setTimeStamp(PHTimeStamp& ts) { fTimeStamp = ts; }

  virtual const char* GetCategory(void) const { return "emcTimeStamp"; }

private:
  PHTimeStamp fTimeStamp;
};
#endif
