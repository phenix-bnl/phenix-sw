#ifndef __MBBCSETUCALMODULE_H__
#define __MBBCSETUCALMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mBbcSetUcalModule
{
public:
  mBbcSetUcalModule() {}
  virtual ~mBbcSetUcalModule() {}
  virtual PHBoolean event(PHCompositeNode *);
};
#endif /*__MBBCSETUCALMODULE_H__*/
