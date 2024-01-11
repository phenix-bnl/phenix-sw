#ifndef __MTOFSETUCALMODULE_H__
#define __MTOFSETUCALMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mTofSetUcalModule
{
public:
  mTofSetUcalModule(){}
  virtual ~mTofSetUcalModule(){}
  PHBoolean event(PHCompositeNode *);
};
#endif /*__MTOFSETUCALMODULE_H__*/
