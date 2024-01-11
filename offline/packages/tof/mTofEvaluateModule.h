#ifndef __MTOFEVALUATEMODULE_H__
#define __MTOFEVALUATEMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mTofEvaluateModule
{
public:
  mTofEvaluateModule() {}
  virtual ~mTofEvaluateModule() {}
  PHBoolean event(PHCompositeNode *);

};
#endif /*__MTOFEVALUATEMODULE_H__*/
