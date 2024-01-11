#ifndef __MBBCOUTEVALMODULE_H__
#define __MBBCOUTEVALMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mBbcOutEvalModule
{
public:
  mBbcOutEvalModule() {}
  virtual ~mBbcOutEvalModule() {}
  virtual PHBoolean event(PHCompositeNode *);
};
#endif /*__MBBCOUTEVALMODULE_H__*/
