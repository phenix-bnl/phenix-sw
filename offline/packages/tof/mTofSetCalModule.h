#ifndef __MTOFSETCALMODULE_H__
#define __MTOFSETCALMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mTofSetCalModule
{
public:
  mTofSetCalModule(){}
  virtual ~mTofSetCalModule(){}
  PHBoolean event(PHCompositeNode *);
};
#endif /*__MTOFSETCALMODULE_H__*/
