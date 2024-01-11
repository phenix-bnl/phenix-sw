#ifndef __MTOFGDIGIRECMODULE_H__
#define __MTOFGDIGIRECMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mTofGdigiRecModule
{
public:
  mTofGdigiRecModule(){}
  virtual ~mTofGdigiRecModule(){}
  PHBoolean event(PHCompositeNode *);
};
#endif /*__MTOFGDIGIRECMODULE_H__*/
