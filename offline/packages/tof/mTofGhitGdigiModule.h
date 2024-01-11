#ifndef __MTOFGHITGDIGIMODULE_H__
#define __MTOFGHITGDIGIMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mTofGhitGdigiModule
{
public:
  mTofGhitGdigiModule(){}
  virtual ~mTofGhitGdigiModule(){}
  PHBoolean event(PHCompositeNode *);
};
#endif /*__MTOFGHITGDIGIMODULE_H__*/
