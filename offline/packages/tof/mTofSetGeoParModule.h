#ifndef __MTOFSETGEOPARMODULE_H__
#define __MTOFSETGEOPARMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mTofSetGeoParModule
{
public:
  mTofSetGeoParModule(){}
  virtual ~mTofSetGeoParModule(){}
  PHBoolean event(PHCompositeNode *);
};
#endif /*__MTOFSETGEOPARMODULE_H__*/
