#ifndef __MTOFSETGEOMODULE_H__
#define __MTOFSETGEOMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mTofSetGeoModule
{
public:
  mTofSetGeoModule(){}
  virtual ~mTofSetGeoModule(){}
  PHBoolean event(PHCompositeNode *);
};
#endif /*__MTOFSETGEOMODULE_H__*/
