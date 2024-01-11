#ifndef __MBBCSETGEOMODULE_H__
#define __MBBCSETGEOMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mBbcSetGeoModule
{
public:
  mBbcSetGeoModule() {}
  virtual ~mBbcSetGeoModule() {}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MBBCSETGEOMODULE_H__*/
