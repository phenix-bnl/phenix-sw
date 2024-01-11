#ifndef __MEMCEVENTMODULE_H__
#define __MEMCEVENTMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mEmcEventModule
{
public:
  mEmcEventModule(){}
  virtual ~mEmcEventModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCEVENTMODULE_H__*/
