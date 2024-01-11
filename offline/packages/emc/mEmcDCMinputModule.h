#ifndef __MEMCDCMINPUTMODULE_H__
#define __MEMCDCMINPUTMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mEmcDCMinputModule
 {
public:
   mEmcDCMinputModule(){}
   virtual ~mEmcDCMinputModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCDCMINPUTMODULE_H__*/
