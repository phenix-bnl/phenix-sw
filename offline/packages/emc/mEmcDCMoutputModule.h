#ifndef __MEMCDCMOUTPUTMODULE_H__
#define __MEMCDCMOUTPUTMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mEmcDCMoutputModule
{
public:
  mEmcDCMoutputModule(){}
  virtual ~mEmcDCMoutputModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCDCMOUTPUTMODULE_H__*/
