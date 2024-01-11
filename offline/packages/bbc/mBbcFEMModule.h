#ifndef __MBBCFEMMODULE_H__
#define __MBBCFEMMODULE_H__

#include "phool.h"

#include "PHPointerList.h"
#include "PHNode.h"

class PHCompositeNode;

class mBbcFEMModule
{
public:
  mBbcFEMModule() {}
  virtual ~mBbcFEMModule() {}
  PHBoolean event(PHCompositeNode *);
  PHBoolean event(PHCompositeNode *,PHString dBbcRawNodeName,PHString dBbcFEMNodeName);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MBBCFEMMODULE_H__*/
