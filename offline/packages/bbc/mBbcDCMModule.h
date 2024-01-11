#ifndef __MBBCDCMMODULE_H__
#define __MBBCDCMMODULE_H__

#include "phool.h"
#include "PHPointerList.h"
#include "PHNode.h"

class PHCompositeNode;

class mBbcDCMModule
{
public:
  mBbcDCMModule() {}
  virtual ~mBbcDCMModule() {}
  PHBoolean event(PHCompositeNode *);
  PHBoolean event(PHCompositeNode *,PHString bbcFEMNodeName,PHString bbcDCMNodeName);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MBBCDCMMODULE_H__*/
