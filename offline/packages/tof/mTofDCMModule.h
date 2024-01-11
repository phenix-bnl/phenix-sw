#ifndef __MTOFDCMMODULE_H__
#define __MTOFDCMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mTofDCMModule
{
public:
  mTofDCMModule(){}
  virtual ~mTofDCMModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MTOFDCMMODULE_H__*/
