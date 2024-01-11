#ifndef __MTOFFEMMODULE_H__
#define __MTOFFEMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mTofFEMModule
{
public:
  mTofFEMModule(){}
  virtual ~mTofFEMModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MTOFFEMMODULE_H__*/
