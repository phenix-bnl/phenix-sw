#ifndef __MDCHDCMMODULE_H__
#define __MDCHDCMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mDchDCMModule
{
public:
  mDchDCMModule(){}
  virtual ~mDchDCMModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MDCHDCMMODULE_H__*/
