#ifndef __MBBCGHITRAWMODULE_H__
#define __MBBCGHITRAWMODULE_H__

#include "phool.h"
#include "PHPointerList.h"
#include "PHNode.h"

class PHCompositeNode;

class mBbcGhitRawModule
{
public:
  mBbcGhitRawModule() {}
  virtual ~mBbcGhitRawModule() {}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MBBCGHITRAWMODULE_H__*/
