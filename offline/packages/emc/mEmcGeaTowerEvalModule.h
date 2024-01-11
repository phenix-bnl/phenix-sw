#ifndef __MEMCGEATOWEREVALMODULE_H__
#define __MEMCGEATOWEREVALMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mEmcGeaTowerEvalModule
{
public:
  mEmcGeaTowerEvalModule(){}
  virtual ~mEmcGeaTowerEvalModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCGEATOWEREVALMODULE_H__*/
