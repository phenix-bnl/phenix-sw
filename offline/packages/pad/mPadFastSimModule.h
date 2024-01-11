#ifndef __MPADFASTSIMMODULE_H__
#define __MPADFASTSIMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mPadFastSimModule
{
public:
  mPadFastSimModule(){}
  virtual ~mPadFastSimModule(){}
  PHBoolean event(PHCompositeNode *);
  int get_pcnumber();
  void set_pcnumber(int);

 private:
  int pcnumber;

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MPADFASTSIMMODULE_H__*/
