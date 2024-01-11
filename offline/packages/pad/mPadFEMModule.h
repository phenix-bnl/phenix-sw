#ifndef __MPADFEMMODULE_H__
#define __MPADFEMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mPadFEMModule
{
public:
  mPadFEMModule(){}
  virtual ~mPadFEMModule(){}
  PHBoolean event(PHCompositeNode *);
  int get_pcnumber();
  void set_pcnumber(int);

 private:
  int pcnumber;

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MPADFEMMODULE_H__*/
