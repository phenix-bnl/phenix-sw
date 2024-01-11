#ifndef __MPADEVALUATEMODULE_H__
#define __MPADEVALUATEMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mPadEvaluateModule
{
public:
  mPadEvaluateModule();
  virtual ~mPadEvaluateModule(){}
  PHBoolean event(PHCompositeNode *);
  int get_pcnumber();
  void set_pcnumber(int);

 private:
  int pcnumber;

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MPADEVALUATEMODULE_H__*/
