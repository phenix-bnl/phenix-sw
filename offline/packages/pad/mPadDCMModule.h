#ifndef __MPADDCMMODULE_H__
#define __MPADDCMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mPadDCMModule
{
public:
  mPadDCMModule();
  virtual ~mPadDCMModule(){}
  PHBoolean event(PHCompositeNode *);
  int get_pcnumber();
  void set_pcnumber(int);

 private:
  int pcnumber;

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MPADDCMMODULE_H__*/
