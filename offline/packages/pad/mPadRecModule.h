#ifndef __MPADRECMODULE_H__
#define __MPADRECMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mPadRecModule
{
public:
  mPadRecModule();
  virtual ~mPadRecModule(){}
  PHBoolean event(PHCompositeNode *);
  int get_pcnumber();
  void set_pcnumber(int);

 private:
  int pcnumber;

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MPADRECMODULE_H__*/
