#ifndef __MPADUNPACKMODULE_H__
#define __MPADUNPACKMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mPadUnpackModule
{
public:
  mPadUnpackModule();
  virtual ~mPadUnpackModule(){}
  PHBoolean event(PHCompositeNode *);
  int get_pcnumber();
  void set_pcnumber(int);

 private:
  int pcnumber;

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MPADUNPACKMODULE_H__*/
