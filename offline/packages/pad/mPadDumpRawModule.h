#ifndef __MPADDUMPRAWMODULE_H__
#define __MPADDUMPRAWMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mPadDumpRawModule
{
public:
  mPadDumpRawModule();
  virtual ~mPadDumpRawModule(){}
  PHBoolean event(PHCompositeNode *);
  int get_pcnumber();
  void set_pcnumber(int);

 private:
  int pcnumber;

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MPADDUMPRAWMODULE_H__*/
