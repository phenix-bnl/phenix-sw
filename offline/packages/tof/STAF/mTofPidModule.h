#ifndef __MTOFPIDMODULE_H__
#define __MTOFPIDMODULE_H__

#include "phool.h"
#include "PHPointerList.h"
#include "PAM.hh"
#include "PHNode.h"

class PHCompositeNode;

class mTofPidModule: public PAM {
public:
  mTofPidModule(){}
  virtual ~mTofPidModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MTOFPIDMODULE_H__*/
