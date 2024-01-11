#ifndef __MTOFASSOCPIDMODULE_H__
#define __MTOFASSOCPIDMODULE_H__

#include "PAM.hh"
#include "PHPointerList.h"
#include "PHNode.h"

class PHCompositeNode;

class mTofAssocPidModule: public PAM {
public:
  mTofAssocPidModule() {}
  virtual ~mTofAssocPidModule() {}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MTOFASSOCPIDMODULE_H__*/
