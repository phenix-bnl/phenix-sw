#ifndef __MTOFRECASSOCMODULE_H__
#define __MTOFRECASSOCMODULE_H__

#include "phool.h"
#include "PHPointerList.h"
#include "PAM.hh"
#include "PHNode.h"

class PHCompositeNode;

class mTofRecAssocModule: public PAM {
public:
  mTofRecAssocModule(){}
  virtual ~mTofRecAssocModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MTOFRECASSOCMODULE_H__*/
