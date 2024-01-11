#ifndef __MTOFPERFECTMODULE_H__
#define __MTOFPERFECTMODULE_H__

#include "phool.h"

class PHCompositeNode;

class mTofPerfectModule
{
public:
  mTofPerfectModule(){}
  virtual ~mTofPerfectModule(){}
  PHBoolean event(PHCompositeNode *root);
};
#endif /*__MTOFPERFECTMODULE_H__*/
