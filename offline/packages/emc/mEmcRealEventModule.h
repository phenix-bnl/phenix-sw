// (c) PHENIX Collaboration 2001/11/16
// Author:  G. David

#ifndef __MEMCREALEVENTMODULE_H__
#define __MEMCREALEVENTMODULE_H__
#include "phool.h"

#include "PHPoint.h"

class VtxOut;
class PHCompositeNode;

class mEmcRealEventModule
{
 public:
  /// default ctor
  mEmcRealEventModule(){}
  /// dtor
  virtual ~mEmcRealEventModule() {} ;

  /// required by PHOOL (kept empty)
  PHBoolean  event(PHCompositeNode * root) ;

  private:
  VtxOut* vtxout;
  PHPoint vertex;
};
#endif /*__MEMCREALEVENTMODULE_H__*/



