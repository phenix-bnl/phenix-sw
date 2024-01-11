#ifndef __MTOFUNPACKMODULE_H__
#define __MTOFUNPACKMODULE_H__

#include "PHModule.h"
#include "phool.h"

class TofAddressObject;
class PHCompositeNode;

class mTofUnpackModule: public PHModule{
public:
  mTofUnpackModule();
  virtual ~mTofUnpackModule(){}
  PHBoolean event(PHCompositeNode *, TofAddressObject *);
  PHBoolean event(PHCompositeNode *);
  void setDebugLevel(int debug) { iDebug = debug; }
private:
  // Debug flag (0 = do not debug)
  int iDebug;
};
#endif /*__MTOFUNPACKMODULE_H__*/
