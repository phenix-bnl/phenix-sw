#ifndef __MTOFRAWRECMODULE_H__
#define __MTOFRAWRECMODULE_H__

#include "PHModule.h"
#include "phool.h"

class PHCompositeNode;
class TofAddressObject;
class TofGeometryObject;
class TofCalibObject;

class mTofRawRecModule: public PHModule{
public:
  mTofRawRecModule();
  virtual ~mTofRawRecModule(){}
  PHBoolean setCutParameter(float charge, float tvcpede);
  PHBoolean event(PHCompositeNode *, TofAddressObject *, 
		  TofGeometryObject *, TofCalibObject *,
		  float charge, float tvcpede);
  PHBoolean event(PHCompositeNode *, TofAddressObject *, 
		  TofGeometryObject *, TofCalibObject *);
  PHBoolean event(PHCompositeNode *);
  void setDebugLevel(int debug) { iDebug = debug; }
private:
  // Debug flag (0 = do not debug)
  int iDebug;

  float chargecut;
  float tvcpedecut;

};
#endif /*__MTOFRAWRECMODULE_H__*/
