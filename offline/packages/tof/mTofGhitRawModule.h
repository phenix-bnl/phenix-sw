#ifndef __MTOFGHITRAWMODULE_H__
#define __MTOFGHITRAWMODULE_H__

#include "phool.h"

class PHComPositeNode;
class TofAddressObject;
class TofGeometryObject;
class TofCalibObject;

class mTofGhitRawModule
{
public:
  mTofGhitRawModule();
  virtual ~mTofGhitRawModule() {}
  PHBoolean setTimingResolution(float sigma);
  PHBoolean setAttenuationLength(float atten);
  PHBoolean event(PHCompositeNode *);
  PHBoolean event(PHCompositeNode *, TofAddressObject *,
		  TofGeometryObject *, TofCalibObject *);

  void setDebugLevel(int debug) { iDebug = debug; }
private:
  // Debug flag (0 = do not debug)
  int iDebug;

  float timingsigma; // intrinsic timing resolution [ns]
  float attenuation; // Scintillator attenuation length [cm]

};
#endif /*__MTOFGHITRAWMODULE_H__*/
