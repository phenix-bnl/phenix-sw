//--------------------------------------------------------------------------
//  Declaration of class TofEvent
//
//  Purpose:     TOF Reconstruction
//  Description: TOF Event Object
//
//  Author: Tatsuya Chujo (Univ. of Tsukuba)
//
//  History: 06/11/00  T.Chujo      First Version
//           06/13/00  A.Kiyomichi  Add q1 q2 t3 t4 in dtofRaw
//           07/08/00  T.Chujo      RawToDst update
//           07/12/00  A.Kiyomichi  Add cut parameter
//           11/01/00  A.Kiyomichi  Add DstReCycle
//           11/29/01  A.Kiyomichi  Add GeaToRaw [response chain]
//           01/11/02  A.Kiyomichi  Add TC4 in DST with RHIC clothing time
//--------------------------------------------------------------------------

#ifndef PHENIX_TOFEVENT_HH
#define PHENIX_TOFEVENT_HH


#include "Tof.hh"
#include "phool.h"
#include "gsl/gsl_rng.h"

class PHCompositeNode;
class TofAddressObject;
class TofGeometryObject;
class TofCalibObject;

class TofEvent{
public:
  TofEvent();
  virtual ~TofEvent();

  PHBoolean DcmToRaw (PHCompositeNode *root, TofAddressObject *address);
  PHBoolean RawToDst (PHCompositeNode *root, TofAddressObject *address,
		      TofGeometryObject *geom, TofCalibObject *calib);
  PHBoolean DstReCycle (PHCompositeNode *root, TofAddressObject *address,
			TofGeometryObject *geom, TofCalibObject *calib);
  // Comment  --AK
  //   RawToDst: Use in Reconstruction  dTofRaw => dTofReconstructed
  //   DstReCycle: Use in DST analysis  dTofReconstructed => dTofReconstructed

  // set cut parameter
  PHBoolean setCutParameter(const float chargecut ,const float tvcpedecut);
  void  setChargeCut(const int slatid, const float v);
  void  setTvcPedeCut(const int lu, const int slatid, const float v);
  void  setCrossingTime(const float v);

  // get cut parameter
  float getChargeCut(const int slatid) {return chargecut[slatid];}
  float getTvcPedeCut(const int lu, const int slatid) {return tvcpedecut[slatid][lu];}

  //==============================
  // function for response chain
  //==============================
  // PISA tofghit => dTofRaw
  PHBoolean GeaToRaw (PHCompositeNode *root, TofAddressObject *address,
		      TofGeometryObject *geom, TofCalibObject *calib);
  PHBoolean setTimingResolution(const float sigma);
  void setTimingResolution(const int slatid, const float v) {timingsigma[slatid] = v;}
  PHBoolean setAttenuationLength(const float atten);
  void setAttenuationLength(const int slatid, const float v){attenuation[slatid] = v;}

  void setDebugLevel(const int debug) { iDebug = debug; }

private:
  // private function
  PHBoolean DstCalculator(PHCompositeNode *root, TofAddressObject *address,
			  TofGeometryObject *geom, TofCalibObject *calib);

  // Debug flag (0 = do not debug)
  int iDebug;

  // cut parameter
  float chargecut[TOF_NSLAT];
  float tvcpedecut[TOF_NSLAT][2];

  // clothing time
  float crossingtime; // RHIC crossing time

  // response parameter
  float timingsigma[TOF_NSLAT]; // intrinsic timing resolution [ns]
  float attenuation[TOF_NSLAT]; // Scintillator attenuation length [cm]

  gsl_rng *rng;
  
};

#endif  /* PHENIX_TOFEVENT_HH */
