//
//  TKH--Changed Interface to Slewing Corrections
//        11-25-2001
//
#ifndef __MNEWDCHCALIBRATOR_H__
#define __MNEWDCHCALIBRATOR_H__

#include "phool.h"
#include "PHCompositeNode.h"
#include "PHPointerList.h"

#include "table_header.h"

#include "dDchRaw.h"
#include "dDchRawWrapper.h"

#include "dDchGhitRaw.h"
#include "dDchGhitRawWrapper.h"

#include "dDchHit.h"
#include "dDchHitWrapper.h"

#include "dDchGhitHits.h"
#include "dDchGhitHitsWrapper.h"

#include "DchHitLineTablev1.hh"
#include "DchHitLineOutv1.hh"
#include "DchRawTable.hh"
#include "gsl/gsl_rng.h"

class PHDchCalibrationObject;
class PHDchAddressObject;
class PHDchGeometryObject;
class PHDchNoiseObject;

class mNewDchCalibrator
{
public:
  mNewDchCalibrator();
  virtual ~mNewDchCalibrator();
  PHBoolean event(PHCompositeNode *);
  int  ResetEvent (PHCompositeNode *topNode);

  const PHDchAddressObject*     getDAO() const { return dchAddressObject;}
  const PHDchGeometryObject*    getDGO() const { return dchGeometryObject;}
  const PHDchCalibrationObject* getDCO() const { return dchCalibrationObject;}
  const PHDchNoiseObject*       getDNO() const { return dchNoiseObject;}

  int   getWireType(int plane) ;
  float getBbcTimeZero() {return timeZeroBBC;}
  float getBbcTimeZeroOffset() ;
  void setVerbose(int val) {verbose = val;}
  int numberOfHits() { return nhits;}
  PHBoolean analyzeTimeZeroBBC();
  PHBoolean correctTime();
  void setCorrectTimeFlag(int val) { correctTimeFlag = val;}
  void setSlewingCorrectionFlag(int val) {slewingCorrectionFlag = val;}
  float slewCorrection(short arm, short plane, float width);

  const gsl_rng_type * T;
  gsl_rng * rand;

  float delt0;
  float deldv;
  float delt0_e;
  float deldv_e;
  float delt0_w;
  float deldv_w;
  void setCalibCorr(float ddt_e, float ddv_e, float ddt_w, float ddv_w ) 
  { delt0_e = ddt_e; deldv_e = ddv_e; delt0_w = ddt_w; deldv_w = ddv_w; }
  
protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
  void fillOutputTables();
  void fillWrapperTables();

private:
  DchRawTable* rawTable;
  DchHitLineTable *hitLineTable;
  PHPointerList<DchHitLineOutv1> hitLineOutList;
  int verbose;
  PHCompositeNode*        topNode;
  PHDchGeometryObject*    dchGeometryObject;
  PHDchAddressObject*     dchAddressObject;
  PHDchCalibrationObject* dchCalibrationObject;
  PHDchNoiseObject*       dchNoiseObject;
  int nhits;
  int timeZeroBBC;
  int correctTimeFlag;
  int slewingCorrectionFlag;
};
#endif /*__MNEWDCHCALIBRATOR_H__*/
