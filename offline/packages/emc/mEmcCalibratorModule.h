// $Id: mEmcCalibratorModule.h,v 2.15 2014/01/26 19:27:56 bbannier Exp $
//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 1999
//
// Purpose: wraps the filling of rdo, mdo, and cdo from prdf
//
// Description: algorithm class
//
// Author:  Gines MARTINEZ SUBATECH, Broohaven Aug-00
//
//-----------------------------------------------------------------------------

#ifndef __MEMCCALIBRATORMODULE_H__
#define __MEMCCALIBRATORMODULE_H__

#include <map>
#include "phool.h"
#include "SubsysReco.h"
#include "Fun4AllReturnCodes.h"
#include "emcMixedDataObject.h"
#include "emcManageable.h"
#include "PHTimeStamp.h"

class emcCalibratedDataObject;
class emcRawDataAccessor;
class emcCalibrator;
class emcRawDataObject;

/** (OLD) Was our calibration module.

See mEmcCalibratorModulev1 and mEmcCalibratorModulev2 now.

@ingroup deprecated
*/

class mEmcCalibratorModule: public SubsysReco
{

public:

  /// default ctor
  mEmcCalibratorModule(const PHTimeStamp& ts,
		       const char* calibrator = "emcRawDataCalibrator",
		       emcManageable::EStorage source = emcManageable::kDB_Objy);

  mEmcCalibratorModule(const char* configfilename = "emcwb.conf",		       
		       const char* calibrator = "emcRawDataCalibrator",
		       emcManageable::EStorage source = emcManageable::kDB_Objy);


  /// dtor
  virtual ~mEmcCalibratorModule()
  {}
 
  /// returns a raw data object
  emcRawDataObject* GetEmcRawDataObject()
  {
    return fRdo;
  }

  /** initializes the module: config file (from DB), type of calibrator
      and data sources (default is DB)
  */

  bool Init(const char* configfilename = "emcwb.conf",
	    const char* calibrator = "emcRawDataCalibrator",
	    emcManageable::EStorage source = emcManageable::kDB_Objy);

  bool Init(const PHTimeStamp& ts,
	    const char* calibrator = "emcRawDataCalibrator",
	    emcManageable::EStorage source = emcManageable::kDB_Objy);
  int Init(PHCompositeNode*) {  // unhide SubsysReco::Init(PHCompositeNode*)
    PHOOL_VIRTUAL_WARNING;
    return ABORTRUN;
  }

  /// Do the real job here.
  int process_event(PHCompositeNode * root);

  /// Setter
  bool SetCalibrator(const char* calibrator);
  /// Setter
  bool SetHighLowLimit(int limit = 512);
  /// Setter
  void SetVerbose(int verbo)
  {
    fVerbose = verbo;
  }
  /// Setter
  bool SetZeroSuppression(bool ZeroSupression = true);
  /// Setter
  void SelectSource(emcManageable::EStorage source);

  /** Tell the calibrator to use only this time stamp.
      If this method is not called, time stamp will be fetched
      from each event. */
  void UseTimeStamp(const PHTimeStamp& when);

  /** Use this one if framework (e.g. Fun4All) disconnect itself from DB 
      even before first event is read in.
  */
  void ForceDBCollection();

private:
  bool Config(const char* calibrator, emcManageable::EStorage source);

private:

  // MV 2001/09/24
  // We need to find which MDO index correspond to each CDO index.
  // ItemId is the only key to bind those internal indeces.
  // fIndexMap has ItemId as a key and an index from EmcDynamicData::DataMap
  // index as a value.
  std::map<int, int> fIndexMap;
  std::map<int, int>::iterator fIndexMapIter;
  PHTimeStamp fTimeStamp;
  bool fUseTimeStamp;
  emcCalibratedDataObject* fCdo;
  emcRawDataAccessor * fRda;
  emcMixedDataObject fMdo;
  emcCalibrator* fRdc;
  emcRawDataObject* fRdo;
  bool fZeroSuppression;
  int fVerbose;
  int fHighLowLimit;
};

#endif /*__MEMCCALIBRATORMODULE_H__*/



