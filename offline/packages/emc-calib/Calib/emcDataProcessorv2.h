#ifndef __EMCDATAPROCESSORV2_H__
#define __EMCDATAPROCESSORV2_H__

#ifndef __EMCDATAPROCESSOR_H__
#include "emcDataProcessor.h"
#include "emcManageable.h"
#endif
#include <set>

class emcPacketProcessor;
class emcRawDataProcessor;
class emcDCProcessor;
class emcCalibrationDataHelper;
class PHTimeStamp;

/** Implementation of emcDataProcessor, for Run3. 
@ingroup calibration
*/

class emcDataProcessorv2 : public emcDataProcessor
{

public:

  emcDataProcessorv2();

  emcDataProcessorv2(int runnumber, const PHTimeStamp& ts, 
		     bool initall = true,
		     emcManageable::EStorage dataSource=emcManageable::kDB_Pg,
		     const char* sectors = "emcal");

  virtual ~emcDataProcessorv2();

  virtual bool calibrate(emcTowerContainer* pbsc, 
			 emcTowerContainer* pbgl,
			 time_t incrementalTime=0);

  virtual bool decode(const Event& event, 
		      emcTowerContainer* pbsc, 
		      emcTowerContainer* pbgl);

  virtual void identify(std::ostream& os=std::cout) const;

  virtual int isValid() const;

  virtual void Reset();
 
  virtual bool toADCandTDC(emcTowerContainer* pbsc, 
			   emcTowerContainer* pbgl,
			   const emcBadModules& bad);

  virtual void setRunNumber(int runnumber) { fRunNumber = runnumber; }

  /// Give access to the calibrationdatahelper we're using.
  emcCalibrationDataHelper* getCalibrationDataHelper() const;

protected:
  // Selects the worker child classes.

  virtual emcDCProcessor* 
  getDCProcessor(emcCalibrationDataHelper*) const;

  virtual emcPacketProcessor* 
  getPacketProcessor() const;
  
  virtual emcRawDataProcessor* 
  getRawDataProcessor(emcCalibrationDataHelper*) const;

private:

  emcPacketProcessor* fPacketProcessor;
  emcRawDataProcessor* fRawDataProcessor;
  emcDCProcessor* fDCProcessor;
  emcCalibrationDataHelper* fCalibrationDataHelper;
  
  int fRunNumber;
  PHTimeStamp* fTimeStamp;
  std::set<int> fFemList;

  ClassDef(emcDataProcessorv2,0) // EMCAL Raw Data Processor v2 = Run 3
};

#endif
