#ifndef __EMCDATAPROCESSORRUN4_H__
#define __EMCDATAPROCESSORRUN4_H__

#ifndef __EMCDATAPROCESSORV2_H__
#include "emcDataProcessorv2.h"
#include "emcManageable.h"
#endif

/** Implementation of emcDataProcessor, for Run4. 
@ingroup calibration
*/

class emcDataProcessorRun4 : public emcDataProcessorv2
{
public:
  emcDataProcessorRun4();

  emcDataProcessorRun4(int runnumber, const PHTimeStamp& ts, 
		       bool initall = true,
		       emcManageable::EStorage dataSource
		       = emcManageable::kDB_Pg,
		       const char* sectors = "emcal");

  virtual ~emcDataProcessorRun4();
  
protected:
  virtual emcDCProcessor* 
  getDCProcessor(emcCalibrationDataHelper*) const;

  virtual emcPacketProcessor* 
  getPacketProcessor() const;

  virtual emcRawDataProcessor* 
  getRawDataProcessor(emcCalibrationDataHelper*) const;

  ClassDef(emcDataProcessorRun4,1) // EMCAL Raw Data Processor for Run4
};

#endif
