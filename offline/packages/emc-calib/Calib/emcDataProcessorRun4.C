#include "emcDataProcessorRun4.h"

#include "emcPacketProcessorv1.h"
#include "emcDCProcessorv3.h"
#include "emcRawDataProcessorv3.h"

//_____________________________________________________________________________
emcDataProcessorRun4::emcDataProcessorRun4()
  : emcDataProcessorv2()
{
}

//_____________________________________________________________________________
emcDataProcessorRun4::emcDataProcessorRun4(int runnumber, 
					   const PHTimeStamp& ts,
					   bool initall,
					   emcManageable::EStorage dataSource,
					   const char* sectors)
  : emcDataProcessorv2(runnumber,ts, initall, dataSource,sectors)
{
}

//_____________________________________________________________________________
emcDataProcessorRun4::~emcDataProcessorRun4()
{
}

//_____________________________________________________________________________
emcDCProcessor*
emcDataProcessorRun4::getDCProcessor(emcCalibrationDataHelper* cdh) const
{
  return new emcDCProcessorv3(cdh);
}

//_____________________________________________________________________________
emcPacketProcessor*
emcDataProcessorRun4::getPacketProcessor() const
{
  return new emcPacketProcessorv1;
}

//_____________________________________________________________________________
emcRawDataProcessor*
emcDataProcessorRun4::getRawDataProcessor(emcCalibrationDataHelper* cdh) const
{
  return new emcRawDataProcessorv3(cdh);
}

