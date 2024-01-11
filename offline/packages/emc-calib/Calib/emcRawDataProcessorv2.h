#ifndef __EMCRAWDATAPROCESSORV2_H___
#define __EMCRAWDATAPROCESSORV2_H___

#ifndef __EMCRAWDATAPROCESSOR_H__
#include "emcRawDataProcessor.h"
#endif

class emcTowerContent;
class emcCalibrationDataHelper;

/** Implementation of emcRawDataProcessor for Run3.
@ingroup calibration
 */

class emcRawDataProcessorv2 : public emcRawDataProcessor
{
public:
  emcRawDataProcessorv2(emcCalibrationDataHelper*);
  virtual ~emcRawDataProcessorv2();

  int isValid() const;

  void identify(std::ostream& out = std::cout) const;

  void Reset();

  bool toADCandTDC(emcTowerContainer* pbsc, 
		   emcTowerContainer* pbgl,
		   const emcBadModules&);
  
private:
  typedef bool (emcRawDataProcessorv2::*FPTR)(emcTowerContent*,float&);
  bool chooseLowGainPbSc(emcTowerContent* tower, float& scale);
  bool chooseLowGainPbGl(emcTowerContent* tower, float& scale);
  void toADCandTDC(emcTowerContent* tower, FPTR function_ptr,
		   const emcBadModules&);

private:
  emcCalibrationDataHelper* fCH;

  ClassDef(emcRawDataProcessorv2,0)
};

#endif
