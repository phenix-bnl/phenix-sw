#ifndef __EMCRAWDATAPROCESSORV3_H__
#define __EMCRAWDATAPROCESSORV3_H__

#ifndef __EMCRAWDATAPROCESSOR_H__
#include "emcRawDataProcessor.h"
#endif

class emcTowerContent;
class emcCalibrationDataHelper;

/** Implementation of emcRawDataProcessor for Run4. 
@ingroup calibration
 */

class emcRawDataProcessorv3 : public emcRawDataProcessor
{
public:
  emcRawDataProcessorv3(emcCalibrationDataHelper*);
  virtual ~emcRawDataProcessorv3();

  int isValid() const;

  void identify(std::ostream& out = std::cout) const;

  void Reset();

  bool toADCandTDC(emcTowerContainer* pbsc, 
		   emcTowerContainer* pbgl,
		   const emcBadModules&);
  
private:
  typedef bool (emcRawDataProcessorv3::*FPTR)(emcTowerContent*,float&);
  bool chooseLowGainPbSc(emcTowerContent* tower, float& scale);
  bool chooseLowGainPbGl(emcTowerContent* tower, float& scale);
  void toADCandTDC(emcTowerContent* tower, FPTR function_ptr,
		   const emcBadModules&);

private:
  emcCalibrationDataHelper* fCH;

  ClassDef(emcRawDataProcessorv3,0)
};

#endif
