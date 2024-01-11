#ifndef __EMCDCPROCESSORV2_H__
#define __EMCDCPROCESSORV2_H__

#ifndef __EMCDCPROCESSOR_H__
#include "emcDCProcessor.h"
#endif
#include <vector>

class emcTowerContent;
class emcCalibrationDataHelper;

/** Implementation of emcDCProcessor for Run3.
@ingroup calibration
*/

class emcDCProcessorv2 : public emcDCProcessor
{
public:
  emcDCProcessorv2(emcCalibrationDataHelper*);

  virtual ~emcDCProcessorv2();

  bool calibrate(emcTowerContainer* pbsc, 
		 emcTowerContainer* pbgl,
		 time_t incrTime);

  int isValid() const;

  void identify(std::ostream& out = std::cout) const;

  void Reset();

private:

  typedef float (emcDCProcessorv2::*FPTR)(emcTowerContent*,time_t);

  float calibrateEnergyPbGl(emcTowerContent* t, time_t);
  float calibrateEnergyPbSc(emcTowerContent* t, time_t incrTime);
  float calibrateTimePbGl(emcTowerContent* t, time_t);
  float calibrateTimePbSc(emcTowerContent* t,time_t incrTime);

  static float Log(int adc);
  static std::vector<float> LogInit(void);

private:
  emcCalibrationDataHelper* fCH;
  bool fZeroSuppression;

  static const int fgADCThreshold = 10;

  ClassDef(emcDCProcessorv2,1)
};

#endif
