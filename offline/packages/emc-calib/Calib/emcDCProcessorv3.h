#ifndef __EMCDCPROCESSORV3_H__
#define __EMCDCPROCESSORV3_H__

#ifndef __EMCDCPROCESSOR_H__
#include "emcDCProcessor.h"
#endif
#include <string>
#include <vector>
#include <set>

class emcTowerContent;
class emcCalibrationDataHelper;

/** Implementation of emcDCProcessor for Run4.
*/

class emcDCProcessorv3 : public emcDCProcessor
{
public:
  emcDCProcessorv3(emcCalibrationDataHelper*);

  virtual ~emcDCProcessorv3();

  bool calibrate(emcTowerContainer* pbsc, 
		 emcTowerContainer* pbgl,
		 time_t incrTime);

  int isValid() const;

  void identify(std::ostream& out = std::cout) const;

  void Reset();

private:

  typedef float (emcDCProcessorv3::*FPTR)(emcTowerContent*,time_t);

  float calibrateEnergyPbGl(emcTowerContent* t, time_t);
  float calibrateEnergyPbSc(emcTowerContent* t, time_t incrTime);
  float calibrateTimePbGl(emcTowerContent* t, time_t);
  float calibrateTimePbSc(emcTowerContent* t,time_t incrTime);

  static float Log(int adc);
  static std::vector<float> LogInit(void);

private:
  emcCalibrationDataHelper* fCH;
  bool fZeroSuppression;

  std::set<int> fNormProblems;

  static const int fgADCThreshold = 10;
  float fgNormtLimitPbSc;

  std::string fksGainsBLR;

  ClassDef(emcDCProcessorv3,1)
};

#endif
