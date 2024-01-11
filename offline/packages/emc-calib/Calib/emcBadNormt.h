#ifndef __EMCBADNORMT_H__
#define __EMCBADNORMT_H__

#include <string>
#include <vector>
#include <iostream>

class TH2;
class emcCalibrationDataHelper;

/** Test class. 

Not for general user !

This class is a check related to the way we deal with energy calibration
 in emcDCProcessorv3::calibrateEnergyPbSc(). We hereby investigate the effect
 of the limit we put on the normt factor.

@ingroup calibration
*/

class emcBadNormt
{
 public:

  emcBadNormt(const char* outputdir="/tmp");

  void addRun(int runnumber)
  {
    fRuns.push_back(runnumber); 
  }

  void clearRun()
  {
    fRuns.clear();
    fNbad.clear();
  }

  void print(std::ostream& out = std::cout);

  void process(float normt_limit=47.84);

 private:
  void initGainBaseLine(emcCalibrationDataHelper&);

 private:

  std::vector<int> fRuns;
  std::vector<int> fNbad;
  std::vector<float> fGainBaseLineFactor;
  std::string fOutputDir;
  std::vector<TH2*> fHistos;
};
#endif
