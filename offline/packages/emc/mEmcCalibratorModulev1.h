#ifndef __mEmcCalibratorModulev1_h__
#define __mEmcCalibratorModulev1_h__

#include <SubsysReco.h>
#include "emcManageable.h"
#include <string>

class emcDataProcessor;
class PHTimeStamp;
class emcBadModules;

/** Calibrator module : converts packets to emcTowerContainer. 

This is essentially an adapter class. The real worker is the underlying emcDataProcessor class.

*/

class mEmcCalibratorModulev1 : public SubsysReco
{
public:
  mEmcCalibratorModulev1(int runnumber, const PHTimeStamp&, 
			 bool constantGains,
			 emcManageable::EStorage source,
			 const char* sectors);

  virtual ~mEmcCalibratorModulev1();
   
  int process_event(PHCompositeNode*);

private:
  emcDataProcessor* fDataProcessor;
  PHTimeStamp* fTimeStamp;
  emcBadModules* fBadModules;
  bool fConstantGains;
  int fRunNumber;
  std::string fSectors;
};

#endif
