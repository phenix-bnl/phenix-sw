#ifndef __MEMCCALIBRATORMODULEV2_H__
#define __MEMCCALIBRATORMODULEV2_H__

#include <SubsysReco.h>
#ifndef __EMCMANAGEABLE_H__
#  include "emcManageable.h"
#endif
#ifndef __EMCDATASTORAGEMAP_H__
#  include "emcDataStorageMap.h"
#endif
#include <string>
#include <vector>

class emcDataProcessor;
class PHTimeStamp;
class emcBadModules;
class emcTowerContainer;

/** Calibrator module : converts packets to emcTowerContainer. 

This is essentially an adapter class. The real worker is the underlying emcDataProcessor class.

@ingroup calibration
*/

class mEmcCalibratorModulev2 : public SubsysReco
{
public:

  /**@ctor
     @param runnumber
     @param ts 
     @param constantGains : whether the emcal gains are evolved during the run
     or not (default = false, meaning we take full advantage of the tracing of
     the gain evolutions)
     @param source : data source(s) for the calibration data (default=Postgres)
     For expert only you can let the data sources be different for 
     different calibration data flavours.
     @param sectors (expert only!) : which part of emcal are to be calibrated,
     can be emcal, pbsc, pbgl, or any sector names combination, e.g. W0E2    
   */
  mEmcCalibratorModulev2
  (int runnumber, 
   const PHTimeStamp& ts, 
   bool constantGains=false,
   const emcDataStorageMap& source = emcDataStorageMap(),
   const char* sectors="emcal");

  virtual ~mEmcCalibratorModulev2();
   
  int process_event(PHCompositeNode*);

 private:
  void changeToF(emcTowerContainer&);

private:
  emcDataProcessor* fDataProcessor;
  PHTimeStamp* fTimeStamp;
  emcBadModules* fBadModules;
  bool fConstantGains;
  int fRunNumber;
  std::string fSectors;
  std::vector<float> fTofSectorOffsets;
};

#endif
