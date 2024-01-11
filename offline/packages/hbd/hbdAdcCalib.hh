//--------------------------------------------------- 
// Class: hbdAdcCalib
// 
// Created by: Takao Sakaguchi
// 
// Description: HBD ADC calibration service class
// 
// created: 1/16/09
//--------------------------------------------------- 


#ifndef __HBDADCCALIB_HH__
#define __HBDADCCALIB_HH__

#include <phool.h>
#include <vector>
#include <map>
#include "Rtypes.h"
#include <PdbHbdModuleGain.hh>

class PHCompositeNode;
class PHTimeStamp;
class TRandom2;

class hbdAdcCalib{ 

public:
  hbdAdcCalib();                             // constructor
  virtual ~hbdAdcCalib();                    // destructor
  
public:

  // Data member access methods
  void set_Verbose(short);             // Set Verbose
  short get_Verbose();                 // Get Verbose
  void set_runnum(int _runnum) {runnum=_runnum;}

  short debug;
  //database code updated 3/6/2006
  PHBoolean fetch(PHTimeStamp& tInt);
  PHBoolean fetch(int run);
  PHBoolean fetchPT(PHTimeStamp& tInt);
  PHBoolean updatePedestal(PHTimeStamp& tStart, PHTimeStamp& tStop);
  PHBoolean updateGain(PHTimeStamp& tStart, PHTimeStamp& tStop);
  PHBoolean updateModuleGain(PHTimeStamp& tStart, PHTimeStamp& tStop);
  PHBoolean updatePT(PHTimeStamp& tStart, PHTimeStamp& tStop);
  PHBoolean isRunCalibrated(){return RunIsCalibrated;};

  void readCalibFromFile(const char *filename);

  void print();

  // Pad-to-Pad Base Gain constants
  void getGain(int ADCch, float& gain, float& gainerr);
  void setGain(int ADCch, float gain, float gainerr);

  // Module Gain constants
  void getModuleGain(int ADCch, float& gain, float& gainerr);
  void setModuleGain(int ADCch, float gain, float gainerr);

  // P Over T constants
  void getPTgain(int arm, float& ptgain, float& ptgainerr);
  void setPTgain(int arm, float ptgain, float ptgainerr);

  // Pedestal constants
  void getPedestal(int ADCch, float& ped, float& pederr);
  void setPedestal(int ADCch, float ped, float pederr);

  void ApplyCalib(int ADCch, int Adcval, float& charge);
  
  // Utility functions to split and join uints to ulongs and vice versa
  static ULong64_t joinTwoInts(unsigned int, unsigned int);
  static void splitULong64(ULong64_t, unsigned int &, unsigned int &);

  // Time dependent module gains
  PHBoolean setClockTick(unsigned int, unsigned int);
  void dumpOldModuleGainToFile(const char*, int);
  void readModuleGainsFromFileTwoIntsFormat(const char *);
  void readModuleGainsFromFileULongFormat(const char *);
  void addValidityStartClockTickAndModuleGain(unsigned int, unsigned int, std::vector<float>);
  void addValidityStartClockTickAndModuleGain(ULong64_t, std::vector<float>);
  PHBoolean updateTimeDepModuleGain(PHTimeStamp& tStart, PHTimeStamp& tStop);
  void printModuleGains();

private:

  // Verbosity level for this class
  short Verbose;
  int runnum;
  // The following pertain to default geometry generation
  // Flag of active sectors
  float GainConst[2304];
  float GainError[2304];
  float ModuleGain[24];
  float ModuleGainError[24];
  float PTConst[2];
  float PTError[2];
  float PedestalConst[2304];
  float PedestalError[2304];


  // Time dependent module gain containers
  std::map<ULong64_t,PdbHbdModuleGain>::const_iterator gainConstIter;
  std::map<ULong64_t,PdbHbdModuleGain> clkTickGainMap;
  PdbHbdModuleGain *currentModuleGain;
  ULong64_t runFirstClockTick;  // holder for the first clock tick of the run (set to the beginning clock tick of first range)
  ULong64_t validityRangeFirstClkTick; // holder for the begining clock tick of current calibration's validity range
  ULong64_t validityRangeLastClkTick; // holder for the last clock tick of current calibration's validity range

  void  ModuleToChannel();
  float ModuleGainByChan[2304];

  bool ApplyChanByChan;
  bool ApplyModByMod;
  bool ApplyTimeDepModByMod;
  bool RunIsCalibrated;

  TRandom2 *T;
}; 

#endif /* __HBDADCCALIB_HH__ */
