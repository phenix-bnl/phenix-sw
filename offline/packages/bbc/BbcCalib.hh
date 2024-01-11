#ifndef __BBCCALIB_HH__
#define __BBCCALIB_HH__

#include "Bbc.hh"

#include "BbcCalibPar.hh"

#include "PdbPmtPeak.hh"
#include "PdbPmtFitPar.hh"
#include "PdbBbcConf.hh"
#include "PHTimeStamp.h"


/**
 * Calibration classes to calculate calibrated values.
 *
 * This class use some calibration parameters provided by calibraton
 * I/O classes (BbcCalibPedestal, BbcCalibOffset, BbcCalibPmtGain,
 * BbcCalibAdc, BbcCalibTdc0, BbcCalibTdc1, BbcCalibSlewingPar.
 *
 * @version August 8, 1999
 * @author Takeshi Kohama
 * @see BbcCalibBase
 *
 * Modification : Sep 10 2001
 * New empirical function [f(x)=a+(b/x)+c*log(x)] was adopted for
 * slewing correction. we asigned the function as function type 1
 * ,and previous function [f(x)=a+(b/sqrt(x))] was function type 0.
 * with respect to this modification, the version of calibration 
 * parameter should be 1000 to use type 1 function for slewing correction.
 * For type 0 function, should be 0.
 */

class BbcCalib
{
public:
  BbcCalib(int version=1000); // Default
 
  int restore(const char* filename);
  int restore(const PHTimeStamp& time);
  int restore();

  int restore(const char* filename, int version);
  int restore(const PHTimeStamp& time, int version);

  void showParameters();
  void showParameterVersion();
  void showConfig();
  void WriteLvl2calibs();

  float getCharge(int PmtIndx, int ADC);
  float getHitTime0(int PmtIndx, int TDC, int ADC);
  float getHitTime1(int PmtIndx, int TDC, int ADC);
  float getAdc(int PmtIndx, int ADC);
  float getArmHitTime(Bbc::ArmType arm, float ArmHitTimeOrg );
  float getCorrectedTzero( float TimingSouth, float TimingNorth );

  int   getPmtNumber( int femid, int femch );
  int   getPmtIDinPISA( int pmtid );

  void  setCutVal(int cut=4) { CutVal = cut; } 
  int   getCutVal()          { return CutVal; } 

  void  setCalibrationVersion(int version=1000) { CalibrationVersion=version; }
  int   getCalibrationVersion() { return CalibrationVersion; }

  void  setSimulation(int flag=1) { Simulation=flag; }
  int   getSimulation() { return Simulation; }
  void  getSimuInfo();

  void  setFunctionType(int type=0);
  int   getFunctionType() {return fittype;}

  void  setVersionAll(int version=0);

  void  setVersionForPedestal(int ver) {Version[0] = ver;}
  void  setVersionForOverflow0(int ver){Version[1] = ver;}
  void  setVersionForOverflow1(int ver){Version[2] = ver;}
  void  setVersionForPmtGain(int ver)  {Version[3] = ver;}
  void  setVersionForOffset(int ver)   {Version[4] = ver;}
  void  setVersionForTZeroOff(int ver) {Version[5] = ver;}
  void  setVersionForAdcGain(int ver)  {Version[6] = ver;}
  void  setVersionForTdcGain0(int ver) {Version[7] = ver;}
  void  setVersionForTdcGain1(int ver) {Version[8] = ver;}
  void  setVersionForSlewing0(int ver) {Version[9] = ver;}
  void  setVersionForSlewing1(int ver) {Version[10]= ver;}
  void  setVersionForConfig(int ver)   {Version[11]= ver;}
  void  setVersionForFakePed(int ver)  {Version[12]= ver;}
  void  setVersionForThreshold(int ver){Version[13]= ver;}
  void  setVersionForTimeReso(int ver) {Version[14]= ver;}
 
  int   getVersionForPedestal() {return Version[0];}
  int   getVersionForOverflow0(){return Version[1];}
  int   getVersionForOverflow1(){return Version[2];}
  int   getVersionForPmtGain()  {return Version[3];}
  int   getVersionForOffset()   {return Version[4];}
  int   getVersionForTZeroOff() {return Version[5];}
  int   getVersionForAdcGain()  {return Version[6];}
  int   getVersionForTdcGain0() {return Version[7];}
  int   getVersionForTdcGain1() {return Version[8];}
  int   getVersionForSlewing0() {return Version[9];}
  int   getVersionForSlewing1() {return Version[10];}
  int   getVersionForConfig()   {return Version[11];}
  int   getVersionForFakePed()  {return Version[12];}
  int   getVersionForThreshold(){return Version[13];}
  int   getVersionForTimeReso() {return Version[14];}
 
  BbcCalibPar<PdbPmtPeak>*   getPedestal() {return &cpedestal;}
  BbcCalibPar<PdbPmtPeak>*   getOverflow0(){return &coverflow0;}
  BbcCalibPar<PdbPmtPeak>*   getOverflow1(){return &coverflow1;}
  BbcCalibPar<PdbPmtPeak>*   getPmtGain()  {return &cpmtgain;}
  BbcCalibPar<PdbPmtPeak>*   getOffset()   {return &coffset;}
  BbcCalibPar<PdbPmtPeak>*   getTZeroOff() {return &ctzerooff;}
  BbcCalibPar<PdbPmtFitPar>* getAdcGain()  {return &cadcgain;}
  BbcCalibPar<PdbPmtFitPar>* getTdcGain0() {return &ctdcgain0;}
  BbcCalibPar<PdbPmtFitPar>* getTdcGain1() {return &ctdcgain1;}
  BbcCalibPar<PdbPmtFitPar>* getSlewing0() {return &cslewing0;}
  BbcCalibPar<PdbPmtFitPar>* getSlewing1() {return &cslewing1;}
  BbcCalibPar<PdbBbcConf>*   getConfig()   {return &config;}
  BbcCalibPar<PdbPmtPeak>*   getFakePed()  {return &cfakeped;}
  BbcCalibPar<PdbPmtPeak>*   getThreshold(){return &cthreshold;}
  BbcCalibPar<PdbPmtPeak>*   getTimeReso() {return &ctimereso;}
 
private:

  //================================================================
  // Calibration parameters
  //================================================================
  BbcCalibPar<PdbPmtPeak>   cpedestal;
  BbcCalibPar<PdbPmtPeak>   coverflow0;
  BbcCalibPar<PdbPmtPeak>   coverflow1;
  BbcCalibPar<PdbPmtPeak>   cpmtgain;
  BbcCalibPar<PdbPmtFitPar> cadcgain;
  BbcCalibPar<PdbPmtFitPar> ctdcgain0;
  BbcCalibPar<PdbPmtFitPar> ctdcgain1;
  BbcCalibPar<PdbPmtFitPar> cslewing0;
  BbcCalibPar<PdbPmtFitPar> cslewing1;
  BbcCalibPar<PdbBbcConf>   config;
  BbcCalibPar<PdbPmtPeak>   coffset;
  BbcCalibPar<PdbPmtPeak>   ctzerooff;
  BbcCalibPar<PdbPmtPeak>   cfakeped;
  BbcCalibPar<PdbPmtPeak>   cthreshold;
  BbcCalibPar<PdbPmtPeak>   ctimereso;

  int CutVal;
  int CalibrationVersion;
  int Version[15];
  int Simulation;

  int fittype;  // 0: base (f(x)=a+b/sqrt(x)), 1: extend: (f(x)=a+b/x+cln(x))

};

#endif /* __BBCCALIB_HH__ */
