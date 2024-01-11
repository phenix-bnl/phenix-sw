#ifndef __PBGLSUPERMODULE_H__
#define __PBGLSUPERMODULE_H__

#ifndef __EMCSTATICDATA_H__
#include "EmcStaticData.h"
#endif
#ifndef __PBGLCALIBRATIONDATA_H__
#include "PbGlCalibrationData.h"
#endif
#ifndef __EMCSUPERMODULE_H__
#include "EmcSuperModule.h"
#endif

/** PbGl implementation of EmcSuperModule.
    PbGl 24-tower wide supermodule decription. 
*/
class PbGlSuperModule: public EmcSuperModule
{
public:
  int SMId;
  int SectorId;
  struct {
    struct {
      int ProductionNumber;
      int SMNumber;
      int ADCBoard; 
      float DecayParam;
      struct {
      int LgcNumber;
      int HVSetting;
      int HVActual;
      float GainAY;
      float GainVY;
      float GainBL;
      float GainRS;
      float GainU0;
      float GainUT;
      float RatioHighLowGain;
      int ADCNumber;
      int Row;
      int Col;
      float PedHigh;
      float PedLow;
      int BadFlag;
      float AYPeak;
      float AYRef;
      float TestPeak;
      float TestRef;
      float GC;
      float C0;
      float G0;
      float CF;
      } Tower[24];
    } Cern; 
     struct {
      int AddressGroup; 
      float DecayParam;
      struct {
      int TowerId;
      int Raw;
      int Column;       
      char Modul[5];   
      int HVBase;  
      char HVAddressBin[10];
      int HVSetting;
      int HVActual;
      float GainAY;
      float GainVY;
      float GainBL;
      float GainRS;
      float GainU0;
      float GainUT;
      float RatioHighLowGain;
      float PeakAY;
      float RmsAY;
      float PeakVY;
      float RmsVY;
      float PeakBL;
      float RmsBL;
      float PinAY;
      float PinVY;
      float PinBL;
      float PedHigh;
      float PedLow;
      int BadFlag;
      } Tower[24];
    } Bnl; 
  } Data;


  PbGlSuperModule(int &, int &);
  ~PbGlSuperModule();

	/** returns ProductionId. Returns 0. Only for compability reasons.
			Will be changed soo. */
	int   getProductionId(){return 0;}
	/** returns LightYield. Returns 0. Only for compability reasons.
			Will be changed soo. */  
  float getScrLightYield(int & ){return 0.;}
	/** returns Muon Peak. Returns 0. Only for compability reasons.
			Will be changed soo. */ 
  float getMuPeak(int & ){return 0.;}
	/** returns Laser raw data. Returns 0. Only for compability reasons.
 		Will be changed soo. */ 
  float getLaserRaw(int & ){return 0.;}
	/** returns nothing. Returns 0. Only for compability reasons.
 		Will be changed soo. */ 
  float getIntSPD(){return 0.;}
	/** returns nothing. Returns 0. Only for compability reasons.
 		Will be changed soo. */ 
  float getIntSPDTP( ){return 0.;}
	/// Load static data for one tower belonging to 24 towers wide supermodulethe supermodule
	void   LoadTowerData(int, float *, char *, char *);
	/// Gets U0 (time independent geain factor)
  float getU0(int & Twr) {return Data.Cern.Tower[Twr].GainU0;}
	/// Gets UT (time dependent geain factor)
  float getUT(int & Twr) {return Data.Cern.Tower[Twr].GainUT;} 
  int   getLgcNumber(int & Twr) {return Data.Cern.Tower[Twr].LgcNumber;} 
  float getAY(int & Twr) {return Data.Cern.Tower[Twr].GainAY;} 
  float getVY(int & Twr) {return Data.Cern.Tower[Twr].GainVY;} 
  float getBL(int & Twr) {return Data.Cern.Tower[Twr].GainBL;} 
  float getRS(int & Twr) {return Data.Cern.Tower[Twr].GainRS;} 
  float getAYPeak(int & Twr) {return Data.Cern.Tower[Twr].AYPeak;} 
  float getAYRef(int & Twr) {return Data.Cern.Tower[Twr].AYRef;} 
  float getTestPeak(int & Twr) {return Data.Cern.Tower[Twr].TestPeak;} 
  float getTestRef(int & Twr) {return Data.Cern.Tower[Twr].TestRef;} 
  float getGC(int & Twr) {return Data.Cern.Tower[Twr].GC;}
  float getC0(int & Twr) {return Data.Cern.Tower[Twr].C0;}
  float getG0(int & Twr) {return Data.Cern.Tower[Twr].G0;}
  float getCF(int & Twr) {return Data.Cern.Tower[Twr].CF;}
 
  int   HVActualCern(int iSM24T){return Data.Cern.Tower[iSM24T].HVActual;}
  int   HVActualBnl (int iSM24T){return Data.Bnl.Tower[iSM24T].HVActual;}

};

#endif





