#ifndef __PHENIX_ZDCCALIB_HH__
#define __PHENIX_ZDCCALIB_HH__

#include "Zdc.hh"
#include "ZdcReturncodes.h"

#include "ZdcCalibPar.hh"

#include "PdbPmtPeak.hh"
#include "PdbPmtFitPar.hh"
#include "PdbZdcLUT.hh"
#include "PHTimeStamp.h"

#include <TObject.h>
#include <TMath.h>

/**
 * Calibration classes to calculate calibrated values.
 *
 */
class ZdcCalib : public TObject {
public:
  ZdcCalib(); // Default
  virtual ~ZdcCalib() { }
 
  int restore(const char* filename);
  int restore(const char* filename, const char *type);
  int restore(const PHTimeStamp& time);
  int restore();

  void showParameters();

  float getCharge(int PmtIndx, int ADC);
  float getHitTime0(int PmtIndx, int TDC, int ADC);
  float getHitTime1(int PmtIndx, int TDC, int ADC);
  float getAdc(int PmtIndx, int ADC);

  ZdcCalibPar<PdbPmtPeak>*   getPedestal() {return &cpedestal;}
  ZdcCalibPar<PdbPmtPeak>*   getOverflow0() {return &coverflow0;}
  ZdcCalibPar<PdbPmtPeak>*   getOverflow1() {return &coverflow1;}
  ZdcCalibPar<PdbPmtPeak>*   getPmtGain() {return &cpmtgain;}
  ZdcCalibPar<PdbPmtFitPar>* getAdcGain() {return &cadcgain;}
  ZdcCalibPar<PdbPmtFitPar>* getTdcGain0() {return &ctdcgain0;}
  ZdcCalibPar<PdbPmtFitPar>* getTdcGain1() {return &ctdcgain1;}
  ZdcCalibPar<PdbPmtFitPar>* getSlewing0() {return &cslewing0;}
  ZdcCalibPar<PdbPmtFitPar>* getSlewing1() {return &cslewing1;}
  ZdcCalibPar<PdbPmtPeak>*   getTdc0LUT() {return &ctdc0lut;}
  ZdcCalibPar<PdbPmtPeak>*   getTdc1LUT() {return &ctdc1lut;}
  ZdcCalibPar<PdbPmtPeak>*   getTzero() {return &ctzero;}
  ZdcCalibPar<PdbPmtPeak>*   getZvtx() {return &czvtx;}
  ZdcCalibPar<PdbPmtPeak>*   getSmdOffset() {return &csmdoffset;}
  
  float zdc_correction(const float a) const;

private:

  ZdcCalibPar<PdbPmtPeak>   cpedestal;
  ZdcCalibPar<PdbPmtPeak>   coverflow0;
  ZdcCalibPar<PdbPmtPeak>   coverflow1;
  ZdcCalibPar<PdbPmtPeak>   cpmtgain;
  ZdcCalibPar<PdbPmtFitPar> cadcgain;
  ZdcCalibPar<PdbPmtFitPar> ctdcgain0;
  ZdcCalibPar<PdbPmtFitPar> ctdcgain1;
  ZdcCalibPar<PdbPmtFitPar> cslewing0;
  ZdcCalibPar<PdbPmtFitPar> cslewing1;
  ZdcCalibPar<PdbPmtPeak>   ctdc0lut;
  ZdcCalibPar<PdbPmtPeak>   ctdc1lut;
  ZdcCalibPar<PdbPmtPeak>   ctzero;
  ZdcCalibPar<PdbPmtPeak>   czvtx;
  ZdcCalibPar<PdbPmtPeak>   csmdoffset;

  ClassDef(ZdcCalib,0)
};

#endif  /* __PHENIX_ZDCCALIB_HH__ */
