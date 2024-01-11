#ifndef EmcCalibratorTOFPed_HH
#define EmcCalibratorTOFPed_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TDirectory.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include "CalibObj.hh"
#include "TH1Xch.hh"
#include "CalibRunsTH1.hh"
#include "EmcCalibrator.hh"
//#include "fitpeak.hh"


class EmcCalibratorTOFPed : public EmcCalibrator {
private:
  bool _initialized;
public:
  CalibRunsTH1* _calibruns;         //->
  TClonesArray* _twr_array;         //->
public:
  EmcCalibratorTOFPed();
  EmcCalibratorTOFPed(const char* name, const char* title,int nch);
  ~EmcCalibratorTOFPed();

  // Analysis method.
  virtual int AnalyzeCalibObj(CalibObj* calibobj,char* opt="");
  virtual int AnalyzeTwr(char* opt="");
  virtual int Reset(char* option="");
  //  int Add(EmcCalibratorTOFPed* emctof);
  //void Print(Option_t* option="") const;

  // Data Accessor

  ClassDef(EmcCalibratorTOFPed,1) //EMC Calibrator for TOF pedestal analysis
};
//
#endif
//
