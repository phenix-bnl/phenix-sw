#ifndef EmcCalibratorQArun_HH
#define EmcCalibratorQArun_HH

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
#include "EmcCalibratorQA.hh"
#include "TH0Xrun.hh"
#include "fitpeak.hh"

#define DEFAULT_SIGMA 3
#define DEFAULT_NBIN 100

class EmcCalibratorQArun : public EmcCalibratorQA {
public:
  TH0Xrun _th0xrun;
public:
  EmcCalibratorQArun();
  EmcCalibratorQArun(const char* name, const char* title);
  ~EmcCalibratorQArun();
  bool Initialize();

  // Analysis method.
  virtual int AnalyzeCalibObj(CalibObj* qa,char* opt="");
  virtual int AnalyzeTwr(char* opt="");
  virtual int Reset(char* option="");
  int AnalyzeRun(char* opt="");
  int AnalyzeRun(TGraph* gra,char* opt="");
  
  // Accessor to the option
  virtual void Print(Option_t* option="") const;
  virtual void Draw(Option_t* option="");

  ClassDef(EmcCalibratorQArun,1) //EMC Calibrator for TOF analysis
};
//
#endif
//
