#ifndef EmcCalibratorMIP_HH
#define EmcCalibratorMIP_HH

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
#include "EmcCalibrator.hh"
#include "CalibRunsTH1.hh"
#include "TH0Xrun.hh"
#include "TH0Xch.hh"
#include "TH1Xch.hh"
#include "fitpeak.hh"
#define FITPEAK_DOWNSIGMA 2
#define FITPEAK_UPSIGMA 0.7

class EmcCalibratorMIP : public EmcCalibrator {
public:
  TH0Xrun* _th0xrun;   // MIP value vs run
  TH1Xch* _th1xch;     // ADC vs tower
  TH0Xch* _th0xch;     // MIP value vs tower
  TH1F* _h_twr_slope;
  //TH1F* _h_twr_slope_fit[3];
  //TGraphErrors* _gra_slope_run;
  //TGraphErrors* _gra_run;
protected:
  float _fit_sigma[2];
private:
  bool initialized;
  void Initialize(int nch,int nbins,Axis_t low,Axis_t up);
public:
  EmcCalibratorMIP();
  EmcCalibratorMIP(const char* name, const char* title);
  EmcCalibratorMIP(const char* name, const char* title,int nch,int nbins,Axis_t low,Axis_t up);
  ~EmcCalibratorMIP();

  // Analysis method.
  virtual int AnalyzeCalibObj(CalibObj* claibobj,char* opt="") ;
  virtual int AnalyzeTwr(char* opt="");
  virtual int Reset(char* option="");
  int AnalyzeSlope(char* opt="",float fit_low=0.03,float fit_high=0.3);
  //  int Add(EmcCalibratorMIP* emctof);
  //  void Print(Option_t* option="") const;

  // Data Accessor
  void SetFitpeakSigma(float down,float up){_fit_sigma[0]=down;_fit_sigma[1]=up; };
  virtual bool WriteFile(char* run_file, char* twr_file );
  bool WriteFileTwr(char* twr_file );
  bool WriteFileRun(char* run_file );

  ClassDef(EmcCalibratorMIP,1) //EMC Calibrator for MIP analysis
};
//
#endif
//
