#ifndef EmcCalibratorTOF_HH
#define EmcCalibratorTOF_HH

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
#include "CalibObj.hh"
#include "TH0Xrun.hh"
#include "TH1Xch.hh"
#include "fitpeak.hh"
#include "EmcCalibratorQA.hh"
#define FITPEAK_DOWNSIGMA 2
#define FITPEAK_UPSIGMA 0.7

class EmcCalibratorTOF : public EmcCalibrator {
public:
  TH0Xrun _th0xrun_shift;
  TH0Xrun _th0xrun_fit[4];
  TH1F* _h_twr_pshift;          //-> T0 preshift
  TH1F* _h_twr_shift;           //-> T0 shift
  TH1F* _h_twr_fit[4];          //-> T0 fitting
  TH1Xch* _th1xch;              //-> T0 hists 
  TH1Xch* _th1xch_afttwr;       //-> T0 hists
protected:
  float _fit_sigma[2];
private:
  int _nbins;
  float _low;
  float _up;
  bool _initialized;
  void Initialize(int nch,int nbins,float low,float up);
public:
  EmcCalibratorTOF();
  EmcCalibratorTOF(const char* name, const char* title);
  EmcCalibratorTOF(const char* name, const char* title,int nch,int nbins,Axis_t low,Axis_t up);
  ~EmcCalibratorTOF();

  // Analysis method.
  virtual int AnalyzeCalibObj(CalibObj* calibobj,char* opt="");
  virtual int AnalyzeTwr(char* opt="");
  virtual int Reset(char* option="");
  int Add(EmcCalibratorTOF* emctof);
  //void Print(Option_t* option="") const;

  // Data Accessor
  int SetEmcCalibratorTOF(EmcCalibratorTOF& calibtof);
  int SetEmcCalibratorTOFrun(EmcCalibratorTOF& calibtof);
  int SetPreshift();
  int SetPreshift(EmcCalibratorTOF& calibtof);
  void SetFitpeakSigma(float down,float up){_fit_sigma[0]=down;_fit_sigma[1]=up; };
  virtual bool WriteFile(char* t0_run_file,char* t0_twr_file);

  ClassDef(EmcCalibratorTOF,4) //EMC Calibrator for TOF analysis
};
//
#endif
//
