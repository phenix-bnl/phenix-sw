#ifndef EmcCalibratorQA_HH
#define EmcCalibratorQA_HH

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
#include "TH0Xch.hh"
#include "fitpeak.hh"

#define DEFAULT_SIGMA 3
#define DEFAULT_NBIN 100

#define QABIT_TOTENE  0x0001
#define QABIT_TOTEVN  0x0002
#define QABIT_MEANE   0x0004
#define QABIT_MIPSM   0x0010
#define QABIT_MIPTWR  0x0020
#define QABIT_TOFT0   0x0100
#define QABIT_TOFLC   0x0200
#define QABIT_TOFSLEW 0x0400
#define QABIT_PIDMIP  0x1000

class EmcCalibratorQA : public EmcCalibrator {
public:
  TH0Xch _th0xch;
  TH1F _h_proj;
  float _cut[2];
protected:
  float _sigma;
  int _err_nch;
  int _nbin;
  bool _initialized;
  bool Initialize(int nch);
public:
  EmcCalibratorQA();
  EmcCalibratorQA(const char* name, const char* title);
  EmcCalibratorQA(const char* name, const char* title,int nch);
  ~EmcCalibratorQA();

  // Analysis method.
  virtual int AnalyzeCalibObj(CalibObj* qa,char* opt="");
  virtual int AnalyzeTwr(char* opt="");
  virtual int Reset(char* option="");
  int Analyze(TGraph* gra,char* opt="");
  int Analyze(TH1* h1,char* opt="");

  //  EmcCalibratorQA operator +(const EmcCalibratorQA& qa);
  //  EmcCalibratorQA operator =(const EmcCalibratorQA& qa);

  // Accessor to the results
  int SetCut(float min,float max);
  int GetErrorNch(){return _err_nch;};
  virtual void Print(Option_t* option="") const;
  virtual void Draw(Option_t* option="");
  
  // Accessor to the option
  void SetOptSigma(float sigma){_sigma = sigma;};
  float GetOptSigma(){return _sigma;};
  void SetOptNbin(int nbin){_nbin = nbin;};
  int GetOptNbin(){return _nbin;};
  virtual bool WriteFile(char* calibfile,int isect);

  ClassDef(EmcCalibratorQA,1) //EMC Calibrator for TOF analysis
};
//
#endif
//
