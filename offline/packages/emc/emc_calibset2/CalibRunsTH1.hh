#ifndef CalibRunsTH1_HH
#define CalibRunsTH1_HH

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
#include <TMap.h>
#include "CalibRuns.hh"
#include "TH1Xch.hh"
#include "fitpeak.hh"

#define DEFAULT_OPT_BUFFERSIZE 300
#define DEFAULT_OPT_BINWIDTH 0.2
#define DEFAULT_OPT_WINDOW 10
#define DEFAULT_OPT_NBIN 300
#define DEFAULT_OPT_PREFIT 0 //CalibRunsTH1::kTOF // FIT_ME
#define DEFAULT_OPT_FIXCENT 0

class CalibRunsTH1 : public CalibRuns {
public:
  enum tPrefit { kPrefit_None, kPrefit_TOF, kPrefit_MIP, kPrefit_FIX, kPrefit_FIXCENT };
private:
  // -- option ---
  int _opt_nbin;
  float _opt_window;
  float _opt_binwidth;
  tPrefit _opt_prefit;  // Fitting process for TOF calibration  
  float _opt_fixcent;
  // -- Parameter for TH1Xch
  float _th1_low;
  float _th1_high;
  float _th1_nbin;
  float _th1_binwidth;
protected:
  virtual int Initialize();  // User must implement
public:
  CalibRunsTH1();
  CalibRunsTH1(const char* name, const char* title,int nch);
  CalibRunsTH1(const char* name, const char* title,int nch,int xnbins,float xlow,float xup);
  ~CalibRunsTH1();

  // --- Accessor to the option
  void SetOptNbin(int nbin){_opt_nbin = nbin;};
  void SetOptBinwidth(float binwidth){_opt_binwidth = binwidth;};
  void SetOptWindow(float window){_opt_window = window; };
  void SetOptPrefit(tPrefit prefit){_opt_prefit = prefit; };
  void SetOptFixcent(float fixcent){_opt_fixcent = fixcent; };
  int GetOptNbin(){return _opt_nbin;};
  float GetOptBinwidth(){return _opt_binwidth;};
  float GetOptWindow(){return _opt_window;};
  tPrefit GetOptPrefit(){return _opt_prefit;};
  float GetOptFixcent(){return _opt_fixcent;};

  ClassDef(CalibRunsTH1,1) //1-dimention of runs collection for calibration
};
//
#endif
//
