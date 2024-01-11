#ifndef CalibRunsTH2_HH
#define CalibRunsTH2_HH

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
#include "TH2Xch.hh"
#include "fitpeak.hh"

class CalibRunsTH2 : public CalibRuns {
protected:
  int _nbins[2];
  float _low[2];
  float _high[2];
  virtual int Initialize();  // User must implement
public:
  CalibRunsTH2();
  CalibRunsTH2(const char* name, const char* title,int nch,
		  int xnbins,float xlow,float xhigh,int ynbins,float ylow,float yhigh);
  ~CalibRunsTH2();

  ClassDef(CalibRunsTH2,1) //2-dimention of runs collection for calibration
};
//
#endif
//
