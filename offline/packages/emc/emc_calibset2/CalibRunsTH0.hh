#ifndef CalibRunsTH0_HH
#define CalibRunsTH0_HH

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
#include "CalibObj.hh"
#include "CalibRuns.hh"
#include "TH0Xch.hh"

class CalibRunsTH0 : public CalibRuns {
protected:
  virtual int Initialize();  // User must implement
public:
  CalibRunsTH0();
  CalibRunsTH0(const char* name, const char* title,int nch);
  ~CalibRunsTH0();

  ClassDef(CalibRunsTH0,1) //0-dimention of runs collection for calibration
};
//
#endif
//
