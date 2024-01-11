#ifndef EmcCalibratorTOFObj_HH
#define EmcCalibratorTOFObj_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TF2.h>
#include <TF3.h>
#include <TDirectory.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include "EmcCalibrator.hh"
#include "CalibRuns.hh"
#include "CalibObj.hh"
#include "EmcCalibratorTOF.hh"
#include "TH2Xch.hh"
#include "fitpeak.hh"
#include "FitPeak2D.hh"

class EmcCalibratorTOFObj : public EmcCalibratorTOF {
public:
  TH2Xch* _th2xch;
  TH2Xch* _th2xch_afttwr;
protected:
  bool _initialized;
  bool Initialize(int nch,int xnbins,float xlow,float xup,int ynbins,float ylow,float yup);
public:
  EmcCalibratorTOFObj();
  EmcCalibratorTOFObj(const char* name, const char* title);
  EmcCalibratorTOFObj(const char* name, const char* title,int nch,
		      int xnbins,float xlow,float xup,int ynbins,float ylow,float yup);
  ~EmcCalibratorTOFObj();

  // Analysis method.
  virtual int AnalyzeCalibObj(CalibObj* calibobj,char* opt="");
  virtual int Reset(char* option="");
  //void Print(Option_t* option="") const;

  // Data Accessor
  ClassDef(EmcCalibratorTOFObj,1)
};
//
#endif
//
