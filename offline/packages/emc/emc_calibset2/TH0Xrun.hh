#ifndef TH0Xrun_HH
#define TH0Xrun_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include <TCanvas.h>
#include "TNamedDir.hh"

class TH0Xrun : public TNamedDir {
private:
public:
  TGraphErrors _gra;
  float _f_all;
public:
  TH0Xrun();
  TH0Xrun(const char* name, const char* title);
  ~TH0Xrun();
  bool Initialize(const char* name, const char* title);
  
  // Operator
  TH0Xrun& operator=(TH0Xrun&);

  // Basic method
  bool Set(int run,float value,float err=0,char* opt="");
  bool Set(TGraph* gra,char* opt="");
  bool SetError(TGraph* gra,char* opt="");
  bool Exist(int run);
  float Get(int run);
  float GetError(int run);
  bool Get(int run,float& run_shift,float& run_shift_err,char* opt="");
  bool FindNear(int run,float& run_shift,float& run_shift_err,int limit=100,char* opt="");
  int GetN(){return _gra.GetN(); };
  double* GetRun(){return _gra.GetX();};
  double* GetValue(){return _gra.GetY();};
  double* GetValueError(){return _gra.GetEY();};
  void Reset(char* opt="");

  ClassDef(TH0Xrun,1) //0 dimention x channel for Calibration.
};
//
#endif
//
