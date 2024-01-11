#ifndef TH0Xch_HH
#define TH0Xch_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include <TCanvas.h>
#include "CalibObj.hh"

class TH0Xch : public CalibObj {
private:
public:
  bool _initialized;
  float _f_all;     //
  TH1F* _h_all;     //->
public:
  TH0Xch();
  TH0Xch(const char* name, const char* title);
  TH0Xch(const char* name, const char* title,int nch);
  TH0Xch(const TH0Xch&);
  ~TH0Xch();
  bool Initialize(const char* name, const char* title,int nch);

  // Basic method
  virtual int Fill(int ch,float x,float w = 1.,float ww = 1.);
  virtual void Reset(char* option="");
  virtual int Add(CalibObj* obj,Double_t c1 = 1 , char* opt = "");
  virtual void Scale(Double_t c1 = 1);
  bool Set(int ch,float x,float ex);
  bool Set(TGraph* gra);
  bool Set(TH1* h1);
  bool SetError(TGraph* gra);
  bool SetError(TH1* h1);

  // operator
  TH0Xch& operator=(TH0Xch& th0xch);
  TH0Xch operator+(TH0Xch& th0xch);
  TH0Xch operator-(TH0Xch& th0xch);
  TH0Xch operator*(TH0Xch& th0xch);
  TH0Xch operator/(TH0Xch& th0xch);

  // Accessor to the data
  TH1* GetHist(){return _h_all;};

  ClassDef(TH0Xch,1) //0 dimention x channel for Calibration.
};
//
#endif
//
