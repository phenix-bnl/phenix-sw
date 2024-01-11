#ifndef TF1ch_HH
#define TF1ch_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include "CalibObj.hh"

class TF1ch : public TF1 {
private:
public:
  bool _initialized;
  float _f_all;     //
  TH1F* _h_all;     //->
public:
  TF1ch();
  TF1ch(const char* name, const char* title);
  TF1ch(const char* name, const char* title,int nch);
  TF1ch(const TF1ch&);
  ~TF1ch();
  bool Initialize(const char* name, const char* title,int nch);

  // Basic method
  virtual int Fill(int ch,float x,float w = 1);
  virtual void Reset(char* option="");
  virtual int Add(CalibObj* obj,Double_t c1 = 1 , char* opt = "");
  virtual void Scale(Double_t c1 = 1);
  bool Set(int ch,float x,float ex);
  bool Set(TGraph* gra);
  bool Set(TH1* h1);
  bool SetError(TGraph* gra);
  bool SetError(TH1* h1);

  // operator
  TF1ch& operator=(TF1ch& TF1ch);
  TF1ch operator+(TF1ch& TF1ch);
  TF1ch operator-(TF1ch& TF1ch);
  TF1ch operator*(TF1ch& TF1ch);
  TF1ch operator/(TF1ch& TF1ch);

  // Accessor to the data
  TH1* GetHist(){return _h_all;};

  ClassDef(TF1ch,1) //0 dimention x channel for Calibration.
};
//
#endif
//
