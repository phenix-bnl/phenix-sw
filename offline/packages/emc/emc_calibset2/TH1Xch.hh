#ifndef TH1Xch_HH
#define TH1Xch_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TCanvas.h>
#include "CalibObj.hh"

class TH1Xch : public CalibObj {
private:
  int _nbins;
  float _low;
  float _up;
  float _width;
  bool _initialized;
public:
  TH2F* _h2_ch;               //->
  TH1F* _h_all;               //->
public:
  TH1Xch();
  TH1Xch(const char* name, const char* title);
  TH1Xch(const char* name, const char* title,
	    int nch,int nbins,float low,float up);
  ~TH1Xch();

  // Basic method
  virtual int Fill(int ch,float x,float w = 1.,float ww=1.);
  virtual void Reset(char* option="");
  virtual int Add(CalibObj* obj,Double_t c1 = 1 , char* opt = "");
  virtual void Scale(Double_t c1 = 1);
  int Sumup(TH1Xch* th1,Double_t shift = 0,char* opt = "" );
  int Sumup(TH1Xch* th1,TH1Xch* ch2,
	    Double_t shift1 = 1,Double_t shift2 = 1,char* opt = "" );
  int Sumup(TH1Xch* th1,TH1* hshift1,char* opt = "" );
  int Sumup(TH1Xch* th1,TH1Xch* th2,TH1* hshift1,TH1* hshift2,char* opt = "" );
  int Sumup(TH1Xch* th1,TGraph* gshift1,char* opt = "" );
  int Sumup(TH1Xch* th1,TH1Xch* th2,TGraph* gshift1,TGraph* gshift2,char* opt = "" );

  // Accessor to the data
  TH1* GetHist(){return _h_all;};
  TH2* GetHist2D(){return _h2_ch; };
  TH1* CreateHistCh(int ch);

  // Accessor to the data
  int Project();
  int GetNbins(){return _nbins;};
  float GetLow(){return _low;};
  float GetUp(){return _up;};
  float GetWidth(){return _width;};

  ClassDef(TH1Xch,1) //1 dimention x channel for Calibration.
};
//
#endif
//
