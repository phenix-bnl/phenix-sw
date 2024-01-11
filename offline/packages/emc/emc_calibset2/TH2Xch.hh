#ifndef TH2Xch_HH
#define TH2Xch_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TGraph.h>
#include <TCanvas.h>
#include "CalibObj.hh"

class TH2Xch : public CalibObj {
private:
  int _nbins[2];
  float _low[2];
  float _up[2];
  float _width[2];
public:
  TH3F* _h3_ch;             //->
  TH2F* _h_all;             //->
public:
  TH2Xch();
  TH2Xch(const char* name, const char* title);
  TH2Xch(const char* name, const char* title,
	    int nch,int xnbins,float xlow,float xup,int ynbins,float ylow,float yup);
  ~TH2Xch();

  // Basic method
  virtual int Fill(int ch,float x,float y,float w=1.);
  void Reset(char* option="");
  int SumupY(TH2Xch* th2,TH1* hshift1,char* opt = "" );
  int SumupY(TH2Xch* th2,TGraph* gshift1,char* opt = "" );

  virtual void Scale(Double_t c1 = 1);
  virtual int Add(CalibObj* th1,Double_t c1 = 1 , char* opt = "");
  //  int Sumup(TH2Xch* th1,Double_t shift = 0,char* opt = "" );
  //  int Sumup(TH2Xch* th1,TH2Xch* ch2,
  //	    Double_t shift1 = 1,Double_t shift2 = 1,char* opt = "" );
  //  int Sumup(TH2Xch* th1,TH1* hshift1,char* opt = "" );
  //  int Sumup(TH2Xch* th1,TH2Xch* th2,TH1* hshift1,TH1* hshift2,char* opt = "" );

  // Accessor to the data
  TH2* GetHist(){return _h_all;};
  TH3* GetHist3D(){return _h3_ch; };
  TH2* CreateHistCh(int ch);
  bool SetHistCh(int ch,TH2* h2);
  bool AddHistCh(int ch,TH2* h2);

  // Accessor to the data
  int Project();
  int GetNbinsX(){return _nbins[0];};
  float GetLowX(){return _low[0];};
  float GetUpX(){return _up[0];};
  float GetWidthX(){return _width[0];};
  int GetNbinsY(){return _nbins[1];};
  float GetLowY(){return _low[1];};
  float GetUpY(){return _up[1];};
  float GetWidthY(){return _width[1];};

  ClassDef(TH2Xch,1) //2 dimention x channel for Calibration.
};
//
#endif
//
