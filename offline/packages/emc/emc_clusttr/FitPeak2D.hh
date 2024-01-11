//==================================================================
#ifndef _FitPeak2D_hh_
#define _FitPeak2D_hh_
//==================================================================

#include <TH1.h>
#include <TF1.h>
#include <TGraphErrors.h>
#include <TH2.h>

class FitPeak2D {
public:
  TGraphErrors* gra[4];
  //---------------------- gra[0] :: par0
  //---------------------- gra[1] :: par1
  //---------------------- gra[2] :: par2
  //---------------------- gra[3] :: Chisqure ( NDF )
public:
  FitPeak2D();
  ~FitPeak2D();
  FitPeak2D(FitPeak2D&);
  int Fit(TH2* h2,char axis,
	  int cutnum,float cutmin,float cutmax,float peakmin,float peakmax,float peaksig,
	  char* addname="_gra",char* fitopt="",float peakupsig=0,int entrylimit = 10,int rebin = 0);
  int Fitbin(TH2* h2,char axis,
	     int cutnum,int* cutbin,float* peakmin,float* peakmax,float* peaksig,
	     char* addname="_gra",char* fitopt="",float* peakupsig=0,int entrylimit = 10, int rebin = 0);

};
//==================================================================
#endif
//==================================================================

