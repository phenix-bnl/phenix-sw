#ifdef COMPILE
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <Rtypes.h>
#include <TSystem.h>
#include <TROOT.h>
#include <TFile.h>
#include <TDirectory.h>
#include <TKey.h>
#include <TTree.h>
#include <TBranch.h>
#include <TNtuple.h>
#include <TCanvas.h>
#include <TPad.h>
#include <TStyle.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
#endif

#include "FitPeak2D.hh"
#include "fitpeak.hh"

//=========================================================================
FitPeak2D::FitPeak2D(){
  int i = 4;
  while( i-- ) gra[i] = 0;
};
//=========================================================================
FitPeak2D::~FitPeak2D(){
  int i = 4;
  while( i-- ) if( gra[i] ) gra[i]->Delete();
};
//=========================================================================
FitPeak2D::FitPeak2D(FitPeak2D& fitp){
  int i = 4;
  while( i-- ) gra[i] = fitp.gra[i];
};
//=========================================================================
int FitPeak2D::Fit(TH2* h2,char axis,
		   int cutnum,float cutmin,float cutmax,
		   float peakmin,float peakmax,float peaksig,
		   char* addname,char* fitopt,float peakupsig,int entrylimit,int rebin){
  TAxis* ta;
  if( axis == 'X' || axis == 'x' ){
    ta = h2->GetXaxis();
  } else if( axis == 'Y' || axis == 'y' ) {
    ta = h2->GetYaxis();
  } else {
    cerr<<"  Error::FitPeak2D:: axis must be 'X' or 'Y' "<<endl;
    return 0;
  }
  char* opt_q;
  opt_q = strstr(fitopt,"Q");
  char* opt_0;
  opt_0 = strstr(fitopt,"0");

  int binmin,binmax;
  int binwidth;
  binmin = ta->FindBin(cutmin);
  binmax = ta->FindBin(cutmax);
  binwidth = (int) ( (binmax+1-binmin)/cutnum );
  int n = cutnum;
  int* cutbin = new int[cutnum+1];
  float* min  = new float[cutnum];
  float* max  = new float[cutnum];
  float* sig  = new float[cutnum];
  float* upsig  = new float[cutnum];
  cutbin[n] = binmax;
  while( n-- ){
    cutbin[n] = binmin + binwidth * n;
    min[n] = peakmin;
    max[n] = peakmax;
    sig[n] = peaksig;
    if( peakupsig > 0 )
      upsig[n] = peakupsig;
    else
      upsig[n] = peaksig;
  }
  int ret = Fitbin(h2,axis,cutnum,cutbin,min,max,sig,addname,fitopt,upsig,entrylimit,rebin);
  delete[] cutbin;
  delete[] min;
  delete[] max;
  delete[] sig;
  delete[] upsig;
  return ret;
};

//=========================================================================
int FitPeak2D::Fitbin(TH2* h2,char axis,
		      int cutnum,int* cutbin,float* peakmin,float* peakmax,float* peaksig,
		      char* addname,char* fitopt,float* peakupsig,int entrylimit,int rebin){
  char* opt_q;
  opt_q = strstr(fitopt,"Q");
  char* opt_0;
  opt_0 = strstr(fitopt,"0");
  char* opt_1;
  opt_1 = strstr(fitopt,"1");

  // cutbin[cutnum+1] array...
  int i,n;
  char hname[128],htitle[128];
  TDirectory* current = gDirectory;
  gROOT->cd();
  float* par[4];
  float* parerr[4];
  i = 4;
  while( i-- ){
    par[i] = new float[cutnum];
    parerr[i] = new float[cutnum];
    n = cutnum;
    while( n-- ){
      par[i][n] = 0;
      parerr[i][n] = 0;
    }
  }
  float* cent = new float[cutnum];
  float* err  = new float[cutnum];

  if(! opt_q ) cout<<" =================== FitPeak2D ==================== "<<endl;
    
  if( h2 == 0 ||
      ( axis != 'X' && axis != 'x' && axis != 'Y' && axis != 'y' )
      ){
    cerr<<" Error in usage:: FitPeak2d.Fit "<<endl;
    cerr<<"               :: fitopt = Q .. no plotting. "<<endl;
    cerr<<"               :: axis = 'X' 'x' 'Y' 'y' "<<endl;
    return 0;
  }
  //----------------------------------
  TCanvas* c1;
  if(! opt_0 ) {
    c1 = (TCanvas*) gROOT->FindObject("c1");
    if( c1 == 0 )
      c1 =new TCanvas("c1","c1");
  }
  TAxis* ta;
  n = cutnum;
  while( n -- ){
    int firstbin = cutbin[n];
    int lastbin = cutbin[n+1] - 1;
    if( axis == 'X' || axis == 'x' )
      ta = h2->GetXaxis();
    else if( axis == 'Y' || axis == 'y' )
      ta = h2->GetYaxis();
    float first = ta->GetBinLowEdge(firstbin);
    float last = ta->GetBinLowEdge(lastbin) + ta->GetBinWidth(lastbin);
    float binwidth = last - first;
    if(! opt_q ){
      cout<<" range("<<first<<","<<last<<") == bin_range("<<firstbin<<","<<lastbin<<") "<<endl;
      cout<<"         fitting region :"<<peakmin[n]<<","<<peakmax[n]<<"   width:"<<peaksig[n];
      if( peakupsig != 0)
	cout<<" - "<<peakupsig[n];
      cout<<endl;
    }
    //
    TObject* obj;
    sprintf(hname,"h2_proj%d_%d",firstbin,lastbin);
    obj = gROOT->FindObject(hname);
    if( obj ) obj->Delete();
    if( axis == 'X' || axis == 'x' )
      h2->ProjectionY(hname,firstbin,lastbin);
    else if( axis == 'Y' || axis == 'y' )
      h2->ProjectionX(hname,firstbin,lastbin);
    TH1D* h2_proj = (TH1D*) gROOT->FindObject(hname);
    //
    //
    TF1* fit = 0 ;
    int peakminbin = h2_proj->FindBin( peakmin[n] );
    int peakmaxbin = h2_proj->FindBin( peakmax[n] );
    float integral_ent = (float) h2_proj->Integral(peakminbin,peakmaxbin);
    //if( h2_proj->GetEntries() < entrylimit || integral_ent < entrylimit ){
    if( integral_ent < entrylimit ){
      if(! opt_q ) cout<<"   FitPeak2D:: Integral entry is too small :"<<integral_ent<<endl;
      i = 4;
      while( i-- ){
	par[i][n] = 0;
	parerr[i][n] = 0;
      }
    } else {
      if(! opt_q ) cout<<"   FitPeak2D:: Integral entry is "<<integral_ent<<endl;
      if( rebin != 0 )
	h2_proj->Rebin(rebin);
      if( peakupsig )
	fit = fitpeak(h2_proj,peakmin[n],peakmax[n],peaksig[n],"_fp2d",fitopt,peakupsig[n]);
      else
	fit = fitpeak(h2_proj,peakmin[n],peakmax[n],peaksig[n],"_fp2d",fitopt);
      i = 3;
      while( i-- ){
	par[i][n] = fit->GetParameter(i);
	parerr[i][n] = fit->GetParError(i);
      }
      par[3][n] = fit->GetChisquare();
      parerr[3][n] = fit->GetNDF();
      if( fit ) fit->Delete();
    }
    cent[n] = first + binwidth/2.0; // FIX.ME
    err[n] = binwidth/2.0;
    if(! opt_0 ) {
      h2_proj->Draw();
      c1->Update();
      getchar();
    }
    if( h2_proj ) h2_proj->Delete();
  }
  //----------------------------------
  current->cd();
  //----------------------------------
  i = 4;
  while( i-- ){
    sprintf(hname,"%s%s%d",h2->GetName(),addname,i);
    sprintf(htitle,"Fitting par %d",i);
    TObject* obj = gROOT->FindObject(hname);
    if( obj != NULL ) {
      if(! opt_q ) cout<<" FitPeak2D:: Existing TGraphErrors "<<hname<<" is deleted."<<endl;
      obj->Delete();
    }
    if(! opt_q ) cout<<" Creating TGraphErrors :"<<hname<<endl;
    if( axis == 'X' || axis == 'x' )
      gra[i] = new TGraphErrors(cutnum,cent,par[i],err,parerr[i]);
    else if( axis == 'Y' || axis == 'y' )
      gra[i] = new TGraphErrors(cutnum,par[i],cent,parerr[i],err);
    gra[i]->SetName(hname);
    gra[i]->SetTitle(htitle);
  }
  if(! opt_0 ) {
    c1->cd();
    c1->Clear();
    c1->Divide(2,2);
    c1->cd(1);
    gra[0]->Draw("AL*");
    c1->cd(2);
    gra[1]->Draw("AL*");
    c1->cd(3);
    gra[2]->Draw("AL*");
    c1->cd(4);
    gra[3]->Draw("AL*");
    c1->cd();
    c1->Update();
    getchar();
  }
  if( opt_1 ){
    h2->Draw("box");
    gra[1]->Draw("L*");
  }
  delete[] cent;
  delete[] err;
  i = 4;
  while( i -- ){
    delete[] par[i];
    delete[] parerr[i];
  }
  if(! opt_q ) cout<<" =================== FitPeak2D ==================== "<<endl;
  return cutnum;
};
//=========================================================================

