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

#include "fit_mpeak.hh"
#include "fitpeak.hh"
#include "fit_mpeak2d.hh"

//=========================================================================
TGraphErrors* fit_mpeak2d(TH2F* h2,TH2F* h2s,
			  int bin,int* bincut,
			  char* addname,char* fitopt){
  // bincut[bin+1] array...
  char hname[128],htitle[128];
  TDirectory* current = gDirectory;
  gROOT->cd();
  float* mip    = new float[bin];
  float* miperr = new float[bin];
  float* mom    = new float[bin];
  float* momerr = new float[bin];
  char opt[32];

  cout<<" =================== fit_mpeak2d ==================== "<<endl;
  if( h2 == 0 || h2s == 0) {
    cerr<<" Error in usage:: fit_mpeak2d.cc(TH1F*,TH1F*,int bin,int* bincut,char* addname,char* fitopt) "<<endl;
    cerr<<"               :: fitopt = Q .. no plotting. "<<endl;
    exit(0);
  }
  
  //----------------------------------
  TCanvas* c1;
  if( strcmp(fitopt,"Q") != 0 )
    c1 =new TCanvas("c1","c1");
  int mombin = bin;
  while( mombin -- ){
    int firstbin = bincut[mombin];
    int lastbin = bincut[mombin+1] - 1;
    float first = h2->GetBinLowEdge(firstbin);
    float last = h2->GetBinLowEdge(lastbin) + h2->GetBinWidth(lastbin);
    float binwidth = last - first;
    cout<<" mom_range("<<first<<","<<last
	<<") == bin_range("<<firstbin<<","<<lastbin<<") "<<endl;
    TObject* obj;
    sprintf(hname,"h2_py%d_%d",firstbin,lastbin);
    obj = gROOT->FindObject(hname);
    if( obj ) obj->Delete();
    h2->ProjectionY(hname,firstbin,lastbin);
    TH1D* h2_py = (TH1D*) gROOT->FindObject(hname);
    sprintf(hname,"h2_py_s%d_%d",firstbin,lastbin);
    obj = gROOT->FindObject(hname);
    if( obj ) obj->Delete();
    h2s->ProjectionY(hname,firstbin,lastbin);
    TH1D* h2_py_s = (TH1D*) gROOT->FindObject(hname);
    //
    if( strcmp(fitopt,"Q") != 0 ){
      h2_py->DrawCopy();
      h2_py_s->SetLineColor(5);
      h2_py_s->Draw("same");
      h2_py->Add(h2_py_s,-1);
      h2_py->SetLineColor(2);
      h2_py->Draw("same");
      c1->Update();
      getchar();
    }
    sprintf(opt,"R%s",fitopt);
    TF1* fit = fitpeak(h2_py,0.1,0.45,0.8,opt);
    mip[mombin] = fit->GetParameter(1);
    miperr[mombin] = fit->GetParError(1);
    mom[mombin] = binwidth/2 + first;
    momerr[mombin] = binwidth/2;
    if( strcmp(fitopt,"Q") != 0 ){
      c1->Update();
      getchar();
    }
    if( h2_py ) h2_py->Delete();
    if( h2_py_s ) h2_py_s->Delete();
  }
  //----------------------------------
  current->cd();
  //----------------------------------
  sprintf(hname,"%s%s",h2->GetName(),addname);
  sprintf(htitle,"MIP energy vs momentum");
  TObject* obj = gROOT->FindObject(hname);
  if( obj != NULL ) {
    cout<<" fit_mpeak2d:: Existing TGraphErrors "<<hname<<" is deleted."<<endl;
    obj->Delete();
  }
  cout<<" Creating TGraphErros in the current directory :"<<hname<<endl;
  TGraphErrors* gra = new TGraphErrors(bin,mom,mip,momerr,miperr);
  if( strcmp(fitopt,"Q") != 0 ){
    TH2F* h2clone = (TH2F*) h2->Clone();
    h2clone->Reset();
    h2clone->DrawCopy();
    h2clone->Delete();
    gra->Draw("L*");
    c1->Update();
    getchar();
  }
  delete[] mip;
  delete[] miperr;
  delete[] mom;
  delete[] momerr;
  gra->SetName(hname);
  gra->SetTitle(htitle);
  cout<<" TGraphErrors:: "<<hname<<" Created ."<<endl;
  cout<<" =================== fit_mpeak2d ==================== "<<endl;
  return gra;
};
//=========================================================================

