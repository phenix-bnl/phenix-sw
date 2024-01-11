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
#endif

#include "fit_mpeak.hh"

TF1* fit_mpeak(TH1* hist,float gmin,float gmax,
	       float emin,float emax,char* addname,
	       int opt,char* fitopt){

  cout<<" =================== fit_mpeak ==================== "<<endl;
  if( hist == 0 ) {
    cerr<<" Error in usage:: fit_mpeak.cc(TH1*) "<<endl;
    cerr<<"                     opt == 0 : landau+exp "<<endl;
    cerr<<"                     opt == 1 : gaus+exp "<<endl;
    cerr<<"                     opt == -n : gaus+poln "<<endl;
    exit(0);
  }
  char hname[128];
  TF1* fit_bg;
  TF1* fitg;
  TF1* fittot;
  if( opt == 0 ){ // Default==Landau
    cout<<" Using landau+exp distribution for mip"<<endl;
    fitg = new TF1("fitg","landau",gmin,gmax);
    fit_bg = new TF1("fit_bg","expo",emin,emax);
    fittot  = new TF1("fittot","landau(0)+expo(3)",emin,emax);
  } if( opt == 1 ){
    cout<<" Using gaussian+exp distribution for mip"<<endl;
    fitg = new TF1("fitg","gaus",gmin,gmax);
    fit_bg = new TF1("fit_bg","expo",emin,emax);
    fittot  = new TF1("fittot","gaus(0)+expo(3)",emin,emax);
  } if( opt < 0 ){
    opt = -1* opt;
    cout<<" Using gaussian+pol"<<opt<<" distribution for mip"<<endl;
    fitg = new TF1("fitg","gaus",gmin,gmax);
    sprintf(hname,"pol%d",opt);
    fit_bg = new TF1("fit_bg",hname,emin,emax);
    sprintf(hname,"gaus(0)+pol%d(3)",opt);
    fittot  = new TF1("fittot",hname,emin,emax);
  }

  int lwidth = hist->GetLineWidth();
  int lcolor = hist->GetLineColor();
  fittot->SetLineWidth(lwidth);
  fittot->SetLineColor(lcolor);

  hist->Fit("fitg","RQ0");
  hist->Fit("fit_bg","RQ0");

  double par[10];
  fitg->GetParameters(par);
  fit_bg->GetParameters(&par[3]);
  fittot->SetParameters(par);

  char option[32];
  sprintf(option,"R%s",fitopt);
  hist->Fit("fittot",option);
  fittot->GetParameters(par);
  int n = 3;
  while( n-- ) par[n] = fabs(par[n]);
  fit_bg->SetParameters(&par[3]);
  fitg->SetParameters(par);

  fit_bg->SetLineWidth(3);
  fit_bg->SetLineColor(3);
  fit_bg->Draw("same");
  fit_bg->DrawCopy("same");
  TH1* histe = fit_bg->GetHistogram();
  histe->DrawCopy("same");
  fitg->SetFillColor(4);
  // Don't Draw the shape of background..........
  fitg->SetFillStyle(3007);
  fitg->SetLineColor(4);
  fitg->SetLineWidth(2);
  fitg->SetRange(emin,emax);
  fitg->Draw("same");
  fitg->DrawCopy("same");
  int blow,bhigh;

  TH1* h_bg = (TH1*)(hist->Clone());
  h_bg->Eval(fit_bg,"R");
  TH1* hg = (TH1*)(hist->Clone());
  hg->Add(h_bg,-1);
  blow = hg->FindBin(emin);
  bhigh = hg->FindBin(emax);
  hg->GetXaxis()->SetRange(blow,bhigh);
  // Don't Draw the shape of background..........
  hg->SetLineColor(2);
  hg->SetLineWidth(2);
  hg->Draw("same");
  hg->DrawCopy("same");

  blow = h_bg->FindBin(emin);
  bhigh = h_bg->FindBin(emax);
  cout<<" Background integral("<<emin<<","<<emax<<") = "
      <<h_bg->Integral(blow,bhigh)<<endl;
  blow = hg->FindBin(gmin);
  bhigh = hg->FindBin(gmax);
  cout<<" Peaks integral("<<gmin<<","<<gmax<<") = "
      <<hg->Integral(blow,bhigh)<<endl;
  char mipname[64],fitname[64];
  sprintf(mipname,"%s%s",hg->GetName(),addname);
  sprintf(fitname,"%s%sfit",hg->GetName(),addname);
  TObject* obj;
  obj = gROOT->FindObject(mipname);
  if(obj){ obj->Delete(); cout<<" existing "<<mipname<<" is deleted"<<endl;}
  obj = gROOT->FindObject(fitname);
  if(obj){ obj->Delete(); cout<<" existing "<<fitname<<" is deleted"<<endl;}
  cout<<" TH1  :"<<mipname<<"    is created"<<endl;
  cout<<" TF1  :"<<fitname<<"    is created"<<endl;
  cout<<" ================================================= "<<endl;
  h_bg->Delete();
  hg->SetName(mipname);
  fitg->Delete();
  fit_bg->Delete();
  fittot->SetName(fitname);
  return fittot;
}
//
