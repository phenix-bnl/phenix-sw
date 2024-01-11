#include <TROOT.h>
#include <TDirectory.h>
#include <TSystem.h>
#include <TStyle.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
//#include <TGraph2D.h>
#include <TText.h>
#include <TPaveText.h>
#include <TGaxis.h>
#include <TRegexp.h>
#include <TBox.h>

#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include <fstream>

using namespace std;
static const int NMODULE=60;
static const int NCHIP=8;

void raterunplotall(Int_t module, Int_t chip) {
  //  TFile *f = new TFile("raterun3.root");
  TFile *f = new TFile("raterun_all_346973_349699_hottrim.root");
  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  //  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  ostringstream oplot;
  oplot << "grate4_"<<module<<"_"<<chip;
   cout << "oplot = " << oplot.str().c_str() << endl;

  ostringstream fplot;
  //  fplot << "grate4_"<<module<<"_"<<chip<<".png";
  fplot << "grate4_"<<module<<"_"<<chip<<"_hottrim.png";

  ostringstream tplot;
  tplot << "Module "<<module<<" Chip "<<chip;

  TGraph *grate = (TGraph*)gROOT->FindObject(oplot.str().c_str());

  TCanvas* c1 = new TCanvas("c1", "c1", 700, 500);

  grate->SetTitle(tplot.str().c_str());
  if (module<20) {
    grate->SetMaximum(0.0050);
  } else {
    grate->SetMaximum(0.0025);
  }
  grate->SetMarkerStyle(23);
  grate->SetMarkerSize(1);
  grate->SetMarkerColor(6);
  grate->Draw("AP");
  //  grate->GetHistogram()->SetTitle(tplot.str().c_str());
  
  c1->SaveAs(fplot.str().c_str());
  //c1->Clear();


}
