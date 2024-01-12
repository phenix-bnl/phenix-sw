#include <TROOT.h>
#include <TSystem.h>
#include <TStyle.h>
#include <TFile.h>
#include <TString.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TCanvas.h>
#include <TPaveStats.h>
#include <TLine.h>
#include <TText.h>
#include <TPostScript.h>
#include <TGraphErrors.h>

#include <iostream>
#include <sstream>

using namespace std;

void plot_rate(){
  gROOT->SetStyle("Plain");
  gStyle->SetOptFit(0);
  gStyle->SetOptStat(0);

  TCanvas *c1 = new TCanvas("c1", "c1", 800, 800);
  c1->Divide(2,2);
  c1->cd(1);
  gPad->SetLogy();
  h_rate_all_w0->SetAxisRange(0,0.012);
  h_rate_all_w0->GetXaxis()->SetNdivisions(205);
  h_rate_all_w0->GetXaxis()->SetTitle("pixel hit count / event");
  h_rate_all_w0->Draw();
  c1->cd(2);
  gPad->SetLogy();
  h_rate_all_e0->SetAxisRange(0,0.012);
  h_rate_all_e0->GetXaxis()->SetNdivisions(205);
  h_rate_all_e0->GetXaxis()->SetTitle("pixel hit count / event");
  h_rate_all_e0->Draw();
  c1->cd(3);
  gPad->SetLogy();
  h_rate_all_w1->SetAxisRange(0,0.006);
  h_rate_all_w1->GetXaxis()->SetNdivisions(205);
  h_rate_all_w1->GetXaxis()->SetTitle("pixel hit count / event");
  h_rate_all_w1->Draw();
  c1->cd(4);
  gPad->SetLogy();
  h_rate_all_e1->SetAxisRange(0,0.006);
  h_rate_all_e1->GetXaxis()->SetNdivisions(205);
  h_rate_all_e1->GetXaxis()->SetTitle("pixel hit count / event");
  h_rate_all_e1->Draw();
  
}
