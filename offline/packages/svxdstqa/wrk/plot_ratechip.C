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

static const int NMODULE=60;
static const int NCHIP  =8;

void drawCanvas(TCanvas *c1, TH2* h_hitmap, TH1* h_ratechip, TH1* h_rate, TH1* h_ratew,
                int nevt, int countZero, int countOF, int countUF);


void plot_ratechip(){
  gROOT->SetStyle("Plain");
  gStyle->SetOptFit(111);

  //const char *fname = "outdir/SvxAnaMain_338555_0000_ana.root";
  const char *fname = "SvxDstQA_340496_0000_ana.root";

  TH1F *h_run=NULL;
  TH1F *h_evt=NULL;
  TH2F *h_hitmap[NMODULE][NCHIP]={{NULL}};
  TH1F *h_ratechip[NMODULE][NCHIP]={{NULL}};
  TH1F *h_rate[NMODULE][NCHIP]={{NULL}};
  TH1F *h_ratew[NMODULE][NCHIP]={{NULL}};
  TGraphErrors* g_ratemean=NULL;
  TGraphErrors* g_rateave=NULL;
  TGraph* g_countZero=NULL;
  TGraph* g_countOF=NULL;
  TGraph* g_countUF=NULL;

  TDirectory *gDir = gDirectory;

  cout<<"Loading file : "<<fname<<endl;
  TFile *infile = TFile::Open(fname);
  {
    gDirectory = gDir;
    TH1F* h_tmp= (TH1F*)infile->Get("h_pxl_run"); if(h_tmp!=NULL)h_run=(TH1F*)h_tmp->Clone();
    h_evt      = (TH1F*)infile->Get("h_pxl_evt")->Clone();
    ostringstream sname;
    for(int imod=0; imod<NMODULE; imod++){
      if(imod%5==0) cout<<"imodule : "<<imod<<endl;
      for(int ichip=0; ichip<NCHIP; ichip++){
        sname.str(""); sname<<"h_pxl_hitmap_"<<imod<<"_"<<ichip;
        h_hitmap[imod][ichip]   = (TH2F*)infile->Get(sname.str().c_str())->Clone();

        sname.str(""); sname<<"h_ratechip_"<<imod<<"_"<<ichip;
        h_ratechip[imod][ichip] = (TH1F*)infile->Get(sname.str().c_str())->Clone();

        sname.str(""); sname<<"h_rate_"<<imod<<"_"<<ichip;
        h_rate[imod][ichip]     = (TH1F*)infile->Get(sname.str().c_str())->Clone();

        sname.str(""); sname<<"h_ratew_"<<imod<<"_"<<ichip;
        h_ratew[imod][ichip]    = (TH1F*)infile->Get(sname.str().c_str())->Clone();
      }
    }
    g_ratemean = (TGraphErrors*)infile->Get("g_ratemean")->Clone();
    g_rateave  = (TGraphErrors*)infile->Get("g_rateave")->Clone();

    g_countZero = (TGraph*)infile->Get("g_countZero")->Clone();
    g_countOF   = (TGraph*)infile->Get("g_countOF")->Clone();
    g_countUF   = (TGraph*)infile->Get("g_countUF")->Clone();
  }
  infile->Close();
  delete infile;

  int nevt = h_evt->GetEntries();

  double rate_min=1.0, rate_max=0.0;
  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      double x, rate;
      int i=imod*8+ichip;
      g_ratemean->GetPoint(i, x, rate);

      if(rate_min>rate) rate_min = rate;
      if(rate_max<rate) rate_max = rate;
    }
  }

  //// plot result  
  cout<<"Printing to PS : "<<endl;

  TCanvas *c1 = new TCanvas("c1", "c1", 500, 750);

  const char *psname = "plot_ratechip.ps";

  TPostScript *ps = new TPostScript(psname);

  c1->Divide(2,3);

  for(int imod=0; imod<NMODULE; imod++){
    if(imod%5==0) cout<<"imodule : "<<imod<<endl;
    for(int ichip=0; ichip<NCHIP; ichip++){
  //for(int imod=0; imod<1; imod++){
  //  for(int ichip=0; ichip<2; ichip++){
      double x, countZero, countOF, countUF;
      int i=imod*8+ichip;
      g_countZero->GetPoint(i, x, countZero);
      g_countOF->GetPoint(i, x, countOF);
      g_countUF->GetPoint(i, x, countUF);

      ps->NewPage();
      drawCanvas(c1, h_hitmap[imod][ichip], h_ratechip[imod][ichip], 
                 h_rate[imod][ichip], h_ratew[imod][ichip],
                 nevt, countZero, countOF, countUF);
      c1->Update();
    }
  }
  ps->Close();
  delete ps;

  cout<<"PS2PDF"<<endl;
  gSystem->Exec(Form("ps2pdf  %s", psname));

  TLine *l = new TLine(160, rate_min, 160, 2.*rate_max);
  l->SetLineColor(4);
  cout<<rate_min<<" "<<rate_max<<endl;

  TCanvas *c2 = new TCanvas("c2", "c2", 500, 750);
  c2->Divide(1,2);
  c2->cd(1);
  g_ratemean->Draw("AP");
  l->Draw();
  c2->cd(2);
  g_rateave->Draw("AP");
  l->Draw();
  c2->Print("plot_ratechip_c2.png");

}

void drawCanvas(TCanvas *c1, 
                TH2* h_hitmap, TH1* h_ratechip, TH1* h_rate, TH1* h_ratew,
                int nevt, int countZero, int countOF, int countUF){

  float height = h_rate->GetFunction("f1")->GetParameter(0);
  float ave    = h_rate->GetFunction("f1")->GetParameter(1);
  float sigma  = h_rate->GetFunction("f1")->GetParameter(2);
  float mean   = h_rate->GetMean();
  float rms    = h_rate->GetRMS();


  c1->cd(1);
  gStyle->SetPalette(1);
//  h_hitmap->SetStats(0);
//  h_hitmap->Draw("colz");

  c1->cd(2);
  h_ratechip->Sumw2();
  h_ratechip->Scale(1.0/nevt);
  h_ratechip->SetStats(0);
  h_ratechip->Draw("hist");

  c1->cd(3);
  gPad->SetLogy();
  h_rate->SetLineColor(2); h_rate->SetAxisRange(0.0, mean+rms*20);
  h_rate->Draw();
  //h_rate->GetFunction("f1")->Draw("same");

  float pos = ave + 10.0*sigma;
  TLine *l = new TLine(pos, 0.0, pos, 1.3*height);
  l->SetLineColor(3);
  l->Draw();
  TPaveStats *st = (TPaveStats*)h_rate->FindObject("stats");
  if(st!=NULL){
    st->SetOptFit(1111);
    st->SetX1NDC(0.5);
    st->SetY1NDC(0.5);
  }

  c1->cd(4);
  gPad->SetLogy();
  h_ratew->SetLineColor(2);
  h_ratew->SetLineWidth(2);
  h_ratew->Draw();

  c1->cd(5);
  gPad->Clear();
  TText *t = new TText();
  //cout<<"size = "<<t->GetTextSize()<<endl;
  float rateOF   = float(countOF)  /8192.*100.;
  float rateUF   = float(countUF)  /8192.*100.;
  float rateZero = float(countZero)/8192.*100.;
  t->SetTextSize(0.07);
  t->DrawText(0.1, 0.8,  Form("Nevent    : %d", nevt));
  t->DrawText(0.1, 0.65, Form("Hot  OF Entry : %d, %4.1f%%", countOF,   rateOF));
  t->DrawText(0.1, 0.5,  Form("Dead UF Entry : %d, %4.1f%%", countUF,   rateUF));
  t->DrawText(0.1, 0.35, Form("    ZeroEntry : %d, %4.1f%%", countZero, rateZero));
}
