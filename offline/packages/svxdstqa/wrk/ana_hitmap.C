#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>

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

#include "utilfuncs.C"

using namespace std;

void fitHitRate(TH1F *h_rate, float *max, float *mean, float *rms, float *height, float *ave, float *sigma);
void calcCount(TH1F *h_ratechip, int nevt, float ave, float sigma, int *countZero, int* countOF, int* countUF);
void findChipHotDead(TH2F *h_map, int imod, int ichip, int nevt, float maxhot, ostream& oshot, ostream &osdead, ostream &osrate, int nhot[2][2], int ndead[2][2],
                     TGraph** g_hot, TGraph** g_dead, TGraph2D** g_rate, TGraph** g_hotcol, TGraph** g_deadcol);

TH2F* confirmHotDead(TH2F *h_map, TGraph *g_hot, TGraph *g_dead); // make 2D hist with hot =2, dead=0, normal=1

void drawPixelHitmap(TCanvas *c1, int run, int seq, int nevt, HistoContainer* cnt, float hot[2][2], float dead[2][2]);



void ana_hitmap(const char *fname, const char *outdir="anaoutdir/"){
  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);

  gSystem->Load("libTHmul.so");


  cout<<"fname  : "<<fname<<endl;
  cout<<"outdir : "<<outdir<<endl;


  int seq = getSeqNumber(fname);

  TH1F *h_run=NULL;
  TH1F *h_evt=NULL;
  TH1F *h_zvtx=NULL;
  TH2F *h_map[NMODULE][NCHIP]={{NULL}};


  // ----- initialize ------
  TDirectory *gDir = gDirectory;
  TFile *f = TFile::Open(fname);
  {
    gDirectory = gDir;

    TH1F* h_tmp= (TH1F*)f->Get("h_pxl_run"); if(h_tmp!=NULL)h_run=(TH1F*)h_tmp->Clone();
    h_evt      = (TH1F*)f->Get("h_pxl_evt")->Clone();
    h_zvtx     = (TH1F*)f->Get("h_pxl_zvtx")->Clone();

    for(int imod=0; imod<NMODULE; imod++){
      for(int ichip=0; ichip<NCHIP; ichip++){
        ostringstream sname;
        sname<<"h_pxl_hitmap_"<<imod<<"_"<<ichip;
  //      cout<<sname.str().c_str()<<endl;
        h_map[imod][ichip] = (TH2F*)f->Get(sname.str().c_str())->Clone();
      }
    }
  }
  
  f->Close();

  // Scale&Rebin
  //int run  = h_run->GetBinContent(1);
  int run = getRunNumber(fname); // temporally solution
  int nevt = h_evt->GetEntries();
  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      h_map[imod][ichip]->Sumw2();
      //h_map[imod][ichip]->Rebin2D(2, 8);
      //h_map[imod][ichip]->Scale(1.0/nevt);
    }
  }

  // init histogram
  TH1F *h_rate[NMODULE][NCHIP]={{NULL}};
  TH1F *h_ratew[NMODULE][NCHIP]={{NULL}};
  TH1F *h_ratechip[NMODULE][NCHIP]={{NULL}};
  TH1F *h_rate_all=NULL;
  TH1F *h_rate_all_w0=NULL;
  TH1F *h_rate_all_w1=NULL;
  TH1F *h_rate_all_e0=NULL;
  TH1F *h_rate_all_e1=NULL;
  TH2F *h_rateladder[NMODULE]={NULL};
  TH2F *h_rateladderw[NMODULE]={NULL};
  float maxrate=0.05;
  float maxrate2=1.000;
  for(int imod=0; imod<NMODULE; imod++){
    ostringstream sname, stitle;
    for(int ichip=0; ichip<NCHIP; ichip++){
      sname.str(""); stitle.str("");
      sname<<"h_rate_"<<imod<<"_"<<ichip;
      stitle<<"hit_rate mod: "<<imod<<" chip:"<<ichip;
      h_rate[imod][ichip] = new TH1F(sname.str().c_str(), stitle.str().c_str(), 5000, 0, maxrate);

      sname.str(""); stitle.str("");
      sname<<"h_ratew_"<<imod<<"_"<<ichip;
      stitle<<"hit_ratew  mod: "<<imod<<" chip:"<<ichip;
      h_ratew[imod][ichip] = new TH1F(sname.str().c_str(), stitle.str().c_str(), 10000, 0, maxrate2);

      sname.str(""); stitle.str("");
      sname<<"h_ratechip_"<<imod<<"_"<<ichip;
      stitle<<"hit_ratechip  mod: "<<imod<<" chip:"<<ichip;
      h_ratechip[imod][ichip] = new TH1F(sname.str().c_str(), stitle.str().c_str(), 8192, -0.5, 8191.5);
    }

    sname.str(""); stitle.str("");
    sname<<"h_rateladder_"<<imod;
    stitle<<"hit_rate vs chip mod: "<<imod;
    h_rateladder[imod] = new TH2F(sname.str().c_str(), stitle.str().c_str(), 8, -7.5, 0.5, 200, 0, maxrate);

    sname.str(""); stitle.str("");
    sname<<"h_rateladderw_"<<imod;
    stitle<<"hit_rate wide vs chip mod: "<<imod;
    h_rateladderw[imod] = new TH2F(sname.str().c_str(), stitle.str().c_str(), 8, -7.5, 0.5, 200, 0, maxrate2);
  }
  h_rate_all = new TH1F("h_rate_all", "hitrate allmodule chip", 10000, 0, maxrate2);

  h_rate_all_w0 = new TH1F("h_rate_all_w0", "hitrate West B0", 1000, 0, 0.1);
  h_rate_all_w1 = new TH1F("h_rate_all_w1", "hitrate West B1", 1000, 0, 0.1);
  h_rate_all_e0 = new TH1F("h_rate_all_e0", "hitrate East B0", 1000, 0, 0.1);
  h_rate_all_e1 = new TH1F("h_rate_all_e1", "hitrate East B1", 1000, 0, 0.1);


  // fill histogram
  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      for(int irow=0; irow<256; irow++){
        for(int icol=0; icol<32; icol++){
          //if(icol==1) cout<<irow<<":"<<icol<<" "<<flush;
          int ent = h_map[imod][ichip]->GetBinContent(icol+1, irow+1);
          float rate = float(ent)/nevt;
          h_rate[imod][ichip]->Fill(rate);
          h_ratew[imod][ichip]->Fill(rate);
          h_rateladder[imod]->Fill(-ichip, rate);
          h_rateladderw[imod]->Fill(-ichip, rate);
          h_rate_all->Fill(rate);

	  if (imod < 10) {//B0 West
	    h_rate_all_w0->Fill(rate);
	  } else if (imod < 20) {//B0 East
	    h_rate_all_e0->Fill(rate);
	  } else if (imod < 40) {//B1 West
	    h_rate_all_w1->Fill(rate);
	  } else  {//B1 East
	    h_rate_all_e1->Fill(rate);
	  }

          h_ratechip[imod][ichip]->SetBinContent(irow+256*icol+1, ent);
        }
        //cout<<endl;
      }
    }
  }


  ////////////////////////////////
  // Fitting with gaussian function
  cout<<"Start Fitting"<<endl;
  float max[NMODULE][NCHIP],    mean[NMODULE][NCHIP], rms[NMODULE][NCHIP];
  float height[NMODULE][NCHIP], ave[NMODULE][NCHIP],  sigma[NMODULE][NCHIP];
  for(int imod=0; imod<NMODULE; imod++){
    if(imod%5==0) cout<<"imodule : "<<imod<<endl;
    for(int ichip=0; ichip<NCHIP; ichip++){
      fitHitRate(h_rate[imod][ichip],
                 &(max[imod][ichip]), &(mean[imod][ichip]), &(rms[imod][ichip]), 
                 &(height[imod][ichip]), &(ave[imod][ichip]), &(sigma[imod][ichip]));
    }
  }


  cout<<"findChipHotDead"<<endl;
  TString hotname = Form("%s/%s", outdir, gSystem->BaseName(fname));
  hotname.ReplaceAll(".root", "_hot.txt");
  cout<<"Write to hot map file : "<<hotname.Data()<<endl;
  ofstream ofh;
  ofh.open(hotname.Data(), ios::out);
  //  ofh<<"#layer\t" << "ladder\t" << "south_north\t" << "module\t" << "ROC\t" << "col\t" << "row\t" <<endl;
  ofh<<"module\t" << "ROC\t" << "col\t" << "row\t" <<endl;

  TString deadname = Form("%s/%s", outdir, gSystem->BaseName(fname));
  deadname.ReplaceAll(".root", "_dead.txt");
  cout<<"Write to dead map file : "<<deadname.Data()<<endl;
  ofstream ofd;
  ofd.open(deadname.Data(), ios::out);
  //  ofd<<"#layer\t" << "ladder\t" << "south_north\t" << "module\t" << "ROC\t" << "col\t" << "row\t" <<endl;
  ofd<<"module\t" << "ROC\t" << "col\t" << "row\t" <<endl;

  TString ratename = Form("%s/%s", outdir, gSystem->BaseName(fname));
  ratename.ReplaceAll(".root", "_rate.txt");
  cout<<"Write to rate map file : "<<ratename.Data()<<endl;
  ofstream ofr;
  ofr.open(ratename.Data(), ios::out);
  ofr << "module\t" << "ROC\t" << "col\t" << "row\t" << "status\t" <<"rate\t" <<endl;

  float hot[2][2]={{0,0},{0,0}}; // layer, W/E
  float dead[2][2]={{0,0},{0,0}}; // layer, W/E
  int nhot[2][2]={{0,0},{0,0}}; // layer, W/E
  int ndead[2][2]={{0,0},{0,0}}; // layer, W/E
  TGraph *g_hot[NMODULE][NCHIP];
  TGraph *g_dead[NMODULE][NCHIP];
  TGraph2D *g_rate[NMODULE][NCHIP];
  TGraph *g_hotcol[NMODULE][NCHIP];
  TGraph *g_deadcol[NMODULE][NCHIP];
  TH2F   *h_hotdeadmap[NMODULE][NCHIP];

  for(int imod=0; imod<NMODULE; imod++){
    float hotlimit = (imod<20) ? 0.01 : 0.003;
    for(int ichip=0; ichip<NCHIP; ichip++){
      for (int il=0;il<2;il++) {
	for (int we=0; we<2; we++) {
	  nhot[il][we] = 0;
	  ndead[il][we] = 0;
	}
      }

      findChipHotDead(h_map[imod][ichip], imod, ichip, nevt, hotlimit, ofh, ofd, ofr, nhot, ndead,
      &(g_hot[imod][ichip]), &(g_dead[imod][ichip]), &(g_rate[imod][ichip]),
        &(g_hotcol[imod][ichip]), &(g_deadcol[imod][ichip])
      );

      for (int il=0;il<2;il++) {
	for (int we=0; we<2; we++) {
	  hot[il][we] += nhot[il][we];
	  dead[il][we] += ndead[il][we];
	}
      }

      h_hotdeadmap[imod][ichip] = 
        confirmHotDead(h_map[imod][ichip], g_hot[imod][ichip], g_dead[imod][ichip]);
    }
  }
  ofh.close();
  ofd.close();
  ofr.close();

  for (int il=0;il<2;il++) {
    for (int we=0; we<2; we++) {
      if (il==0) {
	hot[il][we] /= (float)(10*NCHIP*NCOLUMN*NROW);
	dead[il][we] /= (float)(10*NCHIP*NCOLUMN*NROW);
      } else {
	hot[il][we] /= (float)(20*NCHIP*NCOLUMN*NROW);
	dead[il][we] /= (float)(20*NCHIP*NCOLUMN*NROW);
      }
    }
  }


  TGraph *g_hot_lay;     // [layer][side] = 0/1, s/n=0/1
  TGraph *g_dead_lay;    // [layer][side] = 0/1, s/n=0/1
  TGraph *g_hotcol_lay;  // [layer][side] = 0/1, s/n=0/1
  TGraph *g_deadcol_lay; // [layer][side] = 0/1, s/n=0/1
  {

    g_hot_lay     = new TGraph();
    g_dead_lay    = new TGraph();
    g_hotcol_lay  = new TGraph();
    g_deadcol_lay = new TGraph();

    //// fill
    float nhot_lay[4]    ={0,0,0,0};
    float ndead_lay[4]   ={0,0,0,0};
    float nhotcol_lay[4] ={0,0,0,0};
    float ndeadcol_lay[4]={0,0,0,0};

    for(int imod=0; imod<NMODULE; imod++){
      int layer = (imod<20)  ? 0 : 1;
      int side  = (layer==0) ? imod/10 : (imod-20)/20;
      //cout<<imod<<" "<<layer<<" "<<side<<endl;
      int idx=2*layer+side;

      for(int ichip=0; ichip<NCHIP; ichip++){
        nhot_lay[idx]     += (float)g_hot[imod][ichip]->GetN();
        ndead_lay[idx]    += (float)g_dead[imod][ichip]->GetN();
        nhotcol_lay[idx]  += (float)g_hotcol[imod][ichip]->GetN();
        ndeadcol_lay[idx] += (float)g_deadcol[imod][ichip]->GetN();
      }
    }
    for(int i=0; i<4; i++){
      float nlad = (i<2) ? 10 : 20;
      g_hot_lay    ->SetPoint(i, i, nhot_lay[i]    /float(8192*8*nlad));
      g_dead_lay   ->SetPoint(i, i, ndead_lay[i]   /float(8192*8*nlad));
      g_hotcol_lay ->SetPoint(i, i, nhotcol_lay[i] /float(32*8*nlad));
      g_deadcol_lay->SetPoint(i, i, ndeadcol_lay[i]/float(32*8*nlad));
    }
  }


  ////////////////////////////////
  // count 0 entry
  cout<<"Start Count Calculation"<<endl;
  //int nevt = h_evt->GetEntries();

  int countZero[NMODULE][NCHIP], countOF[NMODULE][NCHIP], countUF[NMODULE][NCHIP];
  for(int imod=0; imod<NMODULE; imod++){
    //if(imod%5==0) cout<<"imodule : "<<imod<<endl;
    for(int ichip=0; ichip<NCHIP; ichip++){
      calcCount(h_ratechip[imod][ichip], nevt,
                ave[imod][ichip], sigma[imod][ichip],
                &(countZero[imod][ichip]), &(countOF[imod][ichip]), &(countUF[imod][ichip]));

      cout<<imod<<" "<<ichip<<" "<<countZero[imod][ichip]<<" "<<h_rate[imod][ichip]->GetBinContent(1)<<endl;
    }
  }

  // Rate vs chip graph
  float rate_max=0.0, rate_min=1.0;
  TGraphErrors* g_ratemean  = new TGraphErrors(480);
  TGraphErrors* g_rateave   = new TGraphErrors(480);
  TGraph*       g_countZero = new TGraph(480);
  TGraph*       g_countOF   = new TGraph(480);
  TGraph*       g_countUF   = new TGraph(480);
  TGraph *gg[5] = {g_ratemean, g_rateave, g_countZero, g_countOF, g_countUF};
  for(int i=0; i<5; i++){
    gg[i]->SetMarkerStyle(20);
    gg[i]->SetMarkerSize(0.5);
  }

  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      double i = NCHIP*imod + ichip;
      g_ratemean->SetPoint(i, i, mean[imod][ichip]);
      g_ratemean->SetPointError(i, 0.0, rms[imod][ichip]);

      g_rateave->SetPoint(i, i, ave[imod][ichip]);
      g_rateave->SetPointError(i, 0.0, sigma[imod][ichip]);

      g_countZero->SetPoint(i, i, countZero[imod][ichip]);
      g_countOF->SetPoint(i, i, countOF[imod][ichip]);
      g_countUF->SetPoint(i, i, countUF[imod][ichip]);

      if(rate_max<mean[imod][ichip]) rate_max=mean[imod][ichip]; 
      if(rate_min>mean[imod][ichip]) rate_min=mean[imod][ichip]; 
    }
  }

  // Plot
  TString s_outdir(outdir);
  if(s_outdir.Length()==0) s_outdir.Append("./");

  cout<<"srun, seq, nevt : "<<run<<" "<<seq<<" "<<nevt<<endl;

  HistoContainer cnt, cnt1;
  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
//   qq   int maxcount = nevt * ((imod<20) ? 0.01 : 0.003);
      int maxcount = nevt * ((imod<20) ? 0.005 : 0.0015);
      h_map[imod][ichip]->SetMaximum(maxcount);
      cnt.h_hist[imod][ichip] = h_map[imod][ichip];
      cnt1.h_hist[imod][ichip] = h_hotdeadmap[imod][ichip];
    }
  }


  // c1
  TCanvas* c1 = new TCanvas("c1", "c1", 2000, 1600);
  drawPixelHitmap(c1, run, seq, nevt, &cnt, hot, dead);
  cout<<Form("%s/hitmap_pixel_%06d_%04d.png", s_outdir.Data(), run, seq)<<endl;
  c1->Print(Form("%s/hitmap_pixel_%06d_%04d.png", s_outdir.Data(), run, seq)); 
  //TCanvas *c1 = drawPixelHitmap(run, seq, nevt, &cnt, outdir);

  // c2
  TCanvas* c2 = new TCanvas("c2", "c2", 1000, 700);
  drawPixelHitmap(c2, run, seq, nevt, &cnt1, hot, dead);
  cout<<Form("%s/hotdeadmap_pixel_%06d_%04d.png", s_outdir.Data(), run, seq)<<endl; 
  c2->Print(Form("%s/hotdeadmap_pixel_%06d_%04d.png", s_outdir.Data(), run, seq)); 


  // Write to file
  TString soname = Form("%s/%s", outdir, gSystem->BaseName(fname));
  soname.ReplaceAll(".root", "_ana.root");
  cout<<"Write To file : "<<soname.Data()<<endl;
  TFile *foroot = TFile::Open(soname.Data(), "recreate");
  {
    if(h_run!=NULL) h_run->Write();
    if(h_evt!=NULL) h_evt->Write();
    if(h_zvtx!=NULL)h_zvtx->Write();
    for(int imod=0; imod<NMODULE; imod++){
      for(int ichip=0; ichip<NCHIP; ichip++){
        if(h_map[imod][ichip]     !=NULL) h_map[imod][ichip]->Write();
        if(h_ratechip[imod][ichip]!=NULL) h_ratechip[imod][ichip]->Write();
        if(h_rate[imod][ichip]    !=NULL) h_rate[imod][ichip]->Write();
        if(h_ratew[imod][ichip]   !=NULL) h_ratew[imod][ichip]->Write();

        if(g_hot[imod][ichip]    !=NULL) g_hot[imod][ichip]->Write();
        if(g_dead[imod][ichip]   !=NULL) g_dead[imod][ichip]->Write();
        if(g_rate[imod][ichip]   !=NULL) g_rate[imod][ichip]->Write();
        if(g_hotcol[imod][ichip] !=NULL) g_hotcol[imod][ichip]->Write();
        if(g_deadcol[imod][ichip]!=NULL) g_deadcol[imod][ichip]->Write();

        if(h_hotdeadmap[imod][ichip]!=NULL) h_hotdeadmap[imod][ichip]->Write();
      }
      if(h_rateladder [imod]!=NULL) h_rateladder[imod]->Write();
      if(h_rateladderw[imod]!=NULL) h_rateladderw[imod]->Write();
    }
    h_rate_all->Write();
    h_rate_all_w0->Write();
    h_rate_all_e0->Write();
    h_rate_all_w1->Write();
    h_rate_all_e1->Write();

    g_ratemean ->SetName("g_ratemean");  g_ratemean ->Write();
    g_rateave  ->SetName("g_rateave");   g_rateave  ->Write();              
    g_countZero->SetName("g_countZero"); g_countZero->Write();
    g_countOF  ->SetName("g_countOF"  ); g_countOF  ->Write();
    g_countUF  ->SetName("g_countUF"  ); g_countUF  ->Write();

    g_hot_lay    ->SetName("g_hot_lay");     g_hot_lay->Write();
    g_dead_lay   ->SetName("g_dead_lay");    g_dead_lay->Write();
    g_hotcol_lay ->SetName("g_hotcol_lay");  g_hotcol_lay->Write();
    g_deadcol_lay->SetName("g_deadcol_lay"); g_deadcol_lay->Write();

  }


  foroot->Close();
  delete f;

}

void fitHitRate(TH1F *h_rate, float *max, float *mean, float *rms, float *height, float *ave, float *sigma){
  TF1 *f1 = new TF1("f1", "gaus(0)");
  f1->SetLineColor(4);
  f1->SetLineWidth(2.0);
  *max = h_rate->GetMaximum();
  *mean= h_rate->GetMean();
  *rms = h_rate->GetRMS();
  f1->SetParameters(*max, *mean, *rms);
  h_rate->Fit("f1", "Q");
  *height= f1->GetParameter(0);
  *ave   = f1->GetParameter(1);
  *sigma = f1->GetParameter(2);
}

void calcCount(TH1F *h_ratechip, int nevt, float ave, float sigma, int *countZero, int* countOF, int* countUF){
  const float nsig=10.0;

  // OF hitrate > 10sigma, UF hitrate < 10 sigma
  (*countZero)=0;
  (*countOF)=0;
  (*countUF)=0;
 
  //int nbin=h_ratechip->GetNbinsX();
  for(int i=0; i<h_ratechip->GetNbinsX(); i++){
    int entry = h_ratechip->GetBinContent(i+1);
    if(entry<=0.0) { (*countZero)++; }
    float rate = float(entry)/nevt;
    if(rate>= (ave + nsig*sigma) ) (*countOF)++;
    if(rate<= (ave - nsig*sigma) ) (*countUF)++;
 
  }
}




