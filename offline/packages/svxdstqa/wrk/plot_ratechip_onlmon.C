#include <TROOT.h>
#include <TSystem.h>
#include <TStyle.h>
#include <TFile.h>
#include <TString.h>
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TPaveStats.h>
#include <TLine.h>
#include <TText.h>
#include <TGraph.h>
#include <TGaxis.h>
#include <TRegexp.h>

#include <iostream>
#include <iomanip>
#include <sstream>

#include <utilfuncs.C>

using namespace std;

//static const int NMODULE=60;
//static const int NCHIP  =8;

/*
class HistoContainer {
  public:
    TH2* h_hist[NMODULE][NCHIP];
};
*/

void findChipHotDead(TH2F *h_map, int imod, int ichip, int nevt, float maxhot, ostream& oshot, ostream &osdead, ostream &osrate, int nhot[2][2], int ndead[2][2],
                     TGraph** g_hot, TGraph** g_dead, TGraph2D** g_rate, TGraph** g_hotcol, TGraph** g_deadcol);

TH2F* confirmHotDead(TH2F *h_map, TGraph *g_hot, TGraph *g_dead); // make 2D hist with hot =2, dead=0, normal=1

void drawPixelHitmap(TCanvas *c1, int run, int seq, int nevt, HistoContainer* cnt);

int getSeqNumber(const char *fname);

void plot_ratechip_onlmon(){
  gROOT->SetStyle("Plain");
//  gStyle->SetOptFit(111);

  //const char *fname = "merge_anaoutdir/SvxDstQA_Merge_344999_0001_ana.root";
  const char *fname = "SvxDstQA_Merge_344999_0001_ana.root";

  int seq = getSeqNumber(fname);

  TH1F *h_run=NULL;
  TH1F *h_evt=NULL;
  TH2F *h_hitmap[NMODULE][NCHIP]={{NULL}};
  TH1F *h_rate[NMODULE][NCHIP]={{NULL}};
  TH1F *h_ratew[NMODULE][NCHIP]={{NULL}};

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

        sname.str(""); sname<<"h_rate_"<<imod<<"_"<<ichip;
        h_rate[imod][ichip]    = (TH1F*)infile->Get(sname.str().c_str())->Clone();

        sname.str(""); sname<<"h_ratew_"<<imod<<"_"<<ichip;
        h_ratew[imod][ichip]    = (TH1F*)infile->Get(sname.str().c_str())->Clone();
      }
    }
  }
  infile->Close();
  delete infile;

  // add rate histogram
  TH1F *h_rate_sum[4];
  TH1F *h_ratew_sum[4];
  int modsta[5] = {0, 10, 20, 40, 60};
  for(int i=0; i<4; i++){
    bool first=true;
    for(int imod=modsta[i]; imod<modsta[i+1]; imod++){
      
      for(int ichip=0; ichip<8; ichip++){
        if(first){
          h_rate_sum[i] = (TH1F*)h_rate[imod][ichip]->Clone();
          h_rate_sum[i]->Sumw2();
          h_ratew_sum[i] = (TH1F*)h_ratew[imod][ichip]->Clone();
          h_ratew_sum[i]->Sumw2();
          first=false;
        } else {
          h_rate_sum[i]->Add(h_rate[imod][ichip]);
          h_ratew_sum[i]->Add(h_ratew[imod][ichip]);
        }
      }
      cout<<"i: "<<i<<" imod:"<<imod<<endl;
    }
  }

  
  // find dead hot
  /*
  TString hotname = Form("%s/%s", outdir, gSystem->BaseName(fname));
  hotname.ReplaceAll(".root", "_hot.txt");
  cout<<"Write to hot map file : "<<hotname.Data()<<endl;
  ofstream ofh;
  ofh.open(hotname.Data(), ios::out);
  ofh<<"#layer\t" << "ladder\t" << "south_north\t" << "module\t" << "ROC\t" << "col\t" << "row\t" <<endl;

  TString deadname = Form("%s/%s", outdir, gSystem->BaseName(fname));
  deadname.ReplaceAll(".root", "_dead.txt");
  cout<<"Write to dead map file : "<<deadname.Data()<<endl;
  ofstream ofd;
  ofd.open(deadname.Data(), ios::out);
  ofd<<"#layer\t" << "ladder\t" << "south_north\t" << "module\t" << "ROC\t" << "col\t" << "row\t" <<endl;
  */

  int run  = h_run->GetBinContent(1);
  int nevt = h_evt->GetEntries();
  int nhot[2][2]={{0,0},{0,0}}; // layer, W/E
  int ndead[2][2]={{0,0},{0,0}}; // layer, W/E

  cout<<run<<" "<<seq<<" "<<nevt<<endl;
  TGraph *g_hot[NMODULE][NCHIP];
  TGraph *g_dead[NMODULE][NCHIP];
  TGraph2D *g_rate[NMODULE][NCHIP];
  TGraph *g_hotcol[NMODULE][NCHIP];
  TGraph *g_deadcol[NMODULE][NCHIP];
  TH2F   *h_hotdeadmap[NMODULE][NCHIP];
  for(int i=0; i<4; i++){
    float hotlimit = (i<2) ? 0.01 : 0.003;
    for(int imod=modsta[i]; imod<modsta[i+1]; imod++){
      for(int ichip=0; ichip<8; ichip++){
      for (int il=0;il<2;il++) {
	for (int we=0; we<2; we++) {
	  nhot[il][we] = 0;
	  ndead[il][we] = 0;
	}
      }

        findChipHotDead(h_hitmap[imod][ichip], imod, ichip, nevt, hotlimit, cout, cout, cout, nhot, ndead,
                        &g_hot[imod][ichip], &g_dead[imod][ichip], &g_rate[imod][ichip],
                        &g_hotcol[imod][ichip], &g_deadcol[imod][ichip]);
        h_hotdeadmap[imod][ichip] = 
          confirmHotDead(h_hitmap[imod][ichip], g_hot[imod][ichip], g_dead[imod][ichip]);
      }
    }
  }


  TString s_outdir("./");
//  if(s_outdir.Length()==0) s_outdir.Append("./");

  // draw
  TCanvas *c1 = new TCanvas("c1", "c1", 600,600);
  c1->Divide(2,2);
  for(int i=0; i<4; i++){
    c1->cd(i+1);
    gPad->SetLogy();
    h_rate_sum[i]->SetAxisRange(0.0, 0.02);
    h_rate_sum[i]->Draw();
  }
  c1->Print(Form("%s/onlmon_pixel_rate_%06d_%04d.png", s_outdir.Data(), run, seq)); 


  TCanvas *c2 = new TCanvas("c2", "c2", 600,600);
  c2->Divide(2,2);
  for(int i=0; i<4; i++){
    c2->cd(i+1);
    gPad->SetLogy();
    h_ratew_sum[i]->SetAxisRange(0.0, 0.2);
    h_ratew_sum[i]->Draw();
  }
  c2->Print(Form("%s/onlmon_pixel_ratew_%06d_%04d.png", s_outdir.Data(), run, seq)); 

  HistoContainer cnt, cnt1;
  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<8; ichip++){
      int maxcount = nevt * ((imod<20) ? 0.01 : 0.003);
      h_hitmap[imod][ichip]->SetMaximum(maxcount);
      cnt.h_hist[imod][ichip] = h_hitmap[imod][ichip];
      cnt1.h_hist[imod][ichip] = h_hotdeadmap[imod][ichip];
    }
  }

  // c3
  TCanvas* c3 = new TCanvas("c3", "c3", 1000, 700);
  drawPixelHitmap(c3, run, seq, nevt, &cnt);
  c3->Print(Form("%s/onlmon_pixel_hitmap_%06d_%04d.png", s_outdir.Data(), run, seq)); 

  TCanvas* c4 = new TCanvas("c4", "c4", 1000, 700);
  drawPixelHitmap(c4, run, seq, nevt, &cnt1);
  c4->Print(Form("%s/onlmon_pixel_hotdeadmap_%06d_%04d.png", s_outdir.Data(), run, seq)); 

}

/*
void findChipHotDead(TH2F *h_map, int imod, int ichip, int nevt, float hotlimit, ostream& os, 
                     TGraph** g_hot, TGraph** g_dead, TGraph** g_hotcol, TGraph** g_deadcol){

  *g_hot     = new TGraph(); (*g_hot)    ->SetName(Form("g_hot_%d_%d", imod, ichip));
  *g_dead    = new TGraph(); (*g_dead)   ->SetName(Form("g_dead_%d_%d", imod, ichip));
  *g_hotcol  = new TGraph(); (*g_hotcol) ->SetName(Form("g_hotcol_%d_%d", imod, ichip));
  *g_deadcol = new TGraph(); (*g_deadcol)->SetName(Form("g_deadcol_%d_%d", imod, ichip));
  cout<<"hotlimit:"<<hotlimit<<endl;

  vector<int> vhot;
  vector<int> vdead;
  int n=0;
  for(int icol=0; icol<32; icol++){
    vhot.clear();
    vdead.clear();
    for(int irow=0; irow<256; irow++){
      //if(icol==1) cout<<irow<<":"<<icol<<" "<<flush;
      int ent = h_map->GetBinContent(icol+1, irow+1);
      float rate = float(ent)/nevt;

      // hotchannel search
      if(hotlimit < rate) { // MAXRATE is now roughly determined. should be determined by more proper method
        vhot.push_back(irow);
        n = (*g_hot)->GetN();
        (*g_hot)->SetPoint(n, icol, irow);
      }
      // dead channel search
      if(ent==0){
        // os<<irow<<" ";
        vdead.push_back(irow);
        n = (*g_dead)->GetN();
        (*g_dead)->SetPoint(n, icol, irow);
      }
    }

    // write hot & dead channel
    if(vhot.size()>0||vdead.size()==256){
      os<<setw(3)<<imod<<" "<<setw(3)<<ichip<<" "<<setw(3)<<icol<<" ";

      // write hotchannel
      os<<" |  ";
      if(vhot.size()>0){
        os<< vhot.size()<<" {";
        for(unsigned int i=0; i<vhot.size(); i++){ os<<vhot[i]<<" "; }
        os<<"} ";
      }

      // write deadchannel
      os<<" | ";
      if(vdead.size()>0){
        os<< vdead.size();
        //os<<" {";
        //for(unsigned int i=0; i<vdead.size(); i++){ os<<vdead[i]<<" "; }
        //os<<"}";
      }
      os<<endl;
    }

    // check hot & dead column
    if(vhot.size()==256) {
      n = (*g_hotcol)->GetN();
      (*g_hotcol)->SetPoint(n, icol, 1);
    }
    if(vdead.size()==256) {
      n = (*g_deadcol)->GetN();
      (*g_deadcol)->SetPoint(n, icol, 1);
    }
  }
  //cout<<"Nhot : "<<(*g_hotcol)->GetN()<<" Ndead : "<<(*g_deadcol)->GetN()<<endl;

}

TH2F* confirmHotDead(TH2F *h_map, TGraph *g_hot, TGraph *g_dead){
  TH2F *h_hotdead = (TH2F*)h_map->Clone();
  TString s(h_hotdead->GetName());
  s.ReplaceAll("hitmap", "hotdead");
  h_hotdead->Reset();
  h_hotdead->SetMaximum(2.0);

  for(int irow=0; irow<256; irow++){
    for(int icol=0; icol<32; icol++){
      h_hotdead->SetBinContent(icol+1, irow+1, 1.0);
    }
  }

  cout<<"Nhot:Ndead "<<g_hot->GetN()<<" "<<g_dead->GetN()<<endl;

  // hot
  double* x = NULL;
  double* y = NULL;
  x = g_hot->GetX();
  y = g_hot->GetY();
  for(int i=0; i<g_hot->GetN(); i++){
    h_hotdead->SetBinContent(x[i]+1, y[i]+1, 2.0);
  }

  // dead
  x = g_dead->GetX();
  y = g_dead->GetY();
  for(int i=0; i<g_dead->GetN(); i++){
    h_hotdead->SetBinContent(x[i]+1, y[i]+1, 0.0);
  }

  return h_hotdead;
}

void drawPixelHitmap(TCanvas *c1, int run, int seq, int nevt, HistoContainer* cnt){
  if(c1==NULL){
    return;
  }
  //cout<<"srun : "<<s_run->Data()<<endl;

//  TH2F*** h_hist = (TH2F***) ptr;

  TText *sSide[4];
  sSide[0] = new TText(0.15, 0.66, "South");
  sSide[1] = new TText(0.37, 0.66, "North");
  sSide[2] = new TText(0.62, 0.66, "North");
  sSide[3] = new TText(0.82, 0.66, "South");
  for(int i=0; i<4; i++){
    sSide[i]->SetNDC();
    sSide[i]->SetTextSize(0.03);
  }


  TGaxis *ax[4];
  ax[0] = new TGaxis(0.255, 0.03, 0.065, 0.03, 0, 7, 10, "BS");
  ax[1] = new TGaxis(0.475, 0.03, 0.285, 0.03, 0, 7, 10, "BS");
  ax[2] = new TGaxis(0.715, 0.03, 0.525, 0.03, 0, 7, 10, "BS");
  ax[3] = new TGaxis(0.935, 0.03, 0.74,  0.03, 0, 7, 10, "BS");
  for(int ia=0; ia<4; ia++){
    ax[ia]->SetTickSize(0.0);
    ax[ia]->SetLabelSize(0.03);
  }

  TGaxis *ax3[4];
  ax3[0] = new TGaxis(0.03, 0.925, 0.0301, 0.73,  0,  4,  5, "BSI");
  ax3[1] = new TGaxis(0.98, 0.925, 0.9801, 0.73,  5,  9,  5, "BSI");
  ax3[2] = new TGaxis(0.03, 0.61, 0.0301, 0.08,  0,  9, 10, "BSI");
  ax3[3] = new TGaxis(0.98, 0.61, 0.9801, 0.08, 10, 19, 10, "BSI");
  for(int ia=0; ia<4; ia++){
    ax3[ia]->SetTickSize(0.0);
    ax3[ia]->SetLabelSize(0.03);
  }

  // 
  TString s[4]={"WestB0", "EastB0", "WestB1", "EastB1"};

  c1->cd();
  TPad *p[4];
  float padx1[4]={0.05, 0.51, 0.05, 0.51};
  float padx2[4]={0.49, 0.95, 0.49, 0.95};
  float pady1[4]={0.70, 0.70, 0.05, 0.05};
  float pady2[4]={0.95, 0.95, 0.65, 0.65};
  char tmp[256];
  int modmax[4]={10, 10, 20, 20};
  int modoffset = 0;

//  float maxcount=MAXRATE*nevt;

  float maxcount=0.01;
  for(int i=0; i<4; i++){
    c1->cd();
    sprintf(tmp, "p%d", i);
    p[i] = new TPad(tmp, s[i].Data(), padx1[i], pady1[i], padx2[i], pady2[i]);
    if(i<2) p[i]->Divide(16, 5, 0.0, 0.0);
    else    p[i]->Divide(16, 10, 0.0, 0.0);
    p[i]->Draw();

    if(i<2){
      float x1 = (i==0)? 0.05 : 0.55;
      TPaveText *ptitle = new TPaveText(x1, 0.96, x1+0.4, 0.99);
      ptitle->AddText(Form("%s %d-%04d %d", s[i].Data(), run, seq, nevt));
      ptitle->Draw();
    }
//    maxcount = nevt * ((i<2) ? 0.01 : 0.003);

    //-- decolation
    for(int is=0; is<4; is++) sSide[is]->Draw();

    for(int ia=0; ia<4; ia++){ ax[ia]->Draw(); }

    for(int ia=0; ia<4; ia++){ ax3[ia]->Draw(); }

    //-- decolation end

    gStyle->SetPalette(1);


    for(int imod=0; imod<modmax[i]; imod++){
      for(int ichip=0; ichip<NCHIP; ichip++){
        int halfmodmax = (modmax[i]/2);
        int offset = 0;
        if(i%2==0) offset = (imod<halfmodmax) ?  0 : -8;
        else       offset = (imod<halfmodmax) ? -8 :  0;
        int ic = 16*((imod%halfmodmax)+1) -ichip + offset;

//        int offset = (imod%2==0) ? 8 : -8;
//        int ic = 8*imod + (7-ichip) + offset +1;
        //cout<<imod<<" "<<ichip<<" "<<ic<<endl;
        p[i]->cd(ic);
        gPad->SetLeftMargin(0.0);
        gPad->SetRightMargin(0.0);
        gPad->SetTopMargin(0.0);
        gPad->SetBottomMargin(0.0);

        int imodd = imod + modoffset; // 0-9,10-19,20-39,40-59
        //if(imod==modmax[i]-1||imod==modmax[i]-2||ichip==7) 
//          cnt->h_hist[imodd][ichip]->SetMaximum(maxcount);
          cnt->h_hist[imodd][ichip]->Draw("colz");

//          cout<<imod<<" "<<ichip<<" "<<cnt->h_hist[imodd][ichip]->GetMaximum()<<endl;

      }
    }

    modoffset += modmax[i];
  }
}

int getSeqNumber(const char *fname){
  TRegexp reg("[0123456789+]");
  TString s_seq(fname);

  int pos = s_seq.Index(reg, 0);
  pos = s_seq.Index("_", pos);
  s_seq.Remove(0, pos+1);
  pos = s_seq.Index(".", 0);
  s_seq.Remove(pos);
  pos = s_seq.Index("_", 0);
  s_seq.Remove(pos);

  int seq = (s_seq.IsDigit()) ? s_seq.Atoi() : 9999;
  //cout<<fname<<" "<<s_seq<<" "<<seq<<endl;

  return seq;
}
*/
