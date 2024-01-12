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
#include <TGraph2D.h>
#include <TGaxis.h>
#include <TRegexp.h>

#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>

#include <svxAddress.hh>

using namespace std;

static const int NMODULE=60;
static const int NCHIP  =8;
static const unsigned int NROW   =256;
static const int NCOLUMN=32;

class HistoContainer {
  public:
    TH2* h_hist[NMODULE][NCHIP];
};


void findChipHotDead(TH2F *h_map, int imod, int ichip, int nevt, float hotlimit, ostream& oshot, ostream& osdead, ostream& osrate, int nhot[2][2], int ndead[2][2],
                     TGraph** g_hot, TGraph** g_dead, TGraph2D** g_rate, TGraph** g_hotcol, TGraph** g_deadcol){

  *g_hot     = new TGraph(); (*g_hot)    ->SetName(Form("g_hot_%d_%d", imod, ichip));
  *g_dead    = new TGraph(); (*g_dead)   ->SetName(Form("g_dead_%d_%d", imod, ichip));
  *g_rate    = new TGraph2D(); (*g_rate)   ->SetName(Form("g_rate_%d_%d", imod, ichip));
  *g_hotcol  = new TGraph(); (*g_hotcol) ->SetName(Form("g_hotcol_%d_%d", imod, ichip));
  *g_deadcol = new TGraph(); (*g_deadcol)->SetName(Form("g_deadcol_%d_%d", imod, ichip));

  const int SOUTH=0;
  const int NORTH=1;
  const int WEST=0;
  const int EAST=1;

  svxAddress address = svxAddress::getInstance();

  const int layer = address.getPixelLayer(imod);
  //  const int ladder = address.getPixelLadder(imod);
  int south_north = SOUTH;
  int west_east = WEST;

  if (imod<10) {
    west_east = WEST;
  } else if (imod<20) {
    west_east = EAST;
  } else if (imod<40) {
    west_east = WEST;
  } else {
    west_east = EAST;
  }

  if (layer==0) {
    south_north = 1-((imod/5)%2);
  } else {
    south_north = 1-((imod/10)%2);
  }

  //    oshot<<"#layer\t" << "ladder\t" << "south_north\t" << "module\t" << "ROC\t" << "col\t" << "row\t" <<endl;
  //    osdead<<"#layer\t" << "ladder\t" << "south_north\t" << "module\t" << "ROC\t" << "col\t" << "row\t" <<endl;


  for (int il=0; il<2; il++) {
    for (int we=0; we<2; we++) {
      nhot[il][we] = 0;
      ndead[il][we] = 0;
    }
  }
  vector<int> vhot;
  vector<int> vdead;
  int n=0;
  for(int icol=0; icol<NCOLUMN; icol++){
    vhot.clear();
    vdead.clear();
    for(unsigned int irow=0; irow<NROW; irow++){
      //if(icol==1) cout<<irow<<":"<<icol<<" "<<flush;
      int ent = h_map->GetBinContent(icol+1, irow+1);
      double rate = (double)ent/(double)nevt;

      // hotchannel search
      int pixel_stat=1;//normal
      if(hotlimit < rate) { // MAXRATE is now roughly determined. should be determined by more proper method
        vhot.push_back(irow);
        n = (*g_hot)->GetN();
        (*g_hot)->SetPoint(n, icol, irow);
	//	oshot<<layer<<"\t"<<ladder<<"\t"<<south_north<<"\t"<<imod<<"\t"<<ichip<<"\t"<<icol<<"\t"<<irow<<endl;
	oshot<<imod<<"\t"<<ichip<<"\t"<<icol<<"\t"<<irow<<endl;
	nhot[layer][west_east] ++;
	pixel_stat = 2;
      }
      // dead channel search
      if(ent==0){
        vdead.push_back(irow);
        n = (*g_dead)->GetN();
        (*g_dead)->SetPoint(n, icol, irow);
	//	osdead<<layer<<"\t"<<ladder<<"\t"<<south_north<<"\t"<<imod<<"\t"<<ichip<<"\t"<<icol<<"\t"<<irow<<endl;
	osdead<<imod<<"\t"<<ichip<<"\t"<<icol<<"\t"<<irow<<endl;
	ndead[layer][west_east] ++;
	pixel_stat = 0;
      }
      //      osrate<<imod<<"\t"<<ichip<<"\t"<<icol<<"\t"<<irow<<"\t"<<rate<<endl;

      int nr = (*g_rate)->GetN();
      (*g_rate)->SetPoint(nr, icol, irow, rate);
      osrate<<imod<<"\t"<<ichip<<"\t"<<icol<<"\t"<<irow<<"\t"<<pixel_stat<<"\t";
      osrate.precision(6);
      osrate.setf(ios::scientific,ios::floatfield);
      osrate<<rate<<endl;
      osrate.unsetf(ios::scientific);
      
      //      cout<<imod<<"\t"<<ichip<<"\t"<<icol<<"\t"<<irow<<"\t";
      //      cout.precision(6);
      //      cout.setf(ios::scientific,ios::floatfield);
      //      cout<<rate<<endl;
      //      cout.unsetf(ios::scientific);
      
    }

    // check hot & dead column
    if(vhot.size()==NROW) {
      n = (*g_hotcol)->GetN();
      (*g_hotcol)->SetPoint(n, icol, 1);
    }
    if(vdead.size()==NROW) {
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
  h_hotdead->SetName(s.Data());
  h_hotdead->Reset();
  h_hotdead->SetMaximum(2.0);
//  h_hotdead->SetMinimum(0.0);

  for(unsigned int irow=0; irow<NROW; irow++){
    for(int icol=0; icol<NCOLUMN; icol++){
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

void drawPixelHitmap(TCanvas *c1, int run, int seq, int nevt, HistoContainer* cnt, float hot[2][2], float dead[2][2]){
  if(c1==NULL){
    return;
  }
  //cout<<"srun : "<<s_run->Data()<<endl;

  TText *sSide[4];
  //  sSide[0] = new TText(0.15, 0.67, "South");
  //  sSide[1] = new TText(0.37, 0.67, "North");
  //  sSide[2] = new TText(0.62, 0.67, "North");
  //  sSide[3] = new TText(0.82, 0.67, "South");
  sSide[0] = new TText(0.132, 0.59, "South");
  sSide[1] = new TText(0.35, 0.59, "North");
  sSide[2] = new TText(0.58, 0.59, "North");
  sSide[3] = new TText(0.83, 0.59, "South");
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
  //  ax3[0] = new TGaxis(0.03, 0.73,  0.0301, 0.925,  0,  4,  5, "BSI");
  //  ax3[1] = new TGaxis(0.98, 0.925, 0.9801, 0.73,  5,  9,  5, "BSI");
  //  ax3[2] = new TGaxis(0.03, 0.08,  0.0301, 0.625,  0,  9, 10, "BSI");
  //  ax3[3] = new TGaxis(0.98, 0.625, 0.9801, 0.08, 10, 19, 10, "BSI");
  ax3[0] = new TGaxis(0.03, 0.65,  0.0301, 0.845,  0,  4,  5, "BSI");
  ax3[1] = new TGaxis(0.98, 0.845, 0.9801, 0.65,  5,  9,  5, "BSI");
  ax3[2] = new TGaxis(0.03, 0.08,  0.0301, 0.545,  0,  9, 10, "BSI");
  ax3[3] = new TGaxis(0.98, 0.545, 0.9801, 0.08, 10, 19, 10, "BSI");
  for(int ia=0; ia<4; ia++){
    ax3[ia]->SetTickSize(0.0);
    ax3[ia]->SetLabelSize(0.03);
  }

  // 
  TString s[4]={"WestB0", "EastB0", "WestB1", "EastB1"};

  c1->cd();
  TPad *p[4];
  //  float padx1[4]={0.05, 0.51, 0.05, 0.51};
  //  float padx2[4]={0.49, 0.95, 0.49, 0.95};
  //  float pady1[4]={0.69, 0.69, 0.05, 0.05};
  //  float pady2[4]={0.95, 0.95, 0.67, 0.67};

  float padx1[4]={0.05, 0.51, 0.05, 0.51};
  float padx2[4]={0.49, 0.95, 0.49, 0.95};
  float pady1[4]={0.61, 0.61, 0.05, 0.05};
  float pady2[4]={0.87, 0.87, 0.59, 0.59};

  for(int i=0; i<4; i++){
    c1->cd();
    p[i] = new TPad(Form("p%d",i), s[i].Data(), padx1[i], pady1[i], padx2[i], pady2[i]);
    if(i<2) p[i]->Divide(16, 5, 0.0, 0.0);
    else    p[i]->Divide(16, 10, 0.0, 0.0);
    p[i]->Draw();

    if(i<2){
      float x1 = (i==0)? 0.07 : 0.53;
      TPaveText *ptitle = new TPaveText(x1, 0.92, x1+0.4, 0.95);
      if (i==0) {
	ptitle->AddText(Form("West"));
      } else {
	ptitle->AddText(Form("East"));
      }
      if (i==0) {
	TPaveText *ptitle0 = new TPaveText(0.30, 0.96, 0.70, 0.99);
	ptitle0->SetTextSize(0.03);
	ptitle0->AddText(Form("Run: %d  Seq: %04d   Nevent : %d", run, seq, nevt));
	ptitle0->SetFillColor(0);
	ptitle0->SetLineColor(0);
	ptitle0->SetBorderSize(0);
	ptitle0->Draw();
      }

      ptitle->SetFillColor(0);
      ptitle->SetLineColor(0);
      ptitle->SetBorderSize(0);
      ptitle->Draw();

      TPaveText *ptitleh = new TPaveText(x1, 0.90, x1+0.4, 0.92);
      ptitleh->SetTextSize(0.02);
      if (i==0) {
	ptitleh->AddText(Form("Hot:   B0W %1.3f     B1W %1.3f",
			      hot[0][0],hot[1][0]));
      } else {
	ptitleh->AddText(Form("Hot:   B0E %1.3f     B1E %1.3f",
			      hot[0][1],hot[1][1]));
      }
      ptitleh->SetFillColor(0);
      ptitleh->SetLineColor(0);
      ptitleh->SetBorderSize(0);
      ptitleh->Draw();

      TPaveText *ptitled = new TPaveText(x1, 0.88, x1+0.4, 0.90);
      ptitled->SetTextSize(0.02);
      if (i==0) {
	ptitled->AddText(Form("Dead: B0W %1.3f     B1W %1.3f",
			      dead[0][0],dead[1][0]));
      } else {
	ptitled->AddText(Form("Dead: B0E %1.3f     B1E %1.3f",
			      dead[0][1],dead[1][1]));
      }
      ptitled->SetFillColor(0);
      ptitled->SetLineColor(0);
      ptitled->SetBorderSize(0);
      ptitled->Draw();
    }

    //-- decolation
    for(int is=0; is<4; is++) sSide[is]->Draw();

    for(int ia=0; ia<4; ia++){ ax[ia]->Draw(); }

    for(int ia=0; ia<4; ia++){ ax3[ia]->Draw(); }

    //-- decolation end
  }

  gStyle->SetPalette(1);


  for(int imod=0; imod<NMODULE; imod++){

    int ip = 0;
    if     (imod<10){ ip = 0;}
    else if(imod<20){ ip = 1;}
    else if(imod<40){ ip = 2;}
    else            { ip = 3;}

    int ican=0; 
    if     (imod< 5) ican = 10-(2*(imod-0));
    else if(imod<10) ican =  9-(2*(imod-5));

    else if(imod<15) ican = 2*(imod-10)+1;
    else if(imod<20) ican = 2*(imod-15)+2;

    else if(imod<30) ican = 20-(2*(imod-20));
    else if(imod<40) ican = 19-(2*(imod-30));

    else if(imod<50) ican = 2*(imod-40)+1;
    else             ican = 2*(imod-50)+2;

    for(int ichip=0; ichip<NCHIP; ichip++){
      TPad *pp = (TPad*)p[ip]->cd(8*ican-ichip);
      cnt->h_hist[imod][ichip]->Draw("colz");
      pp->Modified();
      pp->Update();
    }
    p[ip]->Modified();
    p[ip]->Update();
  }

}

int getSeqNumber(const char *fname){
  TRegexp reg("[0123456789+]");

  TString s_seq(gSystem->BaseName(fname));
//  cout<<s_seq<<endl;
//  for(int i=0; i<100; i++) cout<<i%10;
//  cout<<endl;

  int pos = s_seq.Index(reg, 0); //cout<<"pos "<<pos<<endl;
  pos = s_seq.Index("_", pos);   //cout<<"pos "<<pos<<endl;
  s_seq.Remove(0, pos+1);

  pos = s_seq.Index(".", 0);     //cout<<"1 "<<s_seq<<" "<<pos<<endl;
  s_seq.Remove(pos);

  pos = s_seq.Index("_", 0); if(pos>0) {s_seq.Remove(pos);  cout<<"2 "<<s_seq<<endl;}

  int seq = (s_seq.IsDigit()) ? s_seq.Atoi() : 9999;
  //cout<<fname<<" "<<s_seq<<" "<<seq<<endl;

  return seq;
}


int getRunNumber(const char *fname){
  TRegexp reg("[0123456789+]");

  TString s_run(gSystem->BaseName(fname));
  cout<<s_run<<endl;
  for(int i=0; i<100; i++) cout<<i%10;
  cout<<endl;

  int pos = s_run.Index(reg, 0); //cout<<"pos "<<pos<<endl;
  s_run.Remove(0, pos);
  cout<<s_run<<endl;

  pos = s_run.Index(".", 0);     //cout<<"1 "<<s_run<<" "<<pos<<endl;
  s_run.Remove(pos);

  pos = s_run.Index("_", 0); if(pos>0) {s_run.Remove(pos);  cout<<"2 "<<s_run<<endl;}

  int run = (s_run.IsDigit()) ? s_run.Atoi() : 9999;
  //cout<<fname<<" "<<s_run<<" "<<run<<endl;

  return run;
}
