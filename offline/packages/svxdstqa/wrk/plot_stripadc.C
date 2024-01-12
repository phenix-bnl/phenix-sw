#include <TROOT.h>
#include <TStyle.h>
#include <TDirectory.h>
#include <TFile.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TPad.h>


#include <sstream>
#include <iostream>

using namespace std;

static const int NMODULE=40;
static const int NRCC   =6;
static const int NCHIP  =12;

void plot_stripadc(){
//  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);

  //const char *fname = "anahist_org.root";
  const char *fname = "sim_merge_strip.root";

  TDirectory *gDir = gDirectory;

  TH2F *h_adcmap[NMODULE][NRCC][NCHIP];
  TH2F *h_hitmap[NMODULE][NRCC][2];

  float max=0;
  TFile *f = TFile::Open(fname);
  gDirectory = gDir;
  ostringstream sname;
  for(int imod=0;imod<NMODULE; imod++){
    for(int ircc=0;ircc<NRCC; ircc++){
      for(int ichip=0;ichip<NCHIP; ichip++){
        sname.str("");
        sname<<"h_stp_adcmap_"<<imod<<"_"<<ircc<<"_"<<ichip;
//        h_adcmap[imod][ircc][ichip] = (TH2F*)f->Get(sname.str().c_str())->Clone();
      }
      for(int ilr=0;ilr<2; ilr++){
        sname.str("");
        sname<<"h_stp_hitmap_"<<imod<<"_"<<ircc<<"_"<<ilr;
        h_hitmap[imod][ircc][ilr] = (TH2F*)f->Get(sname.str().c_str())->Clone();
        float mm = h_hitmap[imod][ircc][ilr]->GetMaximum();
        if(max<mm) max=mm;
      }
    }
  }

  f->Close();


  int modary[4]={8, 8, 12, 12};
  int modoffset=0;
/*
  TCanvas *c1 = new TCanvas("c1", "c1", 600, 700);
  //TPad *p1 = new TPad(0.05, 0.05, 0.95, 0.95);
  
  gStyle->SetPalette(1);

  for(int ic=0; ic<4; ic++){
    c1->Clear();
    int nrcc=5;
    if(ic<2) {
       c1->Divide(5,8, 0.002, 0.003);
       nrcc=5;
    }
    else {
      c1->Divide(6,12, 0.002, 0.003);
      nrcc=6;
    }

    for(int imod=0; imod<modary[ic]; imod++){
      int imodd = imod+modoffset;
      cout<<ic<<" "<<imod<<" "<<imodd<<endl;
      for(int ircc=0; ircc<nrcc; ircc++){
        TPad *p = (TPad*)c1->cd(imod*nrcc+ircc+1);
        p->Divide(6,2, 0, 0);
        for(int ichip=0; ichip<NCHIP; ichip++){
          int ican = (ichip<6) ? ichip+1 : 18-ichip;
          TPad* pp = (TPad*)p->cd(ican);
          //h_adcmap[imodd][ircc][ichip]->Draw("colz");
        }
      }
    }
    modoffset+=modary[ic];

    c1->Print(Form("plot_stripadc_c1_%d.png", ic));
  }
*/

  TCanvas *c2 = new TCanvas("c2", "c2", 1100, 650);
  TPad *p1[4];
  p1[0] = new TPad("p1_0", "p1_0", 0.01,  0.70, 0.495, 0.99);
  p1[1] = new TPad("p1_1", "p1_1", 0.505, 0.70, 0.99,  0.99);
  p1[2] = new TPad("p1_2", "p1_2", 0.01,  0.01, 0.495, 0.68);
  p1[3] = new TPad("p1_3", "p1_3", 0.505, 0.01, 0.99,  0.68);
  for(int ip=0; ip<4; ip++) p1[ip]->Draw();
  p1[0]->Divide(6, 8, 0.000 , 0.000);
  p1[1]->Divide(6, 8, 0.000 , 0.000);
  p1[2]->Divide(6, 12, 0.000 , 0.000);
  p1[3]->Divide(6, 12, 0.000 , 0.000);

  //c2->Divide(12,4, 0.002 , 0.001);
  gStyle->SetPalette(1);

  for(int imod=0;imod<40; imod++){
    int ic = 0;
    int ip = 0;
    if(imod<8)       {ip=0; ic=6*(7-imod)+1;}
    else if(imod<16) {ip=1; ic=6*(7-(imod-8))+1;}
    else if(imod<28) {ip=2; ic=6*(11-(imod-16))+1;}
    else             {ip=3; ic=6*(11-(imod-28))+1;}

    //TPad* p2 = (TPad*)c2->cd(ic);
    //p2->Divide(6,1,0.0, 0.0);

    int nrcc = (imod<16) ? 5 : 6;
    //cout<<ip<<" "<<ic<<endl;


    p1[ip]->cd();

    for(int ircc=0;ircc<nrcc; ircc++){
      TPad* pp2 = (TPad*)p1[ip]->cd(ic+ircc); //(TPad*)p2->cd(ircc+1);
      //cout<<" "<<ic+ircc<<endl;
      pp2->Divide(2,1,0.0, 0.0);
      for(int ilr=0;ilr<2; ilr++){
        pp2->cd(ilr+1);
        h_hitmap[imod][ircc][ilr]->SetMaximum(1.2*mm);
        h_hitmap[imod][ircc][ilr]->Draw("colz");
        //cout<<"  "<<ilr<<endl;
      }
      pp2->Modified();
      pp2->Update();
    }
  }
  c2->Print("plot_stripadc_c2.png");
  
  
}
