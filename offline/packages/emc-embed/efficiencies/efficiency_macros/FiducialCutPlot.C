#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TDirectory.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TPaveText.h"

void FiducialCutPlot(char * inputfile = "/afs/rhic/phenix/users/enterria/efficiency3_644_2nd.root") 
{

//   //  gROOT->Reset();
  
  //gROOT->SetStyle("Plain");
  
  Int_t i;
  char sector[3] = "W0";
  
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TFile * in = new TFile(inputfile);
  
  TH2F * frame = new TH2F("frame","frame",10,-30.,430.,10,-10,210);
  frame->SetStats(0);

  TH2F * hyzlocal[8];
  TBox * Acceptance[8];
  TDirectory * FiduCut;
  TDirectory * C10;

  FiduCut = (TDirectory*)  in->Get("FiduNoW3DeadWarnEnergyCut");
  FiduCut->cd();   
  C10 =  (TDirectory*)  gDirectory->Get("C10");
  C10->cd() ;
  hyzlocal[0] = (TH2F*) gDirectory->Get("hyzlocal_W3");
  hyzlocal[1] = (TH2F*) gDirectory->Get("hyzlocal_W2");
  hyzlocal[2] = (TH2F*) gDirectory->Get("hyzlocal_W1");
  hyzlocal[3] = (TH2F*) gDirectory->Get("hyzlocal_W0");
  hyzlocal[4] = (TH2F*) gDirectory->Get("hyzlocal_E3");
  hyzlocal[5] = (TH2F*) gDirectory->Get("hyzlocal_E2");
  hyzlocal[6] = (TH2F*) gDirectory->Get("hyzlocal_E1");
  hyzlocal[7] = (TH2F*) gDirectory->Get("hyzlocal_E0");
  
  TCanvas * WestArm = new TCanvas("WestArm","WestArm", 350,800);
  WestArm->SetObjectStat(0);  
  WestArm->Divide(1,4);
  for(i=0; i<4; i++) {
    sprintf(sector,"W%d",i);
    WestArm->cd((i+1));
    hyzlocal[i]->SetStats(0);
    //hyzlocal[i]->SetTitleSize(4.);
    hyzlocal[i]->SetAxisRange(-30, 430,"x");
    hyzlocal[i]->SetAxisRange(-10, 210,"y");
    hyzlocal[i]->SetTitle(sector);
    hyzlocal[i]->GetXaxis()->SetTitleOffset(1.8);
    hyzlocal[i]->GetYaxis()->SetTitleOffset(0.5);
    if( i==0) {
      hyzlocal[i]->SetTitleSize(0.08,"Y");
      hyzlocal[i]->SetYTitle("Z(cm)");
    }
    if( i==3) {
      hyzlocal[i]->SetTitleSize(0.08,"X");
      hyzlocal[i]->SetXTitle("Y(cm)");
    }
    hyzlocal[i]->Draw("col");
    Acceptance[i] = new TBox(-2.77099, -2.77099, 393.481995+2.77099 , 194.981995+2.77099 );
    Acceptance[i]->SetLineColor(2);
    Acceptance[i]->SetLineWidth(2);
    Acceptance[i]->SetFillStyle(0);
    Acceptance[i]->Draw();
  }
  WestArm->SaveAs("WestArm.eps");
  
  TCanvas * EastArm = new TCanvas("EastArm","EastArm", 350,800);
  EastArm->SetObjectStat(0);  
  EastArm->Divide(1,4);
  for(i=4; i<8; i++) {
    sprintf(sector,"E%d",i-4);
    EastArm->cd((i-3));
    if (i==4 || i==5 ) {
      hyzlocal[i]->SetStats(0);
      //  hyzlocal[i]->SetTitleSize(4.);
      hyzlocal[i]->SetAxisRange(-30, 430,"x");
      hyzlocal[i]->SetAxisRange(-10, 210,"y");
      hyzlocal[i]->SetTitle(sector);
      hyzlocal[i]->GetXaxis()->SetTitleOffset(1.8); 
      hyzlocal[i]->GetYaxis()->SetTitleOffset(0.5);
      hyzlocal[i]->Draw("col");
      Acceptance[i] =  new TBox(-2.77099, -2.77099, 393.481995+2.77099 , 194.981995+2.77099 );
      Acceptance[i]->SetLineColor(2);
      Acceptance[i]->SetLineWidth(2);
      Acceptance[i]->SetFillStyle(0);
      Acceptance[i]->Draw();
    }
    else {
      if (i==7) 
	frame->SetXTitle("Y(cm)");
      else 
	frame->SetXTitle("");
      frame->Draw();
      if (i==7) {
	TPaveText * E0 = new TPaveText(50, 65,350,150);
	E0->AddText("E0 PbGl");
	E0->Draw();
      }
      else {
	TPaveText * E1 = new TPaveText(50, 65,350,150);
	E1->AddText("E1 PbGl");
	E1->Draw();
      }
    }
      if( i==4) {
	hyzlocal[i]->SetTitleSize(0.08,"Y");
	hyzlocal[i]->SetYTitle("Z(cm)");
      }
  }
  EastArm->SaveAs("EastArm.eps");
}
