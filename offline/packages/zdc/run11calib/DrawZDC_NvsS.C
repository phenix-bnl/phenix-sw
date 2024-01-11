#include <iomanip>
#include <iostream>

void DrawZDC_NvsS(char *RootName="defalt.root"){

  TCanvas *pict1 = new TCanvas("pict1","pict1",1600,400);  
  TCanvas *pict2 = new TCanvas("pict2","pict2",1600,400);  

  TCanvas *pic[4];

  pt = new TPaveText(0.20,0.7,0.5,0.87, "NDC");
  pt->SetFillColor(0); // text is black on white
  pt->SetTextSize(0.04); 
  pt->SetTextAlign(12);
  text = pt->AddText("North=Black && South=Red");
  TFile *hfile = new TFile(RootName);
  
  TH1F *hzdc[8];
  TH1F *hzdcped[8];
  //  double pedestal[8]={0,430.8,475.8,464.9,0,438.2,445.2,425.4};
  //  double pedestal[8]={0,-7.4,30.6,39.5,0,0,0,0};
  double pedestal[8]={0,0,0,0,0,0,0,0};
  char hname[256],leafname[256];

  for(int i=0;i<8;i++){
    sprintf(hname,"hzdc%d",i); 
    hzdc[i] = new TH1F(hname,hname,1000,0,4000);
    if(i==0 || i==4)
      sprintf(leafname,"ZdcRawHits.Adc[%d]",i); 
    else
      sprintf(leafname,"ZdcRawHits.Adc[%d]-%f",i,pedestal[i]);
 
    T -> Project(hname,leafname);
  }
  hzdc[0] -> SetTitle(RootName); 

  pict1 -> Clear();
  pict1 -> Divide(4,1);
  for(int i=0;i<4;i++){
    pict1 -> cd(i+1);
    gPad -> SetLogy(1);
    hzdc[i+4] -> Draw();
    hzdc[i]->SetLineColor(2);
    hzdc[i]->Draw("same");
    pt->Draw("same");

    sprintf(hname,"pic%d",i); 
    pic[i]=new TCanvas(hname,hname,600,600);
    gPad -> SetLogy(1);
    hzdc[i+4]->Draw();
    hzdc[i]->SetLineColor(2);
    hzdc[i]->Draw("same");
    pt->Draw("same");

  }

  
  TH1F *div[4];
  pict2 -> Clear();
  pict2 -> Divide(4,1);
  for(int i=0;i<4;i++)
    {
      sprintf(hname,"SvsN%d",i); 
      div[i] = new TH1F(hname,hname,1000,0,4000);
      div[i]->Divide(hzdc[i],hzdc[i+4]);
      pict2->cd(i+1);
      div[i]->Draw();
    }
  
  //  pict1 -> Print("pict1.ps");
}
