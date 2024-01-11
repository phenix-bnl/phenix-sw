#include <iomanip>
#include <iostream>

void DrawADC(int run){

  char runname[10];
  sprintf(runname"run%d",run);  

  char RootName[255];
  sprintf(RootName,"../make_nDST/DSTOut/DST_ZDCSMDPOL-0000%d-0000_ppg.root",run);

  TFile *hfile = new TFile(RootName);
  char hname[256],leafname[256];  
  TH1F *hzdc[40];
  
  double ped[40];
  for(int i=0; i<40; i++)
    ped[i] = 0;
  
  TCanvas *pict1 = new TCanvas("pict1","pict1",800,400);  
  TCanvas *pict2 = new TCanvas("pict2","pict2",1000,1000);  
  TCanvas *pict3 = new TCanvas("pict3","pict3",1000,1000);  
  
  for(int i=0;i<40;i++)
    {
      sprintf(hname,"ch%d",i);
      if( i< 8)
	hzdc[i] = new TH1F(hname,hname,1000,300,1300);
      else
	hzdc[i] = new TH1F(hname,hname,1000,300,1300);
      
      sprintf(leafname,"ZdcRawHits.Adc[%d]",i); 
      // sprintf(leafname,"ZdcRawHits.Adc[%d]-%f",i,ped[i]); 
    
      T -> Project(hname,leafname,"(lvl1_trigscaled&0x40000000)>0");	//these are the LED triggers
      // T -> Project(hname,leafname,"(lvl1_trigscaled&0x10000000)>0");	//these are the PED triggers
      
    }
  hzdc[0]-> SetTitle(RootName); 
  
 
  pict1 -> Clear();
  pict1 -> Divide(4,2);

  for(int i=0;i<8;i++)
    {
      pict1 -> cd(i+1);
      gPad -> SetLogy(1);
      hzdc[i]->SetLineColor(2);
      hzdc[i] -> Draw(); 
    }

 
  pict2 -> Clear();
  pict2 -> Divide(4,4);

  for(int i=8;i<24;i++)
    {
      pict2 -> cd(i-7);
      gPad -> SetLogy(1);
      hzdc[i]->SetLineColor(4);
      hzdc[i] -> Draw();
    }

  pict3 -> Clear();
  pict3 -> Divide(4,4);

  for(int i=24;i<40;i++)
    {
      pict3 -> cd(i-23);
      gPad -> SetLogy(1);
      hzdc[i]->SetLineColor(4);
      hzdc[i] -> Draw();
    }

  pict1->Print("led_adc.pdf(","pdf");
  pict2->Print("led_adc.pdf", "pdf");
  pict3->Print("led_adc.pdf)","pdf");

  char fname[100];
  sprintf(fname,"ZdcCalib.ledmean_%d",runname);
  FILE* fp = fopen(fname);
  for(int i=0;i<40;i++)
    {
      fprintf(fp,"%.1ld\t%.1ld\t0\n", hzdc[i]->GetMean(), hzdc[i]->GetRMS());
    }
  fclose(fp);
}
