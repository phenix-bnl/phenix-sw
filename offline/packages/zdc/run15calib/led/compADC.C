#include <iomanip>
#include <iostream>

void compADC(int run0, int run1)
{
  int run[2] = { run0, run1 };
  char runname[2][10];
  for(int i=0; i<2; i++)
    sprintf(runname[i],"run%d",run[i]);  

  char fname[2][255];
  for(int i=0; i<2; i++)
    sprintf(fname[i],"../make_nDST/DSTOut/DST_ZDCSMDPOL-0000%d-0000_ppg.root",run[i]);

  TFile* fp[2];
  for(int i=0; i<2; i++)
    fp[i]=new TFile(fname[i]);
   
  TTree* T[2];
  for(int i=0; i<2; i++)
    T[i]=(TTree*)fp[i]->Get("T");

  char hname[256],leafname[256];  
  TH1F *hzdc[2][40];
  
  double ped[40];
  for(int ich=0; ich<40; ich++)
    ped[ich] = 0;
  
  TCanvas *pict1 = new TCanvas("pict1","pict1",800,400);  
  TCanvas *pict2 = new TCanvas("pict2","pict2",1000,1000);  
  TCanvas *pict3 = new TCanvas("pict3","pict3",1000,1000);  
  
  
  for(int i=0; i<2; i++)
    for(int ich=0;ich<40;ich++)
      {
	sprintf(hname,"ch%d_run%d",ich,run[i]);
	hzdc[i][ich] = new TH1F(hname,hname,1000,300,1300);
      
	sprintf(leafname,"ZdcRawHits.Adc[%d]",ich); 
	// sprintf(leafname,"ZdcRawHits.Adc[%d]-%f",ich,ped[ich]); 
    
	T[i] -> Project(hname,leafname,"(lvl1_trigscaled&0x40000000)>0");	//these are the LED triggers
	// T[i] -> Project(hname,leafname,"(lvl1_trigscaled&0x10000000)>0");	//these are the PED triggers
      }
  
 
  pict1 -> Clear();
  pict1 -> Divide(4,2);
  
  for(int ich=0;ich<8;ich++)
    {
      pict1 -> cd(ich+1);
      gPad -> SetLogy(1);
    
      hzdc[0][ich]-> SetLineColor(2);
      hzdc[0][ich]-> Draw(); 
      hzdc[1][ich]-> SetLineColor(4);
      hzdc[1][ich]-> Draw("same"); 

      if( ich==0 )
	{
	  TLegend* leg = new TLegend(0.7, 0.8, 0.99, 0.99);
	  for(int i=0; i<2; i++)
	    leg->AddEntry(hzdc[i][ich], runname[i], "l");
	  leg->Draw();
	}
    }
 
  pict2 -> Clear();
  pict2 -> Divide(4,4);

  for(int ich=8;ich<24;ich++)
    {
      pict2 -> cd(ich-7);
      gPad -> SetLogy(1);
    
      hzdc[0][ich]-> SetLineColor(2);
      hzdc[0][ich]-> Draw(); 
      hzdc[1][ich]-> SetLineColor(4);
      hzdc[1][ich]-> Draw("same"); 

      if( ich==8 )
	{
	  TLegend* leg = new TLegend(0.7,0.8,0.99,0.99);
	  for(int i=0; i<2; i++)
	    leg->AddEntry(hzdc[i][ich], runname[i], "l");
	  leg->Draw();
	}
    }

  pict3 -> Clear();
  pict3 -> Divide(4,4);

  for(int ich=24;ich<40;ich++)
    {
      pict3 -> cd(ich-23);
      gPad -> SetLogy(1);
    
      hzdc[0][ich]-> SetLineColor(2);
      hzdc[0][ich]-> Draw(); 
      hzdc[1][ich]-> SetLineColor(4);
      hzdc[1][ich]-> Draw("same"); 

      if( ich==24 )
	{
	  TLegend* leg = new TLegend(0.7,0.8,0.99,0.99);
	  for(int i=0; i<2; i++)
	    leg->AddEntry(hzdc[i][ich], runname[i], "l");
	  leg->Draw();
	}
    }


  pict1->Print("led_adc.pdf(","pdf");
  pict2->Print("led_adc.pdf", "pdf");
  pict3->Print("led_adc.pdf)","pdf");


 
}
