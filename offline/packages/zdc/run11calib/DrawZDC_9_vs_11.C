#include <iomanip>
#include <iostream>

void DrawZDC_9vs11(char *Root9, char *Root11){

  TCanvas *pict1 = new TCanvas("pict1","pict1",1600,800);  
  TCanvas *pict2 = new TCanvas("pict2","pict2",1600,800); 
  // TCanvas *pict3 = new TCanvas("pict3","pict3",1600,800);  

  TCanvas *pic[8];
  TH1F *hzdc9[8];
  TH1F *hzdc11[8];
  double ped9[8]={0,0,0,0,0,0,0,0};
  double ped11[8]={0,0,0,0,0,0,0,0};
  //  double ped9[8]={441.2,396.8,472.3,448.5,416.4,387.2,430.8,425.3};
  //double ped11[8]={466.7,430.8,475.8,464.9,448.3,438.2,445.2,425.4};

  int nrun9,nrun11;

  char hname[256], leafname[256];

  pt = new TPaveText(0.20,0.7,0.5,0.87, "NDC");
  pt->SetFillColor(0); // text is black on white
  pt->SetTextSize(0.04); 
  pt->SetTextAlign(12);
  text = pt->AddText("Run9=Black && Run11=Red");

  TFile *hfile[2];
  hfile[0]= new TFile(Root9);
  hfile[1]= new TFile(Root11);

  for(int j=0; j<2; j++){
    
    hfile[j]->cd();

    for(int i=0;i<8;i++){
      sprintf(hname,"hzdc_run%d_%d",9+2*j,i); 
      
      if(j==0)
	{
	  hzdc9[i] = new TH1F(hname,hname,1000,0,4000);
	  sprintf(leafname,"ZdcRawHits.Adc[%d]-%f",i,ped9[i]); 
	  nrun9=T -> Project(hname,leafname,"((lvl1_trigscaled&0x00800000) || (lvl1_trigscaled&0x00040000))>0","");
	}
      else
	{
	  hzdc11[i]= new TH1F(hname,hname,1000,0,4000);
      	  sprintf(leafname,"(ZdcRawHits.Adc[%d]-%f)",i,ped11[i]);
	  nrun11=T -> Project(hname,leafname,"(lvl1_trigscaled&0x00002000)>0","",114273,0);
	}
    }
  }
  cout<<"Run 9 entries:"<<nrun9<<" Run11 entries:"<<nrun11<<endl;

  hzdc9[0] -> SetTitle(Root9); 
  hzdc11[0] -> SetTitle(Root11); 
  
  pict1 -> Clear();
  pict1 -> Divide(4,2);
  /* pict3 -> Clear();
     pict3 -> Divide(4,2);*/
  for(int i=0;i<8;i++){
    pict1 -> cd(i+1);
    gPad -> SetLogy(1);
    hzdc9[i] -> Draw();
    hzdc11[i]->SetLineColor(2);
    hzdc11[i]->Draw("same");
    /*  pict3->cd(i+1);
    gPad -> SetLogy(1);
    hzdc11[i]->Draw();*/
    //  cout<<hzdc11[i]->Integral()<<" "<<hzdc11[i]->GetEntries()<<endl;
    pt->Draw("same");

  }
    
  TH1F *div[8];
  pict2 -> Clear();
  pict2 -> Divide(4,2);
  for(int i=0;i<8;i++)
    {
      pict2->cd(i+1);
      sprintf(hname,"11vs9_%d",i); 
      div[i] = new TH1F(hname,hname,1000,0,4000);
      div[i]->Divide(hzdc11[i],hzdc9[i]);
      div[i]->Draw();
    }
  
}

