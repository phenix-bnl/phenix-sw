#include <iomanip>
#include <iostream>

void DrawZDCSMD1(char *RootName="defalt.root",int ch){



  TFile *hfile = new TFile(RootName);
  char hname[256],leafname[256];  
  TH1F *hzdc[8];
  TH1F *href[8];
  TH1F *hsmd[30];
  TH1F *hzdcped[8];
  //  double pedestal[8]={466,430.8,475.8,464.9,441.7,438.2,445.2,425.4};
  double ped[40]={466.7,430.8,473.6,464.5,473.6,435.7,443.8,425.4,449.3,664.9,
		  519.9,712.1,691  ,432.8,658  ,682.9,429.4,567.3,500  ,697.8,
		  564.3,576.9,721.1,608.4,761  ,517.4,856.3,769  ,728  ,794  ,
		  793.3,848.6,523.5,569.9,674.1,637.6,430.5,744  ,770.3,705.2 };
  
  //  double ped[40]={0};

  //  double pedestal[8]={0,0,0,0,0,0,0,0};

  /*create a file that has the mean values and the rms of each channel */
  //  ofstream ofile;
  //ofile.open("ZdcCalib.pedestal");


  if(ch==0 || ch==2)
    {
      TCanvas *pict1 = new TCanvas("pict1","pict1",800,400);  
      for(int i=0;i<8;i++){
	sprintf(hname,"hzdc%d",i); 
	//hzdc[i] = new TH1F(hname,hname,1000,0,4000);
	//hzdc[i] = new TH1F(hname,hname,1000,0,1500);
	//sprintf(leafname,"ZdcRawHits.Adc[%d]-%f",i,ped[i]); 
	//T -> Project(hname,leafname);
	//T -> Project(hname,leafname,"","");
	//    ofile<<setw(6)<<hzdc[i]->GetMean()<<" "<<setw(5)<<hzdc[i]->GetRMS()<<" 0"<<endl;
	sprintf(hname,"hzdcped%d",i); 
	hzdcped[i] = new TH1F(hname,hname,1000,0,1500);
	//hzdcped[i] = new TH1F(hname,hname,1000,0,4000);
	sprintf(leafname,"ZdcRawHits.Adc[%d]",i); 
	T -> Project(hname,leafname,"(lvl1_trigscaled&0x40000000)>0");	//these are the LED triggers
	//T -> Project(hname,leafname,"(lvl1_trigscaled&0x10000000)>0");	//these are the PED triggers

      }
      //hzdc[0] -> SetTitle(RootName); 
      hzdcped[0]-> SetTitle(RootName); 
    }
  if(ch==1 || ch==2)
    {
      TCanvas *pict2 = new TCanvas("pict2","pict2",800,1000);  
      TCanvas *pict3 = new TCanvas("pict3","pict3",800,1000);  
      for(int i=0;i<15;i++){
	sprintf(hname,"hsmd%d",i); 
	//hsmd[i] = new TH1F(hname,hname,1000,0,4000);
	hsmd[i] = new TH1F(hname,hname,1000,0,1500);
	sprintf(leafname,"ZdcRawHits.Adc[%d]-%f",i+8,ped[i+8]); 
	T -> Project(hname,leafname);
	//ofile<<setw(6)<<hsmd[i]->GetMean()<<" "<<setw(5)<<hsmd[i]->GetRMS()<<" 0"<<endl;
      }
      for(int i=0;i<15;i++){
	sprintf(hname,"hsmd%d",i+15); 
	//hsmd[i+15] = new TH1F(hname,hname,1000,0,4000);
	hsmd[i+15] = new TH1F(hname,hname,1000,0,1500);
	sprintf(leafname,"ZdcRawHits.Adc[%d]-%f",i+24,ped[i+24]); 
	T -> Project(hname,leafname);
	//ofile<<setw(6)<<hsmd[i+15]->GetMean()<<" "<<setw(5)<<hsmd[i+15]->GetRMS()<<" 0"<<endl;
      }
      hsmd[0] -> SetTitle(RootName);    
    }

  //  ofile.close();

  //   TFile *f3=new TFile("dst/274778_LPol_Calib.root");

  if(ch==0 || ch==2)
    { 

      /*  for(int i=0;i<8;i++)
	{
	  sprintf(hname,"href%d",i);
	  href[i] = new TH1F(hname,hname,1000,0,4000);
	  sprintf(leafname,"ZdcRawHits.Adc[%d]",i);
	  T -> Project(hname,leafname);
	}
      */

      pict1 -> Clear();
      pict1 -> Divide(4,2);
      for(int i=0;i<8;i++){
	pict1 -> cd(i+1);
	gPad -> SetLogy(1);
	//hzdc[i] -> Draw(); 
	//	href[i]->SetLineColor(4);
	//href[i]->Draw("same");
	hzdcped[i]->SetLineColor(2);
	hzdcped[i]->Draw();
	//hzdcped[i]->Draw("same");
      }
      //  pict1 -> Print("pict1.ps");
    }

  if(ch==1 || ch==2)
    {
      pict2 -> Clear();
      pict2 -> Divide(3,5);
      for(int i=0;i<15;i++){
	pict2 -> cd(i+1);
	gPad -> SetLogy(1);
	hsmd[i] -> Draw();
      }

      pict3 -> Clear();
      pict3 -> Divide(3,5);
      for(int i=15;i<30;i++){
	pict3 -> cd(i-14);
	gPad -> SetLogy(1);
	hsmd[i] -> Draw();
      }
      //pict2 -> Print("pict2.ps");
    }

  //  f3->Close();
}
