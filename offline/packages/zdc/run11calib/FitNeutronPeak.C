void FitPeak()
{
  TCanvas *canv1=new TCanvas();
  canv1->Print("FitNeutronPeak.pdf[","pdf");
  TChain *T=new TChain("T");
  
  int max_segnum = 35;
  float g[6]={0.35,0.31,0.51,0.48,0.45,0.46}; //ch1=s00,ch5=n00

  char hname[256];
  for(int segnum=0; segnum<max_segnum; segnum++)
    {
      sprintf(hname,"/data2/phnxzdc/run11/reco/run_0000343000_0000344000/DST_EVE/DST_EVE_run11ZdcCalib-0000343544-%04d.root", segnum);
      TFile *file=new TFile(hname);
      if(!file->IsOpen())
	{
	  cout<<"Skiping file "<<hname<<endl;
	  continue;
	}

      T -> Add(hname);

      file->Close();
      delete file;
    }

  gStyle->SetOptFit(1);
  gROOT->cd();
  float mean[2][2];
  float dmean[2][2];
  float ped11[8]={466.7,430.8,475.8,464.9,448.3,438.2,445.2,425.4};
  float tdccut[16]={2000,3000,
		    2600,3300,
		    2900,3500,
		    2800,3400,
		    2100,2700,
		    2600,3400,
		    2600,3400,
		    2600,3400};
  
  TH1F *h1[8];
  TH1F *hsum[2];
  TF1 *f1 = new TF1("f1", "gaus(0)+gaus(3)", 0, 700);
  f1->SetLineColor(2);
  gStyle->SetOptFit(111);
  char hn1[256];
  
  for(int i=0;i<8;i++)
    {
      sprintf(hname,"h1_%d",i);
      h1[i]= new TH1F(hname,hname,4000,0,4000);
      //for ADC calibration -> require no BBC
      sprintf(hname,"(ZdcRawHits.Adc[%d]-%f)>>h1_%d",i,ped11[i],i);
      sprintf(hn1,"ZdcRawHits.Tdc0[%d]>%f && ZdcRawHits.Tdc0[%d]<%f && Bbc_ZVertex<-1000",i,tdccut[i*2],i,tdccut[2*i+1]);
      cout<<hname<<endl<<hn1<<endl;
      T->Draw(hname,hn1);

      f1->SetParameters(4000, 550, 10, 500, 650, 20);
      h1[i]->Fit(f1,"M","",0,400);
      h1[i]->GetXaxis()->SetRangeUser(0,1000);
      h1[i]->Draw();
      canv1->Print("FitNeutronPeak.pdf","pdf");
      /*      mean[i][0]=f1->GetParameter(1)-ped11[i];
      dmean[i][0]=f1->GetParError(1);
      mean[i][1]=f1->GetParameter(4)-ped11[i];
      dmean[i][1]=f1->GetParError(4);*/
      
    }

  hsum[0]= new TH1F("hsumS","hsumS",4000,0,4000);
  hsum[1]= new TH1F("hsumN","hsumN",4000,0,4000);

  for(int i=1;i<4;i++)
    {

      // h1[i]->Scale(g[i-1]);
      //      hsum[0]=hsum[0]+h1[i];
      hsum[0]->Add(h1[i]);

      //h1[i+4]->Scale(g[i+3]);
      //      hsum[1]=hsum[1]+h1[i+4];
      hsum[1]->Add(h1[i+4]);
    }
  
  cout<<"ck:"<<hsum[0]->GetBinContent(1234)<<"="<<h1[1]->GetBinContent(1234)<<"+"
      <<h1[2]->GetBinContent(1234)<<"+"
      <<h1[3]->GetBinContent(1234)<<endl;

  for(int i=0;i<2;i++)
    {
      
      hsum[i]->Fit(f1,"M","",0,400);
      hsum[i]->GetXaxis()->SetRangeUser(0,1000);
      hsum[i]->Draw();
      canv1->Print("FitNeutronPeak.pdf","pdf");

      mean[i][0]=f1->GetParameter(1)-ped11[i+3*i];
      dmean[i][0]=f1->GetParError(1);

      mean[i][1]=f1->GetParameter(4)-ped11[i+3*i];
      dmean[i][1]=f1->GetParError(4);

      cout<<"mean:"<<mean[i][0]<<" \pm "<<dmean[i][0]<<" "
	  <<mean[i][1]<<" \pm "<<dmean[i][1]<<endl;
    }

  canv1->Print("FitNeutronPeak.pdf]","pdf");

}
