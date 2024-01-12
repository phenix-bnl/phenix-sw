#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TF1.h"
#include "summaryQA.h"
#include <MpcMap.h>
#include <fstream>
#include <iostream>
#include <cmath>
#include "TCanvas.h"
#include <MpcNoise.h>

const int NTHRESH=4;

using namespace std;
int QASummary::processMpc(int plot_type, int write_flag)
{
  cout << "Mpc..." << endl;
  fstream textFile(outputName1, ios::in);
  if (textFile)
    {
      textFile.close();
      textFile.open(outputName1, ios::app | ios::out);
    }
  fstream statusFile(outputName2, ios::in);
  if (statusFile)
    {
      statusFile.close();
      statusFile.open(outputName2, ios::app | ios::out);
    }

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- MPC QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;

  //this actually doesn't do anything, b/c the problem is in the Init() function in QASummary.C
  if(qafile == NULL) 
       {
      statusFile << "Error...QA file " << inputName << " does not exists...Exiting...\n";
      cout << "Error...QA file " << inputName << " does not exists...Exiting...\n";
      return 0;
    }

  // Int_t mpcstatus = 0;
  float adc_thresh_low[NTHRESH] = {0.375,0.525,0.9, 1.5};
  float adc_thresh_high[NTHRESH] = {50.,50.,50.,50.};
 // int adc_thresh_low[NTHRESH] = {25,35,60,100};
  //int adc_thresh[NTHRESH] = {0,10,20,30,40,50,60,70,80,90};
  //int adc_thresh_high[NTHRESH] = {4094,4094,4094,4094};
  //int adc_thresh_low[NTHRESH]; //= {0,10,20,30,40,50,60,70,80,100,150,200,250,300,400,500};
  //int adc_thresh_high[NTHRESH]; //= {10,20,30,40,50,60,70,80,100,150,200,250,300,400,500,600};
  // ifstream din;
  //din.open("adc_thresh.dat");
  //int iadc=0;
  /*while(din && iadc<NTHRESH )
    {
      din >> adc_thresh_low[iadc] >> adc_thresh_high[iadc];
      iadc++;
    }
  */    
  
  int s=0; int n=1;
  TString name, title[2];
  TH2F *h2_nhits[2][NTHRESH];
  MpcMap *mpcmap = MpcMap::instance();
  MpcNoise *mpcnoise = new MpcNoise();
  mpcnoise->setRun(runNumber);  //runNumber is a QASummary protected variable
  for(int ithr=0;ithr<4;ithr++) mpcnoise->setThresh(ithr,(int)(adc_thresh_low[ithr]*1000.0));
  
  TH1F *h_nhits[576];

  for(int irange=0;irange<NTHRESH;irange++)
    {
      name="N_hits_S_";  name+=irange;
      title[0]="South MPC N/N_{ev} thr < "; title[0]+=adc_thresh_low[irange];
      title[1]="South MPC N/N_{ev} "; title[1]+=adc_thresh_low[irange];
      title[1]+="<thr< "; title[1]+=adc_thresh_high[irange];
      h2_nhits[s][irange] = new TH2F(name,title[plot_type],18,-0.5,17.5,18,-0.5,17.5);
      
      name="N_hits_N_";  name+=(irange);
      title[0]="North MPC N/N_{ev} thr > "; title[0]+=(adc_thresh_low[irange]);
      title[1]="North MPC N/N_{ev} "; title[1]+=adc_thresh_low[irange];
      title[1]+="<thr< "; title[1]+=adc_thresh_high[irange];
      h2_nhits[n][irange] = new TH2F(name,title[plot_type],18,-0.5,17.5,18,-0.5,17.5);
 
      for(int impc=0;impc<2;impc++)
	{
	  h2_nhits[impc][irange]->SetXTitle("X");
	  h2_nhits[impc][irange]->SetYTitle("Y");
	}
    }

 
  for(int ich=0;ich<576;ich++)
    {
      name="N_hits_";  name+=ich;
      title[0] = "N hits, T-holds = "; title[0]+= adc_thresh_low[0]; title[0]+=", ";
      title[0]+= adc_thresh_low[1]; title[0]+=", "; title[0]+= adc_thresh_low[2]; title[0]+=", ";
      title[0]+= adc_thresh_low[3];
      h_nhits[ich] = new TH1F(name,title[0],5,-0.5,4.5);
    }
      
  
 
  TH1F *hloadc_driver_run[576];
  TH1F *htdc_run[576];
  TH1F *hloadc_run[576];
  TH1F *h_ntrig_run = (TH1F*) qafile->Get("h_ntrig"); 
  int N_evts = (int) h_ntrig_run->GetBinContent(1);  //this is # of minbias trigs
  mpcnoise->setNtot(N_evts);
 
  for(int ich=0;ich<576;ich++)
    {
      name = "htdc_"; name += ich;
      htdc_run[ich] = (TH1F*) qafile->Get(name);
     
      name = "hloadc_"; name += ich;
      hloadc_run[ich] = (TH1F*) qafile->Get(name);
    }
  
  for(int ich=0;ich<20;ich++)
    {
      name = "hloadc_driver_"; name += ich;
      hloadc_driver_run[ich] = (TH1F*) qafile->Get(name);
    }


 

  int Nhits_tot[2] = {0,0};
  int Nhits_driver[20];
  for(int idriver=0;idriver<20;idriver++)
    {
      Nhits_driver[idriver] = 0;

    }
  for(int ich=0;ich<576;ich++)
    {
      if ( mpcmap->getGridX(ich) < 0 ) continue;
      for(int irange=0;irange<NTHRESH;irange++)
	{
	  int bin_low = hloadc_run[ich]->FindBin((float)adc_thresh_low[irange]);
	  int high_bin = hloadc_run[ich]->FindBin((float)adc_thresh_high[irange]);
	  int Nbins = hloadc_run[ich]->GetNbinsX();
	  int bin_high = plot_type ? high_bin: Nbins;
	  float content = hloadc_run[ich]->Integral(bin_low,bin_high);
	  mpcnoise->setCh(ich,ich);
	  mpcnoise->setN(ich,irange,(int)content);
	  h_nhits[ich]->SetBinContent(irange,content);

	  int gridx = mpcmap->getGridX(ich); int gridy = mpcmap->getGridY(ich);
	  int g2_bin = h2_nhits[s][irange]->FindBin(gridx,gridy);
	  int driver = mpcmap->getDriver(ich);
	  Nhits_driver[driver-1] += (int)content;	  
	  if(ich < 288)
	    {
	      h2_nhits[s][irange]->SetBinContent(g2_bin,content);
              if(irange == 0) Nhits_tot[s]+=(int)content;
            }
	  else
	    { 
	      h2_nhits[n][irange]->SetBinContent(g2_bin,content);
	      if(irange == 0) Nhits_tot[n]+=(int)content;
 	    }
        }
    }

  
  if(write_flag)
    {
      
      TCanvas* c1[2];
      c1[s] = new TCanvas("c_s","c_s",1200,1200);
      c1[n] = new TCanvas("c_n","c_n",1200,1200);
      
      c1[s]->Divide(4,4);
      c1[n]->Divide(4,4);
      name="mpc_hits"; name+=runNumber; name+=".root";
      TFile *f = new TFile(name,"recreate");
      f->cd(); 
      
      for(int irange=0;irange<NTHRESH;irange++)
	{
      	  float Nevts = h_ntrig_run->GetBinContent(1); // # of mbias trigs is in 1st bin
	  c1[s]->cd(irange+1);
	  h2_nhits[s][irange]->Scale(1.0/Nevts);
	  h2_nhits[s][irange]->Draw("colz");
	  c1[n]->cd(irange+1);
	  h2_nhits[n][irange]->Scale(1.0/Nevts);
	  h2_nhits[n][irange]->Draw("colz");
	  h2_nhits[s][irange]->Write();
	  h2_nhits[n][irange]->Write();
	}
      c1[s]->SaveAs("nhits_S.png");
      c1[n]->SaveAs("nhits_N.png");
         
      h_ntrig_run->Write();
      f->Close();
    }
  textFile.close();
  statusFile.close();
  
  
  mpcnoise->StoreInDatabase(runNumber, "NOISE", "Beau Meredith", "Test");
  const char* base_mpc_message = "DEAD TEST MPC ";
  const char* base_driver_message = "DEAD TEST DRIVER ";
  char message[256];
  if(Nhits_tot[n] == 0)
    {
      sprintf(message,"%s N",base_mpc_message);
      CommitToQADatabase("Mpc",message,0,0);
    }
  if(Nhits_tot[s] == 0)
    {
      sprintf(message,"%s S",base_mpc_message);
      CommitToQADatabase("Mpc",message,0,0);
    } 
  
  for(int idriver=0;idriver<20;idriver++)
    {
      if(Nhits_driver[idriver] == 0) 
        {
          sprintf(message,"%s %d",base_driver_message,(idriver+1));
          CommitToQADatabase("Mpc",message,0,0);
        }
    }
        
  cout << "    ...done." << endl;
  return 0;
}
