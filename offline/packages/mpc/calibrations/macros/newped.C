//
// Extract the pedestals from a dst file
// Compare gaussian, histogram mean, and TLinearFitter with LTS regression
//
#include <iomanip>
#include <iostream>
#include <fstream>
#include <string>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TString.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TStyle.h>
#include <TROOT.h>
#include <TSystem.h>
#include <TGraph.h>
#include <TLinearFitter.h>
#include <TCanvas.h>

#ifndef __CINT__
#include <recoConsts.h>
#include <MpcMap.h>
#include <mpcSampleContainer.h>
#include <mpcSample.h>
#endif

using namespace std;

void newped(const char *fname = "dstf_0000.root")
{
  // Get Run Number from filename
  TString temp_fname = fname;
  Int_t index = temp_fname.Index("-0000");
  temp_fname.Remove(0,index+5);
  temp_fname.ReplaceAll(".root","");
  index = temp_fname.Index("-");
  temp_fname.Remove(index,100);
  //cout << temp_fname << endl;
  int runnumber = temp_fname.Atoi();
  cout << "RUNNUMBER " << runnumber << endl;

  gSystem->Load("libmpc.so");

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",runnumber);

  const int NCH = 576;
  const int NSAMPLES = 12;

  MpcMap *mpcmap = MpcMap::instance();

  TFile *infile = new TFile(fname,"READ");

  //Float_t e1sum = 0;
  mpcSampleContainer *adcout = 0;

  TTree *T = (TTree*)infile->Get("T");
  int nbranch = 0;
  TBranch *branch[100] = {0};
  //branch[nbranch] = T->GetBranch("e1sum");
  //branch[nbranch++]->SetAddress(&e1sum);
  branch[nbranch] = T->GetBranch("DST/mpcSampleContainer");
  branch[nbranch++]->SetAddress(&adcout);

  TString name;
  name = "ped_"; name += runnumber; name += ".root";
  TFile *savefile = new TFile(name,"RECREATE");
  TH1F *h_adc[NCH]={0};
  TGraph *g_adc[NCH]={0};
  //Float_t adc[][];
  for (int ich = 0; ich < NCH; ich++)
    {
      name = "h_adc"; name += ich;
      h_adc[ich]=new TH1F(name, "adc distribution", 100, 2000.5, 2100.5);

      name = "g_adc"; name += ich;
      g_adc[ich]=new TGraph();
      g_adc[ich]->SetName(name);
      g_adc[ich]->SetTitle(name);
      g_adc[ich]->SetMarkerColor(2);
      g_adc[ich]->SetMarkerStyle(20);
      g_adc[ich]->SetMarkerSize(0.8);
    }
  TH2 *h2_adc = new TH2F("h2_adc","pedestal vs channel",NCH,-0.5,NCH-0.5,100,2000.5,2100.5);

  string junk;

  Stat_t nentries = T->GetEntries();
  cout << "num entries " << nentries << endl;
  //if ( nentries>10000 ) nentries = 10000;

  // Loop over events
  for (int ievt = 0; ievt<nentries; ievt++)
    {
      T->GetEvent(ievt);

      //cout << adcout->get_nsamples() << endl;
      //cout << "size\t" << adcout->size() << endl;
      int nsamples = adcout->size();
      if ( nsamples==0 ) continue;

      for (int ich=0; ich<NCH; ich++)
        {

          for (int isamp=0; isamp<NSAMPLES; isamp++)
            {
              mpcSample *adcsamp = adcout->GetSample(ich*NSAMPLES + isamp);
              //cout << "yyy\t" << isamp << "\t" << (unsigned int)adcsamp << endl;
              if ( adcsamp==0 ) continue;
 
              int adc = adcsamp->get_adc();	// adc
              int chn = adcsamp->get_ch();     // fee ch, should be same as ich
              int samp = adcsamp->get_sample();  // sample number
 	      
              //cout << chn << "\t" << samp << "\t" << adc << endl;
              // Skipping timing pulse channels
              if ( samp!=0 ) continue;

              h_adc[chn]->Fill( adc );
              h2_adc->Fill( chn, adc );

              // Fill TGraph
              int n = g_adc[chn]->GetN();
              g_adc[chn]->SetPoint(n,n,adc);
            }
	}
    }

  TCanvas *ac = new TCanvas("ac","pedestals",1000,400);
  gStyle->SetPalette(1);
  gROOT->SetStyle("Plain");
  gROOT->ForceStyle();
  name = "pedestals, run "; name += runnumber;
  h2_adc->SetTitle(name);
  h2_adc->SetStats(kFALSE);
  h2_adc->Draw("colz");

  char gifdir[1024];
  //sprintf(gifdir,"/var/www/html/%05d",runnumber);
  sprintf(gifdir,"plots/%05d",runnumber);
  gSystem->MakeDirectory(gifdir);
  name = gifdir; name += "/ped_"; name += runnumber; name += ".png";
  ac->SaveAs(name);
  name.ReplaceAll("png","eps");
  ac->SaveAs(name);

  // Evaluation Histograms
  gStyle->SetOptStat(111111);
  name = "ped mean, run "; name += runnumber;
  TH1F *h_mean = new TH1F("h_mean",name,100,2020,2070);
  name = "ped rms, run "; name += runnumber;
  TH1F *h_rms = new TH1F("h_rms",name,100,0,5);

  name = "diff between mean and gaus fit, run "; name += runnumber;
  TH1F *h_gaushistmean_diff = new TH1F("h_gaushistmean_diff",name,2000,-10,10);
  h_gaushistmean_diff->SetXTitle("Diff (ADC counts)");
  name = "diff between tlinear fit and gaus fit, run "; name += runnumber;
  TH1F *h_linearhistmean_diff = new TH1F("h_linearhistmean_diff",name,2000,-10,10);
  h_linearhistmean_diff->SetXTitle("Diff (ADC counts)");
  h_linearhistmean_diff->SetLineColor(2);

  TH2F *h2_noise[2];
  for (int iarm=0; iarm<2; iarm++)
    {
      name = "h2_noise"; name += iarm;
      h2_noise[iarm] = new TH2F(name,name,18,-0.5,17.5,18,-0.5,17.5);
    }

  // Now write out pedestals
  TCanvas *cfit = new TCanvas("cfit","fit",550,425);
  TCanvas *cfit2 = new TCanvas("cfit2","fit tgraph",550,425);
  TF1 *gaussian = new TF1("gaussian","gaus",2000,2100);
  TF1 *poly0 = new TF1("poly0","pol0",0,100000);
  name = ""; name += runnumber; name += ".gped";
  ofstream pedfile(name.Data());
  name = ""; name += runnumber; name += ".hped";
  ofstream ped2file(name.Data());
  name = ""; name += runnumber; name += ".grped";
  ofstream ped3file(name.Data());
  for (int feech=0; feech<NCH; feech++)
    {
      Double_t mean = h_adc[feech]->GetMean();
      Double_t meanerr = h_adc[feech]->GetMeanError();
      Double_t rms = h_adc[feech]->GetRMS();
      Double_t rmserr = h_adc[feech]->GetRMSError();

      Int_t ncounts = h_adc[feech]->GetEntries();
      //cout << "xxx " << feech << "\t" << meanerr << "\t" << rms/sqrt(ncounts) << endl;

      cfit->cd();
      Double_t height = h_adc[feech]->GetMaximum();
      gaussian->SetParameters(height,mean,rms);

// NEED TO SET RANGE HERE, test on run 367550, 367927, 365902
// ALSO, mean should be replaced by the maximum in the fit...
// Should dump an error if chi2 is very bad
      //gaussian->SetRange(height,mean,rms);

      h_adc[feech]->Fit(gaussian);
      Double_t gaus_mean = gaussian->GetParameter(1);
      Double_t gaus_meanerr = gaussian->GetParError(1);
      Double_t gaus_rms = gaussian->GetParameter(2);
      Double_t gaus_rmserr = gaussian->GetParError(2);
      Double_t gaus_ndf = gaussian->GetNDF();

      h_mean->Fill( mean );
      h_rms->Fill( rms );

      cfit2->cd();
      poly0->SetParameter(0,mean);

/*
      if ( g_adc[feech]->GetN()!=0 )
        {
          g_adc[feech]->Draw("ap");
          g_adc[feech]->Fit(poly0,"rob=0.8");

string junk;
cout << "xxx " << feech << "\t" << mean << "\t" << rms << "\t"
     << gaus_mean << "\t" << gaus_rms << "\t"
     << poly0->GetParameter(0) << "\t"
     << ncounts << "\t" << g_adc[feech]->GetN() << endl;
gPad->Modified();
gPad->Update();
cfit->Modified();
cfit->Update();
cin >> junk;
        }
      else
        {
          cout << "SKIPPING " << feech << " ////////////////////////" << endl;
        }
*/

      if ( h_adc[feech]->GetEntries()>20 )
        {
          // gaus fit
          pedfile << setprecision(6) << feech << "\t"
                  << std::setw(15) << gaus_mean
                  << std::setw(15) << gaus_rms
                  << std::setw(15) << ncounts
                  << endl;

          // histogram
          ped2file << setprecision(6) << feech << "\t"
                  << std::setw(15) << mean
                  << std::setw(15) << rms
                  << std::setw(15) << ncounts
                  << endl;

          // tlinearfitter fit
          ped3file << setprecision(6) << feech << "\t"
                  << std::setw(15) << poly0->GetParameter(0)
                  << std::setw(15) << rms
                  << std::setw(15) << ncounts
                  << endl;
        }

     int arm = mpcmap->getArm(feech);
     int gridx = mpcmap->getGridX(feech);
     int gridy = mpcmap->getGridY(feech);

     cout << feech << "\t" << arm << "\t" << gridx << "\t" <<  gridy << "\t" << rms << endl;

     if ( gridx<-17 )	// empty channel
       {
         if ( rms>1.0 )
           {
             cout << "EMPTY BUT NOISY " << feech << "\t" << mean << "\t" << rms << endl;
           }
         continue;
       }
     else if ( gridx<-1 )	// pin diodes
       {
         gridx = -gridx;
         gridy = -gridy;
       }
       
      // check for noisy channels
      if (rms>3.0) cout << "VERY NOISY " << feech << "\t" << mean << "\t" << rms << endl;

      // check hist mean vs gaus fit 
      if ( gridx<-17 )	// empty channel
        {
          cout << "empty ";
        }
      else
        {
          cout << "eval  ";
        }
/*
      cout << feech
           << " " << (gaus_mean-mean)/gaus_meanerr
           << " " << (gaus_rms-rms)/gaus_rmserr
           << " " << rms
           << endl;
*/

      
      h2_noise[arm]->Fill(gridx,gridy,rms);
    }
  pedfile.close();
  ped2file.close();
  ped3file.close();

  TCanvas *bc = new TCanvas("bc","pedestal info",425,550);
  bc->Divide(1,2);
  bc->cd(1);
  h_mean->Draw();
  bc->cd(2);
  h_rms->Draw();
  name = gifdir; name += "/pedqa_"; name += runnumber; name += ".png";
  bc->SaveAs(name);
  name.ReplaceAll("png","eps");
  bc->SaveAs(name);


  TCanvas *cc = new TCanvas("cc","pedestal",800,400);
  cc->Divide(2,1);
  cc->cd(1);
  h2_noise[0]->Draw("colz");
  cc->cd(2);
  h2_noise[1]->Draw("colz");
  name = gifdir; name += "/pedtowers_"; name += runnumber; name += ".png";
  cc->SaveAs(name);

  TCanvas *dc = new TCanvas("dc","comparisons",550,425);
  

  savefile->Write();
}


