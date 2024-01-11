//
// Process the mpcped_*.root files to extract the pedestals
//
#include <TString.h>
#include <TH1.h>
#include <TFile.h>
#include <TGraphErrors.h>
#include <TCanvas.h>
#include <iostream>
#include <fstream>

using namespace std;

void get_pedestals(const char *fname = "mpcped.root", int run_number = 0)
{
  gROOT->Reset();
/*
  gSystem->Load("libnanoDST.so");
  gSystem->Load("libtrigger.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libmpc.so");
*/

//  MpcMap *mpcmap = MpcMap::instance();

  TString run = "0";

  if ( run_number == 0 )
    {
      run = fname;
      run.ReplaceAll("mpcped_","");
      run.ReplaceAll(".root","");
    }

  //bool saveflag = true;
  bool saveflag = false;
  //bool printflag = true;
  bool printflag = false;
  const int MAXFEM = 4;
  //const int MAXFEM = 1;
  const int MAXCH = 144;	// max channels per fem
  const int MAXAMU = 64;	// max amu's per channel


  // now get the necessary ntuples
/*
  TFile *infile = new TFile(fname,"READ");

  TNtuple *tdc = (TNtuple*)infile->Get("tdc");
  TNtuple *lopre = (TNtuple*)infile->Get("lopre");
  TNtuple *lopost = (TNtuple*)infile->Get("lopost");
  TNtuple *hipre = (TNtuple*)infile->Get("hipre");
  TNtuple *hipost = (TNtuple*)infile->Get("hipost");
  TNtuple *lo = (TNtuple*)infile->Get("lo");
  TNtuple *hi = (TNtuple*)infile->Get("hi");
*/

/*
  TH1F *htdc_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hlopost_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hlopre_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hlo_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hhipost_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hhipre_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hhi_all[MAXFEM][MAXCH] = {{0}};
*/
//  TH1F *htdc_all[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  TH1F *htdc_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hlopost_all[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  TH1F *hlopre_all[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  TH1F *hlo_all[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  TH1F *hhipost_all[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  TH1F *hhipre_all[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  TH1F *hhi_all[MAXFEM][MAXCH][MAXAMU] = {{{0}}};

  // Generate 
  TH1F *hemptyfem[4] = {0};
  hemptyfem[0] = new TH1F("nfem0", "nfem0", 144, -0.5, 143.5);
  hemptyfem[1] = new TH1F("nfem1", "nfem1", 144, 143.5, 287.5);
  hemptyfem[2] = new TH1F("nfem2", "nfem2", 144, 287.5, 431.5);
  hemptyfem[3] = new TH1F("nfem3", "nfem3", 144, 431.5, 575.5);

/*
  for (int ifem = 0; ifem < MAXFEM; ifem++)
    {
      for (int ich = 0; ich < 144; ich++)
	{
          int ifee576ch = 144*ifem + ich;
	  if ( mpcmap->getGridX(ifee576ch)<0 )
            {
              hemptyfem[ifem]->Fill(ifee576ch,4000.);
            }
	}
      hemptyfem[ifem]->SetFillColor(5);
      //hemptyfem[ifem]->SetFillStyle(4100);
    }
*/

  Double_t fee567ch[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t tdcmean[MAXFEM][MAXCH] = {{0}};
  Double_t tdcmeanerr[MAXFEM][MAXCH] = {{0}};
  Double_t tdcrms[MAXFEM][MAXCH] = {{0}};
  Double_t tdcrmserr[MAXFEM][MAXCH] = {{0}};
/*
  Double_t tdcmean[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t tdcmeanerr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t tdcrms[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t tdcrmserr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
*/
  Double_t lopostmean[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lopostmeanerr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lopostrms[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lopostrmserr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lopremean[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lopremeanerr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t loprerms[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t loprermserr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lomean[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lomeanerr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lorms[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t lormserr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hipostmean[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hipostmeanerr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hipostrms[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hipostrmserr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hipremean[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hipremeanerr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hiprerms[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hiprermserr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t himean[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t himeanerr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hirms[MAXFEM][MAXCH][MAXAMU] = {{{0}}};
  Double_t hirmserr[MAXFEM][MAXCH][MAXAMU] = {{{0}}};

  TString histfname;
  TFile *histfile[4] = {0};
  TString name;
  int nch = 0;
  for (int ifem=0; ifem<MAXFEM; ifem++)
    {
      histfname = fname;
      histfname.ReplaceAll(".root","_");
      histfname += ifem; histfname += ".root";
      histfile[ifem] = new TFile(histfname,"READ");
      if ( histfile[ifem]->IsZombie() )
        {
          cout << "Couldn't open file " << histfname << endl;
          continue;
        }

      for (int ich=0; ich<MAXCH; ich++)
        {
          int ifee576ch = ifem*MAXCH + ich;

          if ( ifee576ch%10==0 ) cout << ifee576ch << endl;

          name = "htdc_"; name += ifee576ch;
          //name += "_"; name += iamu;
          htdc_all[ifem][ich] = (TH1F*)histfile[ifem]->Get(name);
    
          if ( htdc_all[ifem][ich]==0 ) break;

          tdcmean[ifem][ich] = htdc_all[ifem][ich]->GetMean();
          //tdcmeanerr[ifem][ich] = htdc_all[ifem][ich]->GetMeanError();
          tdcrms[ifem][ich] = htdc_all[ifem][ich]->GetRMS();
          //tdcrmserr[ifem][ich] = htdc_all[ifem][ich]->GetRMSError();

          int nbinsx = htdc_all[ifem][ich]->GetNbinsX();
          int range = htdc_all[ifem][ich]->GetBinCenter(nbinsx)
                      - htdc_all[ifem][ich]->GetBinCenter(1);
          if ( range>200 )
            {
              cout << "bad tdc " << ifee576ch << "\trange\t" << range
                   << "\t" << tdcmean[ifem][ich] << "\t" << tdcrms[ifem][ich] << endl;
              // Get Truncated Mean (could also get gaussian fit)
              int peakbin = htdc_all[ifem][ich]->GetMaximumBin();
              float peakpos = htdc_all[ifem][ich]->GetBinCenter(peakbin);
              htdc_all[ifem][ich]->SetAxisRange(peakpos-40,peakpos+40);
              tdcmean[ifem][ich] = htdc_all[ifem][ich]->GetMean();
              tdcrms[ifem][ich] = htdc_all[ifem][ich]->GetRMS();
            }
    
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
              name = "hlopost_"; name += ifee576ch; name += "_"; name += iamu;
              hlopost_all[ifem][ich][iamu] = (TH1F*)histfile[ifem]->Get(name);
              name = "hlopre_"; name += ifee576ch; name += "_"; name += iamu;
              hlopre_all[ifem][ich][iamu] = (TH1F*)histfile[ifem]->Get(name);
              name = "hlo_"; name += ifee576ch; name += "_"; name += iamu;
              hlo_all[ifem][ich][iamu] = (TH1F*)histfile[ifem]->Get(name);
              name = "hhipost_"; name += ifee576ch; name += "_"; name += iamu;
              hhipost_all[ifem][ich][iamu] = (TH1F*)histfile[ifem]->Get(name);
              name = "hhipre_"; name += ifee576ch; name += "_"; name += iamu;
              hhipre_all[ifem][ich][iamu] = (TH1F*)histfile[ifem]->Get(name);
              name = "hhi_"; name += ifee576ch; name += "_"; name += iamu;
              hhi_all[ifem][ich][iamu] = (TH1F*)histfile[ifem]->Get(name);
    
              // Process the histograms
              fee567ch[ifem][ich][iamu] = ifee576ch;
    
              lopostmean[ifem][ich][iamu] = hlopost_all[ifem][ich][iamu]->GetMean();
              lopostrms[ifem][ich][iamu] = hlopost_all[ifem][ich][iamu]->GetRMS();
    //cout << ich << "\t" << lopostrms[ifem][ich][iamu] << "\t" << hlopost_all[ifem][ich][iamu]->GetName() << "\t" << hlopost_all[ifem][ich][iamu]->GetRMS() << endl;
              lopremean[ifem][ich][iamu] = hlopre_all[ifem][ich][iamu]->GetMean();
              loprerms[ifem][ich][iamu] = hlopre_all[ifem][ich][iamu]->GetRMS();
              lomean[ifem][ich][iamu] = hlo_all[ifem][ich][iamu]->GetMean();
              lorms[ifem][ich][iamu] = hlo_all[ifem][ich][iamu]->GetRMS();
    
              hipostmean[ifem][ich][iamu] = hhipost_all[ifem][ich][iamu]->GetMean();
              hipostrms[ifem][ich][iamu] = hhipost_all[ifem][ich][iamu]->GetRMS();
              hipremean[ifem][ich][iamu] = hhipre_all[ifem][ich][iamu]->GetMean();
              hiprerms[ifem][ich][iamu] = hhipre_all[ifem][ich][iamu]->GetRMS();
              himean[ifem][ich][iamu] = hhi_all[ifem][ich][iamu]->GetMean();
              hirms[ifem][ich][iamu] = hhi_all[ifem][ich][iamu]->GetRMS();
            }

          ++nch;
        }
    }

  int nfem = nch/MAXCH;
//cout << nfem << "\t" << nch << "\t" << MAXCH << endl;

  // get the mean and rms with amu dependence

  TFile *savefile = 0;
  if ( saveflag )
    {
      savefile = new TFile("getped.root","RECREATE");
    }

  // dump pedestal files
  name = "MpcCal.overflow."; name += run;
  ofstream tacfile(name.Data());
  for (int ifem=0; ifem<nfem; ifem++)
    {
      for (int ich=0; ich<144; ich++)
        {
          int ifee576ch = 144*ifem + ich;
          tacfile << ifee576ch << "\t" << tdcmean[ifem][ich] << "\t" << tdcrms[ifem][ich] << "\t" << 0 << endl;
        }
    }
  tacfile.close();

  name = "MpcCal.lopostped."; name += run;
  ofstream lopostfile(name.Data());
  for (int ifem=0; ifem<nfem; ifem++)
    {
      for (int ich=0; ich<144; ich++)
        {
          int ifee576ch = 144*ifem + ich;
/*
          if ( lopostrms[ifem][ich] > 12 )
            {
              // Get Truncated Mean (could also get gaussian fit)
              int peakbin = htdc_all[ifem][ich]->GetMaximumBin();
              float peakpos = htdc_all[ifem][ich]->GetBinCenter(peakbin);
              htdc_all[ifem][ich]->SetAxisRange(peakpos-40,peakpos+40);
              tdcmean[ifem][ich] = htdc_all[ifem][ich]->GetMean();
              tdcrms[ifem][ich] = htdc_all[ifem][ich]->GetRMS();
            }
*/
          for (int iamu=0; iamu<64; iamu++)
            {
              lopostfile << ifee576ch << "\t" << iamu << "\t" << lopostmean[ifem][ich][iamu] << "\t" << lopostrms[ifem][ich][iamu] << "\t" << 0 << endl;
            }
        }
    }
  lopostfile.close();

  name = "MpcCal.lopreped."; name += run;
  ofstream loprefile(name.Data());
  for (int ifem=0; ifem<nfem; ifem++)
    {
      for (int ich=0; ich<144; ich++)
        {
          int ifee576ch = 144*ifem + ich;

          for (int iamu=0; iamu<64; iamu++)
            {
              loprefile << ifee576ch << "\t" << iamu << "\t" << lopremean[ifem][ich][iamu] << "\t" << loprerms[ifem][ich][iamu] << "\t" << 0 << endl;
            }
        }
    }
  loprefile.close();

  name = "MpcCal.hipostped."; name += run;
  ofstream hipostfile(name.Data());
  for (int ifem=0; ifem<nfem; ifem++)
    {
      for (int ich=0; ich<144; ich++)
        {
          int ifee576ch = 144*ifem + ich;

          for (int iamu=0; iamu<64; iamu++)
            {
              hipostfile << ifee576ch << "\t" << iamu << "\t" << hipostmean[ifem][ich][iamu] << "\t" << hipostrms[ifem][ich][iamu] << "\t" << 0 << endl;
            }
        }
    }
  hipostfile.close();

  name = "MpcCal.hipreped."; name += run;
  ofstream hiprefile(name.Data());
  for (int ifem=0; ifem<nfem; ifem++)
    {
      for (int ich=0; ich<144; ich++)
        {
          int ifee576ch = 144*ifem + ich;

          for (int iamu=0; iamu<64; iamu++)
            {
              hiprefile << ifee576ch << "\t" << iamu << "\t" << hipremean[ifem][ich][iamu] << "\t" << hiprerms[ifem][ich][iamu] << "\t" << 0 << endl;
            }
        }
    }
  hiprefile.close();

  // write out root file
  if ( saveflag )
    {
      savefile->Write();
      savefile->Close();
    }

  for (int ifem=0; ifem<MAXFEM; ifem++)
    {
      histfile[ifem]->Close();
    }

  gSystem->Exit(0);
}

