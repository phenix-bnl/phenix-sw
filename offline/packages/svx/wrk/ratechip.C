#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>

#include <TROOT.h>
#include <TDirectory.h>
#include <TSystem.h>
#include <TStyle.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
//#include <TGraph2D.h>
#include <TText.h>
#include <TPaveText.h>
#include <TGaxis.h>
#include <TRegexp.h>

//#include "utilfuncs.C"

using namespace std;
const int NMODULE=60;
const int NCHIP=8;
const int NCOL=32;
const int NROW=256;
const double DEADTH0 = 0.0004;
const double DEADTH1 = 0.00015;
const double HOTTH0 = 0.0034;
const double HOTTH1 = 0.0023;
const char chipdir[256]="ratechip_data_temp";
const char rundir[256]="raterun_data_temp";

void ratechip(const char *fname) {
  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);

  TFile *f = new TFile(fname);
  int run=-1;
  int seq=0;
  // get run and segment number from file name.
  int nscan = sscanf(fname,"svxdstqa2/SvxDstQA_Merge_%06d_%04d.root", &run, &seq);
  if (nscan<=0) {
    cerr << "Default segment number is used." << endl;
  } else {
    cout << "run, seq = " << run << " " << seq << endl;
  }

  TH2F *hists[NMODULE][NCHIP];
  char hname[100];
  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      sprintf(hname,"h_pxl_hitmap_%d_%d",imod,ichip);
      hists[imod][ichip] = (TH2F *)gROOT->FindObject(hname);
    }
  }

  TH1F *hrun = (TH1F *)gROOT->FindObject("h_pxl_run");
  if ( !hrun ) {
    cout << "ERROR : can't get run information" << endl;
    return;
  }
  int run = hrun->GetBinContent(1);
  TH1F *hevt = (TH1F *)gROOT->FindObject("h_pxl_evt");
  int nevt = hevt->GetEntries();

  gStyle->SetPalette(1);
  TH1F *hrate_chip_all_raw = new TH1F("hrate_chip_all_raw","rate / pixel average within chip (raw)",480,-0.5,479.5);
  TH1F *hrate_chip_all = new TH1F("hrate_chip_all","rate / pixel average within chip (trim)",480,-0.5,479.5);

  char *char_tmp = chipdir;
  char ratechipfile[200];
  sprintf(ratechipfile,"%s/ratechip_%06d_%04d.txt",char_tmp,run,seq);
  ofstream ratechiptxt(ratechipfile);
  cout << "(RUN, SEQUENCE) : (" << run <<", "<< seq <<")"<< endl;

  TH2F *hotdeadmap[NMODULE][NCHIP];
  TH2F *hitmap[NMODULE][NCHIP];

  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      ostringstream ohot;
      ohot << "hotdeadmap" << "_" << imod << "_" << ichip;
      hotdeadmap[imod][ichip] = new TH2F(ohot.str().c_str(),ohot.str().c_str(),32,-0.5,31.5,256,-0.5,255.5);
      ostringstream orate;
      orate << "hitmap" << "_" << imod << "_" << ichip;
      hitmap[imod][ichip] = new TH2F(orate.str().c_str(),orate.str().c_str(),32,-0.5,31.5,256,-0.5,255.5);

      Double_t average = (Double_t)hists[imod][ichip]->GetSum()/(Double_t)nevt/8192.;
      Int_t chipseq = ichip+imod*NCHIP;
      hrate_chip_all_raw->Fill((double)chipseq,average);

      //
      // hard-corded cut parameters to define normal/hot/dead
      //
      Double_t deadth = DEADTH0;
      Double_t hotth = HOTTH0;
      if (imod>=20) {
	deadth = DEADTH1;
	hotth = HOTTH1;
      }

      //recalculate rate by removing hot pixels
      Double_t rtrim=0;
      for (int icol=0;icol<NCOL;icol++) {
	for (int irow=0;irow<NROW;irow++) {
	  Double_t r = hists[imod][ichip]->GetBinContent(icol+1,irow+1)/(Double_t)nevt;
	  if (r<hotth) {
	    rtrim += r;
	  }
	  Int_t stat = 1;
	  if (r<deadth) {
	    stat = 0;
	  } else if (r>hotth) {
	    stat = 2;
	  }
	  hotdeadmap[imod][ichip]->Fill(icol,irow,stat);
	  hitmap[imod][ichip]->Fill(icol,irow,hists[imod][ichip]->GetBinContent(icol+1,irow+1));
	}
      }
      rtrim /= 8192.;
      hrate_chip_all->Fill((double)chipseq,rtrim);
      ratechiptxt << imod<< "\t" << ichip<< "\t" << average << "\t" << rtrim << endl;
    }
  }

  char foname[200];
  sprintf(foname,"%s/ratechip_%06d_%04d.root", char_tmp, run, seq);
  TFile *fo = new TFile(foname,"RECREATE");

  hrate_chip_all->Write();
  hrate_chip_all_raw->Write();
  hevt ->Write();

  for (Int_t imod=0;imod<NMODULE;imod++) {
    for (Int_t ichip=0;ichip<NCHIP;ichip++) {
      hotdeadmap[imod][ichip]->SetMaximum(2.);
      hotdeadmap[imod][ichip]->Write();
      hitmap[imod][ichip]->Write();
    }
  }
  fo->Close();
  delete fo;

  delete hrate_chip_all;
  delete hrate_chip_all_raw;
  for (Int_t imod=0;imod<NMODULE;imod++) {
    for (Int_t ichip=0;ichip<NCHIP;ichip++) {
      delete hotdeadmap[imod][ichip];
      delete hitmap[imod][ichip];
    }
  }

  f->Close();
  delete f;
  ratechiptxt.close();

  cout << "finish" << endl << endl;
}

