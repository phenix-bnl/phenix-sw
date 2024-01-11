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

#include <cstdlib>
#include <fstream>
#include <sstream>
#include <iostream>

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
const char pixeldir[256]="pixelmap_temp";

void ratepixel(const char *infile, char *ratecutfile="ratecut.txt") {
  ///
  /// infile : output of raterun.C (raterun_[runstart]_[runend].root)
  ///
  char *char_tmp = pixeldir;
  char sumname[200];
  sprintf(sumname,"%s/ratepixelsum.txt",char_tmp);
  ofstream sumfile(sumname);

  TFile *fin = new TFile(infile);

  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);
  
  TH1::AddDirectory(kFALSE);
  //  TCanvas* c1 = new TCanvas("c1", "c1", 700, 700);
  //  c1->Divide(1,2);

  char ratepixelname[200];
  sprintf(ratepixelname,"%s/ratepixel.root",char_tmp);
  TFile *fout = new TFile(ratepixelname,"RECREATE");

  const int maxrun = 6000;
  const int maxrunc = 3000;

  const int maxranges = 300;
  ifstream fcut(ratecutfile);
  string rline;
  Float_t ratescut[maxranges][NMODULE][NCHIP];
  Float_t sigmascut[maxranges][NMODULE][NCHIP];
  Float_t minratescut[maxranges][NMODULE][NCHIP];
  Float_t maxratescut[maxranges][NMODULE][NCHIP];
  Int_t nranges[NMODULE][NCHIP];
  Int_t minruns[maxranges][NMODULE][NCHIP];
  Int_t maxruns[maxranges][NMODULE][NCHIP];

  for (Int_t im=0;im<NMODULE;im++) {
    for (Int_t ic=0;ic<NCHIP;ic++) {
      nranges[im][ic]=0;
    }
  }

  while (getline(fcut,rline)) {
    float minrate,maxrate;
    float rate,sigma;
    int im, ic,run0,run1;
    int nscan = sscanf(rline.data(),"%d\t%d\t%d\t%d\t%f\t%f\t%f\t%f",
		       &im,&ic,&run0,&run1,&minrate,&maxrate,&rate,&sigma);
    if (nscan==8) {
      // if lower threshold is abnormally low, change the threshold
      if (im<20) {
	minrate = (minrate>DEADTH0/8192.) ? minrate : DEADTH0/8192.;
      } else {
	minrate = (minrate>DEADTH1/8192.) ? minrate : DEADTH1/8192.;
      }
      minratescut[nranges[im][ic]][im][ic] = minrate;
      maxratescut[nranges[im][ic]][im][ic] = maxrate;
      ratescut[nranges[im][ic]][im][ic] = rate;
      sigmascut[nranges[im][ic]][im][ic] = sigma;
      minruns[nranges[im][ic]][im][ic] = run0;
      maxruns[nranges[im][ic]][im][ic] = run1;
      nranges[im][ic]++;
    } else {
      cerr << "******* Failed to read *******" << endl;
      cerr << rline.c_str() << endl;
    }
  }

  //all
  ostringstream opixb0;
  opixb0 << "h_pxl_rate_b0";
  TH1F *hpixelrateb0 = new TH1F(opixb0.str().c_str(),opixb0.str().c_str(),2000,0,0.010);
  hpixelrateb0->SetDirectory(0);
  ostringstream opixb1;
  opixb1 << "h_pxl_rate_b1";
  TH1F *hpixelrateb1 = new TH1F(opixb1.str().c_str(),opixb1.str().c_str(),2000,0,0.010);
  hpixelrateb1->SetDirectory(0);
  for (Int_t im=0;im<NMODULE;im++) {
    for (Int_t ic=0;ic<NCHIP;ic++) {
      for (Int_t ir=0;ir<nranges[im][ic];ir++) {
	//cout << "start im, ic, ir " << im << " " << ic << " " << ir << endl;
	ostringstream oevt;
	oevt << "h_pxl_evt"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	
	fin->cd("");
	
	Float_t rate0 = ratescut[ir][im][ic];
	TH1F *hevt = (TH1F *)gROOT->FindObject(oevt.str().c_str()); 
	if (hevt == 0) {
	  cout << "hevt == null" << endl;
	  cout << oevt.str().c_str() << endl;
	  if (rate0 == 0) {
	    cout << "rate0 = 0, continuing" << endl;
	    continue;
	  } else {
	    //	    cout << "rate0 != 0, exiting " << rate0 << endl;
	    //	    exit(-1);
	    cout << "rate0 != 0, continuing " << rate0 << endl;
	    continue;
	  }
	}
	
	Int_t nevt = hevt->GetEntries();
	ostringstream omap;
	omap << "h_pxl_hitmap"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	TH1F *hmap  = (TH1F *)gROOT->FindObject(omap.str().c_str()); 
	if (hmap == 0) {
	  cout << "hmap == null" << endl;
	  cout << oevt.str().c_str() << endl;
	  if (rate0 == 0) {
	    cout << "rate0 = 0, continuing" << endl;
	    continue;
	  } else {
	    cout << "rate0 != 0, exiting " << rate0 << endl;
	    exit(-1);
	  }
	}
	
	ostringstream opix;
	opix << "h_pxl_rate"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	TH1F *hpixelrate = new TH1F(opix.str().c_str(),opix.str().c_str(),2000,0,0.010);
	hpixelrate->SetDirectory(0);
	ostringstream ohot;
	ohot << "hotdeadmap"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	TH2F *hotdeadmap = new TH2F(ohot.str().c_str(),ohot.str().c_str(),32,-0.5,31.5,256,-0.5,255.5);
	hotdeadmap->SetDirectory(0);
	ostringstream omask;
	omask << "maskrate"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	TH2F *maskrate = new TH2F(omask.str().c_str(),omask.str().c_str(),32,-0.5,31.5,256,-0.5,255.5);
	maskrate->SetDirectory(0);
	
	Double_t deadth = DEADTH0;
	Double_t hotth = HOTTH0;
	if (im>=20) {
	  deadth = DEADTH1;
	  hotth = HOTTH1;
	}
	
	//write summaryfile
	Int_t nent = hmap->GetEntries();
	sumfile << im << "\t" << ic << "\t" << minruns[ir][im][ic] << "\t" << maxruns[ir][im][ic] << "\t" << nent << endl;
	
	char mapname[200];
	sprintf(mapname,"%s/hotdeadpixel_%d_%d_%06d_%06d.txt",char_tmp,im,ic,minruns[ir][im][ic],maxruns[ir][im][ic]);
	ofstream mapfile(mapname);
	
	for (Int_t col=0;col<NCOL;col++) {
	  for (Int_t row=0;row<NROW;row++) {
	    Int_t count = hmap->GetBinContent(col+1,row+1);
	    Double_t ratepixel = 0;
	    if (nevt>0) {
	      ratepixel = (Double_t)count/(Double_t)nevt;
	    }
	    hpixelrate->Fill(ratepixel,1.);
	    if (im<20) {
	      hpixelrateb0->Fill(ratepixel,1.);
	    } else {
	      hpixelrateb1->Fill(ratepixel,1.);
	    }
	    Int_t stat = 1;
	    if (ratepixel<deadth) {
	      stat = 0;
	    } else if (ratepixel>hotth) {
	      stat = 2;
	    }
	    if (stat==1) {
	      maskrate->Fill(col,row,ratepixel);
	    } else if (stat == 2) {
	      maskrate->Fill(col,row,hotth);
	    }
	    hotdeadmap->Fill(col,row,stat);

	    if ((stat==0)||(stat==2)) {
	      mapfile << minruns[ir][im][ic]<<"\t"<<maxruns[ir][im][ic] <<"\t"<<im<<"\t"<<ic<<"\t"<<col<<"\t"<<row<<"\t"<<stat-1<<endl;
	    }
	  }
	}
	mapfile.close();
	fout->cd("");
	//cout << "going to write " << opix.str().c_str() << endl;
	if (nevt > 0) {
	  hmap->Scale(1./(Double_t)nevt);
	}
	hmap->Write();
	hpixelrate->Write();
	maskrate->Write();
	hotdeadmap->SetMaximum(2.);
	hotdeadmap->Write();
	//cout << "end to write " << opix.str().c_str() << endl;
      }
    }
  }
  fout->cd("");
  hpixelrateb0->Write();
  hpixelrateb1->Write();

  fout->Close();
  sumfile.close();
}


