#ifdef COMPILE
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <Rtypes.h>
#include <TSystem.h>
#include <TROOT.h>
#include <TFile.h>
#include <TKey.h>
#include <TTree.h>
#include <TBranch.h>
#include <TNtuple.h>
#include <TCanvas.h>
#include <TPad.h>
#include <TStyle.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <fstream.h>
#include <vector>
#include "ClustTr.hh"
#include "emcRejectList.hh"
#include "FitPeak2D.hh"
#include "fitpeak.hh"
#endif

#define GLOBAL
#include "clust_tofread.hh"

void clust_tofread(){

  gStyle->SetOptStat(1);
  gStyle->SetOptFit(1);
  int canvas_num =0 ;
  int iarm,isect,ismz,ismy,ism,ir,iz,iy,itwr;
  int iarm_s,isect_s,ismz_s,ismy_s,iz_s,iy_s,itwr_s;
  int towerid;
  int ipar;
  int irun,bin;
  int ipbscgl;
  char hname[256],htitle[256];

  //===================================================================
  //===================================================================
  //=================================================================== Open clust_tofcorr.root
  //===================================================================
  cout<<" Open clust_tofcorr.root... "<<endl;
  char* pardir = getenv("HTPARDIR");
  char filename[128];
  sprintf(filename,"%s/clust_tofcorr.root",pardir);
  //TFile* f_tofcorr  = new TFile("/phenix/u/htorii/local/photon/Calibset1/clust_tofcorr.root");
  TFile* f_tofcorr  = new TFile(filename);
  //
  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      //
      sprintf(hname,"h_tofbbcpar_t0%d",isect);
      h_tofbbcpar_t0[isect] = (TH1F*) gROOT->FindObject(hname);
      sprintf(hname,"h_tofbbcpar_lc%d",isect);
      h_tofbbcpar_lc[isect] = (TH1F*) gROOT->FindObject(hname);
      //
      sprintf(hname,"h2_tofbbcpar_t0%d",isect);
      h2_tofbbcpar_t0[isect] = (TH2F*) gROOT->FindObject(hname);
      sprintf(hname,"h2_tofbbcpar_lc%d",isect);
	h2_tofbbcpar_lc[isect] = (TH2F*) gROOT->FindObject(hname);
      //
      ipar = 4;
      while( ipar-- ){
	sprintf(hname,"h_tofbbc_slipar%d_%d",isect,ipar);
	h_tofbbc_slipar[isect][ipar] = (TH1F*) gROOT->FindObject(hname);
	//
	sprintf(hname,"h_tofbbc_slipar_err%d_%d",isect,ipar);
	h_tofbbc_slipar_err[isect][ipar] = (TH1F*) gROOT->FindObject(hname);
      }
      //----------
      ipar = 3;
      while( ipar-- ){
	sprintf(hname,"h_tofbbcpar%d_%d",isect,ipar);
	h_tofbbcpar[isect][ipar] = (TH1F*) gROOT->FindObject(hname);
	//
	sprintf(hname,"h_tofbbcpar_err%d_%d",isect,ipar);
	h_tofbbcpar_err[isect][ipar] = (TH1F*) gROOT->FindObject(hname);
	//
	sprintf(hname,"h2_tofbbcpar%d_%d",isect,ipar);
	h2_tofbbcpar[isect][ipar] = (TH2F*) gROOT->FindObject(hname);
      }
      sprintf(hname,"h2_tofbbcpar_stat%d",isect);
      h2_tofbbcpar_stat[isect] = (TH2F*) gROOT->FindObject(hname);
    }
  }
  //===================================================================
  //===================================================================
  isect = 8;
  while( isect-- ){
    // LINESM
    if( isect == 0 || isect == 1 || isect == 7 ){
      //if( isect == 0 ){
      //iz = 12;
      iz = (isect>=6 ? 96 : 72 );
      while( iz-- ){
	iy = (isect>=6 ? 48 : 36 );
	while( iy-- ){
	  //cout<<" tower reading:: isect = "<<isect<<endl;
	  //cout<<"              :: iz,iy = "<<iz<<","<<iy<<endl;
	  towerid = (isect>=6 ?  15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz );
	  int bin = h2_tofbbcpar_lc[isect]->FindBin(iz,iy);
	  tofbbcpar_lc[towerid] = h2_tofbbcpar_lc[isect]->GetBinContent(bin);
	  tofbbcpar_lc_err[towerid] = h2_tofbbcpar_lc[isect]->GetBinError(bin);
	  // FIX.ME
	  //tofbbcpar_lc_stat[towerid] = h2_tofbbcpar_stat[isect]->GetBinContent(bin);

	  if( h2_tofbbcpar_lc[isect]->GetBinError(bin) < 0.04 && // bbcT0 (10nsec) --> 400nsec
	      tofbbcpar_lc[towerid] < 0.1 && tofbbcpar_lc[towerid] > -0.3 )
	    tofbbcpar_lc_stat[towerid] = 1;
	  else
	    tofbbcpar_lc_stat[towerid] = 0;
	}
      }
    }
  }
#ifdef SKIPSKIPRUNRUNRUN
  isect = 8;
  while( isect-- ){
    // LINESM
    if( isect == 0 || isect == 1 || isect == 7 ){
      cout<<" ----------------------------------------------------- "<<endl;
      cout<<" isect = "<<isect<<endl;
      iz = (isect>=6 ? 96 : 72 );
      while( iz-- ){
	iy = (isect>=6 ? 48 : 36 );
	cout<<" "<<iy;
	while( iy-- ){
	  towerid = (isect>=6 ?  15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz );
	  cout<<" "<<tofbbcpar_lc_stat[towerid];
	}
	cout<<endl;
      }
      cout<<" ----------------------------------------------------- "<<endl;
      cout<<endl<<endl;
    }
  }
#endif
  //===================================================================
  //===================================================================
  //=================================================================== Open clust_tofcorr2.root
  //===================================================================
  cout<<" Open clust_tofcorr2_0105_step1.2.root... "<<endl;
  //char* pardir = getenv("HTPARDIR");
  //char filename[128];
  sprintf(filename,"%s/clust_tofcorr2_0105_step1.2.root",pardir);
  TFile* f_tofcorr2  = new TFile(filename);
  //===================================================================
  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      ipar = 4;
      while( ipar-- ){
	sprintf(hname,"h_tofe05par%d_%d",isect,ipar);
	h_tofe05par[isect][ipar] = (TH1F*) gROOT->FindObject(hname);
	//
	sprintf(hname,"h_tofe05par_err%d_%d",isect,ipar);
	h_tofe05par_err[isect][ipar] = (TH1F*) gROOT->FindObject(hname);
	//
	sprintf(hname,"h2_tofe05par%d_%d",isect,ipar);
	h2_tofe05par[isect][ipar] = (TH2F*) gROOT->FindObject(hname);
      }
      sprintf(hname,"h2_tofe05par_stat%d",isect);
      h2_tofe05par_stat[isect] = (TH2F*) gROOT->FindObject(hname);
      //
    }
  }

  //===================================================================
  //===================================================================
  //gROOT->cd();
  //TCanvas* c1 = new TCanvas("c1","Cluster TOF correction",700,900);

  //===================================================================
  //=================================================================== e05 Fitting parameter Plotting
  //cout<<" e05 fitting parameter plotting. "<<endl;
  //c1->Clear();
  //c1->Divide(4,4);
  canvas_num = 0;
  isect = 8;
  while( isect-- ){
    // LINESM
    if( isect == 0 || isect == 1 || isect == 7 ){
      //if( isect == 0 ){
      iz = (isect>=6 ? 96 : 72 );
      while( iz-- ){
	iy = (isect>=6 ? 48 : 36 );
	while( iy-- ){
	  //cout<<" tower reading:: isect = "<<isect<<endl;
	  //cout<<"              :: iz,iy = "<<iz<<","<<iy<<endl;
	  towerid = (isect>=6 ?  15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz );
	  int bin = h2_tofe05par_stat[isect]->FindBin(iz,iy);
	  tofe05par_t0[towerid] = h2_tofe05par[isect][1]->GetBinContent(bin);
	  tofe05par_t0_err[towerid] = h2_tofe05par[isect][1]->GetBinError(bin);
	  if( h2_tofe05par[isect][1]->GetBinError(bin) < 0.2 && // 0.2nsec...
	      tofe05par_t0[towerid] < 5 & tofe05par_t0[towerid] > -20 )
	    tofe05par_t0_stat[towerid] = 1;
	  else
	    tofe05par_t0_stat[towerid] = 0;
	}
      }
      //      ipar = 4;
      //      while( ipar-- ){
      //	c1->cd( ++canvas_num );
      //	h_tofe05par[isect][ipar]->Draw();
      //	c1->cd( ++canvas_num );
      //	h_tofe05par_err[isect][ipar]->Draw();
      //	c1->cd( ++canvas_num );
      //	h2_tofe05par[isect][ipar]->Draw("colz");
      //	c1->cd( ++canvas_num );
      //	h2_tofe05par_stat[isect]->Draw("colz");
      //      }
      //      c1->cd();
      //      c1->Update();
      //      getchar();
      //      canvas_num = 0;
    }
  }

  //  c1->cd();
  //  c1->Update();

  //===================================================================
  //===================================================================
  //=================================================================== Open clust_tofrun.root
  //===================================================================
  cout<<" Open clust_tofrun0105_step0.root... "<<endl;
  //char* pardir = getenv("HTPARDIR");
  //char filename[128];
  //sprintf(filename,"%s/clust_tofrun_set1.root",pardir);
  //sprintf(filename,"%s/clust_tofrun_new0.root",pardir);
  sprintf(filename,"%s/clust_tofrun0105_step0.root",pardir);
  TFile* f_tofrun  = new TFile(filename);
  // Run-by-Run TOF Calibration Calibration

  ipbscgl = 2;
  while(ipbscgl--){
    sprintf(hname,"h_tof_pbscglrunall_%d",ipbscgl);
    h_tof_pbscglrunall[ipbscgl] = (TH1F*) gROOT->FindObject(hname);
    irun = 20000;
    while( irun-- ){
      bin = h_tof_pbscglrunall[ipbscgl]->FindBin(irun);
      tof_pbscglrunall[ipbscgl][irun] = h_tof_pbscglrunall[ipbscgl]->GetBinContent(bin);
      tof_pbscglrunall_err[ipbscgl][irun] = h_tof_pbscglrunall[ipbscgl]->GetBinError(bin);
      if( h_tof_pbscglrunall[ipbscgl]->GetBinError(bin) < 0.5 &&
	  h_tof_pbscglrunall[ipbscgl]->GetBinContent(bin) > -10 ) 
	tof_pbscglrunall_stat[ipbscgl][irun] = 1;
    }
  }

  //===================================================================
  //===================================================================
}
//===============================================================
