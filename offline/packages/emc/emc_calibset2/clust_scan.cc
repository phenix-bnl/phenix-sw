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
#include "nt_emc.hh"
#include "EmcTOFCorr.hh"
#include "nEmcClusterLocalExt.hh"
#endif

#include "clust_scan.hh"
#define emcDST_READING

#ifdef mDST_READING
#define NTUPLENAME "nt_emc"
#endif
#ifdef emcDST_READING
#define NTUPLENAME "nEmcClusterLocalExt"
#else
#define NTUPLENAME "cl_nt"
#endif
#include "clust_tofread.hh"

void clust_scan(){

  //gSystem->Load("libclusttr.so"); // My library...
  //gSystem->Load("libcalibset.so"); // My library...
  char* pardir = getenv("HTPARDIR");
  //  char t0_run_pbsc_name[128];
  //  char t0_run_pbgl_name[128];
  //  char t0_tower_name[128];
  //  sprintf(t0_run_pbsc_name,"%s/t0_run_pbsc",pardir);
  //  sprintf(t0_run_pbgl_name,"%s/t0_run_pbgl",pardir);
  //  sprintf(t0_tower_name,"%s/t0_tower",pardir);
  // EmcTOFCorr* emctofcorr = EmcTOFCorr::instance(t0_run_pbsc_name,t0_run_pbgl_name,t0_tower_name);
  //  "/phenix/u/htorii/local/photon/Calibset1/t0_run_pbsc",
  //    "/phenix/u/htorii/local/photon/Calibset1/t0_run_pbgl",
  //    "/phenix/u/htorii/local/photon/Calibset1/t0_tower");

  gStyle->SetOptStat(1);
  gStyle->SetOptFit(1);
  int canvas_num =0 ;
  int iarm,isect,ismz,ismy,ism,ir,iz,iy;
  int iarm_s,isect_s,ismz_s,ismy_s,iz_s,iy_s;
  int towerid;
  int ipbscgl;
  char hname[64],htitle[64];
  float pathl_org,pathl_g,dt_tofcorr,l_tofcorr,lc_tofcorr;
  int current_run = 0 ;
  Int_t nentries;

  // Position difference
  TH1F* h_pdz_sect[8];
  TH1F* h_pdz_sects[8];
  TH1F* h_pdy_sect[8];
  TH1F* h_pdy_sects[8];
  TH2F* h2_pdz_sect[8];
  TH2F* h2_pdz_sects[8];
  TH2F* h2_pdy_sect[8];
  TH2F* h2_pdy_sects[8];

  // TOF Monitor
  TH2F* h2_tofecore_sect[8];
  TH2F* h2_tofecore_chi2_sect[8];
  TH2F* h2_tofecore_lm_sect[8];
  TH2F* h2_tofecore_lmchi2_sect[8];
  TH2F* h2_tofbbct0_sect[8];
  TH2F* h2_tofbbct0_e03_sect[8];
  TH2F* h2_tofbbct0_e05_sect[8];
  TH2F* h2_tofbbct0_e05lm_sect[8];

  // TOF Tower-by-Tower Calibration vs BBCT0
  //TH2F* h2_tofbbct0_twr[24768];
  //TH2F* h2_tofbbct0_e03_twr[24768];
  //TH2F* h2_tofbbct0_e05_twr[24768];

  // TOF Tower-by-Tower Calibration
  TH1F* h_tof_twr[24768];
  //TH1F* h_tof_e05_twr[24768];
  //TH1F* h_tof_e05wide_twr[24768];

  // Run-by-Run TOF Calibration Calibration
  TH1F* h_tof_sectrun[8];
  TH1F* h_tof_e05_sectrun[8];
  TH1F* h_tof_pbscglrun[2];
  TH1F* h_tof_e05_pbscglrun[2];
  TH2F* h2_tofbbct0_e05_pbscglrun[2];

  int create_mode =0 ;
  TFile* f_init;
  TFile* f_smtwr;
  TFile* f_run;
  cout<<" Scan the ntuple : "<<NTUPLENAME<<endl;
  if( gROOT->FindObject(NTUPLENAME) == 0 ) {
    cout<<" Reading mode!!!! open the file clust_scaninit.root "<<endl;
    f_init = new TFile("clust_scaninit.root");
    f_smtwr = new TFile("clust_scansmtwr.root");
    f_run = new TFile("clust_scanrun.root");
  }else {
    cout<<" Creating mode!!!"<<endl;
    create_mode =1;
  }

  //emcRejectList("/ccj/u/htorii/local/photon/Calibset1");
  //emcRejectList("/direct/phenix+u/htorii/local/photon/Calibset1");
  emcRejectList(pardir);
  //===================================================================
  //===================================================================
  //===================================================================
  TTree* cl_nt;
  if( create_mode == 1){
    cl_nt = (TTree*)(gROOT->FindObject(NTUPLENAME));
    if( cl_nt == 0 ){
      cerr<<" Error:: Can't fetch cl_nt..."<<endl;
      exit(0);
    }

    clust_tofread();
    gROOT->cd();

#ifdef mDST_READING
    nt_emc* pt_nt_emc; // This is skeleton of mDST from Y.Akiba.
    pt_nt_emc = new nt_emc(cl_nt);
    ClustTr* clt = &(pt_nt_emc->clt);
#endif
#ifdef emcDST_READING
    nEmcClusterLocalExt* pt_emcdst; // This is skeleton of emcDST
    pt_emcdst = new nEmcClusterLocalExt(cl_nt);
    ClustTr* clt = &(pt_emcdst->clt);
#else
    ClustTr* clt = new ClustTr();
    cl_nt->SetBranchAddress("cl.",&clt);
#endif

    cout<<" Creating clust_scanrun.root for run-by-run info "<<endl;
    f_run = new TFile("clust_scanrun.root","RECREATE");

    cout<<" Creating root file clust_scansmtwr.root"<<endl;
    f_smtwr = new TFile("clust_scansmtwr.root","RECREATE");
    f_smtwr->cd();
    cout<<" Creating histgrams : TH1F* h2_tofbbct0_twr[24768] "<<endl;
    isect = 8;
    while( isect-- ){
      if( isect == 0 || isect == 1 || isect == 7 ){
	iz = (isect>=6 ? 96 : 72 );
	while( iz-- ){
	  iy = (isect>=6 ? 48 : 36 );
	  while( iy-- ){
	    towerid = (isect>=6 ?  15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz );
	    //	    sprintf(hname,"h2_tofbbct0_twr%d",towerid);
	    //	    sprintf(htitle,"Photon TOF vs BBC T0 in twr%d",towerid);
	    //	    h2_tofbbct0_twr[towerid] = new TH2F(hname,htitle,20,-10,10,200,-10,10);
	    //sprintf(hname,"h2_tofbbct0_e03_twr%d",towerid);
	    //sprintf(htitle,"Photon TOF vs BBC T0 ecore>0.3GeV in twr%d",towerid);
	    //h2_tofbbct0_e03_twr[towerid] = new TH2F(hname,htitle,20,-10,10,200,-10,10);
	    //	    sprintf(hname,"h2_tofbbct0_e05_twr%d",towerid);
	    //	    sprintf(htitle,"Photon TOF vs BBC T0 ecore>0.5GeV in twr%d",towerid);
	    //	    h2_tofbbct0_e05_twr[towerid] = new TH2F(hname,htitle,20,-10,10,200,-10,10);
	    //
	    sprintf(hname,"h_tof_twr%d",towerid);
	    sprintf(htitle,"Photon TOF in twr%d",towerid);
	    h_tof_twr[towerid] = new TH1F(hname,htitle,200,-10,10);
	    //sprintf(hname,"h_tof_e05_twr%d",towerid);
	    //sprintf(htitle,"Photon TOF ecore>0.5GeV in twr%d",towerid);
	    //h_tof_e05_twr[towerid] = new TH1F(hname,htitle,200,-10,10);
	    //sprintf(hname,"h_tof_e05wide_twr%d",towerid);
	    //sprintf(htitle,"Photon TOF ecore>0.5GeV in twr%d",towerid);
	    //h_tof_e05wide_twr[towerid] = new TH1F(hname,htitle,200,-100,100);
	  }
	}
      }
    }
    cout<<" Creating sector histgrams : TH1F* h2_tofbbct0_sect[8] etc in memory"<<endl;
    gROOT->cd();
    isect = 8;
    while( isect-- ){
      if( isect == 0 || isect == 1 || isect == 7 ){
	//
	sprintf(hname,"h2_tofecore_sect%d",isect);
	sprintf(htitle,"Photon TOF vs Ecore in Sect%d",isect);
	h2_tofecore_sect[isect] = new TH2F(hname,htitle,50,0,5,200,-10,10);
	//
	sprintf(hname,"h2_tofecore_chi2_sect%d",isect);
	sprintf(htitle,"Photon TOF vs Ecore chi2 in Sect%d",isect);
	h2_tofecore_chi2_sect[isect] = new TH2F(hname,htitle,50,0,5,200,-10,10);
	//
	sprintf(hname,"h2_tofecore_lm_sect%d",isect);
	sprintf(htitle,"Photon TOF vs Ecore multi<100 in Sect%d",isect);
	h2_tofecore_lm_sect[isect] = new TH2F(hname,htitle,50,0,5,200,-10,10);
	//
	sprintf(hname,"h2_tofecore_lmchi2_sect%d",isect);
	sprintf(htitle,"Photon TOF vs Ecore chi2 & multi<100 in Sect%d",isect);
	h2_tofecore_lmchi2_sect[isect] = new TH2F(hname,htitle,50,0,5,200,-10,10);
	//
	sprintf(hname,"h2_tofbbct0_sect%d",isect);
	sprintf(htitle,"Photon TOF vs BBC T0 in Sect%d",isect);
	h2_tofbbct0_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-10,10);
	//if( isect < 6 )
	//  h2_tofbbct0_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-50,-30);
	//else
	//  h2_tofbbct0_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-110,-90);
	//
	sprintf(hname,"h2_tofbbct0_e03_sect%d",isect);
	sprintf(htitle,"Photon TOF vs BBC T0 at ecore>0.3GeV in Sect%d",isect);
	h2_tofbbct0_e03_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-10,10);
	//if( isect < 6 )
	//h2_tofbbct0_e03_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-50,-30);
	//else
	//  h2_tofbbct0_e03_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-110,-90);
	//
	sprintf(hname,"h2_tofbbct0_e05_sect%d",isect);
	sprintf(htitle,"Photon TOF vs BBC T0 at ecore>0.5GeV in Sect%d",isect);
	h2_tofbbct0_e05_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-10,10);
	//if( isect < 6 )
	//  h2_tofbbct0_e05_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-50,-30);
	//else
	//  h2_tofbbct0_e05_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-110,-90);
	//
	sprintf(hname,"h2_tofbbct0_e05lm_sect%d",isect);
	sprintf(htitle,"Photon TOF vs BBC T0 at ecore>0.5GeV nhit<100 in Sect%d",isect);
	h2_tofbbct0_e05lm_sect[isect] = new TH2F(hname,htitle,25,-10,15,200,-10,10);
	//
	sprintf(hname,"h2_pdy_sect%d",isect);
	sprintf(htitle,"Y position difference vs Energy in Sect%d",isect);
	h2_pdy_sect[isect] = new TH2F(hname,htitle,30,0,3,200,-50,50);
	sprintf(hname,"h2_pdy_sects%d",isect);
	sprintf(htitle,"Y position difference vs Energy Swapped in Sect%d",isect);
	h2_pdy_sects[isect] = new TH2F(hname,htitle,30,0,3,200,-50,50);
	//
	sprintf(hname,"h2_pdz_sect%d",isect);
	sprintf(htitle,"Z position difference vs Energy in Sect%d",isect);
	h2_pdz_sect[isect] = new TH2F(hname,htitle,30,0,3,200,-50,50);
	sprintf(hname,"h2_pdz_sects%d",isect);
	sprintf(htitle,"Z position difference vs Energy Swapped in Sect%d",isect);
	h2_pdz_sects[isect] = new TH2F(hname,htitle,30,0,3,200,-50,50);
	//
	sprintf(hname,"h_pdy_sect%d",isect);
	sprintf(htitle,"Y position difference in Sect%d",isect);
	h_pdy_sect[isect] = new TH1F(hname,htitle,200,-50,50);
	sprintf(hname,"h_pdy_sects%d",isect);
	sprintf(htitle,"Y position difference Swapped in Sect%d",isect);
	h_pdy_sects[isect] = new TH1F(hname,htitle,200,-50,50);
	//
	sprintf(hname,"h_pdz_sect%d",isect);
	sprintf(htitle,"Z position difference in Sect%d",isect);
	h_pdz_sect[isect] = new TH1F(hname,htitle,200,-50,50);
	sprintf(hname,"h_pdz_sects%d",isect);
	sprintf(htitle,"Z position difference in Sect%d",isect);
	h_pdz_sects[isect] = new TH1F(hname,htitle,200,-50,50);
	//
      }
    }
    //
    //++++++++++++++++++++++++++++++++++++++++++++++
    cout<<" Filling all histgrams "<<endl;
#ifdef mDST_READING
    nentries = (Int_t) pt_nt_emc->GetEntries();
#endif
#ifdef emcDST_READING
    nentries = (Int_t) pt_emcdst->GetEntries();
#else
    nentries = (Int_t) cl_nt->GetEntries();
#endif

    for (Int_t i=0; i<nentries;i++) {
#ifdef mDST_READING
      pt_nt_emc->GetEntry(i);
#endif
#ifdef emcDST_READING
      pt_emcdst->GetEntry(i);
#else
      cl_nt->GetEntry(i);
#endif
      if( i%10000 == 0 ) cout<<" Event: "<<i<<"/"<<nentries
			     <<" : run "<<clt->run<<":"<<current_run<<endl;    
      //------------------------------------------------------- Finding new run -----
      if ( clt->run != current_run ){
	cerr<<" Found new run  "<<current_run<<" --> "<<clt->run<<endl;
	f_run->cd();
	current_run = clt->run;
	int isect_tmp = 8;
	while( isect_tmp-- ){
	  if( isect_tmp == 0 || isect_tmp == 1 || isect_tmp == 7 ){
	    sprintf(hname,"h_tof_sectrun%d_%d",clt->run,isect_tmp);
	    sprintf(htitle,"TOF sector%d run %d",isect_tmp,clt->run);
	    h_tof_sectrun[isect_tmp] = (TH1F*)gROOT->FindObject(hname);
	    if( h_tof_sectrun[isect_tmp] == 0 ) {
	      cout<<"               :: Creating "<<hname<<endl;
	      h_tof_sectrun[isect_tmp] = new TH1F(hname,htitle,100,-10,10);
	      //if( isect_tmp == 0 )
	      //h_tof_sectrun[isect_tmp] = new TH1F(hname,htitle,1000,-55,-25);
	      //else
	      //h_tof_sectrun[isect_tmp] = new TH1F(hname,htitle,1000,-115,-85);
	      //if( isect_tmp == 0 )
	      //h_tof_sectrun[isect_tmp] = new TH1F(hname,htitle,1000,-50,50);
	      //else
	      //h_tof_sectrun[isect_tmp] = new TH1F(hname,htitle,1000,-100,0);
	    }
	    sprintf(hname,"h_tof_e05_sectrun%d_%d",clt->run,isect_tmp);
	    sprintf(htitle,"TOF ecore>0.5GeV swapped sector%d run %d",isect_tmp,clt->run);
	    h_tof_e05_sectrun[isect_tmp] = (TH1F*)gROOT->FindObject(hname);
	    if( h_tof_e05_sectrun[isect_tmp] == 0 ) {
	      cout<<"               :: Creating "<<hname<<endl;
	      h_tof_e05_sectrun[isect_tmp] = new TH1F(hname,htitle,100,-10,10);
	      //if( isect_tmp == 0 )
	      //	h_tof_e05_sectrun[isect_tmp] = new TH1F(hname,htitle,1000,-50,50);
	      //else
	      //h_tof_e05_sectrun[isect_tmp] = new TH1F(hname,htitle,1000,-100,0);
	    }
	  }
	}
	int ipbscgl_tmp = 2;
	while( ipbscgl_tmp-- ){
	  sprintf(hname,"h_tof_pbscglrun%d_%d",clt->run,ipbscgl_tmp);
	  sprintf(htitle,"TOF pbscgl %d run %d",ipbscgl_tmp,clt->run);
	  h_tof_pbscglrun[ipbscgl_tmp] = (TH1F*)gROOT->FindObject(hname);
	  if( h_tof_pbscglrun[ipbscgl_tmp] == 0 ) {
	    cout<<"               :: Creating "<<hname<<endl;
	    h_tof_pbscglrun[ipbscgl_tmp] = new TH1F(hname,htitle,100,-10,10);
	    //if( ipbscgl_tmp == 0 )
	    //h_tof_pbscglrun[ipbscgl_tmp] = new TH1F(hname,htitle,1000,-50,50);
	    //else
	    //h_tof_pbscglrun[ipbscgl_tmp] = new TH1F(hname,htitle,1000,-100,0);
	  }
	  sprintf(hname,"h_tof_e05_pbscglrun%d_%d",clt->run,ipbscgl_tmp);
	  sprintf(htitle,"TOF ecore>0.5GeV pbscgl %d run %d",ipbscgl_tmp,clt->run);
	  h_tof_e05_pbscglrun[ipbscgl_tmp] = (TH1F*)gROOT->FindObject(hname);
	  if( h_tof_e05_pbscglrun[ipbscgl_tmp] == 0 ) {
	    cout<<"               :: Creating "<<hname<<endl;
	    h_tof_e05_pbscglrun[ipbscgl_tmp] = new TH1F(hname,htitle,100,-10,10);
	    //if( ipbscgl_tmp == 0 )
	    //h_tof_e05_pbscglrun[ipbscgl_tmp] = new TH1F(hname,htitle,1000,-50,50);
	    //else
	    //h_tof_e05_pbscglrun[ipbscgl_tmp] = new TH1F(hname,htitle,1000,-100,0);

	  }

	  sprintf(hname,"h2_tofbbct0_e05_pbscglrun%d_%d",clt->run,ipbscgl_tmp);
	  sprintf(htitle,"TOF vs BBCT0 ecore>0.5GeV pbscgl %d run %d",ipbscgl_tmp,clt->run);
	  h2_tofbbct0_e05_pbscglrun[ipbscgl_tmp] = (TH2F*)gROOT->FindObject(hname);
	  if( h2_tofbbct0_e05_pbscglrun[ipbscgl_tmp] == 0 ) {
	    cout<<"               :: Creating "<<hname<<endl;
	    h2_tofbbct0_e05_pbscglrun[ipbscgl_tmp] = new TH2F(hname,htitle,25,-10,15,200,-10,10);
	    //if( ipbscgl_tmp == 0 )
	    // h2_tofbbct0_e05_pbscglrun[ipbscgl_tmp] = new TH2F(hname,htitle,25,-10,15,200,-50,-30);
	    //else
	    // h2_tofbbct0_e05_pbscglrun[ipbscgl_tmp] = new TH2F(hname,htitle,25,-10,15,200,-110,-90);
	  }
	}
	gROOT->cd();
      }
      //--------------------------------------------------------- 
      if( clt->bbcz>-40 && clt->bbcz<40 &&
	  clt->zdcz>-40 && clt->zdcz<40 &&
	  ( clt->bbcz - clt->zdcz ) < 10 &&
	  ( (clt->arm == 0 && clt->sector == 0 ) ||
	    (clt->arm == 0 && clt->sector == 1 ) ||
	    (clt->arm == 1 && clt->sector == 1 ) ) ){
	isect = (int)(clt->sector + clt->arm * 6); // FIX.ME
	ipbscgl = isect>=6 ? 1 : 0 ;
	ismz = isect>=6 ? (int)(clt->ind[0]/6):(int)(clt->ind[0]/12);
	ismy = isect>=6 ? (int)(clt->ind[1]/4):(int)(clt->ind[1]/12);
	iz = (int)clt->ind[0];
	iy = (int)clt->ind[1];
	//isect_s = (int)(clt->sector_s + clt->arm_s * 6); // FIX.ME
	//ismz_s = isect_s>=6 ? (int)(clt->ind_s[0]/6):(int)(clt->ind_s[0]/12);
	//ismy_s = isect_s>=6 ? (int)(clt->ind_s[1]/4):(int)(clt->ind_s[1]/12);
	//iz_s = (int)clt->ind_s[0];
	//iy_s = (int)clt->ind_s[1];
	towerid = (isect>=6 ?  15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz );
	//cout<<" isect,iz,iy, towerid = "<<isect<<","<<iz<<","<<iy<<","<<towerid<<endl;
	//------------------------------------------------------- Position Filling-----
	if( frejtwr[isect][iz][iy] < 1024 ){
	  if( clt->quality > 20 && clt->ptot < 10 ){
	    h_pdy_sect[isect]->Fill(clt->corpos[1] - clt->newproj[1]);
	    h_pdz_sect[isect]->Fill(clt->corpos[2] - clt->newproj[2]);
	    h2_pdy_sect[isect]->Fill(clt->ecore,clt->corpos[1] - clt->newproj[1]);
	    h2_pdz_sect[isect]->Fill(clt->ecore,clt->corpos[2] - clt->newproj[2]);
	  }
	  if( clt->quality_s > 20 && clt->ptot_s < 10 ){
	    h_pdy_sects[isect]->Fill(clt->corpos_s[1] - clt->newproj_s[1]);
	    h_pdz_sects[isect]->Fill(clt->corpos_s[2] - clt->newproj_s[2]);
	    h2_pdy_sects[isect]->Fill(clt->ecore,clt->corpos_s[1] - clt->newproj_s[1]);
	    h2_pdz_sects[isect]->Fill(clt->ecore,clt->corpos_s[2] - clt->newproj_s[2]);
	  }
	}
	//------------------------------------------------------- Filling-----
	//	pathl_org = sqrt(clt->pos[0]*clt->pos[0]
	//			 + clt->pos[1]*clt->pos[1]
	//			 + clt->pos[2]*clt->pos[2]);
	//	pathl_g = sqrt(clt->pos[0]*clt->pos[0]
	//		       + clt->pos[1]*clt->pos[1]
	//		       + (clt->pos[2] - clt->bbcz)*(clt->pos[2] - clt->bbcz));
	//	dt_tofcorr = (pathl_g - pathl_org)/29.979;


	//l_tofcorr = clt->tofcorr;

	// THIS IS VERY TEMPORALY PART...... FIX.MEEEEEE
	// THIS IS VERY TEMPORALY PART...... FIX.MEEEEEE
	// THIS IS VERY TEMPORALY PART...... FIX.MEEEEEE
	if(tofbbcpar_lc_stat[towerid] && tofbbcpar_lc[towerid] > -1. ) {
	  lc_tofcorr = 1./(1. + tofbbcpar_lc[towerid] );
	  lc_tofcorr = lc_tofcorr/1.075;
	} else {
	  lc_tofcorr = 1.;
	}
	if( tof_pbscglrunall_stat[ipbscgl][clt->run] )
	  dt_tofcorr = tof_pbscglrunall[ipbscgl][clt->run];
	else
	  dt_tofcorr = 0;
	l_tofcorr = ( clt->tofcorr - 30. + clt->bbct0 + dt_tofcorr )* lc_tofcorr 
	  - clt->bbct0 + 30. - dt_tofcorr;

	//	if(tofbbcpar_lc_stat[towerid] && tofbbcpar_lc[towerid] > -1. )
	//	  lc_tofcorr = 1./(1. + tofbbcpar_lc[towerid] );
	//	else
	//	  lc_tofcorr = 1.;

	// For the mDST reading
	//	l_tofcorr = clt->tof * lc_tofcorr - dt_tofcorr - clt->bbct0
	//	  - emctofcorr->get_correction( clt->run, clt->arm, clt->sector, clt->ind[1], clt->ind[0] );
	//	if( tofe05par_t0_stat[towerid] )
	//	  l_tofcorr  = l_tofcorr - tofe05par_t0[towerid];
	//	if( tof_pbscglrunall_stat[ipbscgl][clt->run] )
	//	  l_tofcorr  = l_tofcorr - tof_pbscglrunall[ipbscgl][clt->run];

	//cout<<" --------------------------------------- "<<endl;
	//cout<<" emctofcorr->get_correction() = "
	//<<emctofcorr->get_correction( clt->run, clt->arm, clt->sector, clt->ind[1], clt->ind[0] )
	//<<endl;
	//cout<<" clt->tof = "<<clt->tof<<endl;
	//cout<<" clt->tofcorr = "<<clt->tofcorr<<endl;
	//cout<<" l_tofcorr = "<<l_tofcorr<<endl;

	// For the mDST reading
	//l_tofcorr = clt->tofcorr - dt_tofcorr
	// For the DST reading
	//l_tofcorr = clt->tofcorr - dt_tofcorr - clt->bbct0 ;

	if( frejtwr[isect][iz][iy] < 1024 ){
	  // Scatterplot
	  h2_tofecore_sect[isect]->Fill(clt->ecore, l_tofcorr );
	  if( clt->chi2 < 2 )
	    h2_tofecore_chi2_sect[isect]->Fill(clt->ecore, l_tofcorr );
	  if( clt->emcnhit < 100 ){
	    h2_tofecore_lm_sect[isect]->Fill(clt->ecore, l_tofcorr );
	    if( clt->chi2 < 2 )
	      h2_tofecore_lmchi2_sect[isect]->Fill(clt->ecore, l_tofcorr );
	  }
	  if( clt->chi2 < 2 ){
	    // Run-by-Run
	    if( clt->ecent > 0.1 ){
	      h_tof_sectrun[isect]->Fill(l_tofcorr);
	      h_tof_pbscglrun[ipbscgl]->Fill(l_tofcorr);
	    }
	    if( clt->ecent > 0.5 ){
	      h_tof_e05_sectrun[isect]->Fill(l_tofcorr);
	      h_tof_e05_pbscglrun[ipbscgl]->Fill(l_tofcorr);
	      h2_tofbbct0_e05_pbscglrun[ipbscgl]->Fill(clt->bbct0, l_tofcorr );
	    }
	    // TOF Monitor
	    h2_tofbbct0_sect[isect]->Fill( clt->bbct0, l_tofcorr);
	    if( clt->ecent > 0.3 )
	      h2_tofbbct0_e03_sect[isect]->Fill( clt->bbct0, l_tofcorr);
	    if( clt->ecent > 0.5 )
	      h2_tofbbct0_e05_sect[isect]->Fill( clt->bbct0, l_tofcorr);
	    if( clt->ecent > 0.5 && clt->emcnhit < 100 )
	      h2_tofbbct0_e05lm_sect[isect]->Fill( clt->bbct0, l_tofcorr);
	  }
	}

	// TOF Tower-by-Tower Calibration vs BBCT0
	//if( clt->chi2 < 2 )
	//	h2_tofbbct0_twr[towerid]->Fill( clt->bbct0, l_tofcorr);
	//if( clt->chi2 < 2 && clt->ecore > 0.3 )
	// h2_tofbbct0_e03_twr[towerid]->Fill( clt->bbct0, l_tofcorr);
	//	if( clt->ecore > 0.5 )
	//	  h2_tofbbct0_e05_twr[towerid]->Fill( clt->bbct0, l_tofcorr);

	// TOF Tower-by-Tower Calibration
	if( clt->chi2 < 2 && clt->ecore > 0.3 )
	  h_tof_twr[towerid]->Fill( l_tofcorr);
	//if( clt->ecore > 0.5 ) {
	//h_tof_e05_twr[towerid]->Fill( l_tofcorr);
	//h_tof_e05wide_twr[towerid]->Fill( l_tofcorr );
	//}
	//------------------------------------------------------- Filling-----
      } // End of event selection
      //=== End of loop
    }
    {
      TDirectory* current = gDirectory;
      cout<<" Creating root file clust_scaninit.root"<<endl;
      f_init = new TFile("clust_scaninit.root","RECREATE");
      gROOT->GetList()->Write();
      f_init->Close();
      current->cd();

      cout<<" Closing root file clust_scanrun.root"<<endl;
      f_run->Write();
      f_run->Close();
      cout<<" Opening root file clust_scanrun.root. again"<<endl;
      f_run = new TFile("clust_scanrun.root");

      cout<<" Closing root file clust_scansmtwr.root"<<endl;
      f_smtwr->Write();
      f_smtwr->Close();
      cout<<" Opening root file clust_scansmtwr.root. again"<<endl;
      f_smtwr = new TFile("clust_scansmtwr.root");
    }
    //++++++++++++++++++++++++++++++++++++++++++++++
  } else { // Reading Mode
    f_init->cd();
    isect = 8;
    while( isect-- ){
      if( isect == 0 || isect == 1 || isect == 7 ){
	//
	sprintf(hname,"h2_tofbbct0_sect%d",isect);
	h2_tofbbct0_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_tofbbct0_sect[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
	sprintf(hname,"h2_tofbbct0_e03_sect%d",isect);
	h2_tofbbct0_e03_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_tofbbct0_e03_sect[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
	sprintf(hname,"h2_tofbbct0_e05_sect%d",isect);
	h2_tofbbct0_e05_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_tofbbct0_e05_sect[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
	sprintf(hname,"h2_pdy_sect%d",isect);
	h2_pdy_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_pdy_sect[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	sprintf(hname,"h2_pdy_sects%d",isect);
	h2_pdy_sects[isect] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_pdy_sects[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
	sprintf(hname,"h2_pdz_sect%d",isect);
	h2_pdz_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_pdz_sect[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	sprintf(hname,"h2_pdz_sects%d",isect);
	h2_pdz_sects[isect] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_pdz_sects[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
	sprintf(hname,"h_pdy_sect%d",isect);
	h_pdy_sect[isect] = (TH1F*)(gROOT->FindObject(hname));
	if( h_pdy_sect[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	sprintf(hname,"h_pdy_sects%d",isect);
	h_pdy_sects[isect] = (TH1F*)(gROOT->FindObject(hname));
	if( h_pdy_sects[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
	sprintf(hname,"h_pdz_sect%d",isect);
	h_pdz_sect[isect] = (TH1F*)(gROOT->FindObject(hname));
	if( h_pdz_sect[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	sprintf(hname,"h_pdz_sects%d",isect);
	h_pdz_sects[isect] = (TH1F*)(gROOT->FindObject(hname));
	if( h_pdz_sects[isect] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
      }
    }
  }
  //===================================================================
  //===================================================================

  cout<<" Creating clust_scanplot.ps ... "<<endl;
  gROOT->cd();
  TPostScript* ps = new TPostScript("clust_scanplot.ps");
  TCanvas* c1 = new TCanvas("c1","Cluster energy",700,900);

  //===================================================================
  //===================================================================
  //=================================================================== Position difference
  canvas_num = 0;
  isect = 8;
  c1->Divide(6,3);
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      c1->cd( ++canvas_num );
      h2_pdz_sect[isect]->Draw("box");
      c1->cd( ++canvas_num );
      h2_pdz_sects[isect]->Draw("box");
      c1->cd( ++canvas_num );
      h2_pdy_sect[isect]->Draw("box");
      c1->cd( ++canvas_num );
      h2_pdy_sects[isect]->Draw("box");
      c1->cd( ++canvas_num );
      h_pdz_sect[isect]->Draw();
      h_pdz_sects[isect]->SetLineColor(2);
      h_pdz_sects[isect]->Draw("same");
      c1->cd( ++canvas_num );
      h_pdy_sect[isect]->Draw();
      h_pdy_sects[isect]->SetLineColor(2);
      h_pdy_sects[isect]->Draw("same");
    }
  }
  c1->cd();
  c1->Update();
  //  getchar();
  ps->NewPage();
  //===================================================================
  //=================================================================== Sector Fitting
  FitPeak2D* fp2d_sect[8];
  FitPeak2D* fp2d_e03_sect[8];
  FitPeak2D* fp2d_e05_sect[8];
  TF1* fit_sect[8];
  int cutnum = 19;
  int cutbin[20] = { 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
  //float peakmin[19] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
  float peakmin[19] = {-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10};
  float peakmax[19] = {1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5};
  float peaksig[19] = {0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8};
  TF1* tfpol1 = new TF1("tfpol1","pol1",-5,5);
  gStyle->SetOptFit(1);
  
  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      fp2d_sect[isect] = new FitPeak2D();
      fp2d_sect[isect]->Fitbin(h2_tofbbct0_sect[isect],'x',cutnum,cutbin,peakmin,peakmax,peaksig,"_tof","0");
      c1->Clear();
      c1->cd();
      fp2d_sect[isect]->gra[1]->Fit("tfpol1","R");
      h2_tofbbct0_sect[isect]->Draw("colz");
      fp2d_sect[isect]->gra[1]->Draw("same");
      c1->cd();
      c1->Update();
      //      getchar();
    }
  }

  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      fp2d_e03_sect[isect] = new FitPeak2D();
      fp2d_e03_sect[isect]->Fitbin(h2_tofbbct0_e03_sect[isect],'x',cutnum,cutbin,peakmin,peakmax,peaksig,"_tof","0");
      c1->Clear();
      c1->cd();
      fp2d_e03_sect[isect]->gra[1]->Fit("tfpol1","R");
      h2_tofbbct0_e03_sect[isect]->Draw("colz");
      fp2d_e03_sect[isect]->gra[1]->Draw("same");
      c1->cd();
      c1->Update();
      //      getchar();
    }
  }

  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      fp2d_e05_sect[isect] = new FitPeak2D();
      fp2d_e05_sect[isect]->Fitbin(h2_tofbbct0_e05_sect[isect],'x',cutnum,cutbin,peakmin,peakmax,peaksig,"_tof","0");
      c1->Clear();
      c1->cd();
      fp2d_e05_sect[isect]->gra[1]->Fit("tfpol1","R");
      h2_tofbbct0_e05_sect[isect]->Draw("colz");
      fp2d_e05_sect[isect]->gra[1]->Draw("same");
      c1->cd();
      c1->Update();
      //      getchar();
    }
  }

  //===================================================================
  //=================================================================== Sector Plotting
  TH2F* h2_frame = new TH2F("h2_frame","",10,-10,10,10,-2,3);
  c1->Clear();
  c1->Divide(3,3);
  canvas_num = 0;
  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      c1->cd(++canvas_num);
      h2_frame->DrawCopy();
      fp2d_sect[isect]->gra[1]->SetLineWidth(4);
      fp2d_sect[isect]->gra[1]->SetLineColor(2);
      fp2d_sect[isect]->gra[1]->Draw("L*");
      h2_tofbbct0_sect[isect]->Draw("boxsame");
      //
      c1->cd(++canvas_num);
      h2_frame->DrawCopy();
      fp2d_e03_sect[isect]->gra[1]->SetLineWidth(4);
      fp2d_e03_sect[isect]->gra[1]->SetLineColor(2);
      fp2d_e03_sect[isect]->gra[1]->Draw("L*");
      h2_tofbbct0_e03_sect[isect]->Draw("boxsame");
      //
      c1->cd(++canvas_num);
      h2_frame->DrawCopy();
      fp2d_e05_sect[isect]->gra[1]->SetLineWidth(4);
      fp2d_e05_sect[isect]->gra[1]->SetLineColor(2);
      fp2d_e05_sect[isect]->gra[1]->Draw("L*");
      h2_tofbbct0_e05_sect[isect]->Draw("boxsame");
    }
  }
  h2_frame->Delete();
  c1->cd();
  c1->Update();
  //  getchar();
  ps->NewPage();
  c1->cd();
  c1->Update();
  ps->Close();
  //===================================================================
  //===================================================================
  //===================================================================
  //===================================================================
  {
    TDirectory* current = gDirectory;
    cout<<" Creating root file clust_scanplot.root"<<endl;
    TFile* f_plot = new TFile("clust_scanplot.root","RECREATE");
    gROOT->GetList()->Write();
    f_plot->Close();
    current->cd();
  }

}
//===============================================================
