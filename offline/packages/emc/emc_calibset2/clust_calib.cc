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
#include "TrackCl.hh"
#include "emcRejectList.hh"
#include "emcRejectClust.hh"
#include "fitpeak.hh"
#include "Evt.hh"
#include "Pid.hh"
#include "CalibRunsTH0.hh"
#include "CalibRunsTH1.hh"
#include "CalibRunsTH2.hh"
#include "EmcCalibratorTOF.hh"
#include "EmcCalibratorTOFBBC.hh"
#endif

#include "clust_calib.hh"

void clust_calib(Evt* evt,char* outfname,int maxevent,bool update=false,bool debug=false){

  int iarm,ipbscgl;
  int ipbscgl_s;
  int isect,ismz,ismy,iz,iy,itwr,itwr_sm,itwr_sect,ism_sect,itwr_pbscgl;
  int isect_s,ismz_s,ismy_s,iz_s,iy_s,itwr_s;
  int ism;
  int ipid,ipid_s;
  int iepid,iepid_s;
  int iprob,iprob_s;
  int icent;
  int emce,emce_s;
  char hname[256],htitle[256];
  float angle;
  float tofcorr;
  int imul;

  // --- Open output file
  cout<<" Creating root file "<<outfname<<" from memory histgram "<<endl;
  TFile* f_init;
  if( update )
    f_init = new TFile(outfname,"UPDATE");
  else
    f_init = new TFile(outfname,"RECREATE");
  //char* pardir = getenv("HTPARDIR");
  //emcRejectList(pardir);


  //---------------------------------------------------
  // --- Second Calibration
  bool second_calib = false;
  CalibRunsTH1* tof_2e05_sect[8];
  CalibRunsTH1* tof_2e05lm_sect[8];
  EmcCalibratorTOF* c_tof_sect[8];  
  EmcCalibratorTOFBBC* c_tofbbct0_sect[8];
  EmcCalibratorTOFBBC* c_tofe_sect[8];
  float f_tof_sect[8][4608];
  float f_tofe_sect[8][4608];
  float f_tofbbct0_sect[8][4608];

  if( second_calib ){
    char* basedir = "/phenix/data24/htorii/Myana_01ana/tofcalib/Step8";
    TFile* fin[8];
    isect = 8;
    while( isect-- ){
      sprintf(hname,"%s/clust_calibinit_ana_%d.root",basedir,isect);
      cout<<" Open 1st step calibration from : "<<hname<<endl;
      fin[isect] = new TFile(hname);
      if( fin[isect] == NULL ){
	cout<<" Error :: Can't read 1st step calibration fil ... "<<endl;
	second_calib = false;
      } else {
	sprintf(hname,"c_tof_sect%d",isect);
	c_tof_sect[isect] = (EmcCalibratorTOF*)fin[isect]->Get(hname);
	sprintf(hname,"c_tofe_sect%d",isect);
	c_tofe_sect[isect] = (EmcCalibratorTOFBBC*)fin[isect]->Get(hname);
	sprintf(hname,"c_tofbbct0_sect%d",isect);
	c_tofbbct0_sect[isect] = (EmcCalibratorTOFBBC*)fin[isect]->Get(hname);
	if( c_tof_sect[isect] == NULL || c_tofe_sect[isect] == NULL || c_tofbbct0_sect[isect] == NULL ){
	  cout<<" Error :: Can't read 1st step calibration object ... "<<endl;
	  second_calib = false;
	} else {
	  itwr = (isect < 6 ) ? 2592 : 4608;
	  while( itwr-- ){
	    f_tof_sect[isect][itwr] = c_tof_sect[isect]->_h_twr_shift->GetBinContent(itwr+1) +
	      c_tof_sect[isect]->_h_twr_pshift->GetBinContent(itwr+1);
	    f_tofe_sect[isect][itwr] = c_tofe_sect[isect]->_h_twr_s0.GetBinContent(itwr+1);
	    f_tofbbct0_sect[isect][itwr] = (c_tofbbct0_sect[isect]->_h_twr_s0.GetBinContent(itwr+1) + 1.0);
	  }
	  f_init->cd();
	  c_tof_sect[isect]->_h_twr_pshift->Write();
	  c_tof_sect[isect]->_h_twr_shift->Write();
	  c_tofe_sect[isect]->_h_twr_s0.Write();
	  c_tofbbct0_sect[isect]->_h_twr_s0.Write();
	}
	if( c_tof_sect[isect] )
	  delete c_tof_sect[isect];
	if( c_tofe_sect[isect] )
	  delete c_tofe_sect[isect];
	if( c_tofbbct0_sect[isect] )
	  delete c_tofbbct0_sect[isect];
	//
	fin[isect]->Close();
      }
    }
  }
  if( second_calib ){
    f_init->cd();
    isect = 8;
    while( isect-- ){
      itwr = (isect < 6 ) ? 2592 : 4608;
      sprintf(hname,"tof_2e05_sect%d",isect);
      sprintf(htitle,"tof e>0.5GeV Calibration sect %d",isect);
      tof_2e05_sect[isect] = new CalibRunsTH1(hname,htitle,itwr,500,-50,50);
      //
      sprintf(hname,"tof_2e05lm_sect%d",isect);
      sprintf(htitle,"tof e>0.5GeV low mul sect %d",isect);
      tof_2e05lm_sect[isect] = new CalibRunsTH1(hname,htitle,itwr,500,-50,50);
    }
  }

  //---------------------------------------------------
  // --- Histgrams.
  // Create Calibration histograms.
  CalibRunsTH0* qa_ene_sect[8];
  CalibRunsTH0* qa_evn_sect[8];
  CalibRunsTH1* tof_e03_sect[8];
  CalibRunsTH1* tof_e05_sect[8];
  CalibRunsTH1* tof_e05lm_sect[8];
  CalibRunsTH1* tof_e05sm_sect[8];
  //  CalibRunsTH1* mip_sect[8];
  CalibRunsTH1* mip_p01_sect[8];
  //  CalibRunsTH1* mip_p03_sect[8];
  //  CalibRunsTH1* mip_sm;
  //  CalibRunsTH1* mip_p01_sm;
  //  CalibRunsTH1* mip_p03_sm;
  CalibRunsTH2* tofbbct0_sect[8];
  //CalibRunsTH2* tofnemc_sect[8];
  CalibRunsTH2* tofe_sect[8];
  //
  TH2F* h2_tofnemc_sect[8];
  TH2F* h2_tofbbct0_sect[8];
  TH2F* h2_tofe_sect[8];
  TH1F* h_itwrsect_sect[8];
  TH1F* h_tof;
  TH1F* h_tofcorr;
  TH2F* h2_tofbbct0;
  TH2F* h2_tofe;
  //
  //---------------------------------------------------
  // --- Create Histgrams.
  f_init->cd();
  isect = 8;
  while( isect-- ){
    itwr = (isect < 6 ) ? 2592 : 4608;
    sprintf(hname,"tofbbct0_sect%d",isect);
    sprintf(htitle,"tof vs bbct0 Calibration sect %d",isect);
    if( isect < 6 )
      tofbbct0_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,5,-5,5,125,-50,-25); //It was (-70,-20)
    else
      tofbbct0_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,5,-5,5,125,-12.5,12.5); //It was (-70,-20)
    //
    sprintf(hname,"tofe_sect%d",isect);
    sprintf(htitle,"tof vs e Calibration sect %d",isect);
    if( isect < 6 )
      tofe_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,6,0,0.6,125,-50,-25);
    else
      tofe_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,6,0,0.6,125,-12.5,12.5);
    //
    //sprintf(hname,"tofnemc_sect%d",isect);
    //sprintf(htitle,"tof vs nemc Calibration sect %d",isect);
    //tofnemc_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,4,0,800,250,-70,-20);
    //
    sprintf(hname,"h2_tofnemc_sect%d",isect);
    sprintf(htitle,"tof vs nemc Calibration sect %d",isect);
    h2_tofnemc_sect[isect] = new TH2F(hname,htitle,10,0,1000,200,-100,100);
    //
    sprintf(hname,"h2_tofbbct0_sect%d",isect);
    sprintf(htitle,"tof vs BBC t0 in sect %d",isect);
    h2_tofbbct0_sect[isect] = new TH2F(hname,htitle,400,-20,20,100,-100,100);
    //
    sprintf(hname,"h2_tofe_sect%d",isect);
    sprintf(htitle,"tof vs e Calibration sect %d",isect);
    h2_tofe_sect[isect] = new TH2F(hname,htitle,100,0,1.0,200,-100,100);
    //
    sprintf(hname,"tof_e03_sect%d",isect);
    sprintf(htitle,"tof e>0.3GeV Calibration sect %d",isect);
    tof_e03_sect[isect] = new CalibRunsTH1(hname,htitle,itwr,500,-75,25);
    //
    sprintf(hname,"tof_e05_sect%d",isect);
    sprintf(htitle,"tof e>0.5GeV Calibration sect %d",isect);
    tof_e05_sect[isect] = new CalibRunsTH1(hname,htitle,itwr,500,-75,25);
    //
    sprintf(hname,"tof_e05lm_sect%d",isect);
    sprintf(htitle,"tof e>0.5GeV low mul sect %d",isect);
    tof_e05lm_sect[isect] = new CalibRunsTH1(hname,htitle,itwr,500,-75,25);
    //
    sprintf(hname,"tof_e05sm_sect%d",isect);
    sprintf(htitle,"tof e>0.5GeV SM sect %d",isect);
    tof_e05sm_sect[isect] = new CalibRunsTH1(hname,htitle,itwr,500,-75,25);
    //
    sprintf(hname,"mip_p01_sect%d",isect);
    sprintf(htitle,"MIP Calibration sect %d",isect);
    mip_p01_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    mip_p01_sect[isect]->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
    //
    //sprintf(hname,"mip_p03_sect%d",isect);
    //sprintf(htitle,"MIP Calibration sect %d",isect);
    //mip_p03_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    //mip_p03_sect[isect]->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
    //
    sprintf(hname,"qa_ene_sect%d",isect);
    sprintf(htitle,"QA energy sect %d",isect);
    qa_ene_sect[isect] = new CalibRunsTH0(hname,htitle,itwr);
    //
    sprintf(hname,"qa_evn_sect%d",isect);
    sprintf(htitle,"QA event number sect %d",isect);
    qa_evn_sect[isect] = new CalibRunsTH0(hname,htitle,itwr);
    
    
    sprintf(hname,"h_itwrsect_sect%d",isect);
    sprintf(htitle,"itwr_sect %d",isect);
    h_itwrsect_sect[isect] = new TH1F(hname,"itwr_sect",itwr,0,itwr);
  }
  //
  //    sprintf(hname,"mip_sm");
  //    sprintf(htitle,"MIP Calibration sm");
  //    mip_sm = new CalibRunsTH1(hname,htitle,172);
  //    mip_sm->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
  //
  //    sprintf(hname,"mip_p01_sm");
  //    sprintf(htitle,"MIP Calibration sm");
  //    mip_p01_sm = new CalibRunsTH1(hname,htitle,172);
  //    mip_p01_sm->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
  //
  //    sprintf(hname,"mip_p03_sm");
  //    sprintf(htitle,"MIP Calibration sm");
  //    mip_p03_sm = new CalibRunsTH1(hname,htitle,172);
  //    mip_p03_sm->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
  
  f_init->cd();
  h_tof = new TH1F("h_tof","TDC",100,-100,100);
  h_tofcorr = new TH1F("h_tofcorr","TDC",1000,-1500,1500);
  h2_tofbbct0 = new TH2F("h2_tofbbct0","TDC vs BBCT0",400,-20,20,100,-100,100);
  h2_tofe = new TH2F("h2_tofe","TDC vs E",200,0,2,100,-100,100);
  
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  cout<<" Filling all histgrams "<<endl;
  while ( evt->Next() && (evt->GetN() < maxevent || maxevent == 0 ) ) {
    if( evt->GetN() % 100 == 0 )
      cout<<"   Analyzed event "<<evt->GetN()<<" / "<<evt->GetEntries()<<endl;
    Global& glb = evt->_glb;
    //if( frunstat[evt->_glb.run] > 0 &&
    //glb.bbcz>-40 && glb.bbcz<40 && glb.trig<22000 ){   // Event Selection....
    //if( glb.bbcz>-40 && glb.bbcz < 40 && glb.bbct0 < 100 && glb.bbct0 > -100 ) {   // Event Selection....
#ifdef BROKEN_DST
    if( glb.zdct0 < 40 && glb.zdct0 > -40 ) {   // The zdct0 is bbct0 !!!!
#else
    if( glb.bbct0 < 40 && glb.bbct0 > -40 ) {
#endif
      vector<Track>& vec_trk = evt->_vec_trk;
      vector<Clust>& vec_clt = evt->_vec_clt;
      vector<Ass>& vec_asstrk = evt->_vec_asstrk;
      vector<Ass>& vec_asstrk_s = evt->_vec_asstrk_s;
      //========================================================
      int nclt = vec_clt.size();
      while( nclt-- ){
	Clust& clt = vec_clt[nclt];
	//-----------------------------------------------------------------
	if( clt.arm==0 ){ // WEST arm
	  ipbscgl = 0;
	  isect = clt.sector;
	  itwr = 2592*isect+72*clt.ind[1]+clt.ind[0];
	  ism = 18*isect + 6 * (int)(clt.ind[1]/12) + (int)(clt.ind[0]/12);
	  itwr_sm = 12 * ( clt.ind[1] % 12 ) + (clt.ind[0] % 12);
	  itwr_sect = 72*clt.ind[1]+clt.ind[0];
	  ism_sect = (int)(clt.ind[1]/12) * 6  + (int)(clt.ind[0]/12);
	  itwr_pbscgl = itwr;
	} else if( clt.arm==1 ) { // EAST arm
	  if( clt.sector == 2 || clt.sector == 3 ){ // PbSc
	    ipbscgl = 0;
	    isect = clt.sector + 2;
	    itwr = 2592*isect+72*clt.ind[1]+clt.ind[0];
	    ism = 18*isect + 6 * (int)(clt.ind[1]/12) + (int)(clt.ind[0]/12);
	    itwr_sm = 12 * ( clt.ind[1] % 12 ) + (clt.ind[0] % 12);
	    itwr_sect = 72*clt.ind[1]+clt.ind[0];
	    ism_sect = (int)(clt.ind[1]/12) * 6  + (int)(clt.ind[0]/12);
	    itwr_pbscgl = itwr;
	  } else if ( clt.sector == 0 || clt.sector == 1 ){ // PbGl
	    ipbscgl = 1;
	    isect = clt.sector + 6;
	    itwr = 15552 + 2592*(isect-6) + 96*clt.ind[1]+clt.ind[0];
	    ism = 108 + 32*(isect-6)+ 8 * (int)(clt.ind[1]/12) + (int)(clt.ind[0]/12);
	    itwr_sm = 12 * ( clt.ind[1] % 12 ) + (clt.ind[0] % 12);
	    itwr_sect = 96*clt.ind[1]+clt.ind[0];
	    ism_sect = (int)(clt.ind[1]/12) * 8  + (int)(clt.ind[0]/12);
	    itwr_pbscgl = itwr - 15552;
	  } else {
	    cout<<" Warning::clt.sector,ind are out of range "<<endl;
	  }
	} else {
	  cout<<" Warning::clt.arm is out of range "<<endl;
	  isect = -1;
	  ipbscgl = -1;
	}
	//-----------------------------------------------------------------
	h_tof->Fill( clt.tofcorr );
	// Cluster Selection
	if( isect >= 0 && isect < 8 && ism >= 0 && ism < 172 && clt.tofcorr < 200 && clt.tofcorr> -200 ){
	  h_itwrsect_sect[isect]->Fill(itwr_sect);
	  if( clt.ecore > 0.05 ){
	    qa_evn_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,1);
	    qa_ene_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.ecent);
	  }
	  if( clt.prob > 0.1 ){
	    tofe_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.ecent,clt.tofcorr);
#ifdef BROKEN_DST
	    h2_tofe->Fill(glb.ecent,clt.tofcorr); // The zdct0 is bbct0 !!!!
	    h2_tofe_sect[isect]->Fill(clt.ecent,clt.tofcorr);
#else
	    h2_tofe->Fill(clt.ecent,clt.tofcorr);
	    h2_tofe_sect[isect]->Fill(clt.ecent,clt.tofcorr);
#endif
	  }
	  if( clt.ecore > 0.1 && clt.prob > 0.1 ){ // It was >0.2GeV
	      //	      tof_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.tofcorr);
#ifdef BROKEN_DST
	    h2_tofbbct0->Fill(glb.zdct0,clt.tofcorr); // The zdct0 is bbct0 !!!!
	    h2_tofbbct0_sect[isect]->Fill(glb.zdct0,clt.tofcorr);
	    tofbbct0_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,glb.zdct0,clt.tofcorr);  // The zdct0 is bbct0 in real meaning!!!
	    //tofnemc_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,glb.bbcz+glb.bbct0,clt.tofcorr); //The bbcz+bbct0 is emcnhit!!!
	    h2_tofnemc_sect[isect]->Fill(glb.bbcz+glb.bbct0,clt.tofcorr);  //The bbcz+bbct0 is emcnhit!!!
#else
	    h2_tofbbct0->Fill(glb.bbct0,clt.tofcorr);
	    h2_tofbbct0_sect[isect]->Fill(glb.bbct0,clt.tofcorr);
	    tofbbct0_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,glb.bbct0,clt.tofcorr);  // The zdct0 is bbct0 in real meaning!!!
	    //tofnemc_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,glb.emcnhit.tofcorr);
	    h2_tofnemc_sect[isect]->Fill(glb.emcnhit,clt.tofcorr);  //The bbcz+bbct0 is emcnhit!!!
#endif
	  }
	  if( clt.ecore > 0.3 && clt.prob > 0.1 )
	    tof_e03_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.tofcorr);
	  if( clt.ecore > 0.5 && clt.prob > 0.1 )
	    tof_e05_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.tofcorr);
#ifdef BROKEN_DST
	  if( clt.ecore > 0.5 && clt.prob > 0.1 && (glb.bbcz+glb.bbct0)<200 )
	    tof_e05lm_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.tofcorr);
#else
	  if( clt.ecore > 0.5 && clt.prob > 0.1 && glb.emcnhit<200 )
	    tof_e05lm_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.tofcorr);
#endif
	  if( clt.ecore > 0.5 && clt.prob > 0.1 )
	    tof_e05sm_sect[isect]->Fill(glb.run,glb.seq,ism_sect,clt.tofcorr);
	  if( second_calib ){
	    tofcorr = -1000;
	    //tofcorr = ( clt.tofcorr + c_tof_sect[isect]->_h_twr_shift->GetBinContent(itwr_sect+1) )
	    //  / c_tofbbct0_sect[isect]->_h_twr_s0.GetBinContent(itwr_sect+1)
	    //  - c_tofe_sect[isect]->_h_twr_s0.GetBinContent(itwr_sect+1) * clt.e ;
#ifdef BROKEN_DST
	    if( f_tofbbct0_sect[isect][itwr_sect] != 0 )
	      tofcorr = ( clt.tofcorr + f_tof_sect[isect][itwr_sect] + glb.zdct0 ) / f_tofbbct0_sect[isect][itwr_sect]
		- glb.zdct0 - f_tofe_sect[isect][itwr_sect] * clt.e;
#else
	    if( f_tofbbct0_sect[isect][itwr_sect] != 0 )
	      tofcorr = ( clt.tofcorr + f_tof_sect[isect][itwr_sect] + glb.bbct0 ) / f_tofbbct0_sect[isect][itwr_sect]
		- glb.bbct0 - f_tofe_sect[isect][itwr_sect] * clt.e;
#endif
	    h_tofcorr->Fill( tofcorr );
	    if( clt.ecore > 0.5 && clt.prob > 0.1 ){
	      tof_2e05_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,tofcorr);
#ifdef BROKEN_DST
	      if( (glb.bbcz+glb.bbct0)<200 )
		tof_2e05lm_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,tofcorr);
#else
	      if( glb.emcnhit <200 )
		tof_2e05lm_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,tofcorr);
#endif
	    }
	  }
	  //	    mip_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.e);
	  if( clt.prob < 0.1 )
	    mip_p01_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.e);
	  //	    if( clt.prob < 0.3 )
	  //	      mip_p03_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.e);
	  //	    mip_sm->Fill(glb.run,glb.seq,ism,clt.e);
	  //	    if( clt.prob < 0.1 )
	  //	      mip_p01_sm->Fill(glb.run,glb.seq,ism,clt.e);
	  //	    if( clt.prob < 0.3 )
	  //	      mip_p03_sm->Fill(glb.run,glb.seq,ism,clt.e);
	}
	//-----------------------------------------------------------------
      }
      //========================================================
    } // End of event selection	  
  } //=== End of loop
  {
    cout<<" ----------------------------------------------------------------- "<<endl;
    cout<<" End of process ... "<<endl;
    cout<<" Processed events "<<evt->GetN()<<" / "<<evt->GetEntries()<<endl;
    TDirectory* current = gDirectory;
    cout<<" Creating root file "<<f_init->GetName()<<" from memory histgram "<<endl;
    f_init->cd();
    isect = 8;
    while( isect-- ){
      cout<<" Writing sector = "<<isect<<endl;
      //mip_sect[isect]->Write(); delete mip_sect[isect];
      mip_p01_sect[isect]->Write(); delete mip_p01_sect[isect];
      //mip_p03_sect[isect]->Write(); delete mip_p03_sect[isect];
      //mip_sm->Write(); delete mip_sm[isect];
      //mip_p01_sm->Write(); delete mip_p01_sm[isect];
      //mip_p03_sm->Write(); delete mip_p03_sm[isect];
      //tof_sect[isect]->Write(); delete tof_sect[isect];
      tof_e03_sect[isect]->Write(); delete tof_e03_sect[isect];
      tof_e05_sect[isect]->Write(); delete tof_e05_sect[isect];
      tof_e05lm_sect[isect]->Write(); delete tof_e05lm_sect[isect];
      tof_e05sm_sect[isect]->Write(); delete tof_e05sm_sect[isect];
      if( second_calib ){
	tof_2e05_sect[isect]->Write(); delete tof_2e05_sect[isect];
	tof_2e05lm_sect[isect]->Write(); delete tof_2e05lm_sect[isect];
      }
      qa_ene_sect[isect]->Write(); delete qa_ene_sect[isect];
      qa_evn_sect[isect]->Write(); delete qa_evn_sect[isect];
      tofbbct0_sect[isect]->Write(); delete tofbbct0_sect[isect];
      tofe_sect[isect]->Write(); delete tofe_sect[isect];
      //tofnemc_sect[isect]->Write(); delete tofnemc_sect[isect];
      h2_tofnemc_sect[isect]->Write(); delete h2_tofnemc_sect[isect];
      h2_tofe_sect[isect]->Write(); delete h2_tofe_sect[isect];
      h2_tofbbct0_sect[isect]->Write(); delete h2_tofbbct0_sect[isect];
      h_itwrsect_sect[isect]->Write(); delete h_itwrsect_sect[isect];
    }
    h_tof->Write(); delete h_tof;
    h_tofcorr->Write(); delete h_tofcorr;
    h2_tofbbct0->Write(); delete h2_tofbbct0;
    h2_tofe->Write(); delete h2_tofe;
    f_init->Close();
    current->cd();
    cout<<" ----------------------------------------------------------------- "<<endl;

  }
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

}
//===============================================================
