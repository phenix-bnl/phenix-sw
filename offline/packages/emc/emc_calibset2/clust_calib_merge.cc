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
#include <TObjArray.h>
#include <fstream.h>
#include <vector>
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
#include "clust_calib_merge.hh"

void clust_calib_merge(char* filelist,char* writefile="merge_all.root",int start = 0 ,int stop = 0,
		       bool update = false, bool debug = false){

  //int isectmin = 0,int isectmax = 0,bool _debug = false){
  bool _stop = false;
  bool _fullana = true;
  //================================================================
  // === Create Objects
  //================================================================
  gROOT->cd();
  TFile* f_out;
  if( update )
    f_out = new TFile(writefile,"UPDATE");
  else
    f_out = new TFile(writefile,"RECREATE");
  f_out->cd();
  CalibRunsTH0* qa_evn_sect[8];
  CalibRunsTH0* qa_ene_sect[8];
  CalibRunsTH0* qa_mene_sect[8];
  CalibRunsTH1* tof_sect[8];
  CalibRunsTH1* tof_e03_sect[8];
  CalibRunsTH1* tof_e05_sect[8];
  CalibRunsTH1* tof_e05lm_sect[8];
  CalibRunsTH1* tof_e05sm_sect[8];
  CalibRunsTH1* tof_2e05_sect[8];
  CalibRunsTH1* tof_2e05lm_sect[8];
  CalibRunsTH1* mip_sect[8];
  CalibRunsTH1* mip_p01_sect[8];
  CalibRunsTH2* tofbbct0_sect[8];
  CalibRunsTH2* tofe_sect[8];

  char hname[128],htitle[128];
  int isect,itwr,ism,nfit;
  isect = 8;
  while( isect-- ){
    itwr = (isect < 6 ) ? 2592 : 4608;
    ism = (isect < 6 ) ? 18 : 32;
    gROOT->cd();
    sprintf(hname,"qa_ene_sect%d",isect);
    sprintf(htitle,"QA energy sect %d",isect);
    qa_ene_sect[isect] = new CalibRunsTH0(hname,htitle,itwr);
    sprintf(hname,"qa_evn_sect%d",isect);
    sprintf(htitle,"QA event number sect %d",isect);
    qa_evn_sect[isect] = new CalibRunsTH0(hname,htitle,itwr);
    sprintf(hname,"qa_mene_sect%d",isect);
    sprintf(htitle,"QA mean energy sect %d",isect);
    qa_mene_sect[isect] = new CalibRunsTH0(hname,htitle,itwr);
    sprintf(hname,"tof_sect%d",isect);
    sprintf(htitle,"tof Calibration sect %d",isect);
    tof_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    sprintf(hname,"tof_e03_sect%d",isect);
    sprintf(htitle,"tof e>0.3GeV Calibration sect %d",isect);
    tof_e03_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    sprintf(hname,"tof_e05_sect%d",isect);
    sprintf(htitle,"tof e>0.5GeV Calibration sect %d",isect);
    tof_e05_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    sprintf(hname,"tof_e05lm_sect%d",isect);
    sprintf(htitle,"tof e>0.5GeV low multiplicity Calibration sect %d",isect);
    tof_e05lm_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    sprintf(hname,"tof_e05sm_sect%d",isect);
    sprintf(htitle,"tof e>0.5GeV SM Calibration sect %d",isect);
    tof_e05sm_sect[isect] = new CalibRunsTH1(hname,htitle,ism);
    sprintf(hname,"tof_2e05_sect%d",isect);
    sprintf(htitle,"tof e>0.5GeV 2nd Calibration sect %d",isect);
    tof_2e05_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    sprintf(hname,"tof_2e05lm_sect%d",isect);
    sprintf(htitle,"tof e>0.5GeV low multiplicity 2nd Calibration sect %d",isect);
    tof_2e05lm_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    sprintf(hname,"mip_sect%d",isect);
    sprintf(htitle,"MIP Calibration sect %d",isect);
    mip_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    sprintf(hname,"mip_p01_sect%d",isect);
    sprintf(htitle,"MIP Calibration sect %d",isect);
    mip_p01_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    sprintf(hname,"tofbbct0_sect%d",isect);
    sprintf(htitle,"tof vs bbct0 Calibration sect %d",isect);
    tofbbct0_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,5,-5,5,125,-50,-25);
    sprintf(hname,"tofe_sect%d",isect);
    sprintf(htitle,"tof vs e Calibration sect %d",isect);
    tofe_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,6,0,0.6,125,-50,-25);
  }
  //================================================================
  // === Read one by one from filelist
  //================================================================
  char fname[128];
  //
  int tot_nfile = 0;
  ifstream ftmp(filelist);
  while( ftmp >> fname) tot_nfile++;
  ftmp.close();
  //
  cout<<" ================================================================================= "<<endl;
  TObjArray filearray;
  TFile* tfile;
  int nfile = 0;
  ifstream fin(filelist);
  while( fin >> fname){
    ++nfile;
    if( ( nfile >= start && nfile <= stop ) || stop == 0){
      cout<<" ================================================================================= "<<endl;
      cout<<" Read and add : "<<fname<<" :  "<<nfile<<" / "<<tot_nfile<<endl;
      tfile = new TFile(fname);
      filearray.Add(tfile);
      isect = 8;
      while(isect--){
	qa_evn_sect[isect]->Read(tfile);
	qa_ene_sect[isect]->Read(tfile);
	//	tof_sect[isect]->Read(tfile);
	tof_e03_sect[isect]->Read(tfile);
	tof_e05_sect[isect]->Read(tfile);
	tof_e05lm_sect[isect]->Read(tfile);
	tof_e05sm_sect[isect]->Read(tfile);
	//	tof_2e05_sect[isect]->Read(tfile);
	//	tof_2e05lm_sect[isect]->Read(tfile);
	//	mip_sect[isect]->Read(tfile);
	mip_p01_sect[isect]->Read(tfile);
	tofbbct0_sect[isect]->Read(tfile);
	tofe_sect[isect]->Read(tfile);
      }
      //---------------------------------------------------------------------------
    }
  }
  cout<<" ================================================================================= "<<endl;
  f_out->cd();
  isect = 8;
  while( isect-- ){
    cout<<" Writing sector = "<<isect<<endl;
    //mip_sect[isect]->Write(); delete mip_sect[isect];
    mip_p01_sect[isect]->Write();
    delete mip_p01_sect[isect];
    //mip_p03_sect[isect]->Write(); delete mip_p03_sect[isect];
    //mip_sm->Write(); delete mip_sm[isect];
    //mip_p01_sm->Write(); delete mip_p01_sm[isect];
    //mip_p03_sm->Write(); delete mip_p03_sm[isect];
    //tof_sect[isect]->Write(); delete tof_sect[isect];
    tof_e03_sect[isect]->Write(); delete tof_e03_sect[isect];
    tof_e05_sect[isect]->Write(); delete tof_e05_sect[isect];
    tof_e05lm_sect[isect]->Write(); delete tof_e05lm_sect[isect];
    tof_e05sm_sect[isect]->Write(); delete tof_e05sm_sect[isect];
    //    if( second_calib ){
    //      tof_2e05_sect[isect]->Write(); delete tof_2e05_sect[isect];
    //      tof_2e05lm_sect[isect]->Write(); delete tof_2e05lm_sect[isect];
    //    }
    qa_ene_sect[isect]->Write(); delete qa_ene_sect[isect];
    qa_evn_sect[isect]->Write(); delete qa_evn_sect[isect];
    tofbbct0_sect[isect]->Write(); delete tofbbct0_sect[isect];
    tofe_sect[isect]->Write(); delete tofe_sect[isect];
    //tofnemc_sect[isect]->Write(); delete tofnemc_sect[isect];
    //    h2_tofnemc_sect[isect]->Write(); delete h2_tofnemc_sect[isect];
    //    h2_tofe_sect[isect]->Write(); delete h2_tofe_sect[isect];
    //    h2_tofbbct0_sect[isect]->Write(); delete h2_tofbbct0_sect[isect];
    //    h_itwrsect_sect[isect]->Write(); delete h_itwrsect_sect[isect];
    }
  //  h_tof->Write(); delete h_tof;
  //  h_tofcorr->Write(); delete h_tofcorr;
  //  h2_tofbbct0->Write(); delete h2_tofbbct0;
  //  h2_tofe->Write(); delete h2_tofe;
  f_out->Close();
  TIterator* itnext = filearray.MakeIterator();
  while( tfile = (TFile*) itnext->Next() ){
    tfile->Close();
    delete tfile;
  }
  cout<<" ================================================================================= "<<endl;

  return;


 }
//================================================================
//================================================================
