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
#include "EmcCalibratorMIP.hh"
#include "EmcCalibratorQArun.hh"
#endif
#include "clust_calib_ana.hh"

void clust_calib_ana(char* filelist,char* out_file,int isectmin = 0,int isectmax = 0,
		     bool update = false, bool debug = false){
#ifndef COMPILE
  gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libclusttr.so");
  gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libcalibset2.so");
#endif

  if( isectmin < 0 || isectmin >= 8 ) isectmin = 0;
  if( isectmax < 0 || isectmax >= 8 || isectmax<isectmin ) isectmax = isectmin;
  char hname[128],htitle[128],fname[128];
  int isect,itwr,ism,nfit;
  bool _stop = false;
  bool _fullana = true;
  bool _mipana = false;

  //================================================================
  // === Initialization....
  //================================================================
  cout<<" clust_calib_ana ..... Start........................................... "<<endl;
  TFile* f_out;
  sprintf(fname,"%s_sect%d.root",out_file,isectmin);
  if( update )
    f_out = new TFile(fname,"UPDATE");
  else
    f_out = new TFile(fname,"RECREATE");
  gStyle->SetOptFit(111111);
  gROOT->cd();

  //================================================================
  // === Read root files
  //================================================================
  f_out->cd();
  CalibRunsTH0* qa_evn_sect[8];
  CalibRunsTH0* qa_ene_sect[8];
  CalibRunsTH0* qa_mene_sect[8];
  CalibRunsTH1* tof_sect[8];
  CalibRunsTH1* tof_e03_sect[8];
  CalibRunsTH1* tof_e05_sect[8];
  CalibRunsTH1* tof_e05lm_sect[8];
  CalibRunsTH1* tof_e05sm_sect[8];
  CalibRunsTH1* mip_p01_sect[8];
  CalibRunsTH2* tofbbct0_sect[8];
  CalibRunsTH2* tofe_sect[8];

  for(isect=isectmin; isect<isectmax+1; isect++ ){
    itwr = (isect < 6 ) ? 2592 : 4608;
    ism = (isect < 6 ) ? 18 : 32;
    f_out->cd();
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
    sprintf(hname,"tofbbct0_sect%d",isect);
    sprintf(htitle,"tof vs bbct0 Calibration sect %d",isect);
    tofbbct0_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,5,-5,5,125,-50,-25);
    sprintf(hname,"tofe_sect%d",isect);
    sprintf(htitle,"tof vs e Calibration sect %d",isect);
    tofe_sect[isect] = new CalibRunsTH2(hname,htitle,itwr,6,0,0.6,125,-50,-25);
    if( _mipana ){
      sprintf(hname,"mip_p01_sect%d",isect);
      sprintf(htitle,"MIP Calibration sect %d",isect);
      mip_p01_sect[isect] = new CalibRunsTH1(hname,htitle,itwr);
    }
  }

  cout<<" open input file list "<<filelist<<endl;
  TObjArray filearray;
  char cline[128];
  ifstream fin(filelist);
  while( fin.getline(cline,128) ){
    cout<<" open input file "<<cline<<endl;
    TFile* tfile = new TFile(cline);
    filearray.Add(tfile);
    for(isect=isectmin; isect<isectmax+1; isect++ ){
      qa_evn_sect[isect]->Read(tfile);
      qa_ene_sect[isect]->Read(tfile);
      //tof_e03_sect[isect]->Read(tfile);
      tof_e05_sect[isect]->Append(tfile);
      tof_e05lm_sect[isect]->Append(tfile);
      tof_e05sm_sect[isect]->Append(tfile);
      //tofbbct0_sect[isect]->Append(tfile);
      tofe_sect[isect]->Append(tfile);
      if( _mipana ) mip_p01_sect[isect]->Read(tfile);
    }
  }

  //================================================================
  // == QA anaysis
  //================================================================
  EmcCalibratorQA* c_tof_qaene_sect[8];
  EmcCalibratorQA* c_tof_qaevn_sect[8];
  EmcCalibratorQA* c_tof_qamene_sect[8];
  EmcCalibratorQA* c_tof_qatwr_sect[8];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    itwr = (isect < 6 ) ? 2592 : 4608;
    gROOT->cd();
    sprintf(hname,"c_tof_qaene_sect%d",isect);
    sprintf(htitle,"QA energy sect %d",isect);
    c_tof_qaene_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr);
    c_tof_qaene_sect[isect]->SetQAbit(QABIT_TOTENE);
    sprintf(hname,"c_tof_qaevn_sect%d",isect);
    sprintf(htitle,"QA event number sect %d",isect);
    c_tof_qaevn_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr);
    c_tof_qaevn_sect[isect]->SetQAbit(QABIT_TOTEVN);
    sprintf(hname,"c_tof_qamene_sect%d",isect);
    sprintf(htitle,"QA mean energy sect %d",isect);
    c_tof_qamene_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr);
    c_tof_qamene_sect[isect]->SetQAbit(QABIT_MEANE);
    sprintf(hname,"c_tof_qatwr_sect%d",isect);
    sprintf(htitle,"QA twrshift sect %d",isect);
    c_tof_qatwr_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr);
    c_tof_qatwr_sect[isect]->SetQAbit(QABIT_TOFT0);
    //
    c_tof_qaene_sect[isect]->AnalyzeCalibRuns(qa_ene_sect[isect],"");
    c_tof_qaevn_sect[isect]->AnalyzeCalibRuns(qa_evn_sect[isect],"");
    TH0Xch tmp_th0xch = c_tof_qaene_sect[isect]->_th0xch / c_tof_qaevn_sect[isect]->_th0xch;
    c_tof_qamene_sect[isect]->_th0xch = tmp_th0xch;
    //
    f_out->cd();
    //
    c_tof_qaevn_sect[isect]->AnalyzeTwr("");
    c_tof_qaevn_sect[isect]->AnalyzeTwr("qa");
    c_tof_qaevn_sect[isect]->AnalyzeTwr("qaf");
    c_tof_qaevn_sect[isect]->Write();
    if( debug ){ c_tof_qaevn_sect[isect]->Draw(); if( _stop ) getchar(); }
    //
    c_tof_qaene_sect[isect]->AnalyzeTwr("");
    c_tof_qaene_sect[isect]->AnalyzeTwr("qa");
    c_tof_qaene_sect[isect]->AnalyzeTwr("qaf");
    c_tof_qaene_sect[isect]->Write();
    if( debug ){ c_tof_qaene_sect[isect]->Draw(); if( _stop ) getchar(); }
    //
    c_tof_qamene_sect[isect]->AnalyzeTwr("");
    c_tof_qamene_sect[isect]->AnalyzeTwr("qa");
    c_tof_qamene_sect[isect]->AnalyzeTwr("qaf");
    c_tof_qamene_sect[isect]->Write();
    if( debug ){ c_tof_qamene_sect[isect]->Draw(); if( _stop ) getchar(); }
    //
    if( c_tof_qaevn_sect[isect]->GetNQAtwr_all() < 1000 )
      c_tof_qatwr_sect[isect]->AddEmcCalibratorQA(*c_tof_qaevn_sect[isect]);
    if( c_tof_qaene_sect[isect]->GetNQAtwr_all() < 1000 )
      c_tof_qatwr_sect[isect]->AddEmcCalibratorQA(*c_tof_qaene_sect[isect]);
    if( c_tof_qamene_sect[isect]->GetNQAtwr_all() < 1000 )
      c_tof_qatwr_sect[isect]->AddEmcCalibratorQA(*c_tof_qamene_sect[isect]);
    cout<<"===================================================================================="<<endl;
    cout<<" Total QA results in "<<c_tof_qatwr_sect[isect]->GetNQAtwr_all()<<" bad channels "<<endl;
    cout<<"===================================================================================="<<endl;
  }

  //================================================================
  // == MIP analysis 
  //================================================================
  if( _mipana ) {
    EmcCalibratorMIP* c_mip_p01_sect[8];
    for(isect=isectmin; isect<isectmax+1; isect++ ){
      gROOT->cd();
      c_mip_p01_sect[isect] = new EmcCalibratorMIP("c_mip_p01_sect0","MIP Calibration prob<0.1 sect 0",2592,100,0,1);
      c_mip_p01_sect[isect]->AnalyzeCalibRuns(mip_p01_sect[isect]);
      c_mip_p01_sect[isect]->SetEmcCalibratorQA(*c_tof_qatwr_sect[isect]);
      c_mip_p01_sect[isect]->AnalyzeTwr("qa");
      f_out->cd();
      c_mip_p01_sect[isect]->Write();
      //    c_mip_p01_sect[isect]->_h_twr_slope->Draw();
    }
  }

  //================================================================
  // == W2 qa
  //================================================================
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    if( isect == 2 ){
      EmcCalibratorQA* c_tof_qatwr_w2;
      c_tof_qatwr_w2 = new EmcCalibratorQA("c_tof_qatwr_w2","QAtwr for W2",2592);
      c_tof_qatwr_w2->SetQAbit(QABIT_TOFT0);
      // This is old directory
      // "/direct/phenix+data24/htorii/Myana_01ana/table/qatwr_W2_TDC.txt";
      char* f_qatwr_w2 = "/afs/rhic/phenix/phnxemc/DATA/pbsc_tofqa_W2.txt";
      c_tof_qatwr_w2->ReadFileQAtwr(f_qatwr_w2);
      cout<<"===================================================================================="<<endl;
      cout<<" Read file : "<<f_qatwr_w2<<endl;
      cout<<"           : QA num = "<<c_tof_qatwr_w2->GetNQAtwr()<<endl;
      cout<<"===================================================================================="<<endl;
      c_tof_qatwr_sect[isect]->AddEmcCalibratorQA(*c_tof_qatwr_w2);
      c_tof_qatwr_w2->Write();
      delete c_tof_qatwr_w2;
    }
  }
  //================================================================
  // == TOF analysis 
  //================================================================

  EmcCalibratorTOF* c_tof_pretwr_sect[8];
  EmcCalibratorTOF* c_tof_twr_sect[8];
  EmcCalibratorQA* c_tof_qapretwr_sect[8];
  EmcCalibratorQArun* c_tof_qarun_sect[8];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    itwr = (isect < 6 ) ? 2592 : 4608;
    gROOT->cd();
    sprintf(hname,"c_tof_pretwr_sect%d",isect);
    sprintf(htitle,"TOF pre-Calibration sect %d",isect);
    c_tof_pretwr_sect[isect] = new EmcCalibratorTOF(hname,htitle,itwr,500,-50,50);
    c_tof_pretwr_sect[isect]->SetQAbit(QABIT_TOFT0);
    sprintf(hname,"c_tof_twr_sect%d",isect);
    sprintf(htitle,"TOF Calibration sect %d",isect);
    c_tof_twr_sect[isect] = new EmcCalibratorTOF(hname,htitle,itwr,500,-50,50);
    c_tof_twr_sect[isect]->SetQAbit(QABIT_TOFT0);
    sprintf(hname,"c_tof_qarun_sect%d",isect);
    sprintf(htitle,"QA runshift sect %d",isect);
    c_tof_qarun_sect[isect] = new EmcCalibratorQArun(hname,htitle);
    c_tof_qarun_sect[isect]->SetQAbit(QABIT_TOFT0);
    sprintf(hname,"c_tof_qapretwr_sect%d",isect);
    sprintf(htitle,"QA pretof twrshift sect %d",isect);
    c_tof_qapretwr_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr);
    c_tof_qapretwr_sect[isect]->SetQAbit(QABIT_TOFT0);
    //
    if( _fullana ){
      //----------- Prefit for QA
      c_tof_pretwr_sect[isect]->AddEmcCalibratorQA(*c_tof_qatwr_sect[isect]);
      c_tof_pretwr_sect[isect]->AnalyzeCalibRuns(tof_e05_sect[isect],"q+d+t");  // q+t is for W2 sector
      c_tof_pretwr_sect[isect]->AnalyzeTwr("q");
      c_tof_twr_sect[isect]->SetPreshift(*c_tof_pretwr_sect[isect]);
      //------------ QA for tower-by-tower-calibration
      TH1F* hshift = c_tof_pretwr_sect[isect]->_h_twr_shift;
      c_tof_qapretwr_sect[isect]->SetCut(-1000,1000);
      c_tof_qapretwr_sect[isect]->Analyze(hshift,"d+q+c");
      c_tof_qapretwr_sect[isect]->Analyze(hshift,"d+q");
      c_tof_qapretwr_sect[isect]->Analyze(hshift,"d+q");
      c_tof_qapretwr_sect[isect]->Analyze(hshift,"d+q+f");
      if( debug ) {c_tof_qapretwr_sect[isect]->Draw(); if( _stop ) getchar(); }
      c_tof_qapretwr_sect[isect]->SetCut(0,0.2);
      c_tof_qapretwr_sect[isect]->Analyze(hshift,"d+q+e+c");
      if( debug ) {c_tof_qapretwr_sect[isect]->Draw(); if( _stop ) getchar(); }
      if( c_tof_qapretwr_sect[isect]->GetNQAtwr_all() < 2000 ){   //................... FIX.ME
	c_tof_qatwr_sect[isect]->AddEmcCalibratorQA(*c_tof_qapretwr_sect[isect]);
      }
      cout<<"===================================================================================="<<endl;
      cout<<" Total QA twr results in "<<c_tof_qatwr_sect[isect]->GetNQAtwr_all()<<" bad channels "<<endl;
      cout<<"===================================================================================="<<endl;
      //------------- QA for run-by-run
      TGraph* rungra = (TGraph*) & (c_tof_pretwr_sect[isect]->_th0xrun_shift._gra);
      c_tof_qarun_sect[isect]->AnalyzeRun(rungra,"d+q");
      c_tof_qarun_sect[isect]->AnalyzeRun(rungra,"d+q");
      c_tof_qarun_sect[isect]->AnalyzeRun(rungra,"d+q");
      c_tof_qarun_sect[isect]->AnalyzeRun(rungra,"d+q");
      //      c_tof_qarun_sect[isect]->SetCut(0,0.2);
      //      c_tof_qarun_sect[isect]->AnalyzeRun(rungra,"d+q+e+c");
      nfit = 4;
      while( nfit-- ){
	c_tof_qarun_sect[isect]->AnalyzeRun(&(c_tof_pretwr_sect[isect]->_th0xrun_fit[nfit]._gra),"d+q");
	c_tof_qarun_sect[isect]->AnalyzeRun(&(c_tof_pretwr_sect[isect]->_th0xrun_fit[nfit]._gra),"d+q+e");
      }
      cout<<"===================================================================================="<<endl;
      cout<<" Total QA run results in "<<c_tof_qarun_sect[isect]->GetNQArun_all()<<" bad runs "<<endl;
      cout<<"===================================================================================="<<endl;
    }
    //-------------- Main tower-by-tower calibration
    c_tof_twr_sect[isect]->AddEmcCalibratorQA(*c_tof_qatwr_sect[isect]);
    c_tof_twr_sect[isect]->AddEmcCalibratorQA(*c_tof_qarun_sect[isect]);
    c_tof_twr_sect[isect]->AnalyzeCalibRuns(tof_e05_sect[isect],"q+t+d");
    c_tof_twr_sect[isect]->AnalyzeTwr("q");
    //
    f_out->cd();
    c_tof_qapretwr_sect[isect]->Write();
    c_tof_qatwr_sect[isect]->Write();
    c_tof_qarun_sect[isect]->Write();
    c_tof_pretwr_sect[isect]->Write();
    c_tof_twr_sect[isect]->Write();
    //---- Draw----------------------------------------
    if( debug ){
      TCanvas* c1 = new TCanvas("c1");
      TH1* h1 = c_tof_twr_sect[isect]->_th1xch->GetHist();
      TH1* h1a = c_tof_twr_sect[isect]->_th1xch_afttwr->GetHist();
      c1->Clear();
      c1->Divide(2);
      c1->cd(1);
      fitpeak(h1,-5,5,2,"tof","",1);
      c1->cd(2);
      fitpeak(h1a,-5,5,2,"tof","",1);
      c1->Update();
      if( _stop ) getchar();
    }
    //---- Draw----------------------------------------
  }

  //================================================================
  // == TOF analysis for low multiplicity conditions
  //================================================================
  EmcCalibratorTOF* c_tof_twrlm_sect[8];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    itwr = (isect < 6 ) ? 2592 : 4608;
    gROOT->cd();
    sprintf(hname,"c_tof_twrlm_sect%d",isect);
    sprintf(htitle,"TOF Calibration for low multi sect %d",isect);
    c_tof_twrlm_sect[isect] = new EmcCalibratorTOF(hname,htitle,itwr,500,-50,50);
    c_tof_twrlm_sect[isect]->SetQAbit(QABIT_TOFT0);
    c_tof_twrlm_sect[isect]->SetEmcCalibratorQA(*c_tof_qarun_sect[isect]);
    c_tof_twrlm_sect[isect]->SetEmcCalibratorTOF(*c_tof_twr_sect[isect]);
    c_tof_twrlm_sect[isect]->AnalyzeCalibRuns(tof_e05lm_sect[isect],"0+q");
    c_tof_twrlm_sect[isect]->AnalyzeTwr("0+q");
    f_out->cd();
    c_tof_twrlm_sect[isect]->Write();
  }


  //================================================================
  // == TOF slewing effect analysis.
  //================================================================

  EmcCalibratorTOFBBC* c_tof_slew_sect[8];
  EmcCalibratorQA* c_tof_qaslewtwr_sect[8];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    itwr = (isect < 6 ) ? 2592 : 4608;
    gROOT->cd();
    sprintf(hname,"c_tof_slew_sect%d",isect);
    sprintf(htitle,"TOF-E LC correction sect %d",isect);
    c_tof_slew_sect[isect] = new EmcCalibratorTOFBBC(hname,htitle,itwr,6,0,0.6,250,-25,25);
    sprintf(hname,"c_tof_qaslewtwr_sect%d",isect);
    sprintf(htitle,"QA twr for tofe sect %d",isect);
    c_tof_qaslewtwr_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr);
    c_tof_qaslewtwr_sect[isect]->SetQAbit(QABIT_TOFSLEW);
    //
    c_tof_slew_sect[isect]->SetEmcCalibratorQA(*c_tof_qatwr_sect[isect]);
    c_tof_slew_sect[isect]->AddEmcCalibratorQA(*c_tof_qarun_sect[isect]);
    c_tof_slew_sect[isect]->SetEmcCalibratorTOF(*c_tof_twr_sect[isect]);
    c_tof_slew_sect[isect]->AnalyzeCalibRuns(tofe_sect[isect],"0+q");  //0+d+q+t
    c_tof_slew_sect[isect]->AnalyzeTwr("s+q"); // s+q+d
    //
    c_tof_qaslewtwr_sect[isect]->SetEmcCalibratorQA(*c_tof_qatwr_sect[isect]);
    c_tof_qaslewtwr_sect[isect]->Analyze(c_tof_twr_sect[isect]->_h_twr_fit[0],"q+d");
    c_tof_qaslewtwr_sect[isect]->Analyze(c_tof_twr_sect[isect]->_h_twr_fit[0],"q+d+e");
    c_tof_qatwr_sect[isect]->AddEmcCalibratorQA(*c_tof_qaslewtwr_sect[isect]);
    //
    f_out->cd();
    c_tof_slew_sect[isect]->Write();
    c_tof_qaslewtwr_sect[isect]->Write();
  }

  //================================================================
  // == Summerize TDC resolution
  // == Write text file..
  //================================================================
  cout<<"###################################################################"<<endl;
  cout<<"### Analysis Summary "<<endl;
  cout<<"###################################################################"<<endl;
  char t0_run_fname[128],t0_twr_fname[128];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    sprintf(t0_run_fname,"%s_t0run_sect%d.txt",out_file,isect);
    sprintf(t0_twr_fname,"%s_t0twr_sect%d.txt",out_file,isect);
    cout<<" Open output T0 calibration for run-by-run : "<<t0_run_fname<<endl;
    cout<<" Open output T0 calibration for twr-by-twr : "<<t0_twr_fname <<endl;
    c_tof_twr_sect[isect]->WriteFile(t0_run_fname,t0_twr_fname);
    //
    sprintf(fname,"%s_qatwr_sect%d.txt",out_file,isect);
    cout<<" Open output T0 QA for tower : "<<fname<<endl;
    c_tof_qatwr_sect[isect]->WriteFileQAtwr(fname);
    //
    sprintf(fname,"%s_qarun_sect%d.txt",out_file,isect);
    cout<<" Open output T0 QA for run : "<<fname<<endl;
    c_tof_qarun_sect[isect]->WriteFileQArun(fname);
    //
    sprintf(fname,"%s_slewtwr_sect%d.txt",out_file,isect);
    cout<<" Open output Slewing calibration for twr-by-twr : "<<fname<<endl;
    c_tof_slew_sect[isect]->WriteFileTwr(fname);
    //
  }
  //
  TH1* h1;
  TF1* fit;
  f_out->cd();
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    h1 = c_tof_twr_sect[isect]->_th1xch_afttwr->_h_all;
    h1->Write("h_tof_result");
    fit = fitpeak(h1,-10,10,2,"_t0","Q0");
    cout<<" TOF resolution is "<<fabs(fit->GetParameter(2))<<" nsec"<<endl;
    cout<<"           Final picture is saved as (TH1F*)h_tof_result in "<<f_out->GetName()<<endl;
    //
    h1 = c_tof_twrlm_sect[isect]->_th1xch_afttwr->_h_all;
    h1->Write("h_toflm_result");
    fit = fitpeak(h1,-10,10,2,"_t0","Q0");
    cout<<" TOF resolution in low multi is "<<fabs(fit->GetParameter(2))<<" nsec"<<endl;
    cout<<"           Final picture is saved as (TH1F*)h_toflm_result in "<<f_out->GetName()<<endl;
    //
    cout<<" QA sick tower = "<<c_tof_qatwr_sect[isect]->GetNQAtwr_all()<<" / "<<c_tof_qatwr_sect[isect]->GetNch()<<endl;
    cout<<" QA sick run  = "<<c_tof_qarun_sect[isect]->GetNQArun_all()<<" / "<<c_tof_qarun_sect[isect]->_th0xrun.GetN()<<endl;
    //
  }
  cout<<"###################################################################"<<endl;
  cout<<"###################################################################"<<endl;
  cout<<"###################################################################"<<endl;

  //================================================================
  // == Close files..
  //================================================================
  f_out->Close();
  TIterator* it = filearray.MakeIterator();
  TFile* ftmp;
  while( ftmp = (TFile*) it->Next() )
    delete ftmp;
  return;

  //================================================================
  // End of this script....
  //================================================================
  //================================================================
  //================================================================


#ifdef SKIPSKIPSKIPDDDDDDBUGGGGGGGGGGGGGGGGGG
  //================================================================
  // == This is for debug purpose.
  //================================================================
  EmcCalibratorTOFBBC* c_tof_slew_sect[8];
  int isect = 0;
  int itwr;
  itwr = (isect < 6 ) ? 2592 : 4608;
  char hname[128],htitle[128];

  gROOT->cd();
  CalibRunsTH2* tofe_sect[8];
  tofe_sect[0] = tofe_sect0;
  TFile* f = new TFile("tofcalib/Step8/clust_calibinit_ana_0.root");
  EmcCalibratorQA* c_tof_qatwr_sect[8];
  c_tof_qatwr_sect[0] = c_tof_qatwr_sect0;
  EmcCalibratorQArun* c_tof_qarun_sect[8];
  c_tof_qarun_sect[0] = c_tof_qarun_sect0;
  EmcCalibratorTOF* c_tof_twr_sect[8];
  c_tof_twr_sect[0] = c_tof_twr_sect0;

#endif

  //================================================================
  // == TOF LC analysis.
  //================================================================

  EmcCalibratorTOFBBC* c_tofbbct0_sect[8];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    itwr = (isect < 6 ) ? 2592 : 4608;
    gROOT->cd();
    sprintf(hname,"c_tofbbct0_sect%d",isect);
    sprintf(htitle,"TOF-BBCt0 slewing correction sect %d",isect);
    c_tofbbct0_sect[isect] = new EmcCalibratorTOFBBC(hname,htitle,itwr,5,-5,5,250,-25,25);
    //
    c_tofbbct0_sect[isect]->SetEmcCalibratorQA(*c_tof_qatwr_sect[isect]);
    c_tofbbct0_sect[isect]->AddEmcCalibratorQA(*c_tof_qarun_sect[isect]);
    c_tofbbct0_sect[isect]->SetEmcCalibratorTOF(*c_tof_twr_sect[isect]);
    c_tofbbct0_sect[isect]->AnalyzeCalibRuns(tofbbct0_sect[isect],"0+d+q");  //0+d+q+t
    c_tofbbct0_sect[isect]->AnalyzeTwr("q"); // q+d
    //
    f_out->cd();
    c_tofbbct0_sect[isect]->Write();
  }


  //================================================================
  // == Close files..
  //================================================================

  for(isect=isectmin; isect<isectmax+1; isect++ ){
    f_out->Close();
  }

  return;


 }
//================================================================
//================================================================
