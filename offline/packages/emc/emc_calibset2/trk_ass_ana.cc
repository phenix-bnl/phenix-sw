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
#include "Hist_mip.hh"
#include "Hist_miptwr.hh"
#include "Hist_miptwr2.hh"
#include "Hist_eqa.hh"
#include "CalibRunsTH0.hh"
#include "CalibRunsTH1.hh"
#include "EmcCalibratorMIP.hh"
#include "EmcCalibratorTOFMIP.hh"
#include "EmcCalibratorQA.hh"
#include "EmcCalibratorQArun.hh"
#endif

#include "trk_ass_ana.hh"


void trk_ass_ana(char* filelist,char* out_file,int isectmin,int isectmax,
		 bool update = false, bool debug = false){
  
#ifndef COMPILE
  gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
  gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");
  //gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
#endif
  //
  char hname[128],htitle[128],name[128];
  char fname[128];
  int isect,ism,ipid;
  int itwr_sect,ism_sect;
  int icanvas_num;
  bool _drawopt = true;
  bool _stop = false;
  gStyle->SetOptFit(111111);

  bool _prefit = true;

  //================================================================
  // === Initialization....
  //================================================================
  cout<<" trk_ass_ana ..... Start........................................... "<<endl;
  TFile* f_out;
  sprintf(fname,"%s_sect%d.root",out_file,isectmin);
  cout<<" Open output root files : "<<fname<<endl;
  if( update )
    f_out = new TFile(fname,"UPDATE");
  else
    f_out = new TFile(fname,"RECREATE");

  f_out->cd();
  Hist_eqa* hist_eqa = new Hist_eqa("hist_eqa","EMC energy QA");
  Hist_eqa* hist_eqa_1 = new Hist_eqa("hist_eqa_1","EMC energy QA");
  Hist_mip* hist_mip    = new Hist_mip("hist_mip","MIP ana");
  Hist_mip* hist_mip_s  = new Hist_mip("hist_mip_s","MIP ana flipped emc");
  Hist_miptwr2* hist_miptwr    = new Hist_miptwr2("hist_miptwr","MIP tower ana");
  //Hist_miptwr2* hist_miptwr_s  = new Hist_miptwr2("hist_miptwr_s","MIP tower ana flipped emc");
  gROOT->cd();

  //  TPostScript* ps;
  //  sprintf(fname,"%s.ps",out_file);
  //  cout<<" Creating Postscript file : "<<fname<<endl;
  //  ps = new TPostScript(fname);
  //  TCanvas* c1 = new TCanvas("c1","Cluster energy",700,900);

  //================================================================
  // === Read root files
  //================================================================
  TObjArray filearray;
  ifstream fin(filelist);
  int nfile = 0;
  int read_num;
  while( fin >> fname){
    cout<<" Read and add : "<<fname<<" :  "<<nfile<<endl;
    nfile++;
    TFile* f = new TFile(fname);
    filearray.Add(f);
    f_out->cd();
    //    hist_mip->Add(f);
    //    hist_mip_s->Add(f);
    hist_miptwr->Add(f);
    //hist_miptwr_s->Add(f);
    //    hist_eqa->Add(f);
    //    hist_eqa_1->Add(f);
  }
  fin.close();

  //================================================================
  // == QA anaysis
  //================================================================
  EmcCalibratorQA* c_qa_ene_sect[8];
  EmcCalibratorQA* c_qa_evn_sect[8];
  EmcCalibratorQA* c_qa_mene_sect[8];
  EmcCalibratorQA* c_qatwr_sect[8];
  EmcCalibratorQArun* c_qarun_sect[8];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    itwr_sect = (isect < 6 ) ? 2592 : 4608;
    f_out->cd();
    //
    sprintf(hname,"c_qa_ene_sect%d",isect);
    sprintf(htitle,"QA energy sect %d",isect);
    c_qa_ene_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr_sect);
    c_qa_ene_sect[isect]->SetQAbit(QABIT_TOTENE);

    sprintf(hname,"c_qa_evn_sect%d",isect);
    sprintf(htitle,"QA event number sect %d",isect);
    c_qa_evn_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr_sect);
    c_qa_evn_sect[isect]->SetQAbit(QABIT_TOTEVN);

    sprintf(hname,"c_qa_mene_sect%d",isect);
    sprintf(htitle,"QA mean energy sect %d",isect);
    c_qa_mene_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr_sect);
    c_qa_mene_sect[isect]->SetQAbit(QABIT_MEANE);

    sprintf(hname,"c_qatwr_sect%d",isect);
    sprintf(htitle,"QA twrshift sect %d",isect);
    c_qatwr_sect[isect] = new EmcCalibratorQA(hname,htitle,itwr_sect);
    c_qatwr_sect[isect]->SetQAbit(QABIT_MIPTWR);
    
    sprintf(hname,"c_qarun_sect%d",isect);
    sprintf(htitle,"QA runshift sect %d",isect);
    c_qarun_sect[isect] = new EmcCalibratorQArun(hname,htitle);
    c_qarun_sect[isect]->SetQAbit(QABIT_MIPTWR);
    //
    if( hist_eqa->GetEntries() > 0  ){
      c_qa_ene_sect[isect]->AnalyzeCalibRuns(hist_eqa->ene_sect[isect],"");
      c_qa_evn_sect[isect]->AnalyzeCalibRuns(hist_eqa->evn_sect[isect],"");
      TH0Xch tmp_th0xch = c_qa_ene_sect[isect]->_th0xch / c_qa_evn_sect[isect]->_th0xch;
      c_qa_mene_sect[isect]->_th0xch = tmp_th0xch;
      //
      f_out->cd();
      //
      c_qa_evn_sect[isect]->AnalyzeTwr("");
      c_qa_evn_sect[isect]->AnalyzeTwr("qa");
      c_qa_evn_sect[isect]->AnalyzeTwr("qaf");
      c_qa_evn_sect[isect]->Write();
      if( debug ){ c_qa_evn_sect[isect]->Draw(); if( _stop ) getchar(); }
      //
      c_qa_ene_sect[isect]->AnalyzeTwr("");
      c_qa_ene_sect[isect]->AnalyzeTwr("qa");
      c_qa_ene_sect[isect]->AnalyzeTwr("qaf");
      c_qa_ene_sect[isect]->Write();
      if( debug ){ c_qa_ene_sect[isect]->Draw(); if( _stop ) getchar(); }
      //
      c_qa_mene_sect[isect]->AnalyzeTwr("");
      c_qa_mene_sect[isect]->AnalyzeTwr("qa");
      c_qa_mene_sect[isect]->AnalyzeTwr("qaf");
      c_qa_mene_sect[isect]->Write();
      if( debug ){ c_qa_mene_sect[isect]->Draw(); if( _stop ) getchar(); }
      //
      if( c_qa_evn_sect[isect]->GetNQAtwr_all() < 1000 )
	c_qatwr_sect[isect]->AddEmcCalibratorQA(*c_qa_evn_sect[isect]);
      if( c_qa_ene_sect[isect]->GetNQAtwr_all() < 1000 )
	c_qatwr_sect[isect]->AddEmcCalibratorQA(*c_qa_ene_sect[isect]);
      if( c_qa_mene_sect[isect]->GetNQAtwr_all() < 1000 )
	c_qatwr_sect[isect]->AddEmcCalibratorQA(*c_qa_mene_sect[isect]);
      cout<<"===================================================================================="<<endl;
      cout<<" Total QA results in "<<c_qatwr_sect[isect]->GetNQAtwr_all()<<" bad channels "<<endl;
      cout<<"===================================================================================="<<endl;
    } else {
      cout<<"===================================================================================="<<endl;
      cout<<" QA for tower by tower is skipped.. "<<endl;
      cout<<"===================================================================================="<<endl;
    }

  }

  //================================================================
  // == MIPSM anaysis
  //================================================================
  EmcCalibratorMIP* c_mip_sm_sect[8];
  EmcCalibratorQA* c_mip_qasm_sect[8];
  EmcCalibratorQArun* c_mip_qasmrun_sect[8];
  if( hist_miptwr->GetEntries() > 0 ){
    for(isect=isectmin; isect<isectmax+1; isect++ ){
      itwr_sect = (isect < 6 ) ? 2592 : 4608;
      ism_sect = (isect < 6 ) ? 18 : 32;
      f_out->cd();
      //
      sprintf(hname,"c_mip_sm_sect%d",isect);
      sprintf(htitle,"MIP calibration in sect %d",isect);
      c_mip_sm_sect[isect] = new EmcCalibratorMIP(hname,htitle,ism_sect,100,0,1);
      //
      sprintf(hname,"c_mip_qasm_sect%d",isect);
      sprintf(htitle,"QA smshift sect %d",isect);
      c_mip_qasm_sect[isect] = new EmcCalibratorQA(hname,htitle,ism_sect);
      c_mip_qasm_sect[isect]->SetQAbit(QABIT_MIPSM);
      
      sprintf(hname,"c_mip_qasmrun_sect%d",isect);
      sprintf(htitle,"QA sm runshift sect %d",isect);
      c_mip_qasmrun_sect[isect] = new EmcCalibratorQArun(hname,htitle);
      c_mip_qasmrun_sect[isect]->SetQAbit(QABIT_MIPSM);
      //
      //------------ Analysis
      c_mip_sm_sect[isect]->AnalyzeCalibRuns(hist_miptwr->h_sm_sect[isect],"q+d");
      c_mip_sm_sect[isect]->AnalyzeTwr("q+d");
      //
      //------------ QA for SM-by-SM-calibration
      TH1* h1 = c_mip_sm_sect[isect]->_th0xch->_h_all;
      c_mip_qasm_sect[isect]->Analyze(h1,"q+d");
      c_mip_qasm_sect[isect]->Analyze(h1,"q+d");
      c_mip_qasm_sect[isect]->Analyze(h1,"q+d");
      cout<<"===================================================================================="<<endl;
      cout<<" Total QA SM results in "<<c_mip_qasm_sect[isect]->GetNQAtwr_all()<<" bad channels "<<endl;
      cout<<"===================================================================================="<<endl;
      
      //------------- QA for run-by-run    
      TGraph* rungra = (TGraph*) & (c_mip_sm_sect[isect]->_th0xrun->_gra);
      c_mip_qasmrun_sect[isect]->SetCut(0.2,0.4);
      c_mip_qasmrun_sect[isect]->AnalyzeRun(rungra,"d+q+c");
      c_mip_qasmrun_sect[isect]->AnalyzeRun(rungra,"d+q");
      c_mip_qasmrun_sect[isect]->AnalyzeRun(rungra,"d+q");
      c_mip_qasmrun_sect[isect]->AnalyzeRun(rungra,"d+q");
      cout<<"===================================================================================="<<endl;
      cout<<" Total QA run results in "<<c_mip_qasmrun_sect[isect]->GetNQArun_all()<<" bad runs "<<endl;
      cout<<"===================================================================================="<<endl;
      //
      f_out->cd();
      c_mip_qasm_sect[isect]->Write();
      c_mip_qasmrun_sect[isect]->Write();
      c_mip_sm_sect[isect]->Write();
    }
  }

  //================================================================
  // == MIPtwr anaysis
  //================================================================
  EmcCalibratorMIP* c_mip_pretwr_sect[8];
  EmcCalibratorMIP* c_mip_twr_sect[8];
  if( hist_miptwr->GetEntries() > 0 ){
    for(isect=isectmin; isect<isectmax+1; isect++ ){
      itwr_sect = (isect < 6 ) ? 2592 : 4608;
      ism_sect = (isect < 6 ) ? 18 : 32;
      f_out->cd();
      //
      sprintf(hname,"c_mip_twr_sect%d",isect);
      sprintf(htitle,"MIP calibration in sect %d",isect);
      c_mip_twr_sect[isect] = new EmcCalibratorMIP(hname,htitle,itwr_sect,100,0,1);
      //
      sprintf(hname,"c_mip_pretwr_sect%d",isect);
      sprintf(htitle,"MIP calibration in sect %d",isect);
      c_mip_pretwr_sect[isect] = new EmcCalibratorMIP(hname,htitle,itwr_sect,100,0,1);
      //
      if( _prefit ){
	//------------ Analysis
	c_mip_pretwr_sect[isect]->AddEmcCalibratorQA(*c_qatwr_sect[isect]);
	c_mip_pretwr_sect[isect]->AddEmcCalibratorQA(*c_qarun_sect[isect]);
	c_mip_pretwr_sect[isect]->AnalyzeCalibRuns(hist_miptwr->h_twr_sect[isect],"q+d");
	c_mip_pretwr_sect[isect]->AnalyzeTwr("q+d");
	
	//------------ QA for tower-by-tower-calibration
	TH1* h1 = c_mip_pretwr_sect[isect]->_th0xch->_h_all;
	c_qatwr_sect[isect]->Analyze(h1,"q+d");
	c_qatwr_sect[isect]->Analyze(h1,"q+d");
	c_qatwr_sect[isect]->Analyze(h1,"q+d");
	cout<<"===================================================================================="<<endl;
	cout<<" Total QA twr results in "<<c_qatwr_sect[isect]->GetNQAtwr_all()<<" bad channels "<<endl;
	cout<<"===================================================================================="<<endl;
	
	//------------- QA for run-by-run    
	TGraph* rungra = (TGraph*) & (c_mip_pretwr_sect[isect]->_th0xrun->_gra);
	c_qarun_sect[isect]->SetCut(0.2,0.4);
	c_qarun_sect[isect]->AnalyzeRun(rungra,"d+q+c");
	c_qarun_sect[isect]->AnalyzeRun(rungra,"d+q");
	c_qarun_sect[isect]->AnalyzeRun(rungra,"d+q");
	c_qarun_sect[isect]->AnalyzeRun(rungra,"d+q");
	cout<<"===================================================================================="<<endl;
	cout<<" Total QA run results in "<<c_qarun_sect[isect]->GetNQArun_all()<<" bad runs "<<endl;
	cout<<"===================================================================================="<<endl;
      }

      //------------- Refitting after QA
      c_mip_twr_sect[isect]->AddEmcCalibratorQA(*c_qatwr_sect[isect]);
      c_mip_twr_sect[isect]->AddEmcCalibratorQA(*c_qarun_sect[isect]);
      c_mip_twr_sect[isect]->AnalyzeCalibRuns(hist_miptwr->h_twr_sect[isect],"q+d+t");
      c_mip_twr_sect[isect]->AnalyzeTwr("q+d");

      //------------- Write to file
      f_out->cd();
      c_mip_pretwr_sect[isect]->Write();
      c_mip_twr_sect[isect]->Write();
    }
  }

  //================================================================
  // == Try to find existing TOF calibration.
  //================================================================
  EmcCalibratorTOF* c_tof_twr_sect[8];
  EmcCalibratorQA* c_tof_qatwr_sect[8];
  EmcCalibratorQArun* c_tof_qarun_sect[8];
  bool c_tof_exist[8];
  isect=8; while(isect--) c_tof_exist[isect] = false;
  //
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    sprintf(hname,"c_tof_twr_sect%d",isect);
    c_tof_twr_sect[isect] = (EmcCalibratorTOF*) f_out->Get(hname);
    //
    sprintf(hname,"c_tof_qatwr_sect%d",isect);
    c_tof_qatwr_sect[isect] = (EmcCalibratorQA*) f_out->Get(hname);
    //
    sprintf(hname,"c_tof_qarun_sect%d",isect);
    c_tof_qarun_sect[isect] = (EmcCalibratorQArun*) f_out->Get(hname);
    //
    c_tof_exist[isect] = false;
    if( c_tof_twr_sect[isect] > 0 &&
	c_tof_qatwr_sect[isect] > 0 && c_tof_qarun_sect[isect] > 0 ){
      c_tof_exist[isect] = true;
    }
    cout<<"===================================================================================="<<endl;
    cout<<" Try to find existing TOF calibration " <<endl;
    if( c_tof_twr_sect[isect] > 0 ) cout<<" Import "<<c_tof_twr_sect[isect]->GetName()<<endl;
    if( c_tof_qatwr_sect[isect] > 0 ) cout<<" Import "<<c_tof_qatwr_sect[isect]->GetName()<<endl;
    if( c_tof_qarun_sect[isect] > 0 ) cout<<" Import "<<c_tof_qarun_sect[isect]->GetName()<<endl;
    cout<<"===================================================================================="<<endl;
    if( c_tof_exist[isect] ){
      cout<<"===================================================================================="<<endl;
      cout<<" Original QA twr was = "<<c_qatwr_sect[isect]->GetNQAtwr_all()<<endl;
      cout<<" Original QA run was = "<<c_qarun_sect[isect]->GetNQArun_all()<<endl;
      c_qatwr_sect[isect]->AddEmcCalibratorQA(*c_tof_qatwr_sect[isect]);
      c_qarun_sect[isect]->AddEmcCalibratorQA(*c_tof_qarun_sect[isect]);
      cout<<" Total QA twr results in "<<c_qatwr_sect[isect]->GetNQAtwr_all()<<" bad channels "<<endl;
      cout<<" Total QA run results in "<<c_qarun_sect[isect]->GetNQArun_all()<<" bad runs "<<endl;
      cout<<"===================================================================================="<<endl;
    }
  }

  //================================================================
  // == MIP twr analysis after PID
  //================================================================
  EmcCalibratorTOFMIP* c_mip_twr_etof0_sect[8];
  EmcCalibratorTOFMIP* c_mip_twr2sm_etof0_sect[8];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    if( c_tof_exist[isect] && hist_miptwr->GetEntries() > 0 && isect < 6 ){
      itwr_sect = (isect < 6 ) ? 2592 : 4608;
      ism_sect = (isect < 6 ) ? 18 : 32;
      f_out->cd();
      //
      sprintf(hname,"c_mip_twr_etof0_sect%d",isect);
      sprintf(htitle,"MIP PID twr calibration in sect %d",isect);
      c_mip_twr_etof0_sect[isect] = new EmcCalibratorTOFMIP(hname,htitle,itwr_sect,40,0.1,0.5,125,-12.5,12.5);
      c_mip_twr_etof0_sect[isect]->Set_PIPID_TOFCut(-5.,2.);
      //
      sprintf(hname,"c_mip_twr2sm_etof0_sect%d",isect);
      sprintf(htitle,"MIP PID calibration in sect %d",isect);
      c_mip_twr2sm_etof0_sect[isect] = new EmcCalibratorTOFMIP(hname,htitle,ism_sect,40,0,5,125,-12.5,12.5);
      c_mip_twr2sm_etof0_sect[isect]->Set_PIPID_TOFCut(-5.,2.);
      //
      //------------ Analysis
      c_mip_twr_etof0_sect[isect]->AddEmcCalibratorQA(*c_qatwr_sect[isect]);
      c_mip_twr_etof0_sect[isect]->AddEmcCalibratorQA(*c_qarun_sect[isect]);
      //
      c_mip_twr_etof0_sect[isect]->SetEmcCalibratorTOF(*c_tof_twr_sect[isect]);
      c_mip_twr_etof0_sect[isect]->AnalyzeCalibRuns(hist_miptwr->h_twr_etof0_sect[isect],"0+t+q");
      c_mip_twr_etof0_sect[isect]->AnalyzeTwr("q");
      //
      //------------ Convert from Tower to SM basis
      c_mip_twr2sm_etof0_sect[isect]->AddEmcCalibratorQA(*c_mip_qasm_sect[isect]);
      c_mip_twr2sm_etof0_sect[isect]->AddEmcCalibratorQA(*c_mip_qasmrun_sect[isect]);
      //
      //TH2Xch& th2xch_sm = *(c_mip_twr2sm_etof0_sect[isect]->_th2xch_afttwr);
      TH2Xch& th2xch_sm = *(c_mip_twr2sm_etof0_sect[isect]->_th2xch);
      TH2Xch& th2xch_twr = *(c_mip_twr_etof0_sect[isect]->_th2xch);
      int ch = th2xch_twr.GetNch();
      int ismx,ismy,ism;
      while( ch-- ){
	ismy = (int)( ch / 864 );
	ismx = ( ch % 12 ) % 6 ;
	ism = ismy * 6 + ismx;
	TH2* h2 = th2xch_twr.CreateHistCh(ch);
	th2xch_sm.AddHistCh(ism,h2);
	delete h2;
      }
      //
      c_mip_twr2sm_etof0_sect[isect]->AnalyzeTwr("q");

      //------------- Write to file
      f_out->cd();
      c_mip_twr_etof0_sect[isect]->Write();
      c_mip_twr2sm_etof0_sect[isect]->Write();
    }
  }

  //================================================================
  // == Summerize MIPtwr analysis
  //================================================================
  cout<<endl;
  cout<<"###################################################################"<<endl;
  cout<<"### Analysis Summary "<<endl;
  cout<<"###################################################################"<<endl;
  char run_fname[128],twr_fname[128];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    if( hist_miptwr->GetEntries() > 0 ){
      sprintf(run_fname,"%s_miprun_sect%d.txt",out_file,isect);
      sprintf(twr_fname,"%s_miptwr_sect%d.txt",out_file,isect);
      cout<<" Open output MIP calibration for run-by-run : "<<run_fname<<endl;
      cout<<" Open output MIP calibration for twr-by-twr : "<<twr_fname <<endl;
      c_mip_twr_sect[isect]->WriteFile(run_fname,twr_fname);
      //
      sprintf(fname,"%s_qatwr_sect%d.txt",out_file,isect);
      cout<<" Open output QA for tower : "<<fname<<endl;
      c_qatwr_sect[isect]->WriteFileQAtwr(fname);
      //
      sprintf(fname,"%s_qarun_sect%d.txt",out_file,isect);
      cout<<" Open output QA for run : "<<fname<<endl;
      c_qarun_sect[isect]->WriteFileQArun(fname);
      //
      if( c_tof_exist[isect] && isect < 6 ){
	//
	sprintf(fname,"%s_miptwr_pid_sect%d.txt",out_file,isect);
	cout<<" Open output MIP PID for run : "<<fname<<endl;
	c_mip_twr_etof0_sect[isect]->WriteFileTwr(fname);
	//
	sprintf(fname,"%s_miptwr_pidsm_sect%d.txt",out_file,isect);
	cout<<" Open output MIP PID SM for run : "<<fname<<endl;
	c_mip_twr2sm_etof0_sect[isect]->WriteFileTwr(fname);
	//
	//c_mip_qasm_sect[isect]->Write();
	//c_mip_qasmrun_sect[isect]->Write();
	sprintf(fname,"%s_miptwr_sm_sect%d.txt",out_file,isect);
	cout<<" Open output MIP SM for run : "<<fname<<endl;
	c_mip_sm_sect[isect]->WriteFileTwr(fname);
      }
    }
    //cout<<" QA sick tower = "<<c_qatwr_sect[isect]->GetNQAtwr_all()<<" / "<<c_qatwr_sect[isect]->GetNch()<<endl;
    //cout<<" QA sick run  = "<<c_qarun_sect[isect]->GetNQArun_all()<<" / "<<c_qarun_sect[isect]->_th0xrun.GetN()<<endl;
  }
  cout<<"###################################################################"<<endl;
  cout<<"###################################################################"<<endl;
  cout<<"###################################################################"<<endl;

  //================================================================
  // == Close files..
  //================================================================
  cout<<" Close output file : "<<f_out->GetName()<<endl;
  f_out->cd();
  hist_eqa->Write(); delete hist_eqa;
  hist_eqa_1->Write(); delete hist_eqa_1;
  hist_mip->Write(); delete hist_mip;
  hist_mip_s->Write(); delete hist_mip_s;
  hist_miptwr->Write();  delete hist_miptwr;
  //hist_miptwr_s->Write();  delete hist_miptwr_s;
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    c_qatwr_sect[isect]->Write(); delete c_qatwr_sect[isect];
    c_qarun_sect[isect]->Write(); delete c_qarun_sect[isect];
    c_qa_ene_sect[isect]->Write(); delete c_qa_ene_sect[isect];
    c_qa_evn_sect[isect]->Write(); delete c_qa_evn_sect[isect];
    c_qa_mene_sect[isect]->Write(); delete c_qa_mene_sect[isect];
    if( hist_miptwr->GetEntries() > 0 ){
      delete c_mip_sm_sect[isect];
      delete c_mip_qasm_sect[isect];
      delete c_mip_qasmrun_sect[isect];
      delete c_mip_pretwr_sect[isect];
      delete c_mip_twr_sect[isect];
      if( c_tof_exist[isect] ){
	delete c_tof_twr_sect[isect];
	delete c_tof_qatwr_sect[isect];
	delete c_tof_qarun_sect[isect];
	delete c_mip_twr_etof0_sect[isect];
	delete c_mip_twr2sm_etof0_sect[isect];
      }
    }
  }
  //
  cout<<" Close ....... "<<f_out->GetName()<<endl;
  f_out->Close();
  //
  cout<<" Close filearray  ....... "<<endl;
  TIterator* it = filearray.MakeIterator();
  TFile* ftmp;
  while( ftmp = (TFile*) it->Next() ){
    cout<<"    .... Close:: "<<ftmp->GetName()<<endl;
    ftmp->Close();
  }

  return;
  //================================================================
  // End of this script....
  //================================================================
  //================================================================
  //================================================================



#ifdef SKIPSKIPSKIPSKIP
  //================================================================
  // == MIP SM analysis after PID
  //================================================================
  EmcCalibratorTOFMIP* c_mip_smetof0_sect[8];
  EmcCalibratorTOFMIP* c_mip_smetof1_sect[8];
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    if( c_tof_exist[isect] && hist_miptwr->GetEntries() > 0 ){
      itwr_sect = (isect < 6 ) ? 2592 : 4608;
      ism_sect = (isect < 6 ) ? 18 : 32;
      f_out->cd();
      //
      sprintf(hname,"c_mip_smetof0_sect%d",isect);
      sprintf(htitle,"MIP PID calibration in sect %d",isect);
      //c_mip_smetof0_sect[isect] = new EmcCalibratorTOFMIP(hname,htitle,ism_sect,100,0,1,125,-12.5,12.5);
      c_mip_smetof0_sect[isect] = new EmcCalibratorTOFMIP(hname,htitle,ism_sect,100,0,1,250,-25,25);
      c_mip_smetof0_sect[isect]->Set_PIPID_TOFCut(-5.,2.);
      //
      sprintf(hname,"c_mip_smetof1_sect%d",isect);
      sprintf(htitle,"MIP PID calibration in sect %d",isect);
      c_mip_smetof1_sect[isect] = new EmcCalibratorTOFMIP(hname,htitle,ism_sect,100,0,1,250,-25,25);
      c_mip_smetof1_sect[isect]->Set_PIPID_TOFCut(-5.,2.);
      //
      //------------ Analysis
      c_mip_smetof0_sect[isect]->AddEmcCalibratorQA(*c_qatwr_sect[isect]);
      c_mip_smetof0_sect[isect]->AddEmcCalibratorQA(*c_qarun_sect[isect]);
      c_mip_smetof1_sect[isect]->AddEmcCalibratorQA(*c_qatwr_sect[isect]);
      c_mip_smetof1_sect[isect]->AddEmcCalibratorQA(*c_qarun_sect[isect]);
      //
      c_mip_smetof0_sect[isect]->SetEmcCalibratorTOFrun(*c_tof_twr_sect[isect]);
      c_mip_smetof0_sect[isect]->AnalyzeCalibRuns(hist_miptwr->h_sm_etof0_sect[isect],"0+q"); // "t" opiton
      c_mip_smetof0_sect[isect]->AnalyzeTwr("q");

      //------------- Write to file
      f_out->cd();
      c_mip_smetof0_sect[isect]->Write();
      c_mip_smetof1_sect[isect]->Write();
      delete c_mip_smetof0_sect[isect];
      delete c_mip_smetof1_sect[isect];
    }
  }
#endif

#ifdef SKIPSKIP_APPEND
  //================================================================
  // === Append swapped hists
  //================================================================
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    hist_miptwr->h_twr_sect[isect]->SetOptDebug();
    hist_miptwr->h_twr_sect[isect]->Append(hist_miptwr_s->h_twr_sect[isect],-1.0);
    hist_miptwr->h_sm_sect[isect]->SetOptDebug();
    hist_miptwr->h_sm_sect[isect]->Append(hist_miptwr_s->h_sm_sect[isect],-1.0);
    //hist_miptwr->h_smpid_sect[isect]->SetOptDebug();
    //hist_miptwr->h_smpid_sect[isect]->Append(hist_miptwr_s->h_smpid_sect[isect],-1.0);
    hist_miptwr->h_sm_etof0_sect[isect]->SetOptDebug();
    hist_miptwr->h_sm_etof0_sect[isect]->Append(hist_miptwr_s->h_sm_etof0_sect[isect],-1.0);
    hist_miptwr->h_sm_etof1_sect[isect]->SetOptDebug();
    hist_miptwr->h_sm_etof1_sect[isect]->Append(hist_miptwr_s->h_sm_etof1_sect[isect],-1.0);
  }
#endif

#ifdef SKIPSKIPSKIP
  //================================================================
  // == MIP anaysis for sector
  //================================================================
  TF1* tf_e[8];
  c1->Divide(3,3);
  for(isect=isectmin; isect<isectmax+1; isect++ ){
    //=================================================================== MIP monitor
    c1->cd(1);
    TH1* h_e = (TH1*) hist_mip->h_e_sect[isect]->Clone();
    TH1* h_e_s = (TH1*) hist_mip_s->h_e_sect[isect]->Clone();
    h_e->DrawCopy();
    h_e_s->SetLineColor(4);
    h_e_s->SetLineStyle(2);
    h_e_s->DrawCopy("same");
    h_e->Add(h_e_s,-1);
    h_e->SetLineColor(2);
    h_e->DrawCopy("same");
    tf_e[isect] = fitpeak(h_e,0.2,0.4,1.0);
    delete h_e;
    delete h_e_s;
    //=================================================================== MIP vs mom monitor
    c1->cd(2);
    TH2* h2_e = (TH2*)hist_mip->h2_eangmom_sect[isect]->Clone();
    TH2* h2_e_s = (TH2*)hist_mip_s->h2_eangmom_sect[isect]->Clone();
    //    hist_mip->h2_eangmom_sect[isect]->Draw();
    //    hist_mip_s->h2_eangmom_sect[isect]->Draw("same");
    h2_e->Add(h2_e_s,-1);
    h2_e->DrawCopy("colz");
    delete h2_e;
    delete h2_e_s;
    //
    //=================================================================== MIP vs mom / pid for PbSc
    c1->cd(3);
    int ipid = 8;
    h2_e = (TH2*)hist_mip->h2_eangmom_sectpid[isect][ipid]->Clone();
    h2_e_s = (TH2*)hist_mip_s->h2_eangmom_sectpid[isect][ipid]->Clone();
    h2_e->Add(h2_e_s,-1);
    h2_e->DrawCopy("colz");
    delete h2_e;
    delete h2_e_s;
  }
#endif


};
//
