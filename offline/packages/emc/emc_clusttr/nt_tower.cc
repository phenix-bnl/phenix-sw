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
#endif

#include "nt_tower.hh"


void nt_tower(){

  gStyle->SetOptStat(1);
  gStyle->SetOptFit(1);
  int canvas_num =0 ;
  int iarm,isect,ismz,ismy,ism,ir,iz,iy,ipbscgl;
  int itwr;
  int ibin,irun;
  int create_mode;
  char hname[64],htitle[64];
  int current_run = 0 ;
  Int_t nentries;
  float ftmp;

  // All-of-Run
  TH1F* h_eall_twr;
  TH1F* h_evn_twr;
  TH1F* h_meane_twr;
  TH1F* h_eall;
  TH1F* h_evn;
  TH1F* h_meane;
  TH1F* h_eall_pbscgl[2];
  TH1F* h_evn_pbscgl[2];
  TH1F* h_meane_pbscgl[2];
  // Run-by-Run
  TH1F* h_eall_run;
  TH1F* h_evn_run;
  TH1F* h_meane_run;
  TH1F* h_eall_twr_run;
  TH1F* h_evn_twr_run;
  TH1F* h_meane_twr_run;
  // Run-ALL
  TH1F* h_eall_runall;
  TH1F* h_evn_runall;
  TH1F* h_meane_runall;

  // QA
  TH1F* h_qa_runall;
  int qa_run[15000];
  int num_qa_run = 15000;
  TH1F* h_qa_twrall;
  int qa_twr[24768];
  int num_qa_twr = 24768;

  //
  TTree *nt_tower;
  TFile* f_init;
  TFile* f_run;
  TFile* f_runqa;
  if( gROOT->FindObject("nt_tower") == 0 ) {
    cout<<" Reading mode!!!! open the file clust_scaninit.root "<<endl;
    f_init = new TFile("nt_towerinit.root");
    f_run = new TFile("nt_towerrun.root");
  }else {
    cout<<" Creating mode!!!"<<endl;
    create_mode =1;
  }
  //------------------------------------------------------- Finding Quality -----
  cout<<" Finding quality file in nt_towerqa.root..."<<endl;
  nentries = 0;
  irun = num_qa_run;
  while( irun-- )
    qa_run[irun] = 0;
  f_runqa = new TFile("nt_towerqa.root");
  h_qa_runall = (TH1F*) f_runqa->Get("h_qa_runall");
  h_qa_twrall = (TH1F*) f_runqa->Get("h_qa_twrall");
  if( h_qa_runall == 0 ){
    cout<<"   Set the quality for all run to O.K. "<<endl;
    irun = num_qa_run;
    while( irun-- )
      qa_run[irun] = 1; // All QA is set to O.K.
  } else {
    cout<<"   Found the quality object.... "<<endl;
    irun = num_qa_run;
    while( irun-- ){
      ibin = h_qa_runall->FindBin(irun);
      if( h_qa_runall->GetBinContent(ibin) == 1 ){
	qa_run[irun] = 1;
	nentries++;
      }
    }
  }
  cout<<"   Good quality run # "<<nentries<<endl;
  //
  nentries = 0;
  itwr = num_qa_twr;
  while( itwr-- )
    qa_twr[itwr] = 1; // All QA is set to O.K.
  if( h_qa_twrall == 0 ){
    cout<<"   Set the quality for all twr to O.K. "<<endl;
  } else {
    cout<<"   Found the quality object for twr.... "<<endl;
    itwr = num_qa_twr;
    while( itwr-- ){
      ibin = h_qa_twrall->FindBin(itwr);
      if( h_qa_twrall->GetBinContent(ibin) == 1 ){
	qa_twr[itwr] = 0;
	nentries++;
      }
    }
  }
  cout<<"   Bad quality tower # "<<nentries<<endl;
  
  //------------------------------------------------------- Create Mode. -----
  if( create_mode == 1 ){

    nt_tower = (TTree*)gROOT->FindObject("nt_tower");
    if( nt_tower == 0 ){
      cerr<<" Error:: Can't fetch nt_tower..."<<endl;
      exit(0);
    }

    cout<<" Creating nt_towerrun.root for run-by-run info "<<endl;
    f_run = new TFile("nt_towerrun.root","RECREATE");

    gROOT->cd();

    //Declaration of leaves types
    Float_t         run;
    Float_t         seq;
    Float_t         evt;
    Float_t         swkey;
    Float_t         ecal;
    Float_t         tof;
    
    //Set branch addresses
    nt_tower->SetBranchAddress("run",&run);
    nt_tower->SetBranchAddress("seq",&seq);
    nt_tower->SetBranchAddress("evt",&evt);
    nt_tower->SetBranchAddress("swkey",&swkey);
    nt_tower->SetBranchAddress("ecal",&ecal);
    nt_tower->SetBranchAddress("tof",&tof);

    //------------------- Histogram factory
    isect = 8;
    while( isect-- ){
      if( isect == 0 || isect == 1 || isect == 7 ){
	iz = (isect>=6 ? 96 : 72 );
	while( iz-- ){
	  iy = (isect>=6 ? 48 : 36 );
	  while( iy-- ){
	    itwr = (isect>=6? 15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz);
	  }
	}
      }
    }

    float f_ecall[24768];
    int i_evn[24768];
    float f_meane[24768];

    h_eall_twr = new TH1F("h_eall_twr","Total deposited energy",24768,0,24768);
    h_evn_twr = new TH1F("h_evn_twr","Total Event number",24768,0,24768);
    h_meane_twr = new TH1F("h_meane_twr","Mean deposited energy",24768,0,24768);
    h_eall = new TH1F("h_eall","Total deposited energy",1000,0,5000);
    h_evn = new TH1F("h_evn","Total Event number",1000,0,50000);
    h_meane = new TH1F("h_meane","Mean deposited energy",200,0,2.);
    ipbscgl = 2;
    while( ipbscgl-- ){
      sprintf(hname,"h_eall_pbscgl%d",ipbscgl);
      h_eall_pbscgl[ipbscgl] = new TH1F(hname,"Total deposited energy",1000,0,5000);
      sprintf(hname,"h_evn_pbscgl%d",ipbscgl);
      h_evn_pbscgl[ipbscgl] = new TH1F(hname,"Total Event number",1000,0,50000);
      sprintf(hname,"h_meane_pbscgl%d",ipbscgl);
      h_meane_pbscgl[ipbscgl] = new TH1F(hname,"Mean deposited energy",200,0,2.);
    }

    h_eall_runall = new TH1F("h_eall_runall","Total deposited energy",
			     10000,5000,15000);
    h_evn_runall = new TH1F("h_evn_runall","Total Event number",
			    10000,5000,15000);
    h_meane_runall = new TH1F("h_meane_runall","Mean deposited energy",
			      10000,5000,15000);

    //--------------------------------------------------------- looping
    nentries = (int) nt_tower->GetEntries();
    cout<<" nentries = "<<nentries<<endl;
    for (Int_t i=0; i<nentries;i++) {
      nt_tower->GetEntry(i);
      if( i%100000 == 0 )cout<<" Event: "<<i<<"/"<<nentries<<endl;
      irun = (int) run;
      if( qa_run[irun] >0 ){
	//------------------------------------------------------- Finding new run -----
	if( run != current_run ){
	  cerr<<" Found new run  "<<current_run<<" --> "<<run<<endl;
	  f_run->cd();
	  // Update the information
	  if( h_eall_run != 0 ){
	    h_eall_run->Reset();
	    h_evn_run->Reset();
	    h_meane_run->Reset();
	    itwr = 24768;
	    while( itwr-- ){
	      ftmp = 0;
	      if( h_evn_twr_run->GetBinContent(itwr+1) > 0 ){
		ftmp = h_eall_twr_run->GetBinContent(itwr+1)
		  / h_evn_twr_run->GetBinContent(itwr+1);
		h_meane_twr_run->SetBinContent(itwr+1,ftmp);
	      }
	      h_eall_run->Fill( h_eall_twr_run->GetBinContent(itwr+1) );
	      h_evn_run->Fill( h_evn_twr_run->GetBinContent(itwr+1) );
	      h_meane_run->Fill( ftmp );
	    }
	  }
	  ftmp = 0;
	  ibin = h_evn_runall->FindBin( current_run );
	  if( h_evn_runall->GetBinContent(ibin) > 0 ){
	    ftmp = h_eall_runall->GetBinContent(ibin)
	      / h_evn_runall->GetBinContent(ibin);
	  }
	  h_meane_runall->SetBinContent(ibin,ftmp);
	  // -------- Create the run histgrams.
	  current_run = (int) run;
	  cout<<" run = "<<run<<endl;
	  cout<<" current_run = "<<current_run<<endl;
	  sprintf(hname,"h_eall_run%d",current_run);
	  h_eall_run = (TH1F*)gROOT->FindObject(hname);	
	  if( h_eall_run == 0 ){
	    cout<<"               :: Creating all hists for run "<<run<<endl;
	    sprintf(hname,"h_eall_run%d",current_run);
	    sprintf(htitle,"Total deposited energy run %d",current_run);
	    h_eall_run = new TH1F(hname,htitle,1000,0,100);
	    sprintf(hname,"h_evn_run%d",current_run);
	    sprintf(htitle,"Total Event number run %d",current_run);
	    h_evn_run = new TH1F(hname,htitle,1000,0,1000);
	    sprintf(hname,"h_meane_run%d",current_run);
	    sprintf(htitle,"Mean deposited energy run %d",current_run);
	    h_meane_run = new TH1F(hname,htitle,200,0,2.);
	    //
	    sprintf(hname,"h_eall_twr_run%d",current_run);
	    sprintf(htitle,"Total deposited energy run %d",current_run);
	    h_eall_twr_run = new TH1F(hname,htitle,24768,0,24768);
	    sprintf(hname,"h_evn_twr_run%d",current_run);
	    sprintf(htitle,"Total Event number run %d",current_run);
	    h_evn_twr_run = new TH1F(hname,htitle,24768,0,24768);
	    sprintf(hname,"h_meane_twr_run%d",current_run);
	    sprintf(htitle,"Mean deposited energy run %d",current_run);
	    h_meane_twr_run = new TH1F(hname,htitle,24768,0,24768);
	  } else {
	    cout<<"               :: Fetching all existing hists for run "<<run<<endl;
	    sprintf(hname,"h_evn_run%d",current_run);
	    h_evn_run = (TH1F*)gROOT->FindObject(hname);
	    sprintf(hname,"h_meane_run%d",current_run);
	    h_meane_run = (TH1F*)gROOT->FindObject(hname);
	    sprintf(hname,"h_eall_twr_run%d",current_run);
	    h_eall_twr_run = (TH1F*)gROOT->FindObject(hname);
	    sprintf(hname,"h_evn_twr_run%d",current_run);
	    h_evn_twr_run = (TH1F*)gROOT->FindObject(hname);
	    sprintf(hname,"h_meane_twr_run%d",current_run);
	    h_meane_twr_run = (TH1F*)gROOT->FindObject(hname);
	  }
	  gROOT->cd();
	}
	//------------------------------------------------------- Filling-----
	if(ecal>0.1){
	  if( swkey>0 ){
	    iarm = (int)(swkey / 100000);
	    isect = (int)((swkey-100000*iarm)/10000);
	    iy = (int)((swkey-100000*iarm-10000*isect)/100);
	    iz = (int)( swkey-100000*iarm-10000*isect-100*iy);
	    isect = isect + iarm*6;
	    itwr = (isect>=6 ? 15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz);
	    if( qa_twr[itwr] == 1 ){
	      h_eall_twr->Fill(itwr,ecal);
	      h_evn_twr->Fill(itwr);
	      h_eall_twr_run->Fill(itwr,ecal);
	      h_evn_twr_run->Fill(itwr);
	      h_eall_runall->Fill(run,ecal);
	      h_evn_runall->Fill(run);
	    }
	    //
	  }
	}
      }
    }
    // --------------------------------------------------------- End of looping
    //
    ftmp = 0;
    ibin = h_evn_runall->FindBin( current_run );
    if( h_evn_runall->GetBinContent(ibin) > 0 ){
      ftmp = h_eall_runall->GetBinContent(ibin)
	/ h_evn_runall->GetBinContent(ibin);
    }
    h_meane_runall->SetBinContent(ibin,ftmp);
    //
    itwr = 24768;
    while( itwr-- ){
      ftmp = 0;
      if( h_evn_twr->GetBinContent(itwr+1) > 0 ){
    	ftmp = h_eall_twr->GetBinContent(itwr+1)
	  / h_evn_twr->GetBinContent(itwr+1);
	h_meane_twr->SetBinContent(itwr+1,ftmp);
      }
      h_eall->Fill( h_eall_twr->GetBinContent(itwr+1) );
      h_evn->Fill( h_evn_twr->GetBinContent(itwr+1) );
      h_meane->Fill( ftmp );
      if( itwr < 5184 ){
	ipbscgl = 0;
	h_eall_pbscgl[ipbscgl]->Fill( h_eall_twr->GetBinContent(itwr+1) );
	h_evn_pbscgl[ipbscgl]->Fill( h_evn_twr->GetBinContent(itwr+1) );
	h_meane_pbscgl[ipbscgl]->Fill( ftmp );
      } else if (itwr > 20159 ){
	ipbscgl = 1;
	h_eall_pbscgl[ipbscgl]->Fill( h_eall_twr->GetBinContent(itwr+1) );
	h_evn_pbscgl[ipbscgl]->Fill( h_evn_twr->GetBinContent(itwr+1) );
	h_meane_pbscgl[ipbscgl]->Fill( ftmp );
      }
    }
    //
    {
      TDirectory* current = gDirectory;
      cout<<" Creating root file nt_towerinit.root"<<endl;
      f_init = new TFile("nt_towerinit.root","RECREATE");
      gROOT->GetList()->Write();
      f_init->Close();
      current->cd();

      cout<<" Closing root file nt_towerrun.root"<<endl;
      f_run->Write();
      f_run->Close();
      gROOT->cd();
    }
  } else { // Reading Mode
    f_init->cd();
    h_eall_runall = (TH1F*) f_init->Get("h_eall_runall");
    h_evn_runall = (TH1F*) f_init->Get("h_evn_runall");
    h_meane_runall = (TH1F*) f_init->Get("h_meane_runall");
    h_eall_twr =  (TH1F*) f_init->Get("h_eall_twr");
    h_evn_twr =  (TH1F*) f_init->Get("h_evn_twr");
    h_meane_twr =  (TH1F*) f_init->Get("h_meane_twr");
    ipbscgl = 2;
    while( ipbscgl-- ){
      sprintf(hname,"h_eall_pbscgl%d",ipbscgl);
      h_eall_pbscgl[ipbscgl] = (TH1F*)f_init->Get(hname);
      sprintf(hname,"h_evn_pbscgl%d",ipbscgl);
      h_evn_pbscgl[ipbscgl] = (TH1F*)f_init->Get(hname);
      sprintf(hname,"h_meane_pbscgl%d",ipbscgl);
      h_meane_pbscgl[ipbscgl] = (TH1F*)f_init->Get(hname);
    }
  }

  gROOT->cd();
  TCanvas* c1 = new TCanvas("c1","Tower analysisn");

  cout<<" Run dependence analysis.... "<<endl;
  gROOT->cd();
  TH1F* h_meane_runeach = new TH1F("h_meane_runeach","Mean deposited energy",
				   100,0.1,0.5);
  TH1F* h_evn_runeach = new TH1F("h_evn_runeach","Total Event number",1000,0,10000);
  h_qa_runall = new TH1F("h_qa_runall","QA for run",10000,5000,15000);
  
  f_init->cd();
  int i = 10000;
  while( i-- ){
    if( h_evn_runall->GetBinContent(i+1) > 100 ){
      if( h_meane_runall->GetBinContent(i+1)>0 )
	h_meane_runeach->Fill(h_meane_runall->GetBinContent(i+1));
      if( h_evn_runall->GetBinContent(i+1)>0 )
	h_evn_runeach->Fill(h_evn_runall->GetBinContent(i+1));
    }
    if( h_evn_runall->GetBinContent(i+1) > 100 &&
	h_meane_runall->GetBinContent(i+1) > 0.1 &&
	h_meane_runall->GetBinContent(i+1) < 0.25 ){ // 203MeV +- 4.7MeV... 5sigma
      irun = (int) h_meane_runall->GetBinLowEdge(i+1);
      ibin = h_qa_runall->FindBin(irun);
      h_qa_runall->SetBinContent(ibin,1);
    }
	
  }
  TH2F* h2_frame = new TH2F("h2_frame","Mean deposited energy",
			    100,7000,13000,100,0.05,50);

  c1->Clear();
  c1->Divide(2,2);
  c1->cd(1);
  gPad->SetLogy();
  h2_frame->Draw();
  h_meane_runall->Draw("same");
  c1->cd(2);
  gPad->SetLogy();
  h_meane_runeach->Fit("gaus");
  h_meane_runeach->Draw();
  c1->cd(3);
  gPad->SetLogy();
  h_evn_runall->GetXaxis()->SetRange(2000,8000);
  h_evn_runall->Draw();
  c1->cd(4);
  gPad->SetLogy();
  h_evn_runeach->Draw();
  c1->cd();
  c1->Update();
  getchar();

  gROOT->cd();

  //===================================================================
  //===================================================================
  cout<<" Tower-by-Tower analysis.... "<<endl;

  //===================================================================
  //===================================================================
  //#define SKIPSKIPTMPTMP
#ifdef SKIPSKIPTMPTMP
  {// This is a temporaly part.......
    h_eall = new TH1F("h_eall","Total deposited energy",1000,0,5000);
    h_evn = new TH1F("h_evn","Total Event number",1000,0,50000);
    h_meane = new TH1F("h_meane","Mean deposited energy",200,0,2.);
    ipbscgl = 2;
    while( ipbscgl-- ){
      sprintf(hname,"h_eall_pbscgl%d",ipbscgl);
      h_eall_pbscgl[ipbscgl] = new TH1F(hname,"Total deposited energy",1000,0,5000);
      sprintf(hname,"h_evn_pbscgl%d",ipbscgl);
      h_evn_pbscgl[ipbscgl] = new TH1F(hname,"Total Event number",1000,0,100);
      sprintf(hname,"h_meane_pbscgl%d",ipbscgl);
      h_meane_pbscgl[ipbscgl] = new TH1F(hname,"Mean deposited energy",100,0,1.);
    }
    //
    itwr = 24768;
    while( itwr-- ){
      ftmp = 0;
      if( h_evn_twr->GetBinContent(itwr+1) > 0 ){
    	ftmp = h_eall_twr->GetBinContent(itwr+1)
	  / h_evn_twr->GetBinContent(itwr+1);
	h_meane_twr->SetBinContent(itwr+1,ftmp);
      }
      h_eall->Fill( h_eall_twr->GetBinContent(itwr+1) );
      h_evn->Fill( h_evn_twr->GetBinContent(itwr+1) );
      h_meane->Fill( ftmp );
      if( itwr < 5184 ){
	ipbscgl = 0;
	h_eall_pbscgl[ipbscgl]->Fill( h_eall_twr->GetBinContent(itwr+1) );
	h_evn_pbscgl[ipbscgl]->Fill( h_evn_twr->GetBinContent(itwr+1) );
	h_meane_pbscgl[ipbscgl]->Fill( ftmp );
      } else if (itwr > 20159 ){
	ipbscgl = 1;
	h_eall_pbscgl[ipbscgl]->Fill( h_eall_twr->GetBinContent(itwr+1) );
	h_evn_pbscgl[ipbscgl]->Fill( h_evn_twr->GetBinContent(itwr+1) );
	h_meane_pbscgl[ipbscgl]->Fill( ftmp );
      }
    }
  }
#endif
  //===================================================================
  //===================================================================
  gROOT->cd();
  TH1F* h_qa_twr = new TH1F("h_qa_twr","QA tower",24768,0,24768);
  TH2F* h_qa_twrsect[8];
  h_qa_twrsect[0] = new TH2F("h_qa_twrsect0","QA tower sect0",72,0,72,36,0,36);
  h_qa_twrsect[1] = new TH2F("h_qa_twrsect1","QA tower sect1",72,0,72,36,0,36);
  h_qa_twrsect[7] = new TH2F("h_qa_twrsect7","QA tower sect7",96,0,96,48,0,48);
  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      iz = (isect>=6 ? 96 : 72 );
      while( iz-- ){
	iy = (isect>=6 ? 48 : 36 );
	while( iy-- ){
	  itwr = (isect>=6 ? 15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz);
	  if( isect == 0 || isect == 1 ){
	    if( h_eall_twr->GetBinContent(itwr+1) < 1418.5 ||
		h_eall_twr->GetBinContent(itwr+1) > 3515.5 ||
		h_evn_twr->GetBinContent(itwr+1) < 8305 ||
		h_evn_twr->GetBinContent(itwr+1) > 16303 ||
		h_meane_twr->GetBinContent(itwr+1) < 0.18 ||
		h_meane_twr->GetBinContent(itwr+1) > 0.25 ){
	      h_qa_twr->Fill(itwr);
	      h_qa_twrsect[isect]->Fill(iz,iy);
	    }
	  } else if( isect == 7 ){
	    if( h_eall_twr->GetBinContent(itwr+1) < 1 ||
		h_eall_twr->GetBinContent(itwr+1) > 1184 ||
		h_evn_twr->GetBinContent(itwr+1) < 654 ||
		h_evn_twr->GetBinContent(itwr+1) > 4704 ||
		h_meane_twr->GetBinContent(itwr+1) < 0.1 ||
		h_meane_twr->GetBinContent(itwr+1) > 0.4 ){
	      h_qa_twr->Fill(itwr);
	      //h_qa_twr->SetBinContent(itwr+1,1)
	      h_qa_twrsect[isect]->Fill(iz,iy);
	    }
	  }
	  //
	}
      }
    }
  }
  //===================================================================
  //===================================================================

  c1->Clear();
  c1->Divide(3,2);
  c1->cd(1);
  gPad->SetLogy();
  h_eall_pbscgl[0]->Fit("gaus");
  h_eall_pbscgl[0]->Draw();
  c1->cd(2);
  gPad->SetLogy();
  h_evn_pbscgl[0]->Fit("gaus");
  h_evn_pbscgl[0]->Draw();
  c1->cd(3);
  gPad->SetLogy();
  h_meane_pbscgl[0]->Fit("gaus");
  h_meane_pbscgl[0]->Draw();
  c1->cd(4);
  gPad->SetLogy();
  h_eall_pbscgl[1]->Fit("gaus");
  h_eall_pbscgl[1]->Draw();
  c1->cd(5);
  gPad->SetLogy();
  h_evn_pbscgl[1]->Fit("gaus");
  h_evn_pbscgl[1]->Draw();
  c1->cd(6);
  gPad->SetLogy();
  h_meane_pbscgl[1]->Fit("gaus");
  h_meane_pbscgl[1]->Draw();
  c1->cd();
  c1->Update();
  getchar();

  //===================================================================
  //===================================================================
  //===================================================================
  //===================================================================
  {
    TDirectory* current = gDirectory;
    cout<<" Creating root file nt_towerplot.root"<<endl;
    TFile* f_plot = new TFile("nt_towerplot.root","RECREATE");
    gROOT->GetList()->Write();
    f_plot->Close();
    current->cd();
  }


}
//
