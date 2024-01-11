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
#include "Evt.hh"
#include "Hist.hh"
#include "Hist_ass.hh"
#include "Hist_ele.hh"
#include "Hist_mip.hh"
#include "Hist_trig.hh"
#include "trk_ass_trig_merge.hh"

int trk_ass_trig_merge(char* filelist,char* writefile="merge_all.root",int start = 0, int stop = 0){

  gROOT->cd();
  Hist_ass* hist_ass   = new Hist_ass("hist_ass","Association ana");
  Hist_ass* hist_ass_s = new Hist_ass("hist_ass_s","Association ana flipped emc");
  Hist_trig* hist_trig = new Hist_trig("hist_trig","Trigger study");
  Hist_trig* hist_trig_s = new Hist_trig("hist_trig_s","Trigger study");
  Hist_trig* hist_trig50 = new Hist_trig("hist_trig50","Trigger study");
  Hist_trig* hist_trig50_s = new Hist_trig("hist_trig50_s","Trigger study");

  char fname[128];
  //
  int tot_nfile = 0;
  ifstream ftmp(filelist);
  while( ftmp >> fname) tot_nfile++;
  ftmp.close();
  //
  cout<<" ================================================================================= "<<endl;
  int nfile = 0;
  ifstream fin(filelist);
  while( fin >> fname){
    ++nfile;
    if( ( nfile >= start && nfile <= stop ) || stop == 0){
      cout<<" ================================================================================= "<<endl;
      cout<<" Read and add : "<<fname<<" :  "<<nfile<<" / "<<tot_nfile<<endl;
      TFile* f = new TFile(fname);
      //---------------------------------------------------------------------------
      Hist_ass* t_ass = (Hist_ass*)f->Get("hist_ass");
      if( t_ass != NULL ){
	cout<<"  hist_ass entries : "<<hist_ass->GetEntries()<<" --> "; hist_ass->Add(t_ass); cout<<hist_ass->GetEntries()<<endl;
	delete t_ass;
      }
      Hist_ass* t_ass_s = (Hist_ass*)f->Get("hist_ass_s");
      if( t_ass_s != NULL ){
	cout<<"  hist_ass_s entries : "<<hist_ass_s->GetEntries()<<" --> "; hist_ass_s->Add(t_ass_s); cout<<hist_ass_s->GetEntries()<<endl;
	delete t_ass_s;
      }
      //---------------------------------------------------------------------------
      Hist_trig* t_trig = (Hist_trig*)f->Get("hist_trig");
      if( t_trig != NULL ){
	cout<<"  hist_trig entries : "<<hist_trig->GetEntries()<<" --> "; hist_trig->Add(t_trig); cout<<hist_trig->GetEntries()<<endl;
	delete t_trig;
      }
      Hist_trig* t_trig_s = (Hist_trig*)f->Get("hist_trig_s");
      if( t_trig_s != NULL ){
	cout<<"  hist_trig_s entries : "<<hist_trig_s->GetEntries()<<" --> "; hist_trig_s->Add(t_trig_s); cout<<hist_trig_s->GetEntries()<<endl;
	delete t_trig_s;
      }
      Hist_trig* t_trig50 = (Hist_trig*)f->Get("hist_trig50");
      if( t_trig50 != NULL ){
	cout<<"  hist_trig50 entries : "<<hist_trig50->GetEntries()<<" --> "; hist_trig50->Add(t_trig50); cout<<hist_trig50->GetEntries()<<endl;
	delete t_trig50;
      }
      Hist_trig* t_trig50_s = (Hist_trig*)f->Get("hist_trig50_s");
      if( t_trig50_s != NULL ){
	cout<<"  hist_trig50_s entries : "<<hist_trig50_s->GetEntries()<<" --> "; hist_trig50_s->Add(t_trig50_s); cout<<hist_trig50_s->GetEntries()<<endl;
	delete t_trig50_s;
      }
      //---------------------------------------------------------------------------
      f->Close();
    }
  }

  TFile* nf = new TFile(writefile,"RECREATE");
  hist_ass->Write(); delete hist_ass;
  hist_ass_s->Write(); delete hist_ass_s;
  hist_trig->Write(); delete hist_trig;
  hist_trig_s->Write(); delete hist_trig_s;
  hist_trig50->Write(); delete hist_trig50;
  hist_trig50_s->Write(); delete hist_trig50_s;
  nf->Close();
  fin.close();

  cout<<" ================================================================================= "<<endl;

};
//---------------------------


