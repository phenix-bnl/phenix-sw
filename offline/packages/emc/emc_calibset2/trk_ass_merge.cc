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
#include "Hist_miptwr.hh"
#include "Hist_miptwr2.hh"
#include "Hist_eqa.hh"
#include "trk_ass_merge.hh"

int trk_ass_merge(char* filelist,char* writefile="merge_all.root",int start = 0, int stop = 0,bool update = false){

  int isect;
  TFile* nf;
  if( update )
    nf = new TFile(writefile,"UPDATE");
  else
    nf = new TFile(writefile,"RECREATE");

  Hist_ass* hist_ass   = new Hist_ass("hist_ass","Association ana");
  Hist_ass* hist_ass_s = new Hist_ass("hist_ass_s","Association ana flipped emc");
  //  Hist_ass* hist_ass_per = new Hist_ass("hist_ass_per","Association ana");
  //  Hist_ass* hist_ass_per_s = new Hist_ass("hist_ass_per_s","Association ana flipped emc");
  //  Hist_ass* hist_ass_cent = new Hist_ass("hist_ass_cent","Association ana");
  //  Hist_ass* hist_ass_cent_s = new Hist_ass("hist_ass_cent_s","Association ana flipped emc");
  Hist_ele* hist_ele      = new Hist_ele("hist_ele","Electron ana");
  Hist_ele* hist_ele_s    = new Hist_ele("hist_ele_s","Electron ana flipped emc");
  Hist_ele* hist_ele_rs   = new Hist_ele("hist_ele_rs","Electron ana flipped rich");
  Hist_ele* hist_ele_srs  = new Hist_ele("hist_ele_srs","Electron ana flipped emc&rich");
  Hist_eqa* hist_eqa = new Hist_eqa("hist_eqa","EMC energy QA > 30MeV ");
  Hist_eqa* hist_eqa_1 = new Hist_eqa("hist_eqa_1","EMC energy QA > 100MeV");
  Hist_mip* hist_mip    = new Hist_mip("hist_mip","MIP ana");
  Hist_mip* hist_mip_s  = new Hist_mip("hist_mip_s","MIP ana flipped emc");
  Hist_miptwr2* hist_miptwr    = new Hist_miptwr2("hist_miptwr","MIP tower ana");
  //  Hist_miptwr2* hist_miptwr_s  = new Hist_miptwr2("hist_miptwr_s","MIP tower ana flipped emc");

  char fname[128];
  //
  int tot_nfile = 0;
  ifstream ftmp(filelist);
  while( ftmp >> fname) tot_nfile++;
  ftmp.close();
  //
  cout<<" ================================================================================= "<<endl;
  TObjArray filearray;
  int nfile = 0;
  ifstream fin(filelist);
  while( fin >> fname){
    ++nfile;
    if( ( nfile >= start && nfile <= stop ) || stop == 0){
      cout<<" ================================================================================= "<<endl;
      cout<<" Read and add : "<<fname<<" :  "<<nfile<<" / "<<tot_nfile<<endl;
      TFile* f = new TFile(fname);
      nf->cd();
      filearray.Add(f);
      //---------------------------------------------------------------------------
      hist_ass->Add(f);
      hist_ass_s->Add(f);
      //---------------------------------------------------------------------------
      hist_ele->Add(f);
      hist_ele_s->Add(f);
      hist_ele_rs->Add(f);
      hist_ele_srs->Add(f);
      //---------------------------------------------------------------------------
      hist_mip->Add(f);
      hist_mip_s->Add(f);
      hist_miptwr->Add(f);
      //hist_miptwr_s->Add(f);
      //---------------------------------------------------------------------------
      hist_eqa->Add(f);
      hist_eqa_1->Add(f);
      //---------------------------------------------------------------------------
      //f->Close();
      cout<<" ================================================================================= "<<endl;
    }
  }

  hist_ass->Write(); delete hist_ass;
  hist_ass_s->Write(); delete hist_ass_s;
  //  hist_ass_per->Write(); delete hist_ass_per;
  //  hist_ass_per_s->Write(); delete hist_ass_per_s;
  //  hist_ass_cent->Write(); delete hist_ass_cent;
  //  hist_ass_cent_s->Write(); delete hist_ass_cent_s;
  hist_eqa->Write(); delete hist_eqa;
  hist_eqa_1->Write(); delete hist_eqa_1;
  hist_ele->Write(); delete hist_ele;
  hist_ele_s->Write(); delete hist_ele_s;
  hist_ele_rs->Write(); delete hist_ele_rs;
  hist_ele_srs->Write(); delete hist_ele_srs;
  hist_mip->Write(); delete hist_mip;
  hist_mip_s->Write(); delete hist_mip_s;
  hist_miptwr->Write(); delete hist_miptwr;
  //hist_miptwr_s->Write(); delete hist_miptwr_s;
  nf->Close();
  fin.close();
  TFile* tfile;
  TIterator* itnext = filearray.MakeIterator();
  while( tfile = (TFile*) itnext->Next() ){
    tfile->Close();
    delete tfile;
  }
  cout<<" ================================================================================= "<<endl;

};
//---------------------------


