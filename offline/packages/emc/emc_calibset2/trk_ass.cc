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
#include "nt_trk.hh"
#include "Evt.hh"
#include "Pid.hh"
#include "Hist.hh"
#include "Hist_ass.hh"
#include "Hist_ele.hh"
#include "Hist_mip.hh"
#include "Hist_miptwr.hh"
#include "Hist_miptwr2.hh"
#include "Hist_eqa.hh"
#endif

#include "trk_ass.hh"

void trk_ass(Evt* evt,char* fname,int maxevent =0,bool update=false,bool debug = false){
  int isect,itwr_sect,ism_sect;
  int ipid,iepid,iprob;
  char hname[256],htitle[256];

  // --- Read htorii's private run&tower QA
  emcRejectList(getenv("HTPARDIR"));
  
  //--- Create output file
  cout<<" Open output ntuple "<<fname<<endl;
  TFile* f_output;
  if( update )
    f_output = new TFile(fname,"UPDATE");
  else
    f_output = new TFile(fname,"RECREATE");

  // --- Create histgrams
  //if( debug ) Hist::SetOptDebug();
  Hist_ass* hist_ass   = new Hist_ass("hist_ass","Association ana");
  Hist_ass* hist_ass_s = new Hist_ass("hist_ass_s","Association ana flipped emc");
  Hist_ass* hist_ass_per = new Hist_ass("hist_ass_per","Association ana");
  Hist_ass* hist_ass_per_s = new Hist_ass("hist_ass_per_s","Association ana flipped emc");
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
  //Hist_miptwr2* hist_miptwr_s  = new Hist_miptwr2("hist_miptwr_s","MIP tower ana flipped emc");

  //--- Create ntuple for global info
  //  TTree *t_glb = new TTree("t_glb","Event ana");
  //  Global f_glb;
  //  Global* pt_glb = &f_glb;
  //  TBranch* b_glb = t_glb->Branch("t_glb","Global",&pt_glb,32000,2);

  //--- Create ntuple for Track info
  //  TTree *t_trk = new TTree("t_trk","Track ana");
  //  Track f_trk;
  //  Track* pt_trk = &f_trk;
  //  TBranch* b_trk = t_trk->Branch("t_trk","Track",&pt_trk,32000,2);

  //======================================================================
  int current_run = 0 ;
  cout<<" Filling all histgrams "<<endl;
  int ana_num = 0;
  while ( evt->Next() && (ana_num < maxevent || maxevent == 0 ) ) {
    if( ++ana_num % 100 == 0 || debug ) cout<<"   Analyzed event "<<ana_num<<" / "<<evt->GetEntries()<<endl;
    Global& glb = evt->_glb;
    //    f_glb = glb;
    //    t_glb->Fill();
    // -- Event Selection
    // && glb.trig<22000 
    if( frunstat[evt->_glb.run] == 0 && glb.bbcz>-40 && glb.bbcz<40 ){   // Event Selection....
      vector<Track>& vec_trk = evt->_vec_trk;
      vector<Clust>& vec_clt = evt->_vec_clt;
      vector<Ass>& vec_asstrk = evt->_vec_asstrk;
      vector<Ass>& vec_asstrk_s = evt->_vec_asstrk_s;
      Pid emcpid;
      //======================================================================
      // --- Tracking Base histgrams
      int ntrk = vec_trk.size();
      if( debug ) cout<<" vec_trk:: ntrk = "<<ntrk<<endl;
      while( ntrk-- ){
	Track& trk = vec_trk[ntrk];
	//	f_trk = trk;
	//	t_trk->Fill();
	//
	Ass& asstrk = vec_asstrk[ntrk];
	Ass& asstrk_s = vec_asstrk_s[ntrk];
	// --- Associated Cluster
	int nclt = asstrk.GetSize();
	if( debug ) cout<<" trk_ass:: asstrk size = "<<nclt<<endl;
	if( nclt > 0 ){
	  int clt_id = asstrk.GetID(0); // Only first association...
	  Clust& clt = vec_clt[clt_id];
	  emcpid.set(glb,trk,clt);
	}
	while( nclt-- ){
	  int clt_id = asstrk.GetID(nclt);
	  Clust& clt = vec_clt[clt_id];
	  if( (clt.arm == 0 || clt.arm == 1 ) ){  // &&  emcRejectClust(clt) == 0 ){
	    hist_mip->Fill(glb,trk,clt,emcpid);
	    hist_miptwr->Fill(glb,trk,clt,emcpid);
	    hist_ass->Fill(glb,trk,clt,emcpid);
	    if( glb.centbin >= 12 && glb.centbin < 16 ) //60-80%
	      hist_ass_per->Fill(glb,trk,clt,emcpid);
	    //if( glb.centbin >= 0 && glb.centbin < 2 ) //0-10%
	    // hist_ass_cent->Fill(glb,trk,clt,emcpid);
	    if( trk.crk_acc > 0 && trk.crk_npe0 > 2 && trk.crk_chi2/trk.crk_npe0 < 10 )
	      hist_ele->Fill(glb,trk,clt,emcpid);
	    if( trk.crk_acc_s > 0 && trk.crk_npe0_s > 2 && trk.crk_chi2_s/trk.crk_npe0_s < 10 )
	      hist_ele_rs->Fill(glb,trk,clt,emcpid);
	  }
	} // End of (nclt--)
	// --- Associated Cluster with flipped z geometry.
	int nclt_s = asstrk_s.GetSize();
	// if( nclt_s > 0 ){
	//	  int clt_id = asstrk_s.GetID(0); // Only first Association...
	//	}
	while( nclt_s-- ){
	  int clt_id = asstrk_s.GetID(nclt_s);
	  //
	  Clust clt_s = vec_clt[clt_id];
	  clt_s.pos[2] = evt->swapz( clt_s.pos[2] ); // FIX.ME...
	  if( (clt_s.arm == 0 || clt_s.arm == 1 ) ){  // && emcRejectClust(clt_s) == 0 ){
	    hist_mip_s->Fill(glb,trk,clt_s,emcpid);
	    hist_miptwr->Fill(glb,trk,clt_s,emcpid,-1);
	    //hist_miptwr_s->Fill(glb,trk,clt_s,emcpid);
	    hist_ass_s->Fill(glb,trk,clt_s,emcpid);
	    if( glb.centbin >= 12 && glb.centbin < 16 ) //60-80%
	      hist_ass_per_s->Fill(glb,trk,clt_s,emcpid);
	    //	    if( glb.centbin >= 0 && glb.centbin < 2 ) //0-10%
	    //	      hist_ass_cent_s->Fill(glb,trk,clt_s,emcpid);
	    if( trk.crk_acc > 0 && trk.crk_npe0 > 2 && trk.crk_chi2/trk.crk_npe0 < 10 )
	      hist_ele_s->Fill(glb,trk,clt_s,emcpid);
	    if( trk.crk_acc_s > 0 && trk.crk_npe0_s > 2 && trk.crk_chi2_s/trk.crk_npe0_s < 10 )
	      hist_ele_srs->Fill(glb,trk,clt_s,emcpid);
	  }
	} // End of (nclt--)
      } // End of (ntrk--)
      //======================================================================
      int nclt = vec_clt.size();
      if( debug ) cout<<" vec_clt:: nclt = "<<nclt<<endl;
      Track trk;
      while( nclt-- ){
	if( vec_clt[nclt].ecent > 0.03 )
	  hist_eqa->Fill(glb,trk,vec_clt[nclt],emcpid);
	if( vec_clt[nclt].ecent > 0.1 )
	  hist_eqa_1->Fill(glb,trk,vec_clt[nclt],emcpid);
      }
      //======================================================================
    } // --- End of event selection	  
  } // --- End of loop
  //======================================================================


  cout<<" Finished process at " << ana_num <<endl;
  cout<<"------------------------------------------------------------"<<endl;
  cout<<" Closing all objects. "<<endl;
  f_output->cd();
  //  t_trk->Write();
  //  t_glb->Write();
  hist_ass->Write(); delete hist_ass;
  hist_ass_s->Write(); delete hist_ass_s;
  hist_ass_per->Write(); delete hist_ass_per;
  hist_ass_per_s->Write(); delete hist_ass_per_s;
  //  hist_ass_cent->Write(); delete hist_ass_cent;
  //  hist_ass_cent_s->Write(); delete hist_ass_cent_s;
  hist_ele->Write(); delete hist_ele;
  hist_ele_s->Write(); delete hist_ele_s;
  hist_ele_rs->Write(); delete hist_ele_rs;
  hist_ele_srs->Write(); delete hist_ele_srs;
  hist_eqa->Write(); delete hist_eqa;
  hist_eqa_1->Write(); delete hist_eqa_1;
  hist_mip->Write(); delete hist_mip;
  hist_mip_s->Write(); delete hist_mip_s;
  hist_miptwr->Write(); delete hist_miptwr;
  //hist_miptwr_s->Write(); delete hist_miptwr_s;
  f_output->Close();
  cout<<"------------------------------------------------------------"<<endl;
}
//===============================================================
