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
#include "Hist_trig.hh"
#endif

#include "trk_ass_trig.hh"

void trk_ass_trig(TTree* dst,char* fname,int maxevent =0,bool debug = false){
		  
  int isect;
  int ipid,iepid,iprob;
  char hname[256],htitle[256];
  emcRejectList(getenv("HTPARDIR"));
  //emcRejectList("/phenix/u/htorii/local/photon/Calibset1");

  //--- Open micro-DST
  Evt* evt =  new Evt();
  evt->Init_DST(dst);
  
  //--- Create histgrams
  //if( debug ) Hist::SetOptDebug();
  Hist_ass* hist_ass   = new Hist_ass("hist_ass","Association ana");
  Hist_ass* hist_ass_s = new Hist_ass("hist_ass_s","Association ana flipped emc");
  Hist_trig* hist_trig = new Hist_trig("hist_trig","Trigger study");
  Hist_trig* hist_trig_s = new Hist_trig("hist_trig_s","Trigger study");
  Hist_trig* hist_trig50 = new Hist_trig("hist_trig50","Trigger study");
  Hist_trig* hist_trig50_s = new Hist_trig("hist_trig50_s","Trigger study");

  //--- Create output file
  cout<<" Open output ntuple "<<fname<<endl;
  TFile* f_output = new TFile(fname,"RECREATE");

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
    if( frunstat[evt->_glb.run] > 0 &&
	glb.bbcz>-40 && glb.bbcz<40 && glb.trig<22000 ){   // Event Selection....
      vector<Track>& vec_trk = evt->_vec_trk;
      vector<Clust>& vec_clt = evt->_vec_clt;
      vector<Ass>& vec_asstrk = evt->_vec_asstrk;
      vector<Ass>& vec_asstrk_s = evt->_vec_asstrk_s;
      //======================================================================
      // --- Tracking Base histgrams
      int ntrk = vec_trk.size();
      if( debug ) cout<<" trk_ass_trig:: ntrk = "<<ntrk<<endl;
      while( ntrk-- ){
	Track& trk = vec_trk[ntrk];
	//	f_trk = trk;
	//	t_trk->Fill();
	//
	Ass& asstrk = vec_asstrk[ntrk];
	Ass& asstrk_s = vec_asstrk_s[ntrk];
	Pid emcpid;
	// --- Associated Cluster
	int nclt = asstrk.GetSize();
	if( debug ) cout<<" trk_ass_trig:: asstrk size = "<<nclt<<endl;
	if( nclt > 0 ){
	  int clt_id = asstrk.GetID(0); // Only first association...
	  Clust& clt = vec_clt[clt_id];
	  emcpid.set(glb,trk,clt);
	}
	while( nclt-- ){
	  int clt_id = asstrk.GetID(nclt);
	  Clust& clt = vec_clt[clt_id];
	  if( (clt.arm == 0 || clt.arm == 1 ) &&  emcRejectClust(clt) == 0 ){
	    if( glb.emcnhit < 100 )
	      hist_trig->Fill(glb,trk,clt,emcpid);
	    if( glb.emcnhit < 50 )
	      hist_trig50->Fill(glb,trk,clt,emcpid);
	    hist_ass->Fill(glb,trk,clt,emcpid);
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
	  if( (clt_s.arm == 0 || clt_s.arm == 1 ) && emcRejectClust(clt_s) == 0 ){
	    if( glb.emcnhit < 100 )
	      hist_trig_s->Fill(glb,trk,clt_s,emcpid);
	    if( glb.emcnhit < 50 )
	      hist_trig50_s->Fill(glb,trk,clt_s,emcpid);
	    hist_ass_s->Fill(glb,trk,clt_s,emcpid);
	  }
	} // End of (nclt--)
      } // End of (ntrk--)
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
  hist_trig->Write(); delete hist_trig;
  hist_trig_s->Write(); delete hist_trig_s;
  hist_trig50->Write(); delete hist_trig50;
  hist_trig50_s->Write(); delete hist_trig50_s;
  f_output->Close();
  delete evt;
  cout<<"------------------------------------------------------------"<<endl;
}
//===============================================================
