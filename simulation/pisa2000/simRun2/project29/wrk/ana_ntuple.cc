#ifdef COMPILE
#include "ana.hh"
#include <vector>
#include <math.h>
#endif

void ana_ntuple(char* infname="tmp2_ntuple.root",char* outfname="ana_ntuple_out.root"){

  //================================================================================ Initialization
  //Reset ROOT and connect tree file
  
  cout<<" Loading ana_cc.so ... "<<endl;
  gSystem->Load("ana_cc.so");
  //gSystem->Load("/phenix/data12/htorii/Myana_01sim/project29/wrk/simulation/pisa2000/simRun2/project29/wrk/ana_cc.so");
  cout<<" Loading libudst.so ... "<<endl;
  gSystem->Load("libudst.so");
  gROOT->Reset();
  cout<<" Open input file : "<<infname<<endl;
  TFile* f = new TFile(infname);
  TTree *T = (TTree*)f->Get("T");
  cout<<" Read TTree* nt from : "<<f->GetName()<<endl;

  //===== EVA/pythia
  CL_pythia* pt_pythia = new CL_pythia;
  T->SetBranchAddress("pythia",&pt_pythia);

  //===== DST/BbcOut
  BbcOutv1* pt_BbcOut = new BbcOutv1;
  T->SetBranchAddress("BbcOutv1",&pt_BbcOut);

  //===== EVA/fkin
  TClonesArray* ar_fkin = new TClonesArray("CL_fkin",1000);
  TClonesArray& ref_fkin = *ar_fkin;
  int nn_fkin;
  T->SetBranchAddress("nn_fkin",&nn_fkin);
  T->SetBranchAddress("fkin",&ar_fkin);

  //===== EVA/primary
  TClonesArray* ar_primary = new TClonesArray("CL_primary",1000);
  TClonesArray& ref_primary = *ar_primary;
  int nn_primary;
  T->SetBranchAddress("nn_primary",&nn_primary);
  T->SetBranchAddress("primary",&ar_primary);

  //===== EVA/header
  CL_header* pt_header = new CL_header;
  T->SetBranchAddress("header",&pt_header);

  //===== DST/dEmcGeaClusterTrack
  TClonesArray* ar_dEmcGeaClusterTrack = new TClonesArray("CL_dEmcGeaClusterTrack",200);
  TClonesArray& ref_dEmcGeaClusterTrack = *ar_dEmcGeaClusterTrack;
  int nn_dEmcGeaClusterTrack;
  T->SetBranchAddress("nn_dEmcGeaClusterTrack",&nn_dEmcGeaClusterTrack);
  T->SetBranchAddress("dEmcGeaClusterTrack",&ar_dEmcGeaClusterTrack);

  //===== DST/dEmcGeaTrackCluster
  TClonesArray* ar_dEmcGeaTrackCluster = new TClonesArray("CL_dEmcGeaTrackCluster",200);
  TClonesArray& ref_dEmcGeaTrackCluster = *ar_dEmcGeaTrackCluster;
  int nn_dEmcGeaTrackCluster;
  T->SetBranchAddress("nn.dEmcGeaTrackCluster",&nn_dEmcGeaTrackCluster);
  T->SetBranchAddress("dEmcGeaTrackCluster",&ar_dEmcGeaTrackCluster);

  //===== DST/dEmcGeaTrack
  TClonesArray* ar_dEmcGeaTrack = new TClonesArray("CL_dEmcGeaTrack",200);
  TClonesArray& ref_dEmcGeaTrack = *ar_dEmcGeaTrack;
  int nn_dEmcGeaTrack;
  T->Branch("nn.dEmcGeaTrack",&nn_dEmcGeaTrack,"nn_dEmcGeaTrack/I");
  T->Branch("dEmcGeaTrack",&ar_dEmcGeaTrack);
  T->SetBranchAddress("nn.dEmcGeaTrack",&nn_dEmcGeaTrack);
  T->SetBranchAddress("dEmcGeaTrack",&ar_dEmcGeaTrack);

  //================================================================================ Histgrams
  cout<<" Open output file : "<<outfname<<endl;
  TFile* f_out = new TFile(outfname,"RECREATE");
  // From primary/fkin information
  TH1F* h_pri_rawgam;
  TH1F* h_pri_rawpi0gam;
  TH1F* h_pri_rawpi0;
  TH2F* h2_pri_rawpi0gam_phieta;
  TH1F* h_pri_accgam_cut[4];
  TH1F* h_pri_accpi0gam_cut[4];
  TH1F* h_pri_accpi0_cut[4];
  TH2F* h2_pri_accpi0gam_phieta_cut[4];
  // From dEmcGeaTrackCluster information
  TH1F* h_emcv0_accgam_cut[4];
  TH1F* h_emcv0_accpi0gam_cut[4];
  TH1F* h_emcv0_accpi0_cut[4];
  TH2F* h2_emcv0_accpi0gam_phieta_cut[4];
  // From dEmcGeaClusterTrack information
  TH1F* h_emcpt2_accpi0_cut[4];
  TH1F* h_emcm2_accpi0_cut[4];
  TH1F* h_emcpt0_accpi0_cut[4];
  TH1F* h_emcm0_accpi0_cut[4];
  TH2F* h2_emcmpt0_accpi0_cut[4];
  // From all measured pure-pi0
  TH1F* h_emcpt_purepi0_cut[4];
  TH1F* h_emcm_purepi0_cut[4];
  //
  char hname[256],htitle[256];
  char* cut[4] = { "None","West(PbSc)", "East top(PbSc)","East bottom(PbGl)"};
  int icut,icut_pi0;
  //
  sprintf(hname,"h_pri_rawgam");
  sprintf(htitle,"Primary raw gam");
  h_pri_rawgam = new TH1F(hname,htitle,200,0,20);
  //
  sprintf(hname,"h_pri_rawpi0gam");
  sprintf(htitle,"Primary raw pi0gam");
  h_pri_rawpi0gam = new TH1F(hname,htitle,200,0,20);
  //
  sprintf(hname,"h_pri_rawpi0");
  sprintf(htitle,"Primary raw pi0");
  h_pri_rawpi0 = new TH1F(hname,htitle,200,0,20);
  //
  sprintf(hname,"h2_pri_rawpi0gam_phieta");
  sprintf(htitle,"Primary rawpi0gam phi-eta");
  h2_pri_rawpi0gam_phieta = new TH2F(hname,htitle,100,-180,180,100,-10.0,10.0);
  //
  icut = 4;
  while( icut-- ){
    //
    sprintf(hname,"h_pri_accgam_%d",icut);
    sprintf(htitle,"Primary acc gam %s",cut[icut]);
    h_pri_accgam_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_pri_accpi0gam_%d",icut);
    sprintf(htitle,"Primary acc pi0gam %s",cut[icut]);
    h_pri_accpi0gam_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_pri_accpi0_%d",icut);
    sprintf(htitle,"Primary acc pi0 %s",cut[icut]);
    h_pri_accpi0_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h2_pri_accpi0gam_phieta_%d",icut);
    sprintf(htitle,"Primary accpi0gam phi-eta %s",cut[icut]);
    h2_pri_accpi0gam_phieta_cut[icut] = new TH2F(hname,htitle,100,-180,180,100,-10.0,10.0);
    //
    sprintf(hname,"h_emcv0_accgam_%d",icut);
    sprintf(htitle,"EMCal acc gam %s",cut[icut]);
    h_emcv0_accgam_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_emcv0_accpi0gam_%d",icut);
    sprintf(htitle,"EMCal acc pi0gam %s",cut[icut]);
    h_emcv0_accpi0gam_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_emcv0_accpi0_%d",icut);
    sprintf(htitle,"EMCal acc pi0 %s",cut[icut]);
    h_emcv0_accpi0_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h2_emcv0_accpi0gam_phieta_%d",icut);
    sprintf(htitle,"EMCal accpi0gam phi-eta %s",cut[icut]);
    h2_emcv0_accpi0gam_phieta_cut[icut] = new TH2F(hname,htitle,100,-180,180,100,-10.0,10.0);
    //
    sprintf(hname,"h_emcpt2_accpi0_%d",icut);
    sprintf(htitle,"EMCal pT from 2nd acc pi0 %s",cut[icut]);
    h_emcpt2_accpi0_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_emcm2_accpi0_%d",icut);
    sprintf(htitle,"EMCal Mass from 2nd acc pi0 %s",cut[icut]);
    h_emcm2_accpi0_cut[icut] = new TH1F(hname,htitle,100,0,1.0);
    //
    sprintf(hname,"h_emcpt0_accpi0_%d",icut);
    sprintf(htitle,"EMCal pT from all acc pi0 %s",cut[icut]);
    h_emcpt0_accpi0_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_emcm0_accpi0_%d",icut);
    sprintf(htitle,"EMCal Mass from all acc pi0 %s",cut[icut]);
    h_emcm0_accpi0_cut[icut] = new TH1F(hname,htitle,100,0,1.0);
    //
    sprintf(hname,"h2_emcmpt0_accpi0_%d",icut);
    sprintf(htitle,"EMCal Mass vs pT from all acc pi0 %s",cut[icut]);
    h2_emcmpt0_accpi0_cut[icut] = new TH2F(hname,htitle,100,0,1.0,100,0,2);
    //
    sprintf(hname,"h_emcpt_purepi0_%d",icut);
    sprintf(htitle,"EMCal pT from measured pure pi0 %s",cut[icut]);
    h_emcpt_purepi0_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_emcm_purepi0_%d",icut);
    sprintf(htitle,"EMCal Mass from measured pure pi0 %s",cut[icut]);
    h_emcm_purepi0_cut[icut] = new TH1F(hname,htitle,100,0,1.0);
  }
  //
  //================================================================================ Output container and Ntuple
  // --- definition of vector
  //#define NTUPLE
#ifdef NTUPLE
  TTree* anatr = new TTree("anatr","Output from ana_ntuple");
#endif
  CL_event* pt_event = new CL_event;
#ifdef NTUPLE
  TClonesArray* pt_recpri = pt_event->ar_recpri;
  anatr->Branch("recpri",&pt_recpri);
#endif
  
  //================================================================================
  int nn_tmp0 = 0;
  int nn_tmp1 = 0;
  int nn_tmp2 = 0;
  gROOT->cd();
  bool _debug = false;
  int nn,num;
  int isect;
  float pt,pt_pi0,asym;
  float ptot,px,py,pz;
  bool b_decay;
  Int_t nentries = T->GetEntries();
  Int_t nbytes = 0;
  for (Int_t i=0; i<nentries;i++) {
    nbytes += T->GetEntry(i);
    // ========== Initialization
    float v_dr = sqrt(pt_header->vertex[0]*pt_header->vertex[0] + pt_header->vertex[1]*pt_header->vertex[1]);
    cout<<" +++ Event # : "<<i<<" / "<<nentries<<" nn_primary:"<<nn_primary<<" nn_fkin:"<<nn_fkin
	<<" nn_dEmcGeaTrackCluster:"<<nn_dEmcGeaTrackCluster<<" nn_dEmcGeaClusterTrack:"<<nn_dEmcGeaClusterTrack<<endl;
    //<<" v=("<<pt_header->vertex[0]<<","<<pt_header->vertex[1]<<","<<pt_header->vertex[2]<<") dr:"<<v_dr<<endl;
    // ========== Analyzing header/primary/fkin/dEmcGeaTrackCluster
    pt_event->Reset();
    pt_event->set_header(pt_header);
    pt_event->set_primary(ar_primary);
    pt_event->set_fkin(ar_fkin);
    pt_event->set_dEmcGeaTrackCluster(ar_dEmcGeaTrackCluster);
    pt_event->set_dEmcGeaClusterTrack(ar_dEmcGeaClusterTrack);
    //pt_event->Print();
#ifdef NTUPLE
    anatr->Fill();
#endif
    TClonesArray& ref_recpri = *(pt_event->ar_recpri);
    TClonesArray& ref_part = *(pt_event->ar_part);

    // ========== Filling Histgrams
    num = ref_recpri.GetEntries();
    while( num-- ){
      CL_recpri* recpri = (CL_recpri*) ref_recpri->At(num);
      // === The primary particle is photon.
      if( recpri->primary.idpart == 1 && recpri->b_part ){
	CL_part* part = &(recpri->part);
	// --- No child particle...
	icut = part->i_accept;
	h_pri_rawgam->Fill(part->f_pt);
	nn = part->ar_part->GetEntries();
	if( part->b_fkin &&
	    part->fkin.idpart == 1 && 
	    part->i_accept >= 0 ){
	  h_pri_accgam_cut[icut]->Fill(recpri->part.f_pt);
	  if( recpri->part.b_dEmcGeaTrackCluster ){
	    h_emcv0_accgam_cut[icut]->Fill(recpri->part.f_pt);
	  }
	}
      }
      // === All pi0s including K0s,eta decay.
      // --- From pure-pi0 clusters. 
      TObjArray ar_pi0 = recpri->part->getarray_part_pid(7); // pi0_pid == 7
      nn = ar_pi0.GetEntries();
      if( nn != 1 ) cout<<" DEBUG:: nn = "<<nn<<endl;
      while( nn-- ){
	CL_part* pi0 = (CL_part*) ar_pi0[nn];
	nn_tmp0 ++;
	// Require pi0->2particle decay and coming from vertex or K0s or eta.
	if( pi0->b_fkin && pi0->ar_part->GetEntries() == 2 &&
	    ( pi0->fkin->r_vertex < 0.1 || pi0->fkin->idparent == 10 || pi0->fkin->idparent == 17 ) ){
	  //	  cout<<" DEBUG:: pi0->fkin->r_vertex "<<pi0->fkin->r_vertex<<endl;
	  CL_part* child_part0 = (CL_part*) pi0->ar_part->At(0);
	  CL_part* child_part1 = (CL_part*) pi0->ar_part->At(1);
	  nn_tmp1 ++;
	  if( child_part0->ar_dEmcGeaClusterTrack->GetEntries() == 1 &&
	      child_part1->ar_dEmcGeaClusterTrack->GetEntries() == 1 ){
	    nn_tmp2 ++;
	    icut = 0;
	    if( child_part0->i_accept == child_part1->i_accept && child_part0->i_accept > 0 )
	      icut = child_part0->i_accept;
	    CL_dEmcGeaClusterTrack* g0 = (CL_dEmcGeaClusterTrack*) child_part0->ar_dEmcGeaClusterTrack->At(0);
	    CL_dEmcGeaClusterTrack* g1 = (CL_dEmcGeaClusterTrack*) child_part1->ar_dEmcGeaClusterTrack->At(0);
	    float len0 = sqrt(g0->measxyz[0]*g0->measxyz[0] + g0->measxyz[1]*g0->measxyz[1] + g0->measxyz[2]*g0->measxyz[2]);
	    float len1 = sqrt(g1->measxyz[0]*g1->measxyz[0] + g1->measxyz[1]*g1->measxyz[1] + g1->measxyz[2]*g1->measxyz[2]);
	    float cosine = g0->measxyz[0]*g1->measxyz[0] + g0->measxyz[1]*g1->measxyz[1] + g0->measxyz[2]*g1->measxyz[2];
	    cosine /= len0 * len1;
	    float mass = sqrt( 2.0 * g0.ecore * g1.ecore * ( 1.0 - cosine ) );
	    float px = g0->ecore * g0->measxyz[0]/len0 + g1->ecore * g1->measxyz[0]/len1;
	    float py = g0->ecore * g0->measxyz[1]/len0 + g1->ecore * g1->measxyz[1]/len1;
	    float pt = sqrt( px*px + py*py );
	    h_emcpt_purepi0_cut[icut]->Fill(pt);
	    h_emcm_purepi0_cut[icut]->Fill(mass);
	  }
	}
      }
      // === The primary particle is pi0.
      if( recpri->primary.idpart == 7 && recpri->b_part ){
	// --- Primary pi0
	pt_pi0 = recpri->primary.px_momentum*recpri->primary.px_momentum
	  + recpri->primary.py_momentum*recpri->primary.py_momentum;
	pt_pi0 = sqrt(pt_pi0);
	h_pri_rawpi0->Fill(pt_pi0);
	// --- Looping of all child particles
	nn = recpri->part.ar_part->GetEntries();
	while( nn-- ){
	  CL_part* part = (CL_part*) recpri->part.ar_part->At(nn);
	  if( part->b_fkin && part->fkin.idpart == 1 ){  // Child particle must be photon
	    // --- All child particle
	    h_pri_rawpi0gam->Fill(part->f_pt);
	    // --- Accpeted child photon.
	    if( part->i_accept >= 0 && part->fkin.idpart == 1 ){
	      icut = part->i_accept;
	      h_pri_accpi0gam_cut[icut]->Fill(part->f_pt);
	      h2_pri_accpi0gam_phieta_cut[icut]->Fill(part->f_phi,part->f_eta);
	      if( part->b_dEmcGeaTrackCluster ){
		h_emcv0_accpi0gam_cut[icut]->Fill(part->f_pt);
		h2_emcv0_accpi0gam_phieta_cut[icut]->Fill(part->f_phi,part->f_eta);
	      }
	    }
	  }
	}
	// --- Pi0's two photon are accepted
	nn = recpri->part.ar_part->GetEntries();
	if( nn == 2 ){
	  CL_part* part0 = (CL_part*) recpri->part.ar_part->At(0);
	  CL_part* part1 = (CL_part*) recpri->part.ar_part->At(1);
	  asym = fabs(part0->fkin.ptot - part1->fkin.ptot);
	  if( part0->fkin.ptot + part1->fkin.ptot > 0 )
	    asym /= part0->fkin.ptot + part1->fkin.ptot;
	  else
	    asym = 1.0;
#ifdef DEBUG
	  cout<<" DEBUG::two particle.. "<<endl;
	  cout<<" DEBUG::asym = "<<asym<<endl;
	  cout<<" DEBUG::part0->b_fkin && part1->b_fkin  = "<<(int)(part0->b_fkin && part1->b_fkin)<<endl;
	  cout<<" DEBUG::part0->fkin.idpart = "<<part0->fkin.idpart<<endl;
	  cout<<" DEBUG::part1->fkin.idpart = "<<part1->fkin.idpart<<endl;
	  cout<<" DEBUG::part0->i_accept = "<<part0->i_accept<<endl;
	  cout<<" DEBUG::part1->i_accept = "<<part1->i_accept<<endl;
#endif
	  icut = part0->i_accept;
	  if( part0->b_fkin && part1->b_fkin &&
	      part0->fkin.idpart == 1 && part1->fkin.idpart == 1 &&
	      part0->i_accept >= 0 && part0->i_accept == part1->i_accept &&
	      asym < 0.8 ){
	    h_pri_accpi0_cut[icut]->Fill(pt_pi0);
	    if( part0->b_dEmcGeaTrackCluster && part1->b_dEmcGeaTrackCluster ){
	      h_emcv0_accpi0_cut[icut]->Fill(pt_pi0);
	    }
	    // --- Investigate all combination of Clusters.
	    int n0,n1;
	    // --- From 2nd order clusters. 
	    TObjArray ar2 = recpri->part->getarray_dEmcGeaClusterTrack(2);
	    n0 = ar2.GetEntries();
	    while( n0-- ){
	      n1 = n0;
	      while( n1-- ){
		CL_dEmcGeaClusterTrack* g0 = (CL_dEmcGeaClusterTrack*)ar2[n0];
		CL_dEmcGeaClusterTrack* g1 = (CL_dEmcGeaClusterTrack*)ar2[n1];
		float len0 = sqrt(g0->measxyz[0]*g0->measxyz[0] + g0->measxyz[1]*g0->measxyz[1] + g0->measxyz[2]*g0->measxyz[2]);
		float len1 = sqrt(g1->measxyz[0]*g1->measxyz[0] + g1->measxyz[1]*g1->measxyz[1] + g1->measxyz[2]*g1->measxyz[2]);
		float cosine = g0->measxyz[0]*g1->measxyz[0] + g0->measxyz[1]*g1->measxyz[1] + g0->measxyz[2]*g1->measxyz[2];
		cosine /= len0 * len1;
		float mass = sqrt( 2.0 * g0.ecore * g1.ecore * ( 1.0 - cosine ) );
		float px = g0->ecore * g0->measxyz[0]/len0 + g1->ecore * g1->measxyz[0]/len1;
		float py = g0->ecore * g0->measxyz[1]/len0 + g1->ecore * g1->measxyz[1]/len1;
		float pt = sqrt( px*px + py*py );
		h_emcpt2_accpi0_cut[icut]->Fill(pt);
		h_emcm2_accpi0_cut[icut]->Fill(mass);
	      }
	    }
	    // --- From all order clusters. 
	    TObjArray ar = recpri->part->getarray_dEmcGeaClusterTrack();
	    n0 = ar.GetEntries();
	    while( n0-- ){
	      n1 = n0;
	      while( n1-- ){
		CL_dEmcGeaClusterTrack* g0 = (CL_dEmcGeaClusterTrack*)ar[n0];
		CL_dEmcGeaClusterTrack* g1 = (CL_dEmcGeaClusterTrack*)ar[n1];
		float len0 = sqrt(g0->measxyz[0]*g0->measxyz[0] + g0->measxyz[1]*g0->measxyz[1] + g0->measxyz[2]*g0->measxyz[2]);
		float len1 = sqrt(g1->measxyz[0]*g1->measxyz[0] + g1->measxyz[1]*g1->measxyz[1] + g1->measxyz[2]*g1->measxyz[2]);
		float cosine = g0->measxyz[0]*g1->measxyz[0] + g0->measxyz[1]*g1->measxyz[1] + g0->measxyz[2]*g1->measxyz[2];
		cosine /= len0 * len1;
		float mass = sqrt( 2.0 * g0.ecore * g1.ecore * ( 1.0 - cosine ) );
		float px = g0->ecore * g0->measxyz[0]/len0 + g1->ecore * g1->measxyz[0]/len1;
		float py = g0->ecore * g0->measxyz[1]/len0 + g1->ecore * g1->measxyz[1]/len1;
		float pt = sqrt( px*px + py*py );
		h_emcpt0_accpi0_cut[icut]->Fill(pt);
		h_emcm0_accpi0_cut[icut]->Fill(mass);
		h2_emcmpt0_accpi0_cut[icut]->Fill(mass,pt);
	      }
	    }
	    // --- End of "Investigate.."
	  }
	}
      }
    } // --- End of looping of pripi0....
    
    // ========== Printout CL_event
    if( _debug ){
      //      cout<<" DEBUG:: ref_primary.GetEntries() = "<<ref_primary.GetEntries()<<endl;
      //      cout<<" DEBUG:: ref_fkin.GetEntries() = "<<ref_fkin.GetEntries()<<endl;
      //      cout<<" DEBUG:: ref_dEmcGeaTrackCluster.GetEntries() = "<<ref_dEmcGeaTrackCluster.GetEntries()<<endl;
      //      cout<<" DEBUG:: ref_recpri.GetEntries() = "<<ref_recpri.GetEntries()<<endl;
      //      cout<<" DEBUG:: ref_part.GetEntries() = "<<ref_part.GetEntries()<<endl;
      //pt_event->Print(" DEBUG::");
    }
    // ========== Printout all primary/fkin
    if( _debug ){
      cout<<" --------------------------------------------------------------------------- "<<endl;
      cout<<" Event # : "<<i<<" / "<<nentries<<" v=("<<pt_header->vertex[0]<<","<<pt_header->vertex[1]<<","<<pt_header->vertex[2]<<") dr:"<<v_dr<<endl;
      nn = nn_primary;
      while( nn-- ){
	CL_primary* pr = (CL_primary*) ref_primary->At(nn);
	pr->Print();
      }
      nn = nn_fkin;
      while( nn-- ){
	CL_fkin* fkin = (CL_fkin*)ref_fkin.At(nn);
	fkin->Print();
      }
      nn = nn_dEmcGeaTrackCluster;
      while( nn-- ){
	CL_dEmcGeaTrackCluster* gea = (CL_dEmcGeaTrackCluster*) ref_dEmcGeaTrackCluster->At(nn);
	gea->Print();
      }
      cout<<" --------------------------------------------------------------------------- "<<endl;
    }
  } // --- End of event loop....
  //================================================================================ Finalization
  cout<<" nn_tmp0 = "<<nn_tmp0<<endl;
  cout<<" nn_tmp1 = "<<nn_tmp1<<endl;
  cout<<" nn_tmp2 = "<<nn_tmp2<<endl;

  pt_event->Reset();
  pt_event->Print();
  delete pt_event;
  
  cout<<" Close file : "<<f_out->GetName()<<endl;
  f_out->cd();
  f_out->Write();
  f_out->Close();
  
};
//================================================================================

