#ifdef COMPILE
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <iostream.h>
#include <math.h>
#include <TROOT.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TNtuple.h>
#include <TH1.h>
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TPostScript.h>
#include <TGraphErrors.h>
#include <TLine.h>
#include <TF1.h>
#include <TEnv.h>
#include <TSystem.h>
//#include <Rtypes.h>
//TString st = gEnv->GetValue("Unix.*.Root.MacroPath","aho");
//gEnv->SetValue("Unix.*.Root.MacroPath",st+":$(OFFLINE_MAIN)/include");
#endif

//==========================================================================
class CL_header : public TObject {
public:
   int run;
   int event;
   int multiplicity;
   float b;
   int a1;
   int z1;
   int a2;
   int z2;
   float sqrt_s;
   float bmin;
   float bmax;
   float t0femto;
   float vertex[3];
   int itime;
   int idate;
   int nrndm[2];
   int isqStart;
   int iSeconds;
   int maxTrueTrack;
  CL_header(){ };
};
//==========================================================================
class CL_pythia : public TObject {
public:
   int pyth_proc_id;
   float pyth_bjork[2];
   float pyth_parstu[3];
   float pyth_qsqr;
   float pyth_ptrans;
   int intr_part_id[4];
   float intr_part_p[4][4];
  CL_pythia(){ };
};
//==========================================================================
class CL_primary : public TObject {
public:
   int key;
   int event_track;
   int subevent_track;
   int true_track;
   int subevent;
   int idpart;
   int nfile;
   float px_momentum;
   float py_momentum;
   float pz_momentum;
  CL_primary(){ };
};
//==========================================================================
class CL_fkin : public TObject {
public:
   int true_track;
   int subevent;
   int ntrack;
   float ptot;
   float pthet;
   float pphi;
   float r_vertex;
   float z_vertex;
   float th_vertx;
   float ph_vertx;
   int itparent;
   int idparent;
   int idpart;
   int nfile;
  CL_fkin(){ };
};
//==========================================================================
class CL_dEmcGeaClusterTrack : public TObject {
public:
   int id;
   int clusid;
   int evno;
   int keycent;
   int input;
   int type;
   int arm;
   int sector;
   int trkno[3];
   int tracktwrhit[3];
   float edep_nom[3];
   float pid[3];
   float ptot[3];
   float vertex[3][3];
   float ancestry[3];
   float xyz[3][3];
   float edep[3];
   float efrac[3];
   float measxyz[3];
   float mease;
   float ecore;
   float tof;
   float etof;
   float tofmin;
   float etofmin;
   float tofmax;
   float etofmax;
   int twrhit;
   float disp[2];
   float padisp[2];
   float partesum[8];
   int charged;
   float pc3proj[3];
   float chi2_sh;
   float prob_photon_sh;
   float e_sh[2];
   float chglist[8];
  CL_dEmcGeaClusterTrack() { };
};
//==========================================================================
class CL_dEmcGeaTrackCluster : public TObject {
public:
   int id;
   int trkno;
   int track_ptr;
   int input;
   int clusid[3];
   float pid;
   float ptot;
   float nom_edep;
   float edep[3];
   float efrac[3];
  CL_dEmcGeaTrackCluster() { };
};
//==========================================================================
class CL_dEmcGeaTrack : public TObject {
public:
   int id;
   int trkno;
   int input;
   int anclvl;
   int pid;
   float ekin;
   float xyz[3];
   float ptot;
   float pxyz[3];
   float impxyz[3];
   int itparent;
   int idparent;
   int parent_ptr;
   int twrhit;
   float edep;
  CL_dEmcGeaTrack(){ };
};
//==========================================================================
 
void simDSTtoNtuple(char* infile, char* outfile){
  //------------------------------------------------------------------------------- Initialization
  //gSystem->Load("libgea.so");
  gSystem->Load("libudst.so");
  TFile *f = new TFile(infile);
  if (!f){
    cout<<" Can't open file : "<<infile<<endl;
    return;
  }
  TTree* T = (TTree*) f->Get("T");
  int nentries = T->GetEntries();
  f->Close();
  int num,numx,numy,nn;
  int i,j;
  //------------------------------------------------------------------------------- Output
  TFile* nf = new TFile(outfile,"RECREATE");
  TTree* T = new TTree("T","Analysis ntuple");
  //===== DST/BbcOut
  BbcOutv1* pt_BbcOut = new BbcOutv1;
  T->Branch("BbcOutv1","BbcOutv1",&pt_BbcOut);

  //===== EVA/fkin
  TClonesArray* ar_fkin = new TClonesArray("CL_fkin",1000);
  TClonesArray& ref_fkin = *ar_fkin;
  int nn_fkin;
  T->Branch("nn_fkin",&nn_fkin,"nn_fkin/I");
  T->Branch("fkin",&ar_fkin);

  //===== EVA/primary
  TClonesArray* ar_primary = new TClonesArray("CL_primary",1000);
  TClonesArray& ref_primary = *ar_primary;
  int nn_primary;
  T->Branch("nn_primary",&nn_primary,"nn_primary/I");
  T->Branch("primary",&ar_primary);

  //===== EVA/pythia
  CL_pythia* pt_pythia = new CL_pythia;
  T->Branch("pythia","CL_pythia",&pt_pythia);

  //===== EVA/header
  CL_header* pt_header = new CL_header;
  T->Branch("header","CL_header",&pt_header);

  //===== DST/dEmcGeaClusterTrack
  TClonesArray* ar_dEmcGeaClusterTrack = new TClonesArray("CL_dEmcGeaClusterTrack",200);
  TClonesArray& ref_dEmcGeaClusterTrack = *ar_dEmcGeaClusterTrack;
  int nn_dEmcGeaClusterTrack;
  T->Branch("nn_dEmcGeaClusterTrack",&nn_dEmcGeaClusterTrack,"nn_dEmcGeaClusterTrack/I");
  T->Branch("dEmcGeaClusterTrack",&ar_dEmcGeaClusterTrack);

  //===== DST/dEmcGeaTrackCluster
  TClonesArray* ar_dEmcGeaTrackCluster = new TClonesArray("CL_dEmcGeaTrackCluster",200);
  TClonesArray& ref_dEmcGeaTrackCluster = *ar_dEmcGeaTrackCluster;
  int nn_dEmcGeaTrackCluster;
  T->Branch("nn.dEmcGeaTrackCluster",&nn_dEmcGeaTrackCluster,"nn_dEmcGeaTrackCluster/I");
  T->Branch("dEmcGeaTrackCluster",&ar_dEmcGeaTrackCluster);

  //===== DST/dEmcGeaTrack
  TClonesArray* ar_dEmcGeaTrack = new TClonesArray("CL_dEmcGeaTrack",200);
  TClonesArray& ref_dEmcGeaTrack = *ar_dEmcGeaTrack;
  int nn_dEmcGeaTrack;
  T->Branch("nn.dEmcGeaTrack",&nn_dEmcGeaTrack,"nn_dEmcGeaTrack/I");
  T->Branch("dEmcGeaTrack",&ar_dEmcGeaTrack);


  //------------------------------------------------------------------------------- Setup Node
  PHNodeIOManager* ioman = new PHNodeIOManager(infile,PHReadOnly);
  // --- Reading Eva node
  PHCompositeNode* evanode = new PHCompositeNode("EVA");
  ioman->read(evanode);
  PHNodeIterator evaIter(evanode);
  PHIODataNode<PHTable>* fkinNode = (PHIODataNode<PHTable>*)evaIter.findFirst("PHIODataNode","fkin");
  PHIODataNode<PHTable>* primaryNode = (PHIODataNode<PHTable>*)evaIter.findFirst("PHIODataNode","primary");
  PHIODataNode<PHTable>* pythiaNode = (PHIODataNode<PHTable>*)evaIter.findFirst("PHIODataNode","pythia");
  PHIODataNode<PHTable>* headerNode = (PHIODataNode<PHTable>*)evaIter.findFirst("PHIODataNode","header");
  PHIODataNode<PHTable>* dEmcGeaClusterTrackNode = (PHIODataNode<PHTable>*)evaIter.findFirst("PHIODataNode","dEmcGeaClusterTrack");
  PHIODataNode<PHTable>* dEmcGeaTrackClusterNode = (PHIODataNode<PHTable>*)evaIter.findFirst("PHIODataNode","dEmcGeaTrackCluster");
  PHIODataNode<PHTable>* dEmcGeaTrackNode = (PHIODataNode<PHTable>*)evaIter.findFirst("PHIODataNode","dEmcGeaTrack");
  if(!pythiaNode ){cout<<" Can't read pythia node. Skip pythia node.. "<<endl; }
  if(!fkinNode ){cout<<" Can't read fkin node "<<endl; return;}
  if(!primaryNode ){cout<<" Can't read fprimary node "<<endl; return;}
  if(!headerNode ){cout<<" Can't read headr node "<<endl; return;}
  if(!dEmcGeaClusterTrackNode ){cout<<" Can't read dEmcGeaClusterTrack node "<<endl; return;}
  if(!dEmcGeaTrackClusterNode ){cout<<" Can't read dEmcGeaTrackCluster node "<<endl; return;}
  if(!dEmcGeaTrackNode ){cout<<" Can't read dEmcGeaTrack node "<<endl; return;}
  // --- Reading Dst node
  PHCompositeNode* dstnode = new PHCompositeNode("DST");
  ioman->read(dstnode);
  PHNodeIterator dstIter(evanode);
  PHIODataNode<PHTable>* BbcOutNode = (PHIODataNode<PHTable>*)dstIter.findFirst("PHIODataNode","BbcOut");
  if(! BbcOutNode ){cout<<" Can't read BbcOut node "<<endl; return;} 
  //
  //------------------------------------------------------------------------------- Start loop....
  int nentry = 0;
  while( ioman->read(++nentry) ){
    fkinWrapper *fkin = (fkinWrapper*) fkinNode->getData();
    primaryWrapper *primary = (primaryWrapper*) primaryNode->getData();
    pythiaWrapper *pythia = (pythiaWrapper*) pythiaNode->getData();
    headerWrapper *header = (headerWrapper*) headerNode->getData();
    BbcOut *bbcout = (BbcOut*) BbcOutNode->getData();
    dEmcGeaClusterTrackWrapper *demcgeaclustertrack = (dEmcGeaClusterTrackWrapper*) dEmcGeaClusterTrackNode->getData();
    dEmcGeaTrackClusterWrapper *demcgeatrackcluster = (dEmcGeaTrackClusterWrapper*) dEmcGeaTrackClusterNode->getData();
    dEmcGeaTrackWrapper *demcgeatrack = (dEmcGeaTrackWrapper*) dEmcGeaTrackNode->getData();
    if( nentry % 10 == 0 ){
      cout<<" Analyzing .. "<<nentry<<" / "<<nentries<<endl;
      if( pythia )
	cout<<"         --  pythia->RowCount() = "<<pythia->RowCount()<<endl;
      cout<<"         --  fkin->RowCount() = "<<fkin->RowCount()<<endl;
      cout<<"         --  primary->RowCount() = "<<primary->RowCount()<<endl;
      cout<<"         --  header->RowCount() = "<<header->RowCount()<<endl;
      cout<<"         --  demcgeaclustertrack->RowCount() = "<<demcgeaclustertrack->RowCount()<<endl;
      cout<<"         --  demcgeatrackcluster->RowCount() = "<<demcgeatrackcluster->RowCount()<<endl;
      cout<<"         --  demcgeatrack->RowCount() = "<<demcgeatrack->RowCount()<<endl;
    }
    //------------------------------------------------------------------------------- Filling into contents.
    //===== DST/dEmcGeaTrack
    ref_dEmcGeaTrack->RemoveAll();
    num = demcgeatrack->RowCount();
    nn_dEmcGeaTrack = num;
    for( nn = 0 ; nn < num; nn++ ){
      CL_dEmcGeaTrack* emcg = new (ref_dEmcGeaTrack[nn]) CL_dEmcGeaTrack;
      emcg->id = demcgeatrack->get_id(nn);
      emcg->trkno = demcgeatrack->get_trkno(nn);
      emcg->input = demcgeatrack->get_input(nn);
      emcg->anclvl = demcgeatrack->get_anclvl(nn);
      emcg->pid = demcgeatrack->get_pid(nn);
      emcg->ekin = demcgeatrack->get_ekin(nn);
      emcg->ptot = demcgeatrack->get_ptot(nn);
      emcg->itparent = demcgeatrack->get_itparent(nn);
      emcg->idparent = demcgeatrack->get_idparent(nn);
      emcg->parent_ptr = demcgeatrack->get_parent_ptr(nn);
      emcg->twrhit = demcgeatrack->get_twrhit(nn);
      emcg->edep = demcgeatrack->get_edep(nn);
      i = 3;
      while(i--){
	emcg->xyz[i] = demcgeatrack->get_xyz(i,nn);
	emcg->pxyz[i] = demcgeatrack->get_pxyz(i,nn);
	emcg->impxyz[i] = demcgeatrack->get_impxyz(i,nn);
      }
    }

    //===== DST/dEmcGeaTrackCluster
    ref_dEmcGeaTrackCluster->RemoveAll();
    num = demcgeatrackcluster->RowCount();
    nn_dEmcGeaTrackCluster = num;
    for( nn = 0 ; nn < num; nn++ ){
      CL_dEmcGeaTrackCluster* emcgtr = new (ref_dEmcGeaTrackCluster[nn]) CL_dEmcGeaTrackCluster;
      emcgtr->id = demcgeatrackcluster->get_id(nn);
      emcgtr->trkno = demcgeatrackcluster->get_trkno(nn);
      emcgtr->track_ptr = demcgeatrackcluster->get_track_ptr(nn);
      emcgtr->input = demcgeatrackcluster->get_input(nn);
      emcgtr->pid = demcgeatrackcluster->get_pid(nn);
      emcgtr->ptot = demcgeatrackcluster->get_ptot(nn);
      emcgtr->nom_edep = demcgeatrackcluster->get_nom_edep(nn);
      i = 3;
      while(i--){
	emcgtr->clusid[i] = demcgeatrackcluster->get_clusid(i,nn);
	emcgtr->edep[i] = demcgeatrackcluster->get_edep(i,nn);
	emcgtr->efrac[i] = demcgeatrackcluster->get_efrac(i,nn);
      }
    }

    //===== DST/dEmcGeaClusterTrack
    ref_dEmcGeaClusterTrack->RemoveAll();
    num = demcgeaclustertrack->RowCount();
    nn_dEmcGeaClusterTrack = num;
    for( nn = 0 ; nn < num; nn++ ){
      CL_dEmcGeaClusterTrack* emcgcl = new (ref_dEmcGeaClusterTrack[nn]) CL_dEmcGeaClusterTrack;
      emcgcl->id = demcgeaclustertrack->get_id(nn);
      emcgcl->clusid  = demcgeaclustertrack->get_clusid(nn);
      emcgcl->evno = demcgeaclustertrack->get_evno(nn);
      emcgcl->keycent = demcgeaclustertrack->get_keycent(nn);
      emcgcl->input = demcgeaclustertrack->get_input(nn);
      emcgcl->type = demcgeaclustertrack->get_type(nn);
      emcgcl->arm = demcgeaclustertrack->get_arm(nn);
      emcgcl->sector = demcgeaclustertrack->get_sector(nn);
      int i,j;
      i = 3;
      while(i--){
	emcgcl->trkno[i] = demcgeaclustertrack->get_trkno(i,nn);
	emcgcl->tracktwrhit[i] = demcgeaclustertrack->get_tracktwrhit(i,nn);
	emcgcl->edep_nom[i] = demcgeaclustertrack->get_edep_nom(i,nn);
	emcgcl->pid[i] = demcgeaclustertrack->get_pid(i,nn);
	emcgcl->ptot[i] = demcgeaclustertrack->get_ptot(i,nn);
	j = 3; while( j-- ) emcgcl->vertex[i][j] = demcgeaclustertrack->get_vertex(i,j,nn); 
	emcgcl->ancestry[i] = demcgeaclustertrack->get_ancestry(i,nn);
	j = 3; while( j-- ) emcgcl->xyz[i][j] = demcgeaclustertrack->get_xyz(i,j,nn);
	emcgcl->edep[i] = demcgeaclustertrack->get_edep(i,nn);
	emcgcl->efrac[i] = demcgeaclustertrack->get_efrac(i,nn);
	emcgcl->measxyz[i] = demcgeaclustertrack->get_measxyz(i,nn);
      }
      emcgcl->mease = demcgeaclustertrack->get_mease(nn);
      emcgcl->ecore = demcgeaclustertrack->get_ecore(nn);
      emcgcl->tof = demcgeaclustertrack->get_tof(nn);
      emcgcl->etof = demcgeaclustertrack->get_etof(nn);
      emcgcl->tofmin = demcgeaclustertrack->get_tofmin(nn);
      emcgcl->etofmin = demcgeaclustertrack->get_etofmin(nn);
      emcgcl->tofmax = demcgeaclustertrack->get_tofmax(nn);
      emcgcl->etofmax = demcgeaclustertrack->get_etofmax(nn);
      emcgcl->twrhit = demcgeaclustertrack->get_twrhit(nn);
      i = 2;
      while( i-- ){
	emcgcl->disp[i] = demcgeaclustertrack->get_disp(i,nn);
	emcgcl->padisp[i] = demcgeaclustertrack->get_padisp(i,nn);
      }
      i = 8; while(i-- ) emcgcl->partesum[i] = demcgeaclustertrack->get_partesum(i,nn); 
      emcgcl->charged = demcgeaclustertrack->get_charged(nn);
      i = 3; while(i--) emcgcl->pc3proj[i] = demcgeaclustertrack->get_pc3proj(i,nn);
      emcgcl->chi2_sh = demcgeaclustertrack->get_chi2_sh(nn);
      emcgcl->prob_photon_sh = demcgeaclustertrack->get_prob_photon_sh(nn);
      i = 2; while(i--) emcgcl->e_sh[i] = demcgeaclustertrack->get_e_sh(i,nn);
      i = 8; while(i--) emcgcl->chglist[i] = demcgeaclustertrack->get_chglist(i,nn);
    }

    //===== DST/BbcOut
    pt_BbcOut->set_TimeVertex(bbcout->get_TimeZero(),bbcout->get_dTimeZero(),
			      bbcout->get_VertexPoint(),bbcout->get_dVertexPoint());

    //===== EVA/fkin
    ref_fkin->RemoveAll();
    num = fkin->RowCount();
    nn_fkin = num;
    for( nn = 0 ; nn < num; nn++ ){
      CL_fkin* fk = new (ref_fkin[nn]) CL_fkin;
      fk->true_track = fkin->get_true_track(nn);
      fk->subevent = fkin->get_subevent(nn);
      fk->ntrack = fkin->get_ntrack(nn);
      fk->ptot = fkin->get_ptot(nn);
      fk->pthet = fkin->get_pthet(nn);
      fk->pphi = fkin->get_pphi(nn);
      fk->r_vertex = fkin->get_r_vertex(nn);
      fk->z_vertex = fkin->get_z_vertex(nn);
      fk->th_vertx = fkin->get_th_vertx(nn);
      fk->ph_vertx = fkin->get_ph_vertx(nn);
      fk->itparent = fkin->get_itparent(nn);
      fk->idparent = fkin->get_idparent(nn);
      fk->idpart = fkin->get_idpart(nn);
      fk->nfile = fkin->get_nfile(nn);
    }
    //===== EVA/primary
    ref_primary->RemoveAll();
    num = primary->RowCount();
    nn_primary = num;
    for( nn = 0 ; nn < num; nn++ ){
      CL_primary* pr = new (ref_primary[nn]) CL_primary;
      pr->key = primary->get_key(nn);
      pr->event_track = primary->get_event_track(nn);
      pr->subevent_track = primary->get_subevent_track(nn);
      pr->px_momentum = primary->get_px_momentum(nn);
      pr->py_momentum = primary->get_py_momentum(nn);
      pr->pz_momentum = primary->get_pz_momentum(nn);
      pr->true_track = primary->get_true_track(nn);
      pr->subevent = primary->get_subevent(nn);
      pr->idpart = primary->get_idpart(nn);
      pr->nfile = primary->get_nfile(nn);
    }
    //===== EVA/pythia
    if( pythia ){
      if( pythia->RowCount() != 1 ) cout<<" Warning:: pythia->RowCount() is not 1 "<<endl;
      pt_pythia->pyth_proc_id = pythia->get_pyth_proc_id(0);
      pt_pythia->pyth_bjork[0] = pythia->get_pyth_bjork(0,0);
      pt_pythia->pyth_bjork[1] = pythia->get_pyth_bjork(1,0);
      pt_pythia->pyth_parstu[0] = pythia->get_pyth_parstu(0,0);
      pt_pythia->pyth_parstu[1] = pythia->get_pyth_parstu(1,0);
      pt_pythia->pyth_parstu[2] = pythia->get_pyth_parstu(2,0);
      pt_pythia->pyth_qsqr = pythia->get_pyth_qsqr(0);
      pt_pythia->pyth_ptrans = pythia->get_pyth_ptrans(0);
      numx = 4;
      while( numx-- ){
	pt_pythia->intr_part_id[numx] = pythia->get_intr_part_id(numx,0);
	numy = 4;
	while( numy-- ){
	  pt_pythia->intr_part_p[numx][numy] = pythia->get_intr_part_p(numx,numy,0);
	}
      }
    }
    //===== EVA/header
    if( header->RowCount() != 1 ) cout<<" Warning:: header->RowCount() is not 1 "<<endl;
    pt_header->run = header->get_run(0);
    pt_header->event = header->get_event(0);
    pt_header->multiplicity = header->get_multiplicity(0);
    pt_header->vertex[0] = header->get_vertex(0,0);
    pt_header->vertex[1] = header->get_vertex(1,0);
    pt_header->vertex[2] = header->get_vertex(2,0);
    //===== Filling....
    T->Fill();
    //-------------------------------------------------------------------------------
  }
  cout<<" Analyzed events : "<<nentry<<endl;

  nf->cd();
  //T->Print();
  T->Write();
  nf->Close();

};

//===================================================================================================
