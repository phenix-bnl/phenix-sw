//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Tue May 29 05:18:59 2001 by ROOT version2.25/03)
//   from TTree trktree/TOF Track Tree
//   found on file: toftree.root
//////////////////////////////////////////////////////////


#ifndef TofTrkTree_h
#define TofTrkTree_h

#include <TChain.h>
#include <TFile.h>

class TofTrkTree {
   public :
   TTree          *fChain;   //pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //current Tree number in a TChain
//Declaration of leaves types
   Int_t           run;
   Int_t           event;
   Double_t        bfield;
   Int_t           scaledtrig;
   Int_t           rawtrig;
   Int_t           livetrig;
   Float_t         t0bbc;
   Float_t         t0bbc_err;
   Float_t         z0bbc;
   Float_t         z0bbc_err;
   Short_t         bbcNhitPmt[2];
   Float_t         bbcChargeSum[2];
   Float_t         z0zdc;
   Float_t         z0zdc_err;
   Float_t         zdcEnergy[2];
   Float_t         zdcTiming[2];
   Int_t           tofNhits;
   Int_t           tofNhitsMip;
   Int_t           tofNhitsQ10;
   Int_t           tofNhitsQ150;
   Int_t           dchNhits;
   Int_t           dchNhitsW;
   Int_t           dchNhitsE;
   Int_t           dchNtracks;
   Int_t           dchNtracksW;
   Int_t           dchNtracksE;
   Int_t           pc1Nhits;
   Int_t           pc1NhitsW;
   Int_t           pc1NhitsE;
   Int_t           pc3Nhits;
   Int_t           tecNtracks;
   Int_t           cglNtracks;
   Int_t           cglNparticle;
   Int_t           crkNhits;
   Int_t           emcNhits;
   Int_t           emcNhitsW;
   Int_t           emcNhitsE;
   Float_t         emcEtot;
   Float_t         emcEtotW;
   Float_t         emcEtotE;
  // UInt_t          fUniqueID;
  // UInt_t          fBits;
   Short_t         slatid;
   Float_t         tof;
   Float_t         tof_err;
   Float_t         eloss;
   Float_t         eloss_err;
   Float_t         xtof[3];
   Float_t         xtof_err[3];
   Short_t         qvc[2];
   Short_t         tvc[2];
   Float_t         timing[2];
   Float_t         slewing[2];
   Float_t         slatpos[3];
   Float_t         linepath;
   Float_t         flightpath;
   Float_t         flighttime;
   Float_t         mass2;
  // UInt_t          fUniqueID;
  // UInt_t          fBits;
   Float_t         pxyz[3];
   Float_t         ptot;
   Float_t         pt;
   Float_t         z0cgl;
   Float_t         tofpathlength;
   Float_t         phi;
   Float_t         theta;
   Short_t         charge;
   Float_t         cgl_quality;
   Float_t         tofxyz[3];
   Float_t         tofpro[3];
   Float_t         pc1pro[3];
   Float_t         pc3pro[3];
   Float_t         dchxyz[3];
   Float_t         dchvector[3];
   Short_t         dch_quality;
   Float_t         dch_phi;
   Float_t         dch_alpha;
   Float_t         dch_beta;
   Float_t         dch_phi0;
   Float_t         dch_theta0;
   Float_t         dch_ptot;
   Float_t         pc1xyz[3];
   Float_t         pc3xyz[3];
   Float_t         tecxyzin[3];
   Float_t         tecxyzout[3];
   Float_t         tec_quality;
   Short_t         tec_nhits;
  // UInt_t          fUniqueID;
  // UInt_t          fBits;

//List of branches
   TBranch        *b_Event;
   TBranch        *b_run;
   TBranch        *b_event;
   TBranch        *b_bfield;
   TBranch        *b_scaledtrig;
   TBranch        *b_rawtrig;
   TBranch        *b_livetrig;
   TBranch        *b_t0bbc;
   TBranch        *b_t0bbc_err;
   TBranch        *b_z0bbc;
   TBranch        *b_z0bbc_err;
   TBranch        *b_bbcNhitPmt;
   TBranch        *b_bbcChargeSum;
   TBranch        *b_z0zdc;
   TBranch        *b_z0zdc_err;
   TBranch        *b_zdcEnergy;
   TBranch        *b_zdcTiming;
   TBranch        *b_tofNhits;
   TBranch        *b_tofNhitsMip;
   TBranch        *b_tofNhitsQ10;
   TBranch        *b_tofNhitsQ150;
   TBranch        *b_dchNhits;
   TBranch        *b_dchNhitsW;
   TBranch        *b_dchNhitsE;
   TBranch        *b_dchNtracks;
   TBranch        *b_dchNtracksW;
   TBranch        *b_dchNtracksE;
   TBranch        *b_pc1Nhits;
   TBranch        *b_pc1NhitsW;
   TBranch        *b_pc1NhitsE;
   TBranch        *b_pc3Nhits;
   TBranch        *b_tecNtracks;
   TBranch        *b_cglNtracks;
   TBranch        *b_cglNparticle;
   TBranch        *b_crkNhits;
   TBranch        *b_emcNhits;
   TBranch        *b_emcNhitsW;
   TBranch        *b_emcNhitsE;
   TBranch        *b_emcEtot;
   TBranch        *b_emcEtotW;
   TBranch        *b_emcEtotE;
  // TBranch        *b_fUniqueID;
  // TBranch        *b_fBits;
   TBranch        *b_TofRec;
   TBranch        *b_slatid;
   TBranch        *b_tof;
   TBranch        *b_tof_err;
   TBranch        *b_eloss;
   TBranch        *b_eloss_err;
   TBranch        *b_xtof;
   TBranch        *b_xtof_err;
   TBranch        *b_qvc;
   TBranch        *b_tvc;
   TBranch        *b_timing;
   TBranch        *b_slewing;
   TBranch        *b_slatpos;
   TBranch        *b_linepath;
   TBranch        *b_flightpath;
   TBranch        *b_flighttime;
   TBranch        *b_mass2;
  // TBranch        *b_fUniqueID;
  // TBranch        *b_fBits;
   TBranch        *b_TofTrk;
   TBranch        *b_pxyz;
   TBranch        *b_ptot;
   TBranch        *b_pt;
   TBranch        *b_z0cgl;
   TBranch        *b_tofpathlength;
   TBranch        *b_phi;
   TBranch        *b_theta;
   TBranch        *b_charge;
   TBranch        *b_cgl_quality;
   TBranch        *b_tofxyz;
   TBranch        *b_tofpro;
   TBranch        *b_pc1pro;
   TBranch        *b_pc3pro;
   TBranch        *b_dchxyz;
   TBranch        *b_dchvector;
   TBranch        *b_dch_quality;
   TBranch        *b_dch_phi;
   TBranch        *b_dch_alpha;
   TBranch        *b_dch_beta;
   TBranch        *b_dch_phi0;
   TBranch        *b_dch_theta0;
   TBranch        *b_dch_ptot;
   TBranch        *b_pc1xyz;
   TBranch        *b_pc3xyz;
   TBranch        *b_tecxyzin;
   TBranch        *b_tecxyzout;
   TBranch        *b_tec_quality;
   TBranch        *b_tec_nhits;
  // TBranch        *b_fUniqueID;
  // TBranch        *b_fBits;

   TofTrkTree(TTree *tree=0);
   ~TofTrkTree();
   Int_t  Cut(Int_t entry);
   Int_t  GetEntry(Int_t entry);
   Int_t  LoadTree(Int_t entry);
   void   Init(TTree *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);
};

#endif

#ifdef TofTrkTree_cxx
TofTrkTree::TofTrkTree(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
     // TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("toftree.root");
     // if (!f) {
     //    f = new TFile("toftree.root");
     // }
     // tree = (TTree*)gDirectory->Get("trktree");
     printf("Missing Tree \n");
   }
   //Init(tree);
}

TofTrkTree::~TofTrkTree()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t TofTrkTree::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Int_t TofTrkTree::LoadTree(Int_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Int_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->IsA() != TChain::Class()) return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void TofTrkTree::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fChain    = tree;
   fCurrent = -1;

   fChain->SetBranchAddress("Event",(void*)-1);
   fChain->SetBranchAddress("run",&run);
   fChain->SetBranchAddress("event",&event);
   fChain->SetBranchAddress("bfield",&bfield);
   fChain->SetBranchAddress("scaledtrig",&scaledtrig);
   fChain->SetBranchAddress("rawtrig",&rawtrig);
   fChain->SetBranchAddress("livetrig",&livetrig);
   fChain->SetBranchAddress("t0bbc",&t0bbc);
   fChain->SetBranchAddress("t0bbc_err",&t0bbc_err);
   fChain->SetBranchAddress("z0bbc",&z0bbc);
   fChain->SetBranchAddress("z0bbc_err",&z0bbc_err);
   fChain->SetBranchAddress("bbcNhitPmt[2]",bbcNhitPmt);
   fChain->SetBranchAddress("bbcChargeSum[2]",bbcChargeSum);
   fChain->SetBranchAddress("z0zdc",&z0zdc);
   fChain->SetBranchAddress("z0zdc_err",&z0zdc_err);
   fChain->SetBranchAddress("zdcEnergy[2]",zdcEnergy);
   fChain->SetBranchAddress("zdcTiming[2]",zdcTiming);
   fChain->SetBranchAddress("tofNhits",&tofNhits);
   fChain->SetBranchAddress("tofNhitsMip",&tofNhitsMip);
   fChain->SetBranchAddress("tofNhitsQ10",&tofNhitsQ10);
   fChain->SetBranchAddress("tofNhitsQ150",&tofNhitsQ150);
   fChain->SetBranchAddress("dchNhits",&dchNhits);
   fChain->SetBranchAddress("dchNhitsW",&dchNhitsW);
   fChain->SetBranchAddress("dchNhitsE",&dchNhitsE);
   fChain->SetBranchAddress("dchNtracks",&dchNtracks);
   fChain->SetBranchAddress("dchNtracksW",&dchNtracksW);
   fChain->SetBranchAddress("dchNtracksE",&dchNtracksE);
   fChain->SetBranchAddress("pc1Nhits",&pc1Nhits);
   fChain->SetBranchAddress("pc1NhitsW",&pc1NhitsW);
   fChain->SetBranchAddress("pc1NhitsE",&pc1NhitsE);
   fChain->SetBranchAddress("pc3Nhits",&pc3Nhits);
   fChain->SetBranchAddress("tecNtracks",&tecNtracks);
   fChain->SetBranchAddress("cglNtracks",&cglNtracks);
   fChain->SetBranchAddress("cglNparticle",&cglNparticle);
   fChain->SetBranchAddress("crkNhits",&crkNhits);
   fChain->SetBranchAddress("emcNhits",&emcNhits);
   fChain->SetBranchAddress("emcNhitsW",&emcNhitsW);
   fChain->SetBranchAddress("emcNhitsE",&emcNhitsE);
   fChain->SetBranchAddress("emcEtot",&emcEtot);
   fChain->SetBranchAddress("emcEtotW",&emcEtotW);
   fChain->SetBranchAddress("emcEtotE",&emcEtotE);
   //fChain->SetBranchAddress("fUniqueID",&fUniqueID);
   //fChain->SetBranchAddress("fBits",&fBits);
   fChain->SetBranchAddress("TofRec",(void*)-1);
   fChain->SetBranchAddress("slatid",&slatid);
   fChain->SetBranchAddress("tof",&tof);
   fChain->SetBranchAddress("tof_err",&tof_err);
   fChain->SetBranchAddress("eloss",&eloss);
   fChain->SetBranchAddress("eloss_err",&eloss_err);
   fChain->SetBranchAddress("xtof[3]",xtof);
   fChain->SetBranchAddress("xtof_err[3]",xtof_err);
   fChain->SetBranchAddress("qvc[2]",qvc);
   fChain->SetBranchAddress("tvc[2]",tvc);
   fChain->SetBranchAddress("timing[2]",timing);
   fChain->SetBranchAddress("slewing[2]",slewing);
   fChain->SetBranchAddress("slatpos[3]",slatpos);
   fChain->SetBranchAddress("linepath",&linepath);
   fChain->SetBranchAddress("flightpath",&flightpath);
   fChain->SetBranchAddress("flighttime",&flighttime);
   fChain->SetBranchAddress("mass2",&mass2);
   //fChain->SetBranchAddress("fUniqueID",&fUniqueID);
   //fChain->SetBranchAddress("fBits",&fBits);
   fChain->SetBranchAddress("TofTrk",(void*)-1);
   fChain->SetBranchAddress("pxyz[3]",pxyz);
   fChain->SetBranchAddress("ptot",&ptot);
   fChain->SetBranchAddress("pt",&pt);
   fChain->SetBranchAddress("z0cgl",&z0cgl);
   fChain->SetBranchAddress("tofpathlength",&tofpathlength);
   fChain->SetBranchAddress("phi",&phi);
   fChain->SetBranchAddress("theta",&theta);
   fChain->SetBranchAddress("charge",&charge);
   fChain->SetBranchAddress("cgl_quality",&cgl_quality);
   fChain->SetBranchAddress("tofxyz[3]",tofxyz);
   fChain->SetBranchAddress("tofpro[3]",tofpro);
   fChain->SetBranchAddress("pc1pro[3]",pc1pro);
   fChain->SetBranchAddress("pc3pro[3]",pc3pro);
   fChain->SetBranchAddress("dchxyz[3]",dchxyz);
   fChain->SetBranchAddress("dchvector[3]",dchvector);
   fChain->SetBranchAddress("dch_quality",&dch_quality);
   fChain->SetBranchAddress("dch_phi",&dch_phi);
   fChain->SetBranchAddress("dch_alpha",&dch_alpha);
   fChain->SetBranchAddress("dch_beta",&dch_beta);
   fChain->SetBranchAddress("dch_phi0",&dch_phi0);
   fChain->SetBranchAddress("dch_theta0",&dch_theta0);
   fChain->SetBranchAddress("dch_ptot",&dch_ptot);
   fChain->SetBranchAddress("pc1xyz[3]",pc1xyz);
   fChain->SetBranchAddress("pc3xyz[3]",pc3xyz);
   fChain->SetBranchAddress("tecxyzin[3]",tecxyzin);
   fChain->SetBranchAddress("tecxyzout[3]",tecxyzout);
   fChain->SetBranchAddress("tec_quality",&tec_quality);
   fChain->SetBranchAddress("tec_nhits",&tec_nhits);
   //fChain->SetBranchAddress("fUniqueID",&fUniqueID);
   //fChain->SetBranchAddress("fBits",&fBits);
}

Bool_t TofTrkTree::Notify()
{
//   called when loading a new file
//   get branch pointers
   b_Event = fChain->GetBranch("Event");
   b_run = fChain->GetBranch("run");
   b_event = fChain->GetBranch("event");
   b_bfield = fChain->GetBranch("bfield");
   b_scaledtrig = fChain->GetBranch("scaledtrig");
   b_rawtrig = fChain->GetBranch("rawtrig");
   b_livetrig = fChain->GetBranch("livetrig");
   b_t0bbc = fChain->GetBranch("t0bbc");
   b_t0bbc_err = fChain->GetBranch("t0bbc_err");
   b_z0bbc = fChain->GetBranch("z0bbc");
   b_z0bbc_err = fChain->GetBranch("z0bbc_err");
   b_bbcNhitPmt = fChain->GetBranch("bbcNhitPmt[2]");
   b_bbcChargeSum = fChain->GetBranch("bbcChargeSum[2]");
   b_z0zdc = fChain->GetBranch("z0zdc");
   b_z0zdc_err = fChain->GetBranch("z0zdc_err");
   b_zdcEnergy = fChain->GetBranch("zdcEnergy[2]");
   b_zdcTiming = fChain->GetBranch("zdcTiming[2]");
   b_tofNhits = fChain->GetBranch("tofNhits");
   b_tofNhitsMip = fChain->GetBranch("tofNhitsMip");
   b_tofNhitsQ10 = fChain->GetBranch("tofNhitsQ10");
   b_tofNhitsQ150 = fChain->GetBranch("tofNhitsQ150");
   b_dchNhits = fChain->GetBranch("dchNhits");
   b_dchNhitsW = fChain->GetBranch("dchNhitsW");
   b_dchNhitsE = fChain->GetBranch("dchNhitsE");
   b_dchNtracks = fChain->GetBranch("dchNtracks");
   b_dchNtracksW = fChain->GetBranch("dchNtracksW");
   b_dchNtracksE = fChain->GetBranch("dchNtracksE");
   b_pc1Nhits = fChain->GetBranch("pc1Nhits");
   b_pc1NhitsW = fChain->GetBranch("pc1NhitsW");
   b_pc1NhitsE = fChain->GetBranch("pc1NhitsE");
   b_pc3Nhits = fChain->GetBranch("pc3Nhits");
   b_tecNtracks = fChain->GetBranch("tecNtracks");
   b_cglNtracks = fChain->GetBranch("cglNtracks");
   b_cglNparticle = fChain->GetBranch("cglNparticle");
   b_crkNhits = fChain->GetBranch("crkNhits");
   b_emcNhits = fChain->GetBranch("emcNhits");
   b_emcNhitsW = fChain->GetBranch("emcNhitsW");
   b_emcNhitsE = fChain->GetBranch("emcNhitsE");
   b_emcEtot = fChain->GetBranch("emcEtot");
   b_emcEtotW = fChain->GetBranch("emcEtotW");
   b_emcEtotE = fChain->GetBranch("emcEtotE");
   //b_fUniqueID = fChain->GetBranch("fUniqueID");
   //b_fBits = fChain->GetBranch("fBits");
   b_TofRec = fChain->GetBranch("TofRec");
   b_slatid = fChain->GetBranch("slatid");
   b_tof = fChain->GetBranch("tof");
   b_tof_err = fChain->GetBranch("tof_err");
   b_eloss = fChain->GetBranch("eloss");
   b_eloss_err = fChain->GetBranch("eloss_err");
   b_xtof = fChain->GetBranch("xtof[3]");
   b_xtof_err = fChain->GetBranch("xtof_err[3]");
   b_qvc = fChain->GetBranch("qvc[2]");
   b_tvc = fChain->GetBranch("tvc[2]");
   b_timing = fChain->GetBranch("timing[2]");
   b_slewing = fChain->GetBranch("slewing[2]");
   b_slatpos = fChain->GetBranch("slatpos[3]");
   b_linepath = fChain->GetBranch("linepath");
   b_flightpath = fChain->GetBranch("flightpath");
   b_flighttime = fChain->GetBranch("flighttime");
   b_mass2 = fChain->GetBranch("mass2");
   //b_fUniqueID = fChain->GetBranch("fUniqueID");
   //b_fBits = fChain->GetBranch("fBits");
   b_TofTrk = fChain->GetBranch("TofTrk");
   b_pxyz = fChain->GetBranch("pxyz[3]");
   b_ptot = fChain->GetBranch("ptot");
   b_pt = fChain->GetBranch("pt");
   b_z0cgl = fChain->GetBranch("z0cgl");
   b_tofpathlength = fChain->GetBranch("tofpathlength");
   b_phi = fChain->GetBranch("phi");
   b_theta = fChain->GetBranch("theta");
   b_charge = fChain->GetBranch("charge");
   b_cgl_quality = fChain->GetBranch("cgl_quality");
   b_tofxyz = fChain->GetBranch("tofxyz[3]");
   b_tofpro = fChain->GetBranch("tofpro[3]");
   b_pc1pro = fChain->GetBranch("pc1pro[3]");
   b_pc3pro = fChain->GetBranch("pc3pro[3]");
   b_dchxyz = fChain->GetBranch("dchxyz[3]");
   b_dchvector = fChain->GetBranch("dchvector[3]");
   b_dch_quality = fChain->GetBranch("dch_quality");
   b_dch_phi = fChain->GetBranch("dch_phi");
   b_dch_alpha = fChain->GetBranch("dch_alpha");
   b_dch_beta = fChain->GetBranch("dch_beta");
   b_dch_phi0 = fChain->GetBranch("dch_phi0");
   b_dch_theta0 = fChain->GetBranch("dch_theta0");
   b_dch_ptot = fChain->GetBranch("dch_ptot");
   b_pc1xyz = fChain->GetBranch("pc1xyz[3]");
   b_pc3xyz = fChain->GetBranch("pc3xyz[3]");
   b_tecxyzin = fChain->GetBranch("tecxyzin[3]");
   b_tecxyzout = fChain->GetBranch("tecxyzout[3]");
   b_tec_quality = fChain->GetBranch("tec_quality");
   b_tec_nhits = fChain->GetBranch("tec_nhits");
   //b_fUniqueID = fChain->GetBranch("fUniqueID");
   //b_fBits = fChain->GetBranch("fBits");
   return kTRUE;
}

void TofTrkTree::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t TofTrkTree::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef TofTrkTree_cxx

