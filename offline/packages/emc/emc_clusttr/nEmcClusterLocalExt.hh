//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Fri May 18 17:28:13 2001 by ROOT version3.00/06)
//   from TTree nEmcClusterLocalExt/EMC ntuple
//   found on file: NTP_v04-0000012127-0024.root
//////////////////////////////////////////////////////////


#ifndef nEmcClusterLocalExt_h
#define nEmcClusterLocalExt_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include "ClustTr.hh"

class nEmcClusterLocalExt {
   public :
   ClustTr        clt;

   TTree          *fTree;   //pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //current Tree number in a TChain
//Declaration of leaves types
   Float_t         run;
   Float_t         nev;
   Float_t         trigRawLsb;
   Float_t         trigRawMsb;
   Float_t         trigScaledLsb;
   Float_t         trigScaledMsb;
   Float_t         trigLiveLsb;
   Float_t         trigLiveMsb;
   Float_t         bbc_t0;
   Float_t         bbc_vz;
   Float_t         bbc_en;
   Float_t         bbc_es;
   Float_t         ch_bbc;
   Float_t         zdc_vz;
   Float_t         zdc_e1;
   Float_t         zdc_e2;
   Float_t         e_zdc;
   Float_t         zdc_t1;
   Float_t         zdc_t2;
   Float_t         etot;
   Float_t         e;
   Float_t         tof;
   Float_t         prob;
   Float_t         x;
   Float_t         y;
   Float_t         z;
   Float_t         arm;
   Float_t         sec;
   Float_t         clusid;
   Float_t         ecore;
   Float_t         ecorr;
   Float_t         ecent;
   Float_t         twrhit;
   Float_t         tofmin;
   Float_t         tofmax;
   Float_t         tofcorr;
   Float_t         chi2;
   Float_t         disp_z;
   Float_t         disp_y;
   Float_t         cg_z;
   Float_t         cg_y;
   Float_t         ind_z;
   Float_t         ind_y;
   Float_t         partesum_1;
   Float_t         partesum_2;
   Float_t         partesum_3;
   Float_t         partesum_4;
   Float_t         nemc;
   Float_t         segno;

//List of branches
   TBranch        *b_run;
   TBranch        *b_nev;
   TBranch        *b_trigRawLsb;
   TBranch        *b_trigRawMsb;
   TBranch        *b_trigScaledLsb;
   TBranch        *b_trigScaledMsb;
   TBranch        *b_trigLiveLsb;
   TBranch        *b_trigLiveMsb;
   TBranch        *b_bbc_t0;
   TBranch        *b_bbc_vz;
   TBranch        *b_bbc_en;
   TBranch        *b_bbc_es;
   TBranch        *b_ch_bbc;
   TBranch        *b_zdc_vz;
   TBranch        *b_zdc_e1;
   TBranch        *b_zdc_e2;
   TBranch        *b_e_zdc;
   TBranch        *b_zdc_t1;
   TBranch        *b_zdc_t2;
   TBranch        *b_etot;
   TBranch        *b_e;
   TBranch        *b_tof;
   TBranch        *b_prob;
   TBranch        *b_x;
   TBranch        *b_y;
   TBranch        *b_z;
   TBranch        *b_arm;
   TBranch        *b_sec;
   TBranch        *b_clusid;
   TBranch        *b_ecore;
   TBranch        *b_ecorr;
   TBranch        *b_ecent;
   TBranch        *b_twrhit;
   TBranch        *b_tofmin;
   TBranch        *b_tofmax;
   TBranch        *b_tofcorr;
   TBranch        *b_chi2;
   TBranch        *b_disp_z;
   TBranch        *b_disp_y;
   TBranch        *b_cg_z;
   TBranch        *b_cg_y;
   TBranch        *b_ind_z;
   TBranch        *b_ind_y;
   TBranch        *b_partesum_1;
   TBranch        *b_partesum_2;
   TBranch        *b_partesum_3;
   TBranch        *b_partesum_4;
   TBranch        *b_nemc;
   TBranch        *b_segno;

   nEmcClusterLocalExt(TTree *tree=0);
   ~nEmcClusterLocalExt();
   Int_t  Cut(Int_t entry);
   Int_t  GetEntry(Int_t entry);
  Int_t GetEntries(){
    int i = (int) fTree->GetEntries();
    return i; };
   Int_t  LoadTree(Int_t entry);
   void   Init(TTree *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);
};

#endif

#ifdef nEmcClusterLocalExt_cxx
nEmcClusterLocalExt::nEmcClusterLocalExt(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("NTP_v04-0000012127-0024.root");
      if (!f) {
         f = new TFile("NTP_v04-0000012127-0024.root");
      }
      tree = (TTree*)gDirectory->Get("nEmcClusterLocalExt");

   }
   Init(tree);
}

nEmcClusterLocalExt::~nEmcClusterLocalExt()
{
   if (!fTree) return;
}

Int_t nEmcClusterLocalExt::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fTree) return 0;
   Int_t ibyte = fTree->GetEntry(entry);
   //--------------------------------------------------------
   clt.Reset();
   // Global Information
   clt.run = (int)run;
   clt.evn = (int)nev;
   clt.zdcz = zdc_vz;
   clt.zdct0 = (zdc_t1+zdc_t2)/2.;
   clt.zdcch = (zdc_e1 + zdc_e2);
   clt.bbcz = bbc_vz;
   clt.bbct0 = bbc_t0;
   clt.bbcch = bbc_en + bbc_es;
   clt.bbcnhit = ch_bbc;
   clt.pc1nhit = 0; // Dummy
   clt.emcnhit = nemc;
   //
   // EMCal Clustering information
   clt.arm = (int)arm;
   clt.sector = (int)sec;
   clt.ind[0] = (int)ind_z;
   clt.ind[1] = (int)ind_y;
   if( ind_y > 36 && arm==0 ) clt.ind[1] = (int)ind_y - 36;
   if(0){
     clt.arm = -1;
     clt.sector = 0;
     clt.ind[1] = 0;
     clt.ind[0] = 0;
   }

   clt.twrhit = (int)twrhit;
   //clt.clustno = 0; //FIX.ME!!
   clt.tof = tofmax ;
   clt.e = e;
   clt.ecent = ecent;
   clt.ecore = ecore;
   clt.ecorr = ecorr;
   clt.tofcorr = tofcorr;
   clt.chi2 = chi2; // FIX.ME!!!!!!!!
   clt.pos[0] = x;
   clt.pos[1] = y;
   clt.pos[2] = z;
   //trc.disp[],padisp[]..
   //--------------------------------------------------------
   return ibyte;
}
Int_t nEmcClusterLocalExt::LoadTree(Int_t entry)
{
// Set the environment to read one entry
   if (!fTree) return -5;
   Int_t centry = fTree->LoadTree(entry);
   if (centry < 0) return centry;
   if (fTree->IsA() != TChain::Class()) return centry;
   TChain *chain = (TChain*)fTree;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void nEmcClusterLocalExt::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fTree    = tree;
   fCurrent = -1;

   fTree->SetBranchAddress("run",&run);
   fTree->SetBranchAddress("nev",&nev);
   fTree->SetBranchAddress("trigRawLsb",&trigRawLsb);
   fTree->SetBranchAddress("trigRawMsb",&trigRawMsb);
   fTree->SetBranchAddress("trigScaledLsb",&trigScaledLsb);
   fTree->SetBranchAddress("trigScaledMsb",&trigScaledMsb);
   fTree->SetBranchAddress("trigLiveLsb",&trigLiveLsb);
   fTree->SetBranchAddress("trigLiveMsb",&trigLiveMsb);
   fTree->SetBranchAddress("bbc_t0",&bbc_t0);
   fTree->SetBranchAddress("bbc_vz",&bbc_vz);
   fTree->SetBranchAddress("bbc_en",&bbc_en);
   fTree->SetBranchAddress("bbc_es",&bbc_es);
   fTree->SetBranchAddress("ch_bbc",&ch_bbc);
   fTree->SetBranchAddress("zdc_vz",&zdc_vz);
   fTree->SetBranchAddress("zdc_e1",&zdc_e1);
   fTree->SetBranchAddress("zdc_e2",&zdc_e2);
   fTree->SetBranchAddress("e_zdc",&e_zdc);
   fTree->SetBranchAddress("zdc_t1",&zdc_t1);
   fTree->SetBranchAddress("zdc_t2",&zdc_t2);
   fTree->SetBranchAddress("etot",&etot);
   fTree->SetBranchAddress("e",&e);
   fTree->SetBranchAddress("tof",&tof);
   fTree->SetBranchAddress("prob",&prob);
   fTree->SetBranchAddress("x",&x);
   fTree->SetBranchAddress("y",&y);
   fTree->SetBranchAddress("z",&z);
   fTree->SetBranchAddress("arm",&arm);
   fTree->SetBranchAddress("sec",&sec);
   fTree->SetBranchAddress("clusid",&clusid);
   fTree->SetBranchAddress("ecore",&ecore);
   fTree->SetBranchAddress("ecorr",&ecorr);
   fTree->SetBranchAddress("ecent",&ecent);
   fTree->SetBranchAddress("twrhit",&twrhit);
   fTree->SetBranchAddress("tofmin",&tofmin);
   fTree->SetBranchAddress("tofmax",&tofmax);
   fTree->SetBranchAddress("tofcorr",&tofcorr);
   fTree->SetBranchAddress("chi2",&chi2);
   fTree->SetBranchAddress("disp_z",&disp_z);
   fTree->SetBranchAddress("disp_y",&disp_y);
   fTree->SetBranchAddress("cg_z",&cg_z);
   fTree->SetBranchAddress("cg_y",&cg_y);
   fTree->SetBranchAddress("ind_z",&ind_z);
   fTree->SetBranchAddress("ind_y",&ind_y);
   fTree->SetBranchAddress("partesum_1",&partesum_1);
   fTree->SetBranchAddress("partesum_2",&partesum_2);
   fTree->SetBranchAddress("partesum_3",&partesum_3);
   fTree->SetBranchAddress("partesum_4",&partesum_4);
   fTree->SetBranchAddress("nemc",&nemc);
   fTree->SetBranchAddress("segno",&segno);
   Notify();
}

Bool_t nEmcClusterLocalExt::Notify()
{
//   called when loading a new file
//   get branch pointers
   b_run = fTree->GetBranch("run");
   b_nev = fTree->GetBranch("nev");
   b_trigRawLsb = fTree->GetBranch("trigRawLsb");
   b_trigRawMsb = fTree->GetBranch("trigRawMsb");
   b_trigScaledLsb = fTree->GetBranch("trigScaledLsb");
   b_trigScaledMsb = fTree->GetBranch("trigScaledMsb");
   b_trigLiveLsb = fTree->GetBranch("trigLiveLsb");
   b_trigLiveMsb = fTree->GetBranch("trigLiveMsb");
   b_bbc_t0 = fTree->GetBranch("bbc_t0");
   b_bbc_vz = fTree->GetBranch("bbc_vz");
   b_bbc_en = fTree->GetBranch("bbc_en");
   b_bbc_es = fTree->GetBranch("bbc_es");
   b_ch_bbc = fTree->GetBranch("ch_bbc");
   b_zdc_vz = fTree->GetBranch("zdc_vz");
   b_zdc_e1 = fTree->GetBranch("zdc_e1");
   b_zdc_e2 = fTree->GetBranch("zdc_e2");
   b_e_zdc = fTree->GetBranch("e_zdc");
   b_zdc_t1 = fTree->GetBranch("zdc_t1");
   b_zdc_t2 = fTree->GetBranch("zdc_t2");
   b_etot = fTree->GetBranch("etot");
   b_e = fTree->GetBranch("e");
   b_tof = fTree->GetBranch("tof");
   b_prob = fTree->GetBranch("prob");
   b_x = fTree->GetBranch("x");
   b_y = fTree->GetBranch("y");
   b_z = fTree->GetBranch("z");
   b_arm = fTree->GetBranch("arm");
   b_sec = fTree->GetBranch("sec");
   b_clusid = fTree->GetBranch("clusid");
   b_ecore = fTree->GetBranch("ecore");
   b_ecorr = fTree->GetBranch("ecorr");
   b_ecent = fTree->GetBranch("ecent");
   b_twrhit = fTree->GetBranch("twrhit");
   b_tofmin = fTree->GetBranch("tofmin");
   b_tofmax = fTree->GetBranch("tofmax");
   b_tofcorr = fTree->GetBranch("tofcorr");
   b_chi2 = fTree->GetBranch("chi2");
   b_disp_z = fTree->GetBranch("disp_z");
   b_disp_y = fTree->GetBranch("disp_y");
   b_cg_z = fTree->GetBranch("cg_z");
   b_cg_y = fTree->GetBranch("cg_y");
   b_ind_z = fTree->GetBranch("ind_z");
   b_ind_y = fTree->GetBranch("ind_y");
   b_partesum_1 = fTree->GetBranch("partesum_1");
   b_partesum_2 = fTree->GetBranch("partesum_2");
   b_partesum_3 = fTree->GetBranch("partesum_3");
   b_partesum_4 = fTree->GetBranch("partesum_4");
   b_nemc = fTree->GetBranch("nemc");
   b_segno = fTree->GetBranch("segno");
   return kTRUE;
}

void nEmcClusterLocalExt::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fTree) return;
   fTree->Show(entry);
}
Int_t nEmcClusterLocalExt::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef nEmcClusterLocalExt_cxx

