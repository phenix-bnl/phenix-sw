//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Wed Dec  6 09:38:15 2000 by ROOT version 2.23/12)
//   from TTree nt_trk/track with RICH, EMC, TOF, PC3
//   found on file: run10307.root
//////////////////////////////////////////////////////////


#ifndef nt_trk_h
#define nt_trk_h

#if !defined(__CINT__) || defined(__MAKECINT__)
#include <stdio.h>
#include <iostream>
#include <TTree.h>
#include <TFile.h>
#include <TROOT.h>
#endif
#include "TrackCl.hh"

class nt_trk {
   public :
   TrackCl         trc;

   TTree          *fTree;    //pointer to the analyzed TTree or TChain
   TTree          *fCurrent; //pointer to the current TTree
//Declaration of leaves types
   Float_t         run;
   Float_t         seq;
   Float_t         evt;
   Float_t         trig;
   Float_t         zdcz;
   Float_t         zdce0;
   Float_t         zdce1;
   Float_t         zdct0;
   Float_t         bbcn;
   Float_t         bbcs;
   Float_t         bbcqn;
   Float_t         bbcqs;
   Float_t         bbcz;
   Float_t         bbct0;
   Float_t         ndc;
   Float_t         ndchit;
   Float_t         npc1;
   Float_t         npc3;
   Float_t         ntec;
   Float_t         nemc;
   Float_t         ntof;
   Float_t         ncrk;
   Float_t         ndcw;
   Float_t         ndchitw;
   Float_t         npc1w;
   Float_t         nemcw;
   Float_t         etotw;
   Float_t         etote;
   Float_t         armsid;
   Float_t         quality;
   Float_t         zed;
   Float_t         phi;
   Float_t         alpha;
   Float_t         beta;
   Float_t         phi0;
   Float_t         the0;
   Float_t         mom;
   Float_t         x1m;
   Float_t         x2m;
   Float_t         x1h;
   Float_t         uv1h;
   Float_t         x2h;
   Float_t         uv2h;
   Float_t         pc1ux;
   Float_t         pc1uy;
   Float_t         pc1uz;
   Float_t         intsct;
   Float_t         ppc1x;
   Float_t         ppc1y;
   Float_t         ppc1z;
   Float_t         ppc3x;
   Float_t         ppc3y;
   Float_t         ppc3z;
   Float_t         ptofx;
   Float_t         ptofy;
   Float_t         ptofz;
   Float_t         pemcx;
   Float_t         pemcy;
   Float_t         pemcz;
   Float_t         ptecx;
   Float_t         ptecy;
   Float_t         ptecz;
   Float_t         plcrk;
   Float_t         pltof;
   Float_t         plemc;
   Float_t         sppc1z;
   Float_t         sppc3z;
   Float_t         sptofz;
   Float_t         spemcz;
   Float_t         sptecz;
   Float_t         nx1x2fit;
   Float_t         mchi2;
   Float_t         error;
   Float_t         alphaf;
   Float_t         mpx;
   Float_t         mpy;
   Float_t         mpz;
   Float_t         pc1x;
   Float_t         pc1y;
   Float_t         pc1z;
   Float_t         spc1x;
   Float_t         spc1y;
   Float_t         spc1z;
   Float_t         pc3x;
   Float_t         pc3y;
   Float_t         pc3z;
   Float_t         spc3x;
   Float_t         spc3y;
   Float_t         spc3z;
   Float_t         emcx;
   Float_t         emcy;
   Float_t         emcz;
   Float_t         swkey;
   Float_t         e;
   Float_t         ecore;
   Float_t         ecorr;
   Float_t         ecent;
   Float_t         emctc;
   Float_t         tmax;
   Float_t         prob;
   Float_t         semcx;
   Float_t         semcy;
   Float_t         semcz;
   Float_t         sswkey;
   Float_t         se;
   Float_t         secore;
   Float_t         secorr;
   Float_t         secent;
   Float_t         semctc;
   Float_t         stmax;
   Float_t         sprob;
   Float_t         tofx;
   Float_t         tofy;
   Float_t         tofz;
   Float_t         slat;
   Float_t         toft;
   Float_t         tofe;
   Float_t         stofx;
   Float_t         stofy;
   Float_t         stofz;
   Float_t         sslat;
   Float_t         stoft;
   Float_t         stofe;
   Float_t         tecinx;
   Float_t         teciny;
   Float_t         tecox;
   Float_t         tecoy;
   Float_t         tecnh;
   Float_t         stecinx;
   Float_t         steciny;
   Float_t         stecox;
   Float_t         stecoy;
   Float_t         stecnh;
   Float_t         acc;
   Float_t         n0;
   Float_t         npe0;
   Float_t         n1;
   Float_t         npe1;
   Float_t         n3;
   Float_t         npe3;
   Float_t         ch2;
   Float_t         disp;
   Float_t         rpath;
   Float_t         sacc;
   Float_t         sn0;
   Float_t         snpe0;
   Float_t         sn1;
   Float_t         snpe1;
   Float_t         sn3;
   Float_t         snpe3;
   Float_t         sch2;
   Float_t         sdisp;
   Float_t         srpath;

//List of branches
   TBranch        *b_run;
   TBranch        *b_seq;
   TBranch        *b_evt;
   TBranch        *b_trig;
   TBranch        *b_zdcz;
   TBranch        *b_zdce0;
   TBranch        *b_zdce1;
   TBranch        *b_zdct0;
   TBranch        *b_bbcn;
   TBranch        *b_bbcs;
   TBranch        *b_bbcqn;
   TBranch        *b_bbcqs;
   TBranch        *b_bbcz;
   TBranch        *b_bbct0;
   TBranch        *b_ndc;
   TBranch        *b_ndchit;
   TBranch        *b_npc1;
   TBranch        *b_npc3;
   TBranch        *b_ntec;
   TBranch        *b_nemc;
   TBranch        *b_ntof;
   TBranch        *b_ncrk;
   TBranch        *b_ndcw;
   TBranch        *b_ndchitw;
   TBranch        *b_npc1w;
   TBranch        *b_nemcw;
   TBranch        *b_etotw;
   TBranch        *b_etote;
   TBranch        *b_armsid;
   TBranch        *b_quality;
   TBranch        *b_zed;
   TBranch        *b_phi;
   TBranch        *b_alpha;
   TBranch        *b_beta;
   TBranch        *b_phi0;
   TBranch        *b_the0;
   TBranch        *b_mom;
   TBranch        *b_x1m;
   TBranch        *b_x2m;
   TBranch        *b_x1h;
   TBranch        *b_uv1h;
   TBranch        *b_x2h;
   TBranch        *b_uv2h;
   TBranch        *b_pc1ux;
   TBranch        *b_pc1uy;
   TBranch        *b_pc1uz;
   TBranch        *b_intsct;
   TBranch        *b_ppc1x;
   TBranch        *b_ppc1y;
   TBranch        *b_ppc1z;
   TBranch        *b_ppc3x;
   TBranch        *b_ppc3y;
   TBranch        *b_ppc3z;
   TBranch        *b_ptofx;
   TBranch        *b_ptofy;
   TBranch        *b_ptofz;
   TBranch        *b_pemcx;
   TBranch        *b_pemcy;
   TBranch        *b_pemcz;
   TBranch        *b_ptecx;
   TBranch        *b_ptecy;
   TBranch        *b_ptecz;
   TBranch        *b_plcrk;
   TBranch        *b_pltof;
   TBranch        *b_plemc;
   TBranch        *b_sppc1z;
   TBranch        *b_sppc3z;
   TBranch        *b_sptofz;
   TBranch        *b_spemcz;
   TBranch        *b_sptecz;
   TBranch        *b_nx1x2fit;
   TBranch        *b_mchi2;
   TBranch        *b_error;
   TBranch        *b_alphaf;
   TBranch        *b_mpx;
   TBranch        *b_mpy;
   TBranch        *b_mpz;
   TBranch        *b_pc1x;
   TBranch        *b_pc1y;
   TBranch        *b_pc1z;
   TBranch        *b_spc1x;
   TBranch        *b_spc1y;
   TBranch        *b_spc1z;
   TBranch        *b_pc3x;
   TBranch        *b_pc3y;
   TBranch        *b_pc3z;
   TBranch        *b_spc3x;
   TBranch        *b_spc3y;
   TBranch        *b_spc3z;
   TBranch        *b_emcx;
   TBranch        *b_emcy;
   TBranch        *b_emcz;
   TBranch        *b_swkey;
   TBranch        *b_e;
   TBranch        *b_ecore;
   TBranch        *b_ecorr;
   TBranch        *b_ecent;
   TBranch        *b_emctc;
   TBranch        *b_tmax;
   TBranch        *b_prob;
   TBranch        *b_semcx;
   TBranch        *b_semcy;
   TBranch        *b_semcz;
   TBranch        *b_sswkey;
   TBranch        *b_se;
   TBranch        *b_secore;
   TBranch        *b_secorr;
   TBranch        *b_secent;
   TBranch        *b_semctc;
   TBranch        *b_stmax;
   TBranch        *b_sprob;
   TBranch        *b_tofx;
   TBranch        *b_tofy;
   TBranch        *b_tofz;
   TBranch        *b_slat;
   TBranch        *b_toft;
   TBranch        *b_tofe;
   TBranch        *b_stofx;
   TBranch        *b_stofy;
   TBranch        *b_stofz;
   TBranch        *b_sslat;
   TBranch        *b_stoft;
   TBranch        *b_stofe;
   TBranch        *b_tecinx;
   TBranch        *b_teciny;
   TBranch        *b_tecox;
   TBranch        *b_tecoy;
   TBranch        *b_tecnh;
   TBranch        *b_stecinx;
   TBranch        *b_steciny;
   TBranch        *b_stecox;
   TBranch        *b_stecoy;
   TBranch        *b_stecnh;
   TBranch        *b_acc;
   TBranch        *b_n0;
   TBranch        *b_npe0;
   TBranch        *b_n1;
   TBranch        *b_npe1;
   TBranch        *b_n3;
   TBranch        *b_npe3;
   TBranch        *b_ch2;
   TBranch        *b_disp;
   TBranch        *b_rpath;
   TBranch        *b_sacc;
   TBranch        *b_sn0;
   TBranch        *b_snpe0;
   TBranch        *b_sn1;
   TBranch        *b_snpe1;
   TBranch        *b_sn3;
   TBranch        *b_snpe3;
   TBranch        *b_sch2;
   TBranch        *b_sdisp;
   TBranch        *b_srpath;

   nt_trk(TTree *tree=0);
   ~nt_trk() {};
   Int_t GetEntries(){
     int i = (int) fTree->GetEntries();
     return i; };
   Int_t GetEntry(Int_t entry);
   Int_t LoadTree(Int_t entry);
   void  Init(TTree *tree);
   void  Loop();
   void  Notify();
   void  Show(Int_t entry = -1);
};

#endif

#ifdef nt_trk_cxx
nt_trk::nt_trk(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
  fTree = 0;
  Init(tree);
}

Int_t nt_trk::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fTree) return 0;
   Int_t ibyte = fTree->GetEntry(entry);
   //--------------------------------------------------------
   // Global Information
   trc.run = (int)run;
   trc.evn = (int)evt;
   trc.zdcz = zdcz;
   trc.zdct0 = zdct0;
   trc.zdcch = zdce0 + zdce1;
   trc.bbcz = bbcz;
   trc.bbct0 = bbct0;
   trc.bbcch = bbcqn + bbcqs;
   trc.bbcnhit = bbcn + bbcs;
   trc.pc1nhit = npc1;
   trc.emcnhit = nemc;
   
   // Tracking Information
   if (intsct/256 >= 1 )
     trc.inters = 1;
   else
     trc.inters = 0;
   trc.ptot = mom;
   trc.cglarm = (int)(armsid / 2);
   trc.quality = quality;
   trc.proj[0] = pemcx;
   trc.proj[1] = pemcy;
   trc.proj[2] = pemcz;
   trc.dir[0] = sin(beta)*cos(phi-alpha); // FIX.ME!!!!!
   trc.dir[1] = sin(beta)*sin(phi-alpha);
   trc.dir[2] = cos(beta);
   trc.pathl = plemc;
   trc.alpha = alpha;
   trc.beta = beta;

   // EMCal Clustering information
   if( trc.inters > 0 && swkey>0 ){
     trc.arm = (int)(swkey / 100000);
     trc.sector = (int)((swkey-100000*trc.arm)/10000);
     trc.ind[1] = (int)((swkey-100000*trc.arm-10000*trc.sector)/100);
     trc.ind[0] = (int)( swkey-100000*trc.arm-10000*trc.sector-100*trc.ind[1]);
   } else {
     trc.arm = -1;
     trc.sector = 0;
     trc.ind[1] = 0;
     trc.ind[0] = 0;
   }

   float escale = 0.98; // ENERGY SCALE -2% ....................
   trc.tof = tmax ;
   trc.e = escale* e;
   trc.ecent = escale* ecent;
   trc.ecore = escale* ecore;
   trc.ecorr = escale* ecorr;
   trc.tofcorr = emctc;
   trc.chi2 = prob; // FIX.ME!!!!!!!!
   trc.pos[0] = emcx;
   trc.pos[1] = emcy;
   trc.pos[2] = emcz;
   //trc.disp[],padisp[]..
   trc.newproj[0] = pemcx;
   trc.newproj[1] = pemcy;
   trc.newproj[2] = pemcz;
   trc.drmin = sqrt( (trc.pos[0]-trc.newproj[0])*(trc.pos[0]-trc.newproj[0]) +
		     (trc.pos[1]-trc.newproj[1])*(trc.pos[1]-trc.newproj[1]) +
		     (trc.pos[2]-trc.newproj[2])*(trc.pos[2]-trc.newproj[2]) );
   trc.corpos[0] = emcx;
   trc.corpos[1] = emcy;
   trc.corpos[2] = emcz;
   trc.corpathl = plemc;

   // EMCal Clustering information
   if( trc.inters > 0 && sswkey > 0 ){
     trc.arm_s = (int)(sswkey / 100000);
     trc.sector_s = (int)((sswkey-100000*trc.arm_s)/10000);
     trc.ind_s[1] = (int)((sswkey-100000*trc.arm_s-10000*trc.sector_s)/100);
     trc.ind_s[0] = (int)( sswkey-100000*trc.arm_s-10000*trc.sector_s-100*trc.ind_s[1]);
   } else {
     trc.arm_s = -1;
     trc.sector_s = 0;
     trc.ind_s[1] = 0;
     trc.ind_s[0] = 0;
   }
   trc.tof_s = stmax ;
   trc.e_s = escale* se;
   trc.ecent_s = escale* secent;
   trc.ecore_s = escale* secore;
   trc.ecorr_s = escale* secorr;
   trc.tofcorr_s = semctc;
   trc.chi2_s = sprob; // FIX.ME!!!!!!!!
   trc.pos_s[0] = semcx;
   trc.pos_s[1] = semcy;
   trc.pos_s[2] = semcz;
   //trc.disp[],padisp[]..
   trc.newproj_s[0] = pemcx;
   trc.newproj_s[1] = pemcy;
   trc.newproj_s[2] = spemcz;
   trc.drmin_s = sqrt( (trc.pos_s[0]-trc.newproj_s[0])*(trc.pos_s[0]-trc.newproj_s[0]) +
		       (trc.pos_s[1]-trc.newproj_s[1])*(trc.pos_s[1]-trc.newproj_s[1]) +
		       (trc.pos_s[2]-trc.newproj_s[2])*(trc.pos_s[2]-trc.newproj_s[2]) );
   trc.corpos_s[0] = semcx;
   trc.corpos_s[1] = semcy;
   trc.corpos_s[2] = semcz;
   trc.corpathl_s = plemc; // No Swapped path length....

   // RICH information
   trc.crk_acc = (int)acc;
   trc.crk_npmt0 = (int)n0;
   trc.crk_npmt1 = (int)n1;
   trc.crk_npe0 = npe0;
   trc.crk_npe1 = npe1;
   trc.crk_chi2 = ch2;
   trc.crk_disp = disp;

   // RICH information after Z-flip
   trc.crk_acc_s = (int)sacc;
   trc.crk_npmt0_s = (int)sn0;
   trc.crk_npmt1_s = (int)sn1;
   trc.crk_npe0_s = snpe0;
   trc.crk_npe1_s = snpe1;
   trc.crk_chi2_s = sch2;
   trc.crk_disp_s = sdisp;

   //--------------------------------------------------------
   return ibyte;
}
Int_t nt_trk::LoadTree(Int_t entry)
{
// Set the environment to read one entry
   if (!fTree) return -5;
   Int_t centry = fTree->LoadTree(entry);
   if (centry < 0) return centry;
   if (fTree->GetTree() != fCurrent) {
      fCurrent = fTree->GetTree();
      Notify();
   }
   return centry;
}

void nt_trk::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fTree    = tree;
   fCurrent = 0;

   fTree->SetBranchAddress("run",&run);
   fTree->SetBranchAddress("seq",&seq);
   fTree->SetBranchAddress("evt",&evt);
   fTree->SetBranchAddress("trig",&trig);
   fTree->SetBranchAddress("zdcz",&zdcz);
   fTree->SetBranchAddress("zdce0",&zdce0);
   fTree->SetBranchAddress("zdce1",&zdce1);
   fTree->SetBranchAddress("zdct0",&zdct0);
   fTree->SetBranchAddress("bbcn",&bbcn);
   fTree->SetBranchAddress("bbcs",&bbcs);
   fTree->SetBranchAddress("bbcqn",&bbcqn);
   fTree->SetBranchAddress("bbcqs",&bbcqs);
   fTree->SetBranchAddress("bbcz",&bbcz);
   fTree->SetBranchAddress("bbct0",&bbct0);
   fTree->SetBranchAddress("ndc",&ndc);
   fTree->SetBranchAddress("ndchit",&ndchit);
   fTree->SetBranchAddress("npc1",&npc1);
   fTree->SetBranchAddress("npc3",&npc3);
   fTree->SetBranchAddress("ntec",&ntec);
   fTree->SetBranchAddress("nemc",&nemc);
   fTree->SetBranchAddress("ntof",&ntof);
   fTree->SetBranchAddress("ncrk",&ncrk);
   fTree->SetBranchAddress("ndcw",&ndcw);
   fTree->SetBranchAddress("ndchitw",&ndchitw);
   fTree->SetBranchAddress("npc1w",&npc1w);
   fTree->SetBranchAddress("nemcw",&nemcw);
   fTree->SetBranchAddress("etotw",&etotw);
   fTree->SetBranchAddress("etote",&etote);
   fTree->SetBranchAddress("armsid",&armsid);
   fTree->SetBranchAddress("quality",&quality);
   fTree->SetBranchAddress("zed",&zed);
   fTree->SetBranchAddress("phi",&phi);
   fTree->SetBranchAddress("alpha",&alpha);
   fTree->SetBranchAddress("beta",&beta);
   fTree->SetBranchAddress("phi0",&phi0);
   fTree->SetBranchAddress("the0",&the0);
   fTree->SetBranchAddress("mom",&mom);
   fTree->SetBranchAddress("x1m",&x1m);
   fTree->SetBranchAddress("x2m",&x2m);
   fTree->SetBranchAddress("x1h",&x1h);
   fTree->SetBranchAddress("uv1h",&uv1h);
   fTree->SetBranchAddress("x2h",&x2h);
   fTree->SetBranchAddress("uv2h",&uv2h);
   fTree->SetBranchAddress("pc1ux",&pc1ux);
   fTree->SetBranchAddress("pc1uy",&pc1uy);
   fTree->SetBranchAddress("pc1uz",&pc1uz);
   fTree->SetBranchAddress("intsct",&intsct);
   fTree->SetBranchAddress("ppc1x",&ppc1x);
   fTree->SetBranchAddress("ppc1y",&ppc1y);
   fTree->SetBranchAddress("ppc1z",&ppc1z);
   fTree->SetBranchAddress("ppc3x",&ppc3x);
   fTree->SetBranchAddress("ppc3y",&ppc3y);
   fTree->SetBranchAddress("ppc3z",&ppc3z);
   fTree->SetBranchAddress("ptofx",&ptofx);
   fTree->SetBranchAddress("ptofy",&ptofy);
   fTree->SetBranchAddress("ptofz",&ptofz);
   fTree->SetBranchAddress("pemcx",&pemcx);
   fTree->SetBranchAddress("pemcy",&pemcy);
   fTree->SetBranchAddress("pemcz",&pemcz);
   fTree->SetBranchAddress("ptecx",&ptecx);
   fTree->SetBranchAddress("ptecy",&ptecy);
   fTree->SetBranchAddress("ptecz",&ptecz);
   fTree->SetBranchAddress("plcrk",&plcrk);
   fTree->SetBranchAddress("pltof",&pltof);
   fTree->SetBranchAddress("plemc",&plemc);
   fTree->SetBranchAddress("sppc1z",&sppc1z);
   fTree->SetBranchAddress("sppc3z",&sppc3z);
   fTree->SetBranchAddress("sptofz",&sptofz);
   fTree->SetBranchAddress("spemcz",&spemcz);
   fTree->SetBranchAddress("sptecz",&sptecz);
   fTree->SetBranchAddress("nx1x2fit",&nx1x2fit);
   fTree->SetBranchAddress("mchi2",&mchi2);
   fTree->SetBranchAddress("error",&error);
   fTree->SetBranchAddress("alphaf",&alphaf);
   fTree->SetBranchAddress("mpx",&mpx);
   fTree->SetBranchAddress("mpy",&mpy);
   fTree->SetBranchAddress("mpz",&mpz);
   fTree->SetBranchAddress("pc1x",&pc1x);
   fTree->SetBranchAddress("pc1y",&pc1y);
   fTree->SetBranchAddress("pc1z",&pc1z);
   fTree->SetBranchAddress("spc1x",&spc1x);
   fTree->SetBranchAddress("spc1y",&spc1y);
   fTree->SetBranchAddress("spc1z",&spc1z);
   fTree->SetBranchAddress("pc3x",&pc3x);
   fTree->SetBranchAddress("pc3y",&pc3y);
   fTree->SetBranchAddress("pc3z",&pc3z);
   fTree->SetBranchAddress("spc3x",&spc3x);
   fTree->SetBranchAddress("spc3y",&spc3y);
   fTree->SetBranchAddress("spc3z",&spc3z);
   fTree->SetBranchAddress("emcx",&emcx);
   fTree->SetBranchAddress("emcy",&emcy);
   fTree->SetBranchAddress("emcz",&emcz);
   fTree->SetBranchAddress("swkey",&swkey);
   fTree->SetBranchAddress("e",&e);
   fTree->SetBranchAddress("ecore",&ecore);
   fTree->SetBranchAddress("ecorr",&ecorr);
   fTree->SetBranchAddress("ecent",&ecent);
   fTree->SetBranchAddress("emctc",&emctc);
   fTree->SetBranchAddress("tmax",&tmax);
   fTree->SetBranchAddress("prob",&prob);
   fTree->SetBranchAddress("semcx",&semcx);
   fTree->SetBranchAddress("semcy",&semcy);
   fTree->SetBranchAddress("semcz",&semcz);
   fTree->SetBranchAddress("sswkey",&sswkey);
   fTree->SetBranchAddress("se",&se);
   fTree->SetBranchAddress("secore",&secore);
   fTree->SetBranchAddress("secorr",&secorr);
   fTree->SetBranchAddress("secent",&secent);
   fTree->SetBranchAddress("semctc",&semctc);
   fTree->SetBranchAddress("stmax",&stmax);
   fTree->SetBranchAddress("sprob",&sprob);
   fTree->SetBranchAddress("tofx",&tofx);
   fTree->SetBranchAddress("tofy",&tofy);
   fTree->SetBranchAddress("tofz",&tofz);
   fTree->SetBranchAddress("slat",&slat);
   fTree->SetBranchAddress("toft",&toft);
   fTree->SetBranchAddress("tofe",&tofe);
   fTree->SetBranchAddress("stofx",&stofx);
   fTree->SetBranchAddress("stofy",&stofy);
   fTree->SetBranchAddress("stofz",&stofz);
   fTree->SetBranchAddress("sslat",&sslat);
   fTree->SetBranchAddress("stoft",&stoft);
   fTree->SetBranchAddress("stofe",&stofe);
   fTree->SetBranchAddress("tecinx",&tecinx);
   fTree->SetBranchAddress("teciny",&teciny);
   fTree->SetBranchAddress("tecox",&tecox);
   fTree->SetBranchAddress("tecoy",&tecoy);
   fTree->SetBranchAddress("tecnh",&tecnh);
   fTree->SetBranchAddress("stecinx",&stecinx);
   fTree->SetBranchAddress("steciny",&steciny);
   fTree->SetBranchAddress("stecox",&stecox);
   fTree->SetBranchAddress("stecoy",&stecoy);
   fTree->SetBranchAddress("stecnh",&stecnh);
   fTree->SetBranchAddress("acc",&acc);
   fTree->SetBranchAddress("n0",&n0);
   fTree->SetBranchAddress("npe0",&npe0);
   fTree->SetBranchAddress("n1",&n1);
   fTree->SetBranchAddress("npe1",&npe1);
   fTree->SetBranchAddress("n3",&n3);
   fTree->SetBranchAddress("npe3",&npe3);
   fTree->SetBranchAddress("ch2",&ch2);
   fTree->SetBranchAddress("disp",&disp);
   fTree->SetBranchAddress("rpath",&rpath);
   fTree->SetBranchAddress("sacc",&sacc);
   fTree->SetBranchAddress("sn0",&sn0);
   fTree->SetBranchAddress("snpe0",&snpe0);
   fTree->SetBranchAddress("sn1",&sn1);
   fTree->SetBranchAddress("snpe1",&snpe1);
   fTree->SetBranchAddress("sn3",&sn3);
   fTree->SetBranchAddress("snpe3",&snpe3);
   fTree->SetBranchAddress("sch2",&sch2);
   fTree->SetBranchAddress("sdisp",&sdisp);
   fTree->SetBranchAddress("srpath",&srpath);
}

void nt_trk::Notify()
{
//   called by LoadTree when loading a new file
//   get branch pointers
   b_run = fTree->GetBranch("run");
   b_seq = fTree->GetBranch("seq");
   b_evt = fTree->GetBranch("evt");
   b_trig = fTree->GetBranch("trig");
   b_zdcz = fTree->GetBranch("zdcz");
   b_zdce0 = fTree->GetBranch("zdce0");
   b_zdce1 = fTree->GetBranch("zdce1");
   b_zdct0 = fTree->GetBranch("zdct0");
   b_bbcn = fTree->GetBranch("bbcn");
   b_bbcs = fTree->GetBranch("bbcs");
   b_bbcqn = fTree->GetBranch("bbcqn");
   b_bbcqs = fTree->GetBranch("bbcqs");
   b_bbcz = fTree->GetBranch("bbcz");
   b_bbct0 = fTree->GetBranch("bbct0");
   b_ndc = fTree->GetBranch("ndc");
   b_ndchit = fTree->GetBranch("ndchit");
   b_npc1 = fTree->GetBranch("npc1");
   b_npc3 = fTree->GetBranch("npc3");
   b_ntec = fTree->GetBranch("ntec");
   b_nemc = fTree->GetBranch("nemc");
   b_ntof = fTree->GetBranch("ntof");
   b_ncrk = fTree->GetBranch("ncrk");
   b_ndcw = fTree->GetBranch("ndcw");
   b_ndchitw = fTree->GetBranch("ndchitw");
   b_npc1w = fTree->GetBranch("npc1w");
   b_nemcw = fTree->GetBranch("nemcw");
   b_etotw = fTree->GetBranch("etotw");
   b_etote = fTree->GetBranch("etote");
   b_armsid = fTree->GetBranch("armsid");
   b_quality = fTree->GetBranch("quality");
   b_zed = fTree->GetBranch("zed");
   b_phi = fTree->GetBranch("phi");
   b_alpha = fTree->GetBranch("alpha");
   b_beta = fTree->GetBranch("beta");
   b_phi0 = fTree->GetBranch("phi0");
   b_the0 = fTree->GetBranch("the0");
   b_mom = fTree->GetBranch("mom");
   b_x1m = fTree->GetBranch("x1m");
   b_x2m = fTree->GetBranch("x2m");
   b_x1h = fTree->GetBranch("x1h");
   b_uv1h = fTree->GetBranch("uv1h");
   b_x2h = fTree->GetBranch("x2h");
   b_uv2h = fTree->GetBranch("uv2h");
   b_pc1ux = fTree->GetBranch("pc1ux");
   b_pc1uy = fTree->GetBranch("pc1uy");
   b_pc1uz = fTree->GetBranch("pc1uz");
   b_intsct = fTree->GetBranch("intsct");
   b_ppc1x = fTree->GetBranch("ppc1x");
   b_ppc1y = fTree->GetBranch("ppc1y");
   b_ppc1z = fTree->GetBranch("ppc1z");
   b_ppc3x = fTree->GetBranch("ppc3x");
   b_ppc3y = fTree->GetBranch("ppc3y");
   b_ppc3z = fTree->GetBranch("ppc3z");
   b_ptofx = fTree->GetBranch("ptofx");
   b_ptofy = fTree->GetBranch("ptofy");
   b_ptofz = fTree->GetBranch("ptofz");
   b_pemcx = fTree->GetBranch("pemcx");
   b_pemcy = fTree->GetBranch("pemcy");
   b_pemcz = fTree->GetBranch("pemcz");
   b_ptecx = fTree->GetBranch("ptecx");
   b_ptecy = fTree->GetBranch("ptecy");
   b_ptecz = fTree->GetBranch("ptecz");
   b_plcrk = fTree->GetBranch("plcrk");
   b_pltof = fTree->GetBranch("pltof");
   b_plemc = fTree->GetBranch("plemc");
   b_sppc1z = fTree->GetBranch("sppc1z");
   b_sppc3z = fTree->GetBranch("sppc3z");
   b_sptofz = fTree->GetBranch("sptofz");
   b_spemcz = fTree->GetBranch("spemcz");
   b_sptecz = fTree->GetBranch("sptecz");
   b_nx1x2fit = fTree->GetBranch("nx1x2fit");
   b_mchi2 = fTree->GetBranch("mchi2");
   b_error = fTree->GetBranch("error");
   b_alphaf = fTree->GetBranch("alphaf");
   b_mpx = fTree->GetBranch("mpx");
   b_mpy = fTree->GetBranch("mpy");
   b_mpz = fTree->GetBranch("mpz");
   b_pc1x = fTree->GetBranch("pc1x");
   b_pc1y = fTree->GetBranch("pc1y");
   b_pc1z = fTree->GetBranch("pc1z");
   b_spc1x = fTree->GetBranch("spc1x");
   b_spc1y = fTree->GetBranch("spc1y");
   b_spc1z = fTree->GetBranch("spc1z");
   b_pc3x = fTree->GetBranch("pc3x");
   b_pc3y = fTree->GetBranch("pc3y");
   b_pc3z = fTree->GetBranch("pc3z");
   b_spc3x = fTree->GetBranch("spc3x");
   b_spc3y = fTree->GetBranch("spc3y");
   b_spc3z = fTree->GetBranch("spc3z");
   b_emcx = fTree->GetBranch("emcx");
   b_emcy = fTree->GetBranch("emcy");
   b_emcz = fTree->GetBranch("emcz");
   b_swkey = fTree->GetBranch("swkey");
   b_e = fTree->GetBranch("e");
   b_ecore = fTree->GetBranch("ecore");
   b_ecorr = fTree->GetBranch("ecorr");
   b_ecent = fTree->GetBranch("ecent");
   b_emctc = fTree->GetBranch("emctc");
   b_tmax = fTree->GetBranch("tmax");
   b_prob = fTree->GetBranch("prob");
   b_semcx = fTree->GetBranch("semcx");
   b_semcy = fTree->GetBranch("semcy");
   b_semcz = fTree->GetBranch("semcz");
   b_sswkey = fTree->GetBranch("sswkey");
   b_se = fTree->GetBranch("se");
   b_secore = fTree->GetBranch("secore");
   b_secorr = fTree->GetBranch("secorr");
   b_secent = fTree->GetBranch("secent");
   b_semctc = fTree->GetBranch("semctc");
   b_stmax = fTree->GetBranch("stmax");
   b_sprob = fTree->GetBranch("sprob");
   b_tofx = fTree->GetBranch("tofx");
   b_tofy = fTree->GetBranch("tofy");
   b_tofz = fTree->GetBranch("tofz");
   b_slat = fTree->GetBranch("slat");
   b_toft = fTree->GetBranch("toft");
   b_tofe = fTree->GetBranch("tofe");
   b_stofx = fTree->GetBranch("stofx");
   b_stofy = fTree->GetBranch("stofy");
   b_stofz = fTree->GetBranch("stofz");
   b_sslat = fTree->GetBranch("sslat");
   b_stoft = fTree->GetBranch("stoft");
   b_stofe = fTree->GetBranch("stofe");
   b_tecinx = fTree->GetBranch("tecinx");
   b_teciny = fTree->GetBranch("teciny");
   b_tecox = fTree->GetBranch("tecox");
   b_tecoy = fTree->GetBranch("tecoy");
   b_tecnh = fTree->GetBranch("tecnh");
   b_stecinx = fTree->GetBranch("stecinx");
   b_steciny = fTree->GetBranch("steciny");
   b_stecox = fTree->GetBranch("stecox");
   b_stecoy = fTree->GetBranch("stecoy");
   b_stecnh = fTree->GetBranch("stecnh");
   b_acc = fTree->GetBranch("acc");
   b_n0 = fTree->GetBranch("n0");
   b_npe0 = fTree->GetBranch("npe0");
   b_n1 = fTree->GetBranch("n1");
   b_npe1 = fTree->GetBranch("npe1");
   b_n3 = fTree->GetBranch("n3");
   b_npe3 = fTree->GetBranch("npe3");
   b_ch2 = fTree->GetBranch("ch2");
   b_disp = fTree->GetBranch("disp");
   b_rpath = fTree->GetBranch("rpath");
   b_sacc = fTree->GetBranch("sacc");
   b_sn0 = fTree->GetBranch("sn0");
   b_snpe0 = fTree->GetBranch("snpe0");
   b_sn1 = fTree->GetBranch("sn1");
   b_snpe1 = fTree->GetBranch("snpe1");
   b_sn3 = fTree->GetBranch("sn3");
   b_snpe3 = fTree->GetBranch("snpe3");
   b_sch2 = fTree->GetBranch("sch2");
   b_sdisp = fTree->GetBranch("sdisp");
   b_srpath = fTree->GetBranch("srpath");
}

void nt_trk::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fTree) return;
   fTree->Show(entry);
}
#endif // #ifdef nt_trk_cxx

