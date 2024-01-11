#include "Pi0Analysis.h"
#include "Warnmap.h"
#include "UncalibTowerList.h"
#include "RecalEcore.h"
#include "Pi0MassFitter.h"

#include <TFile.h>
#include <TH1D.h>
#include <TH1F.h>
#include <TH2D.h>
#include <TGraphErrors.h>
#include <TTree.h>
#include <TChain.h>

#include <TSystem.h>
#include <TStyle.h>
#include <TVector3.h>

#include <TMath.h>


#include <cstdlib>
#include <cmath>
#include <iostream>
#include <fstream>


using namespace std;
using namespace EmcAnaCommon;


Pi0Analysis::Pi0Analysis(Int_t bl_ene_corr, const char* fname_warnmap, const char* fname_coef, const char* fname_coef_supermod, const char* fname_uncalib_list)
{

  uncalib_list = new UncalibTowerList();
  uncalib_list->ReadUncalibList(fname_uncalib_list);
  
  m_bl_ene_corr = bl_ene_corr;
  
  m_type_by_type     = false;
  m_sector_by_sector = false;
  m_tower_by_tower   = false;
  m_supermod_by_supermod = false;
  
  m_nev      = 0;
  m_nev_4x4a = 0;
  m_nev_4x4b = 0;
  m_nev_4x4c_bbc_narrow = 0;
  m_nev_bbc  = 0;
  m_nev_bbc_narrow  = 0;

  m_npi0 = 0;
  m_npi0_4x4a = 0;
  m_npi0_4x4b = 0;
  m_npi0_4x4c_bbc_narrow = 0;
  m_npi0_bbc  = 0;
  m_npi0_bbc_narrow  = 0;

  
  m_warnmap = new Warnmap();
  m_warnmap->ReadMap(fname_warnmap);
  
  m_recal_ecore = 0;
  if (m_bl_ene_corr) {
    m_recal_ecore = new RecalEcore();
    m_recal_ecore->ReadCoef(fname_coef, fname_coef_supermod);
  }
}

Pi0Analysis::~Pi0Analysis()
{
  delete uncalib_list;

  if (m_warnmap) delete m_warnmap;
  if (m_recal_ecore) delete m_recal_ecore;
  
  if (!fChain) return;
  delete fChain->GetCurrentFile();
}

Int_t Pi0Analysis::SetTree(TTree* tree)
{
//     if (fChain) delete fChain->GetCurrentFile();
   fChain = tree;
   fCurrent = -1;
   
   fChain->SetBranchAddress("run",&m_run);
//   fChain->SetBranchAddress("evt",&m_evt);
//     fChain->SetBranchAddress("bbct0",&m_bbct0);
   fChain->SetBranchAddress("trig_MPC_2x2",&m_trig_MPC_2x2);
   fChain->SetBranchAddress("trig_4x4c_bbc_narrow",&m_trig_4x4c_bbc_narrow);
   fChain->SetBranchAddress("trig_4x4a",&m_trig_4x4a);
   fChain->SetBranchAddress("trig_4x4b",&m_trig_4x4b);

   fChain->SetBranchAddress("trig_bbc",&m_trig_bbc);
   fChain->SetBranchAddress("trig_bbc_narrow",&m_trig_bbc_narrow);
   fChain->SetBranchAddress("trig_novertex",&m_trig_novertex);//
   fChain->SetBranchAddress("trig_4x4b_pure",&m_trig_4x4b_pure);
   fChain->SetBranchAddress("trig_4x4b_e_narrow",&m_trig_4x4b_e_narrow);

   //Get BBC Charge-infos
   //Use as centrality info in AA!
   //(Hint by Ermias)
   fChain->SetBranchAddress("bbcqn",&m_bbccn);
   fChain->SetBranchAddress("bbcqs",&m_bbccs);



   fChain->SetBranchAddress("bbcz", &m_bbcz);
   fChain->SetBranchAddress("bbct0", &m_bbct0);


   fChain->SetBranchAddress("n_photon",&m_n_photon);
   fChain->SetBranchAddress("armsect",m_armsect);
   fChain->SetBranchAddress("ypos",m_ypos);
   fChain->SetBranchAddress("zpos",m_zpos);
   if (m_bl_ene_corr) {
      fChain->SetBranchAddress("multiplicity",m_multiplicity);
      fChain->SetBranchAddress("multiplicity_total",&m_multiplicity_total);
      fChain->SetBranchAddress("towerid",m_towerid);
      fChain->SetBranchAddress("partesum",m_partesum);
      fChain->SetBranchAddress("e",m_e);
   }
   fChain->SetBranchAddress("ecore",m_ecore);
   fChain->SetBranchAddress("x",m_x);
   fChain->SetBranchAddress("y",m_y);
   fChain->SetBranchAddress("z",m_z);
   fChain->SetBranchAddress("tof",m_tof);
   fChain->SetBranchAddress("chi2",m_chi2);

   
   return 1;
}

Int_t Pi0Analysis::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}

Long64_t Pi0Analysis::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
  if (!fChain) return -5;

   Long64_t centry = fChain->LoadTree(entry);
   if(m_n_photon>N_PHOTON){
     cout<<"You didn't allow for high enough photon number"<<endl;
     cout<<"You should have allowed for at least "<<m_n_photon<<" for entry number "<<entry<<"."<<endl;
    return -999;
   }
   if(m_multiplicity_total>MULTIPLICITY_TOTAL){
     cout<<"You didn't allow for high enough multiplicity"<<endl;
     cout<<"You should have allowed for at least "<<m_multiplicity_total<<" for entry number "<<entry<<"."<<endl;
    return -999;
   }
   if (centry < 0) return centry;
   if (fChain->IsA() != TChain::Class()) return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
   }
   return centry;
}

void Pi0Analysis::InitMassHist(const char* fname_root_for_hist, Int_t type_by_type, Int_t sector_by_sector, Int_t tower_by_tower)
{
   m_ofile = new TFile(fname_root_for_hist, "RECREATE");
   m_ofile->cd();

   m_type_by_type     = type_by_type;
   m_sector_by_sector = sector_by_sector;
   m_tower_by_tower   = tower_by_tower;
   m_supermod_by_supermod = tower_by_tower;
   
   Char_t hname[1024];
   if (m_type_by_type) {
     //for(Int_t iv = 0; iv<3; iv++){
     for(Int_t iv = 2; iv<3; iv++){//HG
       sprintf(hname, "h1_pi0mass_PbGl_v%i", iv);
       m_h1_pi0mass_PbGl[iv] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
       sprintf(hname, "h1_pi0mass_PbSc_v%i", iv);
       m_h1_pi0mass_PbSc[iv] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
       
       sprintf(hname, "h2_pi0mass_PbGl_v%i", iv);
       m_h2_pi0mass_PbGl[iv] = new TH2D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX, N_BINS_PT_PI0, PT_MIN_PI0, PT_MAX_PI0);
       sprintf(hname, "h2_pi0mass_PbSc_v%i", iv);
       m_h2_pi0mass_PbSc[iv] = new TH2D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX, N_BINS_PT_PI0, PT_MIN_PI0, PT_MAX_PI0);

     }
   }
   if (m_sector_by_sector) {
      for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
	//	for(Int_t iv = 0; iv < 3; iv++){
	for(Int_t iv = 2; iv < 3; iv++){
	  sprintf(hname, "h1_pi0mass_as%i_v%i", ias, iv);
         m_h1_pi0mass_as[ias][iv] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
	 sprintf(hname, "h1_tof_as%i_v%i", ias, iv);
         m_h1_tof_as[ias][iv] = new TH1D(hname, "", 2000, -100, 100);

         sprintf(hname, "h2_pi0mass_as%i_v%i", ias, iv);
         m_h2_pi0mass_as[ias][iv] = new TH2D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX, N_BINS_PT_PI0, PT_MIN_PI0, PT_MAX_PI0);

         for (int i = 0; i < 2; i++) {
	   sprintf(hname, "h1_pi0mass_as%ins%i_v%i", ias, i, iv);
            m_h1_pi0mass_asns[ias][i][iv] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
         }
	}
      }
   }
   hBBCC_NS = new TH1F("hBBCC_NS","",1000,0.,1000.);
   if (m_tower_by_tower) {
      for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      for (Int_t iy = 0; iy < N_YPOS_PBGL; iy++) {
      for (Int_t iz = 0; iz < N_ZPOS_PBGL; iz++) {
	//      for(Int_t iv = 0; iv < 3; iv++){
	for(Int_t iv = 2; iv < 3; iv++){
      if (IsValidYZ(ias, iy, iz)) {
	sprintf(hname, "h1_pi0mass_as%iy%.2iz%.2i_v%i", ias, iy, iz, iv);
         m_h1_pi0mass_tower[ias][iy][iz][iv] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
      }
      }
      }
      }
      }
   }
   if (m_supermod_by_supermod) {
     for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
       for (Int_t ism = 0; ism < N_SUPERMOD; ism++){
	 //	 for(Int_t iv = 0; iv < 3; iv++){
	 for(Int_t iv = 2; iv < 3; iv++){
	   if (IsValidSM(ias,ism)){
	     sprintf(hname,"h1_pi0mass_as%ism%.2i_v%i",ias,ism,iv);
	     m_h1_pi0mass_supermod[ias][ism][iv] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
	   }
	 }
       }
     }
   }

   
}

void Pi0Analysis::MakeMassHist(
			       const char* fname, double mom_min_target_pbsc, double mom_min_target_pbgl, double mom_min_pair_pbsc, double mom_min_pair_pbgl, double pT_min_pair_pbsc, double pT_min_pair_pbgl, double frac_energy_in_target, double tof_min, double tof_max, double bbc_max_chargesum)
{
   TFile* ifile = new TFile(fname);
   TTree* itree = (TTree*)ifile->Get("cluster_tree");
   if (itree) ProcessTree(itree, mom_min_target_pbsc, mom_min_target_pbgl, mom_min_pair_pbsc, mom_min_pair_pbgl, pT_min_pair_pbsc, pT_min_pair_pbgl, frac_energy_in_target, tof_min, tof_max,bbc_max_chargesum);
   delete ifile;
}


void Pi0Analysis::ProcessTree(
			      TTree* tree, double mom_min_target_pbsc, double mom_min_target_pbgl, double mom_min_pair_pbsc, double mom_min_pair_pbgl, double pT_min_pair_pbsc, double pT_min_pair_pbgl, double frac_energy_in_target, double tof_min, double tof_max, double bbc_max_chargesum)
{
   SetTree(tree);
   if (fChain == 0) return;
   if (m_type_by_type == false && m_sector_by_sector == false && m_tower_by_tower == false) {
      cerr << "all type-by-type, sector-by-sector and tower-by-tower analysis are set to off." << endl;
      return;
   }

   Long64_t nentries = (Long64_t)fChain->GetEntriesFast();

   Int_t nbytes = 0, nb = 0;
   //   Int_t bbcz_bin = 2;

   for (Long64_t jentry = 0; jentry < nentries; jentry++) {
     //for (Long64_t jentry = 0; jentry < 20; jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0&&ientry!=-999) break;
      if (ientry == -999) continue;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      //here
//      if (! m_trig_bbc) continue;
//       cout << "DEBUG: Pi0Analysis: After fChainGenEntry " << endl;
      //determine z-vertex bin
      if(fabs(m_bbcz)>30){continue;}
      

//       if(fabs(m_bbcz)<=15){bbcz_bin = 0;}//HG
//       else{bbcz_bin = 1;}

//       cout << "DEBUG: Pi0Analysis: After fabsbbc " << endl;

//       if (! m_trig_4x4c_bbc_narrow && !m_trig_4x4b && ! m_trig_4x4a && ! m_trig_bbc && ! m_trig_bbc_narrow && ! m_trig_MPC_2x2) continue;
//       m_nev += 1.0;
//       if (m_trig_4x4a) m_nev_4x4a += 1.0;
//       if (m_trig_4x4b) m_nev_4x4b += 1.0;
//       if (m_trig_4x4c_bbc_narrow) m_nev_4x4c_bbc_narrow += 1.0;
//       if (m_trig_bbc)  m_nev_bbc  += 1.0;
//       if (m_trig_bbc_narrow) m_nev_bbc_narrow += 1.0;
//       if (m_trig_MPC_2x2) m_nev_MPC_2x2 += 1.0;

      if (! m_trig_4x4c_bbc_narrow && !m_trig_4x4b && ! m_trig_4x4a && ! m_trig_bbc && ! m_trig_bbc_narrow && ! m_trig_MPC_2x2 && !m_trig_novertex && !m_trig_4x4b_pure && ! m_trig_4x4b_e_narrow) continue;
      m_nev += 1.0;
      if (m_trig_4x4a) m_nev_4x4a += 1.0;
      if (m_trig_4x4b) m_nev_4x4b += 1.0;
      if (m_trig_4x4c_bbc_narrow) m_nev_4x4c_bbc_narrow += 1.0;
      if (m_trig_bbc)  m_nev_bbc  += 1.0;
      if (m_trig_bbc_narrow) m_nev_bbc_narrow += 1.0;
      if (m_trig_MPC_2x2) m_nev_MPC_2x2 += 1.0;
      if (m_trig_novertex) m_nev_novertex += 1.0;//
      if (m_trig_4x4b_pure) m_nev_4x4b_pure += 1.0;
      if (m_trig_4x4b_e_narrow) m_nev_4x4b_e_narrow += 1.0;
	
//       cout << "DEBUG: Pi0Analysis: After Trigger " << endl;


       // cehck for event centrality.
       hBBCC_NS->Fill((m_bbccn+m_bbccs));

       //apply different centrality cuts for pbgl and pbsc to balance statistics and quality of the mass peaks.//HG
       if ( (m_bbccn+m_bbccs)>bbc_max_chargesum ) continue;
       Int_t ValidPbSc = 0;
       if ( (m_bbccn+m_bbccs)<=300.0 ) ValidPbSc = 1;

       //// cluster energy re-calculation with coefficients
       if (m_bl_ene_corr) 
	 m_recal_ecore->ExecRecal(m_n_photon, m_ecore, m_multiplicity, m_towerid, m_partesum);
       
       //       cout << "DEBUG: Pi0Analysis: After Recal   " << endl;
             
       //// create run-by-run histo
       if (m_type_by_type && 
	   m_h1_pi0mass_PbGl_run[2].find(m_run) == m_h1_pi0mass_PbGl_run[2].end() ) {
	 m_ofile->cd();
	 Char_t hname[1024];
	 //	 for(Int_t iv = 0; iv < 3; iv++){
	 for(Int_t iv = 2; iv < 3; iv++){
	   sprintf(hname, "h1_pi0mass_PbGl_run%i_v%i", m_run, iv);
	   m_h1_pi0mass_PbGl_run[iv][m_run] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
	   sprintf(hname, "h1_pi0mass_PbSc_run%i_v%i", m_run, iv);
	   m_h1_pi0mass_PbSc_run[iv][m_run] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
	   for (int ias = 0; ias < N_ARMSECT; ias++) {
	     sprintf(hname, "h1_pi0mass_as%i_run%i_v%i", ias, m_run, iv);
	     m_h1_pi0mass_as_run[ias][iv][m_run] = new TH1D(hname, "", N_BINS_MASS, MASS_MIN, MASS_MAX);
	   }
	 }
       }
       
       

//       cout << "DEBUG: Pi0Analysis: RUN by RUN    " << endl;
      //// pi0 reconstruction
      ////  iph: photon in target tower ... fill candidate
      ////  jph: pair photon (warnmap is applied)
      for (int iph = 0; iph < m_n_photon; iph++) {
  
         Int_t ias = m_armsect[iph];
	 if(!IsPbGl(ias) && ValidPbSc==0) continue;
	 
	 //cut chi2, since prob photon wasn't working correctly when I did this
	 if(m_chi2[iph]>3){continue;}
         Int_t iy = m_ypos[iph];
         Int_t iz = m_zpos[iph];
	 //	 if(ias==5 && iy==40){cout<<"got one, iy = "<<iy<<endl;}
	 //if(ias==5 && iy==40){cout<<"supermod was = "<<GetSuperModule(ias,iy,iz)<<endl;}
         double ei = m_ecore[iph];

         if (!IsPbGl(ias)&&ei < mom_min_target_pbsc) continue;
         if (IsPbGl(ias)&&ei < mom_min_target_pbgl) continue;

//       cout << "DEBUG: Pi0Analysis: PbGlChecks    " << endl;
         
         TVector3 ri(m_x[iph], m_y[iph], m_z[iph]);
         TVector3 pi = ei * ri.Unit();

// 	 Double_t ti = m_tof[iph] - m_bbct0 - CalculateT4D(m_x[iph],  m_y[iph], m_z[iph], m_bbcz);
// 	 m_h1_tof_as[ias][bbcz_bin]->Fill(ti);
// 	 m_h1_tof_as[ias][2]->Fill(ti);

// 	 if(ti<tof_min||ti>tof_max){continue;}

         for (int jph = 0; jph < m_n_photon; jph++) {
	
	   Int_t jas = m_armsect[jph];
	   
	   if(!IsPbGl(jas) && ValidPbSc==0) continue;//HG

	   if (iph == jph) continue;
            


	    //cut on chi2, since prob photon wasn't working correctly when I did this
	    //no cut for pbgl because it had too low statistics
	    if(m_chi2[jph]>3){continue;}
            Int_t jy = m_ypos[jph];
            Int_t jz = m_zpos[jph];

	    if(uncalib_list->IsInUncalibList(jas, jy, jz)==UncalibTowerList::WARNED){
	      //if(ias==5 && iy==40){cout<<"this was the reason for continuing"<<endl;}
	      continue;
	    }

            if (m_warnmap->IsBad(jas, jy, jz)) continue;

	   

            double ej = m_ecore[jph];
            if (!IsPbGl(jas)&&ej< mom_min_pair_pbsc) continue;
            if (IsPbGl(jas)&&ej< mom_min_pair_pbgl) continue;

	    //if(ei <= ((frac_energy_in_target)/(1.0-frac_energy_in_target))*ej){continue;} 

            TVector3 rj(m_x[jph], m_y[jph], m_z[jph]);
            TVector3 pj = ej * rj.Unit();

	    // Double_t tj = m_tof[jph] - m_bbct0 - CalculateT4D(m_x[jph],  m_y[jph], m_z[jph], m_bbcz);
// 	    if(tj<tof_min||tj>tof_max){continue;}

            //// mass reconstruction with i and j
            TVector3 pij = pi + pj;
            double p = pij.Mag();
            double pT = pij.Pt();
            double m = sqrt( (ei + ej)*(ei + ej) - p*p );

            if (!IsPbGl(ias)&&pT < pT_min_pair_pbsc) continue;
	    if (IsPbGl(ias)&&pT < pT_min_pair_pbgl) continue;

	    if (m_type_by_type 
		&& ! m_warnmap->IsBad(ias, iy, iz)
		) {
	      
	      if (IsPbGl(ias) && IsPbGl(jas)) {
		//		m_h1_pi0mass_PbGl[bbcz_bin]->Fill(m);
		m_h1_pi0mass_PbGl[2]->Fill(m);
		
		//		m_h2_pi0mass_PbGl[bbcz_bin]->Fill(m, pT);
		m_h2_pi0mass_PbGl[2]->Fill(m, pT);
		
		//		m_h1_pi0mass_PbGl_run[bbcz_bin][m_run]->Fill(m);
		m_h1_pi0mass_PbGl_run[2][m_run]->Fill(m);
	      } else if (! IsPbGl(ias) && ! IsPbGl(jas)) {
		//m_h1_pi0mass_PbSc[bbcz_bin]->Fill(m);
		m_h1_pi0mass_PbSc[2]->Fill(m);
		
		//		m_h2_pi0mass_PbSc[bbcz_bin]->Fill(m, pT);
		m_h2_pi0mass_PbSc[2]->Fill(m, pT);
		
		//m_h1_pi0mass_PbSc_run[bbcz_bin][m_run]->Fill(m);
		m_h1_pi0mass_PbSc_run[2][m_run]->Fill(m);
	      }
	    }
	    
	    
	    if (m_sector_by_sector && ias == jas 
		&& ! m_warnmap->IsBad(ias, iy, iz) ) {
	      //m_h1_pi0mass_as[ias][bbcz_bin]->Fill(m);
	      m_h1_pi0mass_as[ias][2]->Fill(m);
	      
	      //m_h2_pi0mass_as[ias][bbcz_bin]->Fill(m, pT);
	      m_h2_pi0mass_as[ias][2]->Fill(m, pT);
	      
	      //m_h1_pi0mass_as_run[ias][bbcz_bin][m_run]->Fill(m);
	      m_h1_pi0mass_as_run[ias][2][m_run]->Fill(m);
	      
	      if ( (  IsPbGl(ias) && iz < N_ZPOS_PBGL/2) ||
		   (! IsPbGl(ias) && iz < N_ZPOS_PBSC/2) ) {
		//m_h1_pi0mass_asns[ias][0][bbcz_bin]->Fill(m);
		m_h1_pi0mass_asns[ias][0][2]->Fill(m);
		
	      } else {
		//m_h1_pi0mass_asns[ias][1][bbcz_bin]->Fill(m);
		m_h1_pi0mass_asns[ias][1][2]->Fill(m);
		
	      }
	    }
	    
	    
	    
	    if (m_tower_by_tower && ias == jas){
	    //if (m_tower_by_tower && (IsPbGl(ias) == IsPbGl(jas))) {
	      
	      if(m>0.112&&m<0.162){
		m_npi0 += 1.0;
		if (m_trig_4x4a) m_npi0_4x4a += 1.0;
		if (m_trig_4x4b) m_npi0_4x4b += 1.0;
		if (m_trig_4x4c_bbc_narrow) m_npi0_4x4c_bbc_narrow += 1.0;
		if (m_trig_bbc)  m_npi0_bbc  += 1.0;
		if (m_trig_bbc_narrow) m_npi0_bbc_narrow += 1.0;
		if (m_trig_MPC_2x2) m_npi0_MPC_2x2 += 1.0;
		if (m_trig_novertex) m_npi0_novertex += 1.0;//HG
		if (m_trig_4x4b_pure) m_npi0_4x4b_pure += 1.0; //HG
		if (m_trig_4x4b_e_narrow) m_npi0_4x4b_e_narrow += 1.0;//HG
	      }

	      //if(ias==5 && iy==40){cout<<"kept it, iy = "<<iy<<endl;}
	      //m_h1_pi0mass_tower[ias][iy][iz][bbcz_bin]->Fill(m);
	      m_h1_pi0mass_tower[ias][iy][iz][2]->Fill(m);
	      
	      if((!uncalib_list->IsInUncalibList(jas, jy, jz))&&(!uncalib_list->IsInUncalibList(ias,iy,iz))){
		//m_h1_pi0mass_supermod[ias][GetSuperModule(ias,iy,iz)][bbcz_bin]->Fill(m);
		m_h1_pi0mass_supermod[ias][GetSuperModule(ias,iy,iz)][2]->Fill(m);
		//if(ias==5 && iy==40){cout<<"supermod was = "<<GetSuperModule(ias,iy,iz)<<endl;}
	      }

	    }
	    
            
         }
      }
//       cout << "DEBUG: Pi0Analysis: End Event     " << endl;
      //cout<<"looped"<<endl;
   } //// end of event loop
   //cout<<"   ended event loop"<<endl;
}

void Pi0Analysis::AnalysisRunByRun()
{
   cout << "Analyzing run-by-run pi0 mass position..." << endl;

   Pi0MassFitter* fitter = new Pi0MassFitter();
   Double_t height, height_err, mean, mean_err, sigma, sigma_err;
   Double_t chi2, signal, bg;
   Int_t NDF, poln;

   Char_t grname[1024];

    TGraphErrors* gr_mean_PbGl[3];
    TGraphErrors* gr_mean_PbSc[3];
    TGraphErrors* gr_sigma_PbGl[3];
    TGraphErrors* gr_sigma_PbSc[3];
   
    //   for(Int_t iv = 0; iv < 3; iv++){
    for(Int_t iv = 2; iv < 3; iv++){
     gr_mean_PbGl[iv]  = new TGraphErrors();
     gr_mean_PbSc[iv]  = new TGraphErrors();
     gr_sigma_PbGl[iv] = new TGraphErrors();
     gr_sigma_PbSc[iv] = new TGraphErrors();
     sprintf(grname,"gr_mean_PbGl_v%i",iv);
     gr_mean_PbGl[iv] ->SetName(grname);
     sprintf(grname,"gr_mean_PbSc_v%i",iv);
     gr_mean_PbSc[iv] ->SetName(grname);
     sprintf(grname,"gr_sigma_PbGl_v%i",iv);
     gr_sigma_PbGl[iv]->SetName(grname);
     sprintf(grname,"gr_sigma_PbSc_v%i",iv);
     gr_sigma_PbSc[iv]->SetName(grname);
   }
      
   TGraphErrors* gr_mean_as [N_ARMSECT][3];
   TGraphErrors* gr_sigma_as[N_ARMSECT][3];
   for (int ias = 0; ias < N_ARMSECT; ias++) {
     //     for(Int_t iv = 0; iv < 3; iv++){
     for(Int_t iv = 2; iv < 3; iv++){
       gr_mean_as [ias][iv] = new TGraphErrors();
       gr_sigma_as[ias][iv] = new TGraphErrors();
       sprintf(grname, "gr_mean_as%i_v%i", ias, iv);
       gr_mean_as [ias][iv]->SetName(grname);
       sprintf(grname, "gr_sigma_as%i_v%i", ias, iv);
       gr_sigma_as[ias][iv]->SetName(grname);
     }
   }

   int i_run = 0;
   for (map<int, TH1D*>::iterator iter = m_h1_pi0mass_PbGl_run[2].begin();
        iter != m_h1_pi0mass_PbGl_run[2].end(); iter++) {
      int run = iter->first;

      //      for(Int_t iv = 0; iv < 3; iv++){
      for(Int_t iv = 2; iv < 3; iv++){
	fitter->SetHist(m_h1_pi0mass_PbGl_run[iv][run]);
	fitter->FitMass();
	fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);
	gr_mean_PbGl[iv] ->SetPoint(i_run, run, mean);
	gr_sigma_PbGl[iv]->SetPoint(i_run, run, sigma);
	gr_mean_PbGl[iv] ->SetPointError(i_run, 0, mean_err);
	gr_sigma_PbGl[iv]->SetPointError(i_run, 0, sigma_err);
	
	fitter->SetHist(m_h1_pi0mass_PbSc_run[iv][run]);
	fitter->FitMass();
	fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);
	gr_mean_PbSc[iv] ->SetPoint(i_run, run, mean);
	gr_sigma_PbSc[iv]->SetPoint(i_run, run, sigma);
	gr_mean_PbSc[iv] ->SetPointError(i_run, 0, mean_err);
	gr_sigma_PbSc[iv]->SetPointError(i_run, 0, sigma_err);
      }

      for (int ias = 0; ias < N_ARMSECT; ias++) {
	//	for(Int_t iv = 0; iv < 3; iv++){
	for(Int_t iv = 2; iv < 3; iv++){
	  fitter->SetHist(m_h1_pi0mass_as_run[ias][iv][run]);
	  fitter->FitMass();
	  fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);
	  gr_mean_as [ias][iv]->SetPoint(i_run, run, mean);
	  gr_sigma_as[ias][iv]->SetPoint(i_run, run, sigma);
	  gr_mean_as [ias][iv]->SetPointError(i_run, 0, mean_err);
	  gr_sigma_as[ias][iv]->SetPointError(i_run, 0, sigma_err);
	}
      }
      i_run++;

      //      for(Int_t iv = 0; iv < 3; iv++){
      for(Int_t iv = 2; iv < 3; iv++){
	delete m_h1_pi0mass_PbGl_run[iv][run];
	delete m_h1_pi0mass_PbSc_run[iv][run];
	for (int ias = 0; ias < N_ARMSECT; ias++) {
	  delete m_h1_pi0mass_as_run[ias][iv][run];
	}
      }
   }

   m_ofile->cd();
   //   for(Int_t iv = 0; iv < 3; iv++){
   for(Int_t iv = 2; iv < 3; iv++){
     gr_mean_PbGl[iv] ->Write();
     gr_mean_PbSc[iv] ->Write();
     gr_sigma_PbGl[iv]->Write();
     gr_sigma_PbSc[iv]->Write();
     for (int ias = 0; ias < N_ARMSECT; ias++) {
       gr_mean_as [ias][iv]->Write();
       gr_sigma_as[ias][iv]->Write();
     }
   }
   delete fitter;
}

void Pi0Analysis::Analysis(const char* fname_table_type, const char* fname_table_sector, const char* fname_table_tower)
{
   cout << "Analyzing pi0 mass..." << endl;

   Pi0MassFitter* fitter = new Pi0MassFitter();
   Double_t height, height_err, mean, mean_err, sigma, sigma_err;
   Double_t chi2, signal, bg;
   Int_t NDF, poln;

   m_ofile->cd();

   FILE* of_result;

   if (m_type_by_type) {
//      fn_result = GetEnvSure("PI0_HIST_TABLE_FILE_NAME_TYPE");
      if ( (of_result = fopen(fname_table_type, "w")) == 0 ) {
         fprintf(stderr, "File open error: %s\n", fname_table_type);
         exit(0);
      }

      //fprintf(of_result, "#event: all %e, 4x4b %e, 4x4a %e, 4x4c&&BBCLL1(narrow) %e, MPC&&2x2 %e,  bbc %e, bbc_nar %e\n", 
      fprintf(of_result, "#event: all %e, 4x4b %e, 4x4a %e, ERT_4x4c&BBCLL1 %e, MPC&&2x2 %e,  bbc %e, bbc_nar %e, bbc_nov %e, ERTLL1_E %e, ERTLL1_E&&BBCLL1_nar %e\n", 
              m_nev, m_nev_4x4b, m_nev_4x4a, m_nev_4x4c_bbc_narrow, m_nev_MPC_2x2, m_nev_bbc, m_nev_bbc_narrow, m_nev_novertex, m_nev_4x4b_pure, m_nev_4x4b_e_narrow);//
      //fprintf(of_result, "#nsignalregion: all %e, 4x4b %e, 4x4a %e, 4x4c&&BBCLL1(narrow) %e, MPC&&2x2 %e,  bbc %e, bbc_nar %e\n", 
      fprintf(of_result, "#nsignalregion: all %e, 4x4b %e, 4x4a %e, ERT_4x4c&BBCLL1 %e, MPC&&2x2 %e,  bbc %e, bbc_nar %e, bbc_nov %e, ERTLL1_E %e, ERTLL1_E&&BBCLL1_nar %e\n", 
              m_npi0, m_npi0_4x4b, m_npi0_4x4a, m_npi0_4x4c_bbc_narrow, m_npi0_MPC_2x2, m_npi0_bbc, m_npi0_bbc_narrow, m_npi0_novertex, m_npi0_4x4b_pure,m_npi0_4x4b_e_narrow);//

      fitter->SetHist(m_h1_pi0mass_PbGl[2]);
      fitter->FitMass();
      fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);

      fprintf(of_result,
              "%f %f  %f %f  %f %f  %f %2i  %i  %f  %f\n",
              height, height_err, mean, mean_err, sigma, sigma_err,
              chi2, NDF, poln, signal, bg);

      fitter->SetHist(m_h1_pi0mass_PbSc[2]);
      fitter->FitMass();
      fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);

      fprintf(of_result,
              "%f %f  %f %f  %f %f  %f %2i  %i  %f  %f\n",
              height, height_err, mean, mean_err, sigma, sigma_err,
              chi2, NDF, poln, signal, bg);

      fclose(of_result);
   }

   if (m_sector_by_sector) {
      if ( (of_result = fopen(fname_table_sector, "w")) == 0 ) {
         fprintf(stderr, "File open error: %s\n", fname_table_sector);
         exit(0);
      }

      for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
         fitter->SetHist(m_h1_pi0mass_as[ias][2]);
         fitter->FitMass();
         fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);

         fprintf(of_result,
                 "%i  %f %f  %f %f  %f %f  %f %2i  %i  %f  %f\n",
                 ias, height, height_err, mean, mean_err, sigma, sigma_err,
                 chi2, NDF, poln, signal, bg);
      }

      fprintf(of_result, "\n");
      for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      for (Int_t i = 0; i < 2; i++) {
         fitter->SetHist(m_h1_pi0mass_asns[ias][i][2]);
         fitter->FitMass();
         fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);

         fprintf(of_result,
                 "%i  %i  %f %f  %f %f  %f %f  %f %2i  %i  %f  %f\n",
                 ias, i, height, height_err, mean, mean_err, sigma, sigma_err,
                 chi2, NDF, poln, signal, bg);
      }
      }

      fclose(of_result);
   }

   if (m_tower_by_tower) {
      if ( (of_result = fopen(fname_table_tower, "w")) == 0 ) {
         fprintf(stderr, "File open error: %s\n", fname_table_tower);
         exit(0);
      }

      m_ofile->cd();
      TH1D* h1_height_dist = new TH1D("h1_height_dist", ";height;", 300, 0, 600);
      TH1D* h1_mean_dist   = new TH1D("h1_mean_dist",   ";mean;",   200, 0, 0.3);
      TH1D* h1_sigma_dist  = new TH1D("h1_sigma_dist",  ";sigma;",  100, 0, 0.05);
      TH1D* h1_rchi2_dist  = new TH1D("h1_rchi2_dist",  ";#chi^{2}/NDF;", 100, 0, 10);
      TH1D* h1_bg_dist     = new TH1D("h1_bg_dist",     ";BG/ALL;",  300, 0, 3);


      TH2D* h2_height_study[N_ARMSECT];
   
      for (int ias = 0; ias < N_ARMSECT; ias++) {
	char hname[200];
	sprintf(hname,"height_study%d",ias);
	int _ny, _nz;
	if (IsPbGl(ias)) { _ny = N_YPOS_PBGL;  _nz = N_ZPOS_PBGL; }
	else             { _ny = N_YPOS_PBSC;  _nz = N_ZPOS_PBSC; }
	h2_height_study[ias] = new TH2D(hname, hname, _nz, 0, _nz, _ny, 0, _ny);
      for (int iy = 0; iy < N_YPOS_PBGL; iy++) {
      for (int iz = 0; iz < N_ZPOS_PBGL; iz++) {
      if (IsValidYZ(ias, iy, iz)) {
         fitter->SetHist(m_h1_pi0mass_tower[ias][iy][iz][2]);
         fitter->FitMass();
         fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);
         
         Double_t coef     = m_recal_ecore->GetCoef(ias, iy, iz);
         Double_t new_coef = CalcNewCoef(coef, mean);
         Double_t new_coef_err = CalcNewCoefErr(new_coef, mean, mean_err);

         fprintf(of_result,
                 "%i %2i %2i  %f %f %f  %f %f  %f %f  %f %f  %f %2i  %i  %f  %f\n",
                 ias, iy, iz, coef, new_coef, new_coef_err,
                 height, height_err, mean, mean_err, sigma, sigma_err,
                 chi2, NDF, poln, signal, bg);
         
         if (! IsEdgePos(ias, iy, iz) && ! uncalib_list->IsInUncalibList(ias, iy, iz)) {
            h1_height_dist->Fill(height);
	    h2_height_study[ias]->Fill(iz,iy,height);
            h1_mean_dist  ->Fill(mean);
            h1_sigma_dist ->Fill(sigma);
            h1_rchi2_dist ->Fill(chi2 / NDF);
            h1_bg_dist    ->Fill(bg / (signal + bg));
         }
      }
      }
      }
      }

      ///
      for (int ias = 0; ias < N_ARMSECT; ias++) {
      for (int ism = 0; ism < N_SUPERMOD; ism++) {
      if (IsValidSM(ias, ism)) {
         fitter->SetHist(m_h1_pi0mass_supermod[ias][ism][2]);
         fitter->FitMass();
         fitter->GetResults(height, height_err, mean, mean_err, sigma, sigma_err, chi2, NDF, poln, signal, bg);
         
        
      }
      }
      }


      ///
      
      m_ofile->Write();
      m_ofile->Close();
   

      fclose(of_result);
   }

   delete fitter;
}

