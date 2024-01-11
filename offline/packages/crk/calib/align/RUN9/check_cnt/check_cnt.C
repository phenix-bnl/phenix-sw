#include "check_cnt.h"

using namespace std;
using namespace findNode;

check_cnt::check_cnt(const char* outfile) : SubsysReco("check_cnt")
{
  strcpy(OutFileName, outfile);
  cout <<" Histograms will be saved to : " << OutFileName << endl;
}

int check_cnt::Init(PHCompositeNode *topNode)
{
  ncalls  = 0;
  n_event = 0;
  
  fout = new TFile(OutFileName,"recreate");

  for(int i=0; i<4; i++){
    switch(i){
    case 0: sprintf(sname, "WN"); break;
    case 1: sprintf(sname, "WS"); break;
    case 2: sprintf(sname, "EN"); break;
    case 3: sprintf(sname, "ES"); break;
    }

    sprintf(hname,  "hn0%s", sname);
    sprintf(htitle, "n0 distribution for %s", sname);
    hn0[i] = new TH1F(hname, htitle, 10, 0, 10);
    hn0[i]->Sumw2();

    sprintf(hname,  "hn1%s", sname);
    sprintf(htitle, "n1 distribution for %s", sname);
    hn1[i] = new TH1F(hname, htitle, 10, 0, 10);
    hn1[i]->Sumw2();
 
    sprintf(hname,  "hsn0%s", sname);
    sprintf(htitle, "sn0 distribution for %s", sname);
    hsn0[i] = new TH1F(hname, htitle, 10, 0, 10);
    hsn0[i]->Sumw2();

    sprintf(hname,  "hsn1%s", sname);
    sprintf(htitle, "sn1 distribution for %s", sname);
    hsn1[i] = new TH1F(hname, htitle, 10, 0, 10);
    hsn1[i]->Sumw2();

    sprintf(hname,  "hnpe0%s", sname);
    sprintf(htitle, "npe0 distribution for %s", sname);
    hnpe0[i] = new TH1F(hname, htitle, 10, 0, 10);
    hnpe0[i]->Sumw2();

    sprintf(hname,  "hnpe1%s", sname);
    sprintf(htitle, "npe1 distribution for %s", sname);
    hnpe1[i] = new TH1F(hname, htitle, 10, 0, 10);
    hnpe1[i]->Sumw2();

    sprintf(hname,  "hsnpe0%s", sname);
    sprintf(htitle, "snpe0 distribution for %s", sname);
    hsnpe0[i] = new TH1F(hname, htitle, 10, 0, 10);
    hsnpe0[i]->Sumw2();

    sprintf(hname,  "hsnpe1%s", sname);
    sprintf(htitle, "snpe1 distribution for %s", sname);
    hsnpe1[i] = new TH1F(hname, htitle, 10, 0, 10);
    hsnpe1[i]->Sumw2();

    sprintf(hname,  "hdisp%s", sname);
    sprintf(htitle, "disp distribution for %s", sname);
    hdisp[i] = new TH1F(hname, htitle, 250, -0.5, 24.5);
    hdisp[i]->Sumw2();

    sprintf(hname,  "hchi2%s", sname);
    sprintf(htitle, "chi2/npe0 distribution for %s", sname);
    hchi2[i] = new TH1F(hname, htitle, 250, -0.5, 24.5);
    hchi2[i]->Sumw2();

    sprintf(hname,  "hpc2dphi%s", sname);
    sprintf(htitle, "pc2dphi distribution for %s", sname);
    hpc2dphi[i] = new TH1F(hname, htitle, 4000, -0.2, 0.2);
    hpc2dphi[i]->Sumw2();

    sprintf(hname,  "hpc2dz%s", sname);
    sprintf(htitle, "pc2dz distribution for %s", sname);
    hpc2dz[i] = new TH1F(hname, htitle, 1000, -50, 50);
    hpc2dz[i]->Sumw2();

    sprintf(hname,  "hpc2sdphi%s", sname);
    sprintf(htitle, "pc2sdphi distribution for %s", sname);
    hpc2sdphi[i] = new TH1F(hname, htitle, 2000, -100, 100);
    hpc2sdphi[i]->Sumw2();

    sprintf(hname,  "hpc2sdz%s", sname);
    sprintf(htitle, "pc2sdz distribution for %s", sname);
    hpc2sdz[i] = new TH1F(hname, htitle, 1000, -50, 50);
    hpc2sdz[i]->Sumw2();

    sprintf(hname,  "hpc3dphi%s", sname);
    sprintf(htitle, "pc3dphi distribution for %s", sname);
    hpc3dphi[i] = new TH1F(hname, htitle, 4000, -0.2, 0.2);
    hpc3dphi[i]->Sumw2();

    sprintf(hname,  "hpc3dz%s", sname);
    sprintf(htitle, "pc3dz distribution for %s", sname);
    hpc3dz[i] = new TH1F(hname, htitle, 1000, -50, 50);
    hpc3dz[i]->Sumw2();

    sprintf(hname,  "hpc3sdphi%s", sname);
    sprintf(htitle, "pc3sdphi distribution for %s", sname);
    hpc3sdphi[i] = new TH1F(hname, htitle, 2000, -100, 100);
    hpc3sdphi[i]->Sumw2();

    sprintf(hname,  "hpc3sdz%s", sname);
    sprintf(htitle, "pc3sdz distribution for %s", sname);
    hpc3sdz[i] = new TH1F(hname, htitle, 1000, -50, 50);
    hpc3sdz[i]->Sumw2();

    sprintf(hname,  "hemcdphi%s", sname);
    sprintf(htitle, "emcdphi distribution for %s", sname);
    hemcdphi[i] = new TH1F(hname, htitle, 4000, -0.2, 0.2);
    hemcdphi[i]->Sumw2();

    sprintf(hname,  "hemcdz%s", sname);
    sprintf(htitle, "emcdz distribution for %s", sname);
    hemcdz[i] = new TH1F(hname, htitle, 1000, -50, 50);
    hemcdz[i]->Sumw2();

    sprintf(hname,  "hemcsdphi%s", sname);
    sprintf(htitle, "emcsdphi distribution for %s", sname);
    hemcsdphi[i] = new TH1F(hname, htitle, 2000, -100, 100);
    hemcsdphi[i]->Sumw2();

    sprintf(hname,  "hemcsdz%s", sname);
    sprintf(htitle, "emcsdz distribution for %s", sname);
    hemcsdz[i] = new TH1F(hname, htitle, 1000, -50, 50);
    hemcsdz[i]->Sumw2();
 }

  hpc1hit = new TH2F("hpc1hit", "pc1 hit map", 200, -100, 100, 500, -1, 4);

  cout << "Complete Initialization" << endl;
  
  return 0;
}

int check_cnt::process_event(PHCompositeNode *topNode)
{
  // informational message...
  if (ncalls%1000 == 0) 
  cout << "RICHAlignment_track::process_event Ncalls = " << ncalls << endl;

  PHCentralTrack  *d_cnt = getClass<PHCentralTrack>(topNode, "PHCentralTrack"
);

  int tottrk = d_cnt->get_npart();
  ntrk = 0;
  
  arm      = -999;
  side     = -999;
  sect     = -999;
  pc2dphi  = -999;
  pc2dz    = -999;
  pc2sdphi = -999;
  pc2sdz   = -999;

  for(int itrk=0; itrk<tottrk; itrk++)
    {
      pc1id = d_cnt->get_pc1id(itrk);
      pc2id = d_cnt->get_pc2id(itrk);
      pc3id = d_cnt->get_pc3id(itrk);
      emcid = d_cnt->get_emcid(itrk);

      arm     = 1-d_cnt->get_dcarm(itrk);
      side    = d_cnt->get_dcside(itrk);	  
      sect    = d_cnt->get_sect(itrk);
      quality = d_cnt->get_quality(itrk);
      
      pc1x   = d_cnt->get_ppc1x(itrk);
      pc1y   = d_cnt->get_ppc1y(itrk);
      pc1z   = d_cnt->get_ppc1z(itrk);
      pc1phi = atan2(pc1y, pc1x);
      if(pc1phi<-1) pc1phi = pc1phi + 6.283184;
      if(quality>9) hpc1hit->Fill(pc1z, pc1phi);

      if(emcid>=0 && pc1id>=0 && pc3id>=0)
	{
	  n0 = d_cnt->get_n0(itrk);
	  n1 = d_cnt->get_n1(itrk);
	  npe0 = d_cnt->get_npe0(itrk);
	  npe1 = d_cnt->get_npe1(itrk);
	  chi2 = d_cnt->get_chi2(itrk);
	  chi2_npe0 = chi2/npe0;
	  disp = d_cnt->get_disp(itrk);
	  sn0 = d_cnt->get_sn0(itrk);
	  sn1 = d_cnt->get_sn1(itrk);
	  snpe0 = d_cnt->get_snpe0(itrk);
	  snpe1 = d_cnt->get_snpe1(itrk);

	  mom = d_cnt->get_mom(itrk);
	  ecore = d_cnt->get_ecore(itrk);
	  ecore_mom = ecore/mom;

	  pc2dz = d_cnt->get_pc2dz(itrk);
	  pc2dphi = d_cnt->get_pc2dphi(itrk);
	  if(arm==1&&pc2dphi>-1000){
	    cout << "Error " << pc2dphi << endl;
	  }
	  pc3dz = d_cnt->get_pc3dz(itrk);
	  pc3dphi = d_cnt->get_pc3dphi(itrk);
	  emcdz_e = d_cnt->get_emcdz(itrk);
	  emcdphi_e = d_cnt->get_emcdphi(itrk);

	  pc2sdz = d_cnt->get_pc2sdz(itrk);
	  pc2sdphi = d_cnt->get_pc2sdphi(itrk);
	  pc3sdz = d_cnt->get_pc3sdz(itrk);
	  pc3sdphi = d_cnt->get_pc3sdphi(itrk);
	  emcsdz_e = d_cnt->get_emcsdz_e(itrk);
	  emcsdphi_e = d_cnt->get_emcsdphi_e(itrk);

	  spc2sdz = d_cnt->get_spc2sdz(itrk);
	  spc2sdphi = d_cnt->get_spc2sdphi(itrk);
	  spc3sdz = d_cnt->get_spc3sdz(itrk);
	  spc3sdphi = d_cnt->get_spc3sdphi(itrk);
	  semcsdz_e = d_cnt->get_semcsdz_e(itrk);
	  semcsdphi_e = d_cnt->get_semcsdphi_e(itrk);

	  if(pc2dz!=-9999 && pc2dphi!=-9999 && pc2sdz!=-9999 && pc2sdphi!=-9999){
	    hpc2dphi[2*arm+side] ->Fill(pc2dphi);
	    hpc2dz[2*arm+side]   ->Fill(pc2dz);
	    hpc2sdphi[2*arm+side]->Fill(pc2sdphi);
	    hpc2sdz[2*arm+side]  ->Fill(pc2sdz);
	  }
	  hpc3dphi[2*arm+side] ->Fill(pc3dphi);
	  hpc3dz[2*arm+side]   ->Fill(pc3dz);
	  hpc3sdphi[2*arm+side]->Fill(pc3sdphi);
	  hpc3sdz[2*arm+side]  ->Fill(pc3sdz);

	  hemcdphi[2*arm+side] ->Fill(emcdphi_e);
	  hemcdz[2*arm+side]   ->Fill(emcdz_e);
	  hemcsdphi[2*arm+side]->Fill(emcsdphi_e);
	  hemcsdz[2*arm+side]  ->Fill(emcsdz_e);
  
	  if(ecore>0.5 && ecore_mom>0.7 && ecore_mom<1.3
	     && pc3sdz<2. && pc3sdz>-2. && pc3sdphi<2. && pc3sdphi>-2. ){
	    hn0[2*arm+side] ->Fill(n0);
	    hn1[2*arm+side] ->Fill(n1);
	    hsn0[2*arm+side]->Fill(sn0);
	    hsn1[2*arm+side]->Fill(sn1);

	    hnpe0[2*arm+side]  ->Fill(npe0);
	    hnpe1[2*arm+side]  ->Fill(npe1);
	    hsnpe0[2*arm+side] ->Fill(snpe0);
	    hsnpe1[2*arm+side] ->Fill(snpe1);

	    hchi2[2*arm+side] ->Fill(chi2_npe0);
	    hdisp[2*arm+side] ->Fill(disp);
	  }
	  
	  ntrk++;
	}
    }
  if(ntrk>0) n_event++;

  ncalls++;
  return 0;
}

int check_cnt::End(PHCompositeNode *topNode)
{
  
  fout->cd();
  for(int i=0; i<4; i++){
    hn0[i]->Write();
    hn0[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hn1[i]->Write();
    hn1[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hsn0[i]->Write();
    hsn0[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hsn1[i]->Write();
    hsn1[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hnpe0[i]->Write();
    hnpe0[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hnpe1[i]->Write();
    hnpe1[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hsnpe0[i]->Write();
    hsnpe0[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hsnpe1[i]->Write();
    hsnpe1[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hdisp[i]->Write();
    hdisp[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hchi2[i]->Write();
    hchi2[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hpc2dz[i]->Write();
    hpc2dz[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hpc2dphi[i]->Write();
    hpc2dphi[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hpc2sdz[i]->Write();
    hpc2sdz[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hpc2sdphi[i]->Write();
    hpc2sdphi[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hpc3dz[i]->Write();
    hpc3dz[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hpc3dphi[i]->Write();
    hpc3dphi[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hpc3sdz[i]->Write();
    hpc3sdz[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hpc3sdphi[i]->Write();
    hpc3sdphi[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hemcdz[i]->Write();
    hemcdz[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hemcdphi[i]->Write();
    hemcdphi[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hemcsdz[i]->Write();
    hemcsdz[i]->Delete();
  }
  for(int i=0; i<4; i++){
    hemcsdphi[i]->Write();
    hemcsdphi[i]->Delete();
  }
  hpc1hit->Write();
  hpc1hit->Delete();

  fout->Close();
  
  cout << n_event << "/" << ncalls << " events were analyzed." << endl;
  return 0;
}


