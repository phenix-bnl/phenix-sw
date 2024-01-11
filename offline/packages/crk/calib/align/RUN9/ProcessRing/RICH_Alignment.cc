#include "RICH_Alignment.h"
#include "CrkGeometryObject_new.hh"

TTree *t_ring ;

RICH_Alignment::RICH_Alignment()
{
  TROOT("RICH_Alignment", "RICH_Alignment");
  i_Eval = 0;

  SetMaxent(-1);
  SetEventOffset(0);
  SetVerbosity(0);
  VerifByPhi(1);

  SetAlignmentFile("alignment.dat");

  memset(f_dz, 0, sizeof(f_dz)); 
  memset(f_dphi, 0, sizeof(f_dphi));
  memset(f_dzfinal, 0, sizeof(f_dzfinal)); 
  memset(f_dphifinal, 0, sizeof(f_dphifinal));
  Dr = 0;
  i_use_hit=1;
  i_end_point=1;
}

RICH_Alignment::RICH_Alignment(char *dstin)
{
  TROOT("RICH_Alignment", "RICH_Alignment");

  i_Eval = 0;
  
  SetMaxent(-1);
  SetEventOffset(0);
  SetVerbosity(0);
  VerifByPhi(1);
  
  SetAlignmentFile("alignment.dat");

  memset(f_dz, 0, sizeof(f_dz)); 
  memset(f_dphi, 0, sizeof(f_dphi));
  memset(f_dzfinal, 0, sizeof(f_dzfinal)); 
  memset(f_dphifinal, 0, sizeof(f_dphifinal));

  memset(i_hotPMT, 0, sizeof(i_hotPMT)); 
  i_nhotPMT = 0;
  Dr = 0;
  i_use_hit=1;
  i_end_point=1;


}

void RICH_Alignment::ImportHotPMTList(char *hotlistf)
{
  ifstream if_hot(hotlistf);
  cout<<"hot pmt list "<<hotlistf<<endl;
  while(!if_hot.eof())
    {
      if_hot >> i_hotPMT[i_nhotPMT];
      cout<<i_hotPMT[i_nhotPMT]<<endl;
      i_nhotPMT++;
    }
  if_hot.close();
}

void RICH_Alignment::SetEval(char *evalout)
{
  i_Eval=1;
  tf_eval = new TFile(evalout,"RECREATE");

  t_ring = new TTree("t_ring","summary of the tree");
  t_ring->Branch("run",&d_run,"run/I");
  t_ring->Branch("ntrk",&d_ntrk,"ntrk/I");
  t_ring->Branch("bbcz",&d_bbcz,"bbcz/F");
  t_ring->Branch("bbcq",&d_bbcq,"bbcq/F");
  t_ring->Branch("zdce",&d_zdce,"zdce/F");
  t_ring->Branch("arm",&d_arm,"arm/I");
  t_ring->Branch("side",&d_side,"side/I");
  t_ring->Branch("panel",&d_panel,"panel/I");
  t_ring->Branch("sside",&d_sside,"sside/I");
  t_ring->Branch("spanel",&d_spanel,"spanel/I");
  t_ring->Branch("v_ref",d_vref,"v_ref[3]/F");
  t_ring->Branch("b_ref",d_bref,"b_ref[3]/F");
  t_ring->Branch("sv_ref",d_svref,"sv_ref[3]/F");
  t_ring->Branch("sb_ref",d_sbref,"sb_ref[3]/F");


  t_ring->Branch("npmt",&d_npmt,"npmt/I");
  t_ring->Branch("pmtid",d_pmtid,"pmtid[npmt]/I");
  t_ring->Branch("npe",d_npe,"npe[npmt]/F");
  t_ring->Branch("tcrk",d_tcrk,"tcrk[npmt]/F");
  t_ring->Branch("posx",d_posx,"posx[npmt]/F");
  t_ring->Branch("posy",d_posy,"posy[npmt]/F");
  t_ring->Branch("posz",d_posz,"posz[npmt]/F");
  t_ring->Branch("R",d_R,"R[npmt]/F");
  t_ring->Branch("X",d_X,"X[npmt]/F");
  t_ring->Branch("Y",d_Y,"Y[npmt]/F");

  t_ring->Branch("snpmt",&d_snpmt,"snpmt/I");
  t_ring->Branch("spmtid",d_spmtid,"spmtid[snpmt]/I");
  t_ring->Branch("snpe",d_snpe,"snpe[snpmt]/F");
  t_ring->Branch("stcrk",d_stcrk,"stcrk[snpmt]/F");
  t_ring->Branch("sposx",d_sposx,"sposx[snpmt]/F");
  t_ring->Branch("sposy",d_sposy,"sposy[snpmt]/F");
  t_ring->Branch("sposz",d_sposz,"sposz[snpmt]/F");
  t_ring->Branch("sR",d_sR,"sR[snpmt]/F");
  t_ring->Branch("sX",d_sX,"sX[snpmt]/F");
  t_ring->Branch("sY",d_sY,"sY[snpmt]/F");

  t_ring->Branch("mom",&d_mom,"mom/F");
  t_ring->Branch("ecore",&d_ecore,"ecore/F");
  t_ring->Branch("n0",&d_n0,"n0/I");
  t_ring->Branch("sn0",&d_sn0,"sn0/I");
  t_ring->Branch("n0_new",&d_n0_new,"n0_new/I");
  

}
void RICH_Alignment::Init(char *infile)
{


  fin = new TFile(infile,"read");
  trk = (TTree*)fin->Get("trk");


  if(!trk)
    {
      cout<<" track tree not found !"<<endl;
      exit(1);
    }

  
  trk->SetBranchAddress("run", &run);
  trk->SetBranchAddress("bbcz", &bbcz);
  trk->SetBranchAddress("bbcq", &bbcq);
  trk->SetBranchAddress("zdce", &zdce);
  trk->SetBranchAddress("ntrk", &tmp_ntrk);
  trk->SetBranchAddress("arm", &arm);
  trk->SetBranchAddress("side", &side);
  trk->SetBranchAddress("sside", &sside);
  trk->SetBranchAddress("panel", &panel);
  trk->SetBranchAddress("spanel", &spanel);
  trk->SetBranchAddress("v_ref", v_ref);
  trk->SetBranchAddress("b_ref", b_ref);
  trk->SetBranchAddress("sv_ref", sv_ref);
  trk->SetBranchAddress("sb_ref", sb_ref);
  trk->SetBranchAddress("npmt", &this_npmt);
  trk->SetBranchAddress("posx", posx);
  trk->SetBranchAddress("posy", posy);
  trk->SetBranchAddress("posz", posz);
  trk->SetBranchAddress("posr", posr);
  trk->SetBranchAddress("posphi", posphi);
  trk->SetBranchAddress("pmt", pmt);
  trk->SetBranchAddress("npe", npe);
  trk->SetBranchAddress("tcrk", tcrk);

  trk->SetBranchAddress("snpmt", &this_snpmt);
  trk->SetBranchAddress("sposx", sposx);
  trk->SetBranchAddress("sposy", sposy);
  trk->SetBranchAddress("sposz", sposz);
  trk->SetBranchAddress("sposr", sposr);
  trk->SetBranchAddress("sposphi", sposphi);
  trk->SetBranchAddress("spmt", spmt);
  trk->SetBranchAddress("snpe", snpe);
  trk->SetBranchAddress("stcrk", stcrk);


  trk->SetBranchAddress("n0", &n0);  
  trk->SetBranchAddress("sn0", &sn0);  
  trk->SetBranchAddress("ecore", &ecore);  
  trk->SetBranchAddress("mom", &mom);  
  trk->SetBranchAddress("ppc1pos", ppc1pos);  
  trk->SetBranchAddress("ppc2pos", ppc2pos);  
  trk->SetBranchAddress("ppc3pos", ppc3pos);  
  trk->SetBranchAddress("pc2dphi", &pc2dphi);  
  trk->SetBranchAddress("pc2dz", &pc2dz);  
  trk->SetBranchAddress("pc3dphi", &pc3dphi);  
  trk->SetBranchAddress("pc3dz", &pc3dz);  
  trk->SetBranchAddress("emcdphi", &emcdphi);  
  trk->SetBranchAddress("emcdz", &emcdz);  
  trk->SetBranchAddress("zed", &zed);  
  trk->SetBranchAddress("flag", &flag);  

  cgo = new CrkGeometryObject_new();
  cout<<" Mirror offset x-y-z "<<offset_x<<" "<<offset_y<<" "<<offset_z<<endl;
  cgo->SetMirrorPosX(offset_x);
  cgo->SetMirrorPosY(offset_y);
  cgo->SetMirrorPosZ(offset_z);
  cgo->UseSurvey(); // April 12 2006


}

void RICH_Alignment::ProcessRings(int iloop)
{  
  ntrk = (int) trk->GetEntries();

  

  TCanvas *tc_printed[4];

  TH2F *h_ring[2][2][24];
  TH2F *h_sring[2][2][24];
  TH2F *hsub_ring[2][2][24];

  TH1F *h_r[2][2][24];
  TH1F *h_sr[2][2][24];
  TH1F *hsub_r[2][2][24];

  TH2F *h_ringtot = NULL;
  TH2F *h_sringtot = NULL;
  TH2F *hsub_ringtot = NULL;

  TH1F *h_R = NULL;
  TH1F *h_sR = NULL;
  TH1F *hsub_R = NULL;

  TH2F *h_ringinit = NULL;
  TH2F *h_sringinit = NULL;
  TH2F *hsub_ringinit = NULL;

  TH1F *h_R_init = NULL;
  TH1F *h_sR_init = NULL;
  TH1F *hsub_R_init = NULL;


  TH1F *h_n0_0[2][2][24];
  TH1F *h_n0_1[2][2][24];
  TH1F *h_n0_2[2][2][24];
  TH1F *h_n0_3[2][2][24];
  TH1F *h_n0_4[2][2][24];
  TH1F *h_n1_0[2][2][24];
  TH1F *h_n1_1[2][2][24];
  TH1F *h_n1_2[2][2][24];
  TH1F *h_n1_3[2][2][24];
  TH1F *h_n1_4[2][2][24];
  TH1F *h_npe0_0[2][2][24];
  TH1F *h_npe0_1[2][2][24];
  TH1F *h_npe0_2[2][2][24];
  TH1F *h_npe0_3[2][2][24];
  TH1F *h_npe0_4[2][2][24];
  TH1F *h_npe1_0[2][2][24];
  TH1F *h_npe1_1[2][2][24];
  TH1F *h_npe1_2[2][2][24];
  TH1F *h_npe1_3[2][2][24];
  TH1F *h_npe1_4[2][2][24];
  TH1F *h_chi2_0[2][2][24];
  TH1F *h_chi2_1[2][2][24];
  TH1F *h_chi2_2[2][2][24];
  TH1F *h_chi2_3[2][2][24];
  TH1F *h_chi2_4[2][2][24];

  
  Text_t drawn[100], tsect[10], r_drawn[100];
  Text_t name_n0_0[100], name_n0_1[100], name_n0_2[100], name_n0_3[100], 
    name_n0_4[100];
  Text_t name_n1_0[100], name_n1_1[100], name_n1_2[100], name_n1_3[100], 
    name_n1_4[100]; 
  Text_t name_npe0_0[100], name_npe0_1[100], name_npe0_2[100], 
    name_npe0_3[100], name_npe0_4[100]; 
  Text_t name_npe1_0[100], name_npe1_1[100], name_npe1_2[100], 
    name_npe1_3[100], name_npe1_4[100];
  Text_t name_chi2_0[100], name_chi2_1[100], name_chi2_2[100], 
    name_chi2_3[100], name_chi2_4[100];

  if(iloop == 20){
    h_ringtot = new TH2F("h_ringtot", "Accumulated for all panels for QA",
			 60, -15, 15, 60, -15, 15);
    h_R = new TH1F("h_R", "Distance from ring center", 150, 0, 30);
    
    h_sringtot = new TH2F("h_sringtot", "swap Accumulated for all panels for QA",
			   60, -15, 15, 60, -15, 15);
    h_sR = new TH1F("h_sR", "swap Distance from ring center", 150, 0, 30);

    
    hsub_ringtot = new TH2F("hsub_ringtot", "(S+B)-B : Accumulated for all panels for QA",
			    60, -15, 15, 60, -15, 15);
    hsub_R = new TH1F("hsub_R", "(S+B)-B: Distance from ring center", 150, 0, 30);

    hsub_ringtot->Sumw2();
    hsub_R->Sumw2();

  }
  if(iloop == -1){
    h_ringinit = new TH2F("h_ringinit",
			  "Accumulated for all panels with non-aligned",
			  60, -15, 15, 60, -15, 15);
    h_R_init = new TH1F("h_R_init", "Distance from ring center", 150, 0, 30);
    
    h_sringinit = new TH2F("h_sringinit",
			   "swap Accumulated for all panels with non-aligned",
			   60, -15, 15, 60, -15, 15);
    h_sR_init = new TH1F("h_sR_init", "swap Distance from ring center", 150, 0, 30);
    
    hsub_ringinit  = new TH2F("hsub_ringinit", "(S+B)-B:Accumulated for all panels with non-aligned",
			      60, -15, 15, 60, -15, 15);
    hsub_R_init = new TH1F("hsub_R_init", "(S+B)-B: Distance from ring center", 150, 0, 30);

    hsub_ringinit->Sumw2();
    hsub_R_init->Sumw2();
  }

  for(int iarm=0; iarm<2; iarm++)
    {
      for(int iside=0; iside<2; iside++)
	{
	  switch(iarm *2 + iside)
	    {
	    case 0:    sprintf(tsect, "WS"); break;
	    case 1:    sprintf(tsect, "WN"); break;
	    case 2:    sprintf(tsect, "ES"); break;
	    case 3:    sprintf(tsect, "EN"); break;
	    }
	  if(iloop == 20)
	    {
	      tc_printed[iarm*2+iside] = new TCanvas(tsect, tsect, 
						     10, 10, 600, 400);
	    }
	  for(int ip=0; ip<24; ip++)
	    {
	      sprintf(drawn, "%s_%d_%d", tsect, ip, iloop);
	      h_ring[iarm][iside][ip] = new TH2F(drawn, drawn, 
						 60, -15, 15, 60, -15, 15);
	      sprintf(r_drawn, "r_%s_%d_%d", tsect, ip, iloop);
	      h_r[iarm][iside][ip] = new TH1F(r_drawn, r_drawn, 150, 0, 30);

	      sprintf(drawn, "%s_swap_%d_%d", tsect, ip, iloop);
	      h_sring[iarm][iside][ip] = new TH2F(drawn, drawn, 
						 60, -15, 15, 60, -15, 15);
	      sprintf(r_drawn, "r_swap_%s_%d_%d", tsect, ip, iloop);
	      h_sr[iarm][iside][ip] = new TH1F(r_drawn, r_drawn, 150, 0, 30);

	      sprintf(drawn, "%s_sub_%d_%d", tsect, ip, iloop);
	      hsub_ring[iarm][iside][ip] = new TH2F(drawn, drawn, 
						 60, -15, 15, 60, -15, 15);
	      sprintf(r_drawn, "r_sub_%s_%d_%d", tsect, ip, iloop);
	      hsub_r[iarm][iside][ip] = new TH1F(r_drawn, r_drawn, 150, 0, 30);

	      hsub_ring[iarm][iside][ip]->Sumw2();
	      hsub_r[iarm][iside][ip]->Sumw2();

	      sprintf(name_n0_0, "n0_0_%s_%d_%d", tsect, ip, iloop);
	      h_n0_0[iarm][iside][ip] = 
		new TH1F(name_n0_0, name_n0_0, 15, 0, 15);
	      sprintf(name_n0_1, "n0_1_%s_%d_%d", tsect, ip, iloop);
	      h_n0_1[iarm][iside][ip] = 
		new TH1F(name_n0_1, name_n0_1, 15, 0, 15);
	      sprintf(name_n0_2, "n0_2_%s_%d_%d", tsect, ip, iloop);
	      h_n0_2[iarm][iside][ip] = 
		new TH1F(name_n0_2, name_n0_2, 15, 0, 15);
	      sprintf(name_n0_3, "n0_3_%s_%d_%d", tsect, ip, iloop);
	      h_n0_3[iarm][iside][ip] = 
		new TH1F(name_n0_3, name_n0_3, 15, 0, 15);
	      sprintf(name_n0_4, "n0_4_%s_%d_%d", tsect, ip, iloop);
	      h_n0_4[iarm][iside][ip] = 
		new TH1F(name_n0_4, name_n0_4, 15, 0, 15);

	      sprintf(name_n1_0, "n1_0_%s_%d_%d", tsect, ip, iloop);
	      h_n1_0[iarm][iside][ip] = 
		new TH1F(name_n1_0, name_n1_0, 15, 0, 15);
	      sprintf(name_n1_1, "n1_1_%s_%d_%d", tsect, ip, iloop);
	      h_n1_1[iarm][iside][ip] = 
		new TH1F(name_n1_1, name_n1_1, 15, 0, 15);
	      sprintf(name_n1_2, "n1_2_%s_%d_%d", tsect, ip, iloop);
	      h_n1_2[iarm][iside][ip] = 
		new TH1F(name_n1_2, name_n1_2, 15, 0, 15);
	      sprintf(name_n1_3, "n1_3_%s_%d_%d", tsect, ip, iloop);
	      h_n1_3[iarm][iside][ip] = 
		new TH1F(name_n1_3, name_n1_3, 15, 0, 15);
	      sprintf(name_n1_4, "n1_4_%s_%d_%d", tsect, ip, iloop);
	      h_n1_4[iarm][iside][ip] = 
		new TH1F(name_n1_4, name_n1_4, 15, 0, 15);

	      sprintf(name_npe0_0, "npe0_0_%s_%d_%d", tsect, ip, iloop);
	      h_npe0_0[iarm][iside][ip] = 
		new TH1F(name_npe0_0, name_npe0_0, 40, 0, 40);
	      sprintf(name_npe0_1, "npe0_1_%s_%d_%d", tsect, ip, iloop);
	      h_npe0_1[iarm][iside][ip] = 
		new TH1F(name_npe0_1, name_npe0_1, 40, 0, 40);
	      sprintf(name_npe0_2, "npe0_2_%s_%d_%d", tsect, ip, iloop);
	      h_npe0_2[iarm][iside][ip] = 
		new TH1F(name_npe0_2, name_npe0_2, 40, 0, 40);
	      sprintf(name_npe0_3, "npe0_3_%s_%d_%d", tsect, ip, iloop);
	      h_npe0_3[iarm][iside][ip] = 
		new TH1F(name_npe0_3, name_npe0_3, 40, 0, 40);
	      sprintf(name_npe0_4, "npe0_4_%s_%d_%d", tsect, ip, iloop);
	      h_npe0_4[iarm][iside][ip] = 
		new TH1F(name_npe0_4, name_npe0_4, 40, 0, 40);

	      sprintf(name_npe1_0, "npe1_0_%s_%d_%d", tsect, ip, iloop);
	      h_npe1_0[iarm][iside][ip] = 
		new TH1F(name_npe1_0, name_npe1_0, 40, 0, 40);
	      sprintf(name_npe1_1, "npe1_1_%s_%d_%d", tsect, ip, iloop);
	      h_npe1_1[iarm][iside][ip] = 
		new TH1F(name_npe1_1, name_npe1_1, 40, 0, 40);
	      sprintf(name_npe1_2, "npe1_2_%s_%d_%d", tsect, ip, iloop);
	      h_npe1_2[iarm][iside][ip] = 
		new TH1F(name_npe1_2, name_npe1_2, 40, 0, 40);
	      sprintf(name_npe1_3, "npe1_3_%s_%d_%d", tsect, ip, iloop);
	      h_npe1_3[iarm][iside][ip] = 
		new TH1F(name_npe1_3, name_npe1_3, 40, 0, 40);
	      sprintf(name_npe1_4, "npe1_4_%s_%d_%d", tsect, ip, iloop);
	      h_npe1_4[iarm][iside][ip] = 
		new TH1F(name_npe1_4, name_npe1_4, 40, 0, 40);

	      sprintf(name_chi2_0, "chi2_0_%s_%d_%d", tsect, ip, iloop);
	      h_chi2_0[iarm][iside][ip] = 
		new TH1F(name_chi2_0, name_chi2_0, 600, 0, 60);
	      sprintf(name_chi2_1, "chi2_1_%s_%d_%d", tsect, ip, iloop);
	      h_chi2_1[iarm][iside][ip] = 
		new TH1F(name_chi2_1, name_chi2_1, 600, 0, 60);
	      sprintf(name_chi2_2, "chi2_2_%s_%d_%d", tsect, ip, iloop);
	      h_chi2_2[iarm][iside][ip] = 
		new TH1F(name_chi2_2, name_chi2_2, 600, 0, 60);
	      sprintf(name_chi2_3, "chi2_3_%s_%d_%d", tsect, ip, iloop);
	      h_chi2_3[iarm][iside][ip] = 
		new TH1F(name_chi2_3, name_chi2_3, 600, 0, 60);
	      sprintf(name_chi2_4, "chi2_4_%s_%d_%d", tsect, ip, iloop);
	      h_chi2_4[iarm][iside][ip] = 
		new TH1F(name_chi2_4, name_chi2_4, 600, 0, 60);
	    } 
	}
    }
  
  if(i_Verb>0 && iloop ==-1) cout << "Total entry picked up is " << ntrk 
				  << endl;
  if(i_Verb>0) cout << iloop << "th loop" << endl;
  if(i_Verb>0) cout << "Dr = " << Dr<<endl;

  if(i_Verb>0){
    if(i_use_hit==1){
      cout<<" i use projection "<<endl;
    }else if(i_use_hit==0){
      cout<<" i use hit "<<endl;
    }

    if(i_end_point==0){
      cout<<" i use pc2 (pc3) as end point for west (east)"<<endl;
    }else if(i_end_point==1){
      cout<<" i use pc3 (pc3) as end point for west (east)"<<endl;
    }

  }

  for(int itrk=0; itrk<ntrk; itrk++)
    {
      if(i_Verb>2) cout << itrk << "th track " << arm << " " << this_npmt 
      			<< endl;
      trk->GetEntry(itrk);
      //      if(ecore <= 0.5) continue;
      //      if(ecore <= 0.5 || n0 <= 2) continue;
      //if(ecore <= 0.5 || n0 < 0) continue;
      if(ecore <= 0.75) continue;
      //      if(n0 <= 2) continue;
      // Make frame of Reflected line.
      // Z is the reflected line. And X is directed to beam axis.


      if(arm==0){
	if(i_end_point==0){
	  if(i_use_hit==1){
	    pstart.setX(ppc1pos[0]);
	    pstart.setY(ppc1pos[1]);
	    pstart.setZ(ppc1pos[2]);
	    pend.setX(ppc2pos[0]);
	    pend.setY(ppc2pos[1]);
	    pend.setZ(ppc2pos[2]);
	  }else{
	    if(flag==1 || flag==111 ||
	       flag==11){
	      pstart.setX(ppc1pos[0]);
	      pstart.setY(ppc1pos[1]);
	      pstart.setZ(ppc1pos[2]);
	      float ppc2r = sqrt(pow(ppc2pos[0],2)+
				 pow(ppc2pos[1],2));
	      float phi_P = atan2(ppc2pos[1], ppc2pos[0]);
	      if(phi_P<-0.5*acos(-1.0)){
		phi_P = phi_P+2*acos(-1.0);
	      }
	      
	      float phi_H = phi_P + pc2dphi;
	      float z_H = ppc2pos[2]+pc2dz;
	      
	      pend.setX(ppc2r*cos(phi_H));
	      pend.setY(ppc2r*sin(phi_H));
	      pend.setZ(z_H);
	    }else{
	      pstart.setX(ppc1pos[0]);
	      pstart.setY(ppc1pos[1]);
	      pstart.setZ(ppc1pos[2]);
	      pend.setX(ppc2pos[0]);
	      pend.setY(ppc2pos[1]);
	      pend.setZ(ppc2pos[2]);
	    }
	  }
	}else if(i_end_point==1){
	  if(i_use_hit==1){
	    pstart.setX(ppc1pos[0]);
	    pstart.setY(ppc1pos[1]);
	    pstart.setZ(ppc1pos[2]);
	    pend.setX(ppc3pos[0]);
	    pend.setY(ppc3pos[1]);
	    pend.setZ(ppc3pos[2]);
	  }else{
	    if(flag==110 ||
	       flag==10 ||
	       flag==111 ||
	       flag==11
	       ){
	      pstart.setX(ppc1pos[0]);
	      pstart.setY(ppc1pos[1]);
	      pstart.setZ(ppc1pos[2]);
	      float ppc3r = sqrt(pow(ppc3pos[0],2)+
				 pow(ppc3pos[1],2));
	      float phi_P = atan2(ppc3pos[1], ppc3pos[0]);
	      if(phi_P<-0.5*acos(-1.0)){
		phi_P = phi_P+2*acos(-1.0);
	      }
	      
	      float phi_H = phi_P + pc3dphi;
	      float z_H = ppc3pos[2]+pc3dz;
	      
	      pend.setX(ppc3r*cos(phi_H));
	      pend.setY(ppc3r*sin(phi_H));
	      pend.setZ(z_H);
	    }else{
	      pstart.setX(ppc1pos[0]);
	      pstart.setY(ppc1pos[1]);
	      pstart.setZ(ppc1pos[2]);
	      pend.setX(ppc3pos[0]);
	      pend.setY(ppc3pos[1]);
	      pend.setZ(ppc3pos[2]);
	    }
	  }
	}
      }else{
	if(i_use_hit==1){
	  pstart.setX(ppc1pos[0]);
	  pstart.setY(ppc1pos[1]);
	  pstart.setZ(ppc1pos[2]);
	  pend.setX(ppc3pos[0]);
	  pend.setY(ppc3pos[1]);
	  pend.setZ(ppc3pos[2]);
	}else{
	  if(flag==110 ||
	     flag==10 ||
	     flag==111 ||
	     flag==11
	     ){
	    pstart.setX(ppc1pos[0]);
	    pstart.setY(ppc1pos[1]);
	    pstart.setZ(ppc1pos[2]);
	    float ppc3r = sqrt(pow(ppc3pos[0],2)+
			       pow(ppc3pos[1],2));
	    float phi_P = atan2(ppc3pos[1], ppc3pos[0]);
	    if(phi_P<-0.5*acos(-1.0)){
		phi_P = phi_P+2*acos(-1.0);
	    }
	    
	    float phi_H = phi_P + pc3dphi;
	    float z_H = ppc3pos[2]+pc3dz;
	    
	    pend.setX(ppc3r*cos(phi_H));
	    pend.setY(ppc3r*sin(phi_H));
	    pend.setZ(z_H);
	  }else{
	    pstart.setX(ppc1pos[0]);
	    pstart.setY(ppc1pos[1]);
	    pstart.setZ(ppc1pos[2]);
	    pend.setX(ppc3pos[0]);
	    pend.setY(ppc3pos[1]);
	    pend.setZ(ppc3pos[2]);
	  }
	}
      }

      if(i_end_point==0 ||
	 i_end_point==1){
	start[0] = pstart.getX();
	start[1] = pstart.getY();
	start[2] = pstart.getZ();
	end[0] = pend.getX();
	end[1] = pend.getY();
	end[2] = pend.getZ();
      }

      if(!(start[0]<=-9999 ||
	   start[1]<=-9999 ||
	   start[2]<=-9999 ||
	   end[0]<=-9999 ||
	   end[1]<=-9999 ||
	   end[2]<=-9999)){
	PHLine trkline(pstart, pend);
	arm = pstart.getX() > 0 ? 0 : 1;
	double path;      
	ref = cgo->Reflect(arm, trkline, side, panel, path);

	if(ref.length()>0){
	  v_ref[0] = ref.getDirection().getX();
	  v_ref[1] = ref.getDirection().getY();
	  v_ref[2] = ref.getDirection().getZ();
	  
	  b_ref[0] = ref.getBasepoint().getX();
	  b_ref[1] = ref.getBasepoint().getY();
	  b_ref[2] = ref.getBasepoint().getZ();
	}
      }
	

      //////////////////////////////////////////
      ////// frist is signal parts

      PHVector newZ(v_ref[0], v_ref[1], v_ref[2]);
      newZ.normalize();
      PHVector newY = newZ.cross(PHVector(0, 0, 1));
      newY.normalize();
      PHVector newX = newY.cross(newZ);
      newX.normalize();

      //   Align Z and Phi axis to positive
      if(newX.getZ()<0) newX = -newX;
      if(i_VerifByPhi==0 && iloop!=20){
	if(newY.getY()<0) newY = -newY;
      }
      newZ = newX.cross(newY);

      if(v_ref[2]!=0 ){	 
	PHPoint base(b_ref[0], b_ref[1], b_ref[2]);    
	PHFrame fr_ref(base, newX, newY, newZ);
	
	npmt0 = npmt1 = 0;
	npe0 = npe1 = chi2 = 0;
	
	d_run = run;
	d_bbcz = bbcz;
	d_ntrk = tmp_ntrk;
	d_bbcq = bbcq;
	d_zdce = zdce;
	d_arm = arm ;
	d_side = side;
	d_panel = panel;
	d_vref[0] = v_ref[0];
	d_vref[1] = v_ref[1];
	d_vref[2] = v_ref[2];
	d_bref[0] = b_ref[0];
	d_bref[1] = b_ref[1];
	d_bref[2] = b_ref[2];
	d_mom = mom;
	d_ecore = ecore;
	d_n0 = n0;
	d_sn0 = sn0;
	d_npmt = this_npmt;
	
	int jpmt=0;
	for(int ipmt=0; ipmt<this_npmt; ipmt++)
	  {
	    int look=1;
	    for(int ibad=0; ibad<i_nhotPMT; ibad++){
	      if(pmt[ipmt]==i_hotPMT[ibad]){
		look = 0;
		break;
	      }
	    }
	    if(look==0){
	      continue;
	    }
	    
	    if(i_Verb>2) cout << ipmt << "th pmt" << endl;
	    //    Set mirror alignment
	    PHPoint pmt_pos(posx[ipmt], posy[ipmt], posz[ipmt]);
	    
	    if(iloop > -1)
	      {
		if(i_VerifByPhi)
		  {
		    posr[ipmt] = sqrt(pow(posx[ipmt],2)+
				      pow(posy[ipmt],2));
		    posphi[ipmt] = atan2(posy[ipmt], posx[ipmt]);
		    if(posphi[ipmt]<-0.5*acos(-1.0)){
		      posphi[ipmt] += 2*acos(-1.0);
		    }

		    pmt_pos.setX(posr[ipmt] 
				 * cos(posphi[ipmt] - 
				       f_dphi[arm][side][panel][iloop]));
		    pmt_pos.setY(posr[ipmt] 
				 * sin(posphi[ipmt] - 
				       f_dphi[arm][side][panel][iloop]));
		    pmt_pos.setZ(posz[ipmt] - f_dz[arm][side][panel][iloop]);
		  }
		else
		  {
		    PHPoint dzdphi(0, f_dphi[arm][side][panel][iloop], 
				   f_dz[arm][side][panel][iloop]);
		    pmt_pos = pmt_pos - dzdphi;
		  }
	      }
	    
	    if(iloop == 20)
	      {
		posr[ipmt] = sqrt(pow(posx[ipmt],2)+
				  pow(posy[ipmt],2));
		posphi[ipmt] = atan2(posy[ipmt], posx[ipmt]);
		if(posphi[ipmt]<-0.5*acos(-1.0)){
		  posphi[ipmt] += 2*acos(-1.0);
		}
		pmt_pos.setX(posr[ipmt] * cos(posphi[ipmt] - 
					      f_dphifinal[arm][side][panel]));
		pmt_pos.setY(posr[ipmt] * sin(posphi[ipmt] - 
					      f_dphifinal[arm][side][panel]));
		pmt_pos.setZ(posz[ipmt] - f_dzfinal[arm][side][panel]);
	      }
	    
	    PHPoint pmt_pos_trans = transformPoint(PHFrame(), pmt_pos, fr_ref);
	    h_ring[arm][side][panel]->Fill(pmt_pos_trans.getX(),
					   pmt_pos_trans.getY());
	    h_r[arm][side][panel]->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
					     pmt_pos_trans.getY()*pmt_pos_trans.getY()));
	    
	    if(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
		    pmt_pos_trans.getY()*pmt_pos_trans.getY())>3.4
	       &&sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
		      pmt_pos_trans.getY()*pmt_pos_trans.getY())<8.4
	       &&npe[ipmt]<8
	       ) 
	      {
		npmt0++;
		npe0+= npe[ipmt];
	      }
	    if(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
		    pmt_pos_trans.getY()*pmt_pos_trans.getY())<11.
	       &&npe[ipmt]<8
	       ) 
	      {
		npmt1 ++;
		npe1 += npe[ipmt];
		chi2 += (sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
			      pmt_pos_trans.getY()*pmt_pos_trans.getY())-5.9)*
		  (sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
			pmt_pos_trans.getY()*pmt_pos_trans.getY())-5.9)*
		  npe[ipmt];
	      }
	    
	    
	    if(iloop == 20)
	      {
		h_ringtot->Fill(pmt_pos_trans.getX(),
				pmt_pos_trans.getY());
		h_R->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
			       pmt_pos_trans.getY()*pmt_pos_trans.getY()));
	      }
	    
	    if(iloop == -1)
	      {
		h_ringinit->Fill(pmt_pos_trans.getX(),
				 pmt_pos_trans.getY());
		h_R_init->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
				    pmt_pos_trans.getY()*pmt_pos_trans.getY()));
	      }
	    
	    
	    if(iloop==20){
	      d_pmtid[jpmt] = pmt[ipmt];
	      d_posx[jpmt] = posx[ipmt];
	      d_posy[jpmt] = posy[ipmt];
	      d_posz[jpmt] = posz[ipmt];
	      d_R[jpmt] = sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
			       pmt_pos_trans.getY()*pmt_pos_trans.getY());
	      d_X[jpmt] = pmt_pos_trans.getX();
	      d_Y[jpmt] = pmt_pos_trans.getY();
	      
	      d_npe[jpmt] = npe[ipmt];
	      jpmt++ ;
	    }
	}

	if(iloop == 20){
	  if(npmt0>=0) h_n0_0[arm][side][panel]->Fill(npmt0);
	  if(npmt0>=1) h_n0_1[arm][side][panel]->Fill(npmt0);
	  if(npmt0>=2) h_n0_2[arm][side][panel]->Fill(npmt0);
	  if(npmt0>=3) h_n0_3[arm][side][panel]->Fill(npmt0);
	  if(npmt0>=4) h_n0_4[arm][side][panel]->Fill(npmt0);
	  
	  if(npmt0>=0) h_npe0_0[arm][side][panel]->Fill(npe0);
	  if(npmt0>=1) h_npe0_1[arm][side][panel]->Fill(npe0);
	  if(npmt0>=2) h_npe0_2[arm][side][panel]->Fill(npe0);
	  if(npmt0>=3) h_npe0_3[arm][side][panel]->Fill(npe0);
	  if(npmt0>=4) h_npe0_4[arm][side][panel]->Fill(npe0);
	  
	  if(npmt1>=0) h_n1_0[arm][side][panel]->Fill(npmt1);
	  if(npmt1>=1) h_n1_1[arm][side][panel]->Fill(npmt1);
	  if(npmt1>=2) h_n1_2[arm][side][panel]->Fill(npmt1);
	  if(npmt1>=3) h_n1_3[arm][side][panel]->Fill(npmt1);
	  if(npmt1>=4) h_n1_4[arm][side][panel]->Fill(npmt1);
	  
	  if(npmt1>=0) h_npe1_0[arm][side][panel]->Fill(npe1);
	  if(npmt1>=1) h_npe1_1[arm][side][panel]->Fill(npe1);
	  if(npmt1>=2) h_npe1_2[arm][side][panel]->Fill(npe1);
	  if(npmt1>=3) h_npe1_3[arm][side][panel]->Fill(npe1);
	  if(npmt1>=4) h_npe1_4[arm][side][panel]->Fill(npe1);
	  
	  if(npmt0>=0) h_chi2_0[arm][side][panel]->Fill(chi2/npe0);
	  if(npmt0>=1) h_chi2_1[arm][side][panel]->Fill(chi2/npe0);
	  if(npmt0>=2) h_chi2_2[arm][side][panel]->Fill(chi2/npe0);
	  if(npmt0>=3) h_chi2_3[arm][side][panel]->Fill(chi2/npe0);
	  if(npmt0>=4) h_chi2_4[arm][side][panel]->Fill(chi2/npe0);
	}
      }


      ////////////////// look at swap /////////////////////////////////////
      if(sv_ref[2]!=0 ){	 
	PHVector snewZ(sv_ref[0], sv_ref[1], sv_ref[2]);
	snewZ.normalize();
	PHVector snewY = snewZ.cross(PHVector(0, 0, 1));
	snewY.normalize();
	PHVector snewX = snewY.cross(snewZ);
	snewX.normalize();
	
	//   Align Z and Phi axis to positive
	if(snewX.getZ()<0) snewX = -snewX;
	if(i_VerifByPhi==0 && iloop!=20){
	  if(snewY.getY()<0) snewY = -snewY;
	}
	snewZ = snewX.cross(snewY);
	
	PHPoint sbase(sb_ref[0], sb_ref[1], sb_ref[2]);    
	PHFrame sfr_ref(sbase, snewX, snewY, snewZ);
	
	npmt0 = npmt1 = 0;
	npe0 = npe1 = chi2 = 0;
	
	d_sside = sside;
	d_spanel = spanel;
	d_svref[0] = sv_ref[0];
	d_svref[1] = sv_ref[1];
	d_svref[2] = sv_ref[2];
	d_sbref[0] = sb_ref[0];
	d_sbref[1] = sb_ref[1];
	d_sbref[2] = sb_ref[2];
	d_snpmt = this_snpmt;
	
	int jpmt=0;
	for(int ipmt=0; ipmt<this_snpmt; ipmt++)
	  {
	    int look=1;
	    for(int ibad=0; ibad<i_nhotPMT; ibad++){
	      if(spmt[ipmt]==i_hotPMT[ibad]){
		look = 0;
		break;
	      }
	    }
	    if(look==0){
	      continue;
	    }
	    
	    if(i_Verb>2) cout << ipmt << "th pmt" << endl;
	    //    Set mirror alignment
	    PHPoint pmt_pos(sposx[ipmt], sposy[ipmt], sposz[ipmt]);
	    
	    if(iloop > -1)
	      {
		if(i_VerifByPhi)
		  {
		    sposr[ipmt] = sqrt(pow(sposx[ipmt],2)+
				      pow(sposy[ipmt],2));
		    sposphi[ipmt] = atan2(sposy[ipmt], sposx[ipmt]);
		    if(sposphi[ipmt]<-0.5*acos(-1.0)){
		      sposphi[ipmt] += 2*acos(-1.0);
		    }
		    pmt_pos.setX(sposr[ipmt] 
				 * cos(sposphi[ipmt] - 
				       f_dphi[arm][sside][spanel][iloop]));
		    pmt_pos.setY(sposr[ipmt] 
				 * sin(sposphi[ipmt] - 
				       f_dphi[arm][sside][spanel][iloop]));
		    pmt_pos.setZ(sposz[ipmt] - f_dz[arm][sside][spanel][iloop]);
		  }
		else
		  {
		    PHPoint dzdphi(0, f_dphi[arm][sside][spanel][iloop], 
				   f_dz[arm][sside][spanel][iloop]);
		    pmt_pos = pmt_pos - dzdphi;
		  }
	      }
	    
	    if(iloop == 20)
	      {
		sposr[ipmt] = sqrt(pow(sposx[ipmt],2)+
				   pow(sposy[ipmt],2));
		sposphi[ipmt] = atan2(sposy[ipmt], sposx[ipmt]);
		if(sposphi[ipmt]<-0.5*acos(-1.0)){
		  sposphi[ipmt] += 2*acos(-1.0);
		}
		pmt_pos.setX(sposr[ipmt] * cos(sposphi[ipmt] - 
					       f_dphifinal[arm][sside][spanel]));
		pmt_pos.setY(sposr[ipmt] * sin(sposphi[ipmt] - 
					       f_dphifinal[arm][sside][spanel]));
		pmt_pos.setZ(sposz[ipmt] - f_dzfinal[arm][sside][spanel]);
	      }
	    
	    PHPoint pmt_pos_trans = transformPoint(PHFrame(), pmt_pos, sfr_ref);
	    //// ******* [side][panel] not [sside][spanel] !
	    h_sring[arm][sside][spanel]->Fill(pmt_pos_trans.getX(),
					      pmt_pos_trans.getY());
	    h_sr[arm][sside][spanel]->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
						pmt_pos_trans.getY()*pmt_pos_trans.getY()));

	    /*	    
	    if(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
		    pmt_pos_trans.getY()*pmt_pos_trans.getY())>3.4
	       &&sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
		      pmt_pos_trans.getY()*pmt_pos_trans.getY())<8.4
	       &&snpe[ipmt]<8
	       ) 
	      {
		npmt0++;
		npe0+= snpe[ipmt];
	      }
	    if(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
		    pmt_pos_trans.getY()*pmt_pos_trans.getY())<11.
	       &&snpe[ipmt]<8
	       ) 
	      {
		npmt1 ++;
		npe1 += snpe[ipmt];
		chi2 += (sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
			      pmt_pos_trans.getY()*pmt_pos_trans.getY())-5.9)*
		  (sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
			pmt_pos_trans.getY()*pmt_pos_trans.getY())-5.9)*
		  snpe[ipmt];
	      }
	    */
	    
	    if(iloop == 20)
	      {
		h_sringtot->Fill(pmt_pos_trans.getX(),
				 pmt_pos_trans.getY());
		h_sR->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
				pmt_pos_trans.getY()*pmt_pos_trans.getY()));
	      }
	    
	    if(iloop == -1)
	      {
		h_sringinit->Fill(pmt_pos_trans.getX(),
				  pmt_pos_trans.getY());
		h_sR_init->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
				     pmt_pos_trans.getY()*pmt_pos_trans.getY()));
	      }
	    

	    if(iloop==20){
	      d_spmtid[jpmt] = spmt[ipmt];
	      d_sposx[jpmt] = sposx[ipmt];
	      d_sposy[jpmt] = sposy[ipmt];
	      d_sposz[jpmt] = sposz[ipmt];
	      d_sR[jpmt] = sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
				pmt_pos_trans.getY()*pmt_pos_trans.getY());
	      d_sX[jpmt] = pmt_pos_trans.getX();
	      d_sY[jpmt] = pmt_pos_trans.getY();
	      
	      d_snpe[jpmt] = snpe[ipmt];
	      jpmt++ ;
	    }
	  }
      }
      ////////////////////////////////////////////////////////////////////
      //////////////////// end of fill //////////////////////////////////
      ///////////////////////////////////////////////////////////////////
      if(iloop==20){
	if(npmt1>=1){
	  d_n0_new = npmt0;
	}else if(npmt1==0){
	  d_n0_new = -9999;
	}
	  
	//	t_ring->Fill();
      }
    }

  ///// subtraction 
  if(iloop==20){
    hsub_ringtot->Add(h_ringtot);
    hsub_ringtot->Add(h_sringtot,-1);
    hsub_ringtot->SetMinimum(0);
    
    hsub_R->Add(h_R);
    hsub_R->Add(h_sR,-1);
    hsub_R->SetMinimum(0);
  }
  if(iloop == -1){
    hsub_ringinit->Add(h_ringinit);
    hsub_ringinit->Add(h_sringinit,-1);
    hsub_ringinit->SetMinimum(0);
    
    hsub_R_init->Add(h_R_init);
    hsub_R_init->Add(h_sR_init,-1);
    hsub_R_init->SetMinimum(0);
  }
   
  for(int iarm=0;iarm<2; iarm++){
    for(int iside=0; iside<2; iside++){
      for(int ipanel=0; ipanel<24; ipanel++){
	/// need to scale 
	h_sring[iarm][iside][ipanel]->Sumw2();
	float scale = h_ring[iarm][iside][ipanel]->GetEntries()/
	  h_ring[iarm][(iside+1)%2][ipanel]->GetEntries();
	h_sring[iarm][iside][ipanel]->Scale(scale);

	hsub_ring[iarm][iside][ipanel]->Add(h_ring[iarm][iside][ipanel]);
	hsub_ring[iarm][iside][ipanel]->Add(h_sring[iarm][iside][ipanel],-1);
	hsub_ring[iarm][iside][ipanel]->SetMinimum(0);

	h_sr[iarm][iside][ipanel]->Sumw2();
	h_sr[iarm][iside][ipanel]->Scale(scale);

	hsub_r[iarm][iside][ipanel]->Add(h_r[iarm][iside][ipanel]);
	hsub_r[iarm][iside][ipanel]->Add(h_sr[iarm][iside][ipanel],-1);
	hsub_r[iarm][iside][ipanel]->SetMinimum(0);
      }
    }
  }


  if(i_Verb>1) cout << iloop << "th loop Fill end" << endl;

  Double_t z, phi;
  TGraph *tg, *guide_in=NULL, *guide_out=NULL;
  float gxin[31], gyin[31], gxout[31], gyout[31];

  if(iloop==20)
    {
      for(int i=0; i<31; i++)
	{
	  gxin[i] = 3.4 * cos ((float)i * M_PI / 15.);
	  gyin[i] = 3.4 * sin ((float)i * M_PI / 15.);
	  gxout[i] = 8.4 * cos ((float)i * M_PI / 15.);
	  gyout[i] = 8.4 * sin ((float)i * M_PI / 15.);
	}
      guide_in = new TGraph(31, gxin, gyin);
      guide_out = new TGraph(31, gxout, gyout);

      tf_eval->cd();
      h_ringtot->Write();
      h_R->Write();
      h_sringtot->Write();
      h_sR->Write();
      hsub_ringtot->Write();
      hsub_R->Write();

      for(int iarm=0; iarm<2; iarm++)
	{
	  for(int iside=0; iside<2; iside++)
	    {
	      for(int ip=0; ip<24; ip++)
		{
		  h_ring[iarm][iside][ip]->Write();
		  h_r[iarm][iside][ip]->Write();
		  h_sring[iarm][iside][ip]->Write();
		  h_sr[iarm][iside][ip]->Write();
		  hsub_ring[iarm][iside][ip]->Write();
		  hsub_r[iarm][iside][ip]->Write();
		  
		  h_n0_0[iarm][iside][ip]->Write();
		  h_n0_1[iarm][iside][ip]->Write();
		  h_n0_2[iarm][iside][ip]->Write();
		  h_n0_3[iarm][iside][ip]->Write();
		  h_n0_4[iarm][iside][ip]->Write();
		  h_n1_0[iarm][iside][ip]->Write();
		  h_n1_1[iarm][iside][ip]->Write();
		  h_n1_2[iarm][iside][ip]->Write();
		  h_n1_3[iarm][iside][ip]->Write();
		  h_n1_4[iarm][iside][ip]->Write();
		  h_npe0_0[iarm][iside][ip]->Write();
		  h_npe0_1[iarm][iside][ip]->Write();
		  h_npe0_2[iarm][iside][ip]->Write();
		  h_npe0_3[iarm][iside][ip]->Write();
		  h_npe0_4[iarm][iside][ip]->Write();
		  h_npe1_0[iarm][iside][ip]->Write();
		  h_npe1_1[iarm][iside][ip]->Write();
		  h_npe1_2[iarm][iside][ip]->Write();
		  h_npe1_3[iarm][iside][ip]->Write();
		  h_npe1_4[iarm][iside][ip]->Write();
		  h_chi2_0[iarm][iside][ip]->Write();
		  h_chi2_1[iarm][iside][ip]->Write();
		  h_chi2_2[iarm][iside][ip]->Write();
		  h_chi2_3[iarm][iside][ip]->Write();
		  h_chi2_4[iarm][iside][ip]->Write();
		  
		}
	    }
	}
    }
  if(iloop==-1 && i_Eval==1)
    {
      tf_eval->cd();
      h_ringinit->Write();
      h_R_init->Write();
      h_sringinit->Write();
      h_sR_init->Write();
    }
  
  for(int ic=0; ic<4; ic++)
    {
      if(iloop==20) tc_printed[ic]->Divide(6, 4);
      for(int ip=0; ip<24; ip++)
	{
	  if(iloop==20)
	    {
	      tc_printed[ic]->cd(ip+1);
	      h_ring[ic/2][ic%2][ip]->Draw("col");
	      guide_in->Draw("CP");
	      guide_out->Draw("CP");
	      if(i_Eval==1)
		{
		  CalcError(h_ring[ic/2][ic%2][ip], ic/2, ic%2, ip);
		  tf_eval->cd();
		  //	  h_ring[ic / 2][ic % 2][ip]->Write();
		}
	    }
	  else
	    {
	      tg = FindCent(h_ring[ic/2][ic%2][ip]);
	      tg->GetPoint(8, z, phi);
	      if(iloop == -1)
		{
		  f_dz[ic/2][ic%2][ip][iloop+1] = z;
		  if(i_VerifByPhi) 
		    f_dphi[ic / 2][ic % 2][ip][iloop+1] = phi / 263.5; 
		  else f_dphi[ic / 2][ic % 2][ip][iloop+1] = phi; 
		}
	      else if(iloop+1 > -1 && iloop + 1 < 20)
		{
		  f_dz[ic/2][ic%2][ip][iloop+1] = 
		    f_dz[ic / 2][ic % 2][ip][iloop] + z;
		  if(i_VerifByPhi) 
		    f_dphi[ic / 2][ic % 2][ip][iloop+1] = 
		      f_dphi[ic / 2][ic % 2][ip][iloop] + (phi / 263.5); 
		  else f_dphi[ic / 2][ic % 2][ip][iloop+1] = 
			 f_dphi[ic / 2][ic % 2][ip][iloop] + phi;
		}
	      
	      if(i_Eval==1)
		{
		  tf_eval->cd();
		  sprintf(drawn,"g_%s",h_ring[ic / 2][ic % 2][ip]->GetName());
		  //	  tg->Write(drawn);
		  //	  h_ring[ic / 2][ic % 2][ip]->Write();
		} 
	    }	  
	} // panel loop
      
      if(iloop==20 && i_Eval==1) tc_printed[ic]->Print();    
      
    } // Arm + Sector loop
  
  TTree *result=NULL;
  float dz, dphi, errdz, errdphi;
  int ia, is, ip;
  if(iloop == 20 && i_Eval==1)
    {
      result = new TTree("result", "Alignment result");
      result->Branch("arm", &ia, "arm/I");
      result->Branch("side", &is, "side/I");
      result->Branch("panel", &ip, "panel/I");
      result->Branch("dz", &dz, "dz/F");
      result->Branch("dphi", &dphi, "dphi/F");
      result->Branch("errdz", &errdz, "errdz/F");
      result->Branch("errdphi", &errdphi, "errdphi/F");
      


      for(ia=0; ia<2; ia++)
	{
	  for(is=0; is<2; is++)
	    {
	      for(ip=0; ip<24; ip++)
		{
		  dz = f_dzfinal[ia][is][ip]; dphi = f_dphifinal[ia][is][ip];
		  errdz = f_errdz[ia][is][ip]; errdphi = f_errdphi[ia][is][ip];
		  if(i_Eval==1) result->Fill();
		}
	    }
	}
      tf_eval->cd();
      result->Write();
    }
  
  for(int ic=0; ic<4; ic++)
    {
      for(int ip=0; ip<24; ip++)
	{
	  delete h_ring[ic / 2][ic % 2][ip];
	}
    }

  if(iloop==20){
    tf_eval->cd();
    //    t_ring->Write();
  }
}

TGraph* RICH_Alignment::FindCent(TH2* hist)
{
  float NBINS = hist->GetNbinsX(); 
  float XMAX = hist->GetXaxis()->GetXmax();
  float XMIN = hist->GetXaxis()->GetXmin();
  float SCAN_W = 2.;
  float binw = (XMAX - XMIN) / (float)NBINS;
  float z[15], phi[15];
  int xmin,xmax;
  Text_t gname[150];

  xmin = (int) ((-SCAN_W -XMIN )/ binw);
  xmax = (int) ((SCAN_W -XMIN )/ binw);

  sprintf(gname, "x%d_%s", 0, hist->GetName());
  TH1D *x = hist->ProjectionX(gname, xmin, xmax);
  x->SetAxisRange(-10., 0);
  z[0] = x->GetMean();
  x->SetAxisRange(0., 10.);
  z[0] += x->GetMean();
  z[0] /=2.;

  //  x->SetAxisRange(-10., 10.);
  //  z[0] = x->GetMean();

  xmin = (int)((-XMIN -z[0] -SCAN_W)/ binw);
  xmax = (int)((-XMIN -z[0] +SCAN_W)/ binw);

  sprintf(gname, "phi%d_%s", 0, hist->GetName());
  TH1D *y = hist->ProjectionY(gname, xmin, xmax);
  y->SetAxisRange(-10., 0);
  phi[0] = y->GetMean();
  y->SetAxisRange(0., 10.);
  phi[0] += y->GetMean();
  phi[0] /=2.;
  delete x, y;

  for(int ig=1; ig<10; ig++)
    {
      xmin = (int) ((-SCAN_W -XMIN -phi[ig-1])/ binw);
      xmax = (int) ((SCAN_W -XMIN -phi[ig-1])/ binw);
      
      sprintf(gname,"x%d_%s", ig, hist->GetName());
      x = hist->ProjectionX(gname, xmin, xmax);

      x->SetAxisRange(z[ig-1]-10., z[ig-1]);
      z[ig] = x->GetMean();
      x->SetAxisRange(z[ig-1], z[ig-1]+10.);
      z[ig] += x->GetMean();
      z[ig] /=2.;

      /*
      x->SetAxisRange(z[ig-1]-10., z[ig-1]+10);
      z[ig] = x->GetMean();
      */

      xmin = (int)((-XMIN -z[ig-1] -SCAN_W)/ binw);
      xmax = (int)((-XMIN -z[ig-1] +SCAN_W)/ binw);
      
      sprintf(gname, "phi%d_%s", ig,hist->GetName());
      y = hist->ProjectionY(gname, xmin, xmax);
      y->SetAxisRange(phi[ig-1]-10., phi[ig-1]);
      phi[ig] = y->GetMean();
      y->SetAxisRange(phi[ig-1], phi[ig-1]+10.);
      phi[ig] += y->GetMean();
      phi[ig] /=2.;
      delete x,y;
    }

  TGraph *out = new TGraph(10, z, phi);
  return out;
}

void RICH_Alignment::CalcError(TH2* hist, int arm, int side, int panel)
{
  float NBINS = hist->GetNbinsX(); 
  float XMAX = hist->GetXaxis()->GetXmax();
  float XMIN = hist->GetXaxis()->GetXmin();
  float SCAN_W = 2.;
  float binw = (XMAX - XMIN) / (float)NBINS;
  int xmin, xmax;
  float err;
  float rms,N;
  Text_t gname[150];

  xmin = (int) ((-SCAN_W -XMIN )/ binw);
  xmax = (int) ((SCAN_W -XMIN )/ binw);
  
  sprintf(gname, "zerr_%s", hist->GetName());
  TH1D *x = hist->ProjectionX(gname, xmin, xmax);

  x->SetAxisRange(-10., 0);
  rms = x->GetRMS();
  N = x->Integral();
  if(N>0) err = rms*rms/N;
  else err = 0;

  x->SetAxisRange(0., 10.);
  rms = x->GetRMS();
  N = x->Integral();
  if(N>0) err += rms*rms/N;
  else err += 0;

  f_errdz[arm][side][panel] = sqrt(err)/2.;

  sprintf(gname, "phierr_%s", hist->GetName());
  TH1D *y = hist->ProjectionY(gname, xmin, xmax);

  y->SetAxisRange(-10., 0);
  rms = y->GetRMS();
  N = y->Integral();
  if(N>0) err = rms*rms/N;
  else err = 0;

  y->SetAxisRange(0., 10.);
  rms = y->GetRMS();
  N = y->Integral();
  if(N>0) err += rms*rms/N;
  else err += 0;

  f_errdphi[arm][side][panel] = (sqrt(err)/2.) / 263.5;
}

void RICH_Alignment::Calc(int fixed, int mean)
{
  int ia, is, ip;
  
  memset(f_dzfinal, 0, sizeof(f_dzfinal)); 
  memset(f_dphifinal, 0, sizeof(f_dphifinal));

  for(ia=0; ia<2; ia++)
    {
      for(is=0; is<2; is++)
	{
	  for(ip=0; ip<24; ip++)
	    {
	      for(int i=0; i<mean; i++)
		{
		  f_dzfinal[ia][is][ip] += f_dz[ia][is][ip][fixed + i + 1];
		  f_dphifinal[ia][is][ip] += f_dphi[ia][is][ip][fixed + i + 1];
		}
	      f_dzfinal[ia][is][ip] /= (float) mean;
	      f_dphifinal[ia][is][ip] /= (float) mean;
	      
	      if(!i_VerifByPhi)
		{
		  f_dphifinal[ia][is][ip] /= 263.5;
		}

	      if(i_Verb>1)
		{
		  cout << ia << " " << is << " " << ip << " " 
		       << f_dzfinal[ia][is][ip] << " " 
		       << f_dphifinal[ia][is][ip] << endl;
		}	  
	    }
	}
    }
}

void RICH_Alignment::Write()
{
  cout << "Writing alignment result on " << c_alignmentf << endl;
  ofstream fout(c_alignmentf);
  for(int ia=0; ia<2; ia++)
    {
      for(int is=0; is<2; is++)
	{
	  for(int ip=0; ip<24; ip++)
	    {
	      fout.width(3);
	      fout << ia;
	      fout.width(3);
	      fout << is;
	      fout.width(3);
	      fout << ip;
	      fout.width(20);
	      fout.precision(6);
	      fout << f_dzfinal[ia][is][ip];
	      fout.width(20);
	      fout.precision(6);
	      fout << f_dphifinal[ia][is][ip] << endl;
	    }
	}
    }
  fout.close();
}

void RICH_Alignment::Write(char *align)
{
  cout << "Writing alignment result on " << align << endl;
  ofstream fout(align);
  for(int ia=0; ia<2; ia++)
    {
      for(int is=0; is<2; is++)
	{
	  for(int ip=0; ip<24; ip++)
	    {
	      fout.width(3);
	      fout << ia;
	      fout.width(3);
	      fout << is;
	      fout.width(3);
	      fout << ip;
	      fout.width(20);
	      fout.precision(6);
	      fout << f_dzfinal[ia][is][ip];
	      fout.width(20);
	      fout.precision(6);
	      fout << f_dphifinal[ia][is][ip] << endl;
	    }
	}
    }
  fout.close();
}

void RICH_Alignment::Verify()
{
  ProcessRings(20);
}

void RICH_Alignment::Process(int fixed, int mean)
{
  if(i_Verb>0)
    cout << "Process DST Entries" << endl;
  
  for(int i=-1; i<fixed+mean; i++)
    {
      if(i_Verb>0 )
	cout << "Find ring center. " << i << "th try" << endl;
      ProcessRings(i);
    }
  cout << "Calculate final ring center positions." << endl;
  Calc(fixed, mean);
  Write();

  if(i_Eval>0) Verify();
}

void RICH_Alignment::ImportOutput(char *evalf,char *alignf)
{
  int arm, side, panel;
  float dz, dphi;
  int cnt = 0;

  TFile *evalimp = new TFile(evalf);
  trk = (TTree*)evalimp->Get("trk");
  ifstream fdzdphi(alignf);
  while(!fdzdphi.eof() && cnt<96)
    {
      fdzdphi >> arm >> side >> panel >> dz >> dphi;
      f_dzfinal[arm][side][panel] = dz;
      f_dphifinal[arm][side][panel] = dphi;
      cnt++;
    }
  fdzdphi.close();
}

void RICH_Alignment::Init_alignment_parameters(char *infile)
{
  cout << "initialize the alignment parameters" << endl;

  FILE *fa ;
  if((fa=fopen(infile,"r"))==NULL)
    {
      cout << "alignment parameter file doesn't exist" << endl;
      exit(0);
    }
  
  int Arm, Side, Panel;
  float Dz, Dphi;
  
  for(int il=0; il<96; il++)
    {
      fscanf(fa, "%d %d %d %f %f", &Arm, &Side, &Panel, &Dz, &Dphi);
      f_dzfinal[Arm][Side][Panel] = Dz;
      f_dphifinal[Arm][Side][Panel] = Dphi;
      
      //    f_dzstart[Arm][Side][Panel]=Dz;
      //    f_dphistart[Arm][Side][Panel]=Dphi;
      
      //    f_dzstart[Arm][Side][Panel]=0;
      //    f_dphistart[Arm][Side][Panel]=0;
      
      if(il==0)
	{
	  cout << "Parameter valus" << endl;
	  cout << " Arm  :  Side  : Panel : f_dz :  f_dph: " << endl;
	}
      cout << Arm << "  " << Side << "  " << Panel << "  " 
	   << f_dzfinal[Arm][Side][Panel] << "  " 
	   << f_dphifinal[Arm][Side][Panel] << endl;
    }
}

void RICH_Alignment::Init_alignment_parameters_0th(char *infile)
{
  cout << "initialize the alignment parameters" << endl;

  FILE *fa ;
  if((fa=fopen(infile,"r"))==NULL)
    {
      cout << "alignment parameter file doesn't exist" << endl;
      exit(0);
    }
  
  int Arm, Side, Panel;
  float Dz, Dphi;
  
  for(int il=0; il<96; il++)
    {
      fscanf(fa, "%d %d %d %f %f", &Arm, &Side, &Panel, &Dz, &Dphi);
      //f_dzfinal[Arm][Side][Panel] = Dz;
      //f_dphifinal[Arm][Side][Panel] = Dphi;
      
      f_dz[Arm][Side][Panel][0]=Dz;
      f_dphi[Arm][Side][Panel][0]=Dphi;
      
      //    f_dzstart[Arm][Side][Panel]=0;
      //    f_dphistart[Arm][Side][Panel]=0;
      
      if(il==0)
	{
	  cout << "Parameter valus " << endl;
	  cout << " *** set the initial value from file : "<<infile<<endl;
	  cout << " Arm  :  Side  : Panel : f_dz :  f_dph: " << endl;
	}
      cout << Arm << "  " << Side << "  " << Panel << "  " 
	   << f_dz[Arm][Side][Panel][0] << "  " 
	   << f_dphi[Arm][Side][Panel][0] << endl;
    }
}
