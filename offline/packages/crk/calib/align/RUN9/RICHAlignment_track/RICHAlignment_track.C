#include "RICHAlignment_track.h"

using namespace std;

RICHAlignment_track::RICHAlignment_track(const char* outfile) 
{
  cout << " Track is extracted by projection point on PC1 and PC3. " << endl;
 
  strcpy(OutFileName, outfile);
  cout <<" Tree will be saved to : " << OutFileName << endl;

  memset(f_dz, 0, sizeof(f_dz));
  memset(f_dphi, 0, sizeof(f_dphi));

}

int RICHAlignment_track::Init(PHCompositeNode *topNode)
{

  cout << " Init " << endl;

  fout = new TFile(OutFileName,"recreate");
  
  tree = new TTree("trk", "track tree");
  tree->Branch("run", &run, "run/I");
  tree->Branch("bbcz", &bbcz, "bbcz/F");
  tree->Branch("bbcq", &bbcq, "bbcq/F");
  tree->Branch("zdce", &zdce, "zdce/F");
  tree->Branch("ntrk", &ntrk, "ntrk/I");
  tree->Branch("arm", &arm, "arm/I");
  tree->Branch("side", &side, "side/I");
  tree->Branch("sside", &side_swap, "sside/I");
  tree->Branch("cross_z", &cross_z, "cross_z/F");
  tree->Branch("cross_phi", &cross_phi, "cross_phi/F");
  tree->Branch("center_z", &center_z, "center_z/F");
  tree->Branch("center_phi", &center_phi, "center_phi/F");
  tree->Branch("n0", &n0, "n0/I");
  tree->Branch("sn0", &sn0, "sn0/I");
  tree->Branch("npe0", &npe0, "npe0/F");
  tree->Branch("panel", &panel, "panel/I");
  tree->Branch("spanel", &panel_swap, "spanel/I");
  tree->Branch("npmt", &npmt, "npmt/I");
  tree->Branch("pmt", pmt, "pmt[npmt]/S");
  tree->Branch("npe", npe, "npe[npmt]/F");
  tree->Branch("tcrk", tcrk, "tcrk[npmt]/F");
  tree->Branch("posx", posx, "posx[npmt]/F");
  tree->Branch("posy", posy, "posy[npmt]/F");
  tree->Branch("posz", posz, "posz[npmt]/F");
  tree->Branch("posr", posr, "posr[npmt]/F");
  tree->Branch("posphi", posphi, "posphi[npmt]/F");
  tree->Branch("snpmt", &snpmt, "snpmt/I");
  tree->Branch("spmt", spmt, "spmt[snpmt]/S");
  tree->Branch("snpe", snpe, "snpe[snpmt]/F");
  tree->Branch("stcrk", stcrk, "stcrk[snpmt]/F");
  tree->Branch("sposx", sposx, "sposx[snpmt]/F");
  tree->Branch("sposy", sposy, "sposy[snpmt]/F");
  tree->Branch("sposz", sposz, "sposz[snpmt]/F");
  tree->Branch("sposr", sposr, "sposr[snpmt]/F");
  tree->Branch("sposphi", sposphi, "sposphi[snpmt]/F");
  tree->Branch("ppc1pos", ppc1pos, "ppc1pos[3]/F");
  tree->Branch("ppc2pos", ppc2pos, "ppc2pos[3]/F");
  tree->Branch("ppc3pos", ppc3pos, "ppc3pos[3]/F");
  tree->Branch("pemcpos", pemcpos, "pemcpos[3]/F");
  tree->Branch("pc2dphi", &pc2dphi, "pc2dphi/F");
  tree->Branch("pc2dz", &pc2dz, "pc2dz/F");
  tree->Branch("pc3dphi", &pc3dphi, "pc3dphi/F");
  tree->Branch("pc3dz", &pc3dz, "pc3dz/F");
  tree->Branch("emcdphi", &emcdphi, "emcdphi/F");
  tree->Branch("emcdz", &emcdz, "emcdz/F");
  tree->Branch("flag", &flag, "flag/I");
  tree->Branch("charge", &charge, "charge/I");
  tree->Branch("alpha", &alpha, "alpha/F");
  tree->Branch("beta", &beta, "beta/F");
  tree->Branch("zed", &zed, "zed/F");
  tree->Branch("phi", &phi, "phi/F");
  tree->Branch("mom", &mom, "mom/F");
  tree->Branch("ecore", &ecore, "ecore/F");
  tree->Branch("start", start, "start[3]/F");
  tree->Branch("end", end, "end[3]/F");
  tree->Branch("b_ref", b_ref, "b_ref[3]/F");
  tree->Branch("v_ref", v_ref, "v_ref[3]/F");
  tree->Branch("sb_ref", sb_ref, "sb_ref[3]/F");
  tree->Branch("sv_ref", sv_ref, "sv_ref[3]/F");

  SetupCGO();

  cout << "initialize the alignment parameters" << endl;

  FILE *fa ;
  char infile[100];
  sprintf(infile,"alignment_Run8.dat");
  if((fa=fopen(infile,"r"))==NULL){
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
      
      f_dz[Arm][Side][Panel]=Dz;
      f_dphi[Arm][Side][Panel]=Dphi;
      
      //    f_dzstart[Arm][Side][Panel]=0;
      //    f_dphistart[Arm][Side][Panel]=0;
      
      if(il==0)
	{
	  cout << "Parameter valus " << endl;
	  cout << " *** set the initial value from file : "<<infile<<endl;
	  cout << " Arm  :  Side  : Panel : f_dz :  f_dph: " << endl;
	}
      cout << Arm << "  " << Side << "  " << Panel << "  "
	   << f_dz[Arm][Side][Panel] << "  "
	   << f_dphi[Arm][Side][Panel] << endl;
    }

  cout<<" start "<<endl;

  return 0;
}

int RICHAlignment_track::process_event(PHCompositeNode *topNode)
{
  // informational message...
  static int ncalls = 0 ;

  if ( ncalls % 2000 == 0) {
    cout << "RICHAlignment_track::process_event Ncalls = "
	 << ncalls << endl;
  }

  //  GetNodes(topNode);
  //PHCentralTrack *d_cnt = getClass<PHCentralTrack>(topNode, "EWGCentralTrack"); // EWG  
  PHCentralTrack *d_cnt = getClass<PHCentralTrack>(topNode, "PHCentralTrack"); // CNT  
    
  PHGlobal *d_glb = getClass<PHGlobal>(topNode, "PHGlobal"); // GLB  

  CrkHit *d_crk = getClass<CrkHit>(topNode, "CrkHit"); // CRK

  SyncObject *sync = getClass<SyncObject>(topNode, "Sync");

  run = sync->RunNumber();

  bbcz = d_glb->getBbcZVertex();
  bbcq = d_glb->getBbcChargeS()+d_glb->getBbcChargeN();
  zdce = d_glb->getZdcEnergyN()+d_glb->getZdcEnergyS();
  if(fabs(bbcz)>30){
    ncalls ++;
    return 0;
  }

  int pc1id = -1;
  int pc2id = -1;
  int pc3id = -1;
  int emcid = -1;
  
  int tottrk = d_cnt->get_npart();
  ntrk = tottrk;



  for(int itrk=0; itrk<tottrk; itrk++){
    pc1id = d_cnt->get_pc1id(itrk);
    pc2id = d_cnt->get_pc2id(itrk);
    pc3id = d_cnt->get_pc3id(itrk);
    emcid = d_cnt->get_emcid(itrk);
    
    int ass=0;      
    
    if(d_crk->get_CrkNHit()<0){
      continue;
    }
    
    if(pc1id>-1){

      //      if(d_cnt->get_quality(itrk)%32!=31) continue; 
      if(d_cnt->get_quality(itrk)<10) continue; 

      n0 = d_cnt->get_n0(itrk);
      sn0 = d_cnt->get_sn0(itrk);
      npe0 = d_cnt->get_npe0(itrk);
      cross_z = d_cnt->get_cross_z(itrk);
      cross_phi = d_cnt->get_cross_phi(itrk);
      center_z = d_cnt->get_center_z(itrk);
      center_phi = d_cnt->get_center_phi(itrk);
      arm =  1-d_cnt->get_dcarm(itrk);
      side = d_cnt->get_dcside(itrk);
      mom = d_cnt->get_mom(itrk);
      ecore = d_cnt->get_ecore(itrk);
      /*	  
		 if(n0>=0
		 && mom>0.5 && mom<5.0
		 && fabs(d_cnt->get_pc3sdphi(itrk))<2. 
		 && fabs(d_cnt->get_pc3sdz(itrk))<2.
		 && ecore>0. && ecore/mom>0.7 && ecore/mom<1.5
		 )
		 {
      */
      alpha = d_cnt->get_alpha(itrk);
      beta = d_cnt->get_beta(itrk);
      zed = d_cnt->get_zed(itrk);
      phi = d_cnt->get_phi(itrk);

      charge =  d_cnt->get_charge(itrk);
      pc3dphi = d_cnt->get_pc3dphi(itrk);
      pc3dz = d_cnt->get_pc3dz(itrk);
      pc2dphi = d_cnt->get_pc2dphi(itrk);
      pc2dz = d_cnt->get_pc2dz(itrk);
      emcdphi = d_cnt->get_emcdphi(itrk);
      emcdz = d_cnt->get_emcdz(itrk);
      
      
      if(mom>0.5 && mom<5.0 && 
      	 ecore>0.75 && ecore/mom>0.7 && ecore/mom<1.3){
	
	ass = 0;

	ppc1pos[0] = d_cnt->get_ppc1x(itrk);
	ppc1pos[1] = d_cnt->get_ppc1y(itrk);
	ppc1pos[2] = d_cnt->get_ppc1z(itrk);
	
	pstart.setX(d_cnt->get_ppc1x(itrk));
	pstart.setY(d_cnt->get_ppc1y(itrk));
	pstart.setZ(d_cnt->get_ppc1z(itrk));
	
	pemcpos[0] = d_cnt->get_pemcx(itrk);
	pemcpos[1] = d_cnt->get_pemcy(itrk);
	pemcpos[2] = d_cnt->get_pemcz(itrk);
	
	ppc3pos[0] = d_cnt->get_ppc3x(itrk);
	ppc3pos[1] = d_cnt->get_ppc3y(itrk);
	ppc3pos[2] = d_cnt->get_ppc3z(itrk);
	
	ppc2pos[0] = d_cnt->get_ppc2x(itrk);
	ppc2pos[1] = d_cnt->get_ppc2y(itrk);
	ppc2pos[2] = d_cnt->get_ppc2z(itrk);
	
	if(emcid>-1){
	  pend.setX(d_cnt->get_pemcx(itrk));
	  pend.setY(d_cnt->get_pemcy(itrk));
	  pend.setZ(d_cnt->get_pemcz(itrk));
	  ass += 100;
	}
	      
	if(pc3id>-1
	   &&fabs(d_cnt->get_pc3dphi(itrk))<0.02
	   &&fabs(d_cnt->get_pc3dz(itrk))<5.0){
	  pend.setX(d_cnt->get_ppc3x(itrk));
	  pend.setY(d_cnt->get_ppc3y(itrk));
	  pend.setZ(d_cnt->get_ppc3z(itrk));
	  ass += 10;
	}
	
	if(pc2id>-1
	   &&fabs(d_cnt->get_pc2dphi(itrk))<0.015
	   &&fabs(d_cnt->get_pc2dz(itrk))<5.0){
	  pend.setX(d_cnt->get_ppc2x(itrk));
	  pend.setY(d_cnt->get_ppc2y(itrk));
	  pend.setZ(d_cnt->get_ppc2z(itrk));
	  ass += 1;
	}
       
	flag = ass;
	
	if(ass<=0){
	  continue;
	}
	
	start[0] = pstart.getX();
	start[1] = pstart.getY();
	start[2] = pstart.getZ();
	end[0] = pend.getX();
	end[1] = pend.getY();
	end[2] = pend.getZ();
	
	PHLine trkline(pstart, pend);
	PHLine trkline_swap = ReflectInZ(trkline);
	
	arm = pstart.getX() > 0 ? 0 : 1;
	
	ref = cgo->Reflect(arm, trkline, side, panel, path);
	ref_swap = cgo->Reflect(arm, trkline_swap, side_swap, 
				panel_swap, path_swap);
	
	
	v_ref[0] = ref.getDirection().getX();
	v_ref[1] = ref.getDirection().getY();
	v_ref[2] = ref.getDirection().getZ();
	
	b_ref[0] = ref.getBasepoint().getX();
	b_ref[1] = ref.getBasepoint().getY();
	b_ref[2] = ref.getBasepoint().getZ();
	
	sv_ref[0] = ref_swap.getDirection().getX();
	sv_ref[1] = ref_swap.getDirection().getY();
	sv_ref[2] = ref_swap.getDirection().getZ();
	
	sb_ref[0] = ref_swap.getBasepoint().getX();
	sb_ref[1] = ref_swap.getBasepoint().getY();
	sb_ref[2] = ref_swap.getBasepoint().getZ();
	
	
	if(ref.length()>0.){
	  cross_to_crk = cgo->HitArray(arm, ref, side);
	  if(cross_to_crk == PHPoint()
	     );
	  else{
	    npmt = 0;
	    int totnhit = (int)d_crk->get_CrkNHit();
	    
	    for(int ihit=0; ihit<totnhit; ihit++){
	      pmt[npmt] = d_crk->get_pmt(ihit);
	      npe[npmt] = d_crk->get_npe(ihit);
	      tcrk[npmt] = d_crk->get_time(ihit);
	      
	      if(npe[npmt]<0.3 || npe[npmt]>8.0) continue;
	      
	      if( (cgo->IdToArm(pmt[npmt]) == arm &&
		   cgo->IdToSide(pmt[npmt]) == side)){
		pmt_pos = 
		  cgo->GetPmtPosition(cgo->IdToArm(pmt[npmt]),
				      cgo->IdToSide(pmt[npmt]),
				      cgo->IdToSm(pmt[npmt]),
				      cgo->IdToPmt(pmt[npmt]));
		
		posx[npmt] = pmt_pos.getX();
		posy[npmt] = pmt_pos.getY();
		posz[npmt] = pmt_pos.getZ();
		posr[npmt] = 
		  sqrt(posx[npmt]*posx[npmt]
		       +posy[npmt]*posy[npmt]);
		posphi[npmt] = atan2(posy[npmt], posx[npmt]);
		if(posphi[npmt]<-0.5*3.14159265) {
		  posphi[npmt] += 2.0*3.14159265;
		}
		npmt++;
	      }
	    }
	  }
	}
	    
	if(ref_swap.length()>0.){
	  cross_to_crk_swap = cgo->HitArray(arm, ref_swap, side_swap);
	  if(cross_to_crk_swap == PHPoint()
	     );
	  else{
	    snpmt = 0;
	    int totnhit = (int)d_crk->get_CrkNHit();
	    
	    for(int ihit=0; ihit<totnhit; ihit++)
	      {
		spmt[snpmt] = d_crk->get_pmt(ihit);
		snpe[snpmt] = d_crk->get_npe(ihit);
		stcrk[snpmt] = d_crk->get_time(ihit);
		
		if(snpe[snpmt]<0.3 || snpe[snpmt]>8.0) continue;
		
		if( (cgo->IdToArm(spmt[snpmt]) == arm &&
		     cgo->IdToSide(spmt[snpmt]) == side_swap)){
		  pmt_pos = 
		    cgo->GetPmtPosition(cgo->IdToArm(spmt[snpmt]),
					cgo->IdToSide(spmt[snpmt]),
					cgo->IdToSm(spmt[snpmt]),
					cgo->IdToPmt(spmt[snpmt]));
		  
		  sposx[snpmt] = pmt_pos.getX();
		  sposy[snpmt] = pmt_pos.getY();
		  sposz[snpmt] = pmt_pos.getZ();
		  sposr[snpmt] = 
		    sqrt(sposx[snpmt]*sposx[snpmt]
			 +sposy[snpmt]*sposy[snpmt]);
		  sposphi[snpmt] = atan2(sposy[snpmt], sposx[snpmt]);
		  if(sposphi[snpmt]<-0.5*3.14159265) {
		    sposphi[snpmt] += 2.0*3.14159265;
		  }
		  snpmt++;
		}
	      }
	  }
	}
	if(!(npmt==0 && snpmt==0)){
	  tree->Fill();
	}
      }
    }
  }
  ncalls++;
  return 0;
}

int RICHAlignment_track::End(PHCompositeNode *topNode)
{
  fout->cd();
  tree->Write();
  tree->Delete();
  fout->Close();

  cout<<"End! See you later!"<<endl;
  return 0;
}




/*
void RICHAlignment_track::GetNodes(PHCompositeNode *topNode) 
{
  //PHCentral
  d_cnt = getClass<PHCentralTrack>(topNode, "PHCentralTrack"); 
  
  //CrkHit
  d_crk = getClass<CrkHit>(topNode, "CrkHit");

  //  cout << "Node " << d_cnt << " " << d_crk << endl;

  if (!d_cnt) cout << "PHCentralTrack not found" << endl;
  if (!d_crk) cout << "CrkHit not found" << endl;
}
*/

PHLine
RICHAlignment_track::ReflectInZ(const PHLine &trk)
{
  PHPoint base_zref = trk.getBasepoint();
  PHVector dir_zref = trk.getDirection();
  base_zref.setZ( -1.0*base_zref.getZ());
  dir_zref.setZ( -1.0*dir_zref.getZ());
  return PHLine(base_zref, dir_zref);
}
