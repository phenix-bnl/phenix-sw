#include "RICHAlignment_track.h"

RICHAlignment_track::RICHAlignment_track(const char* outfile) 
  : SubsysReco("FUN4EXAMPLE Analyzer")
{
  cout << " Track is extracted by projection point on PC1 and PC3. " << endl;
 
  strcpy(OutFileName, outfile);
  cout <<" Tree will be saved to : " << OutFileName << endl;
}

int RICHAlignment_track::Init(PHCompositeNode *topNode)
{
  cout << " Init " << endl;

  fout = new TFile(OutFileName,"recreate");
  
  tree = new TTree("trk", "track tree");
  tree->Branch("ntrk", &ntrk, "ntrk/I");
  tree->Branch("arm", &arm, "arm/I");
  tree->Branch("side", &side, "side/I");
  tree->Branch("cross_z", &cross_z, "cross_z/F");
  tree->Branch("cross_phi", &cross_phi, "cross_phi/F");
  tree->Branch("center_z", &center_z, "center_z/F");
  tree->Branch("center_phi", &center_phi, "center_phi/F");
  tree->Branch("n0", &n0, "n0/I");
  tree->Branch("npe0", &npe0, "npe0/F");
  tree->Branch("panel", &panel, "panel/I");
  tree->Branch("npmt", &npmt, "npmt/I");
  tree->Branch("pmt", pmt, "pmt[npmt]/S");
  tree->Branch("npe", npe, "npe[npmt]/F");
  tree->Branch("posx", posx, "posx[npmt]/F");
  tree->Branch("posy", posy, "posy[npmt]/F");
  tree->Branch("posz", posz, "posz[npmt]/F");
  tree->Branch("posr", posr, "posr[npmt]/F");
  tree->Branch("posphi", posphi, "posphi[npmt]/F");
  tree->Branch("ppc1pos", ppc1pos, "ppc1pos[3]/F");
  tree->Branch("ppc3pos", ppc3pos, "ppc3pos[3]/F");
  tree->Branch("mom", &mom, "mom/F");
  tree->Branch("ecore", &ecore, "ecore/F");
  tree->Branch("start", start, "start[3]/F");
  tree->Branch("end", end, "end[3]/F");
  tree->Branch("b_ref", b_ref, "b_ref[3]/F");
  tree->Branch("v_ref", v_ref, "v_ref[3]/F");

  SetupCGO();

  return 0;
}

int RICHAlignment_track::process_event(PHCompositeNode *topNode)
{
  // informational message...
  static int ncalls = 0;
  if (ncalls%100 == 0) cout << "RICHAlignment_track::process_event Ncalls = "
			    << ncalls << endl;

  GetNodes(topNode);
  
  int pc1id = -1;
  int pc3id = -1;
  int emcid = -1;
  
  int tottrk = d_cnt->get_npart();
  ntrk = 0;

  for(int itrk=0; itrk<tottrk; itrk++)
    {
      pc1id = d_cnt->get_pc1id(itrk);
      pc3id = d_cnt->get_pc3id(itrk);
      emcid = d_cnt->get_emcid(itrk);
      
      if(emcid>-1 && pc1id>-1 && pc3id>-1)
	{
	  if(d_cnt->get_quality(itrk)%32!=31) continue; 
	  
	  n0 = d_cnt->get_n0(itrk);
	  npe0 = d_cnt->get_npe0(itrk);
	  cross_z = d_cnt->get_cross_z(itrk);
	  cross_phi = d_cnt->get_cross_phi(itrk);
	  center_z = d_cnt->get_center_z(itrk);
	  center_phi = d_cnt->get_center_phi(itrk);
	  arm =  1-d_cnt->get_dcarm(itrk);
	  side = d_cnt->get_dcside(itrk);
	  mom = d_cnt->get_mom(itrk);
	  ecore = d_cnt->get_ecore(itrk);
	  
	  if(n0>=0
	     && mom>0.5 && mom<5.0
	     && fabs(d_cnt->get_pc3sdphi(itrk))<2. 
	     && fabs(d_cnt->get_pc3sdz(itrk))<2.
	     && ecore>0. && ecore/mom>0.7 && ecore/mom<1.5
	     )
	    {
	      ppc1pos[0] = d_cnt->get_ppc1x(itrk);
	      ppc1pos[1] = d_cnt->get_ppc1y(itrk);
	      ppc1pos[2] = d_cnt->get_ppc1z(itrk);
	      ppc3pos[0] = d_cnt->get_ppc3x(itrk);
	      ppc3pos[1] = d_cnt->get_ppc3y(itrk);
	      ppc3pos[2] = d_cnt->get_ppc3z(itrk);
	  
	      pstart.setX(d_cnt->get_ppc1x(itrk));
	      pstart.setY(d_cnt->get_ppc1y(itrk));
	      pstart.setZ(d_cnt->get_ppc1z(itrk));
	      
	      pend.setX(d_cnt->get_ppc3x(itrk));
	      pend.setY(d_cnt->get_ppc3y(itrk));
	      pend.setZ(d_cnt->get_ppc3z(itrk));

	      start[0] = pstart.getX();
	      start[1] = pstart.getY();
	      start[2] = pstart.getZ();
	      end[0] = pend.getX();
	      end[1] = pend.getY();
	      end[2] = pend.getZ();
	
	      PHLine trkline(pstart, pend);

	      arm = pstart.getX() > 0 ? 0 : 1;
	
	      ref = cgo->Reflect(arm, trkline, side, panel, path);
	
	      v_ref[0] = ref.getDirection().getX();
	      v_ref[1] = ref.getDirection().getY();
	      v_ref[2] = ref.getDirection().getZ();
	      
	      b_ref[0] = ref.getBasepoint().getX();
	      b_ref[1] = ref.getBasepoint().getY();
	      b_ref[2] = ref.getBasepoint().getZ();

	      if(ref.length()>0.)
		{
		  cross_to_crk = cgo->HitArray(arm, ref, side);
		  
		  if(cross_to_crk == PHPoint());
		  else
		    {
		      npmt = 0;
		      int totnhit = (int)d_crk->get_CrkNHit();
		      
		      for(int ihit=0; ihit<totnhit; ihit++)
			{
			  pmt[npmt] = d_crk->get_pmt(ihit);
			  npe[npmt] = d_crk->get_npe(ihit);
			  
			  if(npe[npmt]<0.3 || npe[npmt]>8.0) continue;
			  
			  if(cgo->IdToArm(pmt[npmt]) == arm &&
			     cgo->IdToSide(pmt[npmt]) == side)
			    {
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
			      if(posphi[npmt]<-0.5*3.14159265) 
				posphi[npmt] += 2.0*3.14159265;
			      
			      npmt++;
			    }
			}
		      tree->Fill();
		    }
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
  fout->Close();

  cout<<"End! See you later!"<<endl;
  return 0;
}

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
