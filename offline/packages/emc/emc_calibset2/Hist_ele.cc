
#include "Hist_ele.hh"

ClassImp(Hist_ele)
//=====================================================================
Hist_ele::Hist_ele(){
};
//=====================================================================
Hist_ele::Hist_ele(char* pname,char* ptitle,char* opt) : Hist(pname,ptitle){
  bool status;
  if( strstr(opt,"hide") ){
    status = TH1::AddDirectoryStatus();
    TH1::AddDirectory(false);
  }
  //=============================================================================
  //=============================================================================
  isect = 8;
  while( isect-- ){
    if( IsValidSector(isect) ){
      sprintf(hname,"%s_h_ep_sect%d",pname,isect);
      sprintf(htitle,"%s ecore/momentum in Sect%d",ptitle,isect);
      Register( h_ep_sect[isect] = new TH1F(hname,htitle,150,0,1.5) );
      //
      sprintf(hname,"%s_h2_ep_sect%d",pname,isect);
      sprintf(htitle,"%s e/momentum vs momentum in Sect%d",ptitle,isect);
      Register( h2_ep_sect[isect] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      //
      sprintf(hname,"%s_h2_ecorrp_sect%d",pname,isect);
      sprintf(htitle,"%s ecorr/momentum vs momentum in Sect%d",ptitle,isect);
      Register( h2_ecorrp_sect[isect] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      //
      sprintf(hname,"%s_h2_ecorep_sect%d",pname,isect);
      sprintf(htitle,"%s ecore/momentum vs momentum in Sect%d",ptitle,isect);
      Register( h2_ecorep_sect[isect] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      //
      sprintf(hname,"%s_h2_ecorepX12_sect%d",pname,isect);
      sprintf(htitle,"%s ecore/momentum vs momentum in Sect%d",ptitle,isect);
      Register( h2_ecorepX12_sect[isect] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      //
      imul = 6;
      while( imul-- ){
	sprintf(hname,"%s_h2_ecorrp_sectmul%d_%d",pname,isect,imul);
	sprintf(htitle,"%s ecorr/momentum vs momentum in Sect%d, Multi%d",ptitle,isect,imul);
	Register( h2_ecorrp_sectmul[isect][imul] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
	//
	sprintf(hname,"%s_h2_ecorrpX12_sectmul%d_%d",pname,isect,imul);
	sprintf(htitle,"%s e/momentum vs momentum in Sect%d, Multi%d",ptitle,isect,imul);
	Register( h2_ecorrpX12_sectmul[isect][imul] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
	//
	sprintf(hname,"%s_h2_ecorep_sectmul%d_%d",pname,isect,imul);
	sprintf(htitle,"%s ecore/momentum vs momentum in Sect%d, Multi%d",ptitle,isect,imul);
	Register( h2_ecorep_sectmul[isect][imul] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
	//
	sprintf(hname,"%s_h2_ecorepX12_sectmul%d_%d",pname,isect,imul);
	sprintf(htitle,"%s ecore/momentum vs momentum in Sect%d, Multi%d",ptitle,isect,imul);
	Register( h2_ecorepX12_sectmul[isect][imul] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      }
      //
      sprintf(hname,"%s_h_epemce_sect%d",pname,isect);
      sprintf(htitle,"%s ecore/momentum emce in Sect%d",ptitle,isect);
      Register( h_epemce_sect[isect] = new TH1F(hname,htitle,150,0,1.5) );
      //
      sprintf(hname,"%s_h2_epemce_sect%d",pname,isect);
      sprintf(htitle,"%s e/momentum vs momentum emce in Sect%d",ptitle,isect);
      Register( h2_epemce_sect[isect] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      //
      sprintf(hname,"%s_h2_ecorrpemce_sect%d",pname,isect);
      sprintf(htitle,"%s ecorr/momentum vs momentum emce in Sect%d",ptitle,isect);
      Register( h2_ecorrpemce_sect[isect] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      //
      sprintf(hname,"%s_h2_ecorepemce_sect%d",pname,isect);
      sprintf(htitle,"%s ecore/momentum vs momentum emce in Sect%d",ptitle,isect);
      Register( h2_ecorepemce_sect[isect] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      //
      sprintf(hname,"%s_h2_ecorepemceX12_sect%d",pname,isect);
      sprintf(htitle,"%s ecore/momentum vs momentum emce in Sect%d",ptitle,isect);
      Register( h2_ecorepemceX12_sect[isect] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      //
      imul = 6;
      while( imul-- ){
	//
	sprintf(hname,"%s_h2_ecorrpemce_sectmul%d_%d",pname,isect,imul);
	sprintf(htitle,"%s ecorr/momentum vs momentum emce in Sect%d, Multi%d",ptitle,isect,imul);
	Register( h2_ecorrpemce_sectmul[isect][imul] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
	//
	sprintf(hname,"%s_h2_ecorrpemceX12_sectmul%d_%d",pname,isect,imul);
	sprintf(htitle,"%s ecorr/momentum vs momentum emce in Sect%d, Multi%d",ptitle,isect,imul);
	Register( h2_ecorrpemceX12_sectmul[isect][imul] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
	//
	sprintf(hname,"%s_h2_ecorepemce_sectmul%d_%d",pname,isect,imul);
	sprintf(htitle,"%s ecore/momentum vs momentum emce in Sect%d, Multi%d",ptitle,isect,imul);
	Register( h2_ecorepemce_sectmul[isect][imul] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
	//
	sprintf(hname,"%s_h2_ecorepemceX12_sectmul%d_%d",pname,isect,imul);
	sprintf(htitle,"%s ecore/momentum vs momentum emce in Sect%d, Multi%d",ptitle,isect,imul);
	Register( h2_ecorepemceX12_sectmul[isect][imul] = new TH2F(hname,htitle,20,0,2,150,0,1.5) );
      }
      //
      iprob = 2;
      while( iprob-- ){
	sprintf(hname,"%s_h2_pm_sectprob%d_%d",pname,isect,iprob);
	sprintf(htitle,"%s Mom vs Mass in sect%d prob%d",ptitle,isect,iprob);
	Register( h2_pm_sectprob[isect][iprob] = new TH2F(hname,htitle,200,0,2,400,-2,2) );
	//
	sprintf(hname,"%s_h2_pm2_sectprob%d_%d",pname,isect,iprob);
	sprintf(htitle,"%s Mom vs Mass^{2} in sect%d prob%d",ptitle,isect,iprob);
	Register( h2_pm2_sectprob[isect][iprob] = new TH2F(hname,htitle,300,-1,2,400,-2,2) );
      }
    }
  }
  //=============================================================================
  //=============================================================================
  if( strstr(opt,"hide") ){
    TH1::AddDirectory(status);
  }
};
//=====================================================================
bool Hist_ele::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.){
  Hist::Fill(glb,trk,clt,emcpid);
  //------------------------------------------------------- Filling-----
  if( trk.quality > 20 && trk.ptot < 10 &&
      fabs(clt.pos[1]-trk.proj[1])<10 &&
      fabs(clt.pos[2]-trk.proj[2])<10 
      ) {
    if( 1 ) {             //------------------------ iepid == ELECTRON || iepid == POSITRON ) {
      // RICH Electron Calibration
      h_ep_sect[isect]->Fill( clt.ecore/ trk.ptot );
      h2_ep_sect[isect]->Fill(trk.ptot, clt.e/ trk.ptot );
      h2_ecorrp_sect[isect]->Fill(trk.ptot, clt.ecorr/ trk.ptot );
      h2_ecorep_sect[isect]->Fill(trk.ptot, clt.ecore/ trk.ptot );
      if( trk.quality == 23 || trk.quality == 31 )
	h2_ecorepX12_sect[isect]->Fill(trk.ptot, clt.ecore/ trk.ptot );
      if( imul >= 0 ){
	h2_ecorrp_sectmul[isect][imul]->Fill(trk.ptot, clt.ecorr/ trk.ptot );
	if( trk.quality == 23 || trk.quality == 31 )
	  h2_ecorrpX12_sectmul[isect][imul]->Fill(trk.ptot, clt.ecorr/ trk.ptot );
	h2_ecorep_sectmul[isect][imul]->Fill(trk.ptot, clt.ecore/ trk.ptot );
	if( trk.quality == 23 || trk.quality == 31 )
	  h2_ecorepX12_sectmul[isect][imul]->Fill(trk.ptot, clt.ecore/ trk.ptot );
      }
      // Particle ID by RICH + EMC electron candidate
      h2_pm_sectprob[isect][iprob]->Fill(emcpid.mass,emcpid.signedmom);
      h2_pm2_sectprob[isect][iprob]->Fill(emcpid.mass2,emcpid.signedmom);
      //if( emce == 3 ){
      if( iemce == 2 ){
	// RICH + EMC Electron Calibration // emcpid.mass<0.1
	h_epemce_sect[isect]->Fill( clt.e/ trk.ptot );
	h2_epemce_sect[isect]->Fill(trk.ptot, clt.e/ trk.ptot );
	h2_ecorrpemce_sect[isect]->Fill(trk.ptot, clt.ecorr/ trk.ptot );
	h2_ecorepemce_sect[isect]->Fill(trk.ptot, clt.ecore/ trk.ptot );
	if( trk.quality == 23 || trk.quality == 31 )
	  h2_ecorepemceX12_sect[isect]->Fill(trk.ptot, clt.ecore/ trk.ptot );
	if( imul >= 0 ){
	  h2_ecorrpemce_sectmul[isect][imul]->Fill(trk.ptot, clt.ecorr/ trk.ptot );
	  if( trk.quality == 23 || trk.quality == 31 )
	    h2_ecorrpemceX12_sectmul[isect][imul]->Fill(trk.ptot, clt.ecorr/ trk.ptot );
	  h2_ecorepemce_sectmul[isect][imul]->Fill(trk.ptot, clt.ecore/ trk.ptot );
	  if( trk.quality == 23 || trk.quality == 31 )
	    h2_ecorepemceX12_sectmul[isect][imul]->Fill(trk.ptot, clt.ecore/ trk.ptot );
	}
      }
    }
  }
    
  return true;
};
//=====================================================================


#ifdef SKIPKSIPSKIPSKIP
//=====================================================================
//=====================================================================
//=====================================================================
//=====================================================================
//=====================================================================
int Hist_ele::Read(char* pname,TDirectory* dir = gDirectory){
  isect = 8;
  while( isect-- ){
    if( IsValidSector(isect) ){
      //----------------------
      sprintf(hname,"%s_h_ep_sect%d",pname,isect);
      h_ep_sect[isect] = (TH1F*)(gROOT->FindObject(hname));
      sprintf(hname,"%s_h2_ep_sect%d",pname,isect);
      h2_ep_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
      sprintf(hname,"%s_h2_ecorrp_sect%d",pname,isect);
      h2_ecorrp_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
      sprintf(hname,"%s_h2_ecorep_sect%d",pname,isect);
      h2_ecorep_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
      sprintf(hname,"%s_h2_ecorepX12_sect%d",pname,isect);
      h2_ecorepX12_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
      //
      if( h_ep_sect[isect] == 0 || h2_ep_sect[isect] == 0 ||
	  h2_ecorrp_sect[isect] == 0 || h2_ecorep_sect[isect] == 0 || 
	  h2_ecorepX12_sect[isect] == 0 ){
	cerr<<" Can't fetch histgram : "<<hname<<endl;
	exit(0);
      }
      //
      imul = 6;
      while( imul-- ){
	sprintf(hname,"%s_h2_ecorrp_sectmul%d_%d",pname,isect,imul);
	h2_ecorrp_sectmul[isect][imul] = (TH2F*)(gROOT->FindObject(hname));
	sprintf(hname,"%s_h2_ecorrpX12_sectmul%d_%d",pname,isect,imul);
	h2_ecorrpX12_sectmul[isect][imul] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_ecorrp_sectmul[isect][imul] == 0 || h2_ecorrpX12_sectmul[isect][imul] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
	sprintf(hname,"%s_h2_ecorep_sectmul%d_%d",pname,isect,imul);
	h2_ecorep_sectmul[isect][imul] = (TH2F*)(gROOT->FindObject(hname));
	sprintf(hname,"%s_h2_ecorepX12_sectmul%d_%d",pname,isect,imul);
	h2_ecorepX12_sectmul[isect][imul] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_ecorep_sectmul[isect][imul] == 0 || h2_ecorepX12_sectmul[isect][imul] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
      }
      //----------------------
      sprintf(hname,"%s_h_epemce_sect%d",pname,isect);
      h_epemce_sect[isect] = (TH1F*)(gROOT->FindObject(hname));
      sprintf(hname,"%s_h2_epemce_sect%d",pname,isect);
      h2_epemce_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
      sprintf(hname,"%s_h2_ecorrpemce_sect%d",pname,isect);
      h2_ecorrpemce_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
      sprintf(hname,"%s_h2_ecorepemce_sect%d",pname,isect);
      h2_ecorepemce_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
      sprintf(hname,"%s_h2_ecorepemceX12_sect%d",pname,isect);
      h2_ecorepemceX12_sect[isect] = (TH2F*)(gROOT->FindObject(hname));
      //
      if( h_epemce_sect[isect] == 0 || h2_epemce_sect[isect] == 0 ||
	  h2_ecorrpemce_sect[isect] == 0 || h2_ecorepemce_sect[isect] == 0 || 
	  h2_ecorepemceX12_sect[isect] == 0 ){
	cerr<<" Can't fetch histgram : "<<hname<<endl;
	exit(0);
      }
      //
      imul = 6;
      while( imul-- ){
	sprintf(hname,"%s_h2_ecorrpemce_sectmul%d_%d",pname,isect,imul);
	h2_ecorrpemce_sectmul[isect][imul] = (TH2F*)(gROOT->FindObject(hname));
	sprintf(hname,"%s_h2_ecorrpemceX12_sectmul%d_%d",pname,isect,imul);
	h2_ecorrpemceX12_sectmul[isect][imul] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_ecorrpemce_sectmul[isect][imul] == 0 || h2_ecorrpemceX12_sectmul[isect][imul] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
	//
	sprintf(hname,"%s_h2_ecorepemce_sectmul%d_%d",pname,isect,imul);
	h2_ecorepemce_sectmul[isect][imul] = (TH2F*)(gROOT->FindObject(hname));
	sprintf(hname,"%s_h2_ecorepemceX12_sectmul%d_%d",pname,isect,imul);
	h2_ecorepemceX12_sectmul[isect][imul] = (TH2F*)(gROOT->FindObject(hname));
	if( h2_ecorepemce_sectmul[isect][imul] == 0 || h2_ecorepemceX12_sectmul[isect][imul] == 0 ){
	  cerr<<" Can't fetch histgram : "<<hname<<endl;
	  exit(0);
	}
      }
    }
  }
};
//=====================================================================
#endif
