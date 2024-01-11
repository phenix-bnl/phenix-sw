
#include "Hist_mip.hh"

ClassImp(Hist_mip)
//=====================================================================
Hist_mip::Hist_mip(){
};
//=====================================================================
Hist_mip::Hist_mip(char* pname,char* ptitle,char* opt) : Hist(pname,ptitle){
  bool status;
  if( strstr(opt,"hide") ){
    status = TH1::AddDirectoryStatus();
    TH1::AddDirectory(false);
  }
  //=============================================================================
  //=============================================================================
  // MIP Monitor Sector-by-Sector
  isect = 8;
  while( isect-- ){
    if( IsValidSector(isect) ){
      //
      sprintf(hname,"%s_h_e_sect%d",pname,isect);
      sprintf(htitle,"%s:Cluster Energy in Sect%d",ptitle,isect);
      Register( h_e_sect[isect] = new TH1F(hname,htitle,100,0,1.0) );
      sprintf(hname,"%s_h2_emom_sect%d",pname,isect);
      sprintf(htitle,"%s:Cluster Energy vs Mom in Sect%d",ptitle,isect);
      Register( h2_emom_sect[isect] = new TH2F(hname,htitle,30,0,3.0,300,0,3.0) );
      sprintf(hname,"%s_h2_eangmom_sect%d",pname,isect);
      sprintf(htitle,"%s:Cluster Energy * cos(angle) vs Mom in Sect%d",ptitle,isect);
      Register( h2_eangmom_sect[isect] = new TH2F(hname,htitle,30,0,3.0,300,0,3.0) );
      //
      imul = 6;
      while( imul-- ){
	sprintf(hname,"%s_h2_mommul_sect%d_%d",pname,isect,imul);
	sprintf(htitle,"%s:Angled Cluster Energy vs Mom in Sect%d Multi%d",ptitle,isect,imul);
	Register( h2_eangmom_sectmul[isect][imul] = new TH2F(hname,htitle,30,0,3.0,300,0,3.0) );
      }
      //
      ipid = 8;
      while( ipid-- ){
	sprintf(hname,"%s_h2_eangmom_lowmul_sectpid%d_%d",pname,isect,ipid);
	sprintf(htitle,"%s:Cluster Energy * cos(angle) vs Mom in low multi, <6deg in Sect%d PID%d",ptitle,isect,ipid);
	Register( h2_eangmom_lowmul_sectpid[isect][ipid] = new TH2F(hname,htitle,20,0,2.0,300,0,3.0) );
	//
	if( ipid != ELECTRON || ipid != POSITRON ){
	  sprintf(hname,"%s_h2_eangmom_sectpid%d_%d",pname,isect,ipid);
	  sprintf(htitle,"%s:Cluster Energy * cos(angle) vs Mom in Sect%d PID%d",ptitle,isect,ipid);
	  Register( h2_eangmom_sectpid[isect][ipid] = new TH2F(hname,htitle,20,0,2.0,300,0,3.0) );
	} else {
	  sprintf(hname,"%s_h2_eangmom_sectpid%d_%d",pname,isect,ipid);
	  sprintf(htitle,"%s:Ecore vs Mom in Sect%d PID%d",ptitle,isect,ipid);
	  Register( h2_eangmom_sectpid[isect][ipid] = new TH2F(hname,htitle,20,0,2.0,300,0,3.0) );
	}
      }
      //
    }
  }
  //=============================================================================
  //=============================================================================
  // MIP SM-by-SM Calibration
  isect = 8;
  while( isect-- ){
    if( IsValidSector(isect) ){
      ism_sect = (isect>=6 ? 32 : 18 );
      sprintf(hname,"%s_h2_eangsm_sect%d",pname,isect);
      sprintf(htitle,"%s:Cluster Energy in Sect%d",ptitle,isect);
      Register( h2_eangsm_sect[isect] = new TH2F(hname,htitle,100,0,1.0,ism_sect,-0.5,(float)ism_sect-0.5) );
      sprintf(hname,"%s_h2_ecentesm_sect%d",pname,isect);
      sprintf(htitle,"%s:Cluster Energy in Sect%d",ptitle,isect);
      Register( h2_ecentesm_sect[isect] = new TH2F(hname,htitle,100,0,1,ism_sect,-0.5,(float)ism_sect-0.5) );
      sprintf(hname,"%s_h2_angsm_sect%d",pname,isect);
      sprintf(htitle,"%s:Cluster Angle in Sect%d",ptitle,isect);
      Register( h2_angsm_sect[isect] = new TH2F(hname,htitle,50,0.5,1.0,ism_sect,-0.5,(float)ism_sect-0.5) );
    }
  }
  //=============================================================================
  //=============================================================================
  if( strstr(opt,"hide") ){
    TH1::AddDirectory(status);
  }
};
//=====================================================================
bool Hist_mip::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.){
  Hist::Fill(glb,trk,clt,emcpid);
  //------------------------------------------------------- Filling-----
  //  if( trk.quality > 20 &&
  //      trk.ptot < 10 &&
  //      fabs(clt.pos[1]-trk.proj[1])<20 &&
  //      fabs(clt.pos[2]-trk.proj[2])<20 
  //      ) {
  if( trk.ptot < 10 ){
    //=============================================================================
    //=============================================================================
    // MIP Monitor Sector-by-Sector
    if( trk.ptot > 0.3 )
      h_e_sect[isect]->Fill(clt.e * angle );
    h2_emom_sect[isect]->Fill(trk.ptot,clt.e);
    h2_eangmom_sect[isect]->Fill(trk.ptot,clt.e * angle);
    if( iepid == ELECTRON || iepid == POSITRON ){
      h2_eangmom_sectpid[isect][iepid]->Fill(trk.ptot,clt.ecore);
      if( imul == 0 || imul == 1 )
	h2_eangmom_lowmul_sectpid[isect][ipid]->Fill(trk.ptot,clt.ecore);
    } else if( ipid > -1 && ipid != ELECTRON && ipid != ELECTRON ){
      h2_eangmom_sectpid[isect][ipid]->Fill(trk.ptot,clt.e * angle);
      if( imul == 0 || imul == 1 )
	h2_eangmom_lowmul_sectpid[isect][ipid]->Fill(trk.ptot,clt.e * angle);
    }
    if( clt.prob < 0.9 && imul >= 0 && ( ipid == PIONPLUS || ipid == PIONMINUS ) ){
      h2_eangmom_sectmul[isect][imul]->Fill(trk.ptot,clt.e * angle);
      //      if( trk.ptot > 0.6 && trk.ptot < 1.0 && imul < 2  )
      //	h_e_sect[isect]->Fill(clt.e * angle );
    }
    //=============================================================================
    //=============================================================================
    // MIP SM-by-SM Calibration
    if( clt.prob < 0.9 && imul >=0 && ( ipid == PIONPLUS || ipid == PIONMINUS ) ){
      if( trk.ptot > 0.6 && trk.ptot < 1.0 ){
	if( imul < 2 ){
	  h2_eangsm_sect[isect]->Fill(clt.e *angle ,ism_sect );
	}
	if( clt.e < 0.4 && clt.e > 0.25 ){
	  h2_ecentesm_sect[isect]->Fill(clt.ecent/clt.e, ism_sect);
	  h2_angsm_sect[isect]->Fill(angle, ism_sect);
	}
      }
      //
    }
    //=============================================================================
    //=============================================================================
  } // End for MIP histgrams-----------
  //------------------------------------------------------- Filling-----

  return true;
};
//=====================================================================
