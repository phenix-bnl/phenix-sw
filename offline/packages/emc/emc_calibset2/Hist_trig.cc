
#include "Hist_trig.hh"

ClassImp(Hist_trig)
//=====================================================================
Hist_trig::Hist_trig(){
};
//=====================================================================
Hist_trig::Hist_trig(char* pname,char* ptitle,char* opt) : Hist(pname,ptitle){
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
      sprintf(hname,"%s_h_e_sect%d",pname,isect);
      sprintf(htitle,"%s:Cluster Energy in Sect%d",ptitle,isect);
      Register( h_e_sect[isect] = new TH1F(hname,htitle,100,0,1.0) );
      sprintf(hname,"%s_h2_emom_sect%d",pname,isect);
      sprintf(htitle,"%s:Cluster Energy vs Mom in Sect%d",ptitle,isect);
      Register( h2_emom_sect[isect] = new TH2F(hname,htitle,30,0,3.0,300,0,3.0) );
      sprintf(hname,"%s_h2_e22mom_sect%d",pname,isect);
      sprintf(htitle,"%s:2x2 energy vs Mom in Sect%d",ptitle,isect);
      Register( h2_e22mom_sect[isect] = new TH2F(hname,htitle,30,0,3.0,300,0,3.0) );
      sprintf(hname,"%s_h2_e44mom_sect%d",pname,isect);
      sprintf(htitle,"%s:4x4 energy vs Mom in Sect%d",ptitle,isect);
      Register( h2_e44mom_sect[isect] = new TH2F(hname,htitle,30,0,3.0,300,0,3.0) );
      sprintf(hname,"%s_h2_epartmom_sect%d",pname,isect);
      sprintf(htitle,"%s:partsum energy vs Mom in Sect%d",ptitle,isect);
      Register( h2_epartmom_sect[isect] = new TH2F(hname,htitle,30,0,3.0,300,0,3.0) );
    }
  }
  //=============================================================================
  //=============================================================================
  if( strstr(opt,"hide") ){
    TH1::AddDirectory(status);
  }
};
//=====================================================================
bool Hist_trig::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid){
  Hist::Fill(glb,trk,clt,emcpid);
  //------------------------------------------------------- Filling-----
  if( trk.quality > 20 &&
      trk.ptot < 10 &&
      fabs(clt.pos[1]-trk.proj[1])<20 &&
      fabs(clt.pos[2]-trk.proj[2])<20 
      ) {
    if( trk.ptot > 0.3 )
      h_e_sect[isect]->Fill(clt.e * angle );
    h2_emom_sect[isect]->Fill(trk.ptot,clt.e);
    h2_e22mom_sect[isect]->Fill(trk.ptot,clt.e22 );
    h2_e44mom_sect[isect]->Fill(trk.ptot,clt.e44 );
    h2_epartmom_sect[isect]->Fill(trk.ptot,clt.epart );
  } // End for MIP histgrams-----------
  //------------------------------------------------------- Filling-----

  return true;
};
//=====================================================================
