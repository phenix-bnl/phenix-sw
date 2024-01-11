
#include "Hist_miptwr2.hh"

ClassImp(Hist_miptwr2)
//=====================================================================
Hist_miptwr2::Hist_miptwr2(){
};
//=====================================================================
Hist_miptwr2::Hist_miptwr2(char* pname,char* ptitle,char* opt) : Hist(pname,ptitle){
  bool status;
  if( strstr(opt,"hide") ){
    status = TH1::AddDirectoryStatus();
    TH1::AddDirectory(false);
  }
  //=============================================================================
  //=============================================================================
  isect = 6;
  while( isect-- ){
    itwr_sect = (isect < 6 ) ? 2592 : 4608;
    ism_sect = (isect < 6 ) ? 18 : 32;
    //
    sprintf(hname,"%s_sec_sect%d",pname,isect);
    sprintf(htitle,"%s MIP Sector Calibration sect %d",pname,isect);
    Register( h_sec_sect[isect] = new TH1F(hname,htitle,100,0,1) );
    //
    sprintf(hname,"%s_sm_sect%d",pname,isect);
    sprintf(htitle,"%s MIP SM Calibration sect %d",pname,isect);
    h_sm_sect[isect] = new CalibRunsTH1(hname,htitle,ism_sect);
    h_sm_sect[isect]->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
    //
    sprintf(hname,"%s_twr_sect%d",pname,isect);
    sprintf(htitle,"%s MIP tower Calibration sect %d",pname,isect);
    h_twr_sect[isect] = new CalibRunsTH1(hname,htitle,itwr_sect);
    h_twr_sect[isect]->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
    //
    sprintf(hname,"%s_sm_etof0_sect%d",pname,isect);
    sprintf(htitle,"%s MIP SM Calib 0.6-1.0GeV/c low-multi sect %d",pname,isect);
    if( isect < 6 )
      h_sm_etof0_sect[isect] = new CalibRunsTH2(hname,htitle,ism_sect,100,0,1,125,-50,-25);
    else
      h_sm_etof0_sect[isect] = new CalibRunsTH2(hname,htitle,ism_sect,100,0,1,125,-12.5,12.5);
    //
    sprintf(hname,"%s_sm_etof1_sect%d",pname,isect);
    sprintf(htitle,"%s MIP SM Calib 0.9-1.1GeV/c low-multi sect %d",pname,isect);
    if( isect < 6 )
      h_sm_etof1_sect[isect] = new CalibRunsTH2(hname,htitle,ism_sect,100,0,1,125,-50,-25);
    else
      h_sm_etof1_sect[isect] = new CalibRunsTH2(hname,htitle,ism_sect,100,0,1,125,-12.5,12.5);
    //
    sprintf(hname,"%s_twr_etof0_sect%d",pname,isect);
    sprintf(htitle,"%s MIP Tower Calib 0.6-1.0GeV/c low-multi sect %d",pname,isect);
    if( isect < 6 )
      h_twr_etof0_sect[isect] = new CalibRunsTH2(hname,htitle,itwr_sect,40,0.1,0.5,125,-50,-25);
    else
      h_twr_etof0_sect[isect] = new CalibRunsTH2(hname,htitle,itwr_sect,40,0.1,0.5,125,-12.5,12.5);

#ifdef FINECALIB
    //
    sprintf(hname,"%s_twr_etof1_sect%d",pname,isect);
    sprintf(htitle,"%s MIP Tower Calib 0.9-1.1GeV/c low-multi sect %d",pname,isect);
    if( isect < 6 )
      h_twr_etof1_sect[isect] = new CalibRunsTH2(hname,htitle,itwr_sect,40,0.1,0.5,125,-50,-25);
    else
      h_twr_etof1_sect[isect] = new CalibRunsTH2(hname,htitle,itwr_sect,40,0.1,0.5,125,-12.5,12.5);
#endif
    //
  }

  //=============================================================================
  //=============================================================================
  if( strstr(opt,"hide") ){
    TH1::AddDirectory(status);
  }
};
//=====================================================================
bool Hist_miptwr2::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.0){
  Hist::Fill(glb,trk,clt,emcpid);
  //------------------------------------------------------- Filling-----
  if( trk.quality > 20 &&
      trk.ptot < 10 &&
      fabs(clt.pos[1]-trk.proj[1])<20 &&
      fabs(clt.pos[2]-trk.proj[2])<20 &&
      isect < 6 
      ) {
    //=============================================================================
    //=============================================================================
    // MIP Tower-by-Tower Calibration // all particle 0.4GeV/c - 2.0GeV/c  // && clt.prob < 0.9 ) {
    if( trk.ptot > 0.4 && trk.ptot < 2.0 ){
      float time_pi;
      float beta;
      beta = trk.ptot / sqrt( trk.ptot*trk.ptot + 0.14*0.14 ); // pi+- mass
      time_pi = clt.tofcorr + 
	sqrt( clt.pos[0]*clt.pos[0] + clt.pos[1]*clt.pos[1] + (clt.pos[2]-glb.bbcz)*(clt.pos[2]-glb.bbcz) ) / 29.979 -
	trk.pathl/29.979/beta ;
      time_pi = clt.tofcorr; 
      h_sec_sect[isect]->Fill(clt.e * angle,weight);
      h_sm_sect[isect]->Fill(glb.run,glb.seq,ism_sect,clt.e*angle,weight);
      h_twr_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.e*angle,weight);
      if( imul < 2 &&  trk.ptot>0.6 && trk.ptot < 1.0 ){
	h_sm_etof0_sect[isect]->Fill(glb.run,glb.seq,ism_sect,clt.e*angle,time_pi,weight);
	if( isect < 6 ) h_twr_etof0_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.e*angle,time_pi,weight);
#ifdef FINECALIB
#endif
	if( trk.ptot > 0.9 && trk.ptot < 1.1 ){
	  h_sm_etof1_sect[isect]->Fill(glb.run,glb.seq,ism_sect,clt.e*angle,time_pi,weight);
#ifdef FINECALIB
	  h_twr_etof1_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.e*angle,time_pi,weight);
#endif
	}
      }
    }
    //=============================================================================
    //=============================================================================
  } // End for MIP histgrams-----------
  //------------------------------------------------------- Filling-----
  
  return true;
};
//=====================================================================
bool Hist_miptwr2::Add(TFile* f){
  bool status = true;
  int read_num = h_sm_sect[0]->Readable(f);
  isect = 6;
  while( isect-- ){
    if( status &&
	read_num == h_sm_sect[isect]->Readable(f) &&
	read_num == h_twr_sect[isect]->Readable(f) &&
	read_num == h_sm_etof0_sect[isect]->Readable(f) &&
	read_num == h_sm_etof1_sect[isect]->Readable(f) &&
	read_num == h_twr_etof0_sect[isect]->Readable(f) ){
    } else{
      status = false;
    }
  }
  if( status ){
    Hist::Add(f);
    isect = 6;
    while( isect-- ){
      h_twr_sect[isect]->Append(f);
      h_sm_sect[isect]->Append(f);
      h_sm_etof0_sect[isect]->Append(f);
      h_sm_etof1_sect[isect]->Append(f);
      h_twr_etof0_sect[isect]->Append(f);
    }
  } else{
    cout<<" "<<GetName()<<"::Add() ERROR!!!! Readable number is strange ... Need experts "<<endl;
    cout<<" "<<GetName()<<"::Add() ERROR!!!! Filename : "<<f->GetName()<<endl;
  }
  return status;
};
//=====================================================================
int Hist_miptwr2::Write(const char* name = 0, Int_t option = 0, Int_t bufsize = 0){
  cout<<" "<<GetName()<<":: Write() private Hist_miptwr2::Write() method... "<<endl;
  Hist::Write(name,option,bufsize);
  int isect = 6;
  while( isect-- ){
    h_sec_sect[isect]->Write();
    h_sm_sect[isect]->Write();
    h_twr_sect[isect]->Write();
    h_sm_etof0_sect[isect]->Write();
    h_sm_etof1_sect[isect]->Write();
    h_twr_etof0_sect[isect]->Write();
#ifdef FINECALIB
    h_twr_etof1_sect[isect]->Write();
#endif
  }
};
//=====================================================================
