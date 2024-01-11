
#include "Hist_miptwr.hh"

ClassImp(Hist_miptwr)
//=====================================================================
Hist_miptwr::Hist_miptwr(){
};
//=====================================================================
Hist_miptwr::Hist_miptwr(char* pname,char* ptitle,char* opt) : Hist(pname,ptitle){
  bool status;
  if( strstr(opt,"hide") ){
    status = TH1::AddDirectoryStatus();
    TH1::AddDirectory(false);
  }
  //=============================================================================
  //=============================================================================
  isect = 8;
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
    sprintf(hname,"%s_secpid_sect%d",pname,isect);
    sprintf(htitle,"%s MIP Sector Calibration PID sect %d",pname,isect);
    Register( h_secpid_sect[isect] = new TH1F(hname,htitle,100,0,1) );
    //
    sprintf(hname,"%s_smpid_sect%d",pname,isect);
    sprintf(htitle,"%s MIP SM Calibration PID sect %d",pname,isect);
    h_smpid_sect[isect] = new CalibRunsTH1(hname,htitle,ism_sect);
    h_smpid_sect[isect]->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
    //
    //    sprintf(hname,"%s_twrpid_sect%d",pname,isect);
    //    sprintf(htitle,"%s MIP tower Calibration PID sect %d",pname,isect);
    //    h_twrpid_sect[isect] = new CalibRunsTH1(hname,htitle,itwr_sect);
    //    h_twrpid_sect[isect]->SetOptPrefit(CalibRunsTH1::kPrefit_MIP);
    //
    sprintf(hname,"%s_secpid10_sect%d",pname,isect);
    sprintf(htitle,"%s MIP Sector Calibration PID sect %d",pname,isect);
    Register( h_secpid10_sect[isect] = new TH1F(hname,htitle,100,0,1) );

  }

  //=============================================================================
  //=============================================================================
  if( strstr(opt,"hide") ){
    TH1::AddDirectory(status);
  }
};
//=====================================================================
bool Hist_miptwr::Add(TFile* f){
  bool status = true;
  int read_num = h_sm_sect[0]->Readable(f);
  isect = 8;
  while( isect-- ){
    if( status &&
	read_num == h_sm_sect[isect]->Readable(f) &&
	read_num == h_twr_sect[isect]->Readable(f) &&
	read_num == h_smpid_sect[isect]->Readable(f) ){
    } else{
      status = false;
    }
  }
  if( status ){
    Hist::Add(f);
    isect = 8;
    while( isect-- ){
      h_twr_sect[isect]->Append(f);
      h_sm_sect[isect]->Append(f);
      h_smpid_sect[isect]->Append(f);
    }
  } else{
    cout<<" "<<GetName()<<"::Add() ERROR!!!! Readable number is strange ... Need experts "<<endl;
    cout<<" "<<GetName()<<"::Add() ERROR!!!! Filename : "<<f->GetName()<<endl;
  }
  return status;
};
//=====================================================================
bool Hist_miptwr::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.){
  Hist::Fill(glb,trk,clt,emcpid);
  //------------------------------------------------------- Filling-----
  if( trk.quality > 20 &&
      trk.ptot < 10 &&
      fabs(clt.pos[1]-trk.proj[1])<20 &&
      fabs(clt.pos[2]-trk.proj[2])<20 
      ) {
    //=============================================================================
    //=============================================================================
    // MIP Tower-by-Tower Calibration // all particle 0.4GeV/c - 2.0GeV/c
    if( trk.ptot > 0.4 && trk.ptot < 2.0 ){                  // && clt.prob < 0.9 ) {
      h_sec_sect[isect]->Fill(clt.e * angle );
      h_sm_sect[isect]->Fill(glb.run,glb.seq,ism_sect,clt.e * angle );
      h_twr_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.e * angle );
      if( imul < 2 &&  //trk.ptot>0.6 && trk.ptot < 1.0 && 
	  ( emcpid.pid == PIONPLUS ||  emcpid.pid == PIONMINUS ) ){
	h_secpid_sect[isect]->Fill( clt.e * angle );
	h_smpid_sect[isect]->Fill(glb.run,glb.seq,ism_sect,clt.e * angle );
	//h_twrpid_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.e * angle );
	if( trk.ptot > 0.9 && trk.ptot < 1.1 )
	  h_secpid10_sect[isect]->Fill( clt.e * angle );
      }
    }
    //=============================================================================
    //=============================================================================
#ifdef SKIPSKIP
    // MIP Tower-by-Tower Calibration // all particle > 0.4GeV
    TH1F* h_twr_sm[8][16][12];              //[8][16][12]
    TH1F* h_twr[24768];                     //[24768]

    //=============================================================================
    //=============================================================================
    // MIP Tower-by-Tower Calibration // all particle 0.4GeV/c - 2.0GeV/c, prob<0.9
    if( clt.prob < 0.9 ){
      if( trk.ptot > 0.4 && trk.ptot < 2.0 ) {
	//	if( ( isect < 6 &&  // 37cm * 0.2 = 7.4 // 37cm*0.4 = 14.8
	//	      emcproj_pos[0]>7.4/twr_size[isect][0] && emcproj_pos[0]<72.-7.4/twr_size[isect][0] &&
	//	      emcproj_pos[1]>14.8/twr_size[isect][1] && emcproj_pos[1]<36.-14.8/twr_size[isect][1]
	//	      ) ||
	//	    ( isect >= 6 && // 40cm * 0.2 = 8.0  // 40cm * 0.4 = 16.0
	//	      emcproj_pos[0]>8./twr_size[isect][0] && emcproj_pos[0]<96.-8./twr_size[isect][0] && 
	//	      emcproj_pos[1]>16./twr_size[isect][1] && emcproj_pos[1]<48.-8./twr_size[isect][1])
	//	    ) {
	h_twr_sm[isect][ismz_sect][ismy_sect]->Fill(clt.e * angle );
	h_twr[itwr]->Fill(clt.e * angle );
	//	}
      }
    }
    //=============================================================================
    //=============================================================================
#endif
    //=============================================================================
    //=============================================================================
  } // End for MIP histgrams-----------
  //------------------------------------------------------- Filling-----

  return true;
};
//=====================================================================
int Hist_miptwr::Write(const char* name = 0, Int_t option = 0, Int_t bufsize = 0){
  cout<<" "<<GetName()<<":: Write() private Hist_miptwr::Write() method... "<<endl;
  Hist::Write(name,option,bufsize);
  int isect = 8;
  while( isect-- ){
    h_sec_sect[isect]->Write();
    h_sm_sect[isect]->Write();
    h_twr_sect[isect]->Write();
    h_secpid_sect[isect]->Write();
    h_smpid_sect[isect]->Write();
    //h_twrpid_sect[isect]->Write();
    h_secpid10_sect[isect]->Write();
  }
};
//=====================================================================
