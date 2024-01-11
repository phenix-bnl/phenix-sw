
#include "Hist_eqa.hh"

ClassImp(Hist_eqa)
//=====================================================================
Hist_eqa::Hist_eqa(){
};
//=====================================================================
Hist_eqa::Hist_eqa(char* pname,char* ptitle,char* opt) : Hist(pname,ptitle){
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
    //
    sprintf(hname,"%s_ene_sect%d",pname,isect);
    sprintf(htitle,"QA energy sect %d",isect);
    ene_sect[isect] = new CalibRunsTH0(hname,htitle,itwr_sect);
    //
    sprintf(hname,"%s_evn_sect%d",pname,isect);
    sprintf(htitle,"QA event number sect %d",isect);
    evn_sect[isect] = new CalibRunsTH0(hname,htitle,itwr_sect);
    //
    sprintf(hname,"%s_e_sect%d",pname,isect);
    sprintf(htitle,"%s Energy sect %d",pname,isect);
    Register( h_e_sect[isect] = new TH1F(hname,htitle,110,-0.1,1) );

  }

  //=============================================================================
  //=============================================================================
  if( strstr(opt,"hide") ){
    TH1::AddDirectory(status);
  }
};
//=====================================================================
bool Hist_eqa::Add(TFile* f){
  bool status = true;
  int read_num = ene_sect[0]->Readable(f);
  isect = 8;
  while( isect-- ){
    if( status &&
	read_num == ene_sect[isect]->Readable(f) &&
	read_num == evn_sect[isect]->Readable(f) ){
    } else{
      status = false;
    }
  }
  if( status ){
    Hist::Add(f);
    isect = 8;
    while( isect-- ){
      ene_sect[isect]->Append(f);
      evn_sect[isect]->Append(f);
    }
  } else{
    cout<<" "<<GetName()<<"::Add() ERROR!!!! Readable number is strange ... Need experts "<<endl;
    cout<<" "<<GetName()<<"::Add() ERROR!!!! Filename : "<<f->GetName()<<endl;
  }
  return status;
};
//=====================================================================
bool Hist_eqa::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.){
  Hist::Fill(glb,trk,clt,emcpid);
  //------------------------------------------------------- Filling-----
  if( isect >= 0 && isect < 8 ){  //&& clt.ecent > 0.05 
    evn_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,1);
    ene_sect[isect]->Fill(glb.run,glb.seq,itwr_sect,clt.ecent);
  }
  h_e_sect[isect]->Fill(clt.ecent);
  //------------------------------------------------------- Filling-----
  return true;
};
//=====================================================================
int Hist_eqa::Write(const char* name = 0, Int_t option = 0, Int_t bufsize = 0){
  cout<<" "<<GetName()<<":: Write() private Hist_eqa::Write() method... "<<endl;
  Hist::Write(name,option,bufsize);
  int isect = 8;
  while( isect-- ){
    evn_sect[isect]->Write();
    ene_sect[isect]->Write();
  }
};
//=====================================================================
