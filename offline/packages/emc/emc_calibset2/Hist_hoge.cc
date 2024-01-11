
#include "Hist_hoge.hh"

ClassImp(Hist_hoge)
//=====================================================================
Hist_hoge::Hist_hoge(){
  //  TH1::AddDirectory(false);
  cout<<" Hist_hoge::default const"<<endl; 
#ifdef SKIPSKIP
  isect = 8;
  while( isect-- ){
    if( IsValidSector(isect) ){
      h_sect[isect] = (TH1F*)next(1);
      h2_sect[isect] = (TH2F*)next(2);
    }
  }
#endif
};
//=====================================================================
Hist_hoge::~Hist_hoge(){
  cout<<" Hist_hoge::default de-structor"<<endl; 
};
//=====================================================================
Hist_hoge::Hist_hoge(char* pname,char* ptitle,char* opt) : Hist(pname,ptitle){
  bool status;
  if( strstr(opt,"hide") ){
    status = TH1::AddDirectoryStatus();
    TH1::AddDirectory(false);
  }

  int i = 0;
  //=============================================================================
  //=============================================================================
  // MIP Monitor Sector-by-Sector
  isect = 8;
  while( isect-- ){
    if( _debug) cout<<" "<<GetName()<<":: Hist_hoge::isect = "<<isect<<endl;
    if( IsValidSector(isect) ){
      sprintf(hname,"h_sect%d",isect);
      sprintf(htitle,"Cluster Energy in Sect%d",isect);
      //      h_sect[isect] = new(next(1)) TH1F(hname,htitle,100,0,1.0);
      Register( h_sect[isect] = new TH1F(hname,htitle,100,0,1.0) );
      //
      sprintf(hname,"h2_sect%d",isect);
      sprintf(htitle,"2D Cluster Energy in Sect%d",isect);
      Register( h2_sect[isect] = new TH2F(hname,htitle,100,0,1.0,100,0,1.0) );
    }
  }
  //=============================================================================
  //=============================================================================
  if( strstr(opt,"hide") ){
    TH1::AddDirectory(status);
  }
};
//=====================================================================
bool Hist_hoge::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid){
};
//=====================================================================
