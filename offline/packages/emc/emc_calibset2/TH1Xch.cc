
#include <stdio.h>
#include <stream.h>
#include "TH1Xch.hh"
#include "SumupTH.hh"

ClassImp(TH1Xch)

//------------------------------------------------------------
TH1Xch::TH1Xch(): CalibObj(){
  _nbins = 0;
  _low = 0;
  _up = 0;
  _width = 0;
  _h_all = NULL;
  _h2_ch = NULL;
  _initialized = false;
};
//------------------------------------------------------------
TH1Xch::TH1Xch(const char* name, const char* title) : CalibObj(name,title){
  _nbins = 0;
  _low = 0;
  _up = 0;
  _width = 0;
  _h_all = NULL;
  _h2_ch = NULL;
  _initialized = false;
};
//------------------------------------------------------------
TH1Xch::TH1Xch(const char* name, const char* title,
		     int nch,int nbins,float low,float up): CalibObj(name,title,nch){
  if( up > low ){
    _nch = nch;
    _nbins = nbins;
    _low = low;
    _up = up;
    _width = (up - low)/((float)nbins);
    char hname[256],htitle[256];
    sprintf(hname,"%s_h_all",name);
    sprintf(htitle,"%s All hist",title);
    _h_all = new TH1F(hname,htitle,nbins,low,up);
    sprintf(hname,"%s_h2_ch",name);
    sprintf(htitle,"%s Channel",title);
    _h2_ch = new TH2F(hname,htitle,nbins,low,up,nch,0,nch);
    _initialized = true;
    //cout<<" "<<GetName()<<"  new TH1Xch("<<name<<","<<title<<","<<nch<<","<<nbins<<","<<low<<","<<up<<") "<<endl;
  } else{
    _nch = 0;
    _nbins = 0;
    _low = 0;
    _up = 0;
    _width = 0;
    _h_all = NULL;
    _h2_ch = NULL;
    _initialized = false;
  }
};
//------------------------------------------------------------
TH1Xch::~TH1Xch(){
  if( _initialized ){
    delete _h2_ch;
    delete _h_all;
  }
};
//------------------------------------------------------------
int TH1Xch::Fill(int ch,float x,float w=1.,float ww=1.){
  int stat = 0;
  if( _initialized ){
    _h_all->Fill(x,w);
    stat = _h2_ch->Fill(x,ch,w);
  }
  return stat;
};
//------------------------------------------------------------
void TH1Xch::Reset(char* option){
  if( _initialized ){
    _h2_ch->Reset(option);
    _h_all->Reset(option);
  }
};
//------------------------------------------------------------
int TH1Xch::Add(CalibObj* obj,Double_t c1 , char* opt ){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  TH1Xch* th1 = dynamic_cast<TH1Xch*>(obj);
  if(! th1  )
    return(0);
  int tnum_sumup = 0;
  if( _nch == th1->GetNch() &&
      _width == th1->GetWidth() ){
    tnum_sumup ++;
    _h2_ch->Add(th1->_h2_ch,c1);
    _h_all->Add(th1->_h_all,c1);
  }
  return tnum_sumup;
}
//------------------------------------------------------------
void TH1Xch::Scale(Double_t c1 ){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return;
  }
  _h2_ch->Scale(c1);
  _h_all->Scale(c1);
  return;
}
//------------------------------------------------------------
int TH1Xch::Sumup(TH1Xch* th1,Double_t shift = 0,char* opt){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  int tnum_sumup = 0;
  TCanvas* ctmp ;
  if( strstr(opt,"D") ){
    ctmp =new TCanvas("ctmp","TH1Xch Sumup"); //,1200,1000);
    ctmp->Divide(3);
    ctmp->cd(1);
    _h_all->DrawCopy();
  }
  //tnum_sumup += SumupTH1(_h_all,_h_all,th1->_h_all,0,shift,opt);
  //tnum_sumup += SumupTH2(_h2_ch,_h2_ch,th1->_h2_ch,0,shift,opt);
  tnum_sumup += SumupTH1(_h_all,th1->_h_all,shift,opt);
  tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,shift,opt);
  if( strstr(opt,"D") ){
    ctmp->cd(2);
    th1->_h_all->DrawCopy();
    ctmp->cd(3);
    _h_all->DrawCopy();
    ctmp->cd();
    ctmp->Update();
    cout<<"  shift = "<<shift<<endl;
    getchar();
  }
  return tnum_sumup;
}
//------------------------------------------------------------
int TH1Xch::Sumup(TH1Xch* th1,TH1Xch* th2,Double_t shift1,Double_t shift2,char* opt){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  int tnum_sumup = 0;
  //tnum_sumup += SumupTH1(_h_all,th1->_h_all,th2->_h_all,shift1,shift2,opt);
  //tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,th2->_h2_ch,shift1,shift2,opt);
  _h_all->Clear();
  _h2_ch->Clear();
  tnum_sumup += SumupTH1(_h_all,th1->_h_all,shift1,opt);
  tnum_sumup += SumupTH1(_h_all,th2->_h_all,shift2,opt);
  tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,shift1,opt);
  tnum_sumup += SumupTH2(_h2_ch,th2->_h2_ch,shift2,opt);
  return tnum_sumup;
}
//------------------------------------------------------------
int TH1Xch::Sumup(TH1Xch* th1,TH1* hshift1,char* opt){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  int tnum_sumup = 0;
  //tnum_sumup += SumupTH2(_h2_ch,_h2_ch,th1->_h2_ch,NULL,hshift1,opt);
  tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,hshift1,opt);
  if( tnum_sumup > 0 )
    tnum_sumup += Project();
  return tnum_sumup;
};
//------------------------------------------------------------
int TH1Xch::Sumup(TH1Xch* th1,TGraph* gshift1,char* opt){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  int tnum_sumup = 0;
  //tnum_sumup += SumupTH2(_h2_ch,_h2_ch,th1->_h2_ch,NULL,hshift1,opt);
  tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,gshift1,opt);
  if( tnum_sumup > 0 )
    tnum_sumup += Project();
  return tnum_sumup;
};
//------------------------------------------------------------
int TH1Xch::Sumup(TH1Xch* th1,TH1Xch* th2,TH1* hshift1,TH1* hshift2,char* opt){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  int tnum_sumup = 0;
  //tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,th2->_h2_ch,hshift1,hshift2,opt);
  _h_all->Clear();
  _h2_ch->Clear();
  tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,hshift1,opt);
  tnum_sumup += SumupTH2(_h2_ch,th2->_h2_ch,hshift2,opt);
  if( tnum_sumup > 0 )
    tnum_sumup += Project();
  return tnum_sumup;
};
//------------------------------------------------------------
int TH1Xch::Sumup(TH1Xch* th1,TH1Xch* th2,TGraph* gshift1,TGraph* gshift2,char* opt){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  int tnum_sumup = 0;
  //tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,th2->_h2_ch,gshift1,gshift2,opt);
  _h_all->Clear();
  _h2_ch->Clear();
  tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,gshift1,opt);
  tnum_sumup += SumupTH2(_h2_ch,th2->_h2_ch,gshift2,opt);
  if( tnum_sumup > 0 )
    tnum_sumup += Project();
  return tnum_sumup;
};
//------------------------------------------------------------
TH1* TH1Xch::CreateHistCh(int ch){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  TH1* h1 = NULL;
  if( ch < _nch && ch >= 0 ){
    char hname[128];
    sprintf(hname,"%s_ch%d",GetName(),ch);
    h1 = (TH1*) _h2_ch->ProjectionX(hname,ch+1,ch+1);
  } else {
    cout<<" TH1Xch::CreateHistCh() Out of channel range!!"<<endl;
  }
  return h1;
};
//------------------------------------------------------------
int TH1Xch::Project(){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  int tnum_sumup = 0;
  TH1D* px = _h2_ch->ProjectionX("tmp_px");
  TAxis* xa = px->GetXaxis();
  int bin;
  for( bin = xa->GetFirst()-1 ; bin <= xa->GetLast()+1 ; bin++ ){
    tnum_sumup++;
    _h_all->SetBinContent(bin,px->GetBinContent(bin));
  }
  _h_all->SetEntries( _h2_ch->GetEntries() );
  px->Delete();
  return tnum_sumup;
};
//------------------------------------------------------------
//
//

