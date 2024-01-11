
#include <stdio.h>
#include <stream.h>
#include "TH2Xch.hh"
#include "SumupTH.hh"

ClassImp(TH2Xch)

//------------------------------------------------------------
TH2Xch::TH2Xch(): CalibObj(){
  int n = 2;
  while(n-- ){
    _nbins[n] = 0;
    _low[n] = 0;
    _up[n] = 0;
    _width[n] = 0;
  }
  _h_all = NULL;
  _h3_ch = NULL;
};
//------------------------------------------------------------
TH2Xch::TH2Xch(const char* name, const char* title) : CalibObj(name,title){
  int n = 2;
  while(n-- ){
    _nbins[n] = 0;
    _low[n] = 0;
    _up[n] = 0;
    _width[n] = 0;
  }
  _h_all = NULL;
  _h3_ch = NULL;
};
//------------------------------------------------------------
TH2Xch::TH2Xch(const char* name, const char* title,
	       int nch,int xnbins,float xlow,float xup,int ynbins,float ylow,float yup) : CalibObj(name,title,nch){
  if( xup > xlow && yup > ylow ){
    _nbins[0] = xnbins;
    _low[0] = xlow;
    _up[0] = xup;
    _width[0] = (xup - xlow)/((float)xnbins);
    _nbins[1] = ynbins;
    _low[1] = ylow;
    _up[1] = yup;
    _width[1] = (yup - ylow)/((float)ynbins);
    //
    char hname[256],htitle[256];
    sprintf(hname,"%s_h_all",name);
    sprintf(htitle,"%s All hist",title);
    _h_all = new TH2F(hname,htitle,xnbins,xlow,xup,ynbins,ylow,yup);
    sprintf(hname,"%s_h3_ch",name);
    sprintf(htitle,"%s Channel",title);
    _h3_ch = new TH3F(hname,htitle,xnbins,xlow,xup,ynbins,ylow,yup,nch,0,nch);
  } else{
    cout<<" "<<GetName()<<" Can't create TH2Xch("<<nch<<","<<xnbins<<","<<xlow<<","<<xup<<","<<ynbins<<","<<ylow<<","<<yup<<") "<<endl;
    _nch = 0;
    int n = 2;
    while(n-- ){
      _low[n] = 0;
      _up[n] = 0;
      _width[n] = 0;
    }
    _h_all = NULL;
    _h3_ch = NULL;
  }
};
//------------------------------------------------------------
TH2Xch::~TH2Xch(){
  delete _h3_ch;
  delete _h_all;
};
//------------------------------------------------------------
int TH2Xch::Fill(int ch,float x,float y,float w=1.){
  _h_all->Fill(x,y,w);
  return _h3_ch->Fill(x,y,ch,w);
};
//------------------------------------------------------------
void TH2Xch::Reset(char* option){
  _h3_ch->Reset(option);
  _h_all->Reset(option);
};
//------------------------------------------------------------
TH2* TH2Xch::CreateHistCh(int ch){
  TH2* h2 = NULL;
  if( ch < _nch && ch >= 0 ){
    char hname[128],htitle[128];
    sprintf(hname,"%s_ch%d",GetName(),ch);
    sprintf(htitle,"%s_ch%d",GetName(),ch);
    h2 = new TH2F(hname,htitle,_nbins[0],_low[0],_up[0],_nbins[1],_low[1],_up[1]);
    //cout<<" "<<GetName()<<":: CreateHistCh("<<ch<<") _nbins = ("<<_nbins[0]<<","<<_nbins[1]<<endl;
    int xbin = _nbins[0] + 1;
    while( xbin-- ){
      int ybin = _nbins[1] + 1;
      while( ybin-- ){
	h2->SetBinContent(xbin,ybin,_h3_ch->GetBinContent(xbin,ybin,ch+1) );
      }
    }
  } else {
    cout<<" TH2Xch::CreateHistCh() Out of channel range!!"<<endl;
  }
  return h2;
};
//------------------------------------------------------------
bool TH2Xch::SetHistCh(int ch,TH2* h2){
  if( h2->GetXaxis()->GetNbins() != _nbins[0] ||
      h2->GetYaxis()->GetNbins() != _nbins[1] ){
    cout<<" "<<GetName()<<"::SetHistCh() has no same binning "<<endl;
    return false;
  }
  if( ch >= _nch || ch < 0 ){
    cout<<" "<<GetName()<<"::CreateHistCh() Out of channel range!!"<<endl;
    return false;
  }
  int bin;
  int xbin = _nbins[0] + 1;
  while( xbin-- ){
    int ybin = _nbins[1] + 1;
    while( ybin-- ){
      bin = _h3_ch->GetBin(xbin,ybin,ch+1);
      _h3_ch->SetBinContent(bin,h2->GetBinContent(xbin,ybin) );
    }
  }
  Project();

  return true;
};
//------------------------------------------------------------
bool TH2Xch::AddHistCh(int ch,TH2* h2){
  if( h2->GetXaxis()->GetNbins() != _nbins[0] ||
      h2->GetYaxis()->GetNbins() != _nbins[1] ){
    cout<<" "<<GetName()<<"::SetHistCh() has no same binning "<<endl;
    return false;
  }
  if( ch >= _nch || ch < 0 ){
    cout<<" "<<GetName()<<"::CreateHistCh() Out of channel range!!"<<endl;
    return false;
  }
  float cont;
  int bin;
  int xbin = _nbins[0] + 1;
  while( xbin-- ){
    int ybin = _nbins[1] + 1;
    while( ybin-- ){
      bin = _h3_ch->GetBin(xbin,ybin,ch+1);
      cont = _h3_ch->GetBinContent(bin);
      cont += h2->GetBinContent(xbin,ybin);
      _h3_ch->SetBinContent(bin,cont);
    }
  }
  Project();

  return true;
};
//------------------------------------------------------------
int TH2Xch::Project(){
  TH1* th1 = _h3_ch->Project3D("yx");
  TH2* th2 = dynamic_cast<TH2*>(th1);
  if( th2 == NULL ){
    cout<<" "<<GetName()<<"::Project() : Error!! "<<endl;
    return 0;
  }
  _h_all->Reset();
  _h_all->Add(th2);
  delete th1;
  return 1;
};
//------------------------------------------------------------
int TH2Xch::SumupY(TH2Xch* th2,TH1* hshift1,char* opt){
  int tnum_sumup = 0;
  tnum_sumup += SumupY_TH3(_h3_ch,th2->_h3_ch,hshift1,opt);
  if( tnum_sumup > 0 )
    tnum_sumup += Project();
  return tnum_sumup;
};
//------------------------------------------------------------
int TH2Xch::SumupY(TH2Xch* th2,TGraph* gshift1,char* opt){
  int tnum_sumup = 0;
  tnum_sumup += SumupY_TH3(_h3_ch,th2->_h3_ch,gshift1,opt);
  if( tnum_sumup > 0 )
    tnum_sumup += Project();
  return tnum_sumup;
};
//------------------------------------------------------------
void TH2Xch::Scale(Double_t c1 ){
  //if(! _initialized ){
  if(! _h3_ch ){   // FIX.ME
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return;
  }
  _h3_ch->Scale(c1);
  _h_all->Scale(c1);
  return;
}
//------------------------------------------------------------
int TH2Xch::Add(CalibObj* obj,Double_t c1=1, char* opt="" ){
  int tnum_sumup = 0;
  TH2Xch* th2 = dynamic_cast<TH2Xch*>(obj);
  if( th2 == NULL ){
    cout<<" "<<GetName()<<"::Add() Error!!! "<<endl;
    return(0);
  }
  if( _nch == th2->GetNch() &&
      GetWidthX() == th2->GetWidthX() &&
      GetWidthY() == th2->GetWidthY() ){
    tnum_sumup ++;
    _h3_ch->Add(th2->_h3_ch,c1);
    _h_all->Add(th2->_h_all,c1);
  }
  return tnum_sumup;
}
//------------------------------------------------------------

//====================================================================================
//====================================================================================
//====================================================================================
//====================================================================================
#ifdef SKIPSKIP
//------------------------------------------------------------
int TH2Xch::Sumup(TH2Xch* th1,Double_t shift = 0,char* opt){
  int tnum_sumup = 0;
  TCanvas* ctmp ;
  if( strstr(opt,"D") ){
    ctmp =new TCanvas("ctmp","TH2Xch Sumup"); //,1200,1000);
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
int TH2Xch::Sumup(TH2Xch* th1,TH2Xch* th2,Double_t shift1,Double_t shift2,char* opt){
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
int TH2Xch::Sumup(TH2Xch* th1,TH1* hshift1,char* opt){
  int tnum_sumup = 0;
  //tnum_sumup += SumupTH2(_h2_ch,_h2_ch,th1->_h2_ch,NULL,hshift1,opt);
  tnum_sumup += SumupTH2(_h2_ch,th1->_h2_ch,hshift1,opt);
  if( tnum_sumup > 0 )
    tnum_sumup += Project();
  return tnum_sumup;
};
//------------------------------------------------------------
int TH2Xch::Sumup(TH2Xch* th1,TH2Xch* th2,TH1* hshift1,TH1* hshift2,char* opt){
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
#endif // SKIPSKIP

//====================================================================================
//====================================================================================
//====================================================================================
//====================================================================================
