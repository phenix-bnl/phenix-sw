
#include <stdio.h>
#include <stream.h>
#include "TH0Xch.hh"

ClassImp(TH0Xch)
//------------------------------------------------------------
TH0Xch::TH0Xch(): CalibObj(){
  _f_all = 0;
  _h_all = NULL;
  _initialized = false;
};
//------------------------------------------------------------
TH0Xch::TH0Xch(const char* name, const char* title) : CalibObj(name,title){
  _f_all = 0;
  _h_all = NULL;
  _initialized = false;
};
//------------------------------------------------------------
TH0Xch::TH0Xch(const char* name, const char* title,int nch): CalibObj(name,title,nch){
  char hname[256],htitle[256];
  sprintf(hname,"%s_h_all",name);
  sprintf(htitle,"%s All hist",title);
  _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  _f_all = 0;
  _initialized = true;
};
//------------------------------------------------------------
bool TH0Xch::Initialize(const char* name, const char* title,int nch){
  if( _initialized )
    return false;
  char hname[256],htitle[256];
  SetName(name);
  SetTitle(title);
  _nch = nch;
  sprintf(hname,"%s_h_all",name);
  sprintf(htitle,"%s All hist",title);
  _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  _f_all = 0;
  _initialized = true;
  return true;
};
//------------------------------------------------------------
TH0Xch::TH0Xch(const TH0Xch& th0xch){
  char hname[128],htitle[128];
  sprintf(hname,"%s_cpy",th0xch.GetName());
  sprintf(htitle,"%s_cpy",th0xch.GetTitle());
  SetName(hname);
  SetTitle(htitle);
  _nch = th0xch._nch;
  //  cout<<" "<<GetName()<<" Start of copy constructor "<<endl;
  //  cout<<"              "<<th0xch.GetName()<<" -> "<<GetName()<<endl;
  sprintf(hname,"%s_h_all",GetName());
  sprintf(htitle,"%s_h_all",GetTitle());
  _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  _f_all = 0;
  int n = _nch + 1;
  while( n-- ){
    _h_all->SetBinContent(n, th0xch._h_all->GetBinContent(n) );
    _f_all += th0xch._h_all->GetBinContent(n);
  }
  _initialized = true;
  //  cout<<" "<<GetName()<<" End of copy constructor "<<endl;
};
//------------------------------------------------------------
TH0Xch::~TH0Xch(){
  if( _initialized ){
    //    cout<<" "<<GetName()<<" Destructor delete "<<_h_all->GetName()<<endl;
    delete _h_all;
  }
};
//------------------------------------------------------------
TH0Xch& TH0Xch::operator=(TH0Xch& th0xch){
  if(  _nch != th0xch.GetNch() ){
    //    cout<<" "<<GetName()<<" Error in operator= "<<endl;
    return *this;
  }
  _h_all->Reset();
  _f_all = 0;
  int n = _nch + 1;
  while( n-- ){
    _h_all->SetBinContent(n, th0xch._h_all->GetBinContent(n) );
    _f_all += th0xch._h_all->GetBinContent(n);
  }
  //  cout<<" "<<GetName()<<" End of operator = "<<endl;
  return *this;
};
//------------------------------------------------------------
TH0Xch TH0Xch::operator+(TH0Xch& input){
  if(  _nch != input.GetNch() ){
    //    cout<<" "<<GetName()<<" Error in operator+ "<<endl;
    return TH0Xch();
  }
  //  cout<<" "<<GetName()<<" Create temporaly object "<<endl;
  TH0Xch th0xch("th0xch","test",_nch);
  th0xch._f_all = 0;
  float content;
  int n = _nch + 1;
  while( n-- ){
    content = _h_all->GetBinContent(n) + input._h_all->GetBinContent(n);
    th0xch._h_all->SetBinContent(n,content);
    th0xch._f_all += content;
  }
  //  cout<<" "<<GetName()<<" End of operator +"<<endl;
  return th0xch;
};
//------------------------------------------------------------
TH0Xch TH0Xch::operator-(TH0Xch& input){
  if(  _nch != input.GetNch() ){
    cout<<" "<<GetName()<<" Error in operator- "<<endl;
    return TH0Xch();
  }
  TH0Xch th0xch("th0xch","test",_nch);
  th0xch._f_all = 0;
  float content;
  int n = _nch + 1;
  while( n-- ){
    content = _h_all->GetBinContent(n) - input._h_all->GetBinContent(n);
    th0xch._h_all->SetBinContent(n,content);
    th0xch._f_all += content;
  }
  return th0xch;
};
//------------------------------------------------------------
TH0Xch TH0Xch::operator*(TH0Xch& input){
  if(  _nch != input.GetNch() ){
    cout<<" "<<GetName()<<" Error in operator* "<<endl;
    return TH0Xch();
  }
  TH0Xch th0xch("th0xch","test",_nch);
  th0xch._f_all = 0;
  float content;
  int n = _nch + 1;
  while( n-- ){
    content = _h_all->GetBinContent(n) * input._h_all->GetBinContent(n);
    th0xch._h_all->SetBinContent(n,content);
    th0xch._f_all += content;
  }
  return th0xch;
};
//------------------------------------------------------------
TH0Xch TH0Xch::operator/(TH0Xch& input){
  if(  _nch != input.GetNch() ){
    cout<<" "<<GetName()<<" Error in operator/ "<<endl;
    return TH0Xch();
  }
  TH0Xch th0xch("th0xch","test",_nch);
  th0xch._f_all = 0;
  float content;
  int n = _nch + 1;
  while( n-- ){
    if( input._h_all->GetBinContent(n) > 0 )
      content = _h_all->GetBinContent(n) / input._h_all->GetBinContent(n);
    else
      content = 0;
    th0xch._h_all->SetBinContent(n,content);
    th0xch._f_all += content;
  }
  return th0xch;
};
//------------------------------------------------------------
int TH0Xch::Fill(int ch,float x,float w){
  int stat = 0;
  if( _initialized ){
    _h_all->Fill(ch,x);
    _f_all += x;
    stat = 1;
  }
  return stat;
};
//------------------------------------------------------------
void TH0Xch::Reset(char* option){
  if( _initialized ){
    _h_all->Reset(option);
    _f_all = 0;
  }
};
//------------------------------------------------------------
bool TH0Xch::Set(int ch,float x,float ex){
  if(! _initialized ){
    cout<<" "<<GetName()<<"::Set(ch,x,ex) Error..."<<endl;
    return false;
  }
  if( ch >= _nch ){
    cout<<" "<<GetName()<<"::Set(ch,x,ex) channel is out of range..."<<endl;
    return false;
  }
  int bin = _h_all->FindBin(ch);
  _f_all -= _h_all->GetBinContent(bin);
  _h_all->SetBinContent(bin,x);
  _h_all->SetBinError(bin,ex);
  _f_all += x;
  return true;
};
//------------------------------------------------------------
bool TH0Xch::Set(TGraph* gra){
  char hname[256],htitle[256];
  if( gra->GetN() == 0 )
    return false;
  int ch = gra->GetN();
  if( _initialized ){
    if( ch != _nch ){
      cout<<" "<<GetName()<<"::Set() Error..."<<endl;
      return false;
    } else
      _h_all->Reset();
  } else {
    sprintf(hname,"%s_h_all",GetName());
    sprintf(htitle,"%s All hist",GetTitle());
    _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  }
  _nch = ch;
  _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  _f_all = 0;
  _initialized = true;
  //
  double* y = gra->GetY();
  int n = _nch;
  while( n-- ){
    int bin = _h_all->FindBin(n);
    _h_all->SetBinContent(bin,y[n]);
    _f_all += y[n];
  }  
  return true;
};
//------------------------------------------------------------
bool TH0Xch::SetError(TGraph* org_gra){
  char hname[256],htitle[256];
  TGraphErrors* gra = dynamic_cast<TGraphErrors*>(org_gra);
  if( org_gra->GetN() == 0 )
    return false;
  if( gra == NULL ){
    cout<<" "<<GetName()<<"::SetError() Can't make cast from TGraph* to TGraphErrors* "<<endl;
    return false;
  }
  int ch = gra->GetN();
  if( _initialized ){
    if( ch != _nch ){
      cout<<" "<<GetName()<<"::Set() Error..."<<endl;
      return false;
    } else
      _h_all->Reset();
  } else {
    sprintf(hname,"%s_h_all",GetName());
    sprintf(htitle,"%s All hist",GetTitle());
    _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  }
  _nch = ch;
  _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  _f_all = 0;
  _initialized = true;
  //
  double* y = gra->GetEY();
  int n = _nch;
  while( n-- ){
    int bin = _h_all->FindBin(n);
    _h_all->SetBinContent(bin,y[n]);
    _f_all += y[n];
  }  
  return true;
};
//------------------------------------------------------------
bool TH0Xch::Set(TH1* h1){
  char hname[256],htitle[256];
  int ch = h1->GetXaxis()->GetLast();
  if(  ch == 0 )
    return false;
  if( _initialized ){
    if( ch != _nch ){
      cout<<" "<<GetName()<<"::Set() Error..."<<endl;
      return false;
    } else
      _h_all->Reset();
  } else {
    sprintf(hname,"%s_h_all",GetName());
    sprintf(htitle,"%s All hist",GetTitle());
    _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  }
  _nch = ch;
  _f_all = 0;
  _initialized = true;
  //
  int n = _nch;
  while( n-- ){
    int bin = h1->FindBin(n);
    _h_all->SetBinContent(bin,h1->GetBinContent(bin));
    _f_all += h1->GetBinContent(bin);
  }  
  return true;
};
//------------------------------------------------------------
bool TH0Xch::SetError(TH1* h1){
  char hname[256],htitle[256];
  int ch = h1->GetXaxis()->GetLast();
  if(  ch == 0 )
    return false;
  if( _initialized ){
    if( ch != _nch ){
      cout<<" "<<GetName()<<"::Set() Error..."<<endl;
      return false;
    } else
      _h_all->Reset();
  } else {
    sprintf(hname,"%s_h_all",GetName());
    sprintf(htitle,"%s All hist",GetTitle());
    _h_all = new TH1F(hname,htitle,_nch,0,_nch);
  }
  _nch = ch;
  _f_all = 0;
  _initialized = true;
  //
  int n = _nch;
  while( n-- ){
    int bin = h1->FindBin(n);
    _h_all->SetBinContent(bin,h1->GetBinError(bin));
    _f_all += h1->GetBinError(bin);
  }  
  return true;
};
//------------------------------------------------------------
int TH0Xch::Add(CalibObj* obj,Double_t c1 , char* opt ){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return(0);
  }
  TH0Xch* th0 = dynamic_cast<TH0Xch*>(obj);
  if(! th0 )
    return 0;
  int tnum_sumup = 0;
  if( _nch == th0->GetNch() ){
    tnum_sumup ++;
    _h_all->Add(th0->_h_all,c1);
    _f_all += th0->_f_all;
  }
  return tnum_sumup;
}
//------------------------------------------------------------
void TH0Xch::Scale(Double_t c1 ){
  if(! _initialized ){
    cout<<" "<<GetName()<<" is not initialized."<<endl;
    return;
  }
  _h_all->Scale(c1);
  _f_all *= c1;
  return;
}

//------------------------------------------------------------
