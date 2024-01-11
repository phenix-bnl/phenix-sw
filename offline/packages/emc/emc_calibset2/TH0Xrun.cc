
#include <stdio.h>
#include <stream.h>
#include "TH0Xrun.hh"

ClassImp(TH0Xrun)
//------------------------------------------------------------
TH0Xrun::TH0Xrun(): TNamedDir(){
  _f_all = 0;
};
//------------------------------------------------------------
TH0Xrun::TH0Xrun(const char* name, const char* title) : TNamedDir(name,title){
  char hname[256],htitle[256];
  sprintf(hname,"%s_gra",name);
  sprintf(htitle,"%s TGraphErrors",title);
  _gra.SetName(hname);
  _gra.SetTitle(htitle);
  _gra.Set(0);
  _f_all = 0;
  return;
};
//------------------------------------------------------------
bool TH0Xrun::Initialize(const char* name, const char* title){
  SetName(name);
  SetTitle(title);
  char hname[256],htitle[256];
  sprintf(hname,"%s_gra",name);
  sprintf(htitle,"%s TGraphErrors",title);
  _gra.SetName(hname);
  _gra.SetTitle(htitle);
  _gra.Set(0);
  _f_all = 0;
  return true;
};
//------------------------------------------------------------
TH0Xrun::~TH0Xrun(){
};
//------------------------------------------------------------
TH0Xrun& TH0Xrun::operator=(TH0Xrun& th0xrun){
  _f_all = th0xrun._f_all;
  int n = th0xrun._gra.GetN();
  double* px = th0xrun._gra.GetX();
  double* py = th0xrun._gra.GetY();
  double* ey = th0xrun._gra.GetEY();
  _gra.Set(n);
  while( n-- ){
    _gra.SetPoint(n,px[n],py[n]);
    _gra.SetPointError(n,0,ey[n]);
  }
  return *this;
};
//------------------------------------------------------------
bool TH0Xrun::Set(TGraph* gra,char* opt){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG optionx  
  int n = gra->GetN();
  double* px = gra->GetX();
  double* py = gra->GetY();
  double* ey;
  TGraphErrors* gra_err = dynamic_cast<TGraphErrors*>(gra);
  if( gra_err )
    ey = gra_err->GetEY();
  else
    if( opt_d ) cout<<" "<<GetName()<<"::Set() can't retrieve the error value from TGraphErrors* "<<endl;

  int run;
  bool status = true;
  while( n-- ){
    run = (int) px[n];
    if( gra_err ){
      if(! Set(run,py[n],ey[n],opt) ) status = false;
      //if( opt_d ) cout<<" "<<GetName()<<"::Set() ("<<run<<","<<py[n]<<","<<ey[n]<<","<<opt<<")"<<endl;
    } else {
      if(! Set(run,py[n],0,opt) ) status = false;
    }
  }
  return status;
}
//------------------------------------------------------------
bool TH0Xrun::SetError(TGraph* gra,char* opt){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG optionx  
  int n = gra->GetN();
  double* px = gra->GetX();
  double* ey;
  TGraphErrors* gra_err = dynamic_cast<TGraphErrors*>(gra);
  if( gra_err )
    ey = gra_err->GetEY();
  else {
    if( opt_d ) cout<<" "<<GetName()<<"::SetError() can't retrieve the error value from TGraphErrors* "<<endl;
    return false;
  }

  ey = gra_err->GetEY();
  int run;
  bool status = true;
  while( n-- ){
    run = (int) px[n];
    if(! Set(run,ey[n],0,opt) ) status = false;
    //if( opt_d ) cout<<" "<<GetName()<<"::SetError() ("<<run<<","<<ey[n]<<","<<0<<","<<opt<<")"<<endl;
  }
  return status;
}
//------------------------------------------------------------
bool TH0Xrun::Set(int run,float value,float err,char* opt){
  char* opt_add;
  opt_add = strstr(opt,"a"); // add option
  char* opt_quadratic;
  opt_quadratic = strstr(opt,"*"); // quadratic option
  //
  double* px = _gra.GetX();
  double* py = _gra.GetY();
  double* ey = _gra.GetEY();
  int n;
  int found = 0;
  int found_n = 0;
  n =_gra.GetN();
  while( n-- ){
    if( run == (int) px[n] ){
      found++;
      found_n = n;
    }
  }
  if( found == 0 ){
    n = _gra.GetN();
    _gra.Set(n+1);
    _gra.SetPoint(n,run,value);
    _gra.SetPointError(n,0,err);
    _f_all += value;
  } else if ( found == 1 ){
    n = found_n;
    if( opt_add ){
      value += py[n];
      err = sqrt(err*err+ey[n]*ey[n]);
    } else if ( opt_quadratic ){
      if( value!=0 && err!=0 && py[n]!=0 && ey[n]!=0 ){
	double tmp = ( 1./err/err + 1./ey[n]/ey[n] );
	double tmp_value = ( value/err/err + py[n]/ey[n]/ey[n] )/tmp;
	double tmp_err = ( 1./value/value + 1./py[n]/py[n] )/tmp;
	tmp_err = sqrt(tmp_err) * fabs(tmp_value);
	value = tmp_value;
	err = tmp_err;
      } else {
	value += py[n];
	err = sqrt(err*err+ey[n]*ey[n]);
      }
    }
    _gra.SetPoint(n,run,value);
    _gra.SetPointError(n,0,err);
    _f_all += value;
  } else {
    cout<<" "<<GetName()<<"::Set(): Two entries for one run..."<<endl;
    return false;
  }
  return true;
};
//------------------------------------------------------------
bool TH0Xrun::Exist(int run){
  double* px = _gra.GetX();
  int n;
  int found = 0;
  bool status = false;
  n =_gra.GetN();
  if( n == 0 )
    return false;
  while( n-- ){
    if( run == (int) px[n] ){
      found++;
    }
  }
  if ( found == 1 )
    status = true;
  return status;
};
//------------------------------------------------------------
float TH0Xrun::Get(int run){
  double* px = _gra.GetX();
  double* py = _gra.GetY();
  int n;
  int found = 0;
  int found_n = 0;
  n =_gra.GetN();
  if( n == 0 )
    return 0;
  while( n-- ){
    if( run == (int) px[n] ){
      found++;
      found_n = n;
    }
  }
  float value;
  if( found == 0 ){
    value = 0;
  } else if ( found == 1 ){
    n = found_n;
    value = py[n];
  } else {
    cout<<" "<<GetName()<<"::Set(): Two entries for one run..."<<endl;
    value = 0;
  }
  return value;
};
//------------------------------------------------------------
bool TH0Xrun::Get(int run,float& run_shift,float& run_shift_err,char* opt=""){
  double* px = _gra.GetX();
  double* py = _gra.GetY();
  double* ey = _gra.GetEY();
  int n;
  int found = 0;
  int found_n = 0;
  bool status = false;
  n =_gra.GetN();
  if( n == 0 )
    return false;
  while( n-- ){
    if( run == (int) px[n] ){
      found++;
      found_n = n;
    }
  }
  run_shift = 0;
  run_shift_err = 0;
  if( found == 0 ){
    status = false;
  } else if ( found == 1 ){
    n = found_n;
    run_shift = py[n];
    run_shift_err = ey[n];
    status = true;
  } else {
    cout<<" "<<GetName()<<"::Set(): Two entries for one run..."<<endl;
    status = false;
  }
  return status;

};
//------------------------------------------------------------
bool TH0Xrun::FindNear(int run,float& run_shift,float& run_shift_err,int limit=100,char* opt=""){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  //
  double* px = _gra.GetX();
  double* py = _gra.GetY();
  double* ey = _gra.GetEY();
  bool status;
  int diff_run = 500;
  int min_num = 0;
  int num = _gra.GetN();
  if( num == 0 )
    return false;
  while( num-- ){
    if( diff_run > abs( run - (int)px[num]) ){
      diff_run = abs( run - (int)px[num]);
      min_num = num;
    }
  }
  if( diff_run < limit ){
    if( opt_d ) cout<<" "<<GetName()<<"::FindNear()  ... Founded nearest run # "<<px[min_num]<<" <-> "<<run<<endl;
    run_shift = py[min_num];
    run_shift_err = ey[min_num];
    status = true;
  } else {
    run_shift = 0;
    run_shift_err = 0;
    status = false;
  }
  if( opt_d ) cout<<" "<<GetName()<<"::FindNear() run_shift = "<<run_shift<<" += "<<run_shift_err<<endl;
  return status;
};
//------------------------------------------------------------
float TH0Xrun::GetError(int run){
  double* px = _gra.GetX();
  double* py = _gra.GetEY();
  int n;
  int found = 0;
  int found_n = 0;
  n =_gra.GetN();
  if( n == 0 )
    return 0;
  while( n-- ){
    if( run == (int) px[n] ){
      found++;
      found_n = n;
    }
  }
  float value;
  if( found == 0 ){
    value = 0;
  } else if ( found == 1 ){
    n = found_n;
    value = py[n];
  } else {
    cout<<" "<<GetName()<<"::Set(): Two entries for one run..."<<endl;
    value = 0;
  }
  return value;
};
//------------------------------------------------------------
void TH0Xrun::Reset(char* opt){
  _gra.Set(0);
  _f_all = 0;
};
//------------------------------------------------------------
