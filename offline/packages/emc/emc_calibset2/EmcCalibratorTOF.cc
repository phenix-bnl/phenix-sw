#include <stdio.h>
#include <stream.h>
#include <TCanvas.h>
#include "EmcCalibratorTOF.hh"

ClassImp(EmcCalibratorTOF)

//------------------------------------------------------------
EmcCalibratorTOF::EmcCalibratorTOF(): EmcCalibrator(){
  _fit_sigma[0] = FITPEAK_DOWNSIGMA;
  _fit_sigma[1] = FITPEAK_UPSIGMA;
  _initialized = false;
  _h_twr_pshift = NULL;
  _h_twr_shift = NULL;
  int i = 4;
  while( i-- )
    _h_twr_fit[i] = NULL;
  _th1xch = NULL;
  _th1xch_afttwr = NULL;
  return;
}
//------------------------------------------------------------
EmcCalibratorTOF::EmcCalibratorTOF(const char* name, const char* title) : EmcCalibrator(name,title){
  _fit_sigma[0] = FITPEAK_DOWNSIGMA;
  _fit_sigma[1] = FITPEAK_UPSIGMA;
  _initialized = false;
  _h_twr_pshift = NULL;
  _h_twr_shift = NULL;
  int i = 4;
  while( i-- )
    _h_twr_fit[i] = NULL;
  _th1xch = NULL;
  _th1xch_afttwr = NULL;
  return;
};
//------------------------------------------------------------
EmcCalibratorTOF::EmcCalibratorTOF(const char* name, const char* title,int nch,int nbins,Axis_t low,Axis_t up) : EmcCalibrator(name,title,nch){
  Initialize(nch,nbins,low,up);
  return;
};
//------------------------------------------------------------
EmcCalibratorTOF::~EmcCalibratorTOF(){
  int i = 4;
  while( i-- )
    if( _h_twr_fit[i] ) delete _h_twr_fit[i];
  if( _h_twr_pshift ) delete _h_twr_pshift;
  if( _h_twr_shift ) delete _h_twr_shift;
  if( _th1xch ) delete _th1xch;
  if( _th1xch_afttwr ) delete _th1xch_afttwr;
  return;
};
//------------------------------------------------------------
int EmcCalibratorTOF::SetPreshift(){
  cout<<" "<<GetName()<<"::SetPreshift() has reset itself with t + qa option. "<<endl;
  this->Reset("t+q");
  _h_twr_pshift->Reset();
  _h_twr_pshift->Add( _h_twr_shift );
  return 1;
};
//------------------------------------------------------------
int EmcCalibratorTOF::SetPreshift(EmcCalibratorTOF& calibtof){
  cout<<" "<<GetName()<<"::SetPreshift() has reset itself with t + qa option. "<<endl;
  this->Reset("t+q");
  _h_twr_pshift->Reset();
  _h_twr_pshift->Add(calibtof._h_twr_shift );
  return 1;
};
//------------------------------------------------------------
int EmcCalibratorTOF::SetEmcCalibratorTOF(EmcCalibratorTOF& calibtof){
  if(! _initialized ){
    cout<<" "<<GetName()<<"::SetEmcCalibratorTOF() Error: EmcCalibratorTOF is not initialized "<<endl;
    return 0;
  }
  if( GetNch() != calibtof.GetNch() ){
    cout<<" "<<GetName()<<"::SetEmcCalibratorTOF() Error: channel # is different. "<<endl;
    return 0;
  }
  int n;
  double *x,*y,*ex,*ey;
  //
 _th0xrun_shift = calibtof._th0xrun_shift;
  int nfit = 4;
  while( nfit-- )
    _th0xrun_fit[nfit] = calibtof._th0xrun_fit[nfit];
  //
  _h_twr_shift->Reset();
  _h_twr_shift->Add(calibtof._h_twr_pshift );
  _h_twr_shift->Add(calibtof._h_twr_shift );
  _h_twr_pshift->Reset();
  _h_twr_pshift->Add(calibtof._h_twr_pshift );
  _h_twr_pshift->Add(calibtof._h_twr_shift );
  // Now, shift = p(re)shift = calibtof.p(re)shift + calibtof.shift...
  n = 4;
  while( n-- ){
    _h_twr_fit[n]->Reset();
    _h_twr_fit[n]->Add(calibtof._h_twr_fit[n] );
  }
  return 1;
};
//------------------------------------------------------------
int EmcCalibratorTOF::SetEmcCalibratorTOFrun(EmcCalibratorTOF& calibtof){
  if(! _initialized ){
    cout<<" "<<GetName()<<"::SetEmcCalibratorTOF() Error: EmcCalibratorTOF is not initialized "<<endl;
    return 0;
  }
  _th0xrun_shift = calibtof._th0xrun_shift;
  int nfit = 4;
  while( nfit-- )
    _th0xrun_fit[nfit] = calibtof._th0xrun_fit[nfit];
  //
  return 1;
};
//------------------------------------------------------------
void EmcCalibratorTOF::Initialize(int nch,int nbins,float low,float up) {
  cout<<" "<<GetName()<<"::EmcCalibratorTOF::Initialize()... "<<endl;
  _initialized = true;
  char hname[128],htitle[128];
  //
  _nbins = nbins;
  _low = low;
  _up = up;
  _fit_sigma[0] = FITPEAK_DOWNSIGMA;
  _fit_sigma[1] = FITPEAK_UPSIGMA;
  //
  sprintf(hname,"%s_twrshift",GetName());
  sprintf(htitle,"%s twr shift",GetTitle());
  _h_twr_shift = new TH1F(hname,htitle,nch,0,nch);
  //
  sprintf(hname,"%s_twrpshift",GetName());
  sprintf(htitle,"%s twr pre-shift",GetTitle());
  _h_twr_pshift = new TH1F(hname,htitle,nch,0,nch);
  //
  sprintf(hname,"%s_th0xrun_shift",GetName());
  sprintf(htitle,"%s TH0Xrun runshift",GetTitle());
  _th0xrun_shift.Initialize(hname,htitle);
  //
  int i = 4;
  while( i-- ){
    sprintf(hname,"%s_twrfit_%d",GetName(),i);
    sprintf(htitle,"%s twr fit %d",GetTitle(),i);
    _h_twr_fit[i] = new TH1F(hname,htitle,nch,0,nch);
    //
    sprintf(hname,"%s_th0xrun_fit%d",GetName(),i);
    sprintf(htitle,"%s TH0Xrun runshift fit %d",GetTitle(),i);
    _th0xrun_fit[i].Initialize(hname,htitle);
  }
  //
  sprintf(hname,"%s_th1xch",GetName());
  sprintf(htitle,"%s th1xch",GetTitle());
  _th1xch = new TH1Xch(hname,htitle,nch,nbins,low,up);
  //
  sprintf(hname,"%s_th1xch_afttwr",GetName());
  sprintf(htitle,"%s th1xch after twr-by-twr",GetTitle());
  _th1xch_afttwr = new TH1Xch(hname,htitle,nch,nbins,low,up);

  return;
};
//------------------------------------------------------------
int EmcCalibratorTOF::Add(EmcCalibratorTOF* emctof){
  cout<<" "<<GetName()<<"::Add() is not available... "<<endl;
  /*
  if( _h_twr_shift == NULL ){
    double* run = emctof->_gra_run_shift->GetX();
    int num = emctof->_gra_run_shift->GetN();
    while( num-- ){
      double* thisrun = this->_gra_run_shift->GetX();
      int thisnum = this->_gra_run_shift->GetN();
      while(  thisnum-- ){
	if( thisrun[thisnum] == run[num] ){
	  // This is under developping.......
	  // Sorry...
	}
      }
    }
  }
  */
  return 0;
};
//------------------------------------------------------------
int EmcCalibratorTOF::AnalyzeCalibObj(CalibObj* calibobj,char* opt){
  if(! _initialized ){
    cout<<" Error: "<<GetName()<<"::AnalyzeCalibObj() is not initialized ."<<endl;
    return 0;
  }
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_0;
  opt_0 = strstr(opt,"0");  // Using current run-by-run T0
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // Using QA info
  char* opt_t;
  opt_t = strstr(opt,"t"); // Using p(re)shift before the run-by-run...
  float run_shift,run_shift_err;
  bool found = false;
  int found_bin = -1;
  //
  TH1Xch* th1xch = dynamic_cast<TH1Xch*>(calibobj);
  if( th1xch == NULL ){
    cout<<" Error: "<<GetName()<<"::AnalyzeCalibObj() get no TH1Xch* object. "<<endl;
    return 0;
  }
  cout<<" "<<GetName()<<"::AnalyzeCalibObj() Processing. (run,seq) = ("<<_cur_run<<","<<_cur_seq<<") : "<<th1xch->GetName()<<endl;
  //============================================================
  if( opt_0 ){
    cout<<" "<<GetName()<<"::AnalyzeCalibObj() Option 0 :: Using current setup... "<<endl;
    found = _th0xrun_shift.FindNear(_cur_run,run_shift,run_shift_err,1000);
    if( found ) cout<<" "<<GetName()<<"::AnalyzeCalibObj() run_shift = "<<run_shift<<" += "<<run_shift_err<<endl;
  //============================================================
  } else { // ............................... !opt_0
    found = _th0xrun_shift.Get(_cur_run,run_shift,run_shift_err);
    if( found ) cout<<" "<<GetName()<<"::AnalyzeCalibObj() run_shift = "<<run_shift<<" += "<<run_shift_err<<endl;
    //
    TH1* h1;
    if( opt_t ){
      _th1xch_afttwr->Reset();
      _th1xch_afttwr->Sumup(th1xch,_h_twr_pshift);
      h1 = _th1xch_afttwr->_h_all;
    } else {
      h1 = th1xch->_h_all;
    }
    //---------------------------- Fitting....
    TAxis* x = h1->GetXaxis();
    TF1* fit;
    char option[128];
    if( opt_d ) {
      TCanvas* c1 = (TCanvas*)gROOT->FindObject("c1");
      if( c1 == NULL )
	c1 = new TCanvas("c1",GetName());
      sprintf(option,"%s",opt);
      fit = fitpeak(h1,x->GetXmin(),x->GetXmax(),_fit_sigma[0],"tof",option,_fit_sigma[1]);
      c1->Update();
    } else {
      sprintf(option,"Q0%s",opt);
      fit = fitpeak(h1,x->GetXmin(),x->GetXmax(),_fit_sigma[0],"tof",option,_fit_sigma[1]);
    }
    double this_shift = -1. * fit->GetParameter(1);
    double this_shift_err = fabs( fit->GetParError(1) );
    int bin,nfit;
    if( found ){
      cout<<" "<<GetName()<<"::AnalyzeTwr():: Data with same run number are found!!!"<<endl;
      double tmp = ( 1./run_shift_err/run_shift_err + 1./this_shift_err/this_shift_err );
      double tmp_run_shift = ( run_shift/run_shift_err/run_shift_err +
			       this_shift/this_shift_err/this_shift_err )/tmp;
      double tmp_run_shift_err = ( 1./run_shift/run_shift + 1./this_shift/this_shift )/tmp;
      tmp_run_shift_err = sqrt(tmp_run_shift_err) * fabs(tmp_run_shift);
      cout<<"            run shift "<<run_shift<<" --> "<<tmp_run_shift<<endl;
      cout<<"                  err "<<run_shift_err<<" --> "<<tmp_run_shift_err<<endl;
      run_shift = tmp_run_shift;
      run_shift_err = tmp_run_shift_err;
    } else {
      found_bin = bin;
      run_shift = this_shift;
      run_shift_err = this_shift_err;
    }
    _th0xrun_shift.Set(_cur_run,run_shift,run_shift_err,"");
    nfit = 3;
    _th0xrun_fit[nfit].Set(_cur_run,fit->GetChisquare(),fit->GetNDF(),"");
    while( nfit-- )
      _th0xrun_fit[nfit].Set(_cur_run,fit->GetParameter(nfit),fit->GetParError(nfit),"");
    delete fit;
  }  // End of opt_0
  //============================================================
  // Sumup th1xch into sumth1....
  TGraph* gshift = new TGraph(_nch);
  int n = _nch;
  if( opt_t ){
    while( n-- ){
      int bin = _h_twr_pshift->FindBin(n);
      float pre_shift = _h_twr_pshift->GetBinContent(bin);
      gshift->SetPoint(n,n,run_shift+pre_shift);
    }
  } else {
    while( n-- )
      gshift->SetPoint(n,n,run_shift);
  }
  if( opt_qa ){  //&& GetNQAtwr() == _nch 
    n = _nch;
    int stat = 0;
    while( n-- )
      if( GetQAtwr(n) > 0 ){
	gshift->SetPoint(n,n,-10000);
	stat++;
      }
    if( opt_d )cout<<" "<<GetName()<<"::AnalyzeCalibObj() QA selection applied for "<<stat<<" / "<<_nch<<endl;
  };

  //---- Main part ----------------------------------------------------------------------
  cout<<" "<<GetName()<<"::AnalyzeCalibObj() calling TH1Xch::Sumup()... "<<endl;
  if( opt_t || opt_qa )
    _th1xch->Sumup(th1xch,gshift,opt);
  else
    _th1xch->Sumup(th1xch,run_shift,opt);
  //---- Main part ----------------------------------------------------------------------
  delete gshift;

  if( opt_d ){
    cout<<" "<<GetName()<<"::AnalyzeTwr() run_shift = "<<run_shift<<" +- "<<run_shift_err<<endl;
    //cout<<" "<<GetName()<<"::AnalyzeTwr:: --------------- graph --------------- "<<endl;
    //_th0xrun_shift._gra.Print();
    //cout<<" "<<GetName()<<"::AnalyzeTwr:: ------------------------------------- "<<endl;
  }
  //
  return(1);
}
//------------------------------------------------------------
int EmcCalibratorTOF::AnalyzeTwr(char* opt){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_0;
  opt_0 = strstr(opt,"0");  // Using current run-by-run T0
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // Using QA info
  //
  int ch,nfit;
  int canvas_num = 0;
  int canvas_rep = 9;
  TCanvas* c_anatwr;
  if( opt_d ){
    c_anatwr = new TCanvas("c_anatwr","Twr-by-twr T0 analysis",900,900);
    c_anatwr->Clear();
    c_anatwr->Divide(3,3);
  }
  _th1xch_afttwr->Reset();
  char option[128];
  //
  //========================================================================
  if( opt_0 ){
    cout<<" "<<GetName()<<"::AnalyzeTwr() Option 0 :: Using current setup... "<<endl;
  //========================================================================
  } else {
    cout<<" "<<GetName()<<"::AnalyzeTwr() Analyzing twr-by-twr T0 ...."<<endl;
    TAxis* x = _th1xch->GetHist()->GetXaxis();
    ch = _th1xch->GetNch();
    cout<<" "<<GetName()<<"::AnalyzeTwr() (ch:"<<ch<<"/"<<_th1xch->GetNch()<<"):: "<<flush;
    while( ch-- ){
      if( ch % 100 == 0 ) cout<<endl<<" "<<GetName()<<"::AnalyzeTwr() (ch:"<<ch<<"/"<<_th1xch->GetNch()<<"):: "<<flush;
      if( ch % 5 == 0 ) cout<<"."<<flush;
      TH1* h1 = _th1xch->CreateHistCh(ch);
      TF1* fit;
      if( opt_d ) {
	c_anatwr->cd(++canvas_num);
	sprintf(option,"%s",opt);
	fit = fitpeak(h1,x->GetXmin(),x->GetXmax(),_fit_sigma[0],"tof",option,_fit_sigma[1]);
	fit->SetLineColor(2);
	fit->SetLineWidth(2);
	h1->DrawCopy();
	if( canvas_num % canvas_rep == 0 ){
	  c_anatwr->cd();
	  c_anatwr->Update();
	  canvas_num = 0;
	}
      } else {
	sprintf(option,"Q0%s",opt);
	fit = fitpeak(h1,x->GetXmin(),x->GetXmax(),_fit_sigma[0],"tof",option,_fit_sigma[1]);
      }
      int bin = _h_twr_shift->FindBin(ch);
      int i = 3;
      _h_twr_fit[i]->SetBinContent(bin,fit->GetChisquare() );
      _h_twr_fit[i]->SetBinError(bin,fit->GetNDF() );
      while( i-- ){
	_h_twr_fit[i]->SetBinContent(bin,fit->GetParameter(i));
	_h_twr_fit[i]->SetBinError(bin,fabs(fit->GetParError(i)));
      }
      if( fit->GetParameter(1) < 10000 && fit->GetParameter(1) > -10000 ){
	_h_twr_shift->SetBinContent(bin,-1.*fit->GetParameter(1));
	_h_twr_shift->SetBinError(bin,fabs(fit->GetParError(1)));
      } else {
	_h_twr_shift->SetBinContent(bin,10000);
	_h_twr_shift->SetBinError(bin,0);
      }
      //
      delete fit;
      delete h1;
    }
    cout<<endl;
  }
  //========================================================================
  if( opt_qa ){
    ch = _nch;
    while( ch-- ){
      if( GetQAtwr(ch) != 0 ){
	int bin = _h_twr_shift->FindBin(ch);
	_h_twr_shift->SetBinContent(bin,-10000);
	_h_twr_shift->SetBinError(bin,0);
	nfit = 4;
	while( nfit-- ){
	  _h_twr_fit[nfit]->SetBinContent(bin,0);
	  _h_twr_fit[nfit]->SetBinError(bin,0);
	}
      }
    }
  }

  _th1xch_afttwr->Sumup(_th1xch,_h_twr_shift);

  return 1;
};
//------------------------------------------------------------
int EmcCalibratorTOF::Reset(char* opt){
  char* opt_t;
  opt_t = strstr(opt,"t");
  //
  EmcCalibrator::Reset(opt);
  int nfit;
  _th0xrun_shift.Reset(opt);
  nfit = 4;
  while( nfit-- )
    _th0xrun_fit[nfit].Reset(opt);
  if( opt_t ){
    cout<<" "<<GetName()<<"::Reset() skip resetting of _h_twr_shift and _h_twr_pshift "<<endl;
  } else {
    _h_twr_pshift->Reset(opt);
    _h_twr_shift->Reset(opt);
  }
  nfit = 4;
  while( nfit-- )
    _h_twr_fit[nfit]->Reset(opt);
  _th1xch->Reset(opt);
  _th1xch_afttwr->Reset(opt);
  return 1;
};
//------------------------------------------------------------
bool EmcCalibratorTOF::WriteFile(char* t0_run_file,char* t0_twr_file){
  //
  char hname[128],htitle[128];
  int bin,ch;
  float shift,err;
  float pshift,perr;
  if(_debug) cout<<" "<<GetName()<<"::WriteFile() writing t0 correction table for twr-by-twr : "<<t0_twr_file<<endl;
  ofstream fout_twr(t0_twr_file);
  fout_twr<<"#CH Shift Error "<<endl;
  ch = _nch;
  while( ch-- ){
    bin = _h_twr_shift->FindBin(ch);
    shift = _h_twr_shift->GetBinContent(bin);
    err = _h_twr_shift->GetBinError(bin);
    pshift = _h_twr_pshift->GetBinContent(bin);
    perr = _h_twr_pshift->GetBinError(bin);
    //
    shift = shift + pshift;
    err = sqrt(err*err + perr*perr);
    fout_twr<<ch<<" "<<shift<<" "<<err<<endl;
  }
  fout_twr.close();
  //
  if(_debug) cout<<" "<<GetName()<<"::WriteFile() writing t0 correction table for run-by-run : "<<t0_run_file<<endl;
  ofstream fout_run(t0_run_file);
  fout_run<<"#Run Shift Error "<<endl;
  TGraphErrors& gra = _th0xrun_shift._gra;
  ch = gra.GetN();
  double* x = gra.GetX();
  double* y = gra.GetY();
  double* ey = gra.GetEY();
  while( ch-- ){
    fout_run<<(int)x[ch]<<" "<< y[ch]<<" "<<ey[ch]<<endl;
  }
  fout_run.close();
  //
  return true;
};
//------------------------------------------------------------
//
//

