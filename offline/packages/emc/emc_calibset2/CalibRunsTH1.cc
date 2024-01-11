#include <stdio.h>
#include <stream.h>
#include "CalibRunsTH1.hh"


ClassImp(CalibRunsTH1)

//------------------------------------------------------------
CalibRunsTH1::CalibRunsTH1(): CalibRuns(){
  // -- option --
  _opt_window = 0;
  _opt_binwidth = 0;
  _opt_nbin = 0;
  _opt_prefit = kPrefit_None;
  _opt_fixcent = DEFAULT_OPT_FIXCENT;
  //
  return;
}
//------------------------------------------------------------
CalibRunsTH1::CalibRunsTH1(const char* name, const char* title,int nch) : CalibRuns(name,title,nch){
  // -- option --
  _opt_window = DEFAULT_OPT_WINDOW;
  _opt_binwidth = DEFAULT_OPT_BINWIDTH;
  _opt_nbin = DEFAULT_OPT_NBIN;
  _opt_prefit = kPrefit_TOF; //DEFAULT_OPT_PREFIT; // FIT_ME
  _opt_fixcent = DEFAULT_OPT_FIXCENT;
  //
  return;
};
//------------------------------------------------------------
CalibRunsTH1::CalibRunsTH1(const char* name, const char* title,int nch,int xnbins,float xlow,float xup)
  : CalibRuns(name,title,nch){
  // -- option --
  _opt_window = DEFAULT_OPT_WINDOW;
  _opt_binwidth = ( xup - xlow ) /xnbins;
  _opt_nbin = xnbins;
  _opt_prefit = kPrefit_FIX;
  SetBuffersize(0);
  //
  _th1_low = xlow;
  _th1_high = xup;
  _th1_nbin = xnbins;
  //
  return;
};
//------------------------------------------------------------
CalibRunsTH1::~CalibRunsTH1(){
  return;
};
//------------------------------------------------------------
int CalibRunsTH1::Initialize(){
  int stat = 0;
  int num;

  //************************************************************************************************
  char hname[256],htitle[256];
  sprintf(hname,"%s_%d_%d",GetName(),_run,_seq);
  sprintf(htitle,"%s run %d:%d",GetTitle(),_run,_seq);
  //cout<<" CalibRunsTH1:: Creating histograms by using first "<<GetBuffersize()<<" events"<<endl;
  if( _all_num > 0 ){
    float mean = _all_sum/_all_num;
    float rms = 0;
    if(  _all_sum2/_all_num > mean*mean )
      rms = sqrt( _all_sum2/_all_num - mean*mean );
    //===================================================================  // Fitting process for TOF calibration  
    if( _opt_prefit == CalibRunsTH1::kPrefit_TOF ){
      double fit_height=0,fit_mean=0,fit_rms=0;
      float h_low,h_high;
      int h_bin;
      cout<<" "<<GetName()
	  <<"    :: Try to create histgrams automatically.......  "<<endl;
      cout<<"                 (mean,rms) = ("<<mean<<","<<rms<<")"<<endl;
      // In case of gaussian, +-3 rms(=sigma) is corresponds to 99.7%.
      // Let me take 10 channels in 1 rms, it means 200 channel in +-10 rmss.
      h_low = mean-10.*rms > _all_low - rms ? mean-10.* rms : _all_low - rms ;
      h_high = mean+10.*rms < _all_high + rms ? mean+10.*rms : _all_high + rms ;
      h_bin = 200;
      cout<<"                  Create temporarly hist "<<h_bin<<"  ("<<h_low<<","<<h_high<<")"<<endl;
      TH1F* htmp = new TH1F("htmp","Temporaly hist",h_bin,h_low,h_high);
      num = _buf_size;
      while( num-- ) htmp->Fill(_buf_x[num]);
      double par[6];
      TF1* fit;
      fit = new TF1("fit","gaus",h_low,h_high);
      htmp->Fit("fit","RWQ0");
      //---------
      if( 0 ){
	TCanvas* c1 = new TCanvas("c1","c1");
	htmp->Fit("fit","");
	htmp->Draw();
	c1->Update();
	getchar();
	delete c1;
      }
      //---------
      fit_mean = fit->GetParameter(1);
      fit_rms = fabs( fit->GetParameter(2) );
      fit_height = fit->GetParameter(0);
      fit->Delete();
      cout<<"                  Temporarly hist shows (mean,rms) = ("<<fit_mean<<","<<fit_rms<<") "<<endl;
      if( fit_mean > mean + 10*rms || fit_mean < mean - 10*rms ){
	cout<<"                  Reset mean & rms because it's out of range. "<<endl;
	fit_mean = mean;
	fit_rms = rms;
      }

      //---------
      fit = new TF1("fit","gaus+gaus(3)",h_low,h_high);
      //  this is for TOF calibration...
      fit->SetParameters(fit_height,fit_mean,fit_rms,fit_height*0.1,fit_mean+80,fit_rms);
      fit->SetParLimits( 3, 0, fit_height*0.5);
      fit->SetParLimits( 4, fit_mean+40, fit_mean+150);
      fit->SetParLimits( 5, 0, fit_rms * 3);
      htmp->Fit("fit","RWQ0");
      fit->GetParameters(par);
      if( par[0]*fabs(par[2]) > par[3]*fabs(par[5]) ){
	fit_mean = par[1];
	fit_rms = fabs(par[2]);
      } else {
	fit_mean = par[4];
	fit_rms = fabs(par[5]);
      }
      cout<<"                  Fit results is (mean,rms) = ("<<fit_mean<<","<<fit_rms<<")"<<endl;
      fit->Delete();
      htmp->Delete();
      //-----------------
      if( fit_rms > 0 && _opt_binwidth > 0 && _opt_window > 0 && _opt_nbin > 0 ){
	_th1_binwidth = _opt_binwidth;
	//	float window_mean = (int) (mean / _th1_binwidth) * _th1_binwidth;
	float window_mean = (int) (fit_mean / _th1_binwidth) * _th1_binwidth;
	int window_bin = (int)(_opt_window * fit_rms /_th1_binwidth) ;
	int max_bin = (int) (_opt_nbin/2 );
	if( window_bin < max_bin ){
	  _th1_nbin = window_bin;
	  _th1_low =  window_bin * _th1_binwidth;
	} else {
	  _th1_nbin = max_bin;
	  _th1_low =  max_bin * _th1_binwidth;
	}
	_th1_low = window_mean - _th1_low;
	//
	if( window_bin < max_bin ){
	  _th1_nbin += window_bin;
	  _th1_high = window_bin * _th1_binwidth;
	} else {
	  _th1_nbin += max_bin ;
	  _th1_high = max_bin * _th1_binwidth;
	}
	_th1_high = window_mean + _th1_high;
	stat = 1;
      }
    }
    //============================================================================= // Fitting process for MIP calibration
    if( _opt_prefit == CalibRunsTH1::kPrefit_MIP ){
      _th1_nbin = 100;
      _th1_high = 1.0;
      _th1_low = 0.0;
      _th1_binwidth = 0.01;
      stat = 1;
    }
    //============================================================================= // FIXCENT region
    if( _opt_prefit == CalibRunsTH1::kPrefit_FIXCENT &&
	_opt_binwidth > 0 && _opt_nbin > 0 ){
      int fixcent_nbin = (int) rint(_opt_fixcent / _opt_binwidth );
      _th1_nbin = (int)(_opt_nbin/2.0);
      _th1_low = (fixcent_nbin -  _th1_nbin) * _opt_binwidth;
      _th1_high = (fixcent_nbin +  _th1_nbin) * _opt_binwidth;
      _th1_binwidth = _opt_binwidth;
      _th1_nbin = _th1_nbin*2;
      //      cout<<"     fixcent_nbin : "<<fixcent_nbin<<endl;
      //      cout<<"     _opt_fixcent : "<<_opt_fixcent<<endl;
      //      cout<<"     _opt_binwidth : "<<_opt_binwidth<<endl;
      //      cout<<"     _opt_nbin : "<<_opt_nbin<<endl;
      //      cout<<"     _th1_nbin : "<<_th1_nbin<<endl;
      //      cout<<"     _th1_low  : "<<_th1_low<<endl;
      //      cout<<"     _th1_high : "<<_th1_high<<endl;
      //      cout<<"     _th1_ibinwidth : "<<_th1_binwidth<<endl;
      stat = 1;
    }
    //============================================================================= // FIX region
    if( _opt_prefit == CalibRunsTH1::kPrefit_FIX &&
	_opt_binwidth > 0 && _opt_nbin > 0 ){
      _th1_binwidth = ( _th1_high - _th1_low ) / _th1_nbin;
      if( _th1_binwidth > 0 )
	stat = 1;
      //      cout<<"     _th1_nbin : "<<_th1_nbin<<endl;
      //      cout<<"     _th1_low  : "<<_th1_low<<endl;
      //      cout<<"     _th1_high : "<<_th1_high<<endl;
      //      cout<<"     _th1_ibinwidth : "<<_th1_binwidth<<endl;
    }
    //============================================================================= // No fitting.....
    if( stat == 0 && _opt_binwidth > 0 && _opt_window > 0 && rms > 0 ){
      _th1_binwidth = _opt_binwidth;
      float window_mean = (int) (mean / _th1_binwidth) * _th1_binwidth;
      int window_bin = (int)(_opt_window * rms /_th1_binwidth);
      int max_bin = (int)((window_mean - _all_low)/ _th1_binwidth ) + 1;
      cout<<" _opt_window, _th1_binwidth "<<_opt_window<<","<<_th1_binwidth<<endl;
      cout<<" window_mean = "<<window_mean<<endl;
      cout<<"  mean, rms = "<<mean<<","<<rms<<endl;
      cout<<" window_bin, max_bin = "<<window_bin<<","<<max_bin<<endl;
      if( window_bin  <  max_bin ){
	_th1_nbin = window_bin;
	_th1_low =  window_bin * _th1_binwidth;
      } else {
	_th1_nbin = max_bin;
	_th1_low =  max_bin * _th1_binwidth;
      }
      _th1_low = window_mean - _th1_low;
      max_bin = (int)((_all_high - window_mean)/ _th1_binwidth ) + 1;
      cout<<" window_bin, max_bin = "<<window_bin<<","<<max_bin<<endl;
      if( window_bin < max_bin ){
	_th1_nbin += window_bin;
	_th1_high = window_bin * _th1_binwidth;
      } else {
	_th1_nbin += max_bin;
	_th1_high = max_bin * _th1_binwidth;
      }
      _th1_high = window_mean + _th1_high;
      stat = 1;
    }
    //=============================================================================
    //============================================================================= //Creating TH1Xch...
    if( stat == 1 ){
      //cout<<GetName()<<" CalibRunsTH1:: Creating TH1Xch ........ "<<endl;
      //      cout<<"                 hname  : "<<hname<<endl;
      //      cout<<"                 htitle : "<<htitle<<endl;
      //      cout<<"                 (nch,nbin,low,high) : ("
      //	  <<_nch<<","<<_th1_nbin<<","<<_th1_low<<","<<_th1_high<<") "<<endl;
      //cout<<"                 _th1_binwidth : "<<_th1_binwidth<<endl;
      _calibobj = (CalibObj*) new TH1Xch(hname,htitle,_nch,_th1_nbin,_th1_low,_th1_high);
    } else {
      cout<<" Can't create histgrams automatically ....... "<<endl;
    }
    //=============================================================================
  } else 
    cout<<" Can't create histgrams with any events... "<<endl;
  //************************************************************************************************
  return stat;
};
//------------------------------------------------------------
//
//

