#include <stdio.h>
#include <stream.h>
#include "EmcCalibratorMIP.hh"

ClassImp(EmcCalibratorMIP)

//------------------------------------------------------------
EmcCalibratorMIP::EmcCalibratorMIP(): EmcCalibrator(){
  initialized = false;
  _th0xrun = NULL;
  _th1xch = NULL;
  _th0xch = NULL;
  _h_twr_slope = NULL;
  _fit_sigma[0] = FITPEAK_DOWNSIGMA;
  _fit_sigma[1] = FITPEAK_UPSIGMA;
  return;
}
//------------------------------------------------------------
EmcCalibratorMIP::EmcCalibratorMIP(const char* name, const char* title) : EmcCalibrator(name,title){
  initialized = false;
  _th0xrun = NULL;
  _th1xch = NULL;
  _th0xch = NULL;
  _h_twr_slope = NULL;
  _fit_sigma[0] = FITPEAK_DOWNSIGMA;
  _fit_sigma[1] = FITPEAK_UPSIGMA;
  return;
};
//------------------------------------------------------------
EmcCalibratorMIP::EmcCalibratorMIP(const char* name, const char* title,int nch,int nbins,Axis_t low,Axis_t up) : EmcCalibrator(name,title,nch){
  Initialize(nch,nbins,low,up);
  return;
};
//------------------------------------------------------------
EmcCalibratorMIP::~EmcCalibratorMIP(){
  if( _th0xrun ) delete _th0xrun;
  if( _th1xch ) delete _th1xch;
  if( _th0xch ) delete _th0xch;
  if( _h_twr_slope ) delete _h_twr_slope;
  return;
};
//------------------------------------------------------------
void EmcCalibratorMIP::Initialize(int nch,int nbins,Axis_t low,Axis_t up) {
  cout<<" "<<GetName()<<"::EmcCalibratorMIP::Initialize()... "<<endl;
  initialized = true;
  char hname[128],htitle[128];
  //
  sprintf(hname,"%s_twrslope",GetName());
  sprintf(htitle,"%s twr slope",GetTitle());
  _h_twr_slope = new TH1F(hname,htitle,nch,0,nch);
  //
  sprintf(hname,"%s_th1xch",GetName());
  sprintf(htitle,"%s th1xch",GetTitle());
  _th1xch = new TH1Xch(hname,htitle,nch,nbins,low,up);
  //
  sprintf(hname,"%s_th0xch",GetName());
  sprintf(htitle,"%s th0xch",GetTitle());
  _th0xch = new TH0Xch(hname,htitle,nch);
  //
  sprintf(hname,"%s_th0xrun",GetName());
  sprintf(htitle,"%s th0xrun",GetTitle());
  _th0xrun = new TH0Xrun(hname,htitle);
  //
  _fit_sigma[0] = FITPEAK_DOWNSIGMA;
  _fit_sigma[1] = FITPEAK_UPSIGMA;
  //
  return;
};
//------------------------------------------------------------
int EmcCalibratorMIP::AnalyzeCalibObj(CalibObj* calibobj,char* opt){
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // Using QA info
  char* opt_d;
  opt_d = strstr(opt,"d");
  //
  TH1Xch* th1xch = dynamic_cast<TH1Xch*>(calibobj);
  if( th1xch == NULL ) {
    cout<<" "<<GetName()<<"::AnalyzeCalibObj() can't retrieve TH1Xch* object. "<<endl;
    return 0;
  }
  //
  if( opt_d ) cout<<" "<<GetName()<<"::AnalyzeCalibObj():: Processing. : ";
  th1xch->Print();
  if(! initialized ){
    cout<<" Error: "<<GetName()<<"::AnalyzeCalibObj() is not initialized yet .. "<<endl;
    return 0;
  }
  //===================================================
  cout<<" "<<GetName()<<"::AnalyzeCalibObj() qa option is "<< ( (bool)opt_qa? "on" : "off" )<<endl;
      
  if( opt_qa && GetQArun(_cur_run) > 0 ){
    _th0xrun->Set(_cur_run,0,0,"*");
  } else {
    TH1* h1 = th1xch->_h_all;
    TF1* fit;
    if( opt_d )
      fit = fitpeak(h1,0.1,0.5,_fit_sigma[0],"_mip","0",_fit_sigma[1]);
    else
      fit = fitpeak(h1,0.1,0.5,_fit_sigma[0],"_mip","Q0",_fit_sigma[1]);
    _th0xrun->Set(_cur_run,fit->GetParameter(1),fit->GetParError(1),"*");
    _th1xch->Add(th1xch,1);
    delete fit;
  }
  //===================================================
  return(1);
};
//------------------------------------------------------------
int EmcCalibratorMIP::AnalyzeTwr(char* opt){
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // Using QA info
  char* opt_d;
  opt_d = strstr(opt,"d"); // DEBUG option
  //
  int ch = _nch;
  _th0xch->Reset(opt);
  cout<<" "<<GetName()<<"::AnalyzeTwr() MIP calibration ...."<<endl;
  cout<<" "<<GetName()<<"::AnalyzeTwr() qa option is "<< ( (bool)opt_qa? "on" : "off" )
      <<" :  "<<GetNQAtwr_all()<<" channels will not be used."<<endl;
  cout<<" "<<GetName()<<"::AnalyzeTwr() (ch:"<<ch<<"/"<<_th1xch->GetNch()<<"):: "<<flush;
  while( ch-- ){
    if( ch % 100 == 0 ) cout<<endl<<" "<<GetName()<<"::AnalyzeTwr() (ch:"<<ch<<"/"<<_th1xch->GetNch()<<"):: "<<flush;
    if( ch % 5 == 0 ) cout<<"."<<flush;
    TH1* h1 = _th1xch->CreateHistCh(ch);
    TF1* fit = fitpeak(h1,0.1,0.5,_fit_sigma[0],"_mip","Q0",_fit_sigma[1]);
    if( opt_qa && GetQAtwr(ch) > 0 ){
      _th0xch->Set(ch,0,0);
    } else {
      _th0xch->Set(ch,fit->GetParameter(1),fit->GetParError(1));
    }
    delete fit;
    delete h1;
  }
  cout<<endl;
  //

  return 0;
};
//------------------------------------------------------------
int EmcCalibratorMIP::AnalyzeSlope(char* opt,float fit_low,float fit_high){
  char* opt_d;
  opt_d = strstr(opt,"D");
  char* opt_p;
  opt_p = strstr(opt,"P");
  //
  int canvas_num = 0;
  int canvas_rep = 18;
  TCanvas* c_anatwr;
  TPostScript* ps;
  if( opt_d ){
    //c_anatwr = new TCanvas("c_anatwr","Twr-by-twr slope analysis",900,900);
    c_anatwr = new TCanvas("c_anatwr","Twr-by-twr slope analysis",800,600);
    c_anatwr->Clear();
  }
  if( opt_p ){
    cout<<" "<<GetName()<<"::AnalyzeSlope() Creating Postscript file : EmcCalibratorMIP.ps "<<endl;
    ps = new TPostScript("EmcCalibratorMIP.ps",112);
  }
  //-------------------
  cout<<" "<<GetName()<<"::AnalyzeSlope() analyzing twr-by-twr slope ...."<<endl;
  int bin;
  TF1* fit;
  TH1* h1;
  fit = new TF1("fit","[0]*exp(-1.*x/[1])",fit_low,fit_high);
  //fit = new TF1("fit","[0]*(x**(-1.*[2]))*exp(-1.*x/[1])",fit_low,fit_high);
  fit->SetParameter(1,0.1);
  h1 = _th1xch->GetHist();
  if( opt_d ) {
    fit->SetLineColor(2);
    fit->SetLineWidth(2);
    gPad->SetLogy();
    if( h1->GetEntries() > 1 ) {
      h1->Fit("fit","R");
      h1->DrawCopy();
    }
    c_anatwr->cd();
    c_anatwr->Update();
    getchar();
    canvas_num = 0;
    c_anatwr->Clear();
    c_anatwr->Divide(6,3);
    //if( opt_p )
    //ps->NewPage();
  } else {
    if( h1->GetEntries() > 1 )
      h1->Fit("fit","RQ0");
  }
  bin = 0;
  _h_twr_slope->SetBinContent(bin,fit->GetParameter(1));
  _h_twr_slope->SetBinError(bin,fit->GetParError(1));
  float slopemean = fit->GetParameter(1);
  //-----------------
  TAxis* x = _th1xch->GetHist()->GetXaxis();
  int ch;
  ch = _th1xch->GetNch();
  ch = 108;
  while( ch-- ){
    if( ch % 5 == 0 ) cout<<"."<<flush;
    if( ch % 100 == 0 ) cout<<" "<<GetName()<<"::AnalyzeSlope() ch :"<<ch<<" / "<<_th1xch->GetNch()<<endl;
    h1 = _th1xch->CreateHistCh(ch);
    h1->Sumw2();
    fit->SetParameter(1,slopemean);
    if( opt_d ) {
      c_anatwr->cd(++canvas_num);
      cout<<" "<<GetName()<<"::AnalyzeSlope() ch = "<<ch<<"     canvas_num = "<<canvas_num<<endl;
      if( h1->GetEntries() > 1 )
	h1->Fit("fit","R");
      fit->SetLineColor(2);
      fit->SetLineWidth(2);
      gPad->SetLogy();
      if( h1->GetEntries() > 1 )
	h1->DrawCopy();
      if( canvas_num % canvas_rep == 0 ){
	c_anatwr->cd();
	c_anatwr->Update();
	getchar();
	c_anatwr->Clear();
	c_anatwr->Divide(6,3);
	canvas_num = 0;
      }
      //if( opt_p )
      //ps->NewPage();
    } else {
      if( h1->GetEntries() > 1 )
	h1->Fit("fit","RQ0");
    }
    float chi2_ndf = 100;
    if( fit->GetNDF() > 0 )
      chi2_ndf = fit->GetChisquare()/fit->GetNDF();
    if( chi2_ndf<10 ){
      bin = _h_twr_slope->FindBin(ch);
      //    int i = 3;
      //    _h_twr_fit[i]->SetBinContent(bin,fit->GetChisquare() );
      //    _h_twr_fit[i]->SetBinError(bin,fit->GetNDF() );
      //    while( i-- ){
      //      _h_twr_fit[i]->SetBinContent(bin,fit->GetParameter(i));
      //      _h_twr_fit[i]->SetBinError(bin,fabs(fit->GetParError(i)));
      //    }
      _h_twr_slope->SetBinContent(bin,fit->GetParameter(1));
      _h_twr_slope->SetBinError(bin,fit->GetParError(1));
    }
    if( opt_d ){
      c_anatwr->cd();
    }
    delete h1;
  }
  //
  if( opt_d && opt_p ){
    c_anatwr->cd();
    c_anatwr->SetLogy(0);
    _h_twr_slope->Draw();
    ps->Close();
  }
  return 1;
};
//------------------------------------------------------------
int EmcCalibratorMIP::Reset(char* option){
  EmcCalibrator::Reset(option);
  _th1xch->Reset(option);
  return 1;
};
//------------------------------------------------------------
bool EmcCalibratorMIP::WriteFile(char* run_file,char* twr_file){
  bool stat0,stat1;
  cout<<" "<<GetName()<<"::WriteFileTwr("<<twr_file<<") "<<endl;
  stat0 = WriteFileTwr(twr_file);
  cout<<" "<<GetName()<<"::WriteFileRun("<<twr_file<<") "<<endl;
  stat1 = WriteFileRun(run_file);
  return stat0 && stat1;
};
//------------------------------------------------------------
bool EmcCalibratorMIP::WriteFileTwr(char* twr_file){
  int n;
  cout<<" "<<GetName()<<"::WriteFile() Tower MIP output file : "<<twr_file<<endl;
  ofstream fout_twr(twr_file);
  fout_twr<<"######################################"<<endl;
  fout_twr<<"# MIP Correction Table "<<endl;
  fout_twr<<"# Tower Corr  Corr_err"<<endl; 
  n = _th0xch->GetNch();
  TH1F* h1 = _th0xch->_h_all;
  int bin;
  while( n-- ){
    bin = n+1;
    fout_twr<<n<<" "<<h1->GetBinContent(bin)<<" "<<h1->GetBinError(bin)<<endl;
  }
  fout_twr.close();
  //
  return true;
};
//------------------------------------------------------------
bool EmcCalibratorMIP::WriteFileRun(char* run_file){
  int n;
  //
  cout<<" "<<GetName()<<"::WriteFile() Run MIP output file : "<<run_file<<endl;
  ofstream fout_run(run_file);
  fout_run<<"######################################"<<endl;
  fout_run<<"# MIP Correction Table "<<endl;
  fout_run<<"# Run Corr  Corr_err"<<endl; 
  n = _th0xrun->GetN();
  TGraphErrors* gra = &(_th0xrun->_gra);
  double* run = gra->GetX();
  double* mip = gra->GetY();
  double* err = gra->GetEY();
  while( n-- ){
    fout_run<<run[n]<<" "<<mip[n]<<" "<<err[n]<<endl;
  }
  fout_run.close();

  return true;
};
//------------------------------------------------------------
//
//========================================================================
