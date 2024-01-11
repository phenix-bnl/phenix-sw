#include <stdio.h>
#include <stream.h>
#include "EmcCalibratorQA.hh"

ClassImp(EmcCalibratorQA)

//------------------------------------------------------------
EmcCalibratorQA::EmcCalibratorQA(): EmcCalibrator(){
  _initialized = false;
  _err_nch = 0;
  _nbin = DEFAULT_NBIN;
  _sigma = DEFAULT_SIGMA;
  return;
};
//------------------------------------------------------------
EmcCalibratorQA::EmcCalibratorQA(const char* name, const char* title) : EmcCalibrator(name,title){
  _initialized = false;
  _err_nch = 0;
  _nbin = DEFAULT_NBIN;
  _sigma = DEFAULT_SIGMA;
  //
  return;
};
//------------------------------------------------------------
EmcCalibratorQA::EmcCalibratorQA(const char* name, const char* title,int nch) : EmcCalibrator(name,title,nch){
  _initialized = false;
  Initialize(nch);
  //
  return;
};
//------------------------------------------------------------
EmcCalibratorQA::~EmcCalibratorQA(){
  return;
};
//------------------------------------------------------------
int EmcCalibratorQA::SetCut(float min,float max){
  _cut[0] = min;
  _cut[1] = max;
};
//------------------------------------------------------------
void EmcCalibratorQA::Draw(Option_t* option=""){
  TCanvas* c1;
  TF1* fit;
  //c1 = dynamic_cast<TCanvas*>(gROOT->FindObject("c1"));
  //if( c1 == NULL )
  c1 = new TCanvas("c1","EmcCalibratorQA");
  c1->cd();
  int divx;
  if( GetNQAtwr() > 0 )
    divx = 3;
  else
    divx = 2;
  c1->Divide(divx);
  //
  //  int binlast = _h_proj.GetXaxis()->GetLast();
  //  float up = _h_proj.GetXaxis()->GetBinUpEdge(binlast);
  //  float low = _h_proj.GetXaxis()->GetBinLowEdge(1);
  //
  c1->cd(1);
  if( _th0xch._h_all->Integral() > 0 )
    gPad->SetLogy();
  _th0xch._h_all->Draw();
  //
  c1->cd(2);
  if( _h_proj.Integral() > 0 )
    gPad->SetLogy();
  _h_proj.Draw();
  fit = _h_proj.GetFunction("fit");
  if( fit )
    fit->Draw("same");
  //
  if( GetNQAtwr() > 0 ){
    c1->cd(3);
    GetQAtwr().Draw("A*");
  }
  //
  c1->cd();
  c1->Update();
  return;
};
//------------------------------------------------------------
bool EmcCalibratorQA::Initialize(int nch){
  if( _debug ) cout<<" EmcCalibratorQA::Initialize()... "<<endl;
  char hname[128],htitle[128];
  if( _initialized ){
    if( _debug ) cout<<"  .. already initialized... "<<endl;
    return false;
  }
  _initialized = true;
  _err_nch = 0;
  _nbin = DEFAULT_NBIN;
  _sigma = DEFAULT_SIGMA;
  //
  sprintf(hname,"%s_th0xch",GetName());
  sprintf(htitle,"%s TH0Xch",GetTitle());
  _th0xch.Initialize(hname,htitle,nch);
  //
  sprintf(hname,"%s_proj_twr",GetName());
  sprintf(htitle,"%s proj twr",GetTitle());
  _h_proj.SetName(hname);
  _h_proj.SetTitle(htitle);
  //
  _cut[0] = 0;
  _cut[1] = 0;

  return true;
};
//------------------------------------------------------------
int EmcCalibratorQA::AnalyzeCalibObj(CalibObj* qa,char* opt=""){
  char* opt_d;
  opt_d = strstr(opt,"d");
  if(! _initialized ){
  }
  TH0Xch* th0xch = dynamic_cast<TH0Xch*>(qa);
  if(! th0xch ) return 0;
  if( opt_d ) cout<<" "<<GetName()<<"  summing up CalibObj("<<qa->GetName()<<") object "<<endl;
  TH0Xch tmp = _th0xch + *th0xch;
  if( opt_d ) cout<<" "<<GetName()<<"  into _th0xch("<<_th0xch.GetName()<<") object "<<endl;
  _th0xch =  tmp;
  return 1;
};
//------------------------------------------------------------
int EmcCalibratorQA::Analyze(TGraph* gra,char* opt=""){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_err;
  opt_err = strstr(opt,"e"); // Analyze error bar;
  if(! _initialized ){
    if( _debug || opt_d ) cout<<" EmcCalibratorQA:: Isn't initialized yet!"<<endl;
    return 0;
  }
  if( opt_err )
    _th0xch.SetError(gra);
  else
    _th0xch.Set(gra);
  return AnalyzeTwr(opt);
};
//------------------------------------------------------------
int EmcCalibratorQA::Analyze(TH1* h1,char* opt=""){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_err;
  opt_err = strstr(opt,"e"); // Analyze error bar;
  if(! _initialized ){
    if( _debug || opt_d ) cout<<" EmcCalibratorQA:: Isn't initialized yet!"<<endl;
    return 0;
  }
  if( opt_err )
    _th0xch.SetError(h1);
  else
    _th0xch.Set(h1);
  return AnalyzeTwr(opt);
};
//------------------------------------------------------------
int EmcCalibratorQA::AnalyzeTwr(char* opt=""){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_fit;
  opt_fit = strstr(opt,"f"); // Fitting option
  char* opt_cut;
  opt_cut = strstr(opt,"c"); // Constant cut option
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // not-overwrite QA
  //
  int n;
  char hname[128],htitle[128];
  float min,max;
  bool init = false;
  TH1F* h1 = _th0xch._h_all;
  //====================== check _qa_bit is valid ?
  if( GetQAbit() == 0 ){
    cout<<" "<<GetName()<<"::AnalyzeTwr() doesn't have valid qa_bit, SetQAbit(int) at first"<<endl;
    return(0);
  }
  //====================== Preparing histgrams
  init = false;
  n = _nch;
  while( n-- ){
    int bin = h1->FindBin(n);
    if( GetQAtwr(n) == 0 || !opt_qa ){
      if( init ){
	if( max < h1->GetBinContent(bin) )
	  max = h1->GetBinContent(bin);
	if( min > h1->GetBinContent(bin) )
	  min = h1->GetBinContent(bin);
      } else {
	min = h1->GetBinContent(bin);
	max = h1->GetBinContent(bin);
	init = true;
      }
    }
  }
  if( max == min ){
    max = max + 1;
    min = min - 1;
  }
  max = max + (max-min)*0.1;
  min = min - (max-min)*0.1;
  if( _debug || opt_d ) cout<<" "<<GetName()<<"::AnalyzeTwr() _h_proj.SetBins("<<_nbin<<","<<min<<","<<max<<") "<<endl;
  _h_proj.SetBins(_nbin,min,max);
  _h_proj.Reset();
  n = _nch;
  while( n-- ){
    int bin = h1->FindBin(n);
    if( GetQAtwr(n) == 0 || !opt_qa ){
      _h_proj.Fill(h1->GetBinContent(bin));
    }
  }
  //================ QA.....................
  TF1* fit = new TF1("fit","gaus");
  if( opt_fit ) _h_proj.Fit("fit","Q0");
  float content;
  int bin;
  _err_nch = 0;
  n = _nch;
  while( n-- ){
    bin = h1->FindBin(n);
    content = h1->GetBinContent(bin);
    if( opt_fit ){
      if( content > fit->GetParameter(1) + fit->GetParameter(2) * _sigma ||
	  content < fit->GetParameter(1) - fit->GetParameter(2) * _sigma ){
	fit->SetRange(fit->GetParameter(1) - fit->GetParameter(2) * _sigma,
		      fit->GetParameter(1) + fit->GetParameter(2) * _sigma );
	AddQAtwr(n);
	_err_nch++;
      }
    } else if( opt_cut ) {
      if( content < _cut[0] || content > _cut[1] ){
	AddQAtwr(n);
	_err_nch++;
      }
    } else {
      if( content > _h_proj.GetMean() + _h_proj.GetRMS() * _sigma ||
	  content < _h_proj.GetMean() - _h_proj.GetRMS() * _sigma ) {
	AddQAtwr(n);
	_err_nch++;
      }
    } // end of if( opt_fit )
  }

  if( _debug || opt_d ) {
    cout<<" "<<GetName()<<"::AnalyzeTwr() ";
    if( opt_fit ){
      cout<<" fit option : (mean,rms,sigma) = ("<<fit->GetParameter(1)<<","<<fit->GetParameter(2)<<","<<_sigma<<") "<<endl;
    } else if( opt_cut ) {
      cout<<" cut option : (cutmin,cutmax) = ("<<_cut[0]<<","<<_cut[1]<<") "<<endl;
    } else {
      cout<<" normal option : (mean,rms,sigma) = ("<<_h_proj.GetMean()<<","<<_h_proj.GetRMS()<<","<<_sigma<<") "<<endl;
    }
  }

  cout<<" "<<GetName()<<"::EmcCalibratorQA() bad channels "<<_err_nch<<endl;
  delete fit;
  return 0;
};
//------------------------------------------------------------
int EmcCalibratorQA::Reset(char* option){
  EmcCalibrator::Reset(option);
  _th0xch.Reset(option);
  _h_proj.Reset(option);
  _cut[0] = 0;
  _cut[0] = 0;
  return 1;
};
//------------------------------------------------------------
void EmcCalibratorQA::Print(Option_t* option) const{
  EmcCalibrator::Print(option);
  cout<<"      QA error "<<_err_nch<<"/"<<_nch<<endl;
  cout<<"      QA error "<<GetNQAtwr()<<"/"<<_nch<<endl;

};

//------------------------------------------------------------
bool EmcCalibratorQA::WriteFile(char* calibfile,int isect){
  //
  char hname[128],htitle[128];
  int ch,ind;
  int qa;
  int ix,iy,itwr;
  double* y;
  sprintf(hname,"qatwr_%s.txt",calibfile);
  cout<<" "<<GetName()<<"::WriteFile() writing QA twr table : "<<hname<<endl;
  ofstream fout_twr(hname);
  //  fout_twr<<"CH# Shift Error "<<endl;
  y  = GetQAtwr().GetY();
  ind = 0;
  ch = _nch;
  while( ch-- ){
    qa = (int) y[ch];
    if( qa > 0 ){
      if( isect < 6 ){
	ix = ch % 72;
	iy = ch / 72;
	itwr = ch + isect * 2592;
      } else {
	ix = ch % 96;
	iy = ch / 96;
	itwr = ch + (isect - 6 )* 4608 + 15552;
      }
      fout_twr<<++ind<<" "<<itwr<<" X "<<ix<<" Y "<<iy<<" dummy 0 dummy 0 dummy 0 0"<<endl;
    }
  }
  fout_twr.close();
  //
  return true;
};
//------------------------------------------------------------
