#include <stdio.h>
#include <stream.h>
#include "EmcCalibratorQArun.hh"

ClassImp(EmcCalibratorQArun)

//------------------------------------------------------------
EmcCalibratorQArun::EmcCalibratorQArun(): EmcCalibratorQA(){
  return;
}
//------------------------------------------------------------
EmcCalibratorQArun::EmcCalibratorQArun(const char* name, const char* title) : EmcCalibratorQA(name,title){
  char hname[256],htitle[256];
  sprintf(hname,"%s_th0xrun",name);
  sprintf(htitle,"%s TH0Xrun",title);
  _th0xrun.Initialize(hname,htitle);
  return;
};
//------------------------------------------------------------
EmcCalibratorQArun::~EmcCalibratorQArun(){
  return;
};
//------------------------------------------------------------
void EmcCalibratorQArun::Draw(Option_t* option=""){
  TCanvas* c1;
  TF1* fit;
  //c1 = dynamic_cast<TCanvas*>(gROOT->FindObject("c1"));
  //if( c1 == NULL )
  c1 = new TCanvas("c1","EmcCalibratorQArun");
  c1->cd();
  int divx;
  if( GetNQArun() > 0 )
    divx = 3;
  else
    divx = 2;
  c1->Divide(divx);
  //
  c1->cd(1);
  //int binlast = _h_proj.GetXaxis()->GetLast();
  //float up = _h_proj.GetXaxis()->GetBinUpEdge(binlast);
  //float low = _h_proj.GetXaxis()->GetBinLowEdge(1);
  _th0xrun._gra.Draw("AP");
  //
  c1->cd(2);
  if( _h_proj.Integral() > 0 )
    gPad->SetLogy();
  _h_proj.Draw();
  fit = _h_proj.GetFunction("fit");
  if( fit )
    fit->Draw("same");
  //
  if( GetNQArun() > 0 ){
    c1->cd(3);
    GetQArun().Draw("A*");
  }
  //
  c1->cd();
  c1->Update();
  return;
};
//------------------------------------------------------------
bool EmcCalibratorQArun::Initialize(){
  if( _debug ) cout<<" EmcCalibratorQArun::Initialize()... "<<endl;
  char hname[128],htitle[128];
  if( _initialized ){
    if( _debug ) cout<<"  .. already initialized... "<<endl;
    return false;
  }
  EmcCalibratorQA::Initialize(0);
  //
  sprintf(hname,"%s_th0xrun",GetName());
  sprintf(htitle,"%s TH0Xrun",GetTitle());
  _th0xrun.Initialize(hname,htitle);
  //
  return true;
};
//------------------------------------------------------------
int EmcCalibratorQArun::AnalyzeCalibObj(CalibObj* qa,char* opt=""){
  cout<<" "<<GetName()<<"::AnalyzeCalibObj() is not implemented yet. "<<endl;
  return 0;
};
//------------------------------------------------------------
int EmcCalibratorQArun::AnalyzeTwr(char* opt=""){
  cout<<" "<<GetName()<<"::AnalyzeCalibObj() is not implemented yet. "<<endl;
  return 0;
};
//------------------------------------------------------------
int EmcCalibratorQArun::AnalyzeRun(TGraph* gra,char* opt=""){
  char* opt_err;
  opt_err = strstr(opt,"e"); // Analyze error bar;
  //
  _th0xrun.Reset();
  if( opt_err ){
    if(! _th0xrun.SetError(gra,opt) )
      return 0;
  } else {
    if(! _th0xrun.Set(gra,opt) )
      return 0;
  }
  return AnalyzeRun(opt);
};
//------------------------------------------------------------
int EmcCalibratorQArun::AnalyzeRun(char* opt=""){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_fit;
  opt_fit = strstr(opt,"f"); // Fitting option
  char* opt_cut;
  opt_cut = strstr(opt,"c"); // Constant cut option
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // not-overwrite QA
  //
  int run,n;
  char hname[128],htitle[128];
  float min,max;
  bool init = false;
  double* x = _th0xrun.GetRun();
  double* y = _th0xrun.GetValue();
  //====================== check _qa_bit is valid ?
  if( GetQAbit() == 0 ){
    cout<<" "<<GetName()<<"::AnalyzeTwr() doesn't have valid qa_bit, SetQAbit(int) at first"<<endl;
    return(0);
  }
  //====================== Preparing histgrams
  init = false;
  n = _th0xrun.GetN();
  while( n-- ){
    run = (int)x[n];
    if( GetQArun(run) == 0 || !opt_qa ){
      if( init ){
	if( max < y[n] ) max = y[n];
	if( min > y[n] ) min = y[n];
      } else {
	min = y[n];
	max = y[n];
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
  if( _debug || opt_d ) cout<<" "<<GetName()<<"::AnalyzeRun() _h_proj.SetBins("<<_nbin<<","<<min<<","<<max<<") "<<endl;
  _h_proj.SetBins(_nbin,min,max);
  _h_proj.Reset();
  n = _th0xrun.GetN();
  while( n-- ){
    run = (int)x[n];
    if( GetQArun(run) == 0 || !opt_qa ){
      _h_proj.Fill(y[n]);
    }
  }
  //================ QA.....................
  TF1* fit = new TF1("fit","gaus");
  if( opt_fit ) _h_proj.Fit("fit","Q0");
  float content;
  int bin;
  _err_nch = 0;
  n = _th0xrun.GetN();
  while( n-- ){
    run = (int)x[n];
    content = y[n];
    if( opt_fit ){
      if( content > fit->GetParameter(1) + fit->GetParameter(2) * _sigma ||
	  content < fit->GetParameter(1) - fit->GetParameter(2) * _sigma ){
	fit->SetRange(fit->GetParameter(1) - fit->GetParameter(2) * _sigma,
		      fit->GetParameter(1) + fit->GetParameter(2) * _sigma );
	AddQArun(run);
	_err_nch++;
      }
    } else if( opt_cut ) {
      if( content < _cut[0] || content > _cut[1] ){
	AddQArun(run);
	_err_nch++;
      }
    } else {
      if( content > _h_proj.GetMean() + _h_proj.GetRMS() * _sigma ||
	  content < _h_proj.GetMean() - _h_proj.GetRMS() * _sigma ) {
	AddQArun(run);
	_err_nch++;
      }
    } // end of if( opt_fit )
  }
  //
  if( _debug || opt_d ) {
    cout<<" "<<GetName()<<"::AnalyzeRun() ";
    if( opt_fit ){
      cout<<" fit option : (mean,rms,sigma) = ("<<fit->GetParameter(1)<<","<<fit->GetParameter(2)<<","<<_sigma<<") "<<endl;
    } else if( opt_cut ) {
      cout<<" cut option : (cutmin,cutmax) = ("<<_cut[0]<<","<<_cut[1]<<") "<<endl;
    } else {
      cout<<" normal option : (mean,rms,sigma) = ("<<_h_proj.GetMean()<<","<<_h_proj.GetRMS()<<","<<_sigma<<") "<<endl;
    }
  }
  //
  delete fit;
  cout<<" "<<GetName()<<": EmcCalibratorQArun:: bad runs "<<_err_nch<<endl;
  return 0;
};
//------------------------------------------------------------
int EmcCalibratorQArun::Reset(char* option){
  EmcCalibratorQA::Reset(option);
  _th0xrun.Reset(option);
  return 1;
};
//------------------------------------------------------------
void EmcCalibratorQArun::Print(Option_t* option) const{
  EmcCalibrator::Print(option);
  cout<<"      QArun error "<<GetNQArun()<<endl;
  cout<<"      Total QArun error "<<GetNQArun_all()<<endl;
};

//------------------------------------------------------------
