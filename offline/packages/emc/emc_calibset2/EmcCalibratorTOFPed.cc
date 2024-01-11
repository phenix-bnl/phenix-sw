#include <stdio.h>
#include <stream.h>
#include <TCanvas.h>
#include "EmcCalibratorTOFPed.hh"

ClassImp(EmcCalibratorTOFPed)

//------------------------------------------------------------
EmcCalibratorTOFPed::EmcCalibratorTOFPed(): EmcCalibrator(){
  _initialized = true;
  _calibruns = new CalibRunsTH1();
  _twr_array = new TClonesArray("TGraphErrors");
  return;
}
//------------------------------------------------------------
EmcCalibratorTOFPed::EmcCalibratorTOFPed(const char* name, const char* title,int nch) : EmcCalibrator(name,title,nch){
  char hname[128],htitle[128];
  //
  sprintf(hname,"%s_calibruns",GetName());
  sprintf(htitle,"%s CalibRuns",GetTitle());
  _calibruns = new CalibRunsTH1(hname,htitle,nch);
  _twr_array = new TClonesArray("TGraphErrors");
  //
  _initialized = true;
  return;
};
//------------------------------------------------------------
EmcCalibratorTOFPed::~EmcCalibratorTOFPed(){
  if( _initialized ){
    _calibruns->Delete();
    _twr_array->Delete();
  }
  return;
};
//------------------------------------------------------------
int EmcCalibratorTOFPed::AnalyzeCalibObj(CalibObj* calibobj,char* opt){
  if(! _initialized ){
    cout<<" Error: "<<GetName()<<"::AnalyzeCalibObj() is not initialized ."<<endl;
    return 0;
  }
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // Using QA info
  //
  TH1Xch* th1xch = dynamic_cast<TH1Xch*>(calibobj);
  if( th1xch == NULL ){
    cout<<" Error: "<<GetName()<<"::AnalyzeCalibObj() get no TH1Xch* object. "<<endl;
    return 0;
  }
  cout<<"\t:: Processing. (run,seq) = ("<<_cur_run<<","<<_cur_seq<<") : "<<th1xch->GetName()<<endl;
  //============================================================
  _calibruns->Append(th1xch,_cur_run,_cur_seq);
  //============================================================
  return(1);
}
//------------------------------------------------------------
int EmcCalibratorTOFPed::AnalyzeTwr(char* opt){
  char hname[128];
  int run,seq;
  int ch,nfit;
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  //-- Open TCanvas.
  int canvas_num = 0;
  int canvas_rep = 9;
  TCanvas* c_anatwr;
  if( opt_d ){
    c_anatwr = new TCanvas("c_anatwr","Twr-by-twr Pedestal analysis",900,900);
    c_anatwr->Clear();
    c_anatwr->Divide(3,3);
  }
  //-- Create contents of TClonesArray..
  if( _nch == 0 ){
    cout<<" "<<GetName()<<"::AnalyzeTwr() Error!!! _nch = 0 "<<endl;
    return 0;
  }
  cout<<" "<<GetName()<<"::AnalyzeTwr() Create "<<_nch<<" TGraphErrors "<<endl;
  TClonesArray& array = *_twr_array;
  TGraphErrors* gra;
  for( ch = 0; ch < _nch; ch++ ){
    gra = new(array[ch]) TGraphErrors();
    sprintf(hname,"%s_gra_%d",GetName(),ch);
    gra->SetName(hname);
  }
  //
  _calibruns->Read(0,0);
  if( opt_d ) cout<<" "<<GetName()<<"::AnalyzeTwr() _clibruns->_keymap->Print() "<<endl;
  if( opt_d )_calibruns->Print();
  sprintf(hname,"%s_%%d_%%d",_calibruns->GetName());
  TIterator* itnext;
  itnext = _calibruns->_keymap->MakeIterator();
  while( TKey* key =  (TKey*)itnext->Next() ){
    TFile* file = (TFile*) _calibruns->_keymap->GetValue(key);
    file->cd();
    sscanf(key->GetName(),hname,&run,&seq);
    cout<<" "<<GetName()<<"::AnalyzeTwr() reading "<<key->GetName()<<" ::  run:seq = "<<run<<":"<<seq<<endl;
    TH1Xch* th1xch = dynamic_cast<TH1Xch*>( key->ReadObj() );
    cout<<" "<<GetName()<<"::AnalyzeTwr() start looping over channels "<<endl;
    for( int ch = 0; ch < th1xch->GetNch() ; ch++ ){
      //for( int ch = 0; ch < 10 ; ch++ ){
      if( ch % 1000 == 0 ) cout<<" "<<GetName()<<"::AnalyzeTwr() reading channel # "<<ch<<" / "<<th1xch->GetNch()<<endl;
      if( opt_d ) cout<<" "<<GetName()<<"::AnalyzeTwr() reading channeld # "<<ch<<endl;
      TH1* h1 =  th1xch->CreateHistCh(ch);
      TF1* fit;
      if( opt_d ){
	c_anatwr->cd(++canvas_num);
	float width = 5.0;
	int ibin0 = h1->FindBin( h1->GetMean() - h1->GetRMS() * width );
	int ibin1 = h1->FindBin( h1->GetMean() + h1->GetRMS() * width );
	cout<<" "<<GetName()<<"AnalyzeTwr()  Bin Range = ("<<ibin0<<","<<ibin1<<") "<<endl;
	fit = fitpeak(h1,500,2000,2,"_ped","0");
	h1->GetXaxis()->SetRange(ibin0,ibin1);
	h1->DrawCopy();
	fit->DrawCopy("same");
	if( canvas_num == canvas_rep ){
	  c_anatwr->Update();
	  canvas_num = 0;
	  getchar();
	}
      } else {
	fit = fitpeak(h1,500,2000,2,"_ped","Q0");
      }
      TGraphErrors* gra = (TGraphErrors*)array[ch];
      int n = gra->GetN();
      gra->Set(n+1);
      if( fit->GetParameter(2) < h1->GetRMS() * 5. || 
	  fit->GetParameter(1) < h1->GetMean() - h1->GetRMS() * 5. || 
	  fit->GetParameter(1) > h1->GetMean() + h1->GetRMS() * 5.
	  ) {
	gra->SetPoint(n,run,fit->GetParameter(1));
	gra->SetPointError(n,0,fit->GetParameter(2));
      } else {
      	gra->SetPoint(n,run,h1->GetMean());
      	gra->SetPointError(n,0,h1->GetRMS());
      }
      //
      delete fit;
      delete h1;
    }
    delete th1xch;
  }
  //
  return 1;
};
//------------------------------------------------------------
int EmcCalibratorTOFPed::Reset(char* opt){
};
//------------------------------------------------------------
//==================
//==================
//==================
//==================
//------------------------------------------------------------
#ifdef SKIPSKIPKSP
bool EmcCalibratorTOFPed::WriteFile(char* calibfile){
  //
  char hname[128],htitle[128];
  int bin,ch;
  float shift,err;
  float pshift,perr;
  sprintf(hname,"t0_twr_%s.txt",calibfile);
  cout<<" "<<GetName()<<"::WriteFile() writing t0 correction table for twr-by-twr : "<<hname<<endl;
  ofstream fout_twr(hname);
  fout_twr<<"CH# Shift Error "<<endl;
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
  sprintf(hname,"t0_run_%s.txt",calibfile);
  cout<<" "<<GetName()<<"::WriteFile() writing t0 correction table for run-by-run : "<<hname<<endl;
  ofstream fout_run(hname);
  fout_run<<"Run# Shift Error "<<endl;
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

#endif
