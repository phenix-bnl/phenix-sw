#include <stdio.h>
#include <stream.h>
#include "SumupTH.hh"
#include "EmcCalibratorTOFMIP.hh"

ClassImp(EmcCalibratorTOFMIP)

//------------------------------------------------------------
EmcCalibratorTOFMIP::EmcCalibratorTOFMIP() : EmcCalibratorTOFObj() {
  return;
};
//------------------------------------------------------------
EmcCalibratorTOFMIP::EmcCalibratorTOFMIP(const char* name,const char* title) : EmcCalibratorTOFObj(name,title) {
  return;
};
//------------------------------------------------------------
EmcCalibratorTOFMIP::EmcCalibratorTOFMIP(const char* name,const char* title,
					 int nch,int xnbins,float xlow,float xup,int ynbins,float ylow, float yup)
  : EmcCalibratorTOFObj(name,title,nch,xnbins,xlow,xup,ynbins,ylow,yup) {
  //
  char hname[128],htitle[128];
  _fit_sigma[0] = FITPEAK_TOFMIP_DOWNSIGMA;
  _fit_sigma[1] = FITPEAK_TOFMIP_UPSIGMA;
  _pi_pidcut[0] = PI_PIDCUT_DOWN;
  _pi_pidcut[1] = PI_PIDCUT_UP;
  //
  sprintf(hname,"%s_twr_mip",GetName());
  sprintf(htitle,"%s twr MIP",GetTitle());
  _h_twr_mip.SetName(hname);
  _h_twr_mip.SetTitle(htitle);
  _h_twr_mip.SetBins(nch,0,nch);
  //
  int i = 4;
  while( i-- ){
    sprintf(hname,"%s_twr_mipfit%d",GetName(),i);
    sprintf(htitle,"%s twr mipfit %d",GetTitle(),i);
    _h_twr_mipfit[i].SetName(hname);
    _h_twr_mipfit[i].SetTitle(htitle);
    _h_twr_mipfit[i].SetBins(nch,0,nch);
  }
  //
  return;
};
//------------------------------------------------------------
int EmcCalibratorTOFMIP::AnalyzeTwr(char* opt){
  if(! _initialized ){
    cout<<" Error: "<<GetName()<<"::AnalyzeCalibObj() is not initialized ."<<endl;
    return 0;
  }
  //
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // Using QA info
  //  char* opt_t;
  //  opt_t = strstr(opt,"t"); // Using p(re)shift before the run-by-run...
  //
  int canvas_num = 0;
  int canvas_repx = 3;
  int canvas_repy = 3;
  TCanvas* c_anatwr;
  if( opt_d ){
    c_anatwr = new TCanvas("c_anatwr","Twr-by-twr TOFMIP analysis",900,900);
    c_anatwr->Clear();
    c_anatwr->Divide(canvas_repx,canvas_repy);
  }
  //========================================================================
  _th2xch_afttwr->SumupY(_th2xch,_h_twr_shift,opt);
  //========================================================================
  cout<<" "<<GetName()<<"::AnalyzeTwr() Analyzing twr-by-twr TOFMIP ...."<<endl;
  TAxis* x = _th2xch_afttwr->GetHist()->GetXaxis();
  TAxis* y = _th2xch_afttwr->GetHist()->GetYaxis();
  int xnbins = (int) x->GetLast();
  int ch;
  ch = _th2xch_afttwr->GetNch();
  char hname[128];
  TF1* fit;
  TH2* h2;
  TH1* h2_p;
  int ybin_cut[2];
  cout<<" "<<GetName()<<"::AnalyzeTwr() (ch:"<<ch<<"/"<<_th2xch_afttwr->GetNch()<<"):: "<<flush;
  while( ch-- ){
    if( ch % 100 == 0 ) cout<<endl<<" "<<GetName()<<"::AnalyzeTwr() (ch:"<<ch<<"/"<<_th2xch_afttwr->GetNch()<<"):: "<<flush;
    if( ch % 5 == 0 ) cout<<"."<<flush;
    sprintf(hname,"fit_%d",ch);
    h2 = _th2xch_afttwr->CreateHistCh(ch); // FIX.ME...
    ybin_cut[0] = h2->GetYaxis()->FindBin( _pi_pidcut[0]);
    ybin_cut[1] = h2->GetYaxis()->FindBin( _pi_pidcut[1]);
    h2_p = h2->ProjectionX("h2_p",ybin_cut[0],ybin_cut[1]);
    if( opt_d && ch < 100 ) {
      canvas_num++;
      cout<<" canvas_Num = "<<canvas_num<<endl;
      c_anatwr->cd(canvas_num);
      fit = fitpeak(h2_p,0.1,0.5,_fit_sigma[0],"_mip","0",_fit_sigma[1]);
      cout<<" "<<GetName()<<"::AnalyzeTwr() fit parameter = "<<fit->GetParameter(1)<<" +- "<<fit->GetParError(1)<<endl;
      h2_p->Draw();
      fit->Draw("same");
      //h2->Draw("colz");
      h2->Print();
      if( canvas_num % (canvas_repx*canvas_repy) == 0 ){
	c_anatwr->cd();
	c_anatwr->Update();
	getchar();
	canvas_num = 0;
	c_anatwr->cd();
	c_anatwr->Clear();
	c_anatwr->Divide(canvas_repx,canvas_repy);
      }
    } else
      fit = fitpeak(h2_p,0.1,0.5,_fit_sigma[0],"_mip","Q0",_fit_sigma[1]);
    int bin = _h_twr_mip.FindBin(ch);
    _h_twr_mip.SetBinContent(bin,fit->GetParameter(1));
    _h_twr_mip.SetBinError(bin,fabs(fit->GetParError(1)));
    int i = 3;
    _h_twr_mipfit[i].SetBinContent(bin,fit->GetChisquare());
    _h_twr_mipfit[i].SetBinError(bin,fit->GetNDF());
    while( i-- ){
      _h_twr_mipfit[i].SetBinContent(bin,fit->GetParameter(i));
      _h_twr_mipfit[i].SetBinError(bin,fabs(fit->GetParError(i)));
    }
    delete h2;
    delete h2_p;
    delete fit;
  }
  cout<<endl;
  //========================================================================

  return 1;
};
//------------------------------------------------------------
bool EmcCalibratorTOFMIP::WriteFileTwr(char* file){
  int n;
  cout<<" "<<GetName()<<"::WriteFileTwr() Tower MIP output file : "<<file<<endl;
  n = _h_twr_mip.GetXaxis()->GetLast();
  if( n != _nch ){
    cout<<" "<<GetName()<<"::WriteFileTwr() Failed to write file.. "<<endl;
    return false;
  }
  ofstream fout_twr(file);
  fout_twr<<"# MIP after PID "<<endl;
  int bin;
  while( n-- ){
    bin = n+1;
    fout_twr<<n<<" "<<_h_twr_mip.GetBinContent(bin)<<" "<<_h_twr_mip.GetBinError(bin)<<endl;
  }
  fout_twr.close();
  //
  return false;
};
//------------------------------------------------------------
