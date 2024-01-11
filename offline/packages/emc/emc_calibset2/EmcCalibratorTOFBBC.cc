#include <stdio.h>
#include <stream.h>
#include "SumupTH.hh"
#include "EmcCalibratorTOFBBC.hh"

ClassImp(EmcCalibratorTOFBBC)

//--------------------------------------------------------------------
EmcCalibratorTOFBBC::EmcCalibratorTOFBBC(const char* name, const char* title,int nch,
					 int xnbins,float xlow,float xup,int ynbins,float ylow,float yup)
  : EmcCalibratorTOFObj(name,title,nch,xnbins,xlow,xup,ynbins,ylow,yup){
  //
  char hname[128],htitle[128];
  //
  sprintf(hname,"%s_twr_t0",GetName());
  sprintf(htitle,"%s twr T0",GetTitle());
  _h_twr_t0.SetName(hname);
  _h_twr_t0.SetTitle(htitle);
  _h_twr_t0.SetBins(nch,0,nch);
  //
  sprintf(hname,"%s_twr_s0",GetName());
  sprintf(htitle,"%s twr S0",GetTitle());
  _h_twr_s0.SetName(hname);
  _h_twr_s0.SetTitle(htitle);
  _h_twr_s0.SetBins(nch,0,nch);
  //
  int i = 3;
  while( i-- ){
    sprintf(hname,"%s_twr_fit%d",GetName(),i);
    sprintf(htitle,"%s twr fit %d",GetTitle(),i);
    _h_twr_fit[i].SetName(hname);
    _h_twr_fit[i].SetTitle(htitle);
    _h_twr_fit[i].SetBins(nch,0,nch);
  }

  return;
};
//--------------------------------------------------------------------
int EmcCalibratorTOFBBC::AnalyzeTwr(char* opt){
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_0;
  opt_0 = strstr(opt,"0");  // Using current run-by-run T0
  //  char* opt_qa;
  //  opt_qa = strstr(opt,"q"); // Using QA info
  //  char* opt_t;
  //  opt_t = strstr(opt,"t"); // Using p(re)shift before the run-by-run...
  char* opt_slew;
  opt_slew = strstr(opt,"s"); // Using Slewing correction...
  if( opt_slew ) cout<<" "<<GetName()<<"::AnalyzeTwr() selected slewing option for TOF-Energy relation"<<endl;
  //
  int canvas_num = 0;
  int canvas_rep = 9;
  TCanvas* c_anatwr;
  if( opt_d ){
    c_anatwr = new TCanvas("c_anatwr","Twr-by-twr T0 analysis",900,900);
    c_anatwr->Clear();
    c_anatwr->Divide(3,3);
  }
  //
  //  if( opt_t )
  //    _th2xch_afttwr->SumupY(_th2xch,_h_twr_pshift);
  //========================================================================
  if( opt_0 ){
    cout<<" "<<GetName()<<"::AnalyzeTwr() Option 0 :: Using current setup... "<<endl;
  //========================================================================
  } else {
    cout<<" "<<GetName()<<"::AnalyzeTwr() twr-by-twr Slewing ...."<<endl;
    TAxis* x = _th2xch->GetHist()->GetXaxis();
    TAxis* y = _th2xch->GetHist()->GetYaxis();
    int xnbins = (int) x->GetLast();
    int ch;
    ch = _th2xch->GetNch();
    FitPeak2D fp2d;
    char hname[128];
    TF1* fit;
    TF1* fx = new TF1("fx","x");
    TF2* fy;
    if( opt_slew )
      fy = new TF2("fy","y-[0]-[1]/x");
    else
      fy = new TF2("fy","y-[0]-[1]*x");
    if( opt_d ) ch = 100<ch? 100 : ch ;///////......... DEBUG............
    cout<<endl<<" "<<GetName()<<"::AnalyzeTwr() (ch:"<<ch<<"/"<<_th2xch->GetNch()<<"):: "<<flush;
    while( ch-- ){
      if( ch % 100 == 0 ) cout<<endl<<" "<<GetName()<<"::AnalyzeTwr() (ch:"<<ch<<"/"<<_th2xch->GetNch()<<"):: "<<flush;
      if( ch % 5 == 0 ) cout<<"."<<flush;
      sprintf(hname,"fit_%d",ch);
      if( opt_slew ){
	fit = new TF1(hname,"[0]+[1]/x",x->GetXmin(),x->GetXmax());
	fit->SetParameters(0,0.3);
      } else {
	fit = new TF1(hname,"[0]+[1]*x",x->GetXmin(),x->GetXmax());
      }
      fit->SetParameters(0,0);
      TH2* h2 = _th2xch->CreateHistCh(ch); // FIX.ME...
      if( opt_d ) {
	c_anatwr->cd(++canvas_num);
	fp2d.Fit(h2,'x',xnbins,x->GetXmin(),x->GetXmax(),y->GetXmin(),y->GetXmax(),_fit_sigma[0],"_gra","Q0",_fit_sigma[1]);
	fp2d.gra[1]->Fit(hname,"R");
	fit->SetLineColor(2);
	fit->SetLineWidth(2);
	h2->DrawCopy("box");
	fp2d.gra[1]->Draw("same");
	fit->DrawCopy("same");
	if( canvas_num % canvas_rep == 0 ){
	  c_anatwr->cd();
	  c_anatwr->Update();
	  getchar();
	  canvas_num = 0;
	}
      } else {
	fp2d.Fit(h2,'x',x->GetNbins(),x->GetXmin(),x->GetXmax(),
		 y->GetXmin(),y->GetXmax(),_fit_sigma[0],"_gra","RQ0",_fit_sigma[1]);
	fp2d.gra[1]->Fit(hname,"RQ0");
      }
      int bin = _h_twr_t0.FindBin(ch);
      if(opt_d) cout<<" "<<GetName()<<"::AnalyzeTwr() fit parameter = "<<fit->GetParameter(0)<<" +- "<<fit->GetParError(0)
		    <<"\t"<<fit->GetParameter(1)<<" +- "<<fit->GetParError(1)<<endl;
      _h_twr_t0.SetBinContent(bin,fit->GetParameter(0));
      _h_twr_t0.SetBinError(bin,fabs(fit->GetParError(0)));
      _h_twr_s0.SetBinContent(bin,fit->GetParameter(1));
      _h_twr_s0.SetBinError(bin,fabs(fit->GetParError(1)));
      int i = 2;
      _h_twr_fit[i].SetBinContent(bin,fit->GetChisquare());
      _h_twr_fit[i].SetBinError(bin,fit->GetNDF());
      while( i-- ){
	_h_twr_fit[i].SetBinContent(bin,fit->GetParameter(i));
	_h_twr_fit[i].SetBinError(bin,fabs(fit->GetParError(i)));
      }
      //
      TH2F* h2c = (TH2F*)h2->Clone();
      h2c->Reset();
      fy->SetParameters(fit->GetParameter(0),fit->GetParameter(1));
      SumupTH2(h2c,h2,fx,fy);
      _th2xch_afttwr->SetHistCh(ch,h2c);
      delete h2c;
      //
      delete h2;
      delete fit;
    }
    delete fx;
    delete fy;
  }
  //========================================================================

  return 1;
};

//--------------------------------------------------------------------
int EmcCalibratorTOFBBC::Reset(char* opt){
  EmcCalibratorTOFObj::Reset(opt);
  _h_twr_t0.Reset(opt);
  _h_twr_s0.Reset(opt);
  int i = 3;
  while( i-- )
    _h_twr_fit[i].Reset(opt);
  return 1;
};
//------------------------------------------------------------
bool EmcCalibratorTOFBBC::WriteFileTwr(char* twr_file){
  char hname[128],htitle[128];
  int bin,ch;
  float slew, slew_err;
  if(_debug) cout<<" "<<GetName()<<"::WriteFile() writing slewing correction table for twr-by-twr : "<<twr_file<<endl;
  ofstream fout_twr(twr_file);
  fout_twr<<"# Slewing Parameters   Tofcorr(nsec) = Tof(nsec) + Slewing/Energy(GeV)"<<endl;
  fout_twr<<"#CH Slewing Slewing_Error"<<endl;
  ch = _nch;
  while( ch-- ){
    bin = _h_twr_s0.FindBin(ch);
    slew = _h_twr_s0.GetBinContent(bin);
    slew_err = _h_twr_s0.GetBinError(bin);
    fout_twr<<ch<<" "<<slew<<" "<<slew_err<<endl;
  }
  fout_twr.close();
  //
  return true;
};
//------------------------------------------------------------
//
//

