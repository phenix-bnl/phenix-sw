#include <TROOT.h>
#include <TDirectory.h>
#include <TSystem.h>
#include <TStyle.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
//#include <TGraph2D.h>
#include <TText.h>
#include <TPaveText.h>
#include <TGaxis.h>
#include <TRegexp.h>
#include <TBox.h>

#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include <fstream>

using namespace std;
const int NMODULE=60;
const int NCHIP=8;
const char chipdir[256]="ratechip_data_temp";
const char rundir[256]="raterun_data_temp";

void getxrange(Double_t &xmin, Double_t &xmax, Int_t im, Int_t ic, Int_t run0=0, Int_t run1=0);

TH1F *drawChipRateFit(TH1F *, Double_t, Double_t, Double_t, Double_t &, Double_t &, Double_t &,Double_t &, Double_t &, Double_t &,char *, Double_t, Double_t);

Int_t searchmaximumbin(TH1F *, Double_t, Double_t, Double_t &);

TBox * DrawRotated(TH1 *, Double_t);


void raterunplot(Int_t imodule=-1,Int_t ichip=-1,char *infile=0) {

  TFile *fo = new TFile("raterunplot.root","RECREATE");

  TFile *fin = 0;
  if (infile != 0) {
    fin = new TFile(infile);
  } else {
    //    fin = new TFile("raterun3.root");
    fin = new TFile("raterun3.root");
  }

  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  //  Int_t m0 = 0;//B0 West North
  //  Int_t m0 = 5;//B0 West South
  //  Int_t m0 = 10;//B0 East North
  //  Int_t m0 = 15;//B0 East South
  //  Int_t m0 = 20;//B1 West North (L0-4)
  //  Int_t m0 = 25;//B1 West North (L5-9)
  //  Int_t m0 = 30;//B1 West South (L0-4)
  //  Int_t m0 = 35;//B1 West South (L5-9)
  //  Int_t m0 = 40;//B1 East North (L10-14)
  //  Int_t m0 = 45;//B1 East North (L15-19)
  //  Int_t m0 = 50;//B1 East South (L10-14)
  //  Int_t m0 = 55;//B1 East South (L15-19)
  Double_t yfit[NMODULE][NCHIP];
  Double_t xfit[NMODULE][NCHIP];
  Double_t sxfit[NMODULE][NCHIP];

  Double_t eyfit[NMODULE][NCHIP];
  Double_t exfit[NMODULE][NCHIP];
  Double_t esxfit[NMODULE][NCHIP];


  TCanvas* c1 = new TCanvas("c1", "c1", 1000, 700);

  Int_t m5start = 0;
  Int_t m5end = NMODULE;
  Int_t m5step = 5;
  Int_t cstart = 0;
  Int_t cend = NCHIP;

  if ((imodule != -1)||(ichip != -1)) {
    m5start = imodule;
    m5end = imodule+1;
    m5step = 1;
    cstart = ichip;
    cend = ichip+1;
  }

  for (Int_t m0=m5start;m0<m5end;m0+=m5step) {
    if ((imodule == -1)&&(ichip == -1)) {
      c1->Divide(NCHIP,5,0,0);
    }

  for (Int_t im=m0;im<m0+m5step;im++) {

    for (Int_t ic=cstart;ic<cend;ic++) {
      Int_t icd = (4-(im-m0))*NCHIP+ NCHIP+1-(ic+1);
      if (((10<=m0)&&(m0<=15))||((40<=m0)&&(m0<=55))) {
	icd = (im-m0)*NCHIP+ NCHIP+1-(ic+1);
      }

      if ((imodule == -1)&&(ichip == -1)) {
	c1->cd(icd);
      }

      Double_t ratemax = 0.005;

      if (im>=20) {
	ratemax = 0.0025;
      }

      //      drawChipRate(im,ic,ratemax);
      //      gPad->SetLogy();
      //      drawChipRateCat(4,im,ic,ratemax,&yfit[im][ic]);


      //      cout << "going to drawChipRateFit" << endl;
      Double_t xmin=0;
      Double_t xmax=0;
      getxrange(xmin,xmax,im,ic);
      cout << "im, ic = " << im << " " << ic << endl;
      
      ostringstream hostr;
      //def  const Int_t icat = 4;
      const Int_t icat = 3;
      hostr << "hrate1d"<<icat<<"_" << im << "_" << ic;
      
      fin->cd();
      
      TH1F *hrate1d = (TH1F*)gROOT->FindObject(hostr.str().c_str());
      
      char *fitoption=0;
      Double_t s0 = 3;
      Double_t s1 = 3;
      drawChipRateFit(hrate1d,xmin,xmax,ratemax,yfit[im][ic],xfit[im][ic],sxfit[im][ic],eyfit[im][ic],exfit[im][ic],esxfit[im][ic],fitoption,s0,s1);
      
      fo->cd();
      hrate1d->Write();
      
      //      drawChipRateCat(3,im,ic,ratemax,"Same");
      //      drawChipRateCat(2,im,ic,ratemax,"Same");
      //      drawChipRateCat(1,im,ic,ratemax,"Same");
      //      drawChipRateCat(0,im,ic,ratemax,"Same");
      //      drawChipRateCat(3,im,ic,ratemax);
      //      Double_t ratemax=getChipRateRun(im,ic);
      //      drawChipRateRatate(im,ic,ratemax);

    }
  }
  ostringstream oplot;
  oplot << rundir << "/hrate1d_module_"<<m0<<"_"<<(m0+m5step-1)<<".png";
  cout << "oplot = " << oplot.str().c_str() << endl;
  c1->SaveAs(oplot.str().c_str());
  c1->Clear();
  
  }

  //  Double_t ratemax = drawChipRateRun(0,0);
  //  c1->cd(2);
  //  drawChipRate(0,0,ratemax);
  //  c1->cd(3);
  //  ratemax = drawChipRateRun(0,1);
  //  c1->cd(4);
  //  drawChipRate(0,1,ratemax);

  //  ofstream fout("raterunplot-ratecut.txt");
  ofstream fout("ratecut-normal.txt");
  const Int_t nindex = NMODULE*NCHIP;
  const Double_t cutfactor = 5.;
  Double_t xmc[nindex];
  Double_t ratecenter[nindex];
  Double_t eratecenter[nindex];
  Double_t ratesigma[nindex];
  Double_t eratesigma[nindex];
  Int_t run0 = 346973;
  Int_t run1 = 349680;
  for (Int_t im=0;im<NMODULE;im++) {
    for (Int_t ic=0;ic<NCHIP;ic++) {
      Int_t index = (im*NCHIP+ic);
      xmc[index] = (Double_t)index;
      ratecenter[index] = xfit[im][ic];
      eratecenter[index] = exfit[im][ic];
      ratesigma[index] = sxfit[im][ic];
      eratesigma[index] = esxfit[im][ic];
      Double_t xlow = xfit[im][ic]-cutfactor*sxfit[im][ic];
      Double_t xhigh = xfit[im][ic]+cutfactor*sxfit[im][ic];

      fout <<im << "\t" << ic << "\t" << run0 << "\t" << run1 << "\t" << xlow << "\t" << xhigh << "\t" << xfit[im][ic] << "\t" << sxfit[im][ic] << endl;           
    }
  }
  fout.close();

  TGraphErrors *gratemean = new TGraphErrors(nindex, xmc, ratecenter, 0, eratecenter);
  TGraphErrors *gratesigma = new TGraphErrors(nindex, xmc, ratesigma, 0, eratesigma);


  //  fo = new TFile("raterunplot.root","UPDATE");
  fo->cd();
  gratemean->Write("gratemean");
  gratesigma->Write("gratesigma");
  fo->Close();
}

void fitchipall(Int_t run0, Int_t run1, const char *infile, const char *outfile) {
  ///
  /// infile : raterun_[runstart]_[runend].root (output of raterun.C)
  ///  
  if (!outfile) {
    cout << "Please enter output file"<< endl;
    return;
  }
  for (Int_t im=0;im<NMODULE;im++) {
    for (Int_t ic=0;ic<NCHIP;ic++) {
      fitchip(im,ic,run0,run1,0,32000,3,3,infile);
      //fitchip(im,ic,run0,run1);
      
      ostringstream cmd;
      cmd << "cat " << rundir << "/ratecut_" << im << "_" << ic << "_" << run0 << "_" << run1 << ".txt >> " << outfile;
      gSystem->Exec(cmd.str().c_str());
    }
  }
}

void fitchipplotall(Int_t run0, Int_t run1) {
  TCanvas* c1 = new TCanvas("c1", "c1", 1000, 700);

  Int_t m5start = 0;
  Int_t m5end = NMODULE;
  Int_t m5step = 5;
  Int_t cstart = 0;
  Int_t cend = NCHIP;

  for (Int_t m0=m5start;m0<m5end;m0+=m5step) {
    c1->Divide(NCHIP,5,0,0);

    for (Int_t im=m0;im<m0+m5step;im++) {
      for (Int_t ic=cstart;ic<cend;ic++) {
	Int_t icd = (im-m0)*NCHIP+ (ic+1);
	cout << "icd = " << icd << endl;
	c1->cd(icd);
	
	ostringstream hfistr;
	hfistr << rundir << "/fitchip"<<"_" << im << "_" << ic<<"_"<<run0<<"_"<<run1<<".root";
	TFile *fi = new TFile(hfistr.str().c_str());
	ostringstream hostr;
	//hostr << "hrate1d"<<icat<<"_" << im << "_" << ic << "_" << run0 << "_" << run1;
	hostr << "hrate1d"<<"_" << im << "_" << ic << "_" << run0 << "_" << run1;
	
	TH1F *hrate1d = (TH1F *)gROOT->FindObject(hostr.str().c_str());
	cout << "hrate1d = " << hrate1d << endl;

	hrate1d->Draw();
	//	fi->Close();//fail if commented out
      }
    }
    ostringstream oplot;
    oplot <<rundir<<"/fitchip_sum_"<<m0<<"_"<<(m0+m5step-1)<<".png";
    cout << "oplot = " << oplot.str().c_str() << endl;
    c1->SaveAs(oplot.str().c_str());
    c1->Clear();
  }
}

void fitchip(Int_t imodule,Int_t ichip,Int_t run0=346973, Int_t run1=349680, char *fitoption=0, Int_t nbin=32000,Double_t s0=3, Double_t s1=3,char *infile=0) {
  ostringstream hfostr;
  hfostr << rundir << "/fitchip"<<"_" << imodule << "_" << ichip<<"_"<<run0<<"_"<<run1<<".root";
  TFile *fo = new TFile(hfostr.str().c_str(),"RECREATE");

  TFile *fin = 0;
  if (infile != 0) {
    fin = new TFile(infile);
  } else {
    fin = new TFile("raterun_data_temp/raterun_all.root");
  }

  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  //  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Double_t yfit[NMODULE][NCHIP];
  Double_t xfit[NMODULE][NCHIP];
  Double_t sxfit[NMODULE][NCHIP];

  Double_t eyfit[NMODULE][NCHIP];
  Double_t exfit[NMODULE][NCHIP];
  Double_t esxfit[NMODULE][NCHIP];

  TCanvas* c1 = new TCanvas("c1", "c1", 1000, 700);

  Int_t im = imodule;
  Int_t ic = ichip;

  Double_t ratemax = 0.005;

  if (im>=20) {
    ratemax = 0.0025;
  }
  if ((im==59)&&(ic==0)) {
    ratemax = 0.4;
  }
  
  Double_t xmin=0;
  Double_t xmax=0;
  //getxrange(xmin,xmax,im,ic,run0,run1);
  //cout << "im, ic = " << im << " " << ic << endl;
  //cout << "ratemax = " << ratemax << endl;

  /*
  Double_t deadth = 0.0004;
  Double_t hotth = 0.0034;
  if (im>=20) {
    deadth = 0.00015;
    hotth = 0.0023;
    }*/
  Double_t deadth=1e-6;
      
  ostringstream gostr;
  gostr << "grate"<<"_" << im << "_" << ic;

  fin->cd();

  TGraph *grate = (TGraph *)gROOT->FindObject(gostr.str().c_str());

  Int_t ndata  = grate->GetN();
  Double_t *runs = grate->GetX();
  Double_t *rates = grate->GetY();
  ostringstream hostr;
  //  hostr << "hrate1d"<<icat<<"_" << im << "_" << ic << "_" << run0 << "_" << run1;
  hostr << "hrate1d"<<"_" << im << "_" << ic << "_" << run0 << "_" << run1;
  
  // Double_t rmax=0.400;//def=0.200
  //test 
  Double_t rmax=0.800;//def=0.200
  TH1F *hrate1d = new TH1F(hostr.str().c_str(), hostr.str().c_str(), nbin,0,rmax);
  for (Int_t id=0;id<ndata;id++) {
    if ((run0<=runs[id])&&(runs[id]<=run1)) {
      if (rates[id]>deadth) {
	hrate1d->Fill(rates[id],1.);
      }
    }
  }

  drawChipRateFit(hrate1d,xmin,xmax,ratemax,yfit[im][ic],xfit[im][ic],sxfit[im][ic],eyfit[im][ic],exfit[im][ic],esxfit[im][ic],fitoption,s0,s1);

  ostringstream fostr;
  fostr << rundir << "/ratecut_" << im << "_" << ic << "_" << run0 << "_" << run1 << ".txt";
  
  ofstream fout(fostr.str().c_str());
  const Double_t cutfactor = 5.;
  Double_t xlow = xfit[im][ic]-cutfactor*sxfit[im][ic];
  Double_t xhigh = xfit[im][ic]+cutfactor*sxfit[im][ic];

  fout <<im << "\t" << ic << "\t" << run0 << "\t" << run1 << "\t" << xlow << "\t" << xhigh << "\t" << xfit[im][ic] << "\t" << sxfit[im][ic] << endl;           
  fout.close();
      
  fo->cd();
  hrate1d->Write();

  ostringstream oplot;
  oplot << rundir << "/hrate1d_module_"<<im<<"_"<<ic<<"_"<<run0<<"_"<<run1<<".png";
  cout << "oplot = " << oplot.str().c_str() << endl;
  c1->SaveAs(oplot.str().c_str());
  //  c1->Clear();
  delete hrate1d;
  delete c1;
  fin->Close();
  delete fin;
  fo->Close();
  delete fo;
}


Double_t drawChipRateRun(Int_t imodule, Int_t ichip) {
  ostringstream ostr;
  ostr << "grate_" << imodule << "_" << ichip;
  TGraph *grate = (TGraph*)gROOT->FindObject(ostr.str().c_str());
  grate->Draw("AP");
  return grate->GetHistogram()->GetMaximum();
}

Double_t getChipRateRun(Int_t imodule, Int_t ichip) {
  ostringstream ostr;
  ostr << "grate_" << imodule << "_" << ichip;
  TGraph *grate = (TGraph*)gROOT->FindObject(ostr.str().c_str());
  //  grate->Draw("AP");
  return grate->GetHistogram()->GetMaximum();
}

void drawChipRate(Int_t imodule, Int_t ichip, Double_t ratemax=0) {
  ostringstream hostr;
  hostr << "hrate1d_" << imodule << "_" << ichip;
  TH1F *hrate1d = (TH1F*)gROOT->FindObject(hostr.str().c_str());
  hrate1d->SetAxisRange(0,ratemax);
  hrate1d->Draw();
  //  TBox *box = DrawRotated((TH1 *)hrate1d,ratemax);
}

void drawChipRateCat(Int_t icat, Int_t imodule, Int_t ichip, Double_t ratemax=0,char *option="") {
  ostringstream hostr;
  hostr << "hrate1d"<<icat<<"_" << imodule << "_" << ichip;
  TH1F *hrate1d = (TH1F*)gROOT->FindObject(hostr.str().c_str());
  hrate1d->SetAxisRange(0,ratemax);
  if (icat==4) {
    hrate1d->SetLineColor(6);
  }
  hrate1d->Draw(option);
  /*
  Int_t mx = hrate1d->GetMaximumBin();
  Double_t my = hrate1d->GetMaximum;
  Double_t xmax = hrate1d->GetXaxis()->GetXmax();
  Double_t xmin = hrate1d->GetXaxis()->GetXmin();
  Int_t nx = hrate1d->GetXaxis()->GetNbins();
  Double_t x0 = xmin + (xmax-xmin)/(Double_t)nx*((Double_t)mx-1.);
  Double_t dx = 0.0001;
  TF1 *f1 = new TF1("f1","gaus(0)",x0-dx,x0+dx);
//  f1->SetParameter(0,my);
  f1->SetParameter(0,10);
  f1->SetParameter(1,mx);
  f1->SetParameter(2,dx);
  hrate1d->Fit("f1","RM");
  */
  //  TBox *box = DrawRotated((TH1 *)hrate1d,ratemax);
}

TH1F *drawChipRateFit(TH1F *hrate1d, Double_t xmin0, Double_t xmax0, Double_t ratemax,Double_t &yfit, Double_t &xfit, Double_t &sxfit,Double_t &eyfit,Double_t &exfit, Double_t &esxfit, char *fitoption,Double_t s0, Double_t s1) {
  
  if (hrate1d->GetEntries() == 0) {
    yfit = 0;
    xfit = 0;
    sxfit = 0;
    eyfit = 0;
    exfit = 0;
    esxfit = 0;
    hrate1d->Draw();
    cout << "hrate1d is empty, no fit" << endl;
    return 0;
  }

  hrate1d->SetAxisRange(0,ratemax);
  hrate1d->SetLineColor(4);

  Int_t mx = hrate1d->GetMaximumBin();
  Double_t ymax=0;
  if ((xmin0!=0)||(xmax0!=0)) {
    mx = searchmaximumbin(hrate1d,xmin0,xmax0,ymax);
    cout << "************** got new xmax (old/new) = " << hrate1d->GetMaximumBin() << " " << mx << endl;
  }

  if (mx==1) {
    yfit = 0;
    xfit = 0;
    sxfit = 0;
    eyfit = 0;
    exfit = 0;
    esxfit = 0;
    hrate1d->Draw();
    cout << "mx = 1, no fit " << endl;
    return 0;
  }

  Double_t my = hrate1d->GetMaximum();
  if ((xmin0!=0)||(xmax0!=0)) {
    my = ymax;
  }
  Double_t xmin = hrate1d->GetXaxis()->GetXmin();
  Double_t xmax = hrate1d->GetXaxis()->GetXmax();
  Int_t nx = hrate1d->GetXaxis()->GetNbins();
  Double_t x0 = xmin + (xmax-xmin)/(Double_t)nx*((Double_t)mx);
  Double_t dx = 0.0001;//0.0025
  Double_t rx = hrate1d->GetRMS();
  //def  if ((rx>0)&&(rx<dx)) {
  //def    dx = rx;
  //def  }
  //def  Double_t sx0 = 0.00001;//0.0025
  Double_t sx0 = 0.00005;//0.0025
  //   Double_t sx0 = 0.000001;//0.0025

  if ((rx>0)&&(rx<sx0)) {
    sx0 = rx;
  }
  Double_t cutfac0 = 3.;
  Double_t xrange0 = x0-cutfac0*dx;
  Double_t xrange1 = x0+cutfac0*dx;
  if ((xmin0!=0)||(xmax0!=0)) {
    xrange0 = xmin0;
    xrange1 = xmax0;
  }

  if (xrange0<0) {
    xrange0 = 0;
    cutfac0 = x0/dx;
    cout << "xrange0<0, recalculate cutfac0 = " << cutfac0 << endl;
    xrange1 = x0+cutfac0*dx;
    cout << "xrange1 = " << xrange1 << endl;
  }
  TF1 *f1 = new TF1("f1","gaus(0)",xrange0,xrange1);
  f1->SetLineWidth(1);
  f1->SetLineColor(2);
  f1->SetParameter(0,my);
  f1->SetParameter(1,x0);
  f1->SetParameter(2,sx0);

  //  if ((xmin0!=0)||(xmax0!=0)) {
  //    f1->SetParameter(2,(xrange1-xrange0)/3.);
  //  }
  //temp
  //hrate1d->Fit("f1","RM");
  //always
  //def
  hrate1d->Fit("f1","RML");
  Double_t chi2 = f1->GetChisquare();

  yfit = f1->GetParameter(0);
  xfit = f1->GetParameter(1);
  sxfit = fabs(f1->GetParameter(2));

  //  f1->SetRange(xfit-s0*sxfit,xfit+s1*sxfit);
  double min = (xfit>0.001) ? xfit*0.9 : xfit-0.0001;
  double max = (xfit>0.001) ? xfit*1.1 : xfit+0.0001;
  f1->SetRange(min,max);
  if ( sxfit>0.00002 ) f1->SetParameter(2,0.00002);

  //  f1->SetRange(xfit-5.*sxfit,xfit+5.*sxfit);
  //def
  //      f1->SetRange(xfit-3.*sxfit,xfit+3.*sxfit);

  //  f1->SetRange(xfit-1.*sxfit,xfit+2.*sxfit);
  //def  
  //hrate1d->Fit("f1","RM");
  if (fitoption==0) {
    hrate1d->Fit("f1","RML");
  } else {
    hrate1d->Fit("f1",fitoption);
    cout <<"!!!!!!!!! fitoption = " << fitoption << endl;
  }
  yfit = f1->GetParameter(0);
  xfit = f1->GetParameter(1);
  sxfit = fabs(f1->GetParameter(2));
  eyfit = f1->GetParError(0);
  exfit = f1->GetParError(1);
  esxfit = f1->GetParError(2);
  //  hrate1d->Write();
  delete f1;

  return hrate1d;
}

void drawChipRateRotate(Int_t imodule, Int_t ichip, Double_t ratemax=0) {
  ostringstream hostr;
  hostr << "hrate1d_" << imodule << "_" << ichip;
  TH1F *hrate1d = (TH1F*)gROOT->FindObject(hostr.str().c_str());
  DrawRotated((TH1 *)hrate1d,ratemax);
}

TBox * DrawRotated(TH1 *h, Double_t xxmax) {
  
  Double_t xmax = xxmax;
     //Draw histogram h horizontaly with bars
   Double_t ymin = h->GetMinimum();
   Double_t ymax = 1.05*h->GetMaximum();
   TAxis *axis   = h->GetXaxis();
   Double_t xmin = axis->GetXmin();
   if (xmax==0) {
     xmax = axis->GetXmax();
   }
   Int_t nbins   = axis->GetNbins();
   TH2F *h2 = new TH2F("h2",h->GetTitle(),10,ymin,ymax,nbins,xmin,xmax);
   h2->SetBit(kCanDelete);
   h2->SetDirectory(0);
   h2->SetStats(0);
   h2->Draw();
   TBox *box = new TBox();
   Int_t color = h->GetFillColor();
   //   cout << "color = " << color << endl;
   if (color == 0) color = 1;
   box->SetFillColor(color);
   Double_t dy;
   Double_t x1,y1,x2,y2;
   for (Int_t i=1;i<=nbins;i++) {
      dy = axis->GetBinWidth(i);
      x1 = 0;
      y1 = axis->GetBinCenter(i)-0.3*dy;
      x2 = h->GetBinContent(i);
      y2 = axis->GetBinCenter(i)+0.3*dy;
      box->DrawBox(x1,y1,x2,y2);
   }
   return box;
}

Int_t searchmaximumbin(TH1F *h, Double_t xmin0, Double_t xmax0, Double_t &ymax) {
  Double_t xmin = h->GetXaxis()->GetXmin();
  Double_t xmax = h->GetXaxis()->GetXmax();
  Int_t nx = h->GetXaxis()->GetNbins();
  Double_t dx = (xmax-xmin)/(Double_t)nx;
  Int_t ixmin = (xmin0-xmin)/dx + 1;
  Int_t ixmax = (xmax0-xmin)/dx + 1;

  ymax = 0;
  Int_t ixbest = ixmin;
  cout << "xmin, xmax, nx = " << xmin << " " << xmax << " " << nx << endl;
  cout << "xmin0, xmax0 = " << xmin0 << " " << xmax0 << endl;
  cout << "search: ixmin, ixmax = " << ixmin << " " << ixmax << endl;
  for (Int_t ix=ixmin;ix<=ixmax;ix++) {
    Double_t y = h->GetBinContent(ix);
    cout << "ix, y = " << ix << " " << y << endl;
    if (y>ymax) {
      ixbest = ix;
      ymax = y;
    }
  }

  return ixbest;
}


void getxrange(Double_t &xmin, Double_t &xmax, Int_t im, Int_t ic, Int_t run0, Int_t run1) {
  //do nothing now!!!
  /*

  if ((im==10)&&(ic==1)) {
    xmin=0.00100;
    xmax=0.00350;
  }
  if ((im==18)&&(ic==7)) {
    xmin=0.00005;
    xmax=0.00015;
  }
  if ((im==30)&&(ic==5)) {
    xmin=0.00025;
    xmax=0.00060;
  }
  if ((im==38)&&(ic==0)) {
    xmin=0.00025;
    xmax=0.00055;
  }
  if ((im==39)&&(ic==6)) {
    xmin=0.00005;
    xmax=0.00030;
  }
  if ((im==43)&&(ic==1)) {
    if ((run0==346973)&&(run1==347079)) {
      xmin=0.00020;
      xmax=0.00040;
      cout <<"xmin, xmax modified" << endl;
    } else if ((run0!=0)||(run1!=0)) {
      //do nothing
    } else {
      xmin=0.00010;
      xmax=0.00030;
    }
  }
  if ((im==46)&&(ic==6)) {
    xmin=0.00025;
    xmax=0.00075;
  }
  if ((im==47)&&(ic==2)) {
    xmin=0.00005;
    xmax=0.00035;
  }
  if ((im==53)&&(ic==1)) {
    xmin=0.00025;
    xmax=0.00075;
  }
  if ((im==53)&&(ic==3)) {
    xmin=0.00025;
    xmax=0.00075;
  }
  if ((im==57)&&(ic==3)) {
    xmin=0.00020;
    xmax=0.00075;
  }
  */
}


void fitchip_allrange(const char *infile, const char *rangelist)
{
  ifstream ifs;
  ifs.open(rangelist);
  string buffer;
  int module;
  int chip;
  int run_start;
  int run_end;
  while ( getline(ifs, buffer) ) {
    int nscan = sscanf(buffer.c_str(), "%d\t%d\t%d\t%d",
		       &module, &chip, &run_start, &run_end);
    if ( nscan!=4 ) {
      cout << "READ ERROR" << endl;
      return;
    }

    fitchip(module,chip,run_start,run_end,0,32000,3,3,infile);
  }
  ifs.close();

  return;
}
