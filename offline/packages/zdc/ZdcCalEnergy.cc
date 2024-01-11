#include "ZdcCalEnergy.h"

#include "ZdcCalib.hh"
#include <packet.h>

#include <TCanvas.h>
#include <TF1.h>
#include <TFile.h>
#include <TH1.h>

#include <iostream>
#include <fstream>

using namespace std;

static const int maxchannels = 40;
static const float adcmin = -0.5;	// min adc 
static const float adcmax = 1000.5;	// max adc

ZdcCalEnergy::ZdcCalEnergy():
  zdccalib(NULL)
{
  InitVars();
  CreateHistos();
}

ZdcCalEnergy::ZdcCalEnergy(ZdcCalib *z):
  zdccalib(z)
{
  InitVars();
  CreateHistos();
}

void
ZdcCalEnergy::InitVars()
{
  zssumescale = NAN;
  zssumerror = NAN;
  zs123escale = NAN;
  zs123error = NAN;
  znsumescale = NAN;
  znsumerror = NAN;
  zn123escale = NAN;
  zn123error = NAN;
}

void
ZdcCalEnergy::CreateHistos()
{
  zssum = new TH1F("zssum","ZDC South Analog Sum, ZDC Only Triggers",4096,-0.5,4095.5);
  znsum = new TH1F("znsum","ZDC North Analog Sum, ZDC Only Triggers",4096,-0.5,4095.5);
  zs123 = new TH1F("zs123","ZDC South Digital Sum, ZDC Only Triggers",8192,-0.5,8191.5);
  zn123 = new TH1F("zn123","ZDC North Digital Sum, ZDC Only Triggers",8192,-0.5,8191.5);
  zs1 = new TH1F("zs1","ZDC South 1",4096,-0.5,4095.5);
  zs2 = new TH1F("zs2","ZDC South 2",4096,-0.5,4095.5);
  zs3 = new TH1F("zs3","ZDC South 3",4096,-0.5,4095.5);
  zn1 = new TH1F("zn1","ZDC North 1",4096,-0.5,4095.5);
  zn2 = new TH1F("zn2","ZDC North 2",4096,-0.5,4095.5);
  zn3 = new TH1F("zn3","ZDC North 3",4096,-0.5,4095.5);
}

int ZdcCalEnergy::FillFromFile(const char *fname)
{
  TFile *zdctreefile = new TFile(fname,"READ");
  FillFromFile( zdctreefile );

  return 1;
}

int ZdcCalEnergy::FillFromFile(TFile *infile)
{
  TH1F *temp;
  temp = (TH1F*)infile->Get("zssum");
  if ( temp && zssum )
    {
      delete zssum;
      zssum = (TH1F*)temp->Clone();
    }
  temp = (TH1F*)infile->Get("znsum");
  if ( temp && znsum )
    {
      delete znsum;
      znsum = (TH1F*)temp->Clone();
    }
  temp = (TH1F*)infile->Get("zs123");
  if ( temp && zs123 )
    {
      delete zs123;
      zs123 = (TH1F*)temp->Clone();
    }
  temp = (TH1F*)infile->Get("zn123");
  if ( temp && zn123 )
    {
      delete zn123;
      zn123 = (TH1F*)temp->Clone();
    }

  return 1;
}

int ZdcCalEnergy::FillFromPacket(Packet *p)
{
  if (!p) return 0;

  // here we define the overflow to be a canonical 3800
  bool hit_in_south = ( p->iValue(0,"T1") < 3800 );
  bool hit_in_north = ( p->iValue(4,"T1") < 3800 );
  bool hit_in_s1 = ( p->iValue(1,"T1") < 3800 );
  bool hit_in_s2 = ( p->iValue(2,"T1") < 3800 );
  bool hit_in_s3 = ( p->iValue(3,"T1") < 3800 );
  bool hit_in_n1 = ( p->iValue(5,"T1") < 3800 );
  bool hit_in_n2 = ( p->iValue(6,"T1") < 3800 );
  bool hit_in_n3 = ( p->iValue(7,"T1") < 3800 );

  float sped = zdccalib->getPedestal()->getCalibPar(0)->getPeakChannel();
  float nped = zdccalib->getPedestal()->getCalibPar(4)->getPeakChannel();
  float s1ped = zdccalib->getPedestal()->getCalibPar(1)->getPeakChannel();
  float s2ped = zdccalib->getPedestal()->getCalibPar(2)->getPeakChannel();
  float s3ped = zdccalib->getPedestal()->getCalibPar(3)->getPeakChannel();
  float n1ped = zdccalib->getPedestal()->getCalibPar(5)->getPeakChannel();
  float n2ped = zdccalib->getPedestal()->getCalibPar(6)->getPeakChannel();
  float n3ped = zdccalib->getPedestal()->getCalibPar(7)->getPeakChannel();
  float sdigped = s1ped + s2ped + s3ped;
  float ndigped = n1ped + n2ped + n3ped;

  // fill ped subtracted adc histograms for any event with a zdc coincidence (dAu)
  if ( hit_in_south && hit_in_north )
    {
      if ( hit_in_north )
        {
	  znsum->Fill( p->iValue(4) - nped );
          zn123->Fill( p->iValue(5) + p->iValue(6) + p->iValue(7) - ndigped );
	}

      // fill Au side for deuteron side 1n cut
      //
//-***** terrible kludge here! we make a deuteron cut on 1n
      //if ( p->iValue(4) > 600 )
      //  {
      if ( hit_in_south )
        {
          zssum->Fill( p->iValue(0) - sped );
          zs123->Fill( p->iValue(1) + p->iValue(2) + p->iValue(3) - sdigped );
	}
      //  }

      if ( hit_in_s1 ) zs1->Fill( p->iValue(1) - s1ped );
      if ( hit_in_s2 ) zs2->Fill( p->iValue(2) - s2ped );
      if ( hit_in_s3 ) zs3->Fill( p->iValue(3) - s3ped );
      if ( hit_in_n1 ) zn1->Fill( p->iValue(5) - n1ped );
      if ( hit_in_n2 ) zn2->Fill( p->iValue(6) - n2ped );
      if ( hit_in_n3 ) zn3->Fill( p->iValue(7) - n3ped );

    }

  return 1;
}

int ZdcCalEnergy::CalculateConstants()
{
  TCanvas *fits = new TCanvas("zdcefits","zdcefits",int(550*1.5),int(425*1.5));
  fits->Divide(2,2);

  cout << endl << "Fitting north analog sum..." << endl;
  fits->cd(1);
  //FitDeuteron( znsum, znsumescale, znsumerror );
  FitAu( znsum, znsumescale, znsumerror );

  cout << endl << "Fitting north digital sum..." << endl;
  fits->cd(2);
  //FitDeuteron( zn123, zn123escale, zn123error );
  FitAu( zn123, zn123escale, zn123error );

  cout << endl << "Fitting south analog sum..." << endl;
  fits->cd(3);
  FitAu( zssum, zssumescale, zssumerror );

  cout << endl << "Fitting south digital sum..." << endl;
  fits->cd(4);
  FitAu( zs123, zs123escale, zs123error );

  // could also do cross-check between digital and analog
  // by seeing if digital vs analog plot has slope of 1

  return 1;
}

double ZdcCalEnergy::FitDeuteron(TH1 *h, double& scale, double& err)
{
  double min = 70.;
  double max = 4095.;

  // find highest non-zero bin to get max
  for (int ibin=h->GetNbinsX(); ibin>=1; ibin++)
    {
      if ( h->GetBinContent(ibin) > 0 )
        {
          max = h->GetBinCenter(ibin);
          break;
        }
    }

  // we need to rebin to get rid of systematics in adc bits
  if ( h->GetNbinsX() == 4096 ) h->Rebin(8);

  // now fit a gaussian
  TF1 gaussian("gaussian","gaus",200,max);
  gaussian.SetParameters(100,400,100);
  h->Fit("gaussian","RL");
  double height = gaussian.GetParameter(0);
  double mean = gaussian.GetParameter(1);
  double sigma = gaussian.GetParameter(2);
  max = mean + 3.0*sigma;
  cout << "Gaussian fit to d is at mean " << mean
       << " +- " << sigma << endl;

/*
  TF1 expgaus("expgaus","gaus+expo(3)",min,max);
  expgaus.SetLineWidth(0.5);
  expgaus.SetLineColor(2);
  expgaus.SetParameters(height,mean,sigma,3,-0.2);
  //-** when we do the fit, should we use "RIL"?
  h->Fit("expgaus","RIL");
*/
  TF1 powgaus("powgaus","gaus+[3]*x^[4]",min,max);
  powgaus.SetLineWidth(1);
  powgaus.SetLineColor(2);
  powgaus.SetParameters(height,mean,sigma,2.0*height,-2.0);
  //-** when we do the fit, should we use "RIL"?
  h->Fit("powgaus","R");

  scale = powgaus.GetParameter(1);
  err = powgaus.GetParError(1);

  double redchisq = (double)powgaus.GetChisquare()/(double)powgaus.GetNDF();
  cout << "Fit to deuteron gives chisq/ndf = " << redchisq << endl;
  return redchisq;
}

// n gaussians for ZDC spectrum fit
// par0 = 1n peak, par1 = sigma, parX = height
static Double_t ngaus(Double_t *x, Double_t *par)
{
  Double_t val = 0.;

  int N = 5;	// number of gaussians
  for (int ipeak=1; ipeak<=N; ipeak++)
    {
      Double_t sigmasq = ipeak*par[1]*par[1];
      Double_t xmeansq = (x[0]-ipeak*par[0])*(x[0]-ipeak*par[0]);
      val += par[1+ipeak]*exp(-0.5*xmeansq/sigmasq);
    }
  
  return val;
}

double ZdcCalEnergy::FitAu(TH1 *h, double& scale, double& err)
{
  double min = 25.;
  double max = 160.*5.;

  if ( h->GetNbinsX() == 4096 ) h->Rebin(8);

  // find first peak
  double peakheight = 0.;
  int peakbin = -1;
  int bin500 = h->FindBin(500.);
  int bin20 = h->FindBin(20.);
  for (int ibin=bin20; ibin<bin500; ibin++)
    {
      if ( h->GetBinContent(ibin)>peakheight )
        {
          peakheight = h->GetBinContent(ibin);
          peakbin = ibin;
        }
    }

  double mean = h->GetBinCenter(peakbin);
  double sigma = mean/4.0;

  cout << "Fitting gaus with min max" << min << "\t"
    << mean+2.0*sigma << endl;

  TF1 gaussian("gaussian","gaus",min,mean+2.0*sigma);
  gaussian.SetParameters(peakheight,mean,sigma);
  h->Fit("gaussian","RL");

  peakheight = gaussian.GetParameter(0);
  mean = gaussian.GetParameter(1);
  sigma = gaussian.GetParameter(2);
  max = 5.0*mean;
 
  cout << "Fitting with height mean sigma guess of "
    << peakheight << "\t" << mean << "\t" << sigma << endl;

  // fit N gaussians
  TF1 ngaussians("ngaus",ngaus,min,max,7);
  ngaussians.SetLineColor(2);
  ngaussians.SetLineWidth(1);
  ngaussians.SetParameters(mean,sigma,peakheight,30,10,10,10);

  h->Fit(&ngaussians,"RI");
  
  // now fit again
  mean = ngaussians.GetParameter(0);
  sigma = ngaussians.GetParameter(1);
  peakheight = ngaussians.GetParameter(2);
  ngaussians.SetParameter(0,mean);
  ngaussians.SetParameter(1,sigma);
  ngaussians.SetParameter(2,peakheight);
  min = mean - 1.75*sigma;
  max = 4.5*mean;
  ngaussians.SetRange(min,max);

  h->Fit(&ngaussians,"RI");

  const double neutron_energy = 100.0;
  scale = neutron_energy/ngaussians.GetParameter(0);
  err = scale*(ngaussians.GetParError(0)/ngaussians.GetParameter(0));

  return 1.;
}

void ZdcCalEnergy::SetCalibration(ZdcCalib *z)
{
  zdccalib = z;
}

int ZdcCalEnergy::SaveToFile(const char *fname)
{
  // output files
  ofstream adcfile( fname );

  adcfile << setiosflags(ios::fixed);
  adcfile.width(9);
  adcfile << zssumescale << " 1 0 0 0 1 0" << endl;
  adcfile << zs123escale << " 1 0 0 0 1 0" << endl;
  adcfile << zs123escale << " 1 0 0 0 1 0" << endl;
  adcfile << zs123escale << " 1 0 0 0 1 0" << endl;
  adcfile << znsumescale << " 1 0 0 0 1 0" << endl;
  adcfile << zn123escale << " 1 0 0 0 1 0" << endl;
  adcfile << zn123escale << " 1 0 0 0 1 0" << endl;
  adcfile << zn123escale << " 1 0 0 0 1 0" << endl;

  // hey, how about the smd energy scale ???
  // currently we hard code this sucka
  adcfile << "1.045   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.518   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.208   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.000   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.099   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.515   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.180   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.010   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.160   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.616   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.010   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.000   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.299   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.081   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.038   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.000   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.079   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.413   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.059   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.000   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "0.869   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.113   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.161   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.099   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.067   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.240   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.119   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.000   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "0.990   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.139   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.075   1.00    0.00    0.00    0.00    1.00    0" << endl;
  adcfile << "1.000   1.00    0.00    0.00    0.00    1.00    0" << endl;

  adcfile.close();
  return 1;
}
