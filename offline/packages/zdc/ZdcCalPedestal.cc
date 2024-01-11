#include "ZdcCalPedestal.h"

#include <packet.h>

#include <TString.h>
#include <TF1.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TROOT.h>

#include <cmath>
#include <iostream>
#include <fstream>

using namespace std;

static const int maxchannels = 40;
static const float adcmin = -0.5;	// min adc 
static const float adcmax = 1000.5;	// max adc

ZdcCalPedestal::ZdcCalPedestal() : pedmean(maxchannels,0.), pedrms(maxchannels,0.)
{
  TString name;
  for (int ich=0; ich<maxchannels; ich++)
    {
      name = "pedestal"; name += ich;
      int adcrange = int(floor(adcmax-adcmin));
      TH1F *temphist = new TH1F(name,name,adcrange,adcmin,adcmax);
      pedhist.push_back(temphist);
    }

  for (int iarm=0; iarm<2; iarm++)
    {
      name = "analogvsdigital_arm"; name += iarm;
      TH2F *temphist = new TH2F(name,name,200,1000.5,3000.5,2700,300.5,3000.5);
      analogvsdigital.push_back(temphist);
    }
}

int ZdcCalPedestal::FillFromPacket(Packet *p)
{
  if (!p) return 0;

  // here we define the overflow to be a canonical 3800
  bool hit_in_south = ( p->iValue(0,"T1") < 3800 );
  bool hit_in_north = ( p->iValue(4,"T1") < 3800 );

  // now fill pedestal histograms for all except Ssum and Nsum
  if ( hit_in_south )
    {
      int digsum = 0;
      for (int ich=1; ich<4; ich++)
        {
          if ( p->iValue(ich,"T2") > 3800 ) pedhist[ich]->Fill( p->iValue(ich) );
          digsum += p->iValue(ich);
        }
      for (int ich=8; ich<24; ich++)
        {
          if ( p->iValue(ich,"T2") > 3800 ) pedhist[ich]->Fill( p->iValue(ich) );
        }
      analogvsdigital[0]->Fill( digsum, p->iValue(0) );
    }
  if ( hit_in_north )
    {
      int digsum = 0;
      for (int ich=5; ich<8; ich++)
        {
          if ( p->iValue(ich,"T2") > 3800 ) pedhist[ich]->Fill( p->iValue(ich) );
          digsum += p->iValue(ich);
        }
      for (int ich=24; ich<40; ich++)
        {
          if ( p->iValue(ich,"T2") > 3800 ) pedhist[ich]->Fill( p->iValue(ich) );
        }
      analogvsdigital[1]->Fill( digsum, p->iValue(4) );
    }

  return 1;
}

int ZdcCalPedestal::FillFromFile(const char *fname)
{
  TFile *calibfile = new TFile(fname,"READ");
  FillFromFile( calibfile );

  return 1;
}

int ZdcCalPedestal::FillFromFile(TFile *infile)
{
  TString name;
  for (int ich=0; ich<maxchannels; ich++)
    {
      name = "pedestal"; name += ich;
      TH1F *pedhtemp = static_cast<TH1F*> (infile->Get( name ));
      if ( !pedhtemp )
        {
          cout << name << " is missing from file" << endl;
          continue;
        }
      if ( !pedhist[ich] ) delete pedhist[ich];
      pedhist[ich] = static_cast<TH1F*> (pedhtemp->Clone());
    }

  for (int iarm=0; iarm<2; iarm++)
    {
      name = "analogvsdigital_arm"; name += iarm;
      TH2F *temphist = static_cast<TH2F*> (infile->Get( name ));
      if ( !temphist ) continue;
      if ( !analogvsdigital[iarm] ) delete analogvsdigital[iarm];
      analogvsdigital[iarm] = static_cast<TH2F*> (temphist->Clone());
    }

  return 1;
}

int ZdcCalPedestal::CalculateConstants()
{
  TF1 gaussian("gaussian","gaus",adcmin,adcmax);
  gaussian.SetLineColor(2);

  for (int ich=0; ich<maxchannels; ich++)
    {

      //-** here we take the mean and rms only
      //-** this is good for runs with some HV off
      pedmean[ich] = pedhist[ich]->GetMean();
      pedrms[ich] = pedhist[ich]->GetRMS();
      //cout << pedmean[ich] << "\t" << pedrms[ich] << endl;
      continue;

      //      int nbins = pedhist[ich]->GetNbinsX();

      // //-** we do a series of fit to determine pedestal
      // // find 1st non-zero bin to get start of range in fit
      // double firstnonzero = 0.;
      // for (int ibin=1; ibin<=nbins; ibin++)
      //   {
      //     if ( pedhist[ich]->GetBinContent(ibin) > 0. )
      //       {
      //         firstnonzero = pedhist[ich]->GetBinLowEdge(ibin);
      //         break;
      //       }
      //   }

      // int maxbin = pedhist[ich]->GetMaximumBin();
      // double peakvalue = pedhist[ich]->GetBinContent(maxbin);
      // double meanguess = pedhist[ich]->GetBinLowEdge(maxbin+1);

      // cout << "*** " << ich << " *** " << firstnonzero << endl;

      // // now pick different ranges and get lowest chisq fit
      // double redchisq = 1e9;
      // double maxstep = (meanguess-firstnonzero);
      // if ( maxstep<20.0 ) maxstep = 20.0;
      // double beststep = 0.;
      // for (double step=5.0; step<=maxstep; step=step+5.0)
      //   {
      //     gaussian.SetRange(firstnonzero,firstnonzero+step);
      //     gaussian.SetParameters(peakvalue,firstnonzero+step,4.);
      //     pedhist[ich]->Fit(&gaussian,"RLQ");
      // 	  if ( gaussian.GetNDF()<10 ) continue;
      //     double tempredchisq = gaussian.GetChisquare()/gaussian.GetNDF();
      //     if ( tempredchisq <= redchisq )
      //       {
      //         redchisq = tempredchisq;
      //         pedmean[ich] = gaussian.GetParameter(1);
      //         pedrms[ich] = gaussian.GetParameter(2);
      // 	      beststep = step;
      //       }
      //     cout << gaussian.GetParameter(1) << " "
      //          << gaussian.GetParameter(2) << " "
      //          << tempredchisq << endl;
      //   }

      // //-** now refit with best fit parameters
      // gaussian.SetRange(firstnonzero,firstnonzero+beststep);
      // gaussian.SetParameters(peakvalue,firstnonzero+beststep,4.);
      // pedhist[ich]->Fit(&gaussian,"RLQ");
    }

  double digital_ped[2] = {0.};
  for (int ich=1; ich<4; ich++) digital_ped[0] += pedmean[ich];
  for (int ich=5; ich<8; ich++) digital_ped[1] += pedmean[ich];

  // calculate the analog pedestals by comparing to
  // the analog sum to the digital sum
  TString name;
  TF1 line("line","[0]+[1]*x",1200,2000);	// need to find range better
  line.SetLineColor(2);
  for (int iarm=0; iarm<2; iarm++)
    {
      analogvsdigital[iarm]->FitSlicesY(0,1,0,1,"LQ");
      name = "analogvsdigital_arm"; name += iarm; name += "_1";
      TH1D *analogvsdigital_1 = (TH1D*)gDirectory->Get( name );
      analogvsdigital_1->Fit(&line,"RQ");

      Double_t m = line.GetParameter(1);
      Double_t b = line.GetParameter(0);
      pedmean[iarm*4] = m*digital_ped[iarm] + b;
      pedrms[iarm*4] = 1.0;	// need to propagate errors here
    }

  return 1;
}

int ZdcCalPedestal::SaveToFile(const char *fname)
{
  // output files
  ofstream pedfile( fname );

  for (int ich=0; ich<maxchannels; ich++)
    {
      pedfile << setiosflags(ios::fixed);
      pedfile.width(9);
      pedfile << setprecision(1) << pedmean[ich];
      pedfile.width(9);
      pedfile << setprecision(1) << pedrms[ich];
      pedfile.width(6);
      pedfile << setprecision(1) << 0 << endl;
    }

  pedfile.close();
  return 1;
}
