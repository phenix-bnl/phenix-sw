#include <ZdcCalOverflow.h>
#include <packet.h>

#include <TF1.h>
#include <TH1.h>

#include <cmath>
#include <iostream>
#include <fstream>

using namespace std;

static const int maxchannels = 40;
static const int maxtdc = 2;
static const double adcmin = 3600.5;	// min adc considered for overflow
static const double adcmax = 4095.5;	// max adc value

ZdcCalOverflow::ZdcCalOverflow() : oflowmean(maxchannels*maxtdc,0.),
                                   oflowrms(maxchannels*maxtdc,0.)
{
  TString name;
  for (int ich=0; ich<maxchannels; ich++)
    for (int itdc=0; itdc<maxtdc; itdc++)
      {
	name = "oflowch"; name += ich; name += "tdc"; name += itdc;
	int adcrange = int(floor(adcmax-adcmin));
	TH1F *temphist = new TH1F(name,name,adcrange,adcmin,adcmax);
	oflowhist.push_back(temphist);
      }
}

int ZdcCalOverflow::FillFromPacket(Packet *p)
{
  if (!p) return 0;

  for (int ich=0; ich<maxchannels; ich++)
      {
	oflowhist[ich*maxtdc+0]->Fill( p->iValue(ich,"T1") );
	oflowhist[ich*maxtdc+1]->Fill( p->iValue(ich,"T2") );
      }

  return 1;
}

int ZdcCalOverflow::CalculateConstants()
{
  // output files
  ofstream oflow0file("ZdcCalib.overflow0.temp");
  ofstream oflow1file("ZdcCalib.overflow1.temp");

  TF1 gaussian("gaussian","gaus",adcmin,adcmax);
  gaussian.SetLineColor(2);

  cout << " *** Calculating Overflow ***" << endl;
  cout << "ch  tdc0_chisq tdc1_chisq" << endl;

  for (int ich=0; ich<maxchannels; ich++)
    {
      cout << ich << "\t";

      for (int itdc=0; itdc<maxtdc; itdc++)
        {
	  int oflowindex = ich*maxtdc+itdc;

	  // find peakvalue and bin number at peak
          int maxbin = oflowhist[oflowindex]->GetMaximumBin(); 
          double peaktdc = oflowhist[oflowindex]->GetBinCenter(maxbin);

	  // fit upper half to get sigma
	  gaussian.SetRange(peaktdc,peaktdc+20);
	  oflowhist[oflowindex]->Fit(&gaussian,"RLQ");
          double sigma = gaussian.GetParameter(2);

	  // now fit between (-2*sigma,+3*sigma);
	  gaussian.SetRange(peaktdc-2.0*sigma,peaktdc+3.0*sigma);
	  oflowhist[oflowindex]->Fit(&gaussian,"RLQ");

          cout << gaussian.GetChisquare()/gaussian.GetNDF() << "\t";

	  /*
          oflowmean[oflowindex] = gaussian.GetParameter(1);
          oflowrms[oflowindex] = gaussian.GetParameter(2);
	  */

	  //-** use mean and rms of distribution (and not gaussian fit)
	  oflowmean[oflowindex] = oflowhist[oflowindex]->GetMean();
	  oflowrms[oflowindex] = oflowhist[oflowindex]->GetRMS();

	  //-** enforce minimum rms
	  if ( oflowrms[oflowindex]<2.0 ) oflowrms[oflowindex] = 2.0;

          if (itdc==0) oflow0file << oflowmean[oflowindex]
	       << " " << oflowrms[oflowindex] << " 0" << endl;
	  else         oflow1file << oflowmean[oflowindex]
	       << " " << oflowrms[oflowindex] << " 0" << endl;
        }

      cout << endl;
    }

  oflow0file.close();
  oflow1file.close();

  return 1;
}
