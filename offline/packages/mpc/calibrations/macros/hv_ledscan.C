// analyize an hv scan of LED, for getting the gain curve
// set.list contains fractional HV setting and run, like this
// 1.0     262508
// 0.95    262509
// 0.90    262510
// ....


#include <iostream>
#include <fstream>
#include <TGraphErrors.h>
#include <TF1.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TStyle.h>
#include <TSystem.h>
//#include <MpcMap.h>

const int NCH = 576;
TGraphErrors *gaindata[576];
TGraphErrors *gainbyDriver[20] = {0};

using namespace std;

void hv_ledscan(const char *fname = "set.list")
{
  gStyle->SetOptStat(0);
  gSystem->Load("libnanoDST.so"); 
  gSystem->Load("libtrigger.so"); 
  gSystem->Load("libPISARoot.so"); 
  gSystem->Load("libmpc.so"); 

  //int normalize = 0;	// whether to make curves such that gain(50) = 1
  int normalize = 1;	// whether to make curves such that gain(50) = 1

  int dofits = 1;	// whether to do fits to the gain curve
  const float RELATIVE_GAIN_SETTING = 0.5;	// relative gain to set

  MpcMap *mpcmap = MpcMap::instance(); 

  const int MAX_HV = 100;	// max number of HV scans
  int bad_channel[576] = {0};	// channels with bad led
  float hv[MAX_HV] = {0};
  float v[NCH][MAX_HV] = {{0}};
  float verr[NCH][MAX_HV] = {{0}};
  int j, n, chan = 0;
  double lowadc, lowadcerr, rms, rmserr, chisq, ndf;
  char buffer[100];
  ifstream infile[MAX_HV];

  // make gain for each channel
  for(int ifee576ch = 0; ifee576ch < NCH; ifee576ch++)
    {
      gaindata[ifee576ch] = new TGraphErrors(12);
      int driver = mpcmap->getDriver(ifee576ch);
      gaindata[ifee576ch]->SetMarkerStyle((driver-1)%10+20);
      gaindata[ifee576ch]->SetMarkerColor((driver-1)%5+1);
      gaindata[ifee576ch]->SetLineColor((driver-1)%5+1);
      gaindata[ifee576ch]->SetMarkerSize(0.5);
    }

  // Make gainbyDriver for each driver
  for (int idriver=0; idriver<20; idriver++)
    {
      gainbyDriver[idriver] = new TGraphErrors();
      gainbyDriver[idriver]->SetMarkerStyle((idriver)%10+20);
      gainbyDriver[idriver]->SetMarkerColor((idriver)%5+1);
      gainbyDriver[idriver]->SetLineColor((idriver)%5+1);
      gainbyDriver[idriver]->SetMarkerSize(0.5);
    }

  ifstream runlist(fname);
  const float offset = 0.002;
  int nrun = 0;
  int run_number = 0;
  while (runlist >> hv[nrun] >> run_number )
    {
      n = sprintf(buffer, "%d.led", run_number);
      infile[nrun].open(buffer);
      chan = 0;
      while( infile[nrun] >> chan >> lowadc >> lowadcerr >> rms >> rmserr >> chisq >> ndf )
	{
	  v[chan][nrun] = lowadc;
	  verr[chan][nrun] = lowadcerr;

          // cut maxed out LEDs and mark as bad
          if ( nrun==0 && lowadc>2000. && lowadcerr<30. )
            {
               bad_channel[chan] = 1;
               continue;
            }
          else if ( bad_channel[chan]>0 )
            {
              continue;
            }

          int driver = mpcmap->getDriver(chan);
          if ( driver<0 ) cout << "ch " << chan << " bad driver, " << driver << endl;
          float hv_offset = (driver-10)*offset;
          if ( dofits )
            {
              hv_offset = 0.;
            }

          if ( normalize ) // make normalized gain curve (ie, gain(50)=1)
            {
	      gaindata[chan]->SetPoint(nrun, hv[nrun]+hv_offset, lowadc/v[chan][0]); 
	      //gaindata[chan]->SetPointError(nrun, 0, lowadcerr/v[chan][0]*sqrt(pow(lowadcerr/lowadc,2)+pow(verr[chan][0]/v[chan][0],2)));

              // add in driver board data
              int npoint = gainbyDriver[driver-1]->GetN();
              gainbyDriver[driver-1]->SetPoint(npoint, hv[nrun]+hv_offset, lowadc/v[chan][0]);
            }
          else // un-normalized gain curve (ie, use led amplitudes)
            {
	      gaindata[chan]->SetPoint(nrun, hv[nrun], lowadc); 
	      gaindata[chan]->SetPointError(nrun, 0, lowadcerr);
            }
	}

      nrun++;
    }


  TString name;
  TCanvas *ac = new TCanvas("ac","gains",550,425);
  TH2F *h2temp = new TH2F("h2temp","",100,0.7,1.05,100,0,1.1);
  h2temp->SetXTitle("Fractional HV");
  h2temp->SetYTitle("Fractional LED Amplitude");

  TF1 *gaincurve[20] = {0};

  if ( dofits )
    {
      ac->Divide(5,4);
      for (int idriver=0; idriver<20; idriver++)
        {
if ( idriver<10 ) continue;
          ac->cd(idriver+1);
          h2temp->DrawCopy();

          gainbyDriver[idriver]->Draw("p");

          name = "pol2expo_"; name += idriver;
          //gaincurve[idriver] = new TF1("pol2expo","[0]*x+[1]*x*x+expo(2)",.75,1.05);
          gaincurve[idriver] = new TF1(name,"[0]*x+[1]*x*x+expo(2)",.75,1.05);
          gaincurve[idriver]->SetLineColor(6);
          gaincurve[idriver]->SetLineWidth(0.5);
          gaincurve[idriver]->SetParameters(0,0,-7.8,7.8);

          gainbyDriver[idriver]->Fit(gaincurve[idriver],"Q");
    
          int found = 0;
          for (double ifraction=1.0; ifraction>0.7; ifraction -= 0.01)
            {
              if ( gaincurve[idriver]->Eval(ifraction)<RELATIVE_GAIN_SETTING )
                {
                  cout << idriver+1 << "\t" << ifraction+0.01/2. << endl;
                  found = 1;
                  break;
                }
            }
          if ( found==0 ) cout << idriver << "\tBAD" << endl;

        }
    }
  else	// just draw on one histogram
    {
      h2temp->Draw();
      for(int ich = 0; ich <576 ; ich++)
        {
          // skip empty channels
          if ( gaindata[ich]->GetN()==0 ) continue;
          gaindata[ich]->Draw("p");
        }
    }

}

