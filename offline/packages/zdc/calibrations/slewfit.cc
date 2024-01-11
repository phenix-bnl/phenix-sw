#include <iostream>
#include <iomanip>
#include <fstream>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TStyle.h>
#include <TString.h>
#include <TGraphErrors.h>
#include <TCanvas.h>
#include <TProfile.h>

#include <TROOT.h>
#include <TApplication.h>

using namespace std;

TROOT groot("slewfit","fit slew curves");

// get x-value where one first gets a hit
TH1D *get_tdiff(TH2F *slewhist, Double_t *slewcorr);

Double_t getmin(TH1 *hist)
{
  int bin = 1;
  int nmaxbins = hist->GetNbinsX();
  for (int ibin=1; ibin<nmaxbins; ibin++)
    {
      if ( hist->GetBinContent(bin)>0 ) break;
      bin++;
    }
  return hist->GetBinLowEdge( bin );
}

// get y-value where one first gets a hit
Double_t getmax(TH1 *hist)
{
  int bin = hist->GetNbinsX();
  while ( hist->GetBinContent(bin)==0 && bin>=1 ) bin--;
  return hist->GetBinCenter( bin ) + hist->GetBinWidth( bin );
}

int main(int argc, char **argv)
{
  //TApplication *theApp = new TApplication("theApp",0,0);

  const char *fname = argv[1];
  cout << "processing " << fname << endl;
  TFile *f = new TFile(fname);
  TCanvas *canvas[2];	// [itdc]

  // these ranges are for the 2nd HV setting (after replacing tubes)
  gStyle->SetFuncWidth(0.5);
  Double_t ranges[8][3][2];	// [ich][ifitregion][imin/imax]
  // left edge of range    // right edge of range
  //ranges[0][0][0] = 450.;  ranges[0][0][1] = 700.;	// p+p
  //ranges[0][1][0] = 600.;  ranges[0][1][1] = 1100.;
  //ranges[0][2][0] = 1000.; ranges[0][2][1] = 2750.;	// p-p
  ranges[0][0][0] = 480.;  ranges[0][0][1] = 1000.; 	// Au+Au
  ranges[0][1][0] = 900.;  ranges[0][1][1] = 1800.;
  ranges[0][2][0] = 1600.; ranges[0][2][1] = 2700.;
  //ranges[1][0][0] = 380.;  ranges[1][0][1] = 700.;
  //ranges[1][1][0] = 600.;  ranges[1][1][1] = 2000.;	// d-Au value
  //ranges[1][2][0] = 1900.; ranges[1][2][1] = 3300.;
  ranges[1][0][0] = 420.;  ranges[1][0][1] = 1000.;
  ranges[1][1][0] = 900.;  ranges[1][1][1] = 2000.;
  ranges[1][2][0] = 1900.; ranges[1][2][1] = 2900.;
  //ranges[2][0][0] = 450.;  ranges[2][0][1] = 700.;	// p-p
  //ranges[2][1][0] = 600.;  ranges[2][1][1] = 1000.;	// p-p value
  //ranges[2][2][0] = 1100.; ranges[2][2][1] = 2800.;	// p-p
  ranges[2][0][0] = 450.;  ranges[2][0][1] = 1000.;
  ranges[2][1][0] = 900.;  ranges[2][1][1] = 2000.;
  ranges[2][2][0] = 1800.; ranges[2][2][1] = 2800.;
  //ranges[3][0][0] = 460.;  ranges[3][0][1] = 700.;	// p-p
  //ranges[3][1][0] = 600.;  ranges[3][1][1] = 1200.;	// p-p
  //ranges[3][2][0] = 1100.; ranges[3][2][1] = 2250.;	// p-p
  ranges[3][0][0] = 460.;  ranges[3][0][1] = 1000.;
  ranges[3][1][0] = 700.;  ranges[3][1][1] = 1200.;
  ranges[3][2][0] = 1100.; ranges[3][2][1] = 2000.;
  //ranges[4][0][0] = 400.;  ranges[4][0][1] = 650.;	// p-p value
  //ranges[4][1][0] = 600.;  ranges[4][1][1] = 800.;	// p-p
  //ranges[4][2][0] = 700.;  ranges[4][2][1] = 1200.;	// p-p
  ranges[4][0][0] = 400.;  ranges[4][0][1] = 1000.;
  ranges[4][1][0] = 900.;  ranges[4][1][1] = 2200.;
  ranges[4][2][0] = 2000.;  ranges[4][2][1] = 3000.;
  //ranges[5][0][0] = 380.;  ranges[5][0][1] = 700.;	// p-p
  //ranges[5][1][0] = 600.;  ranges[5][1][1] = 800.;	// p-p
  //ranges[5][2][0] = 700.;  ranges[5][2][1] = 1500.;	// p-p
  ranges[5][0][0] = 380.;  ranges[5][0][1] = 1000.;
  ranges[5][1][0] = 900.;  ranges[5][1][1] = 2000.;
  ranges[5][2][0] = 1800.;  ranges[5][2][1] = 3350.;
  //ranges[6][0][0] = 380.;  ranges[6][0][1] = 700.;	// p-p
  //ranges[6][1][0] = 600.;  ranges[6][1][1] = 800.;	// p-p
  //ranges[6][1][0] = 600.;  ranges[6][1][1] = 800.;	// p-p
  ranges[6][0][0] = 450.;  ranges[6][0][1] = 800.;
  ranges[6][1][0] = 700.;  ranges[6][1][1] = 1600.;
  ranges[6][2][0] = 1400.;  ranges[6][2][1] = 2900.;
  //ranges[7][1][0] = 600.;  ranges[7][1][1] = 800.;	// p-p value
  //ranges[7][2][0] = 700.; ranges[7][2][1] = 1000.;	// p-p value
  ranges[7][0][0] = 430.;  ranges[7][0][1] = 700.;
  ranges[7][1][0] = 600.;  ranges[7][1][1] = 1200.;
  ranges[7][2][0] = 1100.; ranges[7][2][1] = 2000.;

  // get ranges from a file if specified
  if (argc==3)
    {
       cout << "reading ranges from file " << argv[2] << endl;
       ifstream rangefile(argv[2]);
       for (int ich=0; ich<8; ich++)
         for (int iregion=0; iregion<3; iregion++)
           {
             rangefile >> ranges[ich][iregion][0];	// min
             rangefile >> ranges[ich][iregion][1];	// max
           }
       rangefile.close();
    }

  //-* functional fits
  TString slew1 = "[0] + [1]/([2]-x) + [3]*x + [4]*x^2 + [5]*x^3";
  TString slew2 = "[0] + [1]/([2]-x)";
  TString pol2 = "[0] + [1]*x + [2]*x^2";
  TString pol3 = "[0] + [1]*x + [2]*x^2 + [3]*x^3";
  TString sqrtexpo = "[0] +  [1]/sqrt(x-[2]) + exp([3]+[4]*(x-[2]))";

// for d+Au we used these
// TString fit1 = slew1;
// TString fit2 = pol3;
// TString fit3 = pol3;

  // for p+p we used these
//  TString fit1 = slew1;
//  TString fit2 = slew2;
//  TString fit3 = slew2;

  // for Au+Au we used these
  TString fit1 = sqrtexpo;
  TString fit2 = pol3;
  TString fit3 = pol3;

  TF1 *fits[8][2][3];		// [ich][itdc][ifitregion]
  fits[0][0][0] = new TF1("fit0ch0t0", fit1.Data(), ranges[0][0][0], ranges[0][0][1]);
  fits[0][0][1] = new TF1("fit1ch0t0", fit2.Data(), ranges[0][1][0], ranges[0][1][1]);
  fits[0][0][2] = new TF1("fit2ch0t0", fit3.Data(), ranges[0][2][0], ranges[0][2][1]);
  fits[1][0][0] = new TF1("fit0ch1t0", fit1.Data(), ranges[1][0][0], ranges[1][0][1]);
  fits[1][0][1] = new TF1("fit1ch1t0", fit2.Data(), ranges[1][1][0], ranges[1][1][1]);
  fits[1][0][2] = new TF1("fit2ch1t0", fit3.Data(), ranges[1][2][0], ranges[1][2][1]);
  fits[2][0][0] = new TF1("fit0ch2t0", fit1.Data(), ranges[2][0][0], ranges[2][0][1]);
  fits[2][0][1] = new TF1("fit1ch2t0", fit2.Data(), ranges[2][1][0], ranges[2][1][1]);
  fits[2][0][2] = new TF1("fit2ch2t0", fit3.Data(), ranges[2][2][0], ranges[2][2][1]);
  fits[3][0][0] = new TF1("fit0ch3t0", fit1.Data(), ranges[3][0][0], ranges[3][0][1]);
  fits[3][0][1] = new TF1("fit1ch3t0", fit2.Data(), ranges[3][1][0], ranges[3][1][1]);
  fits[3][0][2] = new TF1("fit2ch3t0", fit3.Data(), ranges[3][2][0], ranges[3][2][1]);
  fits[4][0][0] = new TF1("fit0ch4t0", fit1.Data(), ranges[4][0][0], ranges[4][0][1]);
  fits[4][0][1] = new TF1("fit1ch4t0", fit2.Data(), ranges[4][1][0], ranges[4][1][1]);
  fits[4][0][2] = new TF1("fit2ch4t0", fit3.Data(), ranges[4][2][0], ranges[4][2][1]);
  fits[5][0][0] = new TF1("fit0ch5t0", fit1.Data(), ranges[5][0][0], ranges[5][0][1]);
  fits[5][0][1] = new TF1("fit1ch5t0", fit2.Data(), ranges[5][1][0], ranges[5][1][1]);
  fits[5][0][2] = new TF1("fit2ch5t0", fit3.Data(), ranges[5][2][0], ranges[5][2][1]);
  fits[6][0][0] = new TF1("fit0ch6t0", fit1.Data(), ranges[6][0][0], ranges[6][0][1]);
  fits[6][0][1] = new TF1("fit1ch6t0", fit2.Data(), ranges[6][1][0], ranges[6][1][1]);
  fits[6][0][2] = new TF1("fit2ch6t0", fit3.Data(), ranges[6][2][0], ranges[6][2][1]);
  fits[7][0][0] = new TF1("fit0ch7t0", fit1.Data(), ranges[7][0][0], ranges[7][0][1]);
  fits[7][0][1] = new TF1("fit1ch7t0", fit2.Data(), ranges[7][1][0], ranges[7][1][1]);
  fits[7][0][2] = new TF1("fit2ch7t0", fit3.Data(), ranges[7][2][0], ranges[7][2][1]);
  fits[0][1][0] = new TF1("fit0ch0t1", fit1.Data(), ranges[0][0][0], ranges[0][0][1]);
  fits[0][1][1] = new TF1("fit1ch0t1", fit2.Data(), ranges[0][1][0], ranges[0][1][1]);
  fits[0][1][2] = new TF1("fit2ch0t1", fit3.Data(), ranges[0][2][0], ranges[0][2][1]);
  fits[1][1][0] = new TF1("fit0ch1t1", fit1.Data(), ranges[1][0][0], ranges[1][0][1]);
  fits[1][1][1] = new TF1("fit1ch1t1", fit2.Data(), ranges[1][1][0], ranges[1][1][1]);
  fits[1][1][2] = new TF1("fit2ch1t1", fit3.Data(), ranges[1][2][0], ranges[1][2][1]);
  fits[2][1][0] = new TF1("fit0ch2t1", fit1.Data(), ranges[2][0][0], ranges[2][0][1]);
  fits[2][1][1] = new TF1("fit1ch2t1", fit2.Data(), ranges[2][1][0], ranges[2][1][1]);
  fits[2][1][2] = new TF1("fit2ch2t1", fit3.Data(), ranges[2][2][0], ranges[2][2][1]);
  fits[3][1][0] = new TF1("fit0ch3t1", fit1.Data(), ranges[3][0][0], ranges[3][0][1]);
  fits[3][1][1] = new TF1("fit1ch3t1", fit2.Data(), ranges[3][1][0], ranges[3][1][1]);
  fits[3][1][2] = new TF1("fit2ch3t1", fit3.Data(), ranges[3][2][0], ranges[3][2][1]);
  fits[4][1][0] = new TF1("fit0ch4t1", fit1.Data(), ranges[4][0][0], ranges[4][0][1]);
  fits[4][1][1] = new TF1("fit1ch4t1", fit2.Data(), ranges[4][1][0], ranges[4][1][1]);
  fits[4][1][2] = new TF1("fit2ch4t1", fit3.Data(), ranges[4][2][0], ranges[4][2][1]);
  fits[5][1][0] = new TF1("fit0ch5t1", fit1.Data(), ranges[5][0][0], ranges[5][0][1]);
  fits[5][1][1] = new TF1("fit1ch5t1", fit2.Data(), ranges[5][1][0], ranges[5][1][1]);
  fits[5][1][2] = new TF1("fit2ch5t1", fit3.Data(), ranges[5][2][0], ranges[5][2][1]);
  fits[6][1][0] = new TF1("fit0ch6t1", fit1.Data(), ranges[6][0][0], ranges[6][0][1]);
  fits[6][1][1] = new TF1("fit1ch6t1", fit2.Data(), ranges[6][1][0], ranges[6][1][1]);
  fits[6][1][2] = new TF1("fit2ch6t1", fit3.Data(), ranges[6][2][0], ranges[6][2][1]);
  fits[7][1][0] = new TF1("fit0ch7t1", fit1.Data(), ranges[7][0][0], ranges[7][0][1]);
  fits[7][1][1] = new TF1("fit1ch7t1", fit2.Data(), ranges[7][1][0], ranges[7][1][1]);
  fits[7][1][2] = new TF1("fit2ch7t1", fit3.Data(), ranges[7][2][0], ranges[7][2][1]);

  for (int ich=0; ich<8; ich++)
    {
      for (int itdc=0; itdc<2; itdc++)
        {
	  fits[ich][itdc][0]->SetParameters(1.2,49.,411.,-15.,2.6e-4);
	  fits[ich][itdc][1]->SetParameters(1,1,1,1);
	  fits[ich][itdc][2]->SetParameters(1,1,1,1);
	}
    }

  //-* read in slew profile histograms
  TProfile *zslewprof[8][2];	// [ich][itdc]
  TH2F     *zslew[8][2];	// [ich][itdc]
  TString name;

  cout << "chi2/ndf     fit1    fit2     fit3" << endl;
  for (int itdc=0; itdc<2; itdc++)
    {
      name = "zslewtdc"; name += itdc; name += "Canvas";
      canvas[itdc] = new TCanvas(name,"slew canvas",425,550);
      canvas[itdc]->Divide(2,4);
  
      for (int ich=0; ich<8; ich++)
        {
	  canvas[itdc]->cd(ich+1);

          name = "zslewprofch"; name += ich; name += "tdc"; name += itdc;
          zslewprof[ich][itdc] = (TProfile *)f->Get(name.Data());
	  zslewprof[ich][itdc]->Rebin(8);
          zslewprof[ich][itdc]->Draw();
  
          name = "zslewch"; name += ich; name += "tdc"; name += itdc;
          zslew[ich][itdc] = (TH2F *)f->Get(name.Data());
          zslew[ich][itdc]->Draw("same");

          TH1D *zslewprojx = zslew[ich][itdc]->ProjectionX();
	  Double_t min = getmin( zslewprojx );
	  Double_t max = getmax( zslewprojx );
          delete zslewprojx;

          //-** p+p ranges
/*
	  ranges[ich][0][0] = min; ranges[ich][0][1] = min+250;
	  ranges[ich][1][0] = min+200; ranges[ich][1][1] = min+500;
	  ranges[ich][2][0] = min+350; ranges[ich][2][1] = max;
*/

          //-** d+Au ranges
	  /*
	  ranges[ich][0][0] = min; ranges[ich][0][1] = min+300;
	  ranges[ich][1][0] = min+250; ranges[ich][1][1] = min+950;
	  ranges[ich][2][0] = min+900; ranges[ich][2][1] = max;

          if ( ich>=4 )
            {
	      ranges[ich][0][0] = min; ranges[ich][0][1] = min+250;
	      ranges[ich][1][0] = min+200; ranges[ich][1][1] = min+500;
	      ranges[ich][2][0] = min+350; ranges[ich][2][1] = max;
            }
	  */

	  fits[ich][itdc][0]->SetRange(ranges[ich][0][0],ranges[ich][0][1]);
	  fits[ich][itdc][1]->SetRange(ranges[ich][1][0],ranges[ich][1][1]);
	  fits[ich][itdc][2]->SetRange(ranges[ich][2][0],ranges[ich][2][1]);

          for (int ifit=0; ifit<3; ifit++)
            {
              fits[ich][itdc][ifit]->SetLineColor(ifit+3);

	      //-** set the parameters on last fit to the same as fit 2
	      if ( ifit==2 )
	        {
		  Double_t *par = fits[ich][itdc][1]->GetParameters();
		  fits[ich][itdc][ifit]->SetParameters( par );
		}

              zslewprof[ich][itdc]->Fit(fits[ich][itdc][ifit],"+RQ","same");

              if (ifit==0) cout << "ch " << ich << " tdc " << itdc;
              if (fits[ich][itdc][ifit]->GetNDF() != 0)
	        {
		  Double_t chisq = fits[ich][itdc][ifit]->GetChisquare();
		  Double_t NDF = fits[ich][itdc][ifit]->GetNDF();
                  cout << "  " << chisq/NDF;
		}
            }
          cout << endl;

	  gPad->Modified();
	  gPad->Update();
        }
    }

  // dump results to calibration file
  Double_t slewval[8][2][4096];

  Int_t fitfcn[8][2];
  for (int ich=0; ich<8; ich++)
    {
      for (int itdc=0; itdc<2; itdc++)
        {
          fitfcn[ich][itdc] = 0;
        }
    }

  for (int itdc=0; itdc<2; itdc++)
    {
      ofstream outfile;
      if (itdc==0) outfile.open("ZdcCalib.tdc0lut.temp");
      if (itdc==1) outfile.open("ZdcCalib.tdc1lut.temp");

      for (int iadc=0; iadc<4096; iadc++)
        {
          outfile << iadc << "\t";
          for (int ich=0; ich<8; ich++)
            {
              if ( fitfcn[ich][itdc] != 2 )
                {
                  int midpoint = (ranges[ ich ][ fitfcn[ich][itdc] ][ 1 ] + ranges[ ich ][ fitfcn[ich][itdc]+1 ][ 0 ])/2.0;
                  if ( iadc == midpoint )
		    {
		      fitfcn[ich][itdc]++;
                    }
                }
              slewval[ich][itdc][iadc] = fits[ich][itdc][ fitfcn[ich][itdc] ]->Eval( iadc );
              outfile << setw(10) << setiosflags(ios::fixed|ios::right)
		      << setprecision(3)
		      << fits[ich][itdc][ fitfcn[ich][itdc] ]->Eval( iadc ) << " ";
            }
          outfile << endl;
        }
      outfile.close();
    }

  //-** make channel by channel t0 corrections
  // here i should update the tdc0,1 files to include
  // the rms
  //
  // ZdcCalib.tdc0, ZdcCalib.tdc1 (PdbPmtFitPar)
  //
  //  Par0:   not used, set to 0.
  //  Par1:   tdc2ns, tdc gain to ns.
  //  Par2:   not used, set to 0.
  //  Par3:   not used, set to 0.
  //  Par4:   timing resolution of pmt, in ns.
  //  Chi2:   not used, set to 1.
  //  Status: 0 = nominal, 1 = bad, 2 = warn
  TH1D *ztdiff[8][2];

  cout << "ch    mean        rms" << endl;
  for (int ich=0; ich<8; ich++)
    for (int itdc=0; itdc<2; itdc++)
      {
        name = "ztdiffch"; name += ich; name += "tdc"; name += itdc;
        ztdiff[ich][itdc] = get_tdiff( zslew[ich][itdc], slewval[ich][itdc] );
        ztdiff[ich][itdc]->SetName(name);
        ztdiff[ich][itdc]->SetTitle(name);
        ztdiff[ich][itdc]->SetLineColor(4);
        cout << "ch" << ich << "t" << itdc
             << " " << ztdiff[ich][itdc]->GetMean()
             << " " << ztdiff[ich][itdc]->GetRMS() << endl;
      }

  //theApp->Run();

  //-** write histograms to file
  TFile resultsfile("zdcslew.root","RECREATE");
  for (int itdc=0; itdc<2; itdc++)
    {
      canvas[itdc]->Write();
      for (int ich=0; ich<8; ich++)
        {
          zslewprof[ich][itdc]->Write();
          zslew[ich][itdc]->Write();
          ztdiff[ich][itdc]->Write();
	}
    }
  resultsfile.Write();
  resultsfile.Close();

  return 0;
}

TH1D *get_tdiff(TH2F *slewhist, Double_t *slewcorr)
{
  TH2F *tempslewhist = (TH2F*)slewhist->Clone();
  tempslewhist->SetName("tempslewhist");
  tempslewhist->Reset();
  int nbinsx = tempslewhist->GetNbinsX();
  int nbinsy = tempslewhist->GetNbinsY();

  TH1D *tempyhist = slewhist->ProjectionY("tempyhist");
  TH1D *tempxhist = slewhist->ProjectionX("tempxhist");

  for (int xbin=1; xbin<=nbinsx; xbin++)
    {
      double adc = tempxhist->GetBinCenter(xbin);
      Double_t correction = slewcorr[int(adc)];
      for (int ybin=nbinsy; ybin>0; ybin--)
        {
          double bincontent = slewhist->GetBinContent(xbin,ybin);
          double newyvalue = tempyhist->GetBinCenter(ybin) - correction;
          tempslewhist->Fill(adc,newyvalue,bincontent);
        }
    }

  delete tempxhist;
  delete tempyhist;

  TH1D *tdiff = tempslewhist->ProjectionY("tdiff");
  delete tempslewhist;

  return tdiff;
}

