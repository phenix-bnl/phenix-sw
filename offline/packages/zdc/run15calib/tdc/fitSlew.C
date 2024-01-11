#include <iomanip>

void fitSlew(){

  /* Read Histogram */

  TFile* in=new TFile("Slew.root");
 
  TH2F* tdc[8][2]; //[ich][tdc0, tdc1]
  TProfile* tdcprof[8][2]; //[ich][tdc0, tdc1]

  for(int ich=0; ich<8; ich++)
    {
      char hname[16];
      char pname[16];
      
      sprintf(hname,"tdc0_ch%d",ich); // Tdc0*tdc2ns - Bbc.Timing vs ZDC ADC
      sprintf(pname,"p0_ch%d",ich);

      tdc[ich][0]=(TH2F*)in->Get(hname);
      tdcprof[ich][0]= tdc[ich][0]->ProfileX(pname);

      sprintf(hname,"tdc1_ch%d",ich); // Tdc1*tdc2ns - Bbc.Timing vs ZDC ADC
      sprintf(pname,"p1_ch%d",ich);

      tdc[ich][1]=(TH2F*)in->Get(hname);
      tdcprof[ich][1]= tdc[ich][1]->ProfileX(pname);
    }


  /* Set Fit Functions */

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
  TString fit1 = slew1;
  //TString fit2 = slew2;
  //TString fit3 = slew2;
  
  // for Au+Au we used these
  //TString fit1 = sqrtexpo;
  TString fit2 = pol3;
  TString fit3 = pol3;

 
  
  gStyle->SetOptStat(0);
  gStyle->SetOptFit();
  /* Set Fit Ranges */

  gStyle->SetFuncWidth(0.5);
  Double_t ranges[8][3][2];	// [ich][ifitregion][imin/imax]
  // left edge of range    // right edge of range
  //ranges[0][0][0] = 460.;  ranges[0][0][1] = 700.;	// pp
  //ranges[0][1][0] = 600.;  ranges[0][1][1] = 1400.;
  //ranges[0][2][0] = 600.; ranges[0][2][1] = 2750.;
  ranges[0][0][0] = 450.;  ranges[0][0][1] = 580.; 	// Au+Au
  ranges[0][1][0] = 580.;  ranges[0][1][1] = 800.;
  ranges[0][2][0] = 700.; ranges[0][2][1] = 3000.;

  ranges[1][0][0] = 410.;  ranges[1][0][1] = 520.;
  ranges[1][1][0] = 500.;  ranges[1][1][1] = 800.;	// d-Au value
  ranges[1][2][0] = 760.; ranges[1][2][1] = 3300.;
  //ranges[1][0][0] = 420.;  ranges[1][0][1] = 550.;
  //ranges[1][1][0] = 500.;  ranges[1][1][1] = 1000.;
  //ranges[1][2][0] = 1900.; ranges[1][2][1] = 2900.;

  //ranges[2][0][0] = 450.;  ranges[2][0][1] = 700.;	// p-p
  //ranges[2][1][0] = 600.;  ranges[2][1][1] = 1000.;	// p-p value
  //ranges[2][2][0] = 1100.; ranges[2][2][1] = 2800.;	// p-p
  ranges[2][0][0] = 440.;  ranges[2][0][1] = 540.;
  ranges[2][1][0] = 520.;  ranges[2][1][1] = 800.;
  ranges[2][2][0] = 700.; ranges[2][2][1] = 3000.;

 // ranges[3][0][0] = 480.;  ranges[3][0][1] = 660.;	// p-p
 // ranges[3][1][0] = 550.;  ranges[3][1][1] = 1200.;	// p-p
 // ranges[3][2][0] = 1100.; ranges[3][2][1] = 2250.;	// p-p
  ranges[3][0][0] = 460.;  ranges[3][0][1] = 660.;
  ranges[3][1][0] = 550.;  ranges[3][1][1] = 1200.;
  ranges[3][2][0] = 1100.; ranges[3][2][1] = 2000.;
 
  // ranges[4][0][0] = 450.;  ranges[4][0][1] = 550.;	// p-p value
  // ranges[4][1][0] = 550.;  ranges[4][1][1] = 900.;	// p-p
  // ranges[4][2][0] = 700.;  ranges[4][2][1] = 1200.;	// p-p
  ranges[4][0][0] = 410.;  ranges[4][0][1] = 550.;
  ranges[4][1][0] = 540.;  ranges[4][1][1] = 800.;
  ranges[4][2][0] = 700.;  ranges[4][2][1] = 3000.;

 // ranges[5][0][0] = 410.;  ranges[5][0][1] = 700.;	// p-p
 // ranges[5][1][0] = 600.;  ranges[5][1][1] = 1000.;	// p-p
 // ranges[5][2][0] = 700.;  ranges[5][2][1] = 1500.;	// p-p
  ranges[5][0][0] = 410.;  ranges[5][0][1] = 530.;
  ranges[5][1][0] = 520.;  ranges[5][1][1] = 900.;
  ranges[5][2][0] = 800.;  ranges[5][2][1] = 3000.;

  // ranges[6][0][0] = 460.;  ranges[6][0][1] = 550.;	// p-p
  // ranges[6][1][0] = 550.;  ranges[6][1][1] = 1000.;	// p-p
  // ranges[6][2][0] = 600.;  ranges[6][2][1] = 800.;	// p-p
  ranges[6][0][0] = 460.;  ranges[6][0][1] = 530.;
  ranges[6][1][0] = 520.;  ranges[6][1][1] = 700.;
  ranges[6][2][0] = 660.;  ranges[6][2][1] = 2900.;
  
  ranges[7][0][0] = 460.;  ranges[7][0][1] = 650.;
  // ranges[7][1][0] = 530.;  ranges[7][1][1] = 1000.;	// p-p value
  // ranges[7][2][0] = 700.;  ranges[7][2][1] = 1000.;	// p-p value
  ranges[7][1][0] = 600.;  ranges[7][1][1] = 1200.;
  ranges[7][2][0] = 1100.; ranges[7][2][1] = 2000.;

  
  TF1 *fits[8][2][3];		// [ich][itdc][ifitregion]
  const char *fitname[3]={ fit1.Data(), fit2.Data(), fit3.Data() };
  
  for(int ich=0; ich<8; ich++)
    for(int itdc=0; itdc<2; itdc++)
      for(int irg=0; irg<3; irg++)
	{
	  char tname[32];
	  sprintf(tname, "fit_ch%d_tdc%d_rg%d",ich,itdc,irg);
	  fits[ich][itdc][irg] = new TF1(tname, fitname[irg], ranges[ich][irg][0], ranges[ich][irg][1]);
	}
 
  for(int ich=0; ich<8; ich++)
    for(int itdc=0; itdc<2; itdc++)
      {
	fits[ich][itdc][0]->SetParameters(1.2,49.,411.,-15.,2.6e-4);
	fits[ich][itdc][1]->SetParameters(1,1,1,1);
	fits[ich][itdc][2]->SetParameters(1,1,1,1);
	fits[ich][itdc][0]->SetRange(ranges[ich][0][0],ranges[ich][0][1]);
	fits[ich][itdc][1]->SetRange(ranges[ich][1][0],ranges[ich][1][1]);
	fits[ich][itdc][2]->SetRange(ranges[ich][2][0],ranges[ich][2][1]);
      }

  /* Fit & Draw */

  //TCanvas *canvas[2];	// [itdc]
  TCanvas *cc[8][2];
  TString name;

  for (int itdc=0; itdc<2; itdc++)
    {
      name = "zslewtdc"; name += itdc; name += "Canvas";
      //     canvas[itdc] = new TCanvas(name,"slew canvas",425,550);
      //     canvas[itdc]->Divide(2,4);
  
      for (int ich=0; ich<8; ich++)
        {

	  //if( ich!=4)
	  //  continue;
	  //	  canvas[itdc]->cd(ich+1);
	  
	  tdcprof[ich][itdc]->Rebin(10);
	  double range[2]={ranges[ich][0][0]-100, ranges[ich][2][1]+100};
	  //tdcprof[ich][itdc]->SetBins(8, range);
	  

	  //	  tdcprof[ich][itdc]->Draw();

	  //	  tdc[ich][itdc]->Draw("same");

	  cc[ich][itdc]= new TCanvas();
	  cc[ich][itdc]->cd();
	  tdcprof[ich][itdc]->Draw();
  
	  for (int ifit=0; ifit<3; ifit++)
            {
              fits[ich][itdc][ifit]->SetLineColor(ifit+3);

	      //-** set the parameters on last fit to the same as fit 2
	      if ( ifit==2 )
	        {
		  Double_t *par = fits[ich][itdc][1]->GetParameters();
		  fits[ich][itdc][ifit]->SetParameters( par );
		}

	      tdcprof[ich][itdc]->Fit(fits[ich][itdc][ifit],"+RQ","same");

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

  
  /* Dump results to calibration file */
  
  Double_t slewval[8][2][4096];

  Int_t fitfcn[8][2];
  for (int ich=0; ich<8; ich++)
    for (int itdc=0; itdc<2; itdc++)
      fitfcn[ich][itdc] = 0;
  
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
                  int midpoint = ( ranges[ ich ][ fitfcn[ich][itdc] ][ 1 ] + ranges[ ich ][ fitfcn[ich][itdc]+1 ][ 0 ] )/2.0;
                  if ( iadc == midpoint )
		    fitfcn[ich][itdc]++;
		}
              slewval[ich][itdc][iadc] = fits[ich][itdc][ fitfcn[ich][itdc] ]->Eval( iadc );
	      outfile << std::setw(10) << setiosflags(ios::fixed|ios::right)
		      << setprecision(3)
		      << fits[ich][itdc][ fitfcn[ich][itdc] ]->Eval( iadc ) << " ";

	      // outfile << fits[ich][itdc][ fitfcn[ich][itdc] ]->Eval( iadc ) << " ";

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
        ztdiff[ich][itdc] = get_tdiff( tdc[ich][itdc], slewval[ich][itdc] );
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
      //    canvas[itdc]->Write();
      for (int ich=0; ich<8; ich++)
        {
          tdcprof[ich][itdc]->Write();
          tdc[ich][itdc]->Write();
          ztdiff[ich][itdc]->Write();
	}
    }
  resultsfile.Write();
  resultsfile.Close();
 
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

