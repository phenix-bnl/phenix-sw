// plot results from led runs
void plotled(const char *fname = "mpcled.root")
{
  gROOT->Reset();
  gSystem->Load("libnanoDST.so");
  gSystem->Load("libtrigger.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libmpc.so");

  gStyle->SetOptStat(0);

  gSystem->Load("libnanoDST.so");
  gSystem->Load("libtrigger.so");
  gSystem->Load("libmpc.so");

  MpcMap *mpcmap = MpcMap::instance();

  TFile *infile = new TFile(fname,"READ");

  TNtuple *mpc = (TNtuple*)infile->Get("mpc");

  const int MAXCH = 576;
  TH2F *adcvsch = new TH2F("adcvsch","ADC vs FEE CH",MAXCH,0,MAXCH,4200,-100,4100);
  adcvsch->SetMarkerColor(2);
  adcvsch->SetMarkerSize(0.3);
  adcvsch->SetMarkerStyle(7);
  TH2F *tdcvsch = new TH2F("tdcvsch","TDC vs FEE CH",MAXCH,0,MAXCH,4200,-100,4100);
  tdcvsch->SetMarkerColor(4);
  tdcvsch->SetMarkerSize(0.3);
  tdcvsch->SetMarkerStyle(7);

  // empty hists
  MpcMap *mpcmap = MpcMap::instance();
  TH1F *hemptyfem = new TH1F("hemptyfem","",MAXCH,0,MAXCH);
  for (int ifee576ch = 0; ifee576ch < 576; ifee576ch++)
    {
      if ( mpcmap->getGridX(ifee576ch)<0 )
        {
          hemptyfem->Fill(ifee576ch,4000.);
        }
    }
  hemptyfem->SetFillColor(5);
  //hemptyfem->SetFillStyle(4100);



  // Make ADC Histogram
  mpc->Draw("lopost-lopre:ch>>adcvsch");

  // Make TDC Histogram
  mpc->SetMarkerColor(4);
  mpc->Draw("tdc:ch>>tdcvsch");

  // Now Draw them
  hemptyfem->Draw();
  adcvsch->Draw("same");
  tdcvsch->Draw("same");

  TString name;
  TH1D *ledadc[MAXCH] = {0};
  Double_t ledmean[MAXCH] = {0.};

  int num_south_good = 0;
  int num_north_good = 0;

  TH2F *s_mpc_display = new TH2F("s_mpc_display","South LED",18,-0.5,17.5,18,-0.5,17.5);
  TH2F *n_mpc_display = new TH2F("n_mpc_display","North LED",18,-0.5,17.5,18,-0.5,17.5);

  for (int ifee576ch=0; ifee576ch<MAXCH; ifee576ch++)
    {
      name = "ledadc_"; name += ifee576ch;
      ledadc[ifee576ch] = adcvsch->ProjectionY(name,ifee576ch+1,ifee576ch+1);
      ledmean[ifee576ch] = ledadc[ifee576ch]->GetMean();

      int posx = mpcmap->getGridX(ifee576ch);
      int posy = mpcmap->getGridY(ifee576ch);

      if ( ledmean[ifee576ch]>50 )
        {
          if ( ifee576ch<288 ) ++num_south_good;
          else                 ++num_north_good;

          //cout << ifee576ch << endl;
	  if ( ledmean[ifee576ch]/100. > 25 )
            {
	      cout << ifee576ch << " " << ledmean[ifee576ch] << " x:" << posx << " y:" << posy << endl;
            }

          if ( posx<-3 ) { posx = -posx; posy = -posy; }	// pin channels

          if ( ifee576ch<288 ) s_mpc_display->Fill(posx,posy,ledmean[ifee576ch]/100.);
          else                 n_mpc_display->Fill(posx,posy,ledmean[ifee576ch]/100.);

        }
      else if ( mpcmap->getGridX(ifee576ch)>=0 ) // print out bad towers
        {
	  cout << "Low " << ifee576ch << " " << ledmean[ifee576ch] << " x:" << posx << " y:" << posy
               << " asic: " << mpcmap->getAsic(ifee576ch) << " ch: " << ifee576ch%24
               << " driver: " << mpcmap->getDriver(ifee576ch)
               << endl;
        }
    }

  TCanvas *s_mpc_display_canvas = new TCanvas("s_mpc_display_canvas","MPC LED",400,400);
  if ( s_mpc_display->GetEntries()>0 )
    {
      gStyle->SetPalette(1);
      s_mpc_display->Draw("colz");
    }
  TCanvas *n_mpc_display_canvas = new TCanvas("n_mpc_display_canvas","MPC LED",400,400);
  if ( n_mpc_display->GetEntries()>0 )
    {
      gStyle->SetPalette(1);
      n_mpc_display->Draw("colz");
    }

  cout << "number good south channels (%)\t" << num_south_good << "\t" << num_south_good/200. << endl;
  cout << "number good north channels (%)\t" << num_north_good << "\t" << num_north_good/226. << endl;

}

