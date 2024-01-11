//
// Make a bootstrapped fit of real pulses using new electronics
//
// Produces photon_splices.C which contains normalized photon waveforms
//
// Doesn't select photon showers except by energy
//
#include <iomanip.h>

static const int NCH = 576;
float _ped[NCH];
float _pedrms[NCH];

void bootstrap_newpulse(const char *fname = "DST_MPC_run12pp_mpc-0000367925-0090.root")
{
  gSystem->Load("libmpc.so");

  gStyle->SetOptStat(0);
  gStyle->SetPalette(1);

  int verbose = 0;

  const int NSAMPLES = 12;

  MpcMap *mpcmap = MpcMap::instance();

  TFile *infile = new TFile(fname,"READ");

  TFile *savefile = new TFile("bootstrap_newpulse.root","RECREATE");

  //Float_t e1sum = 0;
  TH1D *h_adc[NCH]={0};
  TH1D *h_pulse[NCH]={0};
  TH2D *h2_pulse[NCH]={0};
  int nfilled[NCH] = {0};

  TString name;
  for (int ich = 0; ich < NCH; ich++)
    {
      name = "h_adc"; name += ich;
      h_adc[ich]=new TH1D(name, "adc distribution", 200, 1900.5, 2100.5);

      name = "h_"; name += ich;
      h_pulse[ich] = new TH1D(name,name,NSAMPLES,-0.5,NSAMPLES-0.5);
      h_pulse[ich]->SetMarkerStyle(20);
      h_pulse[ich]->SetMarkerSize(0.5);
      h_pulse[ich]->SetMarkerColor(4);

      name = "h2_"; name += ich;
      h2_pulse[ich] = new TH2D(name,name,NSAMPLES,-0.5,NSAMPLES-0.5,4095.5-1600.5,1600.5,4095.5);
    }
  h2_adc = new TH2D("h2_adc","pedestal vs channel",192,-0.5,191.5,200,1900.5,2100.5);

  mpcSampleContainer *mpcsamples = 0;
  TrigLvl1 *trigl1 = 0;
  TTree *T = (TTree*)infile->Get("T");
  int nbranch = 0;
  TBranch *branch[100] = {0};
  branch[nbranch] = T->GetBranch("DST/TrigLvl1");
  branch[nbranch++]->SetAddress(&trigl1);
  branch[nbranch] = T->GetBranch("DST/mpcSampleContainer");
  branch[nbranch++]->SetAddress(&mpcsamples);

  string junk;

  Stat_t nentries = T->GetEntries();
  cout << "num entries " << nentries << endl;

  TCanvas *c_display[2] = {0};
  c_display[0] = new TCanvas("c_display0","MPC.S",800,800);
  //c_display[0]->Divide(18,18,-1,-1);
  c_display[1] = new TCanvas("c_display1","MPC.N",800,800);

  c_display[0]->Divide(18,18,0.0001,0.0001);
  c_display[1]->Divide(18,18,0.0001,0.0001);
  //c_display[0]->Divide(18,18,-1,-1);
  //c_display[1]->Divide(18,18,-1,-1);

  // Loop over events
  int max_adc = 0;	// max adc found in event (for triggering)
  for (int ievt = 0; ievt<nentries; ievt++)
    {
      T->GetEvent(ievt);

      for (int ifeech=0; ifeech<NCH; ifeech++)
        {
          h_pulse[ifeech]->Reset();
        }

      max_adc = 0;
      int ch_minadc = 9999;
      int ch_maxadc = -1;
      int ch_maxsamp = -1;

      cout << "size\t" << mpcsamples->size() << endl;
      int nsamples = mpcsamples->size();
      if ( nsamples==0 ) continue;

      cout << "event " << ievt << "\t" << nsamples << endl;

      unsigned int trigscaled = trigl1->get_lvl1_trigscaled();
      if ( (trigscaled&0x2000)!=0x2000 ) continue;	// MPC_A
      //if ( (trigscaled&0x1000)!=0x1000 ) continue;		// MPC_B
      //if ( (trigscaled&0x2)!=0x2 ) continue;		// MB
      //if ( (trigscaled&0x40000000)!=0x40000000 ) continue;		// BLUE LED
      //if ( (trigscaled&0x20000000)!=0x20000000 ) continue;		// RED LED

      unsigned int rbits_word = trigl1->get_lvl1_rbits(2);
      //if ( (rbits_word&rbitmask[77]) == rbitmask[77] ) rbit_fired = 1;	// MPC_B?
      cout << hex << rbits_word << dec << endl;

      int arm = 0;
      int hival = 0;
      for (int isamp=0; isamp<nsamples; isamp++)
        {
          mpcSample *adcsamp = mpcsamples->GetSample(isamp);
          if ( adcsamp==0 ) continue;
 
          int adc = adcsamp->get_adc();      // adc
          int feech = adcsamp->get_ch();     // fee576ch
          int samp = adcsamp->get_sample();  // sample number
 	      
          if ( adc > max_adc ) max_adc = adc;

          h_pulse[feech]->Fill( samp, adc );
          h_pulse[feech]->SetBinError( samp+1, 2.0 );

          h_adc[feech]->Fill( adc );
          h2_adc->Fill( feech, adc );

          if ( adc>2100 ) hival = 1;
          if ( adc>ch_maxadc )
            {
              ch_maxadc = adc;
              ch_maxsamp = samp;
            }
          if ( adc<ch_minadc )
            {
              ch_minadc = adc;
            }

          if ( samp == 0 && verbose ) cout << feech << "\t";
          if ( verbose ) cout << setw(6) << adc;
          if ( samp==11 )
            {
              if ( verbose ) cout << "\t" << ch_maxadc-ch_minadc;
              if ( ch_maxadc>2100&&ch_maxadc<2150&&ch_maxsamp>5&&ch_maxsamp<9 && nfilled[feech]==0 )
                {
                  int nbinsx = h_pulse[feech]->GetNbinsX();
                  for (int ibin=1; ibin<=nbinsx; ibin++)
                    {
                      float sample = h_pulse[feech]->GetBinCenter(ibin);
                      float temp_adc = h_pulse[feech]->GetBinContent(ibin);
                      h2_pulse[feech]->Fill( sample, temp_adc );
                    }
                  ++nfilled[feech];
                }
              ch_maxadc = -1;
              ch_minadc = 9999;
              ch_maxsamp = -1;
              if ( hival==1 && verbose ) cout << "\txxx";
              hival = 0;
            }
          if ( samp == 11 && verbose ) cout << endl;
        }

      continue;
      //if ( max_adc < 2100 ) continue;
                                          
      //for (int ifeech=0; ifeech<NCH; ifeech++)
      for (int ifeech=13; ifeech<14; ifeech++)
        {
          arm = mpcmap->getArm(ifeech);
          int gridx = mpcmap->getGridX(ifeech);
          int gridy = mpcmap->getGridY(ifeech);

          if ( gridx<-17 ) continue;
          if ( gridx<0 )	// pin diodes
            {
              gridx = -gridx;
              gridy = -gridy;
            }

          //cout << ifeech << "\t" << (17-gridy)*18 + gridx + 1 << endl;
          //c_display[arm]->cd( (17-gridy)*18 + gridx + 1 );
          h_pulse[ifeech]->DrawCopy("e");

         arm = 0;
	 c_display[arm]->Modified();
	 c_display[arm]->Update();
	  
         if ( 1 )
           {
             arm = 0;
             name = "event_"; name += ievt; name += ".png";
             //cout << name << endl;
             //c_display[arm]->SaveAs(name);
           }

	 cin >> junk;
	 if ( junk[0] == 'q' ) 
	   {
	     //gSystem->Exit(0);
	     return;
	   }
       }
 
   }

  for (int ifeech=0; ifeech<NCH; ifeech++)
    {
      arm = mpcmap->getArm(ifeech);
      int gridx = mpcmap->getGridX(ifeech);
      int gridy = mpcmap->getGridY(ifeech);

      if ( gridx<-17 ) continue;
      if ( gridx<0 )	// pin diodes
        {
          gridx = -gridx;
          gridy = -gridy;
        }
      else
        {
          h2_pulse[ifeech]->GetYaxis()->SetRangeUser(2040,2160);
        }

      //cout << ifeech << "\t" << (17-gridy)*18 + gridx + 1 << endl;
      c_display[arm]->cd( (17-gridy)*18 + gridx + 1 );
      h2_pulse[ifeech]->Draw("colz");
      //h2_pulse[ifeech]->Draw();
    }

  arm = 0;
  c_display[0]->Modified();
  c_display[0]->Update();
  c_display[1]->Modified();
  c_display[1]->Update();

  //h2_pulse[13]->Draw("colz");
  savefile->Write();
  //savefile->Close();
}

void readped(const char *fname = "xxx.ped")
{
  ifstream pedfile(fname);
  int feech;
  float pederr, pedrmserr;
  int   ncounts;
  while ( pedfile >> feech )
    {
      pedfile >> _ped[feech] >> _pedrms[feech] >> ncounts;
      cout << feech << "\t" << _ped[feech] << "\t" << _pedrms[feech] << endl;
    }
  pedfile.close();
}

void dospline(const char *fname = "fitnewpulse.root")
{
  gSystem->Load("libmpc.so");

  MpcMap *mpcmap = MpcMap::instance();
  readped("/home/phnxmpc/database/Run12pp/368527.ped");

  TFile *infile = new TFile(fname,"READ");

  TH2 *h2_pulse[576] = {0};
  TProfile *h2_profile[576] = {0};
  TGraphErrors *g_residuals[576] = {0};
  TGraphErrors *g_pulse[576] = {0};
  TGraph *g_smooth[576] = {0};
  TString name;
  for (int ich=0; ich<576; ich++)
    {
      name = "h2_"; name += ich;
      h2_pulse[ich] = (TH2*)infile->Get( name );
    }

  string junk;
  TCanvas *ac = new TCanvas("ac","fits",800,800);

  TGraphSmooth *gs = new TGraphSmooth("normal");
  //TSpline3 *spline3 = new TSpline3();
  TSpline3 *spline3;

  ofstream outf("photon_splines.C");

  for (int ich=0; ich<NCH; ich++)
    {
      if ( h2_pulse[ich]->GetEntries() == 0 ) continue;
      if ( mpcmap->getGridX(ich)<0 ) continue;

      h2_profile[ich] = h2_pulse[ich]->ProfileX();
      h2_profile[ich]->SetLineColor(2);
      h2_profile[ich]->SetMarkerColor(2);

      Int_t nbinsx = h2_profile[ich]->GetNbinsX();
      g_pulse[ich] = new TGraphErrors();
      Double_t ped = _ped[ich];
      Double_t max = -9999.;
      for (int ibin=1; ibin<=nbinsx; ibin++)
        {
          Double_t x = h2_profile[ich]->GetBinCenter(ibin);
          Double_t val = h2_profile[ich]->GetBinContent(ibin);
          Double_t err = h2_profile[ich]->GetBinError(ibin);
          if ( ibin<=3 ) g_pulse[ich]->SetPoint(ibin-1,x,0.);
          else          g_pulse[ich]->SetPoint(ibin-1,x,val-ped);
          g_pulse[ich]->SetPointError(ibin-1,0,err);

          if ( val>max ) max = val;

        }
      // Now that we found the max, save to file
      for (int ibin=1; ibin<=nbinsx; ibin++)
        {
          Double_t val = h2_profile[ich]->GetBinContent(ibin);
          //if ( ibin==1 ) ped = val;
          outf << "  photon_spline[" << ich << "][" << ibin-1 << "] = "
               << (val-ped)/(max-ped) << ";" << endl;
        }

      g_pulse[ich]->SetMarkerColor(2);
      g_pulse[ich]->SetMarkerStyle(26);
      //g_pulse[ich]->Draw("apc");

      //cout << g_pulse[ich]->Eval(8.5,0,"S") << endl;

      TSpline3 *s = new TSpline3("grs",g_pulse[ich]);
      //cout << s->Eval(8.5) << endl;

      name = ""; name += ich;
      g_pulse[ich]->SetTitle(name);
      g_pulse[ich]->Draw("apc");
      s->SetLineColor(4);
      s->Draw("same");

/*
spline3->SetLineColor(4);
spline3->Draw("same");
*/

/*
      //g_smooth[ich] = gs->Approx(g_pulse[ich],"linear");
      //g_smooth[ich] = gs->SmoothSuper(g_pulse[ich],);
      g_smooth[ich] = gs->SmoothKern(g_pulse[ich]);
      g_smooth[ich]->SetMarkerStyle(20);
      g_smooth[ich]->Draw("LP");
*/

      gPad->Modified();
      gPad->Update();
      name = "p_"; name += ich; name += ".png";
      gPad->SaveAs(name);

      //cin >> junk;
      if ( junk[0] == 'q' ) return;
    }
  outf.close();
}

