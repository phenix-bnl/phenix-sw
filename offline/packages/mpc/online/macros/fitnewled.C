//
// Study fit of LED pulses using new electronics
// To use: first run fitnewled("dst.root",1);	// where 1=blue,0=red
//
//         then on the fitnewled.root file, run dosplines();
//
//
#include <iomanip.h>

static const int NCH = 576;
float _ped[NCH];
float _pedrms[NCH];

void fitnewled(const char *fname = "DST_MPC_run12pp_mpc-0000360320-0000.root", const int blue_or_red = 1)
{
  gSystem->Load("libmpc.so");

  gStyle->SetOptStat(0);
  gStyle->SetPalette(1);

  int verbose = 0;

  const int NSAMPLES = 12;

  MpcMap *mpcmap = MpcMap::instance();

  TFile *infile = new TFile(fname,"READ");

  TFile *savefile = new TFile("fitnewled.root","RECREATE");

  //Float_t e1sum = 0;
  TH1D *h_adc[NCH]={0};
  TH1D *h_pulse[NCH]={0};
  TH2D *h2_pulse[NCH]={0};

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

      //cout << "size\t" << mpcsamples->size() << endl;
      int nsamples = mpcsamples->size();
      if ( nsamples==0 ) continue;

      cout << "event " << ievt << endl;

      unsigned int trigscaled = trigl1->get_lvl1_trigscaled();
      if ( blue_or_red==1 && (trigscaled&0x40000000)!=0x40000000 ) continue;		// BLUE LED
      if ( blue_or_red==0 && (trigscaled&0x20000000)!=0x20000000 ) continue;		// RED LED

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
          h2_pulse[feech]->Fill( samp, adc );

          h_adc[feech]->Fill( adc );
          h2_adc->Fill( feech, adc );

          if ( adc>2065 ) hival = 1;
          if ( adc>ch_maxadc ) ch_maxadc = adc;
          if ( adc<ch_minadc ) ch_minadc = adc;

          if ( samp == 0 && verbose ) cout << feech << "\t";
          if ( verbose ) cout << setw(6) << adc;
          if ( samp==11 )
            {
              if ( verbose ) cout << "\t" << ch_maxadc-ch_minadc;
              ch_maxadc = -1;
              ch_minadc = 9999;
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
          h2_pulse[ifeech]->SetMaximum(2200);
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

Double_t fitf(Double_t *x, Double_t *par)
{
/*
  // Landau
  Double_t arg = 0;
  if (par[2] != 0) arg = (v[0] - par[1])/par[2];
  Double_t fitval = par[0]*TMath::Landau(-0.5*arg*arg));
*/

/*
  // Weibull distribution P(x;a,b) = a*b*(ax)^{b-1}*exp^{-(ax)^b}
  Double_t xx = x[0] - par[2];
  Double_t val = par[0]+par[1]*par[3]*par[4]*pow(par[3]*xx,par[4]-1.0)*exp(-1.0*pow(par[3]*xx,par[4]));
*/

/*
  // SharpGaussian
  Double_t xx = x[0]-par[2];
  Double_t val = par[0]+par[1]*exp(-pow(xx,par[4])/(2.0*par[3]*par[3]));
*/

/*
  // standard shape
  Double_t xx = x[0]-par[2];
  Double_t val = 0.;
  val = par[0]+par[1]*pow(xx,par[3])*exp(-par[4]*pow(xx,[5]));
  //if ( xx>0. ) par[0]+par[1]*pow(xx,par[3])*exp(-par[4]*xx);
  //else         par[0]+par[1]*pow(xx,par[3]);
*/

  // Gaus + pol3
  //Double_t xx = x[0] - par[2];
  Double_t xx = x[0];
  Double_t val = par[0]+par[1]*TMath::Gaus(x[0],par[2],par[3],kFALSE);
/*
                 + par[13]
                 + par[4]*xx + par[5]*xx*xx + par[6]*xx*xx*xx
                 + par[7]*pow(xx,4.)
                 + par[8]*pow(xx,5.)
                 + par[9]*pow(xx,6.)
                 + par[10]*pow(xx,7.)
                 + par[11]*pow(xx,8.)
                 + par[12]*pow(xx,9.);
*/

/*
  // Poln
  Double_t xx = par[11]*x[0]-par[10];
  Double_t val = par[0]+par[1]*xx+par[2]*xx*xx+par[3]*xx*xx*xx
                 + par[4]*pow(xx,4.)
                 + par[5]*pow(xx,5.)
                 + par[6]*pow(xx,6.)
                 + par[7]*pow(xx,7.)
                 + par[8]*pow(xx,8.)
                 + par[9]*pow(xx,9.);
*/

/*
  // GammaDist
  Double_t xx = x[0] - par[2];
  Double_t val = par[0]+par[1]*TMath::GammaDist(xx,par[3],0.,par[4]);
                 //+ par[4]*xx + par[5]*xx*xx + par[6]*xx*xx*xx;
*/

/*
  // Vavilov
  Double_t xx = x[0] - par[2];
  Double_t val = par[0]+par[1]*TMath::Vavilov(xx,par[3],par[4]);
                 //+ par[4]*xx + par[5]*xx*xx + par[6]*xx*xx*xx;
*/

/*
  // pol9
  Double_t xx = x[0] - par[10];
  Double_t val = par[0]+par[1]*xx + par[2]*xx*xx + par[3]*xx*xx*xx
                 +par[4]*pow(xx,4)
                 +par[5]*pow(xx,5)
                 +par[6]*pow(xx,6)
                 +par[7]*pow(xx,7)
                 +par[8]*pow(xx,8)
                 +par[9]*pow(xx,9);
*/

  return val;
}

Double_t fitres(Double_t *x, Double_t *par)
{
  // pol9
  Double_t xx = x[0];
  Double_t val = par[0] + par[1]*xx + par[2]*xx*xx + par[3]*xx*xx*xx
                 + par[4]*pow(xx,4)
                 + par[5]*pow(xx,5)
                 + par[6]*pow(xx,6)
                 + par[7]*pow(xx,7)
                 + par[8]*pow(xx,8)
                 + par[9]*pow(xx,9);
  return val;
}

Double_t fitfull(Double_t *x, Double_t *par)
{
  Double_t xx = x[0];
  Double_t val = fitf(x,par) + fitres(x,&par[4]);
/*
par[0]+par[1]*TMath::Gaus(x[0],par[2],par[3],kFALSE)
                 + par[4] + par[5]*xx + par[6]*xx*xx + par[7]*xx*xx*xx
                 + par[8]*pow(xx,4)
                 + par[9]*pow(xx,5)
                 + par[10]*pow(xx,6)
                 + par[11]*pow(xx,7)
                 + par[12]*pow(xx,8)
                 + par[13]*pow(xx,9);
*/

  return val;
}

Double_t fitfbkg(Double_t *x, Double_t *par)
{
  // Gaus + pol3
  Double_t xx = x[0] - par[4];
  Double_t val = par[0] + par[1]*xx + par[2]*xx*xx + par[3]*xx*xx*xx;

  return val;
}

void readped(const char *fname = "xxx.ped")
{
  ifstream pedfile(fname);
  int feech;
  float pederr, pedrmserr;
  while ( pedfile >> feech )
    {
      pedfile >> _ped[feech] >> pederr >> _pedrms[feech] >> pedrmserr;
      cout << feech << "\t" << _ped[feech] << "\t" << _pedrms[feech] << endl;
    }
  pedfile.close();
}

void dospline(const char *fname = "fitnewled.root")
{
  gSystem->Load("libmpc.so");

  MpcMap *mpcmap = MpcMap::instance();
  readped("/data2/phnxmpc/run12startup/mpcped/run_0000366000_0000367000/DST_MPC/366616.ped");

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

  ofstream outf("led_splines.C");

  for (int ich=0; ich<NCH; ich++)
    {
      if ( h2_pulse[ich]->GetEntries() == 0 ) continue;
      if ( mpcmap->getGridX(ich)<-17 ) continue;

      h2_profile[ich] = h2_pulse[ich]->ProfileX();
      h2_profile[ich]->SetLineColor(2);
      h2_profile[ich]->SetMarkerColor(2);

      Int_t nbinsx = h2_profile[ich]->GetNbinsX();
      g_pulse[ich] = new TGraphErrors();
      Double_t ped = 0.;
      Double_t max = -9999.;
      for (int ibin=1; ibin<=nbinsx; ibin++)
        {
          Double_t x = h2_profile[ich]->GetBinCenter(ibin);
          Double_t val = h2_profile[ich]->GetBinContent(ibin);
          Double_t err = h2_profile[ich]->GetBinError(ibin);
          g_pulse[ich]->SetPoint(ibin-1,x,val);
          g_pulse[ich]->SetPointError(ibin-1,0,err);

          if ( val>max ) max = val;

        }
      // Now that we found the max, save to file
      for (int ibin=1; ibin<=nbinsx; ibin++)
        {
          Double_t val = h2_profile[ich]->GetBinContent(ibin);
          //if ( ibin==1 ) ped = val;
          ped = _ped[ich];
          outf << "  blue_spline[" << ich << "][" << ibin-1 << "] = "
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

      //cin >> junk;
      if ( junk[0] == 'q' ) return;
    }
  outf.close();
}

void dofit(const char *fname = "fitnewled.root")
{
  gSystem->Load("libmpc.so");

  TFile *infile = new TFile(fname,"READ");

  TH2 *h2_pulse[576] = {0};
  TProfile *h2_profile[576] = {0};
  TGraphErrors *g_residuals[576] = {0};
  TGraphErrors *g_pulse[576] = {0};
  TString name;
  for (int ich=0; ich<576; ich++)
    {
      name = "h2_"; name += ich;
      h2_pulse[ich] = (TH2*)infile->Get( name );
    }

  TCanvas *ac = new TCanvas("ac","fits",800,800);
  ac->Divide(1,2);
  string junk;

  //TF1 *fit = new TF1("fit","pol0+gaus(1)",0,12);
  //fit->SetParameters(2045,200,7,4);

  //TF1 *fit = new TF1("fit","[0]+[1]*landau(2)",0,30);
  //fit->SetParameters(2048,100,7,1);

  TF1 *fit = new TF1("fit",fitf,-0.5,12.6,4);
  //fit->SetParameters(2048,100,7,0.1,2);	// weibull
  //fit->SetParameters(2048,100,7,3);		// pol0+gaus
  //fit->SetParameters(2048,100,1,1,1,1);	// ??

  //TF1 *fit = new TF1("fit",fitf,-0.5,16,7);
  //TF1 *fit = new TF1("fit","[0]+gaus(1)+pol3(4)",0,16);
  //fit->SetParameters(2048,1,1,1,1,1,1,1,1);
  //fit->SetParameters(2048,100,7,3,1,1,1,1,1);

  TF1 *fres = new TF1("fres",fitres,-0.5,12.6,10);
  TF1 *ffull = new TF1("ffull",fitfull,-0.5,12.6,10);
  ffull->SetLineColor(4);

  //TF1 *bkg = new TF1("bkg","[0]+pol3(1)",-1,16);
  TF1 *bkg = new TF1("bkg",fitfbkg,-0.5,16,7);
  bkg->SetLineColor(2);


  for (int ich=0; ich<NCH; ich++)
    {
      if ( h2_pulse[ich]->GetEntries() == 0 ) continue;
      if ( ich<13 ) continue;

      h2_profile[ich] = h2_pulse[ich]->ProfileX();
      h2_profile[ich]->SetLineColor(2);
      h2_profile[ich]->SetMarkerColor(2);


      Int_t nbinsx = h2_profile[ich]->GetNbinsX();
      Double_t ped = h2_profile[ich]->GetBinContent(2);
      Double_t pederr = h2_profile[ich]->GetBinError(2);
cout << "ped " << ich << "\t" << ped << endl;
cout << "err " << ich << "\t" << pederr << endl;
/*
      for (int ibin=1; ibin<=nbinsx; ibin++)
        {
          Double_t val = h2_profile[ich]->GetBinContent(ibin);
          Double_t err = h2_profile[ich]->GetBinError(ibin);
cout << ich << "\t" << ibin << "\t" << val << "\t" << err << endl;
          h2_profile[ich]->SetBinContent(ibin,val);
          h2_profile[ich]->SetBinError(ibin,err);
        }
*/
      ac->cd(1);
      h2_pulse[ich]->Draw("colz");
      //h2_profile[ich]->Draw("same");
      h2_profile[ich]->SetMinimum(2000);
      h2_profile[ich]->Draw();
      //fit->SetRange(0,12);
      fit->SetParameters(ped,100,7,4);		// pol0+gaus
      //fit->SetParameters(ped,100,7,3.,2);		// pol0+sharpgaus
      //fit->SetParameters(ped,100,0.,15.,2.);		// pol0+pmtpulse

      //fit->SetParameters(ped,100,7,4);		// pol0+gaus+pol9
      fit->FixParameter(0,ped);
/*
      fit->SetParameter(1,1);
      fit->SetParameter(2,1);
      fit->SetParameter(3,-3);
      fit->SetParameter(4,2);
      fit->SetParameter(5,-1.);
      fit->SetParameter(6,0.1);
      fit->SetParameter(7,-1e-2);
      fit->SetParameter(8,5e-4);
      fit->SetParameter(9,-1e6);
      fit->FixParameter(10,8.5);
      fit->SetParameter(11,1.0);
*/

/*
      fit->SetParameter(10,-0.14);
      fit->SetParameter(11,6e-4);
      fit->SetParameter(12,-1e-5);
      fit->SetParameter(13,-1.9);
*/
      //fit->SetParameters(ped,100,0,9,1.4);		// pol0+gammadist
      //fit->SetParameters(ped,100,7,0.1,4.5);		// pol0+Weibull
      //fit->SetParameters(ped,0,100,0,0,0,0,0,0,0,0);	// gaus+pol9
      //fit->SetParameter(10,7);
      //fit->SetParameters(ped,60,6.5,2.5,0,0,0,0,0);
      //fit->SetParLimits(2,4,10);
      //fit->FixParameter(0,ped);
      //fit->FixParameter(2,0);
      //fit->FixParameter(4,0.);
      h2_profile[ich]->Fit( fit, "RB" );

      // Get Residuals
      Int_t npoints = 0;
      Double_t x[100];
      Double_t residuals[100];
      Double_t res_err[100];
      for (int ibin=1; ibin<=12; ibin++)
        {
          x[npoints] = h2_profile[ich]->GetBinCenter(ibin);
          Double_t val = fit->Eval(x[npoints]);
          residuals[npoints] = h2_profile[ich]->GetBinContent(ibin) - val;
          res_err[npoints] = h2_profile[ich]->GetBinError(ibin);
          npoints++;
        }
      g_residuals[ich] = new TGraphErrors(npoints,x,residuals,0,res_err);
      name = "g_residual"; name += ich;
      g_residuals[ich]->SetName(name);
      g_residuals[ich]->SetTitle(name);
      g_residuals[ich]->SetMarkerStyle(20);
      g_residuals[ich]->SetMarkerSize(0.6);
      ac->cd(2);
      g_residuals[ich]->Draw("ap");

      fres->SetParameter(0,-1.2);
      fres->SetParameter(1,-1.8);
      fres->SetParameter(2,4.9);
      fres->SetParameter(3,-6.7);
      fres->SetParameter(4,3.9);
      fres->SetParameter(5,-1.1);
      fres->SetParameter(6,0.17);
      fres->SetParameter(7,-0.01);
      fres->SetParameter(8,6e-4);
      fres->SetParameter(9,-1.2);

      g_residuals[ich]->Fit(fres);

      for (int ipar=0; ipar<4; ipar++);
        {
          ffull->SetParameter(ipar,fit->GetParameter(ipar));
        }
      for (int ipar=0; ipar<10; ipar++);
        {
          ffull->SetParameter(ipar+4,fres->GetParameter(ipar));
        }
      
      ac->cd(1);
      ffull->Draw("same");

      bkg->SetParameter(0,fit->GetParameter(0));
      bkg->SetParameter(1,fit->GetParameter(4));
      bkg->SetParameter(2,fit->GetParameter(5));
      bkg->SetParameter(3,fit->GetParameter(6));
      bkg->SetParameter(4,fit->GetParameter(2));
/*
      for (int ipar=4; ipar<8; ipar++)
        {
          Double_t parval = fit->GetParameter(ipar);
          bkg->SetParameter(ipar-3,parval);
        }
*/
      //bkg->Draw("same");

      gPad->Modified();
      gPad->Update();

      cin >> junk;
      if ( junk[0] == 'q' )
        {
          return;
        }
    }

}



