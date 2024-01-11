#include <TPad.h>
#include <TStyle.h>
#include <TFile.h>
#include <TF1.h>
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TSystem.h>
#include <TGraphErrors.h>

#include <fstream>
#include <iostream>
#include <iomanip>

//#include <MpcMap.h>
//#include <recoConsts.h>
//
// plot the pi0 mass by towers
//

TGraph *g_tdiff[576];
//MpcMap *mpcmap;

void plotmpctimecal(const char *fname = "x.root")
{
/*
  gStyle->SetOptStat(0);

  gStyle->SetOptStat(0);
  gSystem->Load("libphflag.so");
*/
  gSystem->Load("libmpc.so");
  MpcMap *mpcmap = MpcMap::instance();

  // Instantiate graphs
  TString name;
  for (int ich=0; ich<576; ich++)
    {
      name = "g_tdiff"; name += ich;
      g_tdiff[ich] = new TGraph();
      g_tdiff[ich]->SetName( name );
    }

  TFile *tfile = new TFile(fname,"READ");
  TTree *t = (TTree*)tfile->Get("t");
  Int_t   f_ch;         // feech
  Float_t f_lo;         // lopost-lopre
  Float_t f_hi;         // hipost-hipre
  Float_t f_e;          // tower energy
  Float_t f_tdc;        // tdc
  Float_t f_tdc2ns;     // tdc 2 ns conversion
  Float_t f_t0offset;   // tdc offset
  Float_t f_bbcz;       // bbc zvertex, etc
  Float_t f_bbct0;
  Float_t f_bbctn;
  Float_t f_bbcts;
  TBranch *branch[100];
  int ibr = 0;
  branch[ibr] = t->GetBranch("ch");
  branch[ibr++]->SetAddress(&f_ch);
  branch[ibr] = t->GetBranch("lo");
  branch[ibr++]->SetAddress(&f_lo);
  branch[ibr] = t->GetBranch("hi");
  branch[ibr++]->SetAddress(&f_hi);
  branch[ibr] = t->GetBranch("e");
  branch[ibr++]->SetAddress(&f_e);
  branch[ibr] = t->GetBranch("tdc");
  branch[ibr++]->SetAddress(&f_tdc);
  branch[ibr] = t->GetBranch("tdc2ns");
  branch[ibr++]->SetAddress(&f_tdc2ns);
  branch[ibr] = t->GetBranch("t0offset");
  branch[ibr++]->SetAddress(&f_t0offset);
  branch[ibr] = t->GetBranch("bbcz");
  branch[ibr++]->SetAddress(&f_bbcz);
  branch[ibr] = t->GetBranch("bbct0");
  branch[ibr++]->SetAddress(&f_bbct0);
  branch[ibr] = t->GetBranch("bbctn");
  branch[ibr++]->SetAddress(&f_bbctn);
  branch[ibr] = t->GetBranch("bbcts");
  branch[ibr++]->SetAddress(&f_bbcts);

  int nentries = t->GetEntries();
  for (int ientry=0; ientry<nentries; ientry++)
    {
      t->GetEvent(ientry);
      //cout << ientry << "\t" << f_ch << "\t" << f_lo << endl;

      if ( f_tdc2ns < 0. ) f_tdc2ns = 0.048;
      int arm = 0;
      float bbctime = f_bbcts;
      if ( f_ch > 287 )
        {
          arm = 1;
          bbctime = f_bbctn;
        }

      // should fine tune for each channel
      if ( f_tdc>2400 ) continue;

      Int_t npoint = g_tdiff[f_ch]->GetN();
      //cout << f_ch << "\t" << npoint << endl;
      //g_tdiff[f_ch]->Set(npoint+1);
      g_tdiff[f_ch]->SetPoint(npoint,f_lo,f_tdc*f_tdc2ns-bbctime);
    }

/*
  //recoConsts *rc = recoConsts::instance();
  //rc->set_IntFlag("RUNNUMBER",run);

  MpcMap *mpcmap = MpcMap::instance();

  if ( mpcmap==0 ) return;

  TString name;
  TString outname;
  cout << "reading file " << name << endl;
  tfile[nrun] = new TFile(fname,"READ");

  // Get Histograms
  for (int ich=0; ich<576; ich++)
    {
      if ( mpcmap->isCrystal(ich)!=1 ) continue;
      int iarm = mpcmap->getArm(ich);
      int ix = mpcmap->getGridX(ich);
      int iy = mpcmap->getGridY(ich);
      name = "hlovshi"; name += ich;
      hlovshi[iarm][iy][ix] = (TH2*)tfile[nrun]->Get(name);
    }
*/

  // Draw on Canvas
  TCanvas *ctdiff[2];
  ctdiff[0] = new TCanvas("ctdiff0","tdiff arm 0",1000,1000);
  ctdiff[0]->Divide(18,18,0.0001,0.0001);
  ctdiff[1] = new TCanvas("ctdiff1","tdiff arm 1",1000,1000);
  ctdiff[1]->Divide(18,18,0.0001,0.0001);

/*
  TF1 *gaussian = new TF1("gaussian","gaus",0.,4096.);
  gaussian->SetLineColor(2);
  gaussian->SetLineWidth(0.5);
*/

  for (int ich=0; ich<576; ich++)
    {
      if ( mpcmap->isCrystal(ich) == 0 ) continue;
      int arm = mpcmap->getArm(ich);
      int ix  = mpcmap->getGridX(ich);
      int iy  = mpcmap->getGridY(ich);

cout << arm << "\t" << ich << "\t" << ix << "\t" << iy << "\t" << (17-iy)*18 + ix + 1 << endl;
      if ( g_tdiff[ich]->GetN()==0 )
        {
          cout << "No entries for " << ich << endl; 
          g_tdiff[ich]->SetPoint(0,0,0);
          //continue;
        }
      ctdiff[arm]->cd( (17-iy)*18 + ix + 1 ); 
      g_tdiff[ich]->Draw("ap");
    }

  return;

  //TString name;
  TH1D *hhilolimit[2][18][18];

  for (int iarm=0; iarm<2; iarm++)
    {
      for (int iy=0; iy<18; iy++)
        {
          for (int ix=0; ix<18; ix++)
            {
              //int feech = mpcmap->getFeeCh(ix,iy,0);
              //if ( feech < 0 ) continue;
              if ( hlovshi[iarm][iy][ix]==0 ) continue;
    
              chilolimit[iarm]->cd( (17-iy)*18 + ix + 1 ); 
    
              //hlovshi[iarm][iy][ix]->Draw();

              int nbinsy = hlovshi[iarm][iy][ix]->GetNbinsY();
              name = "hlimit_"; name += hlovshi[iarm][iy][ix]->GetName();
              hhilolimit[iarm][ix][iy] = hlovshi[iarm][iy][ix]->ProjectionX(name,nbinsy/2,nbinsy);
              if ( iarm==0 && iy==0 && ix==0 )
                {
                  gaussian->SetParameter(1,hhilolimit[iarm][ix][iy]->GetMean());
                  gaussian->SetParameter(2,hhilolimit[iarm][ix][iy]->GetRMS());
                }
              int nentries = hhilolimit[iarm][ix][iy]->GetEntries();
              //hhilolimit[iarm][ix][iy]->DrawCopy();
              hhilolimit[iarm][ix][iy]->Draw();
              if ( nentries>10 )
                {
                  hhilolimit[iarm][ix][iy]->Fit("gaussian","L");
                  gaussian->DrawCopy("same");
                  hilolimitmean[iarm][ix][iy] = gaussian->GetParameter(1);
                  hilolimitrms[iarm][ix][iy] = gaussian->GetParameter(2);
                  hilolimitstatus[iarm][ix][iy] = 0;
                }
              else
                {
                  // These have hi gain good  to 4095...
                  hilolimitmean[iarm][ix][iy] = 4000.;
                  hilolimitrms[iarm][ix][iy] = 0.;
                  hilolimitstatus[iarm][ix][iy] = 0;
                }

              //delete hhilolimit[iarm][ix][iy];
           }
        }
    }

  // Now save to web directory
/*
  outname = "webplots/slopecalib/het_";
  outname = "webplots/slopecalib/het_";
  outname += run;
  outname += ".gif";
  cout << outname << endl;

  chilo[0]->SaveAs( outname );
  outname.ReplaceAll("gif","ps");
  ac->SaveAs( outname );
*/
  chilolimit[0]->SaveAs( "chilolimit0.png" );
  chilolimit[1]->SaveAs( "chilolimit1.png" );

  chilolimit[0]->Delete();
  chilolimit[1]->Delete();

  for (int iarm=0; iarm<2; iarm++)
    {
      for (int iy=0; iy<18; iy++)
        {
          for (int ix=0; ix<18; ix++)
            {
              //int feech = mpcmap->getFeeCh(ix,iy,0);
              //if ( feech < 0 ) continue;
              if ( hlovshi[iarm][iy][ix]==0 ) continue;
              delete hhilolimit[iarm][ix][iy];
            }
        }
    }
    
}

//
// Fit the lo vs hi 2d plot
int myfitLinearRobust(TH2 *h2_lovshi, Double_t hiadc_min, Double_t hiadc_max, Double_t& mean, Double_t& rms)
{
cout << "In fitlinear" << endl;
  int minbin = h2_lovshi->GetXaxis()->FindBin( hiadc_min );
  int maxbin = h2_lovshi->GetXaxis()->FindBin( hiadc_max );

  //TH1 *hhiloratio[1];
  //hhiloratio[0] = new TH1D("hhiloratio0","",3000,0,30);;

  Int_t npoints = 0;
/*
  Double_t lo[1000000];
  Double_t hi[1000000];
  Double_t e[1000000];
*/
//  vector<Double_t> lo;
  Double_t *lo = new double[100000];
  Double_t *hi = new double[100000];
  Double_t *e = new double[100000];

  int nbinsy = h2_lovshi->GetYaxis()->GetNbins();
  for (int ixbin=minbin; ixbin<maxbin; ixbin++)
    {
      for (int iybin=1; iybin<nbinsy; iybin++)
        {
          Double_t ncounts = h2_lovshi->GetBinContent(ixbin,iybin);
          if ( ncounts<=0 ) continue;
          hi[npoints] = h2_lovshi->GetXaxis()->GetBinCenter(ixbin);
          lo[npoints] = h2_lovshi->GetYaxis()->GetBinCenter(iybin);
          e[npoints] = 1.0/sqrt(ncounts);

          //hhiloratio[0]->Fill( hi[npoints]/lo[npoints], ncounts );
          //if ( npoints>=1500000 )
          if ( npoints>=99999 )
            {
              cout << "EXCEEDING 1.6M points" << endl;
              continue;
            }

          npoints++;
        }
    }

  cout << "npoints " << npoints << endl;
  TGraphErrors grr(npoints, hi, lo, 0, e);
  TF1 *ffit1 = new TF1("ffit1", "[0]*x", 0, 4095);
  ffit1->SetLineColor(kBlue);
  grr.Fit(ffit1,"+rob=0.75");

  mean = ffit1->GetParameter(0);
  rms = ffit1->GetParError(0);

  delete ffit1;
  delete lo;
  delete hi;
  delete e;

cout << "Out of fitlinear" << endl;
  return 1;
}

void plothilo(const char *fname = "mpchilo.root")
{
/*
  gStyle->SetOptStat(0);

  gSystem->Load("libphflag.so");
  gSystem->Load("libmpc.so");
*/

  TFile *tfile[2] = {0};

  //TH2 *hlovshi[2][18][18] = {{{0}}};
  //TH1D *hhilolimit[2][18][18] = {{{0}}};

  // Reset hiloratio values
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ix=0; ix<18; ix++)
        {
          for (int iy=0; iy<18; iy++)
            {
              hiloratiomean[iarm][ix][iy] = -1.0;
              hiloratiorms[iarm][ix][iy] = -1.0;
              hiloratiostatus[iarm][ix][iy] = -1;
              hlovshi[iarm][iy][ix] = 0;
            }
        }
    }

  int nrun = 0;

  // Get Run Number
  TString name = fname;
  TString tempname = name;
  int index = tempname.Last('_');
  tempname.Remove(0,index+1);
  tempname.Remove(6,10000);
  int run = tempname.Atoi();
      
  cout << "run is " << run << "\t" << tempname << endl;
  cout << "run is " << run << endl;

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",run);

  mpcmap = MpcMap::instance();

  if ( mpcmap==0 ) return;

  TString outname;
  //name = ""; name += run; name += "/"; name += "mpcslope.root";
  cout << "reading file " << name << endl;
  tfile[nrun] = new TFile(name,"READ");

  for (int ich=0; ich<576; ich++)
    {
      if ( mpcmap->isCrystal(ich)!=1 ) continue;
      int iarm = mpcmap->getArm(ich);
      int ix = mpcmap->getGridX(ich);
      int iy = mpcmap->getGridY(ich);
      name = "hlovshi"; name += ich;
      hlovshi[iarm][iy][ix] = (TH2*)tfile[nrun]->Get(name);
cout << "ich " << ich << " " << (unsigned int)hlovshi[iarm][iy][ix] << endl;
    }

  plothilolimit();

  for (int iarm=0; iarm<2; iarm++)
    {
      for (int iy=0; iy<18; iy++)
        {
          for (int ix=0; ix<18; ix++)
            {
cout << "ich3 " << iarm << "\t" << ix << "\t" << iy << "\t" << (unsigned int)hlovshi[iarm][iy][ix] << endl;
}
}
}

  // Draw on Canvas
  TCanvas *chilo[2];
  chilo[0] = new TCanvas("chilo0","hilo arm 0",1000,1000);
  chilo[0]->Divide(18,18,0.0001,0.0001);
  chilo[1] = new TCanvas("chilo1","hilo arm 1",1000,1000);
  chilo[1]->Divide(18,18,0.0001,0.0001);

  TF1 *line = new TF1("line","[0]*x",0.,4096.);
  line->SetLineColor(2);
  line->SetLineWidth(0.8);

  for (int iarm=0; iarm<2; iarm++)
    {
      for (int iy=0; iy<18; iy++)
        {
          for (int ix=0; ix<18; ix++)
            {
cout << "iarm " << iarm << "\t" << iy << "\t" << ix << "\t" << (unsigned int)hlovshi[iarm][iy][ix] << endl;
              //if ( feech < 0 ) continue;
              if ( hlovshi[iarm][iy][ix]==0 ) continue;
    
              //int feech = mpcmap->getFeeCh(ix,iy,iarm);

              chilo[iarm]->cd( (17-iy)*18 + ix + 1 ); 
    
              Double_t min_hiadc = 500;
              Double_t max_hiadc = hilolimitmean[iarm][ix][iy] - 4*hilolimitrms[iarm][ix][iy];
              if ( max_hiadc < 1000. ) max_hiadc = 1000.;

              Double_t mean = 1.;
              Double_t rms = 1.;

cout << "iarm2 " << iarm << "\t" << iy << "\t" << ix
     << "\t" << (unsigned int)hlovshi[iarm][iy][ix]
     << "\t" << (unsigned int)myfitLinearRobust << endl;

              myfitLinearRobust( hlovshi[iarm][iy][ix], min_hiadc, max_hiadc, mean, rms );

              if ( mean!=0 )
                {
                  hiloratiomean[iarm][ix][iy] = 1.0/mean;
                  hiloratiorms[iarm][ix][iy] = rms/(mean*mean);
                  hiloratiostatus[iarm][ix][iy] = rms/(mean*mean);
                }
              hlovshi[iarm][iy][ix]->Draw();
              line->SetRange(min_hiadc,max_hiadc);
              line->SetParameter(0,mean);
              line->DrawCopy("same");
           }
        }
    }

  // Dump calib files
  ofstream outhiloratiofile("temp_MpcCal.hiloratio");
  ofstream outhilolimitfile("temp_MpcCal.hilolimit");
  for (int ich=0; ich<576; ich++)
    {
      if ( mpcmap->isCrystal(ich) != 1 ) continue;
      int iarm = mpcmap->getArm(ich);
      int ix = mpcmap->getGridX(ich);
      int iy = mpcmap->getGridY(ich);

      if ( hiloratiostatus[iarm][ix][iy] != -1 )
        {
          outhiloratiofile << ich << "\t"
                       << setprecision(6) << hiloratiomean[iarm][ix][iy] << "\t"
                       << setprecision(6) << hiloratiorms[iarm][ix][iy] << "\t"
                       << hiloratiostatus[iarm][ix][iy]
                       << endl;

          // silly... we should have plotted hi vs lo, not lo vs hi
          Double_t temp_mean = hilolimitmean[iarm][ix][iy]/hiloratiomean[iarm][ix][iy];
          Double_t temp_rms = temp_mean*(hilolimitrms[iarm][ix][iy]/hilolimitmean[iarm][ix][iy]);
          outhilolimitfile << ich << "\t"
                       << setprecision(6) << temp_mean << "\t"
                       << setprecision(6) << temp_rms << "\t"
                       << hilolimitstatus[iarm][ix][iy]
                       << endl;
        }
    }

  outhiloratiofile.close();
  outhilolimitfile.close();

  // Now save to web directory
/*
  outname = "webplots/slopecalib/het_";
  outname = "webplots/slopecalib/het_";
  outname += run;
  outname += ".gif";
  cout << outname << endl;

  chilo[0]->SaveAs( outname );
  outname.ReplaceAll("gif","ps");
  ac->SaveAs( outname );
*/
  chilo[1]->SaveAs( "chilo1.png" );
  chilo[0]->SaveAs( "chilo0.png" );

  delete chilo[0];


  delete chilo[1];

}

