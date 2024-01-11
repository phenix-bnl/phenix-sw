//
// Find the scale value to match the gains
// Use with mpcinvslope.root files
//
#include <iostream>
#include <fstream>
#include <TPad.h>
#include <TFile.h>
#include <TSystem.h>
//#include <MpcMap.h>
#include <TStyle.h>
#include <TH1.h>
#include <TCanvas.h>

const Double_t MIN_ENERGY = 4;
const Double_t MAX_ENERGY = 12;
//const Double_t MAX_ENERGY = 120;
Double_t integ_counts_ref[2][18][18] = {{{0.}}};	// [gridy][gridx]
Double_t good_gain[2][18][18] = {{{0.}}};	// [gridy][gridx]

// Get the values for the reference run
void get_reference(const char *fname = "mpcinvslope_ref.root", const int runnumber)
{
  gStyle->SetOptStat(0);
  gSystem->Load("libmpc.so");

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",runnumber);

  //MpcMap *mpcmap = MpcMap::instance();
  MpcMap mpcmap;

  //if ( mpcmap==0 ) return;
 
  TFile *tfile;
  TH1 *hlopt[2][18][18];
  TH1 *hzvtxcut;

  TString name;

  //name = ""; name += run; name += "/"; name += "mpcslope.root";
  cout << "reading file " << fname << endl;
  tfile = new TFile(fname,"READ");
  hzvtxcut = (TH1*)tfile->Get("hzvtxcut");
  Double_t nevents = hzvtxcut->Integral();
  cout << " found nevents " << nevents << endl;

  for (int iarm=0; iarm<2; iarm++)
    {
      for (int iy=0; iy<18; iy++)
        {
          for (int ix=0; ix<18; ix++)
            {
              int ifeech = mpcmap.getFeeCh(ix,iy,iarm);
              //name = "hlo_"; name += ifeech; name += "_"; name += ix; name += "_"; name += iy;
              //name = "het_"; name += ifeech; name += "_"; name += ix; name += "_"; name += iy;
              name = "he_"; name += ifeech; name += "_"; name += ix; name += "_"; name += iy;
              hlopt[iarm][iy][ix] = (TH1*)tfile->Get(name);
              if ( hlopt[iarm][iy][ix]==0 ) continue;
              hlopt[iarm][iy][ix]->Scale(1.0/nevents);
              Double_t minbin = hlopt[iarm][iy][ix]->FindBin(MIN_ENERGY);
              Double_t maxbin = hlopt[iarm][iy][ix]->FindBin(MAX_ENERGY);
              integ_counts_ref[iarm][iy][ix] = hlopt[iarm][iy][ix]->Integral(minbin,maxbin);
/*
              if (iarm==0&&ix==17&&iy==7)
                {
                  hlopt[iarm][iy][ix]->Draw();
                  cout << "iy ix\t" << iy << "\t" << ix << "\t" << integ_counts_ref[iarm][iy][ix] << endl;
                }
*/
            }
        }
    }

}

void matchgain_invslope()
{
  //get_reference("mpcinvslope_run9_500.root",278241);
  get_reference("mpcinvslope_run09pp500.root",278241);

  gStyle->SetOptStat(0);
  gSystem->Load("libmpc.so");

  recoConsts *rc = recoConsts::instance();
  //rc->set_IntFlag("RUNNUMBER",368627);	// run12pp510
  //rc->set_IntFlag("RUNNUMBER",386822);	// run13pp510
  rc->set_IntFlag("RUNNUMBER",388404);	// run13pp510

  //MpcMap *mpcmap = MpcMap::instance();
  MpcMap mpcmap;
  //mpcmap->Download_Maps("/home/phnxmpc/database/Run12pp/MpcCal.map");

  //if ( mpcmap==0 ) return;

  ifstream inrunlist("runs.list");

  TFile *tfile[100];

  TH1 *hlopt[2][18][18];
  TH1 *hzvtxcut = 0;

  TH1 *hnewpt[2][18][18];	// Rescaled version

  ofstream calibfile("temp_MpcCal.gain");
  TString name;
  int nrun = 0;
  int run;
  //while ( inrunlist >> run )
  while ( inrunlist >> name )
    {
      //name = ""; name += run; name += "/"; name += "mpcslope.root";
      cout << "reading file " << name << endl;
      tfile[nrun] = new TFile(name,"READ");
      hzvtxcut = (TH1*)tfile[nrun]->Get("hzvtxcut");
      Double_t nevents = hzvtxcut->Integral();
      cout << "Found nevents " << nevents << endl;

      for (int iarm=0; iarm<2; iarm++)
        {
          for (int iy=0; iy<18; iy++)
            {
              for (int ix=0; ix<18; ix++)
                {
                  //if (iarm!=0||ix!=17||iy!=7) continue;
                  int ifeech = mpcmap.getFeeCh(ix,iy,iarm);
                  if ( ifeech<0 ) continue;
                  //name = "hlo_"; name += ifeech; name += "_"; name += ix; name += "_"; name += iy;
                  //name = "het_"; name += ifeech; name += "_"; name += ix; name += "_"; name += iy;
                  name = "he_"; name += ifeech; name += "_"; name += ix; name += "_"; name += iy;
                  //cout << name << endl;
                  hlopt[iarm][iy][ix] = (TH1F*)tfile[nrun]->Get(name);
                  if ( hlopt[iarm][iy][ix]==0 ) continue;
                  hlopt[iarm][iy][ix]->Scale(1.0/nevents);

                  for (float ig=0.01; ig<2.00; ig += 0.001)
                    {
                      int minbin = hlopt[iarm][iy][ix]->FindBin( MIN_ENERGY/ig );
                      int maxbin = hlopt[iarm][iy][ix]->FindBin( MAX_ENERGY/ig );
                      Double_t temp_integ = hlopt[iarm][iy][ix]->Integral(minbin,maxbin);
                      //cout << ig << "\t" << temp_integ << "\t" << integ_counts_ref[iarm][iy][ix]
                      //     << "\t" << temp_integ/integ_counts_ref[iarm][iy][ix] << endl;
                      if ( integ_counts_ref[iarm][iy][ix]==0. )
                        {
                          cout << "Can't get gain for " << ifeech << "\t" << ix << "\t" << iy << endl;
                          break;
                        }
                      if ( temp_integ/integ_counts_ref[iarm][iy][ix] > 1.0 )
                        {
                          good_gain[iarm][iy][ix] = ig;
                          break;
                        }
                    }
                  if ( good_gain[iarm][iy][ix]==0. )
                    {
                      cout << "Didn't get gain for " << ifeech << "\t" << ix << "\t" << iy << endl;
                    }
                  else
                    {
                      calibfile << ifeech << "\t" << good_gain[iarm][iy][ix] << "\t1\t0" << endl;
                    }
                }
            }
        }

      nrun++;
      if (nrun==1) break;
    }

  calibfile.close();


/*
  TCanvas *c_mpc[2] = {0};

  c_mpc[0] = new TCanvas("mpcs","mpcs",1000,1000);

  gPad->GetName();

  c_mpc[1] = new TCanvas("mpcn","mpcn",1000,1000);

  c_mpc[0]->Divide(18,18,0.0001,0.0001);

  c_mpc[1]->Divide(18,18,0.0001,0.0001);

  for (int iarm=0; iarm<2; iarm++)
    {
      for (int iy=0; iy<18; iy++)
        {
          //if (iy>2) continue;
          for (int ix=0; ix<18; ix++)
            {
              int feech = mpcmap.getFeeCh(ix,iy,iarm);
              if ( feech < 0 ) continue;
              if ( hlopt[iarm][iy][ix]==0 ) continue;
    
              c_mpc[iarm]->cd( (17-iy)*18 + ix + 1 ); 
    
              hlopt[iarm][iy][ix]->SetLineColor(4);
              hlopt[iarm][iy][ix]->Rebin(4);
              //hlopt[iarm][iy][ix]->SetAxisRange(-0.2,1.2);
              hlopt[iarm][iy][ix]->SetAxisRange(-10,200);
    
              //hlopt[iarm][iy][ix]->Fit("expo");
    
              hlopt[iarm][iy][ix]->DrawCopy();
    
              gPad->SetLogy(1);
    
            }
        }
    }

  c_mpc[0]->SaveAs("hlopt0.png");
  c_mpc[0]->SaveAs("hlopt0.eps");
  c_mpc[1]->SaveAs("hlopt1.png");
  c_mpc[1]->SaveAs("hlopt1.eps");
*/

}

