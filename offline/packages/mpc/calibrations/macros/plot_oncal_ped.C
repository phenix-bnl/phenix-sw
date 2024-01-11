//
// Read in a list of pedestal output files (.gped, .ped, or .hped)
// and plot the results, and generate the final pedestals
//
#include <fstream.h>
#include <iostream.h>
#include <iomanip.h>

int deadch[576];

void plot_oncal_ped(const char *fname = "ped.list")
{
  gSystem->Load("libmpc.so");
  MpcMap *mpcmap = MpcMap::instance();
  gStyle->SetOptStat(0);

  ifstream inflist(fname);

  string ascfname;	// onlcal .asc output

  const int MAXRUNS = 10000;
  const int MAXFEECH = 576;
  //const int MAXFEECH = 288;
  float runnumber[MAXFEECH][MAXRUNS];
  int   good_runnumber[MAXRUNS];
  float adcmean[MAXFEECH][MAXRUNS];		// ADC subtraction
  float adcmeanerr[MAXFEECH][MAXRUNS];		// Err on mean
  float adcrms[MAXFEECH][MAXRUNS];		// ADC variance
  float adcntrig[MAXFEECH][MAXRUNS];		// ADC num events processed

  float ratiomean[MAXFEECH][MAXRUNS];	// ADC/PIN after ped subtraction
  float ratiorms[MAXFEECH][MAXRUNS];	// ADC variance per Ntrigs
  float rationtrig[MAXFEECH][MAXRUNS];	// ADC/PIN after ped subtraction

  float adcmax[MAXFEECH] = {-1.};	// max ADC over whole run
  float adcmin[MAXFEECH] = {4096.};	// max ADC over whole run
  for (int ifeech=0; ifeech<MAXFEECH; ifeech++)
    {
      adcmin[ifeech] = 4096.;
    }

  int nrun = 0;


  //readbad();

  while ( inflist >> ascfname )
    {
      ifstream inascfile(ascfname.c_str());
      TString runtext = ascfname;
      runtext.ReplaceAll(".gped","");
      int temp_run = runtext.Atoi();
      cout << "Processing " << ascfname << "\tRun " << nrun << "\t" << temp_run << endl;
      good_runnumber[nrun] = temp_run;

      int feech = -1;
      float temp_mean = 0.;
      float temp_rms = 0.;
      int temp_nevt = 0;
      while ( inascfile >> feech >> temp_mean >> temp_rms >> temp_nevt )
        {
          runnumber[feech][nrun] = temp_run;
          adcmean[feech][nrun] = temp_mean;
          adcrms[feech][nrun] = temp_rms;
          adcntrig[feech][nrun] = temp_nevt;

          if ( adcntrig[feech][nrun] > 0 )
            {
              adcmeanerr[feech][nrun] = adcrms[feech][nrun]/sqrt(adcntrig[feech][nrun]);
            }
     
          // get max and min values over the whole run
          if ( adcmean[feech][nrun] > adcmax[feech] )
            {
              adcmax[feech] = adcmean[feech][nrun];
            }
//cout << temp_run << "\t" << led << "\t" << feech << endl;
          if ( (adcmean[feech][nrun]>20.) && (adcmean[feech][nrun] < adcmin[feech]) )
            {
//cout << feech << "\t" << adcmean[feech][nrun] << "\t" << adcmin[feech] << endl;
              adcmin[feech] = adcmean[feech][nrun];
            }

        }
      inascfile.close();

      nrun++;

      //if ( nrun==20 ) break;
    }
  cout << "Processed " << nrun << " runs" << endl;

  // Make 2d plot of pedestals vs run
  TH2 *h2_ped = new TH2F("h2_ped","pedestals, ch vs run",nrun,0,nrun,MAXFEECH,0,MAXFEECH);
  h2_ped->SetXTitle("run index");
  h2_ped->SetYTitle("feech");
  TH2 *h2_ped_bad = new TH2F("h2_ped_bad","pedestals, before fix, ch vs run",nrun,0,nrun,MAXFEECH,0,MAXFEECH);
  h2_ped_bad->SetXTitle("run index");
  h2_ped_bad->SetYTitle("feech");

  for (int irun=0; irun<nrun; irun++)
    {
      TString name = "MpcCal_"; name += good_runnumber[irun];
      name += ".ped";
      ofstream goodpedfile(name.Data());

      for (int ich=0; ich<MAXFEECH; ich++)
        {
          if ( mpcmap->isCrystal(ich)==0 ) continue;

          h2_ped_bad->Fill(irun,ich,adcmean[ich][irun]);

          if ( runnumber[ich][irun]==0 )
            {
              cout << "BAD " << irun << "\t" << ich << "\t" << good_runnumber[irun]
                   << "\t" << adcmean[ich][irun]
                   << "\t" << adcrms[ich][irun]
                   << "\t" << adcntrig[ich][irun]
                   << endl;

              int goodrun = irun;
              if ( irun==0 )
                {
                  // find first good run
                  for (int irun2=1; irun2<nrun; irun2++)
                    {
                      if ( adcntrig[ich][irun2]>100 )
                        {
                          goodrun = irun2;
                          break;
                        }
                    }
                }
              else if ( irun==(nrun-1) )
                {
                  for (int irun2=nrun-1; irun2>=0; irun2--)
                    {
                      if ( adcntrig[ich][irun2]>100 )
                        {
                          goodrun = irun2;
                          break;
                        }
                    }
                }
              else
                {
                  // find nearest above and below
                  int hi_goodrun = -1; 
                  for (int irun2=irun+1; irun2<nrun; irun2++)
                    {
                       if ( adcntrig[ich][irun2]>100 )
                         {
                           hi_goodrun = irun2;
                           break;
                         }
                    }
                  int lo_goodrun = -1; 
                  for (int irun2=irun-1; irun2>=0; irun2--)
                    {
                       if ( adcntrig[ich][irun2]>100 )
                         {
                           lo_goodrun = irun2;
                           break;
                         }
                    }
cout << "grun " << lo_goodrun << "\t" << irun << "\t" << hi_goodrun << endl;
                  if ( lo_goodrun == -1 && hi_goodrun == -1 )
                    {
                      cout << "ERROR, NO GOOD RUN" << endl;
                    }
                  else if ( lo_goodrun == -1 )
                    {
                      cout << "WARNING, CHECK THIS" << endl;
                      goodrun = hi_goodrun;
                    }
                  else if ( hi_goodrun == -1 )
                    {
                      cout << "WARNING, CHECK THIS" << endl;
                      goodrun = lo_goodrun;
                    }
                  else
                    {
                      // they shouldn't differ by too much, unless we swapped the board
                      if ( fabs(adcmean[ich][lo_goodrun]-adcmean[ich][hi_goodrun]) < 1.0 )
                        {
                          cout << "XXX " << adcmean[ich][lo_goodrun]-adcmean[ich][hi_goodrun]
                               << "\t" << good_runnumber[lo_goodrun]
                               << "\t" << good_runnumber[hi_goodrun]
                               << endl;
                        }
                      // choose closest run 
                      if ( (good_runnumber[irun]-good_runnumber[lo_goodrun]) < (good_runnumber[hi_goodrun]-good_runnumber[irun]) )
                        {
                          goodrun = lo_goodrun;
                        }
                      else
                        {
                          goodrun = hi_goodrun;
                        }
                    }
                }

              if ( goodrun != irun )
                {
                  runnumber[ich][irun] = good_runnumber[goodrun];
                  adcmean[ich][irun] = adcmean[ich][goodrun];
                  adcrms[ich][irun] = adcrms[ich][goodrun];
                  adcntrig[ich][irun] = adcntrig[ich][goodrun];
                }
              else
                {
                  cout << "ERROR, DIDN'T FIND GOOD RUN " << goodrun << endl;
                }

              cout << "NEW " << irun << "\t" << ich << "\t" << good_runnumber[irun]
                   << "\t" << adcmean[ich][irun]
                   << "\t" << adcrms[ich][irun]
                   << "\t" << adcntrig[ich][irun]
                   << endl;

            }

/*
// After the blast, we use the last run for the dead channels.
if ( deadch[ich]==1 && irun>=14 )
{
  adcmean[ich][irun] = adcmean[ich][13];
  adcrms[ich][irun] = adcrms[ich][13];
  adcntrig[ich][irun] = adcntrig[ich][13];
}
*/

          h2_ped->Fill(irun,ich,adcmean[ich][irun]);

          goodpedfile << ich
                      << setw(16) << adcmean[ich][irun]
                      << setw(16) << adcrms[ich][irun]
                      << setw(16) << adcntrig[ich][irun]
                      << endl;
        }

      goodpedfile.close();
    }

  TCanvas *bc = new TCanvas("bc","bc",1100,850);
  h2_ped->Draw("colz");
  bc->Update();
  bc->Modified();
  bc->SaveAs("h2_ped.png");

  TCanvas *cc = new TCanvas("cc","cc",1100,850);
  h2_ped_bad->Draw("colz");
  cc->Update();
  cc->Modified();
  cc->SaveAs("h2_ped_bad.png");

  TCanvas *ac = new TCanvas("ac","ac",550,425);

  TCanvas *c_ledbyrun[2];
  //int SIZE = 1000;
  int SIZE = 2000;
  c_ledbyrun[0] = new TCanvas("c_ledbyrun0","PED By Run, South Arm",SIZE,SIZE);
  c_ledbyrun[1] = new TCanvas("c_ledbyrun1","PED By Run, North Arm",SIZE,SIZE);

  c_ledbyrun[0]->Divide(18,18,-1,-1);
  c_ledbyrun[1]->Divide(18,18,-1,-1);

  TString name;
  TString title;
  TGraphErrors *g_ped[MAXFEECH];	// ped and err on mean
  TGraphErrors *g_pedrms[MAXFEECH];	// ped and rms
  for (int ifeech=0; ifeech<MAXFEECH; ifeech++)
    {
      if ( mpcmap->isCrystal(ifeech)==0 ) continue;
      int arm = mpcmap->getArm(ifeech);
      int xpos = mpcmap->getGridX(ifeech);
      int ypos = mpcmap->getGridY(ifeech);

      c_ledbyrun[arm]->cd((17-ypos)*18 + xpos + 1);

      if ( adcmin[ifeech] > adcmax[ifeech] )
        {
          continue;
        }
      g_pedrms[ifeech] = new TGraphErrors(nrun,runnumber[ifeech],adcmean[ifeech],0,adcrms[ifeech]);
      g_pedrms[ifeech]->SetMarkerColor(4);
      g_pedrms[ifeech]->SetMarkerStyle(20);
      g_pedrms[ifeech]->SetMarkerSize(0.6);
      g_pedrms[ifeech]->SetMinimum(adcmin[ifeech]-5.0);
      g_pedrms[ifeech]->SetMaximum(adcmax[ifeech]+5.0);
      g_pedrms[ifeech]->Draw("ap");

      g_ped[ifeech] = new TGraphErrors(nrun,runnumber[ifeech],adcmeanerr[ifeech],0,adcrms[ifeech]);
      g_ped[ifeech]->SetMarkerColor(3);
      g_ped[ifeech]->SetLineColor(3);
      g_ped[ifeech]->SetMarkerStyle(7);
      //g_ped[ifeech]->SetMinimum(adcmin[ifeech]-5.0);
      //g_ped[ifeech]->SetMaximum(adcmax[ifeech]+5.0);
      g_ped[ifeech]->Draw("[]");

      //if ( ifeech==31 ) 
        {
          ac->cd();
          ac->Clear();
          name = "run12cuauped_"; name += ifeech; name += ".png";
          title = "run12 cuau ped, ch "; title += ifeech;
          title += " "; title += xpos; title += " "; title += ypos;
          g_pedrms[ifeech]->SetTitle(title);
          g_pedrms[ifeech]->Draw("ap");
          g_ped[ifeech]->Draw("[]");
          ac->Modified();
          ac->Update();
          ac->SaveAs(name);
        }
    }

  c_ledbyrun[0]->Modified();
  c_ledbyrun[0]->Update();
  c_ledbyrun[1]->Modified();
  c_ledbyrun[1]->Update();
  c_ledbyrun[0]->SaveAs(".png");
  c_ledbyrun[1]->SaveAs(".png");

}

void readbad()
{
  for (int ich=0; ich<576; ich++) deadch[ich] = 1;
  ifstream badfile("goodchannels.txt");

  int ch;
  while ( badfile >> ch )
    {
      deadch[ch] = 0;
    }

  for (int ich=0; ich<576; ich++) 
    {
      if ( deadch[ich] == 0 ) cout << "GOOD " << ich << endl;
    }

/*
  string junk;
  cout << "XXX ";
  cin >> junk;
*/

  badfile.close();
  
}
