#include "TFile.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "summaryQA.h"

#include <fstream>
#include <iostream>
#include <cmath>

Float_t meanoccW0, meanoccW1, meanoccW2, meanoccW3;
Float_t meanoccE0, meanoccE1, meanoccE2, meanoccE3;

Int_t ndeadW0 = 0, ndeadW1 = 0, ndeadW2 = 0, ndeadW3 = 0;
Int_t ndeadE0 = 0, ndeadE1 = 0, ndeadE2 = 0, ndeadE3 = 0;

Int_t nnoisyW0 = 0, nnoisyW1 = 0, nnoisyW2 = 0, nnoisyW3 = 0;
Int_t nnoisyE0 = 0, nnoisyE1 = 0, nnoisyE2 = 0, nnoisyE3 = 0;

Float_t emct0[8];

using namespace std;
int QASummary::processEmc()
{
  cout << "Emc..." << endl;
  fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
  fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }

  bool ppdata = false;
  if ( (runNumber>34000 && runNumber<65000) || (runNumber>82000 && runNumber<94000) ) ppdata = true;

  textFile << " ----------------------------------------------------" << endl;
  if ( ppdata )
    textFile << " -- EMC QA Summary for pp --" << endl;
  else
    textFile << " -- EMC QA Summary for AuAu --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  textFile << " Run number: " << runNumber << endl;
  
  TH1F* emcWest01ETot = (TH1F *) qafile->Get ("emcWest01ETot");
  TH1F* emcWest23ETot = (TH1F *) qafile->Get ("emcWest23ETot");
  TH1F* emcEastETot = (TH1F *) qafile->Get ("emcEastETot");
  TH1F* emcPbGlETot = (TH1F *) qafile->Get ("emcPbGlETot");

  TH1F* emcWest01E = (TH1F *) qafile->Get ("emcWest01E");
  TH1F* emcWest23E = (TH1F *) qafile->Get ("emcWest23E");
  TH1F* emcEastE = (TH1F *) qafile->Get ("emcEastE");
  TH1F* emcPbGlE = (TH1F *) qafile->Get ("emcPbGlE");

  TH1F* emcWest01E1 = (TH1F *) qafile->Get ("emcWest01E1");
  TH1F* emcWest23E1 = (TH1F *) qafile->Get ("emcWest23E1");
  TH1F* emcEastE1 = (TH1F *) qafile->Get ("emcEastE1");
  //  TH1F* emcPbGlE1 = (TH1F *) qafile->Get ("emcPbGlE1");

  TH1F* emcWest0Time = (TH1F *) qafile->Get ("emcWest0Time");
  TH1F* emcWest1Time = (TH1F *) qafile->Get ("emcWest1Time");
  TH1F* emcWest2Time = (TH1F *) qafile->Get ("emcWest2Time");
  TH1F* emcWest3Time = (TH1F *) qafile->Get ("emcWest3Time");
  TH1F* emcEast2Time = (TH1F *) qafile->Get ("emcEast2Time");
  TH1F* emcEast3Time = (TH1F *) qafile->Get ("emcEast3Time");
  TH1F* emcPbGl0Time = (TH1F *) qafile->Get ("emcPbGl0Time");
  TH1F* emcPbGl1Time = (TH1F *) qafile->Get ("emcPbGl1Time");

  TH2F* emcW0YZ = (TH2F *) qafile->Get ("emcW0YZcut0");
  TH2F* emcW1YZ = (TH2F *) qafile->Get ("emcW1YZcut0");
  TH2F* emcW2YZ = (TH2F *) qafile->Get ("emcW2YZcut0");
  TH2F* emcW3YZ = (TH2F *) qafile->Get ("emcW3YZcut0");
  TH2F* emcE0YZ = (TH2F *) qafile->Get ("emcE0YZcut0");
  TH2F* emcE1YZ = (TH2F *) qafile->Get ("emcE1YZcut0");
  TH2F* emcE2YZ = (TH2F *) qafile->Get ("emcE2YZcut0");
  TH2F* emcE3YZ = (TH2F *) qafile->Get ("emcE3YZcut0");

  TH2F* emcTwrMap01[8];
  emcTwrMap01[0] = (TH2F *) qafile->Get ("emcW0YZcut4");
  emcTwrMap01[1] = (TH2F *) qafile->Get ("emcW1YZcut4");
  emcTwrMap01[2] = (TH2F *) qafile->Get ("emcW2YZcut4");
  emcTwrMap01[3] = (TH2F *) qafile->Get ("emcW3YZcut4");
  emcTwrMap01[4] = (TH2F *) qafile->Get ("emcE3YZcut4");
  emcTwrMap01[5] = (TH2F *) qafile->Get ("emcE2YZcut4");
  emcTwrMap01[6] = (TH2F *) qafile->Get ("emcE1YZcut4");
  emcTwrMap01[7] = (TH2F *) qafile->Get ("emcE0YZcut4");

  TH2F* emcTwrMap1[8];
  emcTwrMap1[0] = (TH2F *) qafile->Get ("emcW0YZcut2");
  emcTwrMap1[1] = (TH2F *) qafile->Get ("emcW1YZcut2");
  emcTwrMap1[2] = (TH2F *) qafile->Get ("emcW2YZcut2");
  emcTwrMap1[3] = (TH2F *) qafile->Get ("emcW3YZcut2");
  emcTwrMap1[4] = (TH2F *) qafile->Get ("emcE3YZcut2");
  emcTwrMap1[5] = (TH2F *) qafile->Get ("emcE2YZcut2");
  emcTwrMap1[6] = (TH2F *) qafile->Get ("emcE1YZcut2");
  emcTwrMap1[7] = (TH2F *) qafile->Get ("emcE0YZcut2");

  //
  // add here analysis and printout results to textFile
  //

  Stat_t nEvents;
  nEvents = emcWest01ETot->GetEntries();
  textFile << " Number of good-vertex events: " << nEvents << endl;

  Int_t maxTimeBin;
  Int_t nbins;
  Int_t nDoF;

  Float_t min, max, inc;
  Float_t xcent, xhi, xlo;
  Double_t fitparams[3], mipparams[5], eparams[2];
  Double_t chisquare;

  min = -20.;
  max = 40.;
  nbins = 600;
  inc = (max - min) / nbins;

  maxTimeBin =  (emcWest0Time->GetMaximumBin());
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f1 = new TF1("f1", "gaus", xlo, xhi);
  emcWest0Time->Fit("f1", "RQN");
  f1->GetParameters(fitparams);
  chisquare = f1->GetChisquare();
  nDoF = f1->GetNDF();
  emct0[0] = fitparams[1];

  textFile << " W0 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest0Time->GetEntries() << endl;

  maxTimeBin = (emcWest1Time->GetMaximumBin());
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  //  TF1 *f1 = new TF1("f1", "gaus", xlo, xhi);
  emcWest1Time->Fit("f1", "RQN");
  f1->GetParameters(fitparams);
  chisquare = f1->GetChisquare();
  nDoF = f1->GetNDF();
  emct0[1] = fitparams[1];

  textFile << " W1 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest1Time->GetEntries() << endl;

  maxTimeBin = emcWest2Time->GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f2 = new TF1("f2", "gaus", xlo, xhi);
  emcWest2Time->Fit("f2", "RQN");
  f2->GetParameters(fitparams);
  chisquare = f2->GetChisquare();
  nDoF = f2->GetNDF();
  emct0[2] = fitparams[1];

  textFile << " W2 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest2Time->GetEntries() << endl;

  maxTimeBin = emcWest3Time->GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  // delete f2;
  f2 = new TF1("f2", "gaus", xlo, xhi);
  emcWest3Time->Fit("f2", "RQN");
  f2->GetParameters(fitparams);
  chisquare = f2->GetChisquare();
  nDoF = f2->GetNDF();
  emct0[3] = fitparams[1];

  textFile << " W3 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest3Time->GetEntries() << endl;

  maxTimeBin = emcEast3Time->GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f3 = new TF1("f3", "gaus", xlo, xhi);
  emcEast3Time->Fit("f3", "RQN");
  f3->GetParameters(fitparams);
  chisquare = f3->GetChisquare();
  nDoF = f3->GetNDF();
  emct0[4] = fitparams[1];

  textFile << " E3 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcEast3Time->GetEntries() << endl;

  maxTimeBin = emcEast2Time->GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  // delete f3;
  f3 = new TF1("f3", "gaus", xlo, xhi);
  emcEast2Time->Fit("f3", "RQN");
  f3->GetParameters(fitparams);
  chisquare = f3->GetChisquare();
  nDoF = f3->GetNDF();
  emct0[5] = fitparams[1];

  textFile << " E2 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcEast2Time->GetEntries() << endl;

  maxTimeBin = emcPbGl1Time->GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f4 = new TF1("f4", "gaus", xlo, xhi);
  emcPbGl1Time->Fit("f4", "RQN");
  f4->GetParameters(fitparams);
  chisquare = f4->GetChisquare();
  nDoF = f4->GetNDF();
  emct0[6] = fitparams[1];

  textFile << " E1 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcPbGl1Time->GetEntries() << endl;

  maxTimeBin = emcPbGl0Time->GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  // delete f4;
  f4 = new TF1("f4", "gaus", xlo, xhi);
  emcPbGl0Time->Fit("f4", "RQN");
  f4->GetParameters(fitparams);
  chisquare = f4->GetChisquare();
  nDoF = f4->GetNDF();
  emct0[7] = fitparams[1];

  textFile << " E0 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcPbGl0Time->GetEntries() << endl;

  TF1 *f5 = new TF1("f5", "expo(0)+gaus(2)", 0.17, 0.9);
  f5->SetParameters(8., -4.4, 800., 0.28, 0.035);
  f5->SetParLimits(2, 0., 100000.);
  f5->SetParLimits(3, 0.2, 0.4);
  f5->SetParLimits(4, 0.0, 0.06);

  emcWest01E1->Fit("f5", "RQNB");
  f5->GetParameters(mipparams);
  chisquare = f5->GetChisquare();
  nDoF = f5->GetNDF();

  textFile << " W01 MIP Mean, Width, Chi2/NDF, No.Ent: " << mipparams[3] << " " << mipparams[4] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest01E1->GetEntries() << endl;

  emcWest23E1->Fit("f5", "RQNB");
  f5->GetParameters(mipparams);
  chisquare = f5->GetChisquare();
  nDoF = f5->GetNDF();

  textFile << " W23 MIP Mean, Width, Chi2/NDF, No.Ent: " << mipparams[3] << " " << mipparams[4] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest23E1->GetEntries() << endl;

  emcEastE1->Fit("f5", "RQNB");
  f5->GetParameters(mipparams);
  chisquare = f5->GetChisquare();
  nDoF = f5->GetNDF();

  textFile << " E23 MIP Mean, Width, Chi2/NDF, No.Ent: " << mipparams[3] << " " << mipparams[4] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcEastE1->GetEntries() << endl;

  TF1 *f6 = new TF1("f6", "expo", 0.4, 2.0);

  emcWest01E->Fit("f6", "RQN");
  f6->GetParameters(eparams);
  chisquare = f6->GetChisquare();
  nDoF = f6->GetNDF();

  textFile << " W01 E InSlope, Chi2/NDF, Mean, No.Ent: " << eparams[1] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest01E->GetMean() << " " << emcWest01E->GetEntries() << endl;

  emcWest23E->Fit("f6", "RQN");
  f6->GetParameters(eparams);
  chisquare = f6->GetChisquare();
  nDoF = f6->GetNDF();

  textFile << " W23 E InSlope, Chi2/NDF, Mean, No.Ent: " << eparams[1] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest23E->GetMean() << " " << emcWest23E->GetEntries() << endl;

  emcEastE->Fit("f6", "RQN");
  f6->GetParameters(eparams);
  chisquare = f6->GetChisquare();
  nDoF = f6->GetNDF();

  textFile << " E23 E InSlope, Chi2/NDF, Mean, No.Ent: " << eparams[1] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcEastE->GetMean() << " " << emcEastE->GetEntries() << endl;

  emcPbGlE->Fit("f6", "RQN");
  f6->GetParameters(eparams);
  chisquare = f6->GetChisquare();
  nDoF = f6->GetNDF();

  textFile << " E01 E InSlope, Chi2/NDF, Mean, No.Ent: " << eparams[1] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcPbGlE->GetMean() << " " << emcPbGlE->GetEntries() << endl;

  textFile << " Mean Total Energy West01: " << emcWest01ETot->GetMean() << endl;
  textFile << " Mean Total Energy West23: " << emcWest23ETot->GetMean() << endl;
  textFile << " Mean Total Energy East23: " << emcEastETot->GetMean() << endl;
  textFile << " Mean Total Energy East01: " << emcPbGlETot->GetMean() << endl;

  long unsigned int EMCWstat = 0, EMCPBSCEstat = 0, EMCPBGLEstat = 0, EMCstat = 0;

  if ( ! ppdata )
    {
      Stat_t nentW0[72][36];
      Stat_t nentW1[72][36];
      Stat_t nentW2[72][36];
      Stat_t nentW3[72][36];
      Stat_t nentE3[72][36];
      Stat_t nentE2[72][36];
      Stat_t nentE1[96][48];
      Stat_t nentE0[96][48];

      //       Int_t ndeadW0 = 0, ndeadW1 = 0, ndeadW2 = 0, ndeadW3 = 0;
      //       Int_t ndeadE0 = 0, ndeadE1 = 0, ndeadE2 = 0, ndeadE3 = 0;

      //       Int_t nnoisyW0 = 0, nnoisyW1 = 0, nnoisyW2 = 0, nnoisyW3 = 0;
      //       Int_t nnoisyE0 = 0, nnoisyE1 = 0, nnoisyE2 = 0, nnoisyE3 = 0;

      Stat_t totentW0, totentW1, totentW2, totentW3;
      Stat_t totentE0, totentE1, totentE2, totentE3;
      //      Float_t meanoccW0, meanoccW1, meanoccW2, meanoccW3;
      //      Float_t meanoccE0, meanoccE1, meanoccE2, meanoccE3;

      if(emcW0YZ) {totentW0 = emcW0YZ->GetEntries();} else {totentW0 = 0;}
      if(emcW1YZ) {totentW1 = emcW1YZ->GetEntries();} else {totentW1 = 0;}
      if(emcW2YZ) {totentW2 = emcW2YZ->GetEntries();} else {totentW2 = 0;}
      if(emcW3YZ) {totentW3 = emcW3YZ->GetEntries();} else {totentW3 = 0;}
      if(emcE0YZ) {totentE0 = emcE0YZ->GetEntries();} else {totentE0 = 0;}
      if(emcE1YZ) {totentE1 = emcE1YZ->GetEntries();} else {totentE1 = 0;}
      if(emcE2YZ) {totentE2 = emcE2YZ->GetEntries();} else {totentE2 = 0;}
      if(emcE3YZ) {totentE3 = emcE3YZ->GetEntries();} else {totentE3 = 0;}

      meanoccW0 = totentW0 / 2592.;
      meanoccW1 = totentW1 / 2592.;
      meanoccW2 = totentW2 / 2592.;
      //      meanoccW3 = totentW3 / 2016.;  // maximum number of installed channels W3 run2
      meanoccW3 = totentW3 / 2592.;  // W3 is fully instrumented starting from Run3
      meanoccE3 = totentE3 / 2592.;
      meanoccE2 = totentE2 / 2592.;
      meanoccE1 = totentE1 / 4608.;
      meanoccE0 = totentE0 / 4608.;

      for (int i = 0; i < 96; i++)
	{
          for (int j = 0; j < 48; j++)
	    {
              if (i < 72 && j < 36)
		{
                  nentW0[i][j] = emcW0YZ->GetBinContent(i+1, j+1);
                  if (nentW0[i][j] == 0 )
		    ndeadW0++;
                  if (nentW0[i][j] > 4.*meanoccW0)
		    nnoisyW0++;
		  
                  nentW1[i][j] = emcW1YZ->GetBinContent(i+1, j+1);
                  if (nentW1[i][j] == 0 )
		    ndeadW1++;
                  if (nentW1[i][j] > 4.*meanoccW1)
		    nnoisyW1++;

                  nentW2[i][j] = emcW2YZ->GetBinContent(i+1, j+1);
                  if (nentW2[i][j] == 0 )
		    ndeadW2++;
                  if (nentW2[i][j] > 4.*meanoccW2)
		    nnoisyW2++;
		  
                  nentW3[i][j] = emcW3YZ->GetBinContent(i+1, j+1);
                  if (nentW3[i][j] == 0 )
		    ndeadW3++;
                  if (nentW3[i][j] > 4.*meanoccW3)
		    nnoisyW3++;

                  nentE3[i][j] = emcE3YZ->GetBinContent(i+1, j+1);
                  if (nentE3[i][j] == 0 )
		    ndeadE3++;
                  if (nentE3[i][j] > 4.*meanoccE3)
		    nnoisyE3++;

                  nentE2[i][j] = emcE2YZ->GetBinContent(i+1, j+1);
                  if (nentE2[i][j] == 0 )
		    ndeadE2++;
                  if (nentE2[i][j] > 4.*meanoccE2)
		    nnoisyE2++;
		  
		}

              nentE1[i][j] = emcE1YZ->GetBinContent(i+1, j+1);
              if (nentE1[i][j] == 0 )
                ndeadE1++;
              if (nentE1[i][j] > 5.*meanoccE1)
                nnoisyE1++;

              nentE0[i][j] = emcE0YZ->GetBinContent(i+1, j+1);
              if (nentE0[i][j] == 0 )
                ndeadE0++;
              if (nentE0[i][j] > 5.*meanoccE0)
                nnoisyE0++;

            }
        }

      textFile << "W0: " << totentW0 << " tower hits, " << ndeadW0 << " 0-towers, mean/tower = " << meanoccW0 << ", #noisy = " << nnoisyW0 << endl;
      textFile << "W1: " << totentW1 << " tower hits, " << ndeadW1 << " 0-towers, mean/tower = " << meanoccW1 << ", #noisy = " << nnoisyW1 << endl;
      textFile << "W2: " << totentW2 << " tower hits, " << ndeadW2 << " 0-towers, mean/tower = " << meanoccW2 << ", #noisy = " << nnoisyW2 << endl;
      textFile << "W3: " << totentW3 << " tower hits, " << ndeadW3 << " 0-towers, mean/tower = " << meanoccW3 << ", #noisy = " << nnoisyW3 << endl;
      textFile << "E3: " << totentE3 << " tower hits, " << ndeadE3 << " 0-towers, mean/tower = " << meanoccE3 << ", #noisy = " << nnoisyE3 << endl;
      textFile << "E2: " << totentE2 << " tower hits, " << ndeadE2 << " 0-towers, mean/tower = " << meanoccE2 << ", #noisy = " << nnoisyE2 << endl;
      textFile << "E1: " << totentE1 << " tower hits, " << ndeadE1 << " 0-towers, mean/tower = " << meanoccE1 << ", #noisy = " << nnoisyE1 << endl;
      textFile << "E0: " << totentE0 << " tower hits, " << ndeadE0 << " 0-towers, mean/tower = " << meanoccE0 << ", #noisy = " << nnoisyE0 << endl;

      //
      // add code to determine emc status
      //

      // need a number of good-vertex events for criteria to be meaningful
      if (nEvents > 1500)
        {
	  if( meanoccW0 >= 10.0 && ndeadW0 >= 10 ) EMCstat += 1;
	  if( meanoccW1 >= 10.0 && ndeadW1 >= 10 ) EMCstat += 2;
	  if( meanoccW2 >= 10.0 && ndeadW2 >= 15 ) EMCstat += 4;
	  if( meanoccW3 >= 10.0 && ndeadW3 >= 20 ) EMCstat += 8;
	  if( meanoccE3 >= 10.0 && ndeadE3 >= 45 ) EMCstat += 16;
	  if( meanoccE2 >= 10.0 && ndeadE3 >= 40 ) EMCstat += 32;

	  if( meanoccW0 >= 10.0 && nnoisyW0 >= 1.2*120 ) EMCstat += 256;
	  if( meanoccW1 >= 10.0 && nnoisyW1 >= 1.2*45 ) EMCstat += 512;
	  if( meanoccW2 >= 10.0 && nnoisyW2 >= 1.2*70 ) EMCstat += 1024;
	  if( meanoccW3 >= 10.0 && nnoisyW3 >= 1.2*60 ) EMCstat += 2048;
	  if( meanoccE3 >= 10.0 && nnoisyE3 >= 1.2*70 ) EMCstat += 4096;
	  if( meanoccE2 >= 10.0 && nnoisyE2 >= 1.2*60 ) EMCstat += 8192;

	  if( fabs(emct0[0])>0.5 ) EMCstat += 65536;
	  if( fabs(emct0[1])>0.5 ) EMCstat += 131072;
	  if( fabs(emct0[2])>0.5 ) EMCstat += 262144;
	  if( fabs(emct0[3])>0.5 ) EMCstat += 524288;
	  if( fabs(emct0[4])>0.5 ) EMCstat += 1048576;
	  if( fabs(emct0[5])>0.5 ) EMCstat += 2097152;
	}

      if (nEvents > 4000)
	{
	  if ( meanoccE1 >= 10.0 && ndeadE1 >= 400 ) EMCstat += 64;
	  if ( meanoccE0 >= 10.0 && ndeadE1 >= 400 ) EMCstat += 128;
	  
	  if ( meanoccE1 >= 10.0 && nnoisyE1 >= 205 ) EMCstat += 16384;
	  if ( meanoccE0 >= 10.0 && nnoisyE1 >= 215 ) EMCstat += 32768;
	  
	}
      EMCWstat = EMCstat;
      EMCPBSCEstat = EMCstat;
      EMCPBGLEstat = EMCstat;
    } // if( ! ppdata )

  else
    {
      
      int nx, ny;
      float nmean01[8], nmean1[8], nthresh;
      int nhot01[8], nhot1[8];
      const char* sname[8] = {"W0", "W1", "W2", "W3", "E3", "E2", "E1", "E0"};
      
      for ( int is = 0; is < 8; is++ )
        {

          // Map > 1GeV

          nhot1[is] = 0;
          nx = emcTwrMap1[is]->GetNbinsX();
          ny = emcTwrMap1[is]->GetNbinsY();
          nmean1[is] = emcTwrMap1[is]->GetEntries();
          nmean1[is] /= (nx * ny);
          //  printf("Before: %d %d %d %f\n",is,nx,ny,nmean1[is]);
          nthresh = nmean1[is] + 5 * sqrt(nmean1[is]);
          if ( nthresh < 3 )
            nthresh = 3;
          nmean1[is] = 0;
          // Turn off warm towers from mean calculation
          for ( int iy = 1; iy <= ny; iy++ )
            {
              for ( int ix = 1; ix <= nx; ix++ )
                {
                  if ( emcTwrMap1[is]->GetBinContent(ix, iy) < nthresh )
                    {
                      nmean1[is] += emcTwrMap1[is]->GetBinContent(ix, iy);
                    };
                }
            }
          nmean1[is] /= (nx * ny);
          //  printf("After:  %d %d %d %f\n",is,nx,ny,nmean1[is]);

          // Find hot/warm towers
          nthresh = 6;
          if ( nmean1[is]*10 > 1 )
            nthresh = 10 * nmean1[is] + 5 * sqrt(10 * nmean1[is]);
          for ( int iy = 1; iy <= ny; iy++ )
            {
              for ( int ix = 1; ix <= nx; ix++ )
                {
                  if ( emcTwrMap1[is]->GetBinContent(ix, iy) > nthresh )
                    {
                      nhot1[is]++;
                    };
                }
            }
          //  printf("sec=%d nhot=%d (lim=%f) \n",is,nhot1[is],nthresh);

          // Map > 0.1 GeV

          nhot01[is] = 0;
          nx = emcTwrMap01[is]->GetNbinsX();
          ny = emcTwrMap01[is]->GetNbinsY();
          nmean01[is] = emcTwrMap01[is]->GetEntries();
          nmean01[is] /= (nx * ny);
          //  printf("Before: %d %d %d %f\n",is,nx,ny,nmean01[is]);
          nthresh = nmean01[is] + 5 * sqrt(nmean01[is]);
          if ( nthresh < 3 )
            nthresh = 3;
          nmean01[is] = 0;
          // Turn off warm towers from mean calculation
          for ( int iy = 1; iy <= ny; iy++ )
            {
              for ( int ix = 1; ix <= nx; ix++ )
                {
                  if ( emcTwrMap01[is]->GetBinContent(ix, iy) < nthresh )
                    {
                      nmean01[is] += emcTwrMap01[is]->GetBinContent(ix, iy);
                    };
                }
            }
          nmean01[is] /= (nx * ny);
          //  printf("After:  %d %d %d %f\n",is,nx,ny,nmean01[is]);

          // Find hot/warm towers
          nthresh = 6;
          if ( nmean01[is]*10 > 1 )
            nthresh = 10 * nmean01[is] + 5 * sqrt(10 * nmean01[is]);
          for ( int iy = 1; iy <= ny; iy++ )
            {
              for ( int ix = 1; ix <= nx; ix++ )
                {
                  if ( emcTwrMap01[is]->GetBinContent(ix, iy) > nthresh )
                    {
                      nhot01[is]++;
                    };
                }
            }
          //  printf("sec=%d nhot=%d (lim=%f) \n",is,nhot01[is],nthresh);
          textFile << sname[is] << ": nHits: " << emcTwrMap1[is]->GetEntries() << "/" << emcTwrMap01[is]->GetEntries() << " Mean: " << nmean1[is] << "/" << nmean01[is] << " nHot: " << nhot1[is] << "/" << nhot01[is] << endl;
        }

      //
      // code to determine emc status
      //

      int nhot1_warn[8] = {0, 0, 1, 0, 0, 0, 0, 2};
      int nhot1_err[8] = {3, 3, 4, 3, 3, 3, 3, 5};
      //      int nhot01_warn[8] = {0, 0, 5, 30, 6, 1, 20, 40};
      //      int nhot01_err[8] = {3, 3, 8, 50, 10, 4, 30, 60};

      // Hot towers: >1GeV / >0.1GeV

      if ( nhot1[0] > nhot1_warn[0] || nhot1[1] > nhot1_warn[1] ||
           nhot1[2] > nhot1_warn[2] || nhot1[3] > nhot1_warn[3] )
        EMCWstat = 2;
      if ( nhot1[0] > nhot1_err[0] || nhot1[1] > nhot1_err[1] ||
           nhot1[2] > nhot1_err[2] || nhot1[3] > nhot1_err[3] )
        EMCWstat = 1;

      if ( nhot1[4] > nhot1_warn[4] || nhot1[5] > nhot1_warn[5] )
        EMCPBSCEstat = 2;
      if ( nhot1[4] > nhot1_err[4] || nhot1[5] > nhot1_err[5] )
        EMCPBSCEstat = 1;

      if ( nhot1[6] > nhot1_warn[6] || nhot1[7] > nhot1_warn[7] )
        EMCPBGLEstat = 2;
      if ( nhot1[6] > nhot1_err[6] || nhot1[7] > nhot1_err[7] )
        EMCPBGLEstat = 1;

      // No hits? Though it depends on trigger setup...

      if ( nmean01[0] / nEvents < 8e-5 || nmean01[1] / nEvents < 8e-5 ||
           nmean01[2] / nEvents < 8e-5 || nmean01[3] / nEvents < 6e-5 )
        EMCWstat |= 4;

      if ( nmean01[4] / nEvents < 8e-5 || nmean01[5] / nEvents < 8e-5 )
        EMCPBSCEstat |= 4;

      if ( nmean01[6] / nEvents < 2.5e-5 || nmean01[7] / nEvents < 2.5e-5 )
        EMCPBGLEstat |= 4;

    } // if( ! ppdata )

  textFile << "EMCW Status: " << EMCWstat << endl;
  textFile << "EMCEPbSc Status: " << EMCPBSCEstat << endl;
  textFile << "EMCEPbgl Status: " << EMCPBGLEstat << endl;
  textFile << "binary status word: ";

  /* binary status word (4 bytes):
     most significant byte: unused
     2nd most significant byte: bad timing (photon peak far from 0)
     3rd most significant byte: too many noisy towers
     least significant byte: too many dead towers
     Each bit within a byte corresponds to one sector, 
     in the following order: E0 E1 E2 E3 W3 W2 W1 W0

     /-unused\           /-noisy-\
     0000 0000 0010 1100 0001 1000 1100 0000
     \-timing/           \--dead-/

     So, in this example, timing is not very good in 
     E2, W3 and W2; E3 and W3 have too many noisy towers; and 
     E0 and E1 have too many dead towers.
     The decimal status word is 2889920.
  */


  for (int i=sizeof(unsigned long int)*8-1; i>=0; i--) 
    {
      int bit = ((EMCstat >> i) & 1);
      textFile << bit;      
      if(i%4==0) textFile << " ";
    }
  textFile << endl;

  statusFile << EMCPBGLEstat  << " " << EMCPBSCEstat << " " << EMCWstat << " ";

  CommitToQADatabase("Emc", "W0: <hits/tower>", meanoccW0, 0.0);
  CommitToQADatabase("Emc", "W1: <hits/tower>", meanoccW1, 0.0);
  CommitToQADatabase("Emc", "W2: <hits/tower>", meanoccW2, 0.0);
  CommitToQADatabase("Emc", "W3: <hits/tower>", meanoccW3, 0.0);
  CommitToQADatabase("Emc", "E0: <hits/tower>", meanoccE0, 0.0);
  CommitToQADatabase("Emc", "E1: <hits/tower>", meanoccE1, 0.0);
  CommitToQADatabase("Emc", "E2: <hits/tower>", meanoccE2, 0.0);
  CommitToQADatabase("Emc", "E3: <hits/tower>", meanoccE3, 0.0);

 CommitToQADatabase("Emc", "W0: dead towers", (float)ndeadW0, 0.0);
 CommitToQADatabase("Emc", "W1: dead towers", (float)ndeadW1, 0.0);
 CommitToQADatabase("Emc", "W2: dead towers", (float)ndeadW2, 0.0);
 CommitToQADatabase("Emc", "W3: dead towers", (float)ndeadW3, 0.0);
 CommitToQADatabase("Emc", "E0: dead towers", (float)ndeadE0, 0.0);
 CommitToQADatabase("Emc", "E1: dead towers", (float)ndeadE1, 0.0);
 CommitToQADatabase("Emc", "E2: dead towers", (float)ndeadE2, 0.0);
 CommitToQADatabase("Emc", "E3: dead towers", (float)ndeadE3, 0.0);

 CommitToQADatabase("Emc", "W0: noisy towers", (float)nnoisyW0, 0.0);
 CommitToQADatabase("Emc", "W1: noisy towers", (float)nnoisyW1, 0.0);
 CommitToQADatabase("Emc", "W2: noisy towers", (float)nnoisyW2, 0.0);
 CommitToQADatabase("Emc", "W3: noisy towers", (float)nnoisyW3, 0.0);
 CommitToQADatabase("Emc", "E0: noisy towers", (float)nnoisyE0, 0.0);
 CommitToQADatabase("Emc", "E1: noisy towers", (float)nnoisyE1, 0.0);
 CommitToQADatabase("Emc", "E2: noisy towers", (float)nnoisyE2, 0.0);
 CommitToQADatabase("Emc", "E3: noisy towers", (float)nnoisyE3, 0.0);

 CommitToQADatabase("Emc", "W0: <TOF>", emct0[0], 0.0);
 CommitToQADatabase("Emc", "W1: <TOF>", emct0[1], 0.0);
 CommitToQADatabase("Emc", "W2: <TOF>", emct0[2], 0.0);
 CommitToQADatabase("Emc", "W3: <TOF>", emct0[3], 0.0);
 CommitToQADatabase("Emc", "E0: <TOF>", emct0[4], 0.0);
 CommitToQADatabase("Emc", "E1: <TOF>", emct0[5], 0.0);
 CommitToQADatabase("Emc", "E2: <TOF>", emct0[6], 0.0);
 CommitToQADatabase("Emc", "E3: <TOF>", emct0[7], 0.0);

 cout << "done..." << endl;
  return 0;
}





