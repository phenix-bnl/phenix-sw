
{
  bool ppdata = false;
  if ( runNumber > 35000 )
    ppdata = true;

  textFile << " ----------------------------------------------------" << endl;
  if ( ppdata )
    textFile << " -- EMC QA Summary for pp --" << endl;
  else
    textFile << " -- EMC QA Summary for AuAu --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  textFile << " Run number: " << runNumber << endl;

  emcWest01ETot = (TH1F *) qafile->Get ("emcWest01ETot");
  emcWest23ETot = (TH1F *) qafile->Get ("emcWest23ETot");
  emcEastETot = (TH1F *) qafile->Get ("emcEastETot");
  emcPbGlETot = (TH1F *) qafile->Get ("emcPbGlETot");

  emcWest01E = (TH1F *) qafile->Get ("emcWest01E");
  emcWest23E = (TH1F *) qafile->Get ("emcWest23E");
  emcEastE = (TH1F *) qafile->Get ("emcEastE");
  emcPbGlE = (TH1F *) qafile->Get ("emcPbGlE");

  emcWest01E1 = (TH1F *) qafile->Get ("emcWest01E1");
  emcWest23E1 = (TH1F *) qafile->Get ("emcWest23E1");
  emcEastE1 = (TH1F *) qafile->Get ("emcEastE1");
  emcPbGlE1 = (TH1F *) qafile->Get ("emcPbGlE1");

  emcWest0Time = (TH1F *) qafile->Get ("emcWest0Time");
  emcWest1Time = (TH1F *) qafile->Get ("emcWest1Time");
  emcWest2Time = (TH1F *) qafile->Get ("emcWest2Time");
  emcWest3Time = (TH1F *) qafile->Get ("emcWest3Time");
  emcEast2Time = (TH1F *) qafile->Get ("emcEast2Time");
  emcEast3Time = (TH1F *) qafile->Get ("emcEast3Time");
  emcPbGl0Time = (TH1F *) qafile->Get ("emcPbGl0Time");
  emcPbGl1Time = (TH1F *) qafile->Get ("emcPbGl1Time");

  emcW0YZ = (TH2F *) qafile->Get ("emcW0YZ");
  emcW1YZ = (TH2F *) qafile->Get ("emcW1YZ");
  emcW2YZ = (TH2F *) qafile->Get ("emcW2YZ");
  emcW3YZ = (TH2F *) qafile->Get ("emcW3YZ");
  emcE0YZ = (TH2F *) qafile->Get ("emcE0YZ");
  emcE1YZ = (TH2F *) qafile->Get ("emcE1YZ");
  emcE2YZ = (TH2F *) qafile->Get ("emcE2YZ");
  emcE3YZ = (TH2F *) qafile->Get ("emcE3YZ");

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

  Int_t nEvents;
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

  maxTimeBin = emcWest0Time.GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f1 = new TF1("f1", "gaus", xlo, xhi);
  emcWest0Time->Fit("f1", "RQN");
  f1->GetParameters(fitparams);
  chisquare = f1->GetChisquare();
  nDoF = f1->GetNDF();

  textFile << " W0 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (chisquare / nDoF) << " " << emcWest0Time->GetEntries() << endl;

  maxTimeBin = emcWest1Time.GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f1 = new TF1("f1", "gaus", xlo, xhi);
  emcWest1Time->Fit("f1", "RQN");
  f1->GetParameters(fitparams);
  chisquare = f1->GetChisquare();
  nDoF = f1->GetNDF();

  textFile << " W1 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest1Time->GetEntries() << endl;

  maxTimeBin = emcWest2Time.GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f2 = new TF1("f2", "gaus", xlo, xhi);
  emcWest2Time->Fit("f2", "RQN");
  f2->GetParameters(fitparams);
  chisquare = f2->GetChisquare();
  nDoF = f2->GetNDF();

  textFile << " W2 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest2Time->GetEntries() << endl;

  maxTimeBin = emcWest3Time.GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f2 = new TF1("f2", "gaus", xlo, xhi);
  emcWest3Time->Fit("f2", "RQN");
  f2->GetParameters(fitparams);
  chisquare = f2->GetChisquare();
  nDoF = f2->GetNDF();

  textFile << " W3 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcWest3Time->GetEntries() << endl;

  maxTimeBin = emcEast3Time.GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f3 = new TF1("f3", "gaus", xlo, xhi);
  emcEast3Time->Fit("f3", "RQN");
  f3->GetParameters(fitparams);
  chisquare = f3->GetChisquare();
  nDoF = f3->GetNDF();

  textFile << " E3 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcEast3Time->GetEntries() << endl;

  maxTimeBin = emcEast2Time.GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f3 = new TF1("f3", "gaus", xlo, xhi);
  emcEast2Time->Fit("f3", "RQN");
  f3->GetParameters(fitparams);
  chisquare = f3->GetChisquare();
  nDoF = f3->GetNDF();

  textFile << " E2 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcEast2Time->GetEntries() << endl;

  maxTimeBin = emcPbGl1Time.GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f4 = new TF1("f4", "gaus", xlo, xhi);
  emcPbGl1Time->Fit("f4", "RQN");
  f4->GetParameters(fitparams);
  chisquare = f4->GetChisquare();
  nDoF = f4->GetNDF();


  textFile << " E1 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcPbGl1Time->GetEntries() << endl;

  maxTimeBin = emcPbGl0Time.GetMaximumBin();
  xcent = min + maxTimeBin * inc - inc / 2.;
  xlo = xcent - 1.15;
  xhi = xcent + 0.65;

  TF1 *f4 = new TF1("f4", "gaus", xlo, xhi);
  emcPbGl0Time->Fit("f4", "RQN");
  f4->GetParameters(fitparams);
  chisquare = f4->GetChisquare();
  nDoF = f4->GetNDF();

  textFile << " E0 Tof Mean, Width, Chi2/NDF, No.Ent: " << fitparams[1] << " " << fitparams[2] << " " << (nDoF != 0 ? chisquare / nDoF : -1) << " " << emcPbGl0Time->GetEntries() << endl;

  TF1 *f5 = new TF1("f5", "expo(0)+gaus(2)", 0.17, 0.9);
  f5.SetParameters(8., -4.4, 800., 0.28, 0.035);
  f5.SetParLimits(2, 0., 100000.);
  f5.SetParLimits(3, 0.2, 0.4);
  f5.SetParLimits(4, 0.0, 0.06);

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

  f6 = new TF1("f6", "expo", 0.4, 2.0);

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

  int EMCWstat = 0, EMCPBSCEstat = 0, EMCPBGLEstat = 0;

  if ( ! ppdata )
    {

      Int_t nentW0[72][36];
      Int_t nentW1[72][36];
      Int_t nentW2[72][36];
      Int_t nentW3[72][36];
      Int_t nentE3[72][36];
      Int_t nentE2[72][36];
      Int_t nentE1[96][48];
      Int_t nentE0[96][48];

      Int_t ndeadW0 = 0, ndeadW1 = 0, ndeadW2 = 0, ndeadW3 = 0;
      Int_t ndeadE0 = 0, ndeadE1 = 0, ndeadE2 = 0, ndeadE3 = 0;

      Int_t nnoisyW0 = 0, nnoisyW1 = 0, nnoisyW2 = 0, nnoisyW3 = 0;
      Int_t nnoisyE0 = 0, nnoisyE1 = 0, nnoisyE2 = 0, nnoisyE3 = 0;

      Int_t totentW0, totentW1, totentW2, totentW3;
      Int_t totentE0, totentE1, totentE2, totentE3;
      Float_t meanoccW0, meanoccW1, meanoccW2, meanoccW3;
      Float_t meanoccE0, meanoccE1, meanoccE2, meanoccE3;

      totentW0 = emcW0YZ->GetEntries();
      totentW1 = emcW1YZ->GetEntries();
      totentW2 = emcW2YZ->GetEntries();
      totentW3 = emcW3YZ->GetEntries();
      totentE0 = emcE0YZ->GetEntries();
      totentE1 = emcE1YZ->GetEntries();
      totentE2 = emcE2YZ->GetEntries();
      totentE3 = emcE3YZ->GetEntries();

      meanoccW0 = totentW0 / 2592.;
      meanoccW1 = totentW1 / 2592.;
      meanoccW2 = totentW2 / 2592.;
      meanoccW3 = totentW3 / 2016.;  // maximum number of installed channels W3 run2
      meanoccE3 = totentE3 / 2592.;
      meanoccE2 = totentE2 / 2592.;
      meanoccE1 = totentE1 / 4608.;
      meanoccE0 = totentE0 / 4608.;

      for (int i = 1; i <= 96; i++)
        {
          for (int j = 1; j <= 48; j++)
            {
              if (i <= 72 && j <= 36)
                {
                  nentW0[i - 1][j - 1] = emcW0YZ->GetBinContent(i, j);
                  if (nentW0[i - 1][j - 1] == 0 )
                    ndeadW0++;
                  if (nentW0[i - 1][j - 1] > 4.*meanoccW0)
                    nnoisyW0++;

                  nentW1[i - 1][j - 1] = emcW1YZ->GetBinContent(i, j);
                  if (nentW1[i - 1][j - 1] == 0 )
                    ndeadW1++;
                  if (nentW1[i - 1][j - 1] > 4.*meanoccW1)
                    nnoisyW1++;

                  nentW2[i - 1][j - 1] = emcW2YZ->GetBinContent(i, j);
                  if (nentW2[i - 1][j - 1] == 0 )
                    ndeadW2++;
                  if (nentW2[i - 1][j - 1] > 4.*meanoccW2)
                    nnoisyW2++;

                  nentW3[i - 1][j - 1] = emcW3YZ->GetBinContent(i, j);
                  if (nentW3[i - 1][j - 1] == 0 )
                    ndeadW3++;
                  if (nentW3[i - 1][j - 1] > 3.*meanoccW3)
                    nnoisyW3++;

                  nentE3[i - 1][j - 1] = emcE3YZ->GetBinContent(i, j);
                  if (nentE3[i - 1][j - 1] == 0 )
                    ndeadE3++;
                  if (nentE3[i - 1][j - 1] > 4.*meanoccE3)
                    nnoisyE3++;

                  nentE2[i - 1][j - 1] = emcE2YZ->GetBinContent(i, j);
                  if (nentE2[i - 1][j - 1] == 0 )
                    ndeadE2++;
                  if (nentE2[i - 1][j - 1] > 4.*meanoccE2)
                    nnoisyE2++;

                }

              nentE1[i - 1][j - 1] = emcE1YZ->GetBinContent(i, j);
              if (nentE1[i - 1][j - 1] == 0 )
                ndeadE1++;
              if (nentE1[i - 1][j - 1] > 5.*meanoccE1)
                nnoisyE1++;

              nentE0[i - 1][j - 1] = emcE0YZ->GetBinContent(i, j);
              if (nentE0[i - 1][j - 1] == 0 )
                ndeadE0++;
              if (nentE0[i - 1][j - 1] > 5.*meanoccE0)
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
          if ( ndeadW3 > 1204 || (ndeadW3 > 674 && ndeadW3 < 1150))
            {
              EMCWstat = 3;
            }

          if (ndeadW0 > 15 || ndeadW1 > 25 || ndeadW2 > 15)
            {
              EMCWstat = 2;

              if (ndeadW0 > 39 || ndeadW1 > 49 || ndeadW2 > 39)
                {
                  EMCWstat = 1;
                }


              if (ndeadE2 > 40 || ndeadE3 > 15)
                {
                  EMCPBSCEstat = 2;

                  if (ndeadE2 > 64 || ndeadE3 > 39)
                    {
                      EMCPBSCEstat = 1;
                    }

                }
            }

          if (nEvents > 4000)
            {
              if (ndeadE0 > 89 || ndeadE1 > 114)
                {
                  EMCPBGLEstat = 1;
                }

            }
        }

    } // if( ! ppdata )

  else
    {

      int nx, ny;
      float nmean01[8], nmean1[8], nthresh;
      int nhot01[8], nhot1[8];
      char* sname[8] = {"W0", "W1", "W2", "W3", "E3", "E2", "E1", "E0"};

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
      int nhot01_warn[8] = {0, 0, 5, 30, 6, 1, 20, 40};
      int nhot01_err[8] = {3, 3, 8, 50, 10, 4, 30, 60};

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

  statusFile << EMCPBGLEstat << " " << EMCPBSCEstat << " " << EMCWstat << " ";
}





