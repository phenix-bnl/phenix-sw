#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TF1.h"
#include "summaryQA.h"
#include <fstream>
#include <iostream>
#include <cmath>

using namespace std;
int QASummary::processBbc()
{
  cout << "Bbc..." << endl;
  fstream textFile(outputName1, ios::in);
  if (textFile)
    {
      textFile.close();
      textFile.open(outputName1, ios::app | ios::out);
    }
  fstream statusFile(outputName2, ios::in);
  if (statusFile)
    {
      statusFile.close();
      statusFile.open(outputName2, ios::app | ios::out);
    }

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- BBC QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;

  Int_t bbcstatus = 0;

  TH1F *bbct0 = (TH1F *) qafile->Get ("bbct0");
  TH1F *bbcvtx = (TH1F *) qafile->Get ("bbcvtx");
  TH1F *bbcnpmt = (TH1F *) qafile->Get ("bbcnpmt");
  TH1F *bbcQ = (TH1F *) qafile->Get ("bbcQ");
  TH2F *bbcQnQs = (TH2F *) qafile->Get ("bbcQnQs");
  TH2F *bbcvtxt0 = (TH2F *) qafile->Get ("bbcvtxt0");

  TProfile *pqsqn = NULL;

  float EndPoint = 0;
  int nHit = 0;
  // add here analysis and printout results to textFile

  if (bbct0 == NULL || bbcvtx == NULL || bbcnpmt == NULL ||
      bbcQ == NULL || bbcQnQs == NULL || bbcvtxt0 == NULL)
    {
      textFile << " BBC ERROR: could not extract histograms" << endl;
      bbcstatus = 1;
    }
  else
    {
      float BbcT0Mean = bbct0->GetMean ();
      float BbcT0RMS = bbct0->GetRMS ();
      float BbcZvtxMean = bbcvtx->GetMean ();
      float BbcZvtxRMS = bbcvtx->GetRMS ();

      TH1D *qs = bbcQnQs->ProjectionX();
      TH1D *qn = bbcQnQs->ProjectionY();
      pqsqn = (TProfile*)bbcQnQs->ProfileX("pqsqn");

      float chargeN = qs->GetMean ();
      float chargeS = qn->GetMean ();
      nHit = (int) bbcnpmt->GetMean ();
      float Sum = 0.;
      int MaxBin = 0, Norm = 0;


      for (int i = 200; i > 0; i--)
        {
          if (bbcQ->GetBinContent(i) > 0)
            {
              MaxBin = i;
              break;
            }
        }


      for (int i = MaxBin; i > MaxBin - 5; i--)
        {
          Norm += (int) bbcQ->GetBinContent(i);
          Sum += i * 10.0 * bbcQ->GetBinContent(i);
        }

      if (Norm > 0)
        {
          EndPoint = Sum / (float)Norm;
        }
      else
        {
          EndPoint = 0.;
          cout << "BBC error Endpoint set to zero" << endl;
        }

      textFile << " BBC TimeZero distribution [ns] = " << BbcT0Mean << " +- " <<
	BbcT0RMS << endl;
      textFile << " BBC Z Vertex distribution [cm] = " << BbcZvtxMean << " +- "
	       << BbcZvtxRMS << endl;

      if (fabs (BbcT0Mean) > 10)
        bbcstatus = 1;
      if (chargeN == 0 || chargeS == 0)
        bbcstatus = 1;
      if (nHit == 0 || nHit > 128)
        bbcstatus = 1;
      if (EndPoint < 1550 || EndPoint > 1650)
        bbcstatus = bbcstatus + 2;

      textFile << " EndPoint(mip) = " << EndPoint << endl
	       << " BBC Status = " << bbcstatus << endl;
      statusFile << bbcstatus << " ";
    }

  textFile.close();
  statusFile.close();

  // Here I'm including some variables people are asking about BBC. Cesar 02/24/2005
  TF1 *fqsqn = new TF1("fqsqn","pol1",0,300);
  pqsqn->Fit(fqsqn,"QNR");
  float qsqnrate = fqsqn->GetParameter(1);
  float qsqnrate_error = fqsqn->GetParError(1);
  delete fqsqn;

  float nHit_error = 0.;
  if (bbcnpmt->GetEntries() > 0)
    nHit_error = bbcnpmt->GetRMS() / bbcnpmt->GetEntries();

  float nEvents =  bbcvtx->GetEntries();

  CommitToQADatabase("Bbc", "Mean T0 (ns)", bbct0->GetMean(), bbct0->GetRMS());
  CommitToQADatabase("Bbc", "Mean Z Vertex (cm)", bbcvtx->GetMean(), bbcvtx->GetRMS());
  CommitToQADatabase("Bbc", "EndPoint (mip)", EndPoint, 0.0);
  CommitToQADatabase("Bbc", "QsQn rate", qsqnrate, qsqnrate_error);
  CommitToQADatabase("Bbc", "Number of PMT hits", (float)nHit, nHit_error);
  CommitToQADatabase("Bbc", "Number of Events", (float)nEvents, 0.);

  cout << "    ...done." << endl;
  return 0;
}
