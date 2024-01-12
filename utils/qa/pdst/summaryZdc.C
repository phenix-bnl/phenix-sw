#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "summaryQA.h"

#include <fstream>
#include <iostream>
#include <cmath>

using namespace std;

//void qasummaryZdc(TFile* qafile, char* outputName1, char* outputName2, int runNumber)
int QASummary::processZdc()
{
  cout << "Zdc..." << endl;

  fstream textFile(outputName1, ios::in);
  if (textFile)
    {
      textFile.close();
      textFile.open(outputName1,ios::app|ios::out);
    }
  fstream statusFile(outputName2, ios::in);
  if (statusFile)
    {
      statusFile.close();
      statusFile.open(outputName2,ios::app|ios::out);
    }

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- ZDC QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;

  Int_t zdcstatus = 0;

  TH1F *zdcenorth = (TH1F *) qafile->Get ("zdcenorth");
  TH1F *zdcesouth = (TH1F *) qafile->Get ("zdcesouth");
  TH1F *zdcesum =   (TH1F *) qafile->Get ("zdcesum");
  TH2F *zdcEnEs =   (TH2F *) qafile->Get ("zdcEnEs");
  TH1F *zdcbbcvtx = (TH1F *) qafile->Get ("zdcbbcvtx");
  TH1F *zdcbbct0 =  (TH1F *) qafile->Get ("zdcbbct0");

  Double_t vtxmean = 0.;
  Double_t vtxrms = 0.;
  Double_t t0mean = 0.;
  Double_t t0rms = 0.;

  float enorthmean = 0.0;
  float esouthmean = 0.0;


  // check that the histograms exist
  if (zdcenorth == NULL || zdcesouth == NULL || zdcesum == NULL ||
      zdcEnEs == NULL || zdcbbcvtx == NULL || zdcbbct0 == NULL )
  {
    textFile << " ZDC ERROR: could not extract histograms" << endl;
    zdcstatus = 1;
  }
  else
  {
    // check the z vertex reconstruction
    vtxrms = zdcbbcvtx->GetRMS();
    vtxmean = zdcbbcvtx->GetMean();
    //-** for Au+Au, vtxrms ~ 1.5-3.5
    if ( vtxrms < 1.5 || vtxrms > 10.0 || fabs(vtxmean) > 4.)
    {
      textFile << " ZDC ERROR: bad vertex or mean " << vtxmean << " rms " << vtxrms << endl;
      zdcstatus = 1;
    }

    // check the time zero reconstruction
    t0rms = zdcbbct0->GetRMS();
    t0mean = zdcbbct0->GetMean();
    //-** for Au+Au, t0rms ~ 0.3
    if (t0rms < .05 || t0rms > 1.0 || fabs(t0mean) > .4 )
    {
      textFile << " ZDC ERROR: bad tzero, mean " << t0mean << " rms " << t0rms << endl;
      zdcstatus = 1;
    }

    // check the energy scale ?? (needs to have good trigger info)

    enorthmean = zdcenorth->GetMean();
    esouthmean = zdcesouth->GetMean();

    textFile << " ZDC-BBC Vertex: " << vtxmean << " +- " << vtxrms << endl;
    textFile << " ZDC-BBC     T0: " << t0mean << " +- " << t0rms << endl;
    textFile << " ZDC     Energy: n " << enorthmean
             << " s " << esouthmean << endl;
    textFile << " ZDC Status = " << zdcstatus << endl;
    statusFile << zdcstatus << " ";
  }

  CommitToQADatabase("Zdc", "ZDC-BBC Vertex", (float)vtxmean, (float)vtxrms);
  CommitToQADatabase("Zdc", "ZDC-BBC T0", (float)t0mean, (float)t0rms);
  CommitToQADatabase("Zdc", "<North Energy>", enorthmean, (float)zdcenorth->GetRMS());
  CommitToQADatabase("Zdc", "<South Energy>", esouthmean, (float)zdcesouth->GetRMS());

  cout << "    ...done." << endl;

  return 0;
}

