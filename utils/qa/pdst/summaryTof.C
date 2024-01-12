#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <summaryQA.h>
#include <fstream>
#include <iostream>

#include <QaBankID.h>  // enumerates unique bank ID's

using namespace std;
int QASummary::processTof()
{

  cout << "Tof..." << endl;
fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- TOF QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  TH1F *tofhist[4];
  tofhist[0] = (TH1F *) qafile->Get ("tofDist");
  tofhist[1] = (TH1F *) qafile->Get ("tofELoss");
  tofhist[2] = (TH1F *) qafile->Get ("tofProY");
  tofhist[3] = (TH1F *) qafile->Get ("tofProZ");
  TH2F* tofHitYZ   = (TH2F *) qafile->Get ("tofYZ_north");

  Float_t tofentries[4], tofmean[4], tofrms[4], tofhitYZ;

  for (Int_t ih = 0; ih < 4; ih++)
  {
    tofentries[ih] = tofhist[ih]->GetEntries ();
    tofmean[ih] = tofhist[ih]->GetMean ();
    tofrms[ih] = tofhist[ih]->GetRMS ();
  }
  tofhitYZ = (float)tofHitYZ->GetEntries ();

  Int_t tofstatus;
  tofstatus = 0;

  if (tofmean[0] < 15.0 || tofmean[0] > 25.0)
    tofstatus = 1;
  if (tofrms[0] > 10.0)
    tofstatus = 1;
  if (tofmean[1] < 0.001 || tofmean[1] > 0.004)
    tofstatus = 1;
  if (tofmean[2] < -3.0 || tofmean[2] > 3.0)
    tofstatus = 1;
  if (tofrms[2] > 25.0)
    tofstatus = 1;
  if (tofmean[3] < -3.0 || tofmean[3] > 3.0)
    tofstatus = 1;
  if (tofrms[3] > 25.0)
    tofstatus = 1;
  if (tofhitYZ<100)
    tofstatus = 1;

  textFile << " TOF: TOF-t0bbc Mean [ns]   = " << tofmean[0] << endl;
  textFile << " TOF: TOF-t0bbc RMS  [ns]   = " << tofrms[0] << endl;
  textFile << " TOF: Eloss Mean [GeV]      = " << tofmean[1] << endl;
  textFile << " TOF: Eloss RMS  [GeV]      = " << tofrms[1] << endl;
  textFile << " TOF: dY Mean (cgl-tof)[cm] = " << tofmean[2] << endl;
  textFile << " TOF: dY RMS  (cgl-tof)[cm] = " << tofrms[2] << endl;
  textFile << " TOF: dZ Mean (cgl-tof)[cm] = " << tofmean[3] << endl;
  textFile << " TOF: dZ RMS  (cgl-tof)[cm] = " << tofrms[3] << endl;
  textFile << " TOF: # TOF hit in north (should be >100)  = " << tofhitYZ << endl;
  textFile << " TOF Status = " << tofstatus << endl;
  statusFile << tofstatus << " ";

  CommitToQADatabase("Tof", "<TOF-BBC t0> (ns)", tofmean[0], tofrms[0]);
  CommitToQADatabase("Tof", "<Eloss> (GeV)", tofmean[1],tofrms[1] );
  CommitToQADatabase("Tof", "<dY> (cgl-tof) [cm]",  tofmean[2], tofrms[2]);
  CommitToQADatabase("Tof", "<dZ> (cgl-tof) [cm]", tofmean[3],  tofrms[3]);
  CommitToQADatabase("Tof", "Hits in North", tofhitYZ, 0.0);

 cout << "    ...done." << endl;
 return 0;
}
