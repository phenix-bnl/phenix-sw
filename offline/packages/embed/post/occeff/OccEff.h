#ifndef OCCEFF_H
#define OCCEFF_H
#include "TH1.h"
#include "TH2.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TFile.h"
#include <string>
#include <vector>

using namespace std;

class OccEff
{
 public:
  OccEff(int ncb, double cb[], int npt, double pt[]);
  ~OccEff(){}

  const int nPid;
  const int nType;
  enum ePid  {PION, KAON, PROT, ALL3};
  enum eType {S, R, E, G}; 
  vector<char*> sPid;
  vector<char*> sType;

  void bookHistos();
  int loopTree(TFile* inFile, string treeName = "EmbedMcRecoTrack", ePid pid = PION);
  void setPC3sigmaCut(double value = 3.0) { nSigma = value; }
  void setDchZedCut(double value = 80.0) {zedCut = value; } // cm
  double spectralWeight(double pt);

 private:
  const int nCentBins;
  const int nPtBins;
  vector<double> centBin;
  vector<double> ptBin;
  vector<string> sCentBin;
  vector<string> sPtBin;
  TH1F* hCentBin;
  TH1F* hPtBin;

  TFile* outFile;
  vector<vector<vector<TH1F*> > > hMom;  // [pid][type][cent]
  vector<vector<vector<TH1F*> > > hCen;  // [pid][type][pt]

  double nSigma;
  double zedCut;
};
#endif // OCCEFF_H
