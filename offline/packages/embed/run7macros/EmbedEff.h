#ifndef EMBEDEFF_H
#define EMBEDEFF_H

#include "TFile.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TCanvas.h"

#ifndef __CINT__
#include "boost/array.hpp"
#include "boost/multi_array.hpp"
#endif

#include <string>
#include <sstream>
#include <vector>
#include <map>


typedef std::map<int, TNtuple*> NtupleMap;
typedef std::map<int, std::pair<float, float> > PairMap;
typedef std::map<int, std::string> StrMap;
#ifndef __CINT__
typedef boost::multi_array<TH1F*, 3> TH1array3;
typedef boost::multi_array<TH1F*, 4> TH1array4;
typedef TH1array3::index Index;
#endif

class EmbedEff
{
 public:
  // If an empty string is passed for the filename, 
  // no output file will be written.
  EmbedEff(int ncentbins,
	   int nmombins, 
	   std::string filename = "embedEff.root",
	   std::string option = "RECREATE"
	   );
  
  ~EmbedEff();
  
  static const int fNflavor = 4;
  static const int fNtrackType = 4;
  enum EFlavor {PION, 
		KAON, 
		PROT, 
		ALL3};
  enum ETrackType {TYPE_G, 
		   TYPE_S, 
		   TYPE_R,
		   TYPE_E}; // Not really a track type, but....
  enum EVerbosity {QUIET,
		   MEDIUM,
		   FULL};
  void setVerbosity(int v) { fVerbosity = v; }
  int setupCentBins(int nCentBins, float centLo[], float centHi[]);
  int setupMomBins(int nMomBins, float momLo[], float momHi[]);
  int addNtuple(std::string fileName, 
		std::string ntName,
		EFlavor fl = PION);
  void setEvalType(int type = 2);
  void setPC2sigmaCut(float sig = 3.0);
  void setPC3sigmaCut(float sig = 3.0);
  void setDchZedCut(float zed = 80.0);

  void setDchQualCut(int qual1 = 63);
  void setDchQual1orQual2Cuts(int qual1 = 31, int qual2 = 63);

  void setTrackCutExpression(ETrackType tt);
  void fillMomHisto(EFlavor fl, ETrackType tt, int centbin,
		    std::string binning = "(10, 0, 10)");
  void fillCenHisto(EFlavor fl, ETrackType tt, int ptbin,
		    std::string binning = "(10, 0, 100)");
  void fillHistos();
  void makeMomEffHisto(EFlavor fl, int centbin);
  void makeCenEffHisto(EFlavor fl, int centbin);
  void drawHistos();

 private:
  const int fNcent, fNmom;
  int fVerbosity;
  int fEvalType;
  float fPC2sigmaCut, fPC3sigmaCut;
  float fZedCut;
  int fQual1, fQual2;
  NtupleMap fNtup; // hash ntuple to an EFlavor value
  PairMap fCentBin, fMomBin;
  StrMap fCut;
  std::ostringstream fss; // For ubiquitous scratch use.
                          // Always reset first!
  boost::array<std::string, fNtrackType> fLetter;
  boost::array<std::string, fNflavor> fLabel;

  TFile* fOutFile;
  bool fShouldSave;
#ifndef __CINT__
  TH1array3 fHmom;    // [EFlavor][ETrackType][centbin]
  TH1array3 fHcen;    // [EFlavor][ETrackType][mombin]
  TH1array3 fHpc3z;   // [ETrackType][centbin][mombin]
  TH1array3 fHpc3p;   // [ETrackType][centbin][mombin]
#endif
  
};

#endif
