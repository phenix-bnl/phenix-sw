
#ifndef __REACTIONPLANECALIB_H__
#define __REACTIONPLANECALIB_H__

#include <string>
#include "RpConst.h"

class PdbCalBank;
class PdbApplication;
class PHCompositeNode;
class PHGlobal;
class PHTimeStamp;

class ReactionPlaneCalib
{
 public:
  ReactionPlaneCalib();
  virtual ~ReactionPlaneCalib() {}

  void Reset();

  float Flattening(const int idet, const int ihar, const int imul, 
                   const int izps, const float psi);

  // Fetch calibration constatns
  int Fetch(const int runNumber); 
  void Fetch(const PHTimeStamp& tstart);
  int Fetch(const char* filename);

  // Update parameters to DB
  void Update(const PHTimeStamp& tstart, const PHTimeStamp& tstop);
  void Update(const int beginrun, const int endrun);

  // Write parameter to file
  void Write(const char* filename);

  float GetSumXmean(const int idet, const int ihar, const int imul);
  float GetSumXsigma(const int idet, const int ihar, const int imul);
  float GetSumYmean(const int idet, const int ihar, const int imul);
  float GetSumYsigma(const int idet, const int ihar, const int imul);

  float GetFlatCos(const int idet, const int ihar, const int imul,
                   const int izps, const int iord);
  float GetFlatSin(const int idet, const int ihar, const int imul,
                   const int izps, const int iord);

  // Utility function to get centrality and z-vertex bin
  int GetCentrality(PHCompositeNode *topNode);
  int GetCentralityBin(const int cent);
  int GetZVertexBin(const float bbcz);
  int GetCentralityBin2(const int cent);
  int GetZVertexBin2(const float bbcz);

  void Verbosity(const int val) { verbosity = val; }
  void SetRunNumber(const int val) { RunNumber = val; }

 private:

  void Fetch(PdbCalBank* rpBank);
  void Update(PdbCalBank* rpBank, PdbApplication* application);

  // Back ID
  //  1 =  Run4, Run5
  //  2 =  Run7 -
  int GetBankID() const;

  // DB length
  int GetDBLength(const int bankid) const;

  // Correction for SumX/Y variables
  float SumXmean[RP::NDET][RP::NHAR][RP::NMUL];
  float SumXsigma[RP::NDET][RP::NHAR][RP::NMUL];
  float SumYmean[RP::NDET][RP::NHAR][RP::NMUL];
  float SumYsigma[RP::NDET][RP::NHAR][RP::NMUL];
  // Correction for SumX/Y variables for RXN
  float SumXmean2[RP::NDET2-RP::NDET][RP::NHAR2][RP::NMUL2];
  float SumXsigma2[RP::NDET2-RP::NDET][RP::NHAR2][RP::NMUL2];
  float SumYmean2[RP::NDET2-RP::NDET][RP::NHAR2][RP::NMUL2];
  float SumYsigma2[RP::NDET2-RP::NDET][RP::NHAR2][RP::NMUL2];

  // Correction for flattening
  float FlatCos[RP::NDET][RP::NHAR][RP::NMUL][RP::NZPS][RP::NORD];
  float FlatSin[RP::NDET][RP::NHAR][RP::NMUL][RP::NZPS][RP::NORD];
  // Correction for flattening
  float FlatCos2[RP::NDET2-RP::NDET][RP::NHAR2][RP::NMUL2][RP::NZPS2][RP::NORD2];
  float FlatSin2[RP::NDET2-RP::NDET][RP::NHAR2][RP::NMUL2][RP::NZPS2][RP::NORD2];

  int verbosity;
  int RunNumber;

  std::string databasename;
};

#endif
