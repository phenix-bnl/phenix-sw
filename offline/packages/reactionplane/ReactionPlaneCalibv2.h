//
// This is for Run-8 d+Au reaction plane calibration
// 

#ifndef __REACTIONPLANECALIBV2_H__
#define __REACTIONPLANECALIBV2_H__

#include <string>
#include "RpConst.h"

class PdbCalBank;
class PdbApplication;
class PHCompositeNode;
class PHGlobal;
class PHTimeStamp;

class ReactionPlaneCalibv2
{
 public:
  ReactionPlaneCalibv2();
  virtual ~ReactionPlaneCalibv2() {}

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

  float GetSumXmean(const int idet, const int ihar, const int imul,
                   const int izps);
  float GetSumXsigma(const int idet, const int ihar, const int imul,
                   const int izps);
  float GetSumYmean(const int idet, const int ihar, const int imul,
                   const int izps);
  float GetSumYsigma(const int idet, const int ihar, const int imul,
                   const int izps);

  float GetFlatCos(const int idet, const int ihar, const int imul,
                   const int izps, const int iord);
  float GetFlatSin(const int idet, const int ihar, const int imul,
                   const int izps, const int iord);

  // Utility function to get centrality and z-vertex bin
  int GetCentrality(PHCompositeNode *topNode);
  int GetCentralityBin(const int cent);
  int GetZVertexBin(const float bbcz);

  void Verbosity(const int val) { verbosity = val; }
  void SetRunNumber(const int val) { RunNumber = val; }

 private:

  void Fetch(PdbCalBank* rpBank);
  void Update(PdbCalBank* rpBank, PdbApplication* application);

  // Back ID
  //  1 =  Run4, Run5
  //  2 =  Run7 -
  //  3 =  Run8
  int GetBankID() const;

  // DB length
  int GetDBLength(const int bankid) const;

  // Correction for SumX/Y variables
  float SumXmean[RP::NMUL5][RP::NZPS5][RP::NHAR5][RP::NDET5];
  float SumXsigma[RP::NMUL5][RP::NZPS5][RP::NHAR5][RP::NDET5];
  float SumYmean[RP::NMUL5][RP::NZPS5][RP::NHAR5][RP::NDET5];
  float SumYsigma[RP::NMUL5][RP::NZPS5][RP::NHAR5][RP::NDET5];

  // Correction for flattening
  float FlatCos[RP::NMUL5][RP::NZPS5][RP::NHAR5][RP::NDET5][RP::NORD5];
  float FlatSin[RP::NMUL5][RP::NZPS5][RP::NHAR5][RP::NDET5][RP::NORD5];

  int verbosity;
  int RunNumber;

  std::string databasename;
};

#endif
