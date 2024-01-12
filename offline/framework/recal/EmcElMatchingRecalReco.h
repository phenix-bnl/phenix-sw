#ifndef EMCELMATCHINGRECALRECO_H__
#define EMCELMATCHINGRECALRECO_H__

#include "Recalibrator.h"
#include "EmcPar.h"

class PHCompositeNode;

class EmcElMatchingRecalReco : public Recalibrator
{
 public:
  EmcElMatchingRecalReco(const std::string &name="EmcElMatchingRecalReco");
  virtual ~EmcElMatchingRecalReco(){}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  int isValidRun(const int runno) const;
  int FetchFromFile(const std::string &filename = "EmcElMatchingRecalReco-const.dat", const int depver = 1);
  int FetchFromFile_run13pp510(const std::string &filename = "EmcElMatchingRecalReco-const.dat");
  void setRunNumber(const int a) {run = a;}
  float getMatchPar(const int parid, const int sect, const int par, const int mompar) const;
  std::string bankDescription(const int bankid) const;
  int Fetch();
  int UpdateDB(const int run_beg, const int run_end, const int depver = 1);
  int FetchMatch();
  int FetchMatch(const int runno);
  int UpdateMatchDB(const int run_beg, const int run_end);
  void test(const short emcsector, const float pt, const float zed, const short charge) const;

 
 protected:
  float emcsdphi_cor(const float emcdphi, const int charge, const float mom, const float zed, const short emcsector) const;
  float emcsdz_cor(const float emcdz, const int charge, const float mom, const float theta, const short emcsector) const;
  float emcsdphi_cor_run13pp510(const float emcdphi, const int charge, const float mom, const float zed, const short emcsector) const;
  float emcsdz_cor_run13pp510(const float emcdz, const int charge, const float mom, const float theta, const short emcsector) const;
  int UpdateParDB(const int bankid, const int run_beg, const int run_end);
  int FetchParDB(const int bankid);
  int depversion;
  int run10auau200version;
  int run;
  bool run13pp510;
  static const int NSECT = EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR;
  float phimatch_pos_mean[NSECT][3][3];
  float phimatch_pos_sigma[NSECT][3][3];
  float phimatch_neg_mean[NSECT][3][3];
  float phimatch_neg_sigma[NSECT][3][3];
  float zmatch_pos_mean[NSECT][2][4];
  float zmatch_pos_sigma[NSECT][2][4];
  float zmatch_neg_mean[NSECT][2][4];
  float zmatch_neg_sigma[NSECT][2][4];

};

#endif /*  EMCELMATCHINGRECALRECO_H__ */
