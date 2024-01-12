#ifndef RUN14AUAU200PC2MATCHRECAL_H
#define RUN14AUAU200PC2MATCHRECAL_H

#include <Recalibrator.h>

class PHCompositeNode;
class PHCentralTrack;

class Run14AuAu200PC2MatchRecal : public Recalibrator
{
 public:
  Run14AuAu200PC2MatchRecal(const std::string &name="Run14AuAu200PC2MatchRecal");
  virtual ~Run14AuAu200PC2MatchRecal() {}
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  void initParameters();
  void help() const;
  float GetPC2sdphi(const int iarm, const int icharge,
		    const float pt, const float pc2dphi);
  float GetPC2sdz(const int iarm, const int icharge,
		  const float pt, const float pc2dz);
  enum {NARM=2, NCH=2, NZED=1, NCENT=1};
 private:
  double PC2_dphifirst_mean[NARM][NCH][NZED];
  double PC2_dphifirst_sigma[NARM][NCH][NZED];
  double PC2_dzfirst_mean[NARM][NCH][NZED];
  double PC2_dzfirst_sigma[NARM][NCH][NZED];
  double PC2_dphi_mean[NARM][NCH][7];
  double PC2_dphi_sigma[NARM][NCH][8];
  double PC2_dz_mean[NARM][NCH][7];
  double PC2_dz_sigma[NARM][NCH][8];
};

#endif //RUN14AUAU200PC2MATCHRECAL_H
