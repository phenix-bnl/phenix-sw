#ifndef MATCHRECALRECORUN15PP_H
#define MATCHRECALRECORUN15PP_H

#include <Recalibrator.h>

class PHCompositeNode;
class PHCentralTrack;

class MatchrecalRecoRun15pp : public Recalibrator
{
 public:
  MatchrecalRecoRun15pp(const std::string &name="MatchrecalRecoRun15pp");
  virtual ~MatchrecalRecoRun15pp() {}
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  void initParameters();
  void help() const;
  float GetPC3sdphi(const int iarm, const int icharge,
		    const float pt, const float pc3dphi);
  float GetPC3sdz(const int iarm, const int icharge,
		  const float pt, const float pc3dz);
  float GetPC2sdphi(const int iarm, const int icharge,
		    const float pt, const float pc2dphi);
  float GetPC2sdz(const int iarm, const int icharge,
		  const float pt, const float pc2dz);
  enum {NARM=2, NCH=2, NCENT=1};
 private:
  double PC3_dphi_mean[NARM][NCH][7];
  double PC3_dphi_sigma[NARM][NCH][8];
  double PC3_dz_mean[NARM][NCH][7];
  double PC3_dz_sigma[NARM][NCH][8];
  double PC2_dphi_mean[NARM][NCH][7];
  double PC2_dphi_sigma[NARM][NCH][8];
  double PC2_dz_mean[NARM][NCH][7];
  double PC2_dz_sigma[NARM][NCH][8];
};

#endif //MATCHRECALRECORUN15PP_H
