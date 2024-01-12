#ifndef MATCHRECALRECO39GEVRUN16DAU_H
#define MATCHRECALRECO39GEVRUN16DAU_H

#include <Recalibrator.h>

class PHCompositeNode;
class PHCentralTrack;

class MatchrecalReco39GeVRun16dAu : public Recalibrator
{
 public:
  MatchrecalReco39GeVRun16dAu(const std::string &name="MatchrecalReco39GeVRun16dAu");
  virtual ~MatchrecalReco39GeVRun16dAu() {}
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  void initParameters();
  void help() const;
  float GetPC3sdphi(const int iarm, const int icharge, const int ivz,
		    const float pt, const float pc3dphi);
  float GetPC3sdz(const int iarm, const int icharge, const int ivz,
		  const float pt, const float pc3dz);
  float GetPC2sdphi(const int iarm, const int icharge, const int ivz,
        const float pt, const float pc2dphi);
  float GetPC2sdz(const int iarm, const int icharge, const int ivz,
      const float pt, const float pc2dz);
  enum {NARM=2, NCH=2, NZED=4, NCENT=1};
 private:
  double PC3_dphifirst_mean[NARM][NCH][NZED];
  double PC3_dphifirst_sigma[NARM][NCH][NZED];
  double PC3_dzfirst_mean[NARM][NCH][NZED];
  double PC3_dzfirst_sigma[NARM][NCH][NZED];
  double PC3_dphi_mean[NARM][NCH][NZED][7];
  double PC3_dphi_sigma[NARM][NCH][NZED][8];
  double PC3_dz_mean[NARM][NCH][NZED][7];
  double PC3_dz_sigma[NARM][NCH][NZED][8];
  double PC2_dphifirst_mean[NARM][NCH][NZED];
  double PC2_dphifirst_sigma[NARM][NCH][NZED];
  double PC2_dzfirst_mean[NARM][NCH][NZED];
  double PC2_dzfirst_sigma[NARM][NCH][NZED];
  double PC2_dphi_mean[NARM][NCH][NZED][7];
  double PC2_dphi_sigma[NARM][NCH][NZED][8];
  double PC2_dz_mean[NARM][NCH][NZED][7];
  double PC2_dz_sigma[NARM][NCH][NZED][8];
};

#endif // MINIRECAL_H
