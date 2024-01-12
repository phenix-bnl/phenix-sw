#ifndef __MATCHRECALRECORUN14HEAU_H__
#define __MATCHRECALRECORUN14HEAU_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;

class MatchrecalRecoRun14HeAu : public Recalibrator
{
 public:
  MatchrecalRecoRun14HeAu(const char*name="MatchrecalRecoRun14HeAu");
  virtual ~MatchrecalRecoRun14HeAu() {}

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  int runNumber;
  //short b_field;

  PHCentralTrack *d_cnt;
  //PHGlobal *d_global;

  float pc3dphimean[2][2][4];
  float pc3dphisigma[2][2][4];
  float pc3dzmean[2][2][4];
  float pc3dzsigma[2][2][4];

  float tofdphimean[2][2][4];
  float tofdphisigma[2][2][4];
  float tofdzmean[2][2][4];
  float tofdzsigma[2][2][4];
  
  double pc2_dphifirst_mean[2][2][1];
  double pc2_dphifirst_sigma[2][2][1];
  double pc2_dzfirst_mean[2][2][1];
  double pc2_dzfirst_sigma[2][2][1];
  double pc2_dphi_mean[2][2][7];
  double pc2_dphi_sigma[2][2][8];
  double pc2_dz_mean[2][2][7];
  double pc2_dz_sigma[2][2][8];
  
  // Common functions
  float Getpc2sdphi(const int i_ch, const int i_zed, const float pt, const float dphi);
  float Getpc3sdphi(const short i_arm, const short i_ch, const float pt, const float dphi);
  float Gettofsdphi(const short i_arm, const short i_ch, const float pt, const float dphi);
  //float Getemcsdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);

  //float Getpc2sdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Getpc2sdz(const int i_ch, const int i_zed, const float pt, const float dz);
  float Getpc3sdz(const short i_arm, const short i_ch, const float pt, const float dz);
  float Gettofsdz(const short i_arm, const short i_ch, const float pt, const float dz);
  //float Getemcsdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);

  
 protected:

};

#endif /* __MATCHRECALRECORUN14HEAU_H__ */
