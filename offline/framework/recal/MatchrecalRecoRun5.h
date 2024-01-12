#ifndef __MATCHRECALRECORUN5_H__
#define __MATCHRECALRECORUN5_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;

class MatchrecalRecoRun5 : public Recalibrator
{
 public:
  MatchrecalRecoRun5(const char*name="MatchrecalRecoRun5");
  virtual ~MatchrecalRecoRun5() {}

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  int runNumber;
  short b_field;

  PHCentralTrack *d_cnt;
  PHGlobal *d_global;

  int Common_Run5();
  int AfterBurner_Run5CuCu();
  int AfterBurner_Run5pp();

  // Common functions
  float Getpc2sdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Getpc3sdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Gettofsdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Getemcsdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);

  float Getpc2sdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Getpc3sdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Gettofsdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Getemcsdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);

  // After Burner for Run5CuCu
  float Getpc2sdphi_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Getpc3sdphi_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Gettofsdphi_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Getemcsdphi_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);

  float Getpc2sdz_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Getpc3sdz_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Gettofsdz_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Getemcsdz_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);

  // After Burner for Run5pp
  float Getpc2sdphi_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Getpc3sdphi_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Gettofsdphi_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);
  float Getemcsdphi_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi);

  float Getpc2sdz_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Getpc3sdz_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Gettofsdz_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);
  float Getemcsdz_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz);

 protected:

};

#endif /* __MATCHRECALRECORUN5_H__ */
