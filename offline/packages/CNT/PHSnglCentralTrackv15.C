#include "PHSnglCentralTrackv15.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv15)

using namespace std;

PHSnglCentralTrackv15::PHSnglCentralTrackv15()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv15::PHSnglCentralTrackv15(const PHSnglCentralTrack &track)
{

  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv15::identify(ostream &os) const
{
  os << "PHSnglTrack v15" << endl;
  os << "quality : " << get_quality() << endl;
  return;
}

void PHSnglCentralTrackv15::set_mom(const float val) 
{
  mom = val; 
  set_Px(get_px());
  set_Py(get_py());
  set_Pz(get_pz());
  return;
}
 
void PHSnglCentralTrackv15::set_phi0(const float val)
{
  phi0 = val;
  set_Px(get_px());
  set_Py(get_py());
  set_Pz(get_pz());
  return;
}

void PHSnglCentralTrackv15::set_the0(const float val)
{
  the0 = val;
  set_Px(get_px());
  set_Py(get_py());
  set_Pz(get_pz());
  return;
}

void PHSnglCentralTrackv15::set_momphi0the0(const float valmom, const float valphi0, const float valthe0)
{
  the0 = valthe0;
  phi0 = valphi0;
  mom = valmom;
  set_Px(get_px());
  set_Py(get_py());
  set_Pz(get_pz());
  return;
}

void
PHSnglCentralTrackv15::Copy(const PHSnglCentralTrack &src)
{
  ShutUp();
 
  // PHPartile methods
  set_px(src.get_px());
  set_py(src.get_py());
  set_pz(src.get_pz());
  set_E(src.get_E());
  set_charge(src.get_charge());
  set_PID(src.get_PID());
 
  // and here the glorious rest...
  set_quality(src.get_quality());
  set_zed(src.get_zed());
  set_phi(src.get_phi());
  set_alpha(src.get_alpha());
  set_beta(src.get_beta());
  set_momphi0the0(src.get_mom(), src.get_phi0(), src.get_the0());
//  set_phi0(src.get_phi0());
//  set_the0(src.get_the0());
//  set_mom(src.get_mom());
  set_nx1hits(src.get_nx1hits());
  set_nx2hits(src.get_nx2hits());
  set_ppc1x(src.get_ppc1x());
  set_ppc1y(src.get_ppc1y());
  set_ppc1z(src.get_ppc1z());
  set_ppc2x(src.get_ppc2x());
  set_ppc2y(src.get_ppc2y());
  set_ppc2z(src.get_ppc2z());
  set_ppc3x(src.get_ppc3x());
  set_ppc3y(src.get_ppc3y());
  set_ppc3z(src.get_ppc3z());
  set_pemcx(src.get_pemcx());
  set_pemcy(src.get_pemcy());
  set_pemcz(src.get_pemcz());
  set_plemc(src.get_plemc());
  set_pc3dphi(src.get_pc3dphi());
  set_pc3dz(src.get_pc3dz());
  set_emcdphi(src.get_emcdphi());
  set_emcdz(src.get_emcdz());
  set_spc3dphi(src.get_spc3dphi());
  set_spc3dz(src.get_spc3dz());
  set_semcdphi(src.get_semcdphi());
  set_semcdz(src.get_semcdz());
  set_arm(src.get_arm());
  set_sect(src.get_sect());
  set_ysect(src.get_ysect());
  set_zsect(src.get_zsect());
  set_ecorr(src.get_ecorr());
  set_ecore(src.get_ecore());
  set_emce(src.get_emce());
  set_emcdispy(src.get_emcdispy());
  set_emcdispz(src.get_emcdispz());
  set_temc(src.get_temc());
  set_prob(src.get_prob());
  set_ecent(src.get_ecent());
  set_twrhit(src.get_twrhit());
  set_e9(src.get_e9());
  set_re9(src.get_re9());
  set_emcchi2(src.get_emcchi2());
  set_secorr(src.get_secorr());
  set_secore(src.get_secore());
  set_semce(src.get_semce());
  set_stemc(src.get_stemc());
  set_se9(src.get_se9());
  set_n0(src.get_n0());
  set_npe0(src.get_npe0());
  set_n1(src.get_n1());
  set_npe1(src.get_npe1());
  set_chi2(src.get_chi2());
  set_disp(src.get_disp());
  set_tcrk(src.get_tcrk());
  set_cross_phi(src.get_cross_phi());
  set_cross_z(src.get_cross_z());
  set_center_phi(src.get_center_phi());
  set_center_z(src.get_center_z());
  set_sn0(src.get_sn0());
  set_snpe0(src.get_snpe0());
  set_sn1(src.get_sn1());
  set_snpe1(src.get_snpe1());
  set_schi2(src.get_schi2());
  set_sdisp(src.get_sdisp());
  set_stcrk(src.get_stcrk());
  set_scross_phi(src.get_scross_phi());
  set_scross_z(src.get_scross_z());
  set_scenter_phi(src.get_scenter_phi());
  set_scenter_z(src.get_scenter_z());
  set_pc3sdphi(src.get_pc3sdphi());
  set_pc3sdz(src.get_pc3sdz());
  set_emcsdphi(src.get_emcsdphi());
  set_emcsdz(src.get_emcsdz());
  set_spc3sdphi(src.get_spc3sdphi());
  set_spc3sdz(src.get_spc3sdz());
  set_semcsdphi(src.get_semcsdphi());
  set_semcsdz(src.get_semcsdz());
  set_m2tof(src.get_m2tof());
  set_m2emc(src.get_m2emc());
  set_dcarm(src.get_dcarm());
  set_dcside(src.get_dcside());
  set_emcsdphi_e(src.get_emcsdphi_e());
  set_emcsdz_e(src.get_emcsdz_e());
  set_semcsdphi_e(src.get_semcsdphi_e());
  set_semcsdz_e(src.get_semcsdz_e());
  set_deadmap(src.get_deadmap());
  set_warnmap(src.get_warnmap());
  set_sdeadmap(src.get_sdeadmap());
  set_swarnmap(src.get_swarnmap());
  set_Px(src.get_Px());
  set_Py(src.get_Py());
  set_Pz(src.get_Pz());
 
  ShutUp(0);
  return ;
}

