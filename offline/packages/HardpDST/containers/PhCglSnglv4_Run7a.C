#include "PhCglSnglv4_Run7a.h"

ClassImp(PhCglSnglv4_Run7a)

using namespace std;

PhCglSnglv4_Run7a::PhCglSnglv4_Run7a()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PhCglSnglv4_Run7a::PhCglSnglv4_Run7a(const PhCglSnglv4_Run7a &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);
  return;
}

void
PhCglSnglv4_Run7a::Copy(const PHSnglCentralTrack &src)
{

  ////////////////////////////////
  // Definitely put this set_dcarm method at the beginning!
  // Lots of methods, such as set_m2tof, relies on this for East/West identification!

  set_dcarm(src.get_dcarm());

  ////////////////////////////////


  set_charge(src.get_charge());
  set_quality(src.get_quality());
  set_zed(src.get_zed());
  set_phi(src.get_phi());
  set_alpha(src.get_alpha());
  set_mom(src.get_mom());
  set_the0(src.get_the0());
  set_phi0(src.get_phi0());
  set_ppc1x(src.get_ppc1x());
  set_ppc1y(src.get_ppc1y());
  set_ppc1z(src.get_ppc1z());
  set_ppc3x(src.get_ppc3x());
  set_ppc3y(src.get_ppc3y());
  set_ppc3z(src.get_ppc3z());
  set_plemc(src.get_plemc());
  set_pc2dphi(src.get_pc2dphi());
  set_pc2dz(src.get_pc2dz());
  set_pc3dphi(src.get_pc3dphi());
  set_pc3dz(src.get_pc3dz());
  set_emcdphi(src.get_emcdphi());
  set_emcdz(src.get_emcdz());
  set_sect(src.get_sect());
  set_ysect(src.get_ysect());
  set_zsect(src.get_zsect());
  set_emce(src.get_emce());
  set_temc(src.get_temc());
  set_emcrawtdc(src.get_emcrawtdc());
  set_emcrawadc(src.get_emcrawadc());
  set_emcrawadclg(src.get_emcrawadclg());
  set_prob(src.get_prob());
  set_ecent(src.get_ecent());
  set_ecore(src.get_ecore());
  set_emcchi2(src.get_emcchi2());
  set_twrhit(src.get_twrhit());
  set_n0(src.get_n0());
  set_npe0(src.get_npe0());
  set_n1(src.get_n1());
  set_npe1(src.get_npe1());
  set_chi2(src.get_chi2());
  set_disp(src.get_disp());
  
  set_spc2dphi(src.get_spc2dphi());
  set_spc2dz(src.get_spc2dz());
  set_spc3dphi(src.get_spc3dphi());
  set_spc3dz(src.get_spc3dz());
  set_semcdphi(src.get_semcdphi());
  set_semcdz(src.get_semcdz());

  set_m2emc(src.get_m2emc());
  set_isPi(src.get_isPi());
  set_isK(src.get_isK());
  set_isP(src.get_isP());
  set_deadmap(src.get_deadmap());
  set_warnmap(src.get_warnmap());
  set_aerindex(src.get_aerindex());
  set_aersindex(src.get_aersindex());
  set_emcid(src.get_emcid());
  set_beta(src.get_beta());
  set_ppc2x(src.get_ppc2x());
  set_ppc2y(src.get_ppc2y());
  set_ppc2z(src.get_ppc2z());
  set_pemcx(src.get_pemcx());
  set_pemcy(src.get_pemcy());
  set_pemcz(src.get_pemcz());


  ///////////////////////////////
  // Beginning from PHCentralTrack v22, it will return -9999 if it's not the right arm, 
  // e.g., you ask for tofw on an east-arm track. 
  // However, we are writting both TOF east/west into the same space.
  // So we have to be careful not to overwrite any variables. An arm-check has been done in all set functions.

  set_pltof(src.get_pltof());
  set_pltofw(src.get_pltofw());

  set_tofdphi(src.get_tofdphi());
  set_tofdz(src.get_tofdz());
  set_tofwdphi(src.get_tofwdphi());
  set_tofwdz(src.get_tofwdz());

  set_stofdphi(src.get_stofdphi());
  set_stofdz(src.get_stofdz());
  set_stofwdphi(src.get_stofwdphi());
  set_stofwdz(src.get_stofwdz());

  set_slat(src.get_slat());
  set_ttof(src.get_ttof());
  set_etof(src.get_etof());
  set_striptofw(src.get_striptofw());
  set_ttofw(src.get_ttofw());
  set_qtofw(0.5*(src.get_tofwadcup()+src.get_tofwadcdw()));

  set_m2tof(src.get_m2tof());
  set_m2tofw(src.get_m2tofw());

  set_ptofx(src.get_ptofx());
  set_ptofwx(src.get_ptofwx());
  set_ptofy(src.get_ptofy());
  set_ptofwy(src.get_ptofwy());
  set_ptofz(src.get_ptofz());
  set_ptofwz(src.get_ptofwz());
  set_tofph1(src.get_tofph1());
  set_tofwadcup(src.get_tofwadcup());
  set_tofph2(src.get_tofph2());
  set_tofwadcdw(src.get_tofwadcdw());
  set_toftdc1(src.get_toftdc1());
  set_tofwtdcup(src.get_tofwtdcup());
  set_toftdc2(src.get_toftdc2());
  set_tofwtdcdw(src.get_tofwtdcdw());
  
  return ;
}
