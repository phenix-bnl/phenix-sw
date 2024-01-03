#include <PHSnglCentralTrack.h>

#include <cmath>
#include <iostream>

ClassImp(PHSnglCentralTrack)

using namespace std;

static int shutup = 0;

void PHSnglCentralTrack::Init()
{
  ShutUp();

  // PHPartile methods
  set_px(-9999);
  set_py(-9999);
  set_pz(-9999);
  set_E(-9999);
  set_charge(-9999);
  set_PID(-9999);

  // set the dc arm to 0, otherwise the set tof which distinguish tofw and tof via the arms
  // leaves them uninitialized
  set_dcarm(0);

  // and here the glorious rest...
  set_momphi0the0(-9999,-9999,-9999);
  set_quality(-9999);
  set_zed(-9999);
  set_phi(-9999);
  set_alpha(-9999);
  set_beta(-9999);
  set_mom(-9999);
  set_the0(-9999);
  set_phi0(-9999);
  set_mompx(-9999);
  set_mompy(-9999);
  set_mompz(-9999);
  set_status(-9999);
  set_alpha1(-9999);
  set_alpha2(-9999);
  set_nx1hits(-9999);
  set_nx2hits(-9999);
  set_mx1dist(-9999);
  set_mx2dist(-9999);
  set_chi2x1(-9999);
  set_chi2x2(-9999);
  set_nx1x2fit(-9999);
  set_mchi2(-9999);
  set_error(-9999);
  set_alphaf(-9999);
  set_pc1id(-9999);
  set_pc2id(-9999);
  set_pc3id(-9999);
  set_emcid(-9999);
  set_tofid(-9999);
  set_tofeid(-9999);
  set_tofwid(-9999);
  set_tecid(-9999);
  set_hbdid(-9999);
  set_mrpcid(-9999);
  set_spc2id(-9999);
  set_spc3id(-9999);
  set_semcid(-9999);
  set_stofid(-9999);
  set_stofeid(-9999);
  set_ppc1x(-9999.);
  set_ppc1y(-9999.);
  set_ppc1z(-9999.);
  set_ppc2x(-9999.);
  set_ppc2y(-9999.);
  set_ppc2z(-9999.);
  set_ptecx(-9999);
  set_ptecy(-9999);
  set_ptecz(-9999);
  set_ppc3x(-9999.);
  set_ppc3y(-9999.);
  set_ppc3z(-9999.);
  set_pemcx(-9999);
  set_pemcy(-9999);
  set_pemcz(-9999);
  set_ptofx(-9999);
  set_ptofy(-9999);
  set_ptofz(-9999);
  set_ptofwx(-9999);
  set_ptofwy(-9999);
  set_ptofwz(-9999);
  set_pmrpcx(-9999);
  set_pmrpcy(-9999);
  set_pmrpcz(-9999);
  set_phbdx(-9999);
  set_phbdy(-9999);
  set_phbdz(-9999);
  set_pltof(-9999);
  set_pltofw(-9999);
  set_plemc(-9999);
  set_plmrpc(-9999);
  set_pc2dphi(-9999);
  set_pc2dz(-9999);
  set_pc3dphi(-9999);
  set_pc3dz(-9999);
  set_emcdphi(-9999);
  set_emcdz(-9999);
  set_tofdphi(-9999);
  set_tofdz(-9999);
  set_tofwdphi(-9999);
  set_tofwdz(-9999);
  set_tecdphi(-9999);
  set_tecdalpha(-9999);
  set_mrpcdphi(-9999);
  set_mrpcdz(-9999);
  set_spc2dphi(-9999);
  set_spc2dz(-9999);
  set_spc3dphi(-9999);
  set_spc3dz(-9999);
  set_semcdphi(-9999);
  set_semcdz(-9999);
  set_stofdphi(-9999);
  set_stofdz(-9999);
  set_stofwdphi(-9999);
  set_stofwdz(-9999);
  set_stecdphi(-9999);
  set_stecdalpha(-9999);
  set_arm(-9999);
  set_sect(-9999);
  set_ysect(-9999);
  set_zsect(-9999);
  set_ecorr(-9999);
  set_ecore(-9999);
  set_dep(-9999);
  set_emce(-9999);
  set_emcdispy(-9999);
  set_emcdispz(-9999);
  set_temc(-9999);
  set_prob(-9999);
  set_ecent(-9999);
  set_twrhit(-9999);
  set_e9(-9999);
  set_re9(-9999);
  set_emcchi2(-9999);
  set_sysect(-9999);
  set_szsect(-9999);
  set_secorr(-9999);
  set_secore(-9999);
  set_semce(-9999);
  set_semcdispy(-9999);
  set_semcdispz(-9999);
  set_stemc(-9999);
  set_sprob(-9999);
  set_secent(-9999);
  set_stwrhit(-9999);
  set_se9(-9999);
  set_sre9(-9999);
  set_semcchi2(-9999);
  set_slat(-9999);
  set_ttof(-9999);
  set_etof(-9999);
  set_sttof(-9999);
  set_setof(-9999);
  set_striptofw(-9999);
  
  set_tofwx(-9999);
  set_tofwy(-9999);
  set_tofwz(-9999);
  
  set_qtofw(-9999);
  set_ttofw(-9999);
  set_tofwadcup(-9999);
  set_tofwadcdw(-9999);
  set_tofwtdcup(-9999);
  set_tofwtdcdw(-9999);
  set_slat_mrpc(-9999);
  set_ttof_mrpc(-9999);
  set_ttofd_mrpc(-9999);
  set_qtof_mrpc(-9999);
  set_acc(-9999);
  set_ring(-9999);
  set_n0(-9999);
  set_npe0(-9999);
  set_n1(-9999);
  set_npe1(-9999);
  set_chi2(-9999);
  set_disp(-9999);
  set_tcrk(-9999);
  set_cross_phi(-9999);
  set_cross_z(-9999);
  set_center_phi(-9999);
  set_center_z(-9999);
  set_sacc(-9999);
  set_sring(-9999);
  set_sn0(-9999);
  set_snpe0(-9999);
  set_sn1(-9999);
  set_snpe1(-9999);
  set_schi2(-9999);
  set_sdisp(-9999);
  set_stcrk(-9999);
  set_scross_phi(-9999);
  set_scross_z(-9999);
  set_scenter_phi(-9999);
  set_scenter_z(-9999);
  set_tecdedx1(-9999);
  set_tecdedx2(-9999);
  set_pc2sdphi(-9999);
  set_pc2sdz(-9999);
  set_pc3sdphi(-9999);
  set_pc3sdz(-9999);
  set_emcsdphi(-9999);
  set_emcsdz(-9999);
  set_tofsdphi(-9999);
  set_tofsdz(-9999);
  set_tofwsdphi(-9999);
  set_tofwsdz(-9999);
  set_tecsdphi(-9999);
  set_tecsdalpha(-9999);
  set_spc2sdphi(-9999);
  set_spc2sdz(-9999);
  set_spc3sdphi(-9999);
  set_spc3sdz(-9999);
  set_semcsdphi(-9999);
  set_semcsdz(-9999);
  set_stofsdphi(-9999);
  set_stofsdz(-9999);
  set_stofwsdphi(-9999);
  set_stofwsdz(-9999);
  set_stecsdphi(-9999);
  set_stecsdalpha(-9999);
  set_hbdsector(-9999);
  set_hbdsize(-9999);
  set_hbdcharge(-9999);
  set_hbdx(-9999);
  set_hbdy(-9999);
  set_hbdz(-9999);
  set_hbddphi(-9999);
  set_hbddz(-9999);
  set_hbdsdphi(-9999);
  set_hbdsdz(-9999);
  set_m2tof(-9999);
  set_m2tofw(-9999);
  set_m2emc(-9999);
  set_isPi(-9999);
  set_isK(-9999);
  set_isP(-9999);
  set_isPiTofw(-9999);
  set_isKTofw(-9999);
  set_isPTofw(-9999);
  set_dcside(-9999);
  set_pc1sect(-9999);
  set_pc2sect(-9999);
  set_pc3sect(-9999);
  set_pc1phi(-9999);
  set_pc1z(-9999);
  set_pc2phi(-9999);
  set_pc2z(-9999);
  set_pc3phi(-9999);
  set_pc3z(-9999);
  set_tofphi(-9999);
  set_tofz(-9999);
  set_tecphi(-9999);
  set_tecalpha(-9999);
  set_emcphi(-9999);
  set_emcz(-9999);
  set_spc1phi(-9999);
  set_spc1z(-9999);
  set_spc2phi(-9999);
  set_spc2z(-9999);
  set_spc3phi(-9999);
  set_spc3z(-9999);
  set_stofphi(-9999);
  set_stofz(-9999);
  set_stecphi(-9999);
  set_stecalpha(-9999);
  set_semcphi(-9999);
  set_semcz(-9999);
  set_emcsdphi_e(-9999);
  set_emcsdz_e(-9999);
  set_semcsdphi_e(-9999);
  set_semcsdz_e(-9999);
  set_tecnhit(-9999);
  set_n2(-9999);
  set_npe2(-9999);
  set_n3(-9999);
  set_npe3(-9999);
  set_sn2(-9999);
  set_snpe2(-9999);
  set_sn3(-9999);
  set_snpe3(-9999);
  set_deadmap(-9999);
  set_warnmap(-9999);
  set_sdeadmap(-9999);
  set_swarnmap(-9999);
  set_tofecut(-9999);
  set_tofsame(-9999);
  set_slatnext(-9999);
  set_ttofnext(-9999);
  set_etofnext(-9999);
  set_tzrid(-9999);
  set_pcrid(-9999);
  set_ptzrx(-9999);
  set_ptzry(-9999);
  set_ptzrz(-9999);
  set_ppcrx(-9999);
  set_ppcry(-9999);
  set_ppcrz(-9999);
  set_tzrtof(-9999);
  set_tzrslat(-9999);
  set_tzreloss(-9999);
  set_tzrx(-9999);
  set_tzry(-9999);
  set_tzrz(-9999);
  set_pcrtof(-9999);
  set_pcrslat(-9999);
  set_pcreloss(-9999);
  set_pcrx(-9999);
  set_pcry(-9999);
  set_pcrz(-9999);
  set_tzrsdphi(-9999);
  set_tzrsdz(-9999);
  set_m2tzr(-9999);
  set_m2ntctof(-9999);
  set_pltzr(-9999);
  set_isPitzr(-9999);
  set_isKtzr(-9999);
  set_isPtzr(-9999);
  set_isPintctof(-9999);
  set_isKntctof(-9999);
  set_isPntctof(-9999);
  set_tzrecut(-9999);
  set_L1Trig(-9999);
  set_pc1wid(-9999);
  set_pc2wid(-9999);
  set_pc3wid(-9999);
  set_tofph1(-9999);
  set_tofph2(-9999);
  set_toftdc1(-9999);
  set_toftdc2(-9999);
  set_aerindex(-9999);
  set_aersindex(-9999);
  set_aerph1(-9999);
  set_aerph2(-9999);
  set_aert1(-9999);
  set_aert2(-9999);
  set_aernpe(-9999);
  set_aerstatus(-9999);
  set_aerph1_0(-9999);
  set_aerph2_0(-9999);
  set_aert1_0(-9999);
  set_aert2_0(-9999);
  set_aerph1_1(-9999);
  set_aerph2_1(-9999);
  set_aert1_1(-9999);
  set_aert2_1(-9999);
  set_aerph1_2(-9999);
  set_aerph2_2(-9999);
  set_aert1_2(-9999);
  set_aert2_2(-9999);
  set_aerph1_3(-9999);
  set_aerph2_3(-9999);
  set_aert1_3(-9999);
  set_aert2_3(-9999);
  set_aerhitid(-9999);
  set_aerhitconfig(-9999);
  set_aersph1_0(-9999);
  set_aersph2_0(-9999);
  set_aerst1_0(-9999);
  set_aerst2_0(-9999);
  set_aersph1_1(-9999);
  set_aersph2_1(-9999);
  set_aerst1_1(-9999);
  set_aerst2_1(-9999);
  set_aersph1_2(-9999);
  set_aersph2_2(-9999);
  set_aerst1_2(-9999);
  set_aerst2_2(-9999);
  set_aersph1_3(-9999);
  set_aersph2_3(-9999);
  set_aerst1_3(-9999);
  set_aerst2_3(-9999);
  set_aershitid(-9999);
  set_aershitconfig(-9999);
  set_tecde(-9999);
  set_tecde06(-9999);
  set_tectrklen(-9999);
  set_tecnde(-9999);
  set_tecnhit100(-9999);
  set_tecnhit200(-9999);
  set_tecnhit50(-9999);
  set_tecwtb(-9999);
  set_tecwtbsq(-9999);
  set_mcid(-9999);
  set_dchid(-9999);
  set_emcrawtdc(-9999);
  set_emcrawadc(-9999);
  set_emcrawadclg(-9999);
  set_categoryl2eLowPt(-9999);
  set_categoryl2eHighPt(-9999);
  set_candIDl2e(-9999);
  set_nlvl2MatchLowOcupy(-9999);
  set_RawL1(-9999);
  set_LivL1(-9999);
  set_SclL1(-9999);
  set_Px(-9999);
  set_Py(-9999);
  set_Pz(-9999);
  set_idtrk_tof(-9999);
  set_idtrk_tofw(-9999);
  set_idtrk_tec(-9999);
  set_idtrk_crk(-9999);
  set_idtrk_pc2(-9999);
  set_idtrk_pc3(-9999);
  set_idtrk_stof(-9999);
  set_idtrk_stec(-9999);
  set_idtrk_scrk(-9999);
  set_idtrk_spc2(-9999);
  set_idtrk_spc3(-9999);
  // finally set the dcarm to -9999
  set_dcarm(-9999);
  set_hbdhubcharge(-9999.);
  set_hbdspokecharge1(-9999.);
  set_hbdspokecharge2(-9999.);
  set_hbdlocalmax(-9999.);
  set_hbdscharge(-9999.);


  set_teccharge(0,-9999);
  set_teccharge(1,-9999);
  set_teccharge(2,-9999);
  set_teccharge(3,-9999);
  set_teccharge(4,-9999);
  set_teccharge(5,-9999);

  set_tecntimebins(0,-9999);
  set_tecntimebins(1,-9999);
  set_tecntimebins(2,-9999);
  set_tecntimebins(3,-9999);
  set_tecntimebins(4,-9999);
  set_tecntimebins(5,-9999);

  set_tecavgtimebin(0,-9999);
  set_tecavgtimebin(1,-9999);
  set_tecavgtimebin(2,-9999);
  set_tecavgtimebin(3,-9999);
  set_tecavgtimebin(4,-9999);
  set_tecavgtimebin(5,-9999);

  set_tecdphiplane(0,-9999);
  set_tecdphiplane(1,-9999);
  set_tecdphiplane(2,-9999);
  set_tecdphiplane(3,-9999);
  set_tecdphiplane(4,-9999);
  set_tecdphiplane(5,-9999);

  set_tecMomentum(-9999);
  set_tectrlike(-9999);

  set_steccharge(0,-9999);
  set_steccharge(1,-9999);
  set_steccharge(2,-9999);
  set_steccharge(3,-9999);
  set_steccharge(4,-9999);
  set_steccharge(5,-9999);

  set_stecntimebins(0,-9999);
  set_stecntimebins(1,-9999);
  set_stecntimebins(2,-9999);
  set_stecntimebins(3,-9999);
  set_stecntimebins(4,-9999);
  set_stecntimebins(5,-9999);

  set_stecavgtimebin(0,-9999);
  set_stecavgtimebin(1,-9999);
  set_stecavgtimebin(2,-9999);
  set_stecavgtimebin(3,-9999);
  set_stecavgtimebin(4,-9999);
  set_stecavgtimebin(5,-9999);

  set_stecdphiplane(0,-9999);
  set_stecdphiplane(1,-9999);
  set_stecdphiplane(2,-9999);
  set_stecdphiplane(3,-9999);
  set_stecdphiplane(4,-9999);
  set_stecdphiplane(5,-9999);

  set_stecMomentum(-9999);
  set_stectrlike(-9999);

  for(int itmp=0; itmp<4; itmp++) {set_svxdphi      (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_svxdz        (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_svxid        (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_svxsdphi     (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_svxsdz       (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_svxsid       (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_psvxx        (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_psvxy        (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_psvxz        (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_spsvxx       (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_spsvxy       (itmp,-9999);}
  for(int itmp=0; itmp<4; itmp++) {set_spsvxz       (itmp,-9999);}
  set_svxtrackid   (-9999);
  set_svxtrackquality     (-9999);
  set_svxdca2d     (-9999);
  set_svxdca3d     (-9999);
  set_svxdca2dkf   (-9999);
  set_svxdca3dkf   (-9999);
  for(int itmp=0; itmp<3; itmp++) {set_svxxyz0      (itmp,-9999);}
  for(int itmp=0; itmp<3; itmp++) {set_svxpxyz0     (itmp,-9999);}
  for(int itmp=0; itmp<3; itmp++) {set_svxxyzkf     (itmp,-9999);}
  for(int itmp=0; itmp<3; itmp++) {set_svxpxyzkf    (itmp,-9999);}


  ShutUp(0);
  return ;
}

void
PHSnglCentralTrack::identify(ostream &os) const
{
  os << "identify: virtual PHSnglCentralTrack object" << endl;
  return ;
}

void
PHSnglCentralTrack::ShutUp(const int i)
{
  shutup = i;
  return ;
}

void
PHSnglCentralTrack::warning(const char* field) const
{
   if (!shutup)
     {
       cout << PHWHERE << "using virtual function, doing nothing" << endl;
       cout << "Single Track Offending field == " << field << endl;
     }
  return ;
}

float 
PHSnglCentralTrack::get_px() const
{
  float p      =     get_mom ();
  float sinTH  = sin(get_the0());
  float cosPHI = cos(get_phi0());
  return p*sinTH*cosPHI; 
}

float 
PHSnglCentralTrack::get_py() const
{
  float p      =     get_mom ();
  float sinTH  = sin(get_the0());
  float sinPHI = sin(get_phi0());
  return p*sinTH*sinPHI; 
}

float 
PHSnglCentralTrack::get_pz() const
{
  float p      =     get_mom ();
  float cosTH  = cos(get_the0());
  return p*cosTH; 
}

float 
PHSnglCentralTrack::get_E() const
{
  return NAN;
}

void
PHSnglCentralTrack::Copy(const PHSnglCentralTrack &src)
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

  ////////////////////////////////
  // Definitely put this set_dcarm method at the beginning!
  // Lots of methods, such as set_m2tof, relies on this for East/West identification!

  set_dcarm(src.get_dcarm());

  ////////////////////////////////


  set_quality(src.get_quality());
  set_zed(src.get_zed());
  set_phi(src.get_phi());
  set_alpha(src.get_alpha());
  set_beta(src.get_beta());
  set_mom(src.get_mom());
  set_the0(src.get_the0());
  set_phi0(src.get_phi0());
  set_mompx(src.get_mompx());
  set_mompy(src.get_mompy());
  set_mompz(src.get_mompz());
  set_status(src.get_status());
  set_alpha1(src.get_alpha1());
  set_alpha2(src.get_alpha2());
  set_nx1hits(src.get_nx1hits());
  set_nx2hits(src.get_nx2hits());
  set_mx1dist(src.get_mx1dist());
  set_mx2dist(src.get_mx2dist());
  set_chi2x1(src.get_chi2x1());
  set_chi2x2(src.get_chi2x2());
  set_nx1x2fit(src.get_nx1x2fit());
  set_mchi2(src.get_mchi2());
  set_error(src.get_error());
  set_alphaf(src.get_alphaf());
  set_pc1id(src.get_pc1id());
  set_pc2id(src.get_pc2id());
  set_pc3id(src.get_pc3id());
  set_emcid(src.get_emcid());
  set_tofid(src.get_tofid());
  set_tofeid(src.get_tofeid());
  set_tofwid(src.get_tofwid());
  set_tecid(src.get_tecid());
  set_hbdid(src.get_hbdid());
  set_mrpcid(src.get_mrpcid());
  set_spc2id(src.get_spc2id());
  set_spc3id(src.get_spc3id());
  set_semcid(src.get_semcid());
  set_stofid(src.get_stofid());
  set_stofeid(src.get_stofeid());
  set_stecid(src.get_stecid());
  set_ppc1x(src.get_ppc1x());
  set_ppc1y(src.get_ppc1y());
  set_ppc1z(src.get_ppc1z());
  set_ppc2x(src.get_ppc2x());
  set_ppc2y(src.get_ppc2y());
  set_ppc2z(src.get_ppc2z());
  set_ptecx(src.get_ptecx());
  set_ptecy(src.get_ptecy());
  set_ptecz(src.get_ptecz());
  set_ppc3x(src.get_ppc3x());
  set_ppc3y(src.get_ppc3y());
  set_ppc3z(src.get_ppc3z());
  set_pemcx(src.get_pemcx());
  set_pemcy(src.get_pemcy());
  set_pemcz(src.get_pemcz());
  set_ptofx(src.get_ptofx());
  set_ptofy(src.get_ptofy());
  set_ptofz(src.get_ptofz());
  set_ptofwx(src.get_ptofwx());
  set_ptofwy(src.get_ptofwy());
  set_ptofwz(src.get_ptofwz());
  set_pmrpcx(src.get_pmrpcx());
  set_pmrpcy(src.get_pmrpcy());
  set_pmrpcz(src.get_pmrpcz());
  set_phbdx(src.get_phbdx());
  set_phbdy(src.get_phbdy());
  set_phbdz(src.get_phbdz());
  set_pltof(src.get_pltof());
  set_pltofw(src.get_pltofw());
  set_plemc(src.get_plemc());
  set_plmrpc(src.get_plmrpc());
  set_pc2dphi(src.get_pc2dphi());
  set_pc2dz(src.get_pc2dz());
  set_pc3dphi(src.get_pc3dphi());
  set_pc3dz(src.get_pc3dz());
  set_emcdphi(src.get_emcdphi());
  set_emcdz(src.get_emcdz());
  set_tofdphi(src.get_tofdphi());
  set_tofdz(src.get_tofdz());
  set_tofwdphi(src.get_tofwdphi());
  set_tofwdz(src.get_tofwdz());
  set_tecdphi(src.get_tecdphi());
  set_tecdalpha(src.get_tecdalpha());
  set_mrpcdphi(src.get_mrpcdphi());
  set_mrpcdz(src.get_mrpcdz());
  set_spc2dphi(src.get_spc2dphi());
  set_spc2dz(src.get_spc2dz());
  set_spc3dphi(src.get_spc3dphi());
  set_spc3dz(src.get_spc3dz());
  set_semcdphi(src.get_semcdphi());
  set_semcdz(src.get_semcdz());
  set_stofdphi(src.get_stofdphi());
  set_stofdz(src.get_stofdz());
  set_stofwdphi(src.get_stofwdphi());
  set_stofwdz(src.get_stofwdz());
  set_stecdphi(src.get_stecdphi());
  set_stecdalpha(src.get_stecdalpha());
  set_arm(src.get_arm());
  set_sect(src.get_sect());
  set_ysect(src.get_ysect());
  set_zsect(src.get_zsect());
  set_ecorr(src.get_ecorr());
  set_ecore(src.get_ecore());
  set_dep(src.get_dep());
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
  set_sysect(src.get_sysect());
  set_szsect(src.get_szsect());
  set_secorr(src.get_secorr());
  set_secore(src.get_secore());
  set_semce(src.get_semce());
  set_semcdispy(src.get_semcdispy());
  set_semcdispz(src.get_semcdispz());
  set_stemc(src.get_stemc());
  set_sprob(src.get_sprob());
  set_secent(src.get_secent());
  set_stwrhit(src.get_stwrhit());
  set_se9(src.get_se9());
  set_sre9(src.get_sre9());
  set_semcchi2(src.get_semcchi2());
  set_slat(src.get_slat());
  set_ttof(src.get_ttof());
  set_etof(src.get_etof());
  set_sttof(src.get_sttof());
  set_setof(src.get_setof());
  set_striptofw(src.get_striptofw());
  
  set_tofwx(src.get_tofwx());
  set_tofwy(src.get_tofwy());
  set_tofwz(src.get_tofwz());
  
  set_ttofw(src.get_ttofw());
  set_qtofw(src.get_qtofw());
  set_tofwadcup(src.get_tofwadcup());
  set_tofwadcdw(src.get_tofwadcdw());
  set_tofwtdcup(src.get_tofwtdcup());
  set_tofwtdcdw(src.get_tofwtdcdw());
  set_slat_mrpc(src.get_slat_mrpc());
  set_ttof_mrpc(src.get_ttof_mrpc());
  set_ttofd_mrpc(src.get_ttofd_mrpc());
  set_qtof_mrpc(src.get_qtof_mrpc());
  set_acc(src.get_acc());
  set_ring(src.get_ring());
  set_sring(src.get_sring());
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
  set_sacc(src.get_sacc());
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
  set_tecdedx1(src.get_tecdedx1());
  set_tecdedx2(src.get_tecdedx2());
  set_pc2sdphi(src.get_pc2sdphi());
  set_pc2sdz(src.get_pc2sdz());
  set_pc3sdphi(src.get_pc3sdphi());
  set_pc3sdz(src.get_pc3sdz());
  set_emcsdphi(src.get_emcsdphi());
  set_emcsdz(src.get_emcsdz());
  set_tofsdphi(src.get_tofsdphi());
  set_tofsdz(src.get_tofsdz());
  set_tofwsdphi(src.get_tofwsdphi());
  set_tofwsdz(src.get_tofwsdz());
  set_tecsdphi(src.get_tecsdphi());
  set_tecsdalpha(src.get_tecsdalpha());
  set_spc2sdphi(src.get_spc2sdphi());
  set_spc2sdz(src.get_spc2sdz());
  set_spc3sdphi(src.get_spc3sdphi());
  set_spc3sdz(src.get_spc3sdz());
  set_semcsdphi(src.get_semcsdphi());
  set_semcsdz(src.get_semcsdz());
  set_stofsdphi(src.get_stofsdphi());
  set_stofsdz(src.get_stofsdz());
  set_stofwsdphi(src.get_stofwsdphi());
  set_stofwsdz(src.get_stofwsdz());
  set_stecsdphi(src.get_stecsdphi());
  set_stecsdalpha(src.get_stecsdalpha());
  set_hbdsector(src.get_hbdsector());
  set_hbdsize(src.get_hbdsize());
  set_hbdcharge(src.get_hbdcharge());
  set_hbdx(src.get_hbdx());
  set_hbdy(src.get_hbdy());
  set_hbdz(src.get_hbdz());
  set_hbddphi(src.get_hbddphi());
  set_hbddz(src.get_hbddz());
  set_hbdsdphi(src.get_hbdsdphi());
  set_hbdsdz(src.get_hbdsdz());
  set_m2tof(src.get_m2tof());
  set_m2tofw(src.get_m2tofw());
  set_m2emc(src.get_m2emc());
  set_isPi(src.get_isPi());
  set_isK(src.get_isK());
  set_isP(src.get_isP());
  set_isPiTofw(src.get_isPiTofw());
  set_isKTofw(src.get_isKTofw());
  set_isPTofw(src.get_isPTofw());
  set_dcside(src.get_dcside());
  set_pc1sect(src.get_pc1sect());
  set_pc2sect(src.get_pc2sect());
  set_pc3sect(src.get_pc3sect());
  set_pc1phi(src.get_pc1phi());
  set_pc1z(src.get_pc1z());
  set_pc2phi(src.get_pc2phi());
  set_pc2z(src.get_pc2z());
  set_pc3phi(src.get_pc3phi());
  set_pc3z(src.get_pc3z());
  set_tofphi(src.get_tofphi());
  set_tofz(src.get_tofz());
  set_tecphi(src.get_tecphi());
  set_tecalpha(src.get_tecalpha());
  set_emcphi(src.get_emcphi());
  set_emcz(src.get_emcz());
  set_spc1phi(src.get_spc1phi());
  set_spc1z(src.get_spc1z());
  set_spc2phi(src.get_spc2phi());
  set_spc2z(src.get_spc2z());
  set_spc3phi(src.get_spc3phi());
  set_spc3z(src.get_spc3z());
  set_stofphi(src.get_stofphi());
  set_stofz(src.get_stofz());
  set_stecphi(src.get_stecphi());
  set_stecalpha(src.get_stecalpha());
  set_semcphi(src.get_semcphi());
  set_semcz(src.get_semcz());
  set_emcsdphi_e(src.get_emcsdphi_e());
  set_emcsdz_e(src.get_emcsdz_e());
  set_semcsdphi_e(src.get_semcsdphi_e());
  set_semcsdz_e(src.get_semcsdz_e());
  set_tecnhit(src.get_tecnhit());
  set_n2(src.get_n2());
  set_npe2(src.get_npe2());
  set_n3(src.get_n3());
  set_npe3(src.get_npe3());
  set_sn2(src.get_sn2());
  set_snpe2(src.get_snpe2());
  set_sn3(src.get_sn3());
  set_snpe3(src.get_snpe3());
  set_deadmap(src.get_deadmap());
  set_warnmap(src.get_warnmap());
  set_sdeadmap(src.get_sdeadmap());
  set_swarnmap(src.get_swarnmap());
  set_tofecut(src.get_tofecut());
  set_tofsame(src.get_tofsame());
  set_slatnext(src.get_slatnext());
  set_ttofnext(src.get_ttofnext());
  set_etofnext(src.get_etofnext());
  set_tzrid(src.get_tzrid());
  set_pcrid(src.get_pcrid());
  set_ptzrx(src.get_ptzrx());
  set_ptzry(src.get_ptzry());
  set_ptzrz(src.get_ptzrz());
  set_ppcrx(src.get_ppcrx());
  set_ppcry(src.get_ppcry());
  set_ppcrz(src.get_ppcrz());
  set_tzrtof(src.get_tzrtof());
  set_tzrslat(src.get_tzrslat());
  set_tzreloss(src.get_tzreloss());
  set_tzrx(src.get_tzrx());
  set_tzry(src.get_tzry());
  set_tzrz(src.get_tzrz());
  set_pcrtof(src.get_pcrtof());
  set_pcrslat(src.get_pcrslat());
  set_pcreloss(src.get_pcreloss());
  set_pcrx(src.get_pcrx());
  set_pcry(src.get_pcry());
  set_pcrz(src.get_pcrz());
  set_tzrsdphi(src.get_tzrsdphi());
  set_tzrsdz(src.get_tzrsdz());
  set_m2tzr(src.get_m2tzr());
  set_m2ntctof(src.get_m2ntctof());
  set_pltzr(src.get_pltzr());
  set_isPitzr(src.get_isPitzr());
  set_isKtzr(src.get_isKtzr());
  set_isPtzr(src.get_isPtzr());
  set_isPintctof(src.get_isPintctof());
  set_isKntctof(src.get_isKntctof());
  set_isPntctof(src.get_isPntctof());
  set_tzrecut(src.get_tzrecut());
  set_L1Trig(src.get_L1Trig());
  set_pc1wid(src.get_pc1wid());
  set_pc2wid(src.get_pc2wid());
  set_pc3wid(src.get_pc3wid());
  set_tofph1(src.get_tofph1());
  set_tofph2(src.get_tofph2());
  set_toftdc1(src.get_toftdc1());
  set_toftdc2(src.get_toftdc2());
  set_aerindex(src.get_aerindex());
  set_aersindex(src.get_aersindex());
  set_aerph1(src.get_aerph1());
  set_aerph2(src.get_aerph2());
  set_aert1(src.get_aert1());
  set_aert2(src.get_aert2());
  set_aernpe(src.get_aernpe());
  set_aerstatus(src.get_aerstatus());
  set_aerph1_0(src.get_aerph1_0());
  set_aerph2_0(src.get_aerph2_0());
  set_aert1_0(src.get_aert1_0());
  set_aert2_0(src.get_aert2_0());
  set_aerph1_1(src.get_aerph1_1());
  set_aerph2_1(src.get_aerph2_1());
  set_aert1_1(src.get_aert1_1());
  set_aert2_1(src.get_aert2_1());
  set_aerph1_2(src.get_aerph1_2());
  set_aerph2_2(src.get_aerph2_2());
  set_aert1_2(src.get_aert1_2());
  set_aert2_2(src.get_aert2_2());
  set_aerph1_3(src.get_aerph1_3());
  set_aerph2_3(src.get_aerph2_3());
  set_aert1_3(src.get_aert1_3());
  set_aert2_3(src.get_aert2_3());
  set_aerhitid(src.get_aerhitid());
  set_aerhitconfig(src.get_aerhitconfig());
  set_aersph1_0(src.get_aersph1_0());
  set_aersph2_0(src.get_aersph2_0());
  set_aerst1_0(src.get_aerst1_0());
  set_aerst2_0(src.get_aerst2_0());
  set_aersph1_1(src.get_aersph1_1());
  set_aersph2_1(src.get_aersph2_1());
  set_aerst1_1(src.get_aerst1_1());
  set_aerst2_1(src.get_aerst2_1());
  set_aersph1_2(src.get_aersph1_2());
  set_aersph2_2(src.get_aersph2_2());
  set_aerst1_2(src.get_aerst1_2());
  set_aerst2_2(src.get_aerst2_2());
  set_aersph1_3(src.get_aersph1_3());
  set_aersph2_3(src.get_aersph2_3());
  set_aerst1_3(src.get_aerst1_3());
  set_aerst2_3(src.get_aerst2_3());
  set_aershitid(src.get_aershitid());
  set_aershitconfig(src.get_aershitconfig());
  set_tecde(src.get_tecde());
  set_tecde06(src.get_tecde06());
  set_tectrklen(src.get_tectrklen());
  set_tecnde(src.get_tecnde());
  set_tecnhit100(src.get_tecnhit100());
  set_tecnhit200(src.get_tecnhit200());
  set_tecnhit50(src.get_tecnhit50());
  set_tecwtb(src.get_tecwtb());
  set_tecwtbsq(src.get_tecwtbsq());
  set_mcid(src.get_mcid());
  set_dchid(src.get_dchid());
  set_emcrawtdc(src.get_emcrawtdc());
  set_emcrawadc(src.get_emcrawadc());
  set_emcrawadclg(src.get_emcrawadclg());
  set_categoryl2eLowPt(src.get_categoryl2eLowPt());
  set_categoryl2eHighPt(src.get_categoryl2eHighPt());
  set_candIDl2e(src.get_candIDl2e());
  set_nlvl2MatchLowOcupy(src.get_nlvl2MatchLowOcupy());
  set_RawL1(src.get_RawL1());
  set_LivL1(src.get_LivL1());
  set_SclL1(src.get_SclL1());
  set_Px(src.get_Px());
  set_Py(src.get_Py());
  set_Pz(src.get_Pz());
  set_idtrk_tof(src.get_idtrk_tof());
  set_idtrk_tofw(src.get_idtrk_tofw());
  set_idtrk_tec(src.get_idtrk_tec());
  set_idtrk_crk(src.get_idtrk_crk());
  set_idtrk_pc2(src.get_idtrk_pc2());
  set_idtrk_pc3(src.get_idtrk_pc3());
  set_idtrk_stof(src.get_idtrk_stof());
  set_idtrk_stec(src.get_idtrk_stec());
  set_idtrk_scrk(src.get_idtrk_scrk());
  set_idtrk_spc2(src.get_idtrk_spc2());
  set_idtrk_spc3(src.get_idtrk_spc3());
  set_hbdhubcharge(src.get_hbdhubcharge());
  set_hbdspokecharge1(src.get_hbdspokecharge1());
  set_hbdspokecharge2(src.get_hbdspokecharge2());
  set_hbdscharge(src.get_hbdscharge());
  set_hbdlocalmax(src.get_hbdlocalmax());



  set_pc1dphi(src.get_pc1dphi());
  set_pc1dz(src.get_pc1dz());
  set_sdchid(src.get_sdchid());
  set_spc1id(src.get_spc1id());
  set_stofwid(src.get_stofwid());
  set_spemcx(src.get_spemcx());
  set_spemcy(src.get_spemcy());
  set_spemcz(src.get_spemcz());
  set_sppc1x(src.get_sppc1x());
  set_sppc1y(src.get_sppc1y());
  set_sppc1z(src.get_sppc1z());
  set_sppc2x(src.get_sppc2x());
  set_sppc2y(src.get_sppc2y());
  set_sppc2z(src.get_sppc2z());
  set_sppc3x(src.get_sppc3x());
  set_sppc3y(src.get_sppc3y());
  set_sppc3z(src.get_sppc3z());
  set_sptofex(src.get_sptofex());
  set_sptofey(src.get_sptofey());
  set_sptofez(src.get_sptofez());
  set_sptofwx(src.get_sptofwx());
  set_sptofwy(src.get_sptofwy());
  set_sptofwz(src.get_sptofwz());
  set_spc1dphi(src.get_spc1dphi());
  set_spc1dz(src.get_spc1dz());
  set_ptofex(src.get_ptofex());
  set_ptofey(src.get_ptofey());
  set_ptofez(src.get_ptofez());
  set_pltofe(src.get_pltofe());

  set_teccharge(0,src.get_teccharge(0));
  set_teccharge(1,src.get_teccharge(1));
  set_teccharge(2,src.get_teccharge(2));
  set_teccharge(3,src.get_teccharge(3));
  set_teccharge(4,src.get_teccharge(4));
  set_teccharge(5,src.get_teccharge(5));

  set_tecntimebins(0,src.get_tecntimebins(0));
  set_tecntimebins(1,src.get_tecntimebins(1));
  set_tecntimebins(2,src.get_tecntimebins(2));
  set_tecntimebins(3,src.get_tecntimebins(3));
  set_tecntimebins(4,src.get_tecntimebins(4));
  set_tecntimebins(5,src.get_tecntimebins(5));

  set_tecavgtimebin(0,src.get_tecavgtimebin(0));
  set_tecavgtimebin(1,src.get_tecavgtimebin(1));
  set_tecavgtimebin(2,src.get_tecavgtimebin(2));
  set_tecavgtimebin(3,src.get_tecavgtimebin(3));
  set_tecavgtimebin(4,src.get_tecavgtimebin(4));
  set_tecavgtimebin(5,src.get_tecavgtimebin(5));

  set_tecdphiplane(0,src.get_tecdphiplane(0));
  set_tecdphiplane(1,src.get_tecdphiplane(1));
  set_tecdphiplane(2,src.get_tecdphiplane(2));
  set_tecdphiplane(3,src.get_tecdphiplane(3));
  set_tecdphiplane(4,src.get_tecdphiplane(4));
  set_tecdphiplane(5,src.get_tecdphiplane(5));

  set_tecMomentum(src.get_tecMomentum());
  set_tectrlike(src.get_tectrlike());

  set_steccharge(0,src.get_steccharge(0));
  set_steccharge(1,src.get_steccharge(1));
  set_steccharge(2,src.get_steccharge(2));
  set_steccharge(3,src.get_steccharge(3));
  set_steccharge(4,src.get_steccharge(4));
  set_steccharge(5,src.get_steccharge(5));

  set_stecntimebins(0,src.get_stecntimebins(0));
  set_stecntimebins(1,src.get_stecntimebins(1));
  set_stecntimebins(2,src.get_stecntimebins(2));
  set_stecntimebins(3,src.get_stecntimebins(3));
  set_stecntimebins(4,src.get_stecntimebins(4));
  set_stecntimebins(5,src.get_stecntimebins(5));

  set_stecavgtimebin(0,src.get_stecavgtimebin(0));
  set_stecavgtimebin(1,src.get_stecavgtimebin(1));
  set_stecavgtimebin(2,src.get_stecavgtimebin(2));
  set_stecavgtimebin(3,src.get_stecavgtimebin(3));
  set_stecavgtimebin(4,src.get_stecavgtimebin(4));
  set_stecavgtimebin(5,src.get_stecavgtimebin(5));

  set_stecdphiplane(0,src.get_stecdphiplane(0));
  set_stecdphiplane(1,src.get_stecdphiplane(1));
  set_stecdphiplane(2,src.get_stecdphiplane(2));
  set_stecdphiplane(3,src.get_stecdphiplane(3));
  set_stecdphiplane(4,src.get_stecdphiplane(4));
  set_stecdphiplane(5,src.get_stecdphiplane(5));

  set_stecMomentum(src.get_stecMomentum());
  set_stectrlike(src.get_stectrlike());

  for(int itmp=0; itmp<4; itmp++) {set_svxdphi      (itmp,src.get_svxdphi(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_svxdz        (itmp,src.get_svxdz(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_svxid        (itmp,src.get_svxid(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_svxsdphi     (itmp,src.get_svxsdphi(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_svxsdz       (itmp,src.get_svxsdz(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_svxsid       (itmp,src.get_svxsid(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_psvxx        (itmp,src.get_psvxx(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_psvxy        (itmp,src.get_psvxy(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_psvxz        (itmp,src.get_psvxz(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_spsvxx       (itmp,src.get_spsvxx(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_spsvxy       (itmp,src.get_spsvxy(itmp));}
  for(int itmp=0; itmp<4; itmp++) {set_spsvxz       (itmp,src.get_spsvxz(itmp));}
  set_svxtrackid   (src.get_svxtrackid());
  set_svxtrackquality     (src.get_svxtrackquality());
  set_svxdca2d     (src.get_svxdca2d());
  set_svxdca3d     (src.get_svxdca3d());
  set_svxdca2dkf   (src.get_svxdca2dkf());
  set_svxdca3dkf   (src.get_svxdca3dkf());
  for(int itmp=0; itmp<3; itmp++) {set_svxxyz0      (itmp,src.get_svxxyz0(itmp));}
  for(int itmp=0; itmp<3; itmp++) {set_svxpxyz0     (itmp,src.get_svxpxyz0(itmp));}
  for(int itmp=0; itmp<3; itmp++) {set_svxxyzkf     (itmp,src.get_svxxyzkf(itmp));}
  for(int itmp=0; itmp<3; itmp++) {set_svxpxyzkf    (itmp,src.get_svxpxyzkf(itmp));}

  ShutUp(0);
  return ;
}

int
PHSnglCentralTrack::isValid(const float f) const
{
  if (f == -9999)
    {
      return 0;
    }
  return 1;
}

int
PHSnglCentralTrack::isValid(const int i) const
{
  if (i == -9999)
    {
      return 0;
    }
  return 1;
}

int
PHSnglCentralTrack::isImplemented(const float f) const
{
  if (isnan(f))
    {
      return 0;
    }
  return 1;
}

int
PHSnglCentralTrack::isImplemented(const int i) const
{
  if (i == -9998)
    {
      return 0;
    }
  return 1;
}
