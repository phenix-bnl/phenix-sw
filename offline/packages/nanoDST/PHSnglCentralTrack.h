#ifndef __PHSNGLCENTRALTRACK_HH_
#define __PHSNGLCENTRALTRACK_HH_

#include <iostream>

#include <TObject.h>

#include <phool.h>
#include <cmath>

//
//  The SnglCentralTrack is one of the so-called "measured particles"
//  The SnglCentralTrack inherits its 4-vector from the PHParticle class
//  (since, of course a 4-vector is common to ALL kinds of particles)
//  and then adds virtual (junk-filled) versions of fields
//  that are specific to those particles which are measured as SnglCentral 
//  Arm Tracks.
//
//  This completes the set of all virtual base classes for making SnglCentral
//  arm tracks.  Following this, "versioned" SnglCentralTrack objects will
//  inherit from here and provide the real implementations.
//
//                                    TKH 3-7-2002
//
// we need to add the PHObject methods here since this piece of crap does not inherit from PHObject
// like it should.
//
// VERY IMPORTANT!!!!!!!!!!!!!!!!!!!
// If you add a new get/set method (means you introduced yet another variable in one
// of the gazillion PHCentralTrack Versions) do not forget to update the Copy method
// which is used to fill PHSnglCentralTrack Objects generically (otherwise the copying
// of nodes won't work correctly

class PHSnglCentralTrack : public TObject
{

 public:

  virtual ~PHSnglCentralTrack(){}

  virtual void identify(std::ostream &os=std::cout) const;

  // this should be a generic copy content of any PHCentralTrack into any PHCentralTrack
  virtual void Copy(const PHSnglCentralTrack &src);
  virtual void Copy(TObject &object) const { PHOOL_VIRTUAL_WARNING; }
  void Init();
  // kill this stupid virtual warnings
  void ShutUp(const int i=1);

  // Here comes the PHParticle stuff (sorry if we had though about this
  // in greater depth this would not be here)
  virtual void set_px(const float /*val*/) {warning("set_px(const float val)");}
  virtual void set_py(const float /*val*/) {warning("set_py(const float val)");}
  virtual void set_pz(const float /*val*/) {warning("set_pz(const float val)");}
  virtual void set_E(const float /*val*/) {warning("set_E(const float val)");}
  virtual void set_charge(const short /*val*/) {warning("set_charge(const short val)");}
  virtual void set_PID(const short /*val*/) {warning("set_PID(const short val)");}

  virtual float get_px() const;
  virtual float get_py() const;
  virtual float get_pz() const;
  virtual float get_E() const;
  virtual short get_charge() const {return -9998;}
  virtual short get_PID() const {return -9998;}

  // Set the values in the SnglCentralTrack...
  // These virtual functions should ALL be overridden!
  // If the local version is called by mistake, the user sees a
  // warning on their screen.
  //                          THK 3-2-2002
  virtual void set_quality    (const short /*val*/)   {warning("set_quality");}
  virtual void set_zed        (const float /*val*/)   {warning("set_zed");}
  virtual void set_phi        (const float /*val*/)   {warning("set_phi");}
  virtual void set_alpha      (const float /*val*/)   {warning("set_alpha");}
  virtual void set_beta       (const float /*val*/)   {warning("set_beta");}
  virtual void set_momphi0the0 (const float /*valmom*/, const float /*valphi0*/, const float /*valthe0*/) {warning("set_momphi0the0");}
  virtual void set_phi0       (const float /*val*/)   {warning("set_phi0");}
  virtual void set_the0       (const float /*val*/)   {warning("set_the0");}
  virtual void set_mom        (const float /*val*/)   {warning("set_mom");}
  virtual void set_mompx      (const float /*val*/)   {warning("set_mompx");}
  virtual void set_mompy      (const float /*val*/)   {warning("set_mompy");}
  virtual void set_mompz      (const float /*val*/)   {warning("set_mompz");}
  virtual void set_status     (const short /*val*/)   {warning("set_status");}
  virtual void set_alpha1     (const float /*val*/)   {warning("set_alpha1");}
  virtual void set_alpha2     (const float /*val*/)   {warning("set_alpha2");}
  virtual void set_nx1hits    (const short /*val*/)   {warning("set_nx1hits");}
  virtual void set_nx2hits    (const short /*val*/)   {warning("set_nx2hits");}
  virtual void set_mx1dist    (const float /*val*/)   {warning("set_mx1dist");}
  virtual void set_mx2dist    (const float /*val*/)   {warning("set_mx2dist");}
  virtual void set_chi2x1     (const float /*val*/)   {warning("set_chi2x1");}
  virtual void set_chi2x2     (const float /*val*/)   {warning("set_chi2x2");}
  virtual void set_nx1x2fit   (const short /*val*/)   {warning("set_nx1x2fit");}
  virtual void set_mchi2      (const float /*val*/)   {warning("set_mchi2");}
  virtual void set_error      (const float /*val*/)   {warning("set_error");}
  virtual void set_alphaf     (const float /*val*/)   {warning("set_alphaf");}
  virtual void set_pc1id      (const short /*val*/)   {warning("set_pc1id");}
  virtual void set_pc2id      (const short /*val*/)   {warning("set_pc2id");}
  virtual void set_pc3id      (const short /*val*/)   {warning("set_pc3id");}
  virtual void set_emcid      (const short /*val*/)   {warning("set_emcid");}
  virtual void set_tofid      (const short /*val*/)   {warning("set_tofid");}
  virtual void set_tofeid     (const short /*val*/)   {warning("set_tofeid");}
  virtual void set_tecid      (const short /*val*/)   {warning("set_tecid");}
  virtual void set_hbdid      (const short /*val*/)   {warning("set_hbdid");}
  virtual void set_mrpcid     (const short /*val*/)   {warning("set_mrpcid");}
  virtual void set_tofwid     (const short /*val*/)   {warning("set_tofwid");}
  virtual void set_spc2id     (const short /*val*/)   {warning("set_spc2id");}
  virtual void set_spc3id     (const short /*val*/)   {warning("set_spc3id");}
  virtual void set_semcid     (const short /*val*/)   {warning("set_semcid");}
  virtual void set_stofid     (const short /*val*/)   {warning("set_stofid");}
  virtual void set_stofeid    (const short /*val*/)   {warning("set_stofeid");}
  virtual void set_stecid     (const short /*val*/)   {warning("set_stecid");}
  virtual void set_ppc1x      (const float /*val*/)   {warning("set_ppc1x");}
  virtual void set_ppc1y      (const float /*val*/)   {warning("set_ppc1y");}
  virtual void set_ppc1z      (const float /*val*/)   {warning("set_ppc1z");}
  virtual void set_ppc2x      (const float /*val*/)   {warning("set_ppc2x");}
  virtual void set_ppc2y      (const float /*val*/)   {warning("set_ppc2y");}
  virtual void set_ppc2z      (const float /*val*/)   {warning("set_ppc2z");}
  virtual void set_ptecx      (const float /*val*/)   {warning("set_ptecx");}
  virtual void set_ptecy      (const float /*val*/)   {warning("set_ptecy");}
  virtual void set_ptecz      (const float /*val*/)   {warning("set_ptecz");}
  virtual void set_ppc3x      (const float /*val*/)   {warning("set_ppc3x");}
  virtual void set_ppc3y      (const float /*val*/)   {warning("set_ppc3y");}
  virtual void set_ppc3z      (const float /*val*/)   {warning("set_ppc3z");}
  virtual void set_pemcx      (const float /*val*/)   {warning("set_pemcx");}
  virtual void set_pemcy      (const float /*val*/)   {warning("set_pemcy");}
  virtual void set_pemcz      (const float /*val*/)   {warning("set_pemcz");}
  virtual void set_ptofx      (const float /*val*/)   {warning("set_ptofx");}
  virtual void set_ptofy      (const float /*val*/)   {warning("set_ptofy");}
  virtual void set_ptofz      (const float /*val*/)   {warning("set_ptofz");}
  virtual void set_phbdx      (const float /*val*/)   {warning("set_phbdx");}
  virtual void set_phbdy      (const float /*val*/)   {warning("set_phbdy");}
  virtual void set_phbdz      (const float /*val*/)   {warning("set_phbdz");}
  virtual void set_pmrpcx     (const float /*val*/)   {warning("set_pmrpcx");}
  virtual void set_pmrpcy     (const float /*val*/)   {warning("set_pmrpcy");}
  virtual void set_pmrpcz     (const float /*val*/)   {warning("set_ptofwz");}
  virtual void set_ptofwx     (const float /*val*/)   {warning("set_ptofwx");}
  virtual void set_ptofwy     (const float /*val*/)   {warning("set_ptofwy");}
  virtual void set_ptofwz     (const float /*val*/)   {warning("set_pmrpcz");}
  virtual void set_pltof      (const float /*val*/)   {warning("set_pltof");}
  virtual void set_plemc      (const float /*val*/)   {warning("set_plemc");}
  virtual void set_plmrpc     (const float /*val*/)   {warning("set_plmrpc");}
  virtual void set_pltofw     (const float /*val*/)   {warning("set_pltofw");}
  virtual void set_pc2dphi    (const float /*val*/)   {warning("set_pc2dphi");}
  virtual void set_pc2dz      (const float /*val*/)   {warning("set_pc2dz");}
  virtual void set_pc3dphi    (const float /*val*/)   {warning("set_pc3dphi");}
  virtual void set_pc3dz      (const float /*val*/)   {warning("set_pc3dz");}
  virtual void set_emcdphi    (const float /*val*/)   {warning("set_emcdphi");}
  virtual void set_emcdz      (const float /*val*/)   {warning("set_emcdz");}
  virtual void set_tofdphi    (const float /*val*/)   {warning("set_tofdphi");}
  virtual void set_tofdz      (const float /*val*/)   {warning("set_tofdz");}
  virtual void set_tecdphi    (const float /*val*/)   {warning("set_tecdphi");}
  virtual void set_tecdalpha  (const float /*val*/)   {warning("set_tecdalpha");}
  virtual void set_mrpcdphi   (const float /*val*/)   {warning("set_mrpcdphi");}
  virtual void set_mrpcdz     (const float /*val*/)   {warning("set_mrpcdz");}
  virtual void set_tofwdphi   (const float /*val*/)   {warning("set_tofwdphi");}
  virtual void set_tofwdz     (const float /*val*/)   {warning("set_tofwdz");}
  virtual void set_spc2dphi   (const float /*val*/)   {warning("set_spc2dphi");}
  virtual void set_spc2dz     (const float /*val*/)   {warning("set_spc2dz");}
  virtual void set_spc3dphi   (const float /*val*/)   {warning("set_spc3dphi");}
  virtual void set_spc3dz     (const float /*val*/)   {warning("set_spc3dz");}
  virtual void set_semcdphi   (const float /*val*/)   {warning("set_semcdphi");}
  virtual void set_semcdz     (const float /*val*/)   {warning("set_semcdz");}
  virtual void set_stofdphi   (const float /*val*/)   {warning("set_stofdphi");}
  virtual void set_stofdz     (const float /*val*/)   {warning("set_stofdz");}
  virtual void set_stofwdphi  (const float /*val*/)   {warning("set_stofwdphi");}
  virtual void set_stofwdz    (const float /*val*/)   {warning("set_stofwdz");}
  virtual void set_stecdphi   (const float /*val*/)   {warning("set_stecdphi");}
  virtual void set_stecdalpha (const float /*val*/)   {warning("set_stecdalpha");}
  virtual void set_arm        (const short /*val*/)   {warning("set_arm");}
  virtual void set_sect       (const short /*val*/)   {warning("set_sect");}
  virtual void set_ysect      (const short /*val*/)   {warning("set_ysect");}
  virtual void set_zsect      (const short /*val*/)   {warning("set_zsect");}
  virtual void set_ecorr      (const float /*val*/)   {warning("set_ecorr");}
  virtual void set_ecore      (const float /*val*/)   {warning("set_ecore");}
  virtual void set_dep      (const float /*val*/)   {warning("set_dep");}
  virtual void set_emce       (const float /*val*/)   {warning("set_emce");}
  virtual void set_emcdispy   (const float /*val*/)   {warning("set_emcdispy");}
  virtual void set_emcdispz   (const float /*val*/)   {warning("set_emcdispz");}
  virtual void set_temc       (const float /*val*/)   {warning("set_temc");}
  virtual void set_prob       (const float /*val*/)   {warning("set_prob");}
  virtual void set_ecent      (const float /*val*/)   {warning("set_ecent");}
  virtual void set_twrhit     (const short /*val*/)   {warning("set_tset_wrhit");}
  virtual void set_e9         (const float /*val*/)   {warning("set_e9(const float val)");}
  virtual void set_re9        (const float /*val*/)   {warning("set_re9 (const float val)");}
  virtual void set_emcchi2    (const float /*val*/)   {warning("set_emcchi2");}
  virtual void set_sysect     (const short /*val*/)   {warning("set_sysect");}
  virtual void set_szsect     (const short /*val*/)   {warning("set_szsect");}
  virtual void set_secorr     (const float /*val*/)   {warning("set_secorr");}
  virtual void set_secore     (const float /*val*/)   {warning("set_secore");}
  virtual void set_semce      (const float /*val*/)   {warning("set_semce");}
  virtual void set_semcdispy  (const float /*val*/)   {warning("set_semcdispy");}
  virtual void set_semcdispz  (const float /*val*/)   {warning("set_semcdispz");}
  virtual void set_stemc      (const float /*val*/)   {warning("set_stemc");}
  virtual void set_sprob      (const float /*val*/)   {warning("set_sprob");}
  virtual void set_secent     (const float /*val*/)   {warning("set_secent");}
  virtual void set_stwrhit    (const short /*val*/)   {warning("set_stwrhit");}
  virtual void set_se9        (const float /*val*/)   {warning("set_se9");}
  virtual void set_sre9       (const float /*val*/)   {warning("set_sre9");}
  virtual void set_semcchi2   (const float /*val*/)   {warning("set_semcchi2");}
  virtual void set_slat       (const int   /*val*/)   {warning("set_slat");}
  virtual void set_ttof       (const float /*val*/)   {warning("set_ttof");}
  virtual void set_etof       (const float /*val*/)   {warning("set_etof");}
  virtual void set_sttof      (const float /*val*/)   {warning("set_sttof");}
  virtual void set_setof      (const float /*val*/)   {warning("set_setof");}
  virtual void set_slat_mrpc  (const int   /*val*/)   {warning("set_slat_mrpc");}
  virtual void set_ttof_mrpc  (const float /*val*/)   {warning("set_ttof_mrpc");}
  virtual void set_ttofd_mrpc (const float /*val*/)   {warning("set_ttofd_mrpc");}
  virtual void set_qtof_mrpc  (const float /*val*/)   {warning("set_qtof_mrpc");}
  virtual void set_striptofw  (const int   /*val*/)   {warning("set_stripmrpc");}

  virtual void set_tofwx      (const float /*val*/)   {warning("set_tofwx");}
  virtual void set_tofwy      (const float /*val*/)   {warning("set_tofwy");}
  virtual void set_tofwz      (const float /*val*/)   {warning("set_tofwz");}

  virtual void set_ttofw      (const float /*val*/)   {warning("set_ttofw");}
  virtual void set_qtofw      (const float /*val*/)   {warning("set_qtofw");}
  virtual void set_tofwadcup  (const float /*val*/)   {warning("set_tofwadcup");}
  virtual void set_tofwadcdw  (const float /*val*/)   {warning("set_tofwadcdw");}
  virtual void set_tofwtdcup  (const float /*val*/)   {warning("set_tofwtdcup");}
  virtual void set_tofwtdcdw  (const float /*val*/)   {warning("set_tofwtdcdw");}
  virtual void set_acc        (const short /*val*/)   {warning("set_acc");}
  virtual void set_ring       (const int   /*val*/)   {warning("set_ring");}
  virtual void set_n0         (const short /*val*/)   {warning("set_n0");}
  virtual void set_npe0       (const float /*val*/)   {warning("set_npe0");}
  virtual void set_n1         (const short /*val*/)   {warning("set_n1");}
  virtual void set_npe1       (const float /*val*/)   {warning("set_npe1");}
  virtual void set_chi2       (const float /*val*/)   {warning("set_chi2");}
  virtual void set_disp       (const float /*val*/)   {warning("set_disp");}
  virtual void set_tcrk       (const float /*val*/)   {warning("set_tcrk");}
  virtual void set_cross_phi  (const float /*val*/)   {warning("set_cross_phi");}
  virtual void set_cross_z    (const float /*val*/)   {warning("set_cross_z");}
  virtual void set_center_phi (const float /*val*/)   {warning("set_center_phi");}
  virtual void set_center_z   (const float /*val*/)   {warning("set_center_z");}
  virtual void set_sacc       (const short /*val*/)   {warning("set_sacc");}
  virtual void set_sring      (const int   /*val*/)   {warning("set_sring");}
  virtual void set_sn0        (const short /*val*/)   {warning("set_sn0");}
  virtual void set_snpe0      (const float /*val*/)   {warning("set_snpe0");}
  virtual void set_sn1        (const short /*val*/)   {warning("set_sn1");}
  virtual void set_snpe1      (const float /*val*/)   {warning("set_snpe1");}
  virtual void set_schi2      (const float /*val*/)   {warning("set_schi2");}
  virtual void set_sdisp      (const float /*val*/)   {warning("set_sdisp");}
  virtual void set_stcrk      (const float /*val*/)   {warning("set_stcrk");}
  virtual void set_scross_phi (const float /*val*/)   {warning("set_scross_phi");}
  virtual void set_scross_z   (const float /*val*/)   {warning("set_scross_z");}
  virtual void set_scenter_phi(const float /*val*/)   {warning("set_scenter_phi");}
  virtual void set_scenter_z  (const float /*val*/)   {warning("set_scenter_z");}
  virtual void set_tecdedx1   (const float /*val*/)   {warning("set_tecdedx1");}
  virtual void set_tecdedx2   (const float /*val*/)   {warning("set_tecdedx2");}
  virtual void set_pc2sdphi   (const float /*val*/)   {warning("set_pc2sdphi");}
  virtual void set_pc2sdz     (const float /*val*/)   {warning("set_pc2sdz");}
  virtual void set_pc3sdphi   (const float /*val*/)   {warning("set_pc3sdphi");}
  virtual void set_pc3sdz     (const float /*val*/)   {warning("set_pc3sdz");}
  virtual void set_emcsdphi   (const float /*val*/)   {warning("set_emcsdphi");}
  virtual void set_emcsdz     (const float /*val*/)   {warning("set_emcsdz");}
  virtual void set_tofsdphi   (const float /*val*/)   {warning("set_tofsdphi");}
  virtual void set_tofsdz     (const float /*val*/)   {warning("set_tofsdz");}
  virtual void set_tofwsdphi  (const float /*val*/)   {warning("set_tofwsdphi");}
  virtual void set_tofwsdz    (const float /*val*/)   {warning("set_tofwsdz");}
  virtual void set_tecsdphi   (const float /*val*/)   {warning("set_tecsdphi");}
  virtual void set_tecsdalpha (const float /*val*/)   {warning("set_tecsdalpha");}
  virtual void set_spc2sdphi  (const float /*val*/)   {warning("set_spc2sdphi");}
  virtual void set_spc2sdz    (const float /*val*/)   {warning("set_spc2sdz");}
  virtual void set_spc3sdphi  (const float /*val*/)   {warning("set_spc3sdphi");}
  virtual void set_spc3sdz    (const float /*val*/)   {warning("set_spc3sdz");}
  virtual void set_semcsdphi  (const float /*val*/)   {warning("set_semcsdphi");}
  virtual void set_semcsdz    (const float /*val*/)   {warning("set_semcsdz");}
  virtual void set_stofsdphi  (const float /*val*/)   {warning("set_stofsdphi");}
  virtual void set_stofsdz    (const float /*val*/)   {warning("set_stofsdz");}
  virtual void set_stofwsdphi (const float /*val*/)   {warning("set_stofwsdphi");}
  virtual void set_stofwsdz   (const float /*val*/)   {warning("set_stofwsdz");}
  virtual void set_stecsdphi  (const float /*val*/)   {warning("set_stecsdphi");}
  virtual void set_stecsdalpha(const float /*val*/)   {warning("set_stecsdalpha");}
  virtual void set_hbdsector  (const short /*val*/)   {warning("set_hbdsector");}
  virtual void set_hbdsize    (const short /*val*/)   {warning("set_hbdsize");}
  virtual void set_hbdcharge  (const float /*val*/)   {warning("set_hbdcharge");}
  virtual void set_hbdx       (const float /*val*/)   {warning("set_hbdx");}
  virtual void set_hbdy       (const float /*val*/)   {warning("set_hbdy");}
  virtual void set_hbdz       (const float /*val*/)   {warning("set_hbdz");}
  virtual void set_hbddphi    (const float /*val*/)   {warning("set_hbddphi");}
  virtual void set_hbddz      (const float /*val*/)   {warning("set_hbddz");}
  virtual void set_hbdsdphi   (const float /*val*/)   {warning("set_hbdsdphi");}
  virtual void set_hbdsdz     (const float /*val*/)   {warning("set_hbdsdz");}

  virtual void set_m2tof      (const float /*val*/)   {warning("set_m2tof");}
  virtual void set_m2tofw     (const float /*val*/)   {warning("set_m2tofw");}
  virtual void set_m2emc      (const float /*val*/)   {warning("set_m2emc");}
  virtual void set_isPi       (const float /*val*/)   {warning("set_isPi");}
  virtual void set_isK        (const float /*val*/)   {warning("set_isK");}
  virtual void set_isP        (const float /*val*/)   {warning("set_isP");}
  virtual void set_isPiTofw   (const float /*val*/)   {warning("set_isPiTofw");}
  virtual void set_isKTofw    (const float /*val*/)   {warning("set_isKTofw");}
  virtual void set_isPTofw    (const float /*val*/)   {warning("set_isPTofw");}
  virtual void set_categoryl2eLowPt(const long /*input*/) {warning("set_categoryl2eLowPt");}
  virtual void set_categoryl2eHighPt(const long /*input*/){warning("set_categoryl2eHighPt");}
  virtual void set_candIDl2e  (const short /*input*/){warning("set_candIDl2e");}
  virtual void set_nlvl2MatchLowOcupy(const int /*input*/) {warning("set_nlvl2MatchLowOcupy");}
  virtual void set_RawL1(const int /*val*/) {warning("set_RawL1(const int val)");}
  virtual void set_LivL1(const int /*val*/) {warning("set_LivL1(const int val)");}
  virtual void set_SclL1(const int /*val*/) {warning("set_SclL1(const int val)");}
  virtual void set_dcarm      (const short /*val*/)  {warning("set_dcarm");}
  virtual void set_dcside     (const short /*val*/)  {warning("set_dcside");}
  virtual void set_pc1sect    (const short /*val*/)  {warning("set_pc1sect");}
  virtual void set_pc2sect    (const short /*val*/)  {warning("set_pc2sect");}
  virtual void set_pc3sect    (const short /*val*/)  {warning("set_pc3sect");}
  virtual void set_pc1phi     (const float /*val*/)  {warning("set_pc1phi");}
  virtual void set_pc1z	      (const float /*val*/)  {warning("set_pc1z");}
  virtual void set_pc2phi     (const float /*val*/)  {warning("set_pc2phi");}
  virtual void set_pc2z	      (const float /*val*/)  {warning("set_pc2z");}
  virtual void set_pc3phi     (const float /*val*/)  {warning("set_pc3phi");}
  virtual void set_pc3z	      (const float /*val*/)  {warning("set_pc3z");}
  virtual void set_tofphi     (const float /*val*/)  {warning("set_tofphi");}
  virtual void set_tofz	      (const float /*val*/)  {warning("set_tofz");}
  virtual void set_tecphi     (const float /*val*/)  {warning("set_tecphi");}
  virtual void set_tecalpha   (const float /*val*/)  {warning("set_tecalpha");}
  virtual void set_emcphi     (const float /*val*/)  {warning("set_emcphi");}
  virtual void set_emcz	      (const float /*val*/)  {warning("set_emcz");}
  virtual void set_spc1phi    (const float /*val*/)  {warning("set_spc1phi");}
  virtual void set_spc1z      (const float /*val*/)  {warning("set_spc1z");}
  virtual void set_spc2phi    (const float /*val*/)  {warning("set_spc2phi");}
  virtual void set_spc2z      (const float /*val*/)  {warning("set_spc2z");}
  virtual void set_spc3phi    (const float /*val*/)  {warning("set_spc3phi");}
  virtual void set_spc3z      (const float /*val*/)  {warning("set_spc3z");}
  virtual void set_stofphi    (const float /*val*/)  {warning("set_stofphi");}
  virtual void set_stofz      (const float /*val*/)  {warning("set_stofz");}
  virtual void set_stecphi    (const float /*val*/)  {warning("set_stecphi");}
  virtual void set_stecalpha  (const float /*val*/)  {warning("set_stecalpha");}
  virtual void set_semcphi    (const float /*val*/)  {warning("set_emcphi");}
  virtual void set_semcz      (const float /*val*/)  {warning("set_emcz");}
  virtual void set_emcsdphi_e (const float /*val*/)  {warning("set_emcsdphi_e");}
  virtual void set_emcsdz_e   (const float /*val*/)  {warning("set_emcsdz_e");}
  virtual void set_semcsdphi_e(const float /*val*/)  {warning("set_semcsdphi_e");}
  virtual void set_semcsdz_e  (const float /*val*/)  {warning("set_semcsdz_e");}
  virtual void set_tecnhit    (const short /*val*/)  {warning("set_tecnhit");}
  virtual void set_n2	      (const short /*val*/)  {warning("set_n2");}
  virtual void set_npe2	      (const float /*val*/)  {warning("set_npe2");}
  virtual void set_n3	      (const short /*val*/)  {warning("set_n3");}
  virtual void set_npe3	      (const float /*val*/)  {warning("set_npe3");}
  virtual void set_sn2	      (const short /*val*/)  {warning("set_sn2");}
  virtual void set_snpe2      (const float /*val*/)  {warning("set_snpe2");}
  virtual void set_sn3	      (const short /*val*/)  {warning("set_sn3");}
  virtual void set_snpe3      (const float /*val*/)  {warning("set_snpe3");}
  virtual void set_deadmap    (const int   /*val*/)  {warning("set_deadmap");}
  virtual void set_warnmap    (const int   /*val*/)  {warning("set_warnmap");}
  virtual void set_sdeadmap   (const int   /*val*/)  {warning("set_sdeadmap");}
  virtual void set_swarnmap   (const int   /*val*/)  {warning("set_swarnmap");}
  virtual void set_tofecut    (const short /*val*/)  {warning("set_tofecut");}
  virtual void set_tofsame    (const short /*val*/)  {warning("set_tofsame");}
  virtual void set_slatnext   (const short /*val*/)  {warning("set_slatnext");}
  virtual void set_ttofnext   (const float /*val*/)  {warning("set_ttofnext");}
  virtual void set_etofnext   (const float /*val*/)  {warning("set_etofnext");}
  virtual void set_tzrid      (const short /*val*/)  {warning("set_tzrid");}
  virtual void set_pcrid      (const short /*val*/)  {warning("set_pcrid");}
  virtual void set_ptzrx      (const float /*val*/)  {warning("set_ptzrx");}
  virtual void set_ptzry      (const float /*val*/)  {warning("set_ptzry");}
  virtual void set_ptzrz      (const float /*val*/)  {warning("set_ptzrz");}
  virtual void set_ppcrx      (const float /*val*/)  {warning("set_ppcrx");}
  virtual void set_ppcry      (const float /*val*/)  {warning("set_ppcry");}
  virtual void set_ppcrz      (const float /*val*/)  {warning("set_ppcrz");}
  virtual void set_tzrtof     (const float /*val*/)  {warning("set_tzrtof");}
  virtual void set_tzrslat    (const short /*val*/)  {warning("set_tzrslat");}
  virtual void set_tzreloss   (const float /*val*/)  {warning("set_tzreloss");}
  virtual void set_tzrx	      (const float /*val*/)  {warning("set_tzrx");}
  virtual void set_tzry	      (const float /*val*/)  {warning("set_tzry");}
  virtual void set_tzrz	      (const float /*val*/)  {warning("set_tzrz");}
  virtual void set_pcrtof     (const float /*val*/)  {warning("set_pcrtof");}
  virtual void set_pcrslat    (const short /*val*/)  {warning("set_pcrslat");}
  virtual void set_pcreloss   (const float /*val*/)  {warning("set_pcreloss");}
  virtual void set_pcrx	      (const float /*val*/)  {warning("set_pcrx");}
  virtual void set_pcry	      (const float /*val*/)  {warning("set_pcry");}
  virtual void set_pcrz	      (const float /*val*/)  {warning("set_pcrz");}
  // new stuff HWG v3
  virtual void set_tzrsdphi   (const float /*val*/)  {warning("set_tzrsdphi");}
  virtual void set_tzrsdz     (const float /*val*/)  {warning("set_tzrsdz" );}
  virtual void set_m2tzr      (const float /*val*/)  {warning("set_m2tzr"  );}
  virtual void set_m2ntctof   (const float /*val*/)  {warning("set_m2ntctof");}
  virtual void set_pltzr      (const float /*val*/)  {warning("set_pltzr"  );}
  virtual void set_isPitzr    (const float /*val*/)  {warning("set_isPitzr");}
  virtual void set_isKtzr     (const float /*val*/)  {warning("set_isKtzr");}
  virtual void set_isPtzr     (const float /*val*/)  {warning("set_isPtzr");}
  virtual void set_isPintctof (const float /*val*/)  {warning("set_isPintctof");}
  virtual void set_isKntctof  (const float /*val*/)  {warning("set_isKntctof");}
  virtual void set_isPntctof  (const float /*val*/)  {warning("set_isPntctof");}
  virtual void set_tzrecut    (const short /*val*/)  {warning("set_tzrecut" );}
  virtual void set_L1Trig     (const int   /*val*/)  {warning("set_L1Trig(const int val)");}
  virtual void set_pc1wid     (const float /*val*/)  {warning("set_pc1wid(const float val)");}
  virtual void set_pc2wid     (const float /*val*/)  {warning("set_pc2wid(const float val)");}
  virtual void set_pc3wid     (const float /*val*/)  {warning("set_pc3wid(const float val)");}
  virtual void set_tofph1     (const float /*val*/)  {warning("set_tofph1");}
  virtual void set_tofph2     (const float /*val*/)  {warning("set_tofph2");}
  virtual void set_toftdc1    (const float /*val*/)  {warning("set_toftdc1");}
  virtual void set_toftdc2    (const float /*val*/)  {warning("set_toftdc2");}
  virtual void set_aerindex   (const int /*val*/)    {warning("set_aerindex");}
  virtual void set_aersindex  (const int /*val*/)    {warning("set_aersindex");}
  virtual void set_aerph1     (const float /*val*/)  {warning("set_aerph1");}
  virtual void set_aerph2     (const float /*val*/)  {warning("set_aerph2");}
  virtual void set_aert1      (const float /*val*/)  {warning("set_aert1");}
  virtual void set_aert2      (const float /*val*/)  {warning("set_aert2");}
  virtual void set_aernpe     (const float /*val*/)  {warning("set_aernpe");}
  virtual void set_aerstatus  (const short /*val*/)  {warning("set_aerstatus");}
  virtual void set_aerph1_0     (const float /*val*/)  {warning("set_aerph1_0");}
  virtual void set_aerph2_0     (const float /*val*/)  {warning("set_aerph2_0");}
  virtual void set_aert1_0      (const float /*val*/)  {warning("set_aert1_0");}
  virtual void set_aert2_0      (const float /*val*/)  {warning("set_aert2_0");}
  virtual void set_aerph1_1     (const float /*val*/)  {warning("set_aerph1_1");}
  virtual void set_aerph2_1     (const float /*val*/)  {warning("set_aerph2_1");}
  virtual void set_aert1_1      (const float /*val*/)  {warning("set_aert1_1");}
  virtual void set_aert2_1      (const float /*val*/)  {warning("set_aert2_1");}
  virtual void set_aerph1_2     (const float /*val*/)  {warning("set_aerph1_2");}
  virtual void set_aerph2_2     (const float /*val*/)  {warning("set_aerph2_2");}
  virtual void set_aert1_2      (const float /*val*/)  {warning("set_aert1_2");}
  virtual void set_aert2_2      (const float /*val*/)  {warning("set_aert2_2");}
  virtual void set_aerph1_3     (const float /*val*/)  {warning("set_aerph1_3");}
  virtual void set_aerph2_3     (const float /*val*/)  {warning("set_aerph2_3");}
  virtual void set_aert1_3      (const float /*val*/)  {warning("set_aert1_3");}
  virtual void set_aert2_3      (const float /*val*/)  {warning("set_aert2_3");}
  virtual void set_aerhitid     (const int /*val*/)    {warning("set_aerhitid");}
  virtual void set_aerhitconfig (const int /*val*/)    {warning("set_aerhitconfig");}
  virtual void set_aersph1_0     (const float /*val*/)  {warning("set_aersph1_0");}
  virtual void set_aersph2_0     (const float /*val*/)  {warning("set_aersph2_0");}
  virtual void set_aerst1_0      (const float /*val*/)  {warning("set_aerst1_0");}
  virtual void set_aerst2_0      (const float /*val*/)  {warning("set_aerst2_0");}
  virtual void set_aersph1_1     (const float /*val*/)  {warning("set_aersph1_1");}
  virtual void set_aersph2_1     (const float /*val*/)  {warning("set_aersph2_1");}
  virtual void set_aerst1_1      (const float /*val*/)  {warning("set_aerst1_1");}
  virtual void set_aerst2_1      (const float /*val*/)  {warning("set_aerst2_1");}
  virtual void set_aersph1_2     (const float /*val*/)  {warning("set_aersph1_2");}
  virtual void set_aersph2_2     (const float /*val*/)  {warning("set_aersph2_2");}
  virtual void set_aerst1_2      (const float /*val*/)  {warning("set_aerst1_2");}
  virtual void set_aerst2_2      (const float /*val*/)  {warning("set_aerst2_2");}
  virtual void set_aersph1_3     (const float /*val*/)  {warning("set_aersph1_3");}
  virtual void set_aersph2_3     (const float /*val*/)  {warning("set_aersph2_3");}
  virtual void set_aerst1_3      (const float /*val*/)  {warning("set_aerst1_3");}
  virtual void set_aerst2_3      (const float /*val*/)  {warning("set_aerst2_3");}
  virtual void set_aershitid     (const int /*val*/)    {warning("set_aershitid");}
  virtual void set_aershitconfig (const int /*val*/)    {warning("set_aershitconfig");}
  virtual void set_tecde      (const float /*val*/)  {warning("set_tecde");}
  virtual void set_tecde06    (const float /*val*/)  {warning("set_tecde06");}
  virtual void set_tectrklen  (const float /*val*/)  {warning("set_tectrklen");}
  virtual void set_tecnde     (const short /*val*/)  {warning("set_tecnde");}
  virtual void set_tecnhit100 (const short /*val*/)  {warning("set_tecnhit100");}
  virtual void set_tecnhit200 (const short /*val*/)  {warning("set_tecnhit200");}
  virtual void set_tecnhit50  (const short /*val*/)  {warning("set_tecnhit50");}
  virtual void set_tecwtb     (const float /*val*/)  {warning("set_tecwtb");}
  virtual void set_tecwtbsq   (const float /*val*/)  {warning("set_tecwtbsq");}
  virtual void set_mcid       (const int   /*val*/)  {warning("set_mcid");}
  virtual void set_dchid      (const int   /*val*/)  {warning("set_dchid");}
  virtual void set_emcrawtdc  (const int   /*val*/)  {warning("set_emcrawtdc");}
  virtual void set_emcrawadc  (const int   /*val*/)  {warning("set_emcrawadc");}
  virtual void set_emcrawadclg(const int   /*val*/)  {warning("set_emcrawadclg");}
  virtual void set_Px(const float /*val*/)  {warning("set_Px");}
  virtual void set_Py(const float /*val*/)  {warning("set_Py");}
  virtual void set_Pz(const float /*val*/)  {warning("set_Pz");}
  virtual void set_idtrk_tof(const int /*val*/) {warning("set_idtrk_tof");}
  virtual void set_idtrk_tofw(const int /*val*/) {warning("set_idtrk_tofw");}
  virtual void set_idtrk_tec(const int /*val*/) {warning("set_idtrk_tec");}
  virtual void set_idtrk_crk(const int /*val*/) {warning("set_idtrk_crk");}
  virtual void set_idtrk_pc2(const int /*val*/) {warning("set_idtrk_pc2");}
  virtual void set_idtrk_pc3(const int /*val*/) {warning("set_idtrk_pc3");}
  virtual void set_idtrk_hbd(const int /*val*/) {warning("set_idtrk_hbd");}
  virtual void set_idtrk_stof(const int /*val*/) {warning("set_idtrk_stof");}
  virtual void set_idtrk_stec(const int /*val*/) {warning("set_idtrk_stec");}
  virtual void set_idtrk_scrk(const int /*val*/) {warning("set_idtrk_scrk");}
  virtual void set_idtrk_spc2(const int /*val*/) {warning("set_idtrk_spc2");}
  virtual void set_idtrk_spc3(const int /*val*/) {warning("set_idtrk_spc3");}
  virtual void set_hbdhubcharge     (const float /*val*/) {warning("set_hbdhubcharge");   }
  virtual void set_hbdspokecharge1  (const float /*val*/) {warning("set_hbdspokecharge1");}
  virtual void set_hbdspokecharge2  (const float /*val*/) {warning("set_hbdspokecharge2");}
  virtual void set_hbdscharge   (const float /*val*/)   {warning("set_hbdscharge");}
  virtual void set_hbdlocalmax     (const float /*val*/)   {warning("set_hbdlocalmax");}


  virtual void set_pc1dphi    (const float /*val*/)  {warning("set_pc1dphi");}
  virtual void set_pc1dz      (const float /*val*/)  {warning("set_pc1dz");}
  virtual void set_sdchid     (const int /*val*/)   {warning("set_sdchid");}
  virtual void set_spc1id     (const short /*val*/)   {warning("set_spc1id");}
  virtual void set_stofwid    (const short /*val*/)   {warning("set_stofwid");}
  virtual void set_spemcx     (const float /*val*/)  {warning("set_spemcx");}
  virtual void set_spemcy     (const float /*val*/)  {warning("set_spemcy");}
  virtual void set_spemcz     (const float /*val*/)  {warning("set_spemcz");}
  virtual void set_sppc1x     (const float /*val*/)   {warning("set_sppc1x");}
  virtual void set_sppc1y     (const float /*val*/)   {warning("set_sppc1y");}
  virtual void set_sppc1z     (const float /*val*/)   {warning("set_sppc1z");}
  virtual void set_sppc2x     (const float /*val*/)   {warning("set_sppc2x");}
  virtual void set_sppc2y     (const float /*val*/)   {warning("set_sppc2y");}
  virtual void set_sppc2z     (const float /*val*/)   {warning("set_sppc2z");}
  virtual void set_sppc3x     (const float /*val*/)   {warning("set_sppc3x");}
  virtual void set_sppc3y     (const float /*val*/)   {warning("set_sppc3y");}
  virtual void set_sppc3z     (const float /*val*/)   {warning("set_sppc3z");}
  virtual void set_sptofex      (const float /*val*/)   {warning("set_sptofex");}
  virtual void set_sptofey      (const float /*val*/)   {warning("set_sptofey");}
  virtual void set_sptofez      (const float /*val*/)   {warning("set_sptofez");}
  virtual void set_sptofwx      (const float /*val*/)   {warning("set_sptofwx");}
  virtual void set_sptofwy      (const float /*val*/)   {warning("set_sptofwy");}
  virtual void set_sptofwz      (const float /*val*/)   {warning("set_sptofwz");}
  virtual void set_spc1dphi   (const float /*val*/)  {warning("set_spc1dphi");}
  virtual void set_spc1dz     (const float /*val*/)  {warning("set_spc1dz");}
  virtual void set_ptofex      (const float /*val*/)   {warning("set_ptofex");}
  virtual void set_ptofey      (const float /*val*/)   {warning("set_ptofey");}
  virtual void set_ptofez      (const float /*val*/)   {warning("set_ptofez");}
  virtual void set_pltofe      (const float /*val*/)   {warning("set_pltofe");}

  virtual void set_teccharge      (const unsigned int /*iplane*/,  const float /*val*/)   {warning("set_teccharge");}
  virtual void set_tecntimebins   (const unsigned int /*iplane*/,  const int /*val*/)   {warning("set_tecntimebins");}
  virtual void set_tecavgtimebin  (const unsigned int /*iplane*/,  const int /*val*/)   {warning("set_tecavgtimebin");}
  virtual void set_tecdphiplane   (const unsigned int /*iplane*/,  const float /*val*/)   {warning("set_tecdphiplane");}
  virtual void set_tecMomentum    (const float /*val*/)   {warning("set_tecMomentum");}
  virtual void set_tectrlike      (const float /*val*/)   {warning("set_tectrlike");}

  virtual void set_steccharge      (const unsigned int /*iplane*/,  const float /*val*/)   {warning("set_steccharge");}
  virtual void set_stecntimebins   (const unsigned int /*iplane*/,  const int /*val*/)   {warning("set_stecntimebins");}
  virtual void set_stecavgtimebin  (const unsigned int /*iplane*/,  const int /*val*/)   {warning("set_stecavgtimebin");}
  virtual void set_stecdphiplane   (const unsigned int /*iplane*/,  const float /*val*/)   {warning("set_stecdphiplane");}
  virtual void set_stecMomentum    (const float /*val*/)   {warning("set_stecMomentum");}
  virtual void set_stectrlike      (const float /*val*/)   {warning("set_stectrlike");}

  // Silicon Vertex (SVX) stuff
  virtual void set_svxdphi            (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_svxdphi");}
  virtual void set_svxdz              (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_svxdz");}
  virtual void set_svxid              (const unsigned int /*ilayer*/, const int /*val*/)    {warning("set_svxid");}
  virtual void set_svxsdphi           (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_svxsdphi");}
  virtual void set_svxsdz             (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_svxsdz");}
  virtual void set_svxsid             (const unsigned int /*ilayer*/, const int /*val*/)  {warning("set_svxsid");}
  virtual void set_psvxx              (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_psvxx");}
  virtual void set_psvxy              (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_psvxy");}
  virtual void set_psvxz              (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_psvxz");}
  virtual void set_spsvxx             (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_spsvxx");}
  virtual void set_spsvxy             (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_spsvxy");}
  virtual void set_spsvxz             (const unsigned int /*ilayer*/, const float /*val*/)  {warning("set_spsvxz");}
  // entries above are for svx clusters, entries below ate for svx standalone tracks
  virtual void set_svxtrackid         (const int /*val*/)  {warning("set_svxtrackid");}
  virtual void set_svxtrackquality           (const float /*val*/)  {warning("set_svxtrackquality");} // standalone
  virtual void set_svxdca2d           (const float /*val*/)  {warning("set_svxdca2d");} // standalone
  virtual void set_svxdca3d           (const float /*val*/)  {warning("set_svxdca3d");}
  virtual void set_svxdca2dkf         (const float /*val*/)  {warning("set_svxdca2d");} // KalFit
  virtual void set_svxdca3dkf         (const float /*val*/)  {warning("set_svxdca3d");}
  // The entries below represent a point of closest approach, and
  // track momentum components ar this point.
  // They are needed for secondary vertex determination for a pair of tracks
  // They come in two flavors: one determined by svx standalone tracking (0)
  // and the second one determined by KalFit (kf)
  virtual void set_svxxyz0            (const unsigned int /*icoor*/, const float /*val*/)  {warning("set_svxxyz0");}
  virtual void set_svxpxyz0           (const unsigned int /*icoor*/, const float /*val*/)  {warning("set_svxpxyz0");}
  virtual void set_svxxyzkf           (const unsigned int /*icoor*/, const float /*val*/)  {warning("set_svxxyzkf");}
  virtual void set_svxpxyzkf          (const unsigned int /*icoor*/, const float /*val*/)  {warning("set_svxpxyzkf");}


  // Get the values from the SnglCentralTrack...
  // The virtual base class prints warning then returns crap...
  virtual short get_quality    () const {warning("get_quality()"); return -9998;}
  virtual float get_zed        () const {warning("get_zed()"); return NAN;}
  virtual float get_phi        () const {warning("get_phi()"); return NAN;}
  virtual float get_alpha      () const {warning("get_alpha()"); return NAN;}
  virtual float get_beta       () const {warning("get_beta()"); return NAN;}
  virtual float get_phi0       () const {warning("get_phi0()"); return NAN;}
  virtual float get_the0       () const {warning("get_the0()"); return NAN;}
  virtual float get_mom        () const {warning("get_mom()"); return NAN;}
  virtual float get_mompx      () const {warning("get_mompx()"); return NAN;}
  virtual float get_mompy      () const {warning("get_mompy()"); return NAN;}
  virtual float get_mompz      () const {warning("get_mompz()"); return NAN;}
  virtual short get_status     () const {warning("get_status()"); return -9998;}
  virtual float get_alpha1     () const {warning("get_alpha1()"); return NAN;}
  virtual float get_alpha2     () const {warning("get_alpha2()"); return NAN;}
  virtual short get_nx1hits    () const {warning("get_nx1hits()"); return -9998;}
  virtual short get_nx2hits    () const {warning("get_nx2hits()"); return -9998;}
  virtual float get_mx1dist    () const {warning("get_mx1dist()"); return NAN;}
  virtual float get_mx2dist    () const {warning("get_mx2dist()"); return NAN;}
  virtual float get_chi2x1     () const {warning("get_chi2x1()"); return NAN;}
  virtual float get_chi2x2     () const {warning("get_chi2x2()"); return NAN;}
  virtual short get_nx1x2fit   () const {warning("get_nx1x2fit()"); return -9998;}
  virtual float get_mchi2      () const {warning("get_mchi2()"); return NAN;}
  virtual float get_error      () const {warning("get_error()"); return NAN;}
  virtual float get_alphaf     () const {warning("get_alphaf()"); return NAN;}
  virtual short get_pc1id      () const {warning("get_pc1id()"); return -9998;}
  virtual short get_pc2id      () const {warning("get_pc2id()"); return -9998;}
  virtual short get_pc3id      () const {warning("get_pc3id()"); return -9998;}
  virtual short get_emcid      () const {warning("get_emcid()"); return -9998;}
  virtual short get_tofid      () const {warning("get_tofid()"); return -9998;}
  virtual short get_tofeid     () const {warning("get_tofid()"); return -9998;}
  virtual short get_tecid      () const {warning("get_tecid()"); return -9998;}
  virtual short get_hbdid      () const {warning("get_hbdid()"); return -9998;}
  virtual short get_mrpcid     () const {warning("get_mrpcid()"); return -9998;}
  virtual short get_tofwid     () const {warning("get_tofwid()"); return -9998;}
  virtual short get_spc2id     () const {warning("get_spc2id()"); return -9998;}
  virtual short get_spc3id     () const {warning("get_spc3id()"); return -9998;}
  virtual short get_semcid     () const {warning("get_semcid()"); return -9998;}
  virtual short get_stofid     () const {warning("get_stofid()"); return -9998;}
  virtual short get_stofeid    () const {warning("get_stofeid()"); return -9998;}
  virtual short get_stecid     () const {warning("get_stecid()"); return -9998;}
  virtual float get_ppc1x      () const {warning("get_ppc1x()"); return NAN;}
  virtual float get_ppc1y      () const {warning("get_ppc1y()"); return NAN;}
  virtual float get_ppc1z      () const {warning("get_ppc1z()"); return NAN;}
  virtual float get_ppc2x      () const {warning("get_ppc2x()"); return NAN;}
  virtual float get_ppc2y      () const {warning("get_ppc2y()"); return NAN;}
  virtual float get_ppc2z      () const {warning("get_ppc2z()"); return NAN;}
  virtual float get_ptecx      () const {warning("get_ptecx()"); return NAN;}
  virtual float get_ptecy      () const {warning("get_ptecy()"); return NAN;}
  virtual float get_ptecz      () const {warning("get_ptecz()"); return NAN;}
  virtual float get_ppc3x      () const {warning("get_ppc3x()"); return NAN;}
  virtual float get_ppc3y      () const {warning("get_ppc3y()"); return NAN;}
  virtual float get_ppc3z      () const {warning("get_ppc3z()"); return NAN;}
  virtual float get_pemcx      () const {warning("get_pemcx()"); return NAN;}
  virtual float get_pemcy      () const {warning("get_pemcy()"); return NAN;}
  virtual float get_pemcz      () const {warning("get_pemcz()"); return NAN;}
  virtual float get_ptofx      () const {warning("get_ptofx()"); return NAN;}
  virtual float get_ptofy      () const {warning("get_ptofy()"); return NAN;}
  virtual float get_ptofz      () const {warning("get_ptofz()"); return NAN;}
  virtual float get_phbdx      () const {warning("get_phbdx()"); return NAN;}
  virtual float get_phbdy      () const {warning("get_phbdy()"); return NAN;}
  virtual float get_phbdz      () const {warning("get_phbdz()"); return NAN;}
  virtual float get_pmrpcx     () const {warning("get_pmrpcx()"); return NAN;}
  virtual float get_pmrpcy     () const {warning("get_pmrpcy()"); return NAN;}
  virtual float get_pmrpcz     () const {warning("get_pmrpcz()"); return NAN;}
  virtual float get_ptofwx     () const {warning("get_ptofwx()"); return NAN;}
  virtual float get_ptofwy     () const {warning("get_ptofwy()"); return NAN;}
  virtual float get_ptofwz     () const {warning("get_ptofwz()"); return NAN;}
  virtual float get_pltof      () const {warning("get_pltof()"); return NAN;}
  virtual float get_plemc      () const {warning("get_plemc()"); return NAN;}
  virtual float get_plmrpc     () const {warning("get_plmrpc()"); return NAN;}
  virtual float get_pltofw     () const {warning("get_pltofw()"); return NAN;}
  virtual float get_pc2dphi    () const {warning("get_pc2dphi()"); return NAN;}
  virtual float get_pc2dz      () const {warning("get_pc2dz()"); return NAN;}
  virtual float get_pc3dphi    () const {warning("get_pc3dphi()"); return NAN;}
  virtual float get_pc3dz      () const {warning("get_pc3dz()"); return NAN;}
  virtual float get_emcdphi    () const {warning("get_emcdphi()"); return NAN;}
  virtual float get_emcdz      () const {warning("get_emcdz()"); return NAN;}
  virtual float get_tofdphi    () const {warning("get_tofdphi()"); return NAN;}
  virtual float get_tofdz      () const {warning("get_tofdz()"); return NAN;}
  virtual float get_tecdphi    () const {warning("get_tecdphi()"); return NAN;}
  virtual float get_tecdalpha  () const {warning("get_tecdalpha()"); return NAN;}
  virtual float get_mrpcdphi   () const {warning("get_mrpcdphi()"); return NAN;}
  virtual float get_mrpcdz     () const {warning("get_mrpcdz()"); return NAN;}
  virtual float get_tofwdphi   () const {warning("get_tofwdphi()"); return NAN;}
  virtual float get_tofwdz     () const {warning("get_tofwdz()"); return NAN;}
  virtual float get_spc2dphi   () const {warning("get_spc2dphi()"); return NAN;}
  virtual float get_spc2dz     () const {warning("get_spc2dz()"); return NAN;}
  virtual float get_spc3dphi   () const {warning("get_spc3dphi()"); return NAN;}
  virtual float get_spc3dz     () const {warning("get_spc3dz()"); return NAN;}
  virtual float get_semcdphi   () const {warning("get_semcdphi()"); return NAN;}
  virtual float get_semcdz     () const {warning("get_semcdz()"); return NAN;}
  virtual float get_stofdphi   () const {warning("get_stofdphi()"); return NAN;}
  virtual float get_stofdz     () const {warning("get_stofdz()"); return NAN;}
  virtual float get_stofwdphi  () const {warning("get_stofwdphi()"); return NAN;}
  virtual float get_stofwdz    () const {warning("get_stofwdz()"); return NAN;}
  virtual float get_stecdphi   () const {warning("get_stecdphi()"); return NAN;}
  virtual float get_stecdalpha () const {warning("get_stecdalpha()"); return NAN;}
  virtual short get_hbdsector  () const {warning("get_hbdsector()"); return -9999;}
  virtual short get_hbdsize    () const {warning("get_hbdsize()"); return -9999;}
  virtual float get_hbdcharge  () const {warning("get_hbdcharge()"); return NAN;}
  virtual float get_hbdx       () const {warning("get_hbdx()"); return NAN;}
  virtual float get_hbdy       () const {warning("get_hbdy()"); return NAN;}
  virtual float get_hbdz       () const {warning("get_hbdz()"); return NAN;}
  virtual float get_hbddphi    () const {warning("get_hbddphi()"); return NAN;}
  virtual float get_hbddz      () const {warning("get_hbddz()"); return NAN;}
  virtual float get_hbdsdphi   () const {warning("get_hbdsdphi()"); return NAN;}
  virtual float get_hbdsdz     () const {warning("get_hbdsdz()"); return NAN;}
  virtual short get_arm        () const {warning("get_arm()"); return -9998;}
  virtual short get_sect       () const {warning("get_sect()"); return -9998;}
  virtual short get_ysect      () const {warning("get_ysect()"); return -9998;}
  virtual short get_zsect      () const {warning("get_zsect()"); return -9998;}
  virtual float get_ecorr      () const {warning("get_ecorr()"); return NAN;}
  virtual float get_ecore      () const {warning("get_ecore()"); return NAN;}
  virtual float get_dep        () const {warning("get_dep()"); return NAN;}
  virtual float get_emce       () const {warning("get_emce()"); return NAN;}
  virtual float get_emcdispy   () const {warning("get_emcdispy()"); return NAN;}
  virtual float get_emcdispz   () const {warning("get_emcdispz()"); return NAN;}
  virtual float get_temc       () const {warning("get_temc()"); return NAN;}
  virtual float get_prob       () const {warning("get_prob()"); return NAN;}
  virtual float get_ecent      () const {warning("get_ecent()"); return NAN;}
  virtual short get_twrhit     () const {warning("get_twrhit()"); return -9998;}
  virtual float get_e9         () const {warning("get_e9()"); return NAN;}
  virtual float get_re9        () const {warning("get_re9()"); return NAN;}
  virtual float get_emcchi2    () const {warning("get_emcchi2()"); return NAN;}
  virtual short get_sysect     () const {warning("get_sysect()"); return -9998;}
  virtual short get_szsect     () const {warning("get_szsect()"); return -9998;}
  virtual float get_secorr     () const {warning("get_secorr()"); return NAN;}
  virtual float get_secore     () const {warning("get_secore()"); return NAN;}
  virtual float get_semce      () const {warning("get_semce()"); return NAN;}
  virtual float get_semcdispy  () const {warning("get_semcdispy()"); return NAN;}
  virtual float get_semcdispz  () const {warning("get_semcdispz()"); return NAN;}
  virtual float get_stemc      () const {warning("get_stemc()"); return NAN;}
  virtual float get_sprob      () const {warning("get_sprob()"); return NAN;}
  virtual float get_secent     () const {warning("get_secent()"); return NAN;}
  virtual short get_stwrhit    () const {warning("get_stwrhit()"); return -9998;}
  virtual float get_se9        () const {warning("get_se9()"); return NAN;}
  virtual float get_sre9       () const {warning("get_sre9()"); return NAN;}
  virtual float get_semcchi2   () const {warning("get_semcchi2()"); return NAN;}
  virtual int   get_slat       () const {warning("get_slat()"); return -9998;}
  virtual float get_ttof       () const {warning("get_ttof()"); return NAN;}
  virtual float get_etof       () const {warning("get_etof()"); return NAN;}
  virtual float get_sttof      () const {warning("get_sttof()"); return NAN;}
  virtual float get_setof      () const {warning("get_setof()"); return NAN;}
  virtual int   get_slat_mrpc  () const {warning("get_slat_mrpc()"); return -9998;}
  virtual float get_ttof_mrpc  () const {warning("get_ttof_mrpc()"); return NAN;}
  virtual float get_ttofd_mrpc () const {warning("get_ttof_mrpc()"); return NAN;}
  virtual float get_qtof_mrpc  () const {warning("get_qtof_mrpc()"); return NAN;}
  virtual int   get_striptofw  () const {warning("get_striptofw()"); return -9998;}
  
  virtual float get_tofwx      () const {warning("get_tofwx()"); return NAN;}
  virtual float get_tofwy      () const {warning("get_tofwy()"); return NAN;}
  virtual float get_tofwz      () const {warning("get_tofwz()"); return NAN;}
  
  virtual float get_ttofw      () const {warning("get_ttofw()"); return NAN;}
  virtual float get_qtofw      () const {warning("get_qtofw()"); return NAN;}
  virtual float get_tofwadcup  () const {warning("get_tofwadcup()"); return NAN;}
  virtual float get_tofwadcdw  () const {warning("get_tofwadcdw()"); return NAN;}
  virtual float get_tofwtdcup  () const {warning("get_tofwtdcup()"); return NAN;}
  virtual float get_tofwtdcdw  () const {warning("get_tofwtdcdw()"); return NAN;}
  virtual short get_acc        () const {warning("get_acc()"); return -9998;}
  virtual int   get_ring       () const {warning("get_ring()"); return -9998;}
  virtual short get_n0         () const {warning("get_n0()"); return -9998;}
  virtual float get_npe0       () const {warning("get_npe0()"); return NAN;}
  virtual short get_n1         () const {warning("get_n1()"); return -9998;}
  virtual float get_npe1       () const {warning("get_npe1()"); return NAN;}
  virtual float get_chi2       () const {warning("get_chi2()"); return NAN;}
  virtual float get_disp       () const {warning("get_disp()"); return NAN;}
  virtual float get_tcrk       () const {warning("get_tcrk()"); return NAN;}
  virtual float get_cross_phi  () const {warning("get_cross_phi()"); return NAN;}
  virtual float get_cross_z    () const {warning("get_cross_z()"); return NAN;}
  virtual float get_center_phi () const {warning("get_center_phi()"); return NAN;}
  virtual float get_center_z   () const {warning("get_center_z()"); return NAN;}
  virtual short get_sacc       () const {warning("get_sacc()"); return -9998;}
  //virtual int   get_sring      () const {warning("get_sring()"); return -9998;}
  virtual int   get_sring      () const {warning("get_sring()"); return -98;}
  virtual short get_sn0        () const {warning("get_sn0()"); return -9998;}
  virtual float get_snpe0      () const {warning("get_snpe0()"); return NAN;}
  virtual short get_sn1        () const {warning("get_sn1()"); return -9998;}
  virtual float get_snpe1      () const {warning("get_snpe1()"); return NAN;}
  virtual float get_schi2      () const {warning("get_schi2()"); return NAN;}
  virtual float get_sdisp      () const {warning("get_sdisp()"); return NAN;}
  virtual float get_stcrk      () const {warning("get_stcrk()"); return NAN;}
  virtual float get_scross_phi () const {warning("get_scross_phi()"); return NAN;}
  virtual float get_scross_z   () const {warning("get_scross_z()"); return NAN;}
  virtual float get_scenter_phi() const {warning("get_scenter_phi()"); return NAN;}
  virtual float get_scenter_z  () const {warning("get_scenter_z()"); return NAN;}
  virtual float get_tecdedx1   () const {warning("get_tecdedx1()"); return NAN;}
  virtual float get_tecdedx2   () const {warning("get_tecdedx2()"); return NAN;}
  virtual float get_pc2sdphi   () const {warning("get_pc2sdphi()"); return NAN;}
  virtual float get_pc2sdz     () const {warning("get_pc2sdz()"); return NAN;}
  virtual float get_pc3sdphi   () const {warning("get_pc3sdphi()"); return NAN;}
  virtual float get_pc3sdz     () const {warning("get_pc3sdz()"); return NAN;}
  virtual float get_emcsdphi   () const {warning("get_emcsdphi()"); return NAN;}
  virtual float get_emcsdz     () const {warning("get_emcsdz()"); return NAN;}
  virtual float get_tofsdphi   () const {warning("get_tofsdphi()"); return NAN;}
  virtual float get_tofsdz     () const {warning("get_tofsdz()"); return NAN;}
  virtual float get_tofwsdphi  () const {warning("get_tofwsdphi()"); return NAN;}
  virtual float get_tofwsdz    () const {warning("get_tofwsdz()"); return NAN;}
  virtual float get_tecsdphi   () const {warning("get_tecsdphi()"); return NAN;}
  virtual float get_tecsdalpha () const {warning("get_tecsdalpha()"); return NAN;}
  virtual float get_spc2sdphi  () const {warning("get_spc2sdphi()"); return NAN;}
  virtual float get_spc2sdz    () const {warning("get_spc2sdz()"); return NAN;}
  virtual float get_spc3sdphi  () const {warning("get_spc3sdphi()"); return NAN;}
  virtual float get_spc3sdz    () const {warning("get_spc3sdz()"); return NAN;}
  virtual float get_semcsdphi  () const {warning("get_semcsdphi()"); return NAN;}
  virtual float get_semcsdz    () const {warning("get_semcsdz()"); return NAN;}
  virtual float get_stofsdphi  () const {warning("get_stofsdphi()"); return NAN;}
  virtual float get_stofsdz    () const {warning("get_stofsdz()"); return NAN;}
  virtual float get_stofwsdphi () const {warning("get_stofwsdphi()"); return NAN;}
  virtual float get_stofwsdz   () const {warning("get_stofwsdz()"); return NAN;}
  virtual float get_stecsdphi  () const {warning("get_stecsdphi()"); return NAN;}
  virtual float get_stecsdalpha() const {warning("get_stecsdalpha()"); return NAN;}
  virtual float get_m2tof      () const {warning("get_m2tof()"); return NAN;}
  virtual float get_m2tofw     () const {warning("get_m2tofw()"); return NAN;}
  virtual float get_m2emc      () const {warning("get_m2emc()"); return NAN;}
  virtual float get_isPi       () const {warning("get_isPi()"); return NAN;}
  virtual float get_isK        () const {warning("get_isK()"); return NAN;}
  virtual float get_isP        () const {warning("get_isP()"); return NAN;}
  virtual float get_isPiTofw   () const {warning("get_isPiTofw()"); return NAN;}
  virtual float get_isKTofw    () const {warning("get_isKTofw()"); return NAN;}
  virtual float get_isPTofw    () const {warning("get_isPTofw()"); return NAN;}
  virtual long get_categoryl2eLowPt() const {warning("get_categoryl2eLowPt()"); return -9998;}
  virtual long get_categoryl2eHighPt() const {warning("get_categoryl2eHightPt()"); return -9998;}
  virtual short get_candIDl2e  () const {warning("get_candIDl2e()"); return -9998;}
  virtual int   get_nlvl2MatchLowOcupy() const {warning("get_nlvl2MatchLowOcupy()"); return -9998; }
  virtual int get_RawL1() const {warning("get_RawL1()"); return -9998; }
  virtual int get_LivL1() const {warning("get_LivL1()"); return -9998; }
  virtual int get_SclL1() const {warning("get_SclL1()"); return -9998; }

  virtual short get_dcarm      () const {warning("get_dcarm()"); return -9998;}
  virtual short get_dcside     () const {warning("get_dcside()"); return -9998;}
  virtual short get_pc1sect    () const {warning("get_pc1sect()"); return -9998;}
  virtual short get_pc2sect    () const {warning("get_pc2sect()"); return -9998;}
  virtual short get_pc3sect    () const {warning("get_pc3sect()"); return -9998;}
  virtual float get_pc1phi     () const {warning("get_pc1phi()"); return NAN;}
  virtual float get_pc1z       () const {warning("get_pc1z()"); return NAN;}
  virtual float get_pc2phi     () const {warning("get_pc2phi()"); return NAN;}
  virtual float get_pc2z       () const {warning("get_pc2z()"); return NAN;}
  virtual float get_pc3phi     () const {warning("get_pc3phi()"); return NAN;}
  virtual float get_pc3z       () const {warning("get_pc3z()"); return NAN;}
  virtual float get_tofphi     () const {warning("get_tofphi()"); return NAN;}
  virtual float get_tofz       () const {warning("get_tofz()"); return NAN;}
  virtual float get_tecphi     () const {warning("get_tecphi()"); return NAN;}
  virtual float get_tecalpha   () const {warning("get_tecalpha()"); return NAN;}
  virtual float get_emcphi     () const {warning("get_emcphi()"); return NAN;}
  virtual float get_emcz       () const {warning("get_emcz()"); return NAN;}
  virtual float get_spc1phi    () const {warning("get_spc1phi()"); return NAN;}
  virtual float get_spc1z      () const {warning("get_spc1z()"); return NAN;}
  virtual float get_spc2phi    () const {warning("get_spc2phi()"); return NAN;}
  virtual float get_spc2z      () const {warning("get_spc2z()"); return NAN;}
  virtual float get_spc3phi    () const {warning("get_spc3phi()"); return NAN;}
  virtual float get_spc3z      () const {warning("get_spc3z()"); return NAN;}
  virtual float get_stofphi    () const {warning("get_stofphi()"); return NAN;}
  virtual float get_stofz      () const {warning("get_stofz()"); return NAN;}
  virtual float get_stecphi    () const {warning("get_stecphi()"); return NAN;}
  virtual float get_stecalpha  () const {warning("get_stecalpha()"); return NAN;}
  virtual float get_semcphi    () const {warning("get_emcphi()"); return NAN;}
  virtual float get_semcz      () const {warning("get_emcz()"); return NAN;}
  virtual float get_emcsdphi_e () const {warning("get_emcsdphi_e()"); return NAN;}
  virtual float get_emcsdz_e   () const {warning("get_emcsdz_e()"); return NAN;}
  virtual float get_semcsdphi_e() const {warning("get_semcsdphi_e()"); return NAN;}
  virtual float get_semcsdz_e  () const {warning("get_semcsdz_e()"); return NAN;}
  virtual short get_tecnhit    () const {warning("get_tecnhit()"); return -9998;}
  virtual short get_n2	       () const {warning("get_n2()"); return -9998;}
  virtual float get_npe2       () const {warning("get_npe2()"); return NAN;}
  virtual short get_n3	       () const {warning("get_n3()"); return -9998;}
  virtual float get_npe3       () const {warning("get_npe3()"); return NAN;}
  virtual short get_sn2	       () const {warning("get_sn2()"); return -9998;}
  virtual float get_snpe2      () const {warning("get_snpe2()"); return NAN;}
  virtual short get_sn3	       () const {warning("get_sn3()"); return -9998;}
  virtual float get_snpe3      () const {warning("get_snpe3()"); return NAN;}
  virtual int   get_deadmap    () const {warning("get_deadmap()"); return -9998;}
  virtual int   get_warnmap    () const {warning("get_warnmap()"); return -9998;}
  virtual int   get_sdeadmap   () const {warning("get_sdeadmap()"); return -9998;}
  virtual int   get_swarnmap   () const {warning("get_swarnmap()"); return -9998;}
  virtual short get_tofecut    () const {warning("get_tofecut()"); return -9998;}
  virtual short get_tofsame    () const {warning("get_tofsame()"); return -9998;}
  virtual short get_slatnext   () const {warning("get_slatnext()"); return -9998;}
  virtual float get_ttofnext   () const {warning("get_etofnext()"); return NAN;}
  virtual float get_etofnext   () const {warning("get_etofnext()"); return NAN;}
  virtual short get_tzrid      () const {warning("get_tzrid()"); return -9998;}
  virtual short get_pcrid      () const {warning("get_pcrid()"); return -9998;}
  virtual float get_ptzrx      () const {warning("get_ptzrx()"); return NAN;}
  virtual float get_ptzry      () const {warning("get_ptzry()"); return NAN;}
  virtual float get_ptzrz      () const {warning("get_ptzrz()"); return NAN;}
  virtual float get_ppcrx      () const {warning("get_ppcrx()"); return NAN;}
  virtual float get_ppcry      () const {warning("get_ppcry()"); return NAN;}
  virtual float get_ppcrz      () const {warning("get_ppcrz()"); return NAN;}
  virtual float get_tzrtof     () const {warning("get_tzrtof()"); return NAN;}
  virtual short get_tzrslat    () const {warning("get_tzrslat()"); return -9998;}
  virtual float get_tzreloss   () const {warning("get_tzreloss()"); return NAN;}
  virtual float get_tzrx       () const {warning("get_tzrx()"); return NAN;}
  virtual float get_tzry       () const {warning("get_tzry()"); return NAN;}
  virtual float get_tzrz       () const {warning("get_tzrz()"); return NAN;}
  virtual float get_pcrtof     () const {warning("get_pcrtof()"); return NAN;}
  virtual short get_pcrslat    () const {warning("get_pcrslat()"); return -9998;}
  virtual float get_pcreloss   () const {warning("get_pcreloss()"); return NAN;}
  virtual float get_pcrx       () const {warning("get_pcrx()"); return NAN;}
  virtual float get_pcry       () const {warning("get_pcry()"); return NAN;}
  virtual float get_pcrz       () const {warning("get_pcrz()"); return NAN;}
  virtual float get_tzrsdphi   () const {warning("get_tzrsdphi()"); return NAN;}
  virtual float get_tzrsdz     () const {warning("get_tzrsdz()");return NAN;}
  virtual float get_m2tzr      () const {warning("get_m2tzr()");return NAN;}
  virtual float get_m2ntctof   () const {warning("get_m2ntctof()"); return NAN;}
  virtual float get_pltzr      () const {warning("get_pltzr()");return NAN;}
  virtual float get_isPitzr    () const {warning("get_isPitzr()");return NAN;}
  virtual float get_isKtzr     () const {warning("get_isKtzr()");return NAN;}
  virtual float get_isPtzr     () const {warning("get_isPtzr()");return NAN;}
  virtual float get_isPintctof () const {warning("get_isPintctof()");return NAN;}
  virtual float get_isKntctof  () const {warning("get_isKntctof()");return NAN;}
  virtual float get_isPntctof  () const {warning("get_isPntctof()");return NAN;}
  virtual short get_tzrecut    () const {warning("get_tzrecut()");return -9998;}
  virtual int get_L1Trig() const {warning("get_L1Trig()");return -9998;}
  virtual float get_pc1wid() const {warning("get_pc1wid()"); return NAN;}
  virtual float get_pc2wid() const {warning("get_pc2wid()"); return NAN;}
  virtual float get_pc3wid() const {warning("get_pc3wid()"); return NAN;}
  virtual float get_tofph1     () const {warning("get_tofph1()"); return NAN;}
  virtual float get_tofph2     () const {warning("get_tofph2()"); return NAN;}
  virtual float get_toftdc1    () const {warning("get_toftdc1()"); return NAN;}
  virtual float get_toftdc2    () const {warning("get_toftdc2()"); return NAN;}
  virtual int get_aerindex     () const {warning("get_aerindex()"); return -9998;}
  virtual int get_aersindex    () const {warning("get_aersindex()"); return -9998;}
  virtual float get_aerph1     () const {warning("get_aerph1()"); return NAN;}
  virtual float get_aerph2     () const {warning("get_aerph2()"); return NAN;}
  virtual float get_aert1      () const {warning("get_aert1()"); return NAN;}
  virtual float get_aert2      () const {warning("get_aert2()"); return NAN;}
  virtual float get_aernpe     () const {warning("get_aernpe()"); return NAN;}
  virtual short get_aerstatus  () const {warning("get_aerstatus()"); return -9998;}
  virtual float get_aerph1_0     () const {warning("get_aerph1_0()"); return NAN;}
  virtual float get_aerph2_0    () const {warning("get_aerph2_0()"); return NAN;}
  virtual float get_aert1_0      () const {warning("get_aert1_0()"); return NAN;}
  virtual float get_aert2_0      () const {warning("get_aert2_0()"); return NAN;}
  virtual float get_aerph1_1     () const {warning("get_aerph1_1()"); return NAN;}
  virtual float get_aerph2_1    () const {warning("get_aerph2_1()"); return NAN;}
  virtual float get_aert1_1      () const {warning("get_aert1_1()"); return NAN;}
  virtual float get_aert2_1      () const {warning("get_aert2_1()"); return NAN;}
  virtual float get_aerph1_2     () const {warning("get_aerph1_2()"); return NAN;}
  virtual float get_aerph2_2    () const {warning("get_aerph2_2()"); return NAN;}
  virtual float get_aert1_2      () const {warning("get_aert1_2()"); return NAN;}
  virtual float get_aert2_2      () const {warning("get_aert2_2()"); return NAN;}
  virtual float get_aerph1_3     () const {warning("get_aerph1_3()"); return NAN;}
  virtual float get_aerph2_3    () const {warning("get_aerph2_3()"); return NAN;}
  virtual float get_aert1_3      () const {warning("get_aert1_3()"); return NAN;}
  virtual float get_aert2_3      () const {warning("get_aert2_3()"); return NAN;}
  virtual int get_aerhitid      () const {warning("get_aerhitid()"); return -9998;}
  virtual int get_aerhitconfig  () const {warning("get_aerhitconfig()"); return -9998;}
  virtual float get_aersph1_0     () const {warning("get_aersph1_0()"); return NAN;}
  virtual float get_aersph2_0    () const {warning("get_aersph2_0()"); return NAN;}
  virtual float get_aerst1_0      () const {warning("get_aerst1_0()"); return NAN;}
  virtual float get_aerst2_0      () const {warning("get_aerst2_0()"); return NAN;}
  virtual float get_aersph1_1     () const {warning("get_aersph1_1()"); return NAN;}
  virtual float get_aersph2_1    () const {warning("get_aersph2_1()"); return NAN;}
  virtual float get_aerst1_1      () const {warning("get_aerst1_1()"); return NAN;}
  virtual float get_aerst2_1      () const {warning("get_aerst2_1()"); return NAN;}
  virtual float get_aersph1_2     () const {warning("get_aersph1_2()"); return NAN;}
  virtual float get_aersph2_2    () const {warning("get_aersph2_2()"); return NAN;}
  virtual float get_aerst1_2      () const {warning("get_aerst1_2()"); return NAN;}
  virtual float get_aerst2_2      () const {warning("get_aerst2_2()"); return NAN;}
  virtual float get_aersph1_3     () const {warning("get_aersph1_3()"); return NAN;}
  virtual float get_aersph2_3    () const {warning("get_aersph2_3()"); return NAN;}
  virtual float get_aerst1_3      () const {warning("get_aerst1_3()"); return NAN;}
  virtual float get_aerst2_3      () const {warning("get_aerst2_3()"); return NAN;}
  virtual int get_aershitid      () const {warning("get_aershitid()"); return -9998;}
  virtual int get_aershitconfig  () const {warning("get_aershitconfig()"); return -9998;}
  virtual float get_tecde      () const {warning("get_tecde()"); return NAN;}
  virtual float get_tecde06    () const {warning("get_tecde06()"); return NAN;}
  virtual float get_tectrklen  () const {warning("get_tectrklen()"); return NAN;}
  virtual short get_tecnde     () const {warning("get_tecnde()"); return -9998;}
  virtual short get_tecnhit100 () const {warning("get_tecnhit100()"); return -9998;}
  virtual short get_tecnhit200 () const {warning("get_tecnhit200()"); return -9998;}
  virtual short get_tecnhit50  () const {warning("get_tecnhit50()"); return -9998;}
  virtual float get_tecwtb     () const {warning("get_tecwtb()"); return NAN;}
  virtual float get_tecwtbsq   () const {warning("get_tecwtbsq()"); return NAN;}
  virtual int   get_mcid       () const {warning("get_mcid()"); return -9998;}
  virtual int   get_dchid      () const {warning("get_dchid()"); return -9998;}
  virtual int   get_emcrawtdc  () const {warning("get_emcrawtdc()"); return -9998;}
  virtual int   get_emcrawadc  () const {warning("get_emcrawadc()"); return -9998;}
  virtual int   get_emcrawadclg() const {warning("get_emcrawadclg()"); return -9998;}
  virtual float get_Px() const {warning("get_Px()"); return NAN;}
  virtual float get_Py() const {warning("get_Py()"); return NAN;}
  virtual float get_Pz() const {warning("get_Pz()"); return NAN;}
  virtual int   get_idtrk_tof() const {warning("get_idtrk_tof()"); return -9998;}
  virtual int   get_idtrk_tofw() const {warning("get_idtrk_tofw()"); return -9998;}
  virtual int   get_idtrk_tec() const {warning("get_idtrk_tec()"); return -9998;}
  virtual int   get_idtrk_crk() const {warning("get_idtrk_crk()"); return -9998;}
  virtual int   get_idtrk_pc2() const {warning("get_idtrk_pc2()"); return -9998;}
  virtual int   get_idtrk_pc3() const {warning("get_idtrk_pc3()"); return -9998;}
  virtual int   get_idtrk_hbd() const {warning("get_idtrk_hbd()"); return -9998;}
  virtual int   get_idtrk_stof() const {warning("get_idtrk_stof()"); return -9998;}
  virtual int   get_idtrk_stec() const {warning("get_idtrk_stec()"); return -9998;}
  virtual int   get_idtrk_scrk() const {warning("get_idtrk_scrk()"); return -9998;}
  virtual int   get_idtrk_spc2() const {warning("get_idtrk_spc2()"); return -9998;}
  virtual int   get_idtrk_spc3() const {warning("get_idtrk_spc3()"); return -9998;}

  virtual float get_hbdscharge     () const {warning("get_hbdscharge");   return -9998.;}
  virtual float get_hbdlocalmax  () const {warning("get_hbdlocalmax");return -9998.;}

  virtual float get_hbdhubcharge     () const {warning("get_hbdhubcharge");   return -9998.;}
  virtual float get_hbdspokecharge1  () const {warning("get_hbdspokecharge1");return -9998.;}
  virtual float get_hbdspokecharge2  () const {warning("get_hbdspokecharge2");return -9998.;}

  virtual float get_pc1dphi    () const  {warning("get_pc1dphi()"); return -9998;}
  virtual float get_pc1dz      () const  {warning("get_pc1dz()"); return -9998;}
  virtual int get_sdchid     () const  {warning("get_sdchid()"); return -9998;}
  virtual short get_spc1id     () const  {warning("get_spc1id()"); return -9998;}
  virtual short get_stofwid    () const  {warning("get_stofwid()"); return -9998;}
  virtual float get_spemcx     () const  {warning("get_spemcx()"); return -9998;}
  virtual float get_spemcy     () const  {warning("get_spemcy()"); return -9998;}
  virtual float get_spemcz     () const  {warning("get_spemcz()"); return -9998;}
  virtual float get_sppc1x     () const  {warning("get_sppc1x()"); return -9998;}
  virtual float get_sppc1y     () const  {warning("get_sppc1y()"); return -9998;}
  virtual float get_sppc1z     () const  {warning("get_sppc1z()"); return -9998;}
  virtual float get_sppc2x     () const  {warning("get_sppc2x()"); return -9998;}
  virtual float get_sppc2y     () const  {warning("get_sppc2y()"); return -9998;}
  virtual float get_sppc2z     () const  {warning("get_sppc2z()"); return -9998;}
  virtual float get_sppc3x     () const  {warning("get_sppc3x()"); return -9998;}
  virtual float get_sppc3y     () const  {warning("get_sppc3y()"); return -9998;}
  virtual float get_sppc3z     () const  {warning("get_sppc3z()"); return -9998;}
  virtual float get_sptofex    () const  {warning("get_sptofex()"); return -9998;}
  virtual float get_sptofey    () const  {warning("get_sptofey()"); return -9998;}
  virtual float get_sptofez    () const  {warning("get_sptofez()"); return -9998;}
  virtual float get_sptofwx    () const  {warning("get_sptofwx()"); return -9998;}
  virtual float get_sptofwy    () const  {warning("get_sptofwy()"); return -9998;}
  virtual float get_sptofwz    () const  {warning("get_sptofwz()"); return -9998;}
  virtual float get_spc1dphi   () const  {warning("get_spc1dphi()"); return -9998;}
  virtual float get_spc1dz     () const  {warning("get_spc1dz()"); return -9998;}
  virtual float get_ptofex    () const  {warning("get_ptofex()"); return -9998;}
  virtual float get_ptofey    () const  {warning("get_ptofey()"); return -9998;}
  virtual float get_ptofez    () const  {warning("get_ptofez()"); return -9998;}
  virtual float get_pltofe    () const  {warning("get_pltofe()"); return -9998;}

  virtual float get_teccharge      (const unsigned int /*iplane*/) const  {warning("get_teccharge()"); return -9998;}
  virtual int   get_tecntimebins   (const unsigned int /*iplane*/) const  {warning("get_tecntimebins"); return -9998;}
  virtual int   get_tecavgtimebin  (const unsigned int /*iplane*/) const  {warning("get_tecavgtimebin"); return -9998;}
  virtual float get_tecdphiplane   (const unsigned int /*iplane*/) const  {warning("get_tecdphiplane"); return -9998;}
  virtual float get_tecMomentum    () const  {warning("get_tecMomentum()"); return -9998;}
  virtual float get_tectrlike      () const  {warning("get_tectrlike()"); return -9998;}

  virtual float get_steccharge      (const unsigned int /*iplane*/) const  {warning("get_steccharge()"); return -9998;}
  virtual int   get_stecntimebins   (const unsigned int /*iplane*/) const  {warning("get_stecntimebins"); return -9998;}
  virtual int   get_stecavgtimebin  (const unsigned int /*iplane*/) const  {warning("get_stecavgtimebin"); return -9998;}
  virtual float get_stecdphiplane   (const unsigned int /*iplane*/) const  {warning("get_stecdphiplane"); return -9998;}
  virtual float get_stecMomentum    () const  {warning("get_stecMomentum()"); return -9998;}
  virtual float get_stectrlike      () const  {warning("get_stectrlike()"); return -9998;}

  // Silicon Vertex (SVX) stuff
  virtual float get_svxdphi            (const unsigned int /*ilayer*/) const {warning("get_svxdphi"); return -9998;};
  virtual float get_svxdz              (const unsigned int /*ilayer*/) const {warning("get_svxdz"); return -9998;};
  virtual int get_svxid                (const unsigned int /*ilayer*/) const {warning("get_svxid"); return -9998;};
  virtual float get_svxsdphi           (const unsigned int /*ilayer*/) const {warning("get_svxsdphi"); return -9998;};
  virtual float get_svxsdz             (const unsigned int /*ilayer*/) const {warning("get_svxsdz"); return -9998;};
  virtual int get_svxsid               (const unsigned int /*ilayer*/) const {warning("get_svxsid"); return -9998;};
  virtual float get_psvxx              (const unsigned int /*ilayer*/) const {warning("get_psvxx"); return -9998;};
  virtual float get_psvxy              (const unsigned int /*ilayer*/) const {warning("get_psvxy"); return -9998;};
  virtual float get_psvxz              (const unsigned int /*ilayer*/) const {warning("get_psvxz"); return -9998;};
  virtual float get_spsvxx             (const unsigned int /*ilayer*/) const {warning("get_spsvxx"); return -9998;};
  virtual float get_spsvxy             (const unsigned int /*ilayer*/) const {warning("get_spsvxy"); return -9998;};
  virtual float get_spsvxz             (const unsigned int /*ilayer*/) const {warning("get_spsvxz"); return -9998;};
  // entries above are for svx clusters, entries below ate for svx standalone tracks
  virtual int get_svxtrackid           () const {warning("get_svxtrackid"); return -9998;};
  virtual float get_svxtrackquality           () const {warning("get_svxtrackquality"); return -9998;};  // standalone
  virtual float get_svxdca2d           () const {warning("get_svxdca2d"); return -9998;};  // standalone
  virtual float get_svxdca3d           () const {warning("get_svxdca3d"); return -9998;};
  virtual float get_svxdca2dkf         () const {warning("get_svxdca2d"); return -9998;};  // KalFit
  virtual float get_svxdca3dkf         () const {warning("get_svxdca3d"); return -9998;};
  // The entries below represent a point of closest approach, and
  // track momentum components ar this point.
  // They are needed for secondary vertex determination for a pair of tracks
  // They come in two flavors: one determined by svx standalone tracking (0)
  // and the second one determined by KalFit (kf)
  virtual float get_svxxyz0            (const unsigned int /*icoor*/) const {warning("get_svxxyz0"); return -9998;};
  virtual float get_svxpxyz0           (const unsigned int /*icoor*/) const {warning("get_svxpxyz0"); return -9998;};
  virtual float get_svxxyzkf           (const unsigned int /*icoor*/) const {warning("get_svxxyzkf"); return -9998;};
  virtual float get_svxpxyzkf          (const unsigned int /*icoor*/) const {warning("get_svxpxyzkf"); return -9998;};


  virtual int isValid(const float f) const;
  virtual int isValid(const int i) const;
  virtual int isImplemented(const float f) const;
  virtual int isImplemented(const int i) const;

 private:
  void warning(const char* field) const;

  ClassDef(PHSnglCentralTrack,1)
};
#endif /* __PHCENTRALTRACK_HH_ */
