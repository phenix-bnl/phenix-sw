#include "PHSnglCentralTrackv24.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv24)

PHSnglCentralTrackv24::PHSnglCentralTrackv24() :
  // Instead of calling PHSnglCentralTrack::Init here simply create and
  // initialize our variables in one go. Init got pretty expensive, probably
  // because of virtual function calls. Doing everything here makes this very
  // fast.
  charge(-9999),
  quality(-9999),
  zed(-9999),
  phi(-9999),
  alpha(-9999),
  beta(-9999),
  phi0(-9999),
  the0(-9999),
  mom(-9999),
  status(-9999),
  alpha1(-9999),
  alpha2(-9999),
  nx1hits(-9999),
  nx2hits(-9999),
  ppc1x(-9999),
  ppc1y(-9999),
  ppc1z(-9999),
  sppc1x(-9999),
  sppc1y(-9999),
  sppc1z(-9999),
  ppc2x(-9999),
  ppc2y(-9999),
  ppc2z(-9999),
  sppc2x(-9999),
  sppc2y(-9999),
  sppc2z(-9999),
  ptecx(-9999),
  ptecy(-9999),
  ptecz(-9999),
  ppc3x(-9999),
  ppc3y(-9999),
  ppc3z(-9999),
  sppc3x(-9999),
  sppc3y(-9999),
  sppc3z(-9999),
  pemcx(-9999),
  pemcy(-9999),
  pemcz(-9999),
  spemcx(-9999),
  spemcy(-9999),
  spemcz(-9999),
  ptofex(-9999),
  ptofey(-9999),
  ptofez(-9999),
  sptofex(-9999),
  sptofey(-9999),
  sptofez(-9999),
  ptofwx(-9999),
  ptofwy(-9999),
  ptofwz(-9999),
  sptofwx(-9999),
  sptofwy(-9999),
  sptofwz(-9999),
  pltofe(-9999),
  pltofw(-9999),
  plemc(-9999),
  sect(-9999),
  ysect(-9999),
  zsect(-9999),
  ecore(-9999),
  emce(-9999),
  emcdispy(-9999),
  emcdispz(-9999),
  temc(-9999),
  prob(-9999),
  ecent(-9999),
  twrhit(-9999),
  e9(-9999),
  emcchi2(-9999),
  secore(-9999),
  semce(-9999),
  semcdispy(-9999),
  semcdispz(-9999),
  stemc(-9999),
  sprob(-9999),
  stwrhit(-9999),
  semcchi2(-9999),
  slat(-9999),
  striptofw(-9999),
  ttofe(-9999),
  etofe(-9999),
  sttofe(-9999),
  setofe(-9999),
  ttofw(-9999),
  etofw(-9999),
  n0(-9999),
  npe0(-9999),
  n1(-9999),
  npe1(-9999),
  chi2(-9999),
  disp(-9999),
  tcrk(-9999),
  cross_phi(-9999),
  cross_z(-9999),
  center_phi(-9999),
  center_z(-9999),
  sn0(-9999),
  snpe0(-9999),
  sn1(-9999),
  snpe1(-9999),
  schi2(-9999),
  sdisp(-9999),
  stcrk(-9999),
  pc2sdphi(-9999),
  pc2sdz(-9999),
  pc3sdphi(-9999),
  pc3sdz(-9999),
  emcsdphi(-9999),
  emcsdz(-9999),
  tofesdphi(-9999),
  tofesdz(-9999),
  tofwsdphi(-9999),
  tofwsdz(-9999),
  tecsdphi(-9999),
  tecsdalpha(-9999),
  spc2sdphi(-9999),
  spc2sdz(-9999),
  spc3sdphi(-9999),
  spc3sdz(-9999),
  semcsdphi(-9999),
  semcsdz(-9999),
  stofesdphi(-9999),
  stofesdz(-9999),
  stofwsdphi(-9999),
  stofwsdz(-9999),
  stecsdphi(-9999),
  stecsdalpha(-9999),
  m2tofe(-9999),
  m2tofw(-9999),
  m2emc(-9999),
  isPi(-9999),
  isK(-9999),
  isP(-9999),
  isPiTofw(-9999),
  isKTofw(-9999),
  isPTofw(-9999),
  dcarm(-9999),
  dcside(-9999),
  pc1sect(-9999),
  pc2sect(-9999),
  pc3sect(-9999),
  emcsdphi_e(-9999),
  emcsdz_e(-9999),
  semcsdphi_e(-9999),
  semcsdz_e(-9999),
  mx1dist(-9999),
  mx2dist(-9999),
  n2(-9999),
  npe2(-9999),
  n3(-9999),
  npe3(-9999),
  sn2(-9999),
  snpe2(-9999),
  sn3(-9999),
  snpe3(-9999),
  deadmap(-9999),
  warnmap(-9999),
  sdeadmap(-9999),
  swarnmap(-9999),
  pc1id(-9999),
  pc2id(-9999),
  pc3id(-9999),
  spc1id(-9999),
  spc2id(-9999),
  spc3id(-9999),
  emcid(-9999),
  semcid(-9999),
  tofeid(-9999),
  stofeid(-9999),
  tofwid(-9999),
  stofwid(-9999),
  tecid(-9999),
  ring(-9999),
  sring(-9999),
  pc1dphi(-9999),
  pc1dz(-9999),
  pc2dphi(-9999),
  pc2dz(-9999),
  pc3dphi(-9999),
  pc3dz(-9999),
  emcdphi(-9999),
  emcdz(-9999),
  tofedphi(-9999),
  tofedz(-9999),
  tofwdphi(-9999),
  tofwdz(-9999),
  tecdphi(-9999),
  tecdalpha(-9999),
  tofeph1(-9999),
  tofeph2(-9999),
  tofetdc1(-9999),
  tofetdc2(-9999),
  tofwtdcup(-9999),
  tofwtdcdw(-9999),
  tofwadcup(-9999),
  tofwadcdw(-9999),
  scross_phi(-9999),
  scross_z(-9999),
  mcid(-9999),
  dchid(-9999),
  sdchid(-9999),
  emcrawtdc(-9999),
  emcrawadc(-9999),
  emcrawadclg(-9999),
  se9(-9999),
  secent(-9999),
  aerindex(-9999),
  aersindex(-9999),
  stecid(-9999),
  hbdid(-9999),
  hbdsector(-9999),
  hbdsize(-9999),
  hbdcharge(-9999),
  hbdx(-9999),
  hbdy(-9999),
  hbdz(-9999),
  phbdx(-9999),
  phbdy(-9999),
  phbdz(-9999),
  hbddphi(-9999),
  hbddz(-9999),
  hbdsdphi(-9999),
  hbdsdz(-9999),
  hbdhubcharge(-9999),
  hbdspokecharge1(-9999),
  hbdspokecharge2(-9999),
  hbdlocalmax(-9999),
  hbdscharge(-9999),
  spc1dphi(-9999),
  spc1dz(-9999),
  spc2dphi(-9999),
  spc2dz(-9999),
  spc3dphi(-9999),
  spc3dz(-9999),
  semcdphi(-9999),
  semcdz(-9999),
  stofedphi(-9999),
  stofedz(-9999),
  stofwdphi(-9999),
  stofwdz(-9999),
  tecMomentum(-9999),
  tectrlike(-9999),
  stecMomentum(-9999),
  stectrlike(-9999),
  svxtrackid(-9999),
  svxtrackquality(-9999),
  svxdca2d(-9999),
  svxdca3d(-9999),
  svxdca2dkf(-9999),
  svxdca3dkf(-9999),
  dep(-9999.)
{
  // Only array member variables left to be initialized

  for (unsigned i=0; i<6; ++i) {
    set_teccharge(i, -9999);
    set_tecntimebins(i, -9999);
    set_tecavgtimebin(i, -9999);
    set_tecdphiplane(i, -9999);
    set_steccharge(i, -9999);
    set_stecntimebins(i, -9999);
    set_stecavgtimebin(i, -9999);
    set_stecdphiplane(i, -9999);
  }

  for (unsigned i = 0; i<4; ++i) {
    set_svxdphi(i, -9999);
    set_svxdz(i, -9999);
    set_svxid(i, -9999);
    set_svxsdphi(i, -9999);
    set_svxsdz(i, -9999);
    set_svxsid(i, -9999);
    set_psvxx(i, -9999);
    set_psvxy(i, -9999);
    set_psvxz(i, -9999);
    set_spsvxx(i, -9999);
    set_spsvxy(i, -9999);
    set_spsvxz(i, -9999);
  }

  for (unsigned i = 0; i<3; ++i) {
    set_svxxyz0(i, -9999);
    set_svxpxyz0(i, -9999);
    set_svxxyzkf(i, -9999);
    set_svxpxyzkf(i, -9999);
  }
}

PHSnglCentralTrackv24::PHSnglCentralTrackv24(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);

  return;
}


void
PHSnglCentralTrackv24::identify(std::ostream &os) const
{
  os << "PHSnglTrack v24" << std::endl;
  os << "quality : " << get_quality() << std::endl;
  return;
}


