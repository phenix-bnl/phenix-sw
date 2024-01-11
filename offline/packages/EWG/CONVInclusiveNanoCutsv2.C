#include "CONVInclusiveNanoCutsv2.h"
#include "PHCentralTrack.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include "PHGlobal.h"

#include <vector>
//INCLUDECHECKER: Removed this line: #include <iostream>

ClassImp(CONVInclusiveNanoCutsv2)

using namespace std;

typedef PHIODataNode<PHGlobal> PHGlobalNode_t;

CONVInclusiveNanoCutsv2::CONVInclusiveNanoCutsv2() {
 Reset();
}

void CONVInclusiveNanoCutsv2::Reset() {
  ptlowcut      = BIGNEGATIVEFLOAT;
  pthighcut     = BIGPOSITIVEFLOAT;
  vertexcut     = BIGPOSITIVEFLOAT;
  pc3Phicut     = BIGPOSITIVEFLOAT;
  pc3Zcut       = BIGPOSITIVEFLOAT;
  emcPhicut     = BIGPOSITIVEFLOAT;
  emcZcut       = BIGPOSITIVEFLOAT;
  nx1cut        = BIGNEGATIVEINT;
  nx2cut        = BIGNEGATIVEINT;
  chi2overnpe0max = BIGNEGATIVEFLOAT;
  n0min         = BIGNEGATIVEINT;
  n1min         = BIGNEGATIVEINT;
  sn0min        = BIGNEGATIVEINT;
  qualitymin    = BIGNEGATIVEINT;
  eoverpmin     = BIGNEGATIVEFLOAT;
  eoverpmax     = BIGPOSITIVEFLOAT;
  dispcut       = BIGNEGATIVEFLOAT;
  eplowcut      = BIGNEGATIVEFLOAT;
  ephighcut     = BIGPOSITIVEFLOAT;
  phivcut1	= BIGPOSITIVEFLOAT;
  phivcut2	= BIGPOSITIVEFLOAT;
  invmasscut	= BIGPOSITIVEFLOAT;
  identifyElectrons = 0;
}

CONVInclusiveNanoCutsv2::CONVInclusiveNanoCutsv2(const CONVInclusiveNanoCutsv2& t) {
  ptlowcut     = t.ptlowcut;
  pthighcut    = t.pthighcut;
  vertexcut    = t.vertexcut;
  pc3Phicut    = t.pc3Phicut;
  pc3Zcut      = t.pc3Zcut;
  emcPhicut    = t.emcPhicut;
  emcZcut      = t.emcZcut;
  nx1cut       = t.nx1cut;
  nx2cut       = t.nx2cut;
  chi2overnpe0max = t.chi2overnpe0max;
  n0min        = t.n0min;
  n1min        = t.n1min;
  sn0min       = t.sn0min;
  qualitymin   = t.qualitymin;
  eoverpmin    = t.eoverpmin;
  eoverpmax    = t.eoverpmax;
  dispcut      = t.dispcut;
  eplowcut     = t.eplowcut;
  ephighcut    = t.ephighcut;
  phivcut1     = t.phivcut1;
  phivcut2     = t.phivcut2;
  invmasscut   = t.invmasscut;
  identifyElectrons = t.identifyElectrons;
}

void CONVInclusiveNanoCutsv2::Initialize(PHCompositeNode* topnode) {
  Reset();
  ptlowcut         =  0.150;  //  in GeV
  pthighcut        =  25.0;   //  in Gev
  vertexcut        =  30.0;   //  in cm
  pc3Phicut        =  999999.0;   //  in sigma
  pc3Zcut          =  999999.0;   //  in sigma
  emcPhicut        =  4.0;   //  in sigma
  emcZcut          =  4.0;   //  in sigma
  nx1cut           =     0;   //  >= Dch hits required
  nx2cut           =     0;   //  >= Dch hits required
  n0min            =     3;   //  >= tubes
  n1min            =     3;   //  >= tubes
  sn0min           =     3;   //  >= tubes
  chi2overnpe0max  =    15.;  // divide by ~5 to get it in sigmas
  dispcut          =    5.;   // in cm
  eplowcut         =    0.5; 
  ephighcut        =    1.5;
  //phivcut1     	   =  0.005;  // pair is accepted as a conversion pair if
  phivcut1     	   =  0.1;  // pair is accepted as a conversion pair if
  phivcut2         =  0.007;  // phiv < phivcut1+phivcut2/invmass
  invmasscut       =  0.5;   // in GeV
  identifyElectrons = 1;  // if 0, use only phiv cut
}

PHBoolean CONVInclusiveNanoCutsv2::CentralTrackOK(PHCentralTrack* php, const unsigned int itrk) {

// sanity check
  if(itrk>=(unsigned int)php->get_npart()) {return False;}

  // Reject particles that failed to produce momenta:
  if (php->get_the0(itrk)<0) {return False;}

  // The cuts located below this line make physically reasonable
  // decisions.  For testing putposes, we will apply only a single
  // simple cut to be clear about whether it was indeed applied...
  float pt = php->get_pt(itrk);
  if(pt<ptlowcut || pt>pthighcut) {return False;}

// Loop over all tracks, find electrons and put their indices into a vector.
// Return True if particle # itrk is an electron.

  vector<unsigned int> electrons; 

  for(unsigned int i=0; i<(unsigned int)php->get_npart(); i++) {
    if(identifyElectrons) {
      if(php->get_n0(i)<n0min) {continue;}
      if(php->get_npe0(i)==0) {continue;}
      if(php->get_chi2(i)/php->get_npe0(i)>chi2overnpe0max) {continue;}
      if(fabs(php->get_emcdphi(i))>emcPhicut) {continue;}
      if(fabs(php->get_emcdz(i))>emcZcut) {continue;}   
      float p = php->get_mom(i);
      if(p==0.) {continue;}
      float e = php->get_ecore(i);
      if((e/p)<eplowcut || (e/p)>ephighcut) {continue;}
      if(php->get_disp(i)>dispcut) {continue;}
//      if(php->get_quality(i)<qualitycut) {continue;}
      float px = php->get_px(i);
      float py = php->get_px(i);
      float pt = sqrt(px*px+py*py);
      if(pt<ptlowcut || pt>pthighcut) {continue;}
    }
// If we came to this point, we have found an electron
      electrons.push_back(i);
  }

// Check if track # itrk is an electron
  if(identifyElectrons) {
    for(unsigned int i=0; i<electrons.size(); i++) {
      if(electrons[i]==itrk) {return True;}
    }
  }

// If not, check if track # itrk satisfies conversion pair cut 
// with one of the electrons (if any).

  for(unsigned int i=0; i<electrons.size(); i++) {
    if(electrons[i]!=itrk) {
      float invmass = get_invmass(php, electrons[i], itrk);
      float phiv = get_phiv(php, electrons[i], itrk);
      if(invmass>0. && invmass<invmasscut && phiv<(phivcut1+phivcut2/invmass)) {return True;}
    }
  }

  //electrons.clear();
  return False;

}

float CONVInclusiveNanoCutsv2::get_invmass(PHCentralTrack* tracks, int i1, int i2) {

float invmass=0.;

  float px1 = tracks->get_px(i1);
  float py1 = tracks->get_py(i1);
  float pz1 = tracks->get_pz(i1);
  float px2 = tracks->get_px(i2);
  float py2 = tracks->get_py(i2);
  float pz2 = tracks->get_pz(i2);
// We assume that these are electrons, and neglect their mass
  float e1 = sqrt(px1*px1+py1*py1+pz1*pz1);
  float e2 = sqrt(px2*px2+py2*py2+pz2*pz2);
  float dotproduct = px1*px2+py1*py2+pz1*pz2;
  float mass2 = 2.*(e1*e2-dotproduct);
  if(mass2>0.) { invmass=sqrt(mass2); }
    else { invmass=0.; }
//  float cosphi = dotproduct/e1/e2;
//  float assymmetry = fabs(e1-e2)/(e1+e2);
//  float q = e1*e1 + e2*e2 - 2.*dotproduct;

  return invmass;
}

float CONVInclusiveNanoCutsv2::get_phiv(PHCentralTrack* tracks, int i1, int i2) {

  float px1 = tracks->get_px(i1);
  float py1 = tracks->get_py(i1);
  float pz1 = tracks->get_pz(i1);
  float px2 = tracks->get_px(i2);
  float py2 = tracks->get_py(i2);
  float pz2 = tracks->get_pz(i2);
  float ux = px1+px2;  // normalized photon direction
  float uy = py1+py2;
  float uz = pz1+pz2;
  float uu = sqrt(ux*ux+uy*uy+uz*uz);
  ux=ux/uu;
  uy=uy/uu;
  uz=uz/uu;
  float vx = py1*pz2-pz1*py2; // normalized perpendicular to decay plane
  float vy = -px1*pz2+pz1*px2;
  float vz = px1*py2-py1*px2;
  float vv = sqrt(vx*vx+vy*vy+vz*vz);
  vx=vx/vv;
  vy=vy/vv;
  vz=vz/vv;
  float wx = uy*vz-uz*vy; // perpendicular to both u and v
  float wy = -ux*vz+uz*vx;
  float wz = ux*vy-uy*vx;
  float uax = uy/sqrt(uy*uy+ux*ux);
  float uay = -ux/sqrt(uy*uy+ux*ux);
  float uaz = 0.;
  float phiv = acos(wx*uax+wy*uay+wz*uaz); // angle between mag. field direction and
                                           // perpendicular to decay plane
  return phiv;
}

