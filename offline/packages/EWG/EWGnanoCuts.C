#include "EWGnanoCuts.h"
#include "PHCentralTrack.h"
#include "PHGlobal.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include <iostream>

ClassImp(EWGnanoCuts)

using namespace std;

typedef PHIODataNode<PHGlobal> PHGlobalNode_t;

EWGnanoCuts::EWGnanoCuts() {
 Reset();
}

void EWGnanoCuts::Reset() {
  qualitycut	= BIGNEGATIVEINT;
  ptlowcut	= BIGNEGATIVEFLOAT;
  pthighcut	= BIGPOSITIVEFLOAT;
  demcphicut	= BIGPOSITIVEFLOAT;
  demczcut	= BIGPOSITIVEFLOAT;
  dpc1phicut	= BIGPOSITIVEFLOAT;
  dpc1zcut	= BIGPOSITIVEFLOAT;
  dpc2phicut	= BIGPOSITIVEFLOAT;
  dpc2zcut	= BIGPOSITIVEFLOAT;
  dpc3phicut	= BIGPOSITIVEFLOAT;
  dpc3zcut	= BIGPOSITIVEFLOAT;
  dtofphicut	= BIGPOSITIVEFLOAT;
  dtofzcut	= BIGPOSITIVEFLOAT;
  dtecphicut	= BIGPOSITIVEFLOAT;
  dtecalphacut	= BIGPOSITIVEFLOAT;
  eplowcut	= BIGNEGATIVEFLOAT;
  ephighcut	= BIGPOSITIVEFLOAT;
  npmt0cut	= BIGNEGATIVEINT;
  chi2cut	= BIGPOSITIVEFLOAT;
}

EWGnanoCuts::EWGnanoCuts(const EWGnanoCuts& t) {
  qualitycut    = t.qualitycut;
  ptlowcut      = t.ptlowcut;
  pthighcut     = t.pthighcut;
  demcphicut    = t.demcphicut;
  demczcut      = t.demczcut;
  dpc1phicut    = t.dpc1phicut;
  dpc1zcut      = t.dpc1zcut;
  dpc2phicut    = t.dpc2phicut;
  dpc2zcut      = t.dpc2zcut;
  dpc3phicut    = t.dpc3phicut;
  dpc3zcut      = t.dpc3zcut;
  dtofphicut    = t.dtofphicut;
  dtofzcut      = t.dtofzcut;
  dtecphicut    = t.dtecphicut;
  dtecalphacut  = t.dtecalphacut;
  eplowcut      = t.eplowcut;
  ephighcut     = t.ephighcut;
  npmt0cut      = t.npmt0cut;
  chi2cut       = t.chi2cut;
}

void EWGnanoCuts::Initialize(PHCompositeNode* topnode) {
  //  In principle, the cuts which people want to
  //  apply could depend upon a variety of different
  //  characteristics of the run in question
  //  (e.g. what is the run # ?
  //        what is the B-field?
  //
  //  All such questions can be answered by searching 
  //  the node tree.  This is why we supply it as an
  //  arguement to the initialize function.
  //
  //                         TKH 3-12-2002
  //
  set_ElectronCuts();
}

void EWGnanoCuts::set_ElectronCuts() {
  Reset();
  npmt0cut      = 2;
  chi2cut 	= 10.;
  ptlowcut	= 0.1;
  pthighcut	= 5.0;
  vertexcut	= 40.;
}

PHBoolean EWGnanoCuts::ParticleOK(PHParticle* php0, const unsigned int itrk) {
  //  Here we pull a very sneaky trick!!!
  //  In order for the base class to be 
  //  most general, we put the arguement type as
  //  PHParticle.
  //    In the electron analysis, the particle is actually a
  //  CentralTrack.  Sasha's trick is that he casts the PHParticle
  //  into a PHCentralTrack and then hands this to a 
  //  PHCentralTrack cutting routine.  Very Cool!
  //                 TKH 3-12-2002
  PHCentralTrack* php = (PHCentralTrack*)php0;
  return CentralTrackOK(php, itrk);
}

PHBoolean EWGnanoCuts::CentralTrackOK(PHCentralTrack* php, const unsigned int itrk) {

  //cout << "You have arrived in the Electron Track cut!!!" << endl;

// sanity check
  if(itrk>=(unsigned int)php->get_npart()) return False;

  // Reject particles that failed to produce momenta:
  if (php->get_the0(itrk)<0) return False;

  // The cuts located below this line make physically reasonable
  // decisions.  For testing putposes, we will apply only a single
  // simple cut to be clear about whether it was indeed applied...
  float pt = php->get_pt(itrk);
  if(pt<ptlowcut || pt>pthighcut)         return False;

//  // rich cuts
//    if(php->get_n0(itrk)<npmt0cut) return False;
//    if(npmt0cut>=0) {
//      if(php->get_n0(itrk)!=0) {
//        if(php->get_chi2(itrk)/php->get_n0(itrk)>chi2cut) return False;
//      } 
//      else {
//        return False;
//      }
//    }

//  // association cuts
//    if(fabs(php->get_pc2dphi(itrk))>dpc2phicut) return False;
//    if(fabs(php->get_pc3dphi(itrk))>dpc3phicut) return False;
//    if(fabs(php->get_pc2dz(itrk))>dpc2zcut)     return False;
//    if(fabs(php->get_pc3dz(itrk))>dpc3zcut)     return False;
//    if(fabs(php->get_emcdphi(itrk))>demcphicut) return False;
//    if(fabs(php->get_emcdz(itrk))>demczcut)     return False;
//    if(fabs(php->get_tofdphi(itrk))>dtofphicut) return False;
//    if(fabs(php->get_tofdz(itrk))>dtofzcut)     return False;
//    if(fabs(php->get_tecdphi(itrk))>dtecphicut) return False;

//  // emcal cuts. assumes emcal arm numbering convention
//    float p = php->get_mom(itrk);
//    float e = php->get_ecore(itrk);
//    if(php->get_arm(itrk)==1 && php->get_sect(itrk)<2) e = php->get_ecorr(itrk);
//    if(p==0.) {
//      return False;
//    }
//    else {
//      if((e/p)<eplowcut || (e/p)>ephighcut) return False;
//    }

//  // dch track cuts
//    if(php->get_quality(itrk)<qualitycut)   return False;
//    float px = php->get_px(itrk);
//    float py = php->get_px(itrk);
//    float pt = sqrt(px*px+py*py);
//    if(pt<ptlowcut || pt>pthighcut)         return False;


  return True;

}


PHBoolean EWGnanoCuts::GlobalOK(PHCompositeNode* topNode) {

  // This example cutter throws away anything with
  // a Z vertex outside of +/- 20 cm.
  //                   TKH 3-7-2002

  // Here we search the NDST node for the PHGlobal object...
  PHNodeIterator iter(topNode);
  PHCompositeNode *udstNode, *ndstNode;
  udstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  ndstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "NDST"));

  if(!udstNode) cout << "WARNING: udstnode not found " << endl;
  if(!ndstNode) cout << "WARNING: ndstnode not found " << endl;

  // Find Global Summary Object...
  PHTypedNodeIterator<PHGlobal> iGBL(ndstNode);
  PHGlobalNode_t *GBL = iGBL.find("PHGlobal");
  if(!GBL) {
    cout << "EWG Global Cut: no Global Summary Found" << endl;
    return False;
  } 
  PHGlobal *d_global = GBL->getData();

  // Now we apply the cut to the PHGlobal Object...

  
  //cout << "Z = " << d_global->getZVertex() << "  Cut = " << vertexcut << endl;
  if (fabs(d_global->getZVertex())>vertexcut) return False;
  return True;


}

