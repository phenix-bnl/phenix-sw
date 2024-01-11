#include "EWGInclusiveNanoCutsv1.h"
#include "PHCentralTrack.h"
#include "PHGlobal.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"

#include <iostream>

ClassImp(EWGInclusiveNanoCutsv1)

using namespace std;

typedef PHIODataNode<PHGlobal> PHGlobalNode_t;

EWGInclusiveNanoCutsv1::EWGInclusiveNanoCutsv1() {
 Reset();
}

void EWGInclusiveNanoCutsv1::Reset() {
  ptlowcut	= BIGNEGATIVEFLOAT;
  pthighcut	= BIGPOSITIVEFLOAT;
  vertexcut	= BIGPOSITIVEFLOAT;
  pc3Phicut	= BIGPOSITIVEFLOAT;
  pc3Zcut	= BIGPOSITIVEFLOAT;
  emcPhicut	= BIGPOSITIVEFLOAT;
  emcZcut	= BIGPOSITIVEFLOAT;
  nx1cut	= BIGNEGATIVEINT;
  nx2cut	= BIGNEGATIVEINT;
  chi2overnpe0max = BIGPOSITIVEFLOAT;
  n0min         = BIGNEGATIVEINT;
  sn0min        = BIGNEGATIVEINT;
  qualitymin    = BIGNEGATIVEINT;
  eoverpmin     = BIGNEGATIVEFLOAT;
  eoverpmax     = BIGPOSITIVEFLOAT;

}

void EWGInclusiveNanoCutsv1::Initialize(PHCompositeNode* topnode) {
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

  Reset();

  cout << endl << "CNTnanoCuts::set_CentralTrackCuts() by Felix Matathias for the PHENIX electron Working Group" << endl << endl;

  ptlowcut	   =  0.150;  // 150 MeV
  pthighcut	   =  25.0;   //  20 Gev
  vertexcut	   =  50.0;   //  30 cm
  pc3Phicut	   =   999995.0;   //  5 sigma
  pc3Zcut	   =   999995.0;   //  5 sigma
  emcPhicut	   =   999995.0;   //  5 sigma
  emcZcut	   =   999995.0;   //  5 sigma
  nx1cut	   =     0;   //  >= 2 hits required
  nx2cut	   =     0;   //  >= 2 hits required
  n0min            =     2;   //  >= 2 tubes
  sn0min           =     2;   //  >= 2 tubes

}

PHBoolean EWGInclusiveNanoCutsv1::CentralTrackOK(PHCentralTrack* php, const unsigned int itrk) {
  //cout << "Central Track cutting routine..." << endl;

  // sanity check
  if(itrk>=(unsigned int)php->get_npart()) return False;

  // Reject particles that failed to produce momenta:
  if (php->get_the0(itrk)<0) return False;


  // pt cuts -- a simple example...
  float pt = php->get_pt(itrk);
  if(pt<ptlowcut || pt>pthighcut) return False;


  //quality cut
  //if(php->get_quality(itrk) < qualitymin) return False;
  

  // Electron ID cuts
  if( (php->get_n0(itrk) < n0min) && (php->get_sn0(itrk) < sn0min) ) return False;
  

  // We want a PC3 match _OR_ an EMC match...
  PHBoolean inPC3 = True;
  PHBoolean inEMC = True;
  if ( fabs(php->get_pc3sdphi  (itrk)) > pc3Phicut ) inPC3 = False;
  if ( fabs(php->get_pc3sdz    (itrk)) > pc3Zcut   ) inPC3 = False;
  if ( fabs(php->get_emcsdphi_e(itrk)) > emcPhicut ) inEMC = False;
  if ( fabs(php->get_emcsdz_e  (itrk)) > emcZcut   ) inEMC = False;
  if ( !(inEMC || inPC3) ) return False;

  // We want at least 2 hits in each layer..
  if(php->get_nx1hits(itrk)<nx1cut) return False;
  if(php->get_nx2hits(itrk)<nx2cut) return False;

 
  //float energyovermomentum = php->get_ecore(itrk) / php->get_mom(itrk) ;
  //if((energyovermomentum<eoverpmin) || (energyovermomentum>eoverpmax)) return False;
  //if( (php->get_chi2(itrk)/php->get_npe0(itrk)) >= chi2overnpe0max ) return False;
  
  
  return True;
  //return False;
}


PHBoolean EWGInclusiveNanoCutsv1::GlobalOK(PHCompositeNode* topNode) {

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

