#include "CNTnanoCuts.h"
#include "PHGlobal.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include <iostream>

ClassImp(CNTnanoCuts)

using namespace std;

typedef PHIODataNode<PHGlobal> PHGlobalNode_t;

CNTnanoCuts::CNTnanoCuts() {
 Reset();
}

void CNTnanoCuts::Reset() {
  ptlowcut	= BIGNEGATIVEFLOAT;
  pthighcut	= BIGPOSITIVEFLOAT;
  vertexcut	= BIGPOSITIVEFLOAT;
  pc3Phicut	= BIGPOSITIVEFLOAT;
  pc3Zcut	= BIGPOSITIVEFLOAT;
  emcPhicut	= BIGPOSITIVEFLOAT;
  emcZcut	= BIGPOSITIVEFLOAT;
  nx1cut	= BIGPOSITIVEINT;
  nx2cut	= BIGPOSITIVEINT;
}

CNTnanoCuts::CNTnanoCuts(const CNTnanoCuts& t) {
  ptlowcut     = t.ptlowcut;
  pthighcut    = t.pthighcut;
  vertexcut    = t.vertexcut;
  pc3Phicut    = t.pc3Phicut;
  pc3Zcut      = t.pc3Zcut;
  emcPhicut    = t.emcPhicut;
  emcZcut      = t.emcZcut;
  nx1cut       = t.nx1cut;
  nx2cut       = t.nx2cut;
}

void CNTnanoCuts::Initialize(PHCompositeNode* topnode) {
  set_CentralTrackCuts();
}

void CNTnanoCuts::set_CentralTrackCuts() {
  //  These cuts should be fetched in a more intelligent way 
  //  from a database instead of hard-coded here...
  Reset();
  ptlowcut	=  0.150;  // 200 MeV
  pthighcut	=  20.0;   //  20 Gev
  vertexcut	=  35.0;   //  30 cm
  pc3Phicut	=   5.0;   //  5 sigma
  pc3Zcut	=   5.0;   //  5 sigma
  emcPhicut	=   5.0;   //  5 sigma
  emcZcut	=   5.0;   //  5 sigma
  nx1cut	=     2;   //  >= 2 hits required
  nx2cut	=     2;   //  >= 2 hits required
}

PHBoolean CNTnanoCuts::ParticleOK(PHParticle* php0, const unsigned int itrk) {
  //  Here we pull a very sneaky trick!!!
  //  In order for the base class to be 
  //  most general, we put the arguement type as
  //  PHParticle.
  //    In the analysis, the particle is actually a
  //  CentralTrack.  Sasha's trick is that he casts the PHParticle
  //  into a PHCentralTrack and then hands this to a 
  //  PHCentralTrack cutting routine.  Very Cool!
  //                 TKH 3-12-2002
  PHCentralTrack* php = (PHCentralTrack*)php0;
  return CentralTrackOK(php, itrk);
}

PHBoolean CNTnanoCuts::CentralTrackOK(PHCentralTrack* php, const unsigned int itrk) {

  //cout << "Central Track cutting routine..." << endl;

  // sanity check, itrk is unsigned and therefore cannot be <0
  if(itrk>=(unsigned int)php->get_npart()) return False;

  // Reject particles that failed to produce momenta:
  if (php->get_the0(itrk)<0) return False;


  // pt cuts -- a simple example...
  float pt = php->get_pt(itrk);
  if(pt<ptlowcut || pt>pthighcut) return False;

  // We want a PC3 match _OR_ an EMC match...
  PHBoolean inPC3 = True;
  PHBoolean inEMC = True;
  if ( fabs(php->get_pc3sdphi(itrk)) > pc3Phicut ) inPC3 = False;
  if ( fabs(php->get_pc3sdz  (itrk)) > pc3Zcut   ) inPC3 = False;
  if ( fabs(php->get_emcsdphi(itrk)) > emcPhicut ) inEMC = False;
  if ( fabs(php->get_emcsdz  (itrk)) > emcZcut   ) inEMC = False;
  if ( !(inEMC || inPC3) ) return False;

  // We want at least 2 hits in each layer..
  if(php->get_nx1hits(itrk)<nx1cut) return False;
  if(php->get_nx2hits(itrk)<nx2cut) return False;

  return True;
  //return False;
}


PHBoolean CNTnanoCuts::GlobalOK(PHCompositeNode* topNode) {

  // This example cutter throws away anything with
  // a Z vertex outside of the specified range.
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
    cout << "CNT Global Cut: no Global Summary Found" << endl;
    return False;
  } 
  PHGlobal *d_global = GBL->getData();

  // Now we apply the cut to the PHGlobal Object...
  //cout << "Hadron Event cutting routine..." << endl;

  if (fabs(d_global->getZVertex())>vertexcut) return False;
  return True;

}







