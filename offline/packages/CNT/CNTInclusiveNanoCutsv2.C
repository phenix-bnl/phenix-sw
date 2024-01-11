#include "CNTInclusiveNanoCutsv2.h"
#include "PHCentralTrack.h"
#include "PHGlobal.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include <iostream>

ClassImp(CNTInclusiveNanoCutsv2)

using namespace std;

typedef PHIODataNode<PHGlobal> PHGlobalNode_t;

CNTInclusiveNanoCutsv2::CNTInclusiveNanoCutsv2() {
 Reset();
}

void CNTInclusiveNanoCutsv2::Reset() {
  ptlowcut	= BIGNEGATIVEFLOAT;
  pthighcut	= BIGPOSITIVEFLOAT;
  vertexcut	= BIGPOSITIVEFLOAT;
  pc3Phicut	= BIGPOSITIVEFLOAT;
  pc3Zcut	= BIGPOSITIVEFLOAT;
  emcPhicut	= BIGPOSITIVEFLOAT;
  emcZcut	= BIGPOSITIVEFLOAT;
  nx1cut	= BIGPOSITIVEINT;
  nx2cut	= BIGPOSITIVEINT;
  for (int idet=0; idet<NUMEMCTYPES; idet++)
    {
      elowcut[idet]  = BIGNEGATIVEFLOAT;
      ehighcut[idet] = BIGPOSITIVEFLOAT;
      tofcut[idet]   = BIGPOSITIVEFLOAT;
    }
}

void CNTInclusiveNanoCutsv2::Initialize(PHCompositeNode* topnode) {
  //  These cuts should be fetched in a more intelligent way 
  //  from a database instead of hard-coded here...
  Reset();
  ptlowcut	=  0.150;  // 150 MeV
  pthighcut	=  25.0;   //  25 Gev
  vertexcut	=  50.0;   //  50 cm
  pc3Phicut	=   999999.0;   //  inf sigma
  pc3Zcut	=   999999.0;   //  inf sigma
  emcPhicut	=   999999.0;   //  inf sigma
  emcZcut	=   999999.0;   //  inf sigma
  nx1cut	=     0;   //  >= 0 hits required
  nx2cut	=     0;   //  >= 0 hits required
  for (int idet=0; idet<NUMEMCTYPES; idet++)
    {
      elowcut[idet] = -99999.;
      ehighcut[idet] = 99999.;
    }
  tofcut[PBSC] = 15.;	// PBSC tof cut (ns)
  tofcut[PBGL] = 20.;	// PBGL tof cut
}

PHBoolean CNTInclusiveNanoCutsv2::CentralTrackOK(PHCentralTrack* php, const unsigned int itrk) {

  //cout << "Central Track cutting routine..." << endl;

  // sanity check, itrk is unsigned int and therefore cannot be < 0
  if(itrk>= php->get_npart())
    {
      return False;
    }
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
}

PHBoolean CNTInclusiveNanoCutsv2::GlobalOK(PHCompositeNode* topNode) {

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







