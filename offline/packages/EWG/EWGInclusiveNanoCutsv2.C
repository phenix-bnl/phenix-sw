#include "EWGInclusiveNanoCutsv2.h"
#include "PHCentralTrack.h"
#include "PHGlobal.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"

#include <iostream>

ClassImp(EWGInclusiveNanoCutsv2)

using namespace std;

typedef PHIODataNode<PHGlobal> PHGlobalNode_t;

EWGInclusiveNanoCutsv2::EWGInclusiveNanoCutsv2() {
 Reset();
}

//cout << "About to set the cut values." << endl;

void EWGInclusiveNanoCutsv2::Reset() {
  chi2overnpe0max = BIGPOSITIVEFLOAT;
  qualitymin    = BIGNEGATIVEINT;
  ptlowcut	   =  0.150;   		//  GeV
  pthighcut	   =  25.0;   		//  Gev
  vertexcut	   =  30.0;   		//  cm
  pc3Phicut	   =  999995.0;   	//  sigma
  pc3Zcut	   =  999995.0;   	//  sigma
  emcPhicut	   =  999.0;   		//  sigma
  emcZcut	   =  999.0;    		//  sigma
  nx1cut	   =  0;   		//  >= hits required
  nx2cut	   =  0;   		//  >= hits required
  n0min            =  1;   		//  >= tubes
  sn0min           =  1;   		//  >= tubes
  eoverpmin        =  0;
  eoverpmax        =  999.0;
  dispcut          =  999.0;
  chi2overnpecut   =  999.0;

}

void EWGInclusiveNanoCutsv2::Initialize(PHCompositeNode* topnode) {
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
  //  Modified eID cuts for compact EWG nanodsts
  //  for run4 AuAu  (SL Sep-15-2004) 
  cout << endl << "EWGInclusiveNanoCutsv2 initialized." << endl;

}

PHBoolean EWGInclusiveNanoCutsv2::CentralTrackOK(PHCentralTrack* php, const unsigned int itrk) {


  // sanity check

  if(itrk>=(unsigned int)php->get_npart()) 
    {
      //cout << "Sanity check failed." << endl;
      return False;
    }
  
  // Reject particles that failed to produce momenta:
  
  if (php->get_the0(itrk)<0) 
    {
      return False;  
    }

  // pt cuts -- a simple example...
  float pt = php->get_pt(itrk);

  if(pt<ptlowcut || pt>pthighcut) 
    {
      return False;
    }
  
    
  // Electron ID cuts

  if ( (php->get_n0(itrk) < n0min) && (php->get_sn0(itrk) < sn0min) ) 
    { 
      return False; 
    }

  if ( (php->get_disp(itrk) > dispcut) && (php->get_sdisp(itrk) > dispcut) )
    {
      return False;
    }

  float chi2overnpe = php->get_npe0(itrk) / php->get_chi2(itrk) ;

  if (chi2overnpe > chi2overnpecut)
    {
      return False;
    }

  float energyovermomentum = php->get_ecore(itrk) / php->get_mom(itrk) ;

  if((energyovermomentum<eoverpmin) || (energyovermomentum>eoverpmax)) 
    {
      return False;
    }  

  if ( fabs(php->get_emcsdphi_e(itrk)) > emcPhicut ) 
    {
      //return False;
    }
  if ( fabs(php->get_emcsdz_e  (itrk)) > emcZcut   ) 
    {
      //return False;
    }

  // We want at least 2 hits in each layer..


  if(php->get_nx1hits(itrk)<nx1cut) 
    {
      return False;
    }
  if(php->get_nx2hits(itrk)<nx2cut) 
    {
      return False;
    }

  /*cout << "Success!! "  << endl;
  cout << "n0 = " << php->get_n0(itrk)
       << " pT = " << pt
       << " E/p = " << energyovermomentum
       << " track = " << itrk
       << " theta0 = " << php->get_the0(itrk)
       << endl;*/

  return True;
}


PHBoolean EWGInclusiveNanoCutsv2::GlobalOK(PHCompositeNode* topNode) {

  // This example cutter throws away anything with
  // a Z vertex outside of zvertexcut
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

  if (fabs(d_global->getZVertex())>vertexcut) 
    {
      return False;
      //  cout << "z_vertex was out of designated bounds." << endl;
    }
  return True;

}

