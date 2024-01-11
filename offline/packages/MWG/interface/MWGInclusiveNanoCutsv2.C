// $Id: MWGInclusiveNanoCutsv2.C,v 1.2 2016/05/01 17:04:43 pinkenbu Exp $

#include "MWGInclusiveNanoCutsv2.h"
#include "rcp.h"

#include <PHMuoTracksOut.h>
#include <PHGlobal.h>
//INCLUDECHECKER: Removed this line: #include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include <iostream>

using namespace std;

ClassImp(MWGInclusiveNanoCutsv2)

//___________________________________________________________________
typedef PHIODataNode<PHGlobal> PHGlobalNode_t;

MWGInclusiveNanoCutsv2::MWGInclusiveNanoCutsv2() {
 Reset();
}

//___________________________________________________________________
void MWGInclusiveNanoCutsv2::Reset() 
{

  // default cuts
  ghostsel = true;
  ptlowcut	= 0.0;
  pthighcut	= 1000.0; 
  vertexcut	= 10000.0;    
  minhitcut =  0;      
  mintrackcut = 0;
  maxtrackcut = 1000;
  
  dodimu        = true;
  dimasscut     = 0.2;
  
  dofilter = false;
  min_dimass_filter = BIGNEGATIVEFLOAT;
  max_dimass_filter = BIGPOSITIVEFLOAT;

}


//___________________________________________________________________
void MWGInclusiveNanoCutsv2::Initialize(PHCompositeNode* topnode) 
{

  // set default values for cuts
  cout << "Set Muon Working Group parameters:" << endl;
  if (RCP::file_ok("MWG.rcp"))
  {
    
    cout << "MWGInclusiveNanoCutsv2::Initialize - reading cuts from MWG.rcp" <<endl;
    bool all_ok( true );
    all_ok &= RCP::get<bool>(   "MWG.rcp","ghostsel", ghostsel );
    all_ok &= RCP::get<float>(  "MWG.rcp","ptlowcut", ptlowcut );
    all_ok &= RCP::get<float>(  "MWG.rcp","pthighcut", pthighcut );
    all_ok &= RCP::get<float>(  "MWG.rcp","vertexcut", vertexcut );
    all_ok &= RCP::get<int>(    "MWG.rcp","minhitcut", minhitcut );
    all_ok &= RCP::get<bool>(   "MWG.rcp","dodimu", dodimu );
    all_ok &= RCP::get<float>(  "MWG.rcp","dimasscut", dimasscut );

    all_ok &= RCP::get<bool>(  "MWG.rcp","dofilter", dofilter );
    all_ok &= RCP::get<float>(  "MWG.rcp","min_dimass_filter", min_dimass_filter );
    all_ok &= RCP::get<float>(  "MWG.rcp","max_dimass_filter", max_dimass_filter );

    if( !all_ok ) cout << "MWGInclusiveNanoCutsv2::Initialize - WARNING: some variables could not be found. using default." <<endl;

  } else cout << "MWGInclusiveNanoCutsv2::Initialize - using default cuts" <<endl;

  // dump values
  cout << "   ghostsel    = " << get_ghostsel()<<endl;
  cout << "   ptlowcut    = " << get_ptlowcut()<<endl;
  cout << "   pthighcut   = " << get_pthighcut()<<endl;
  cout << "   minhitcut   = " << get_minhitcut()<<endl;
  cout << "   vertexcut   = " << get_vertexcut()<<endl;
  cout << "   dodimu      = " << get_dodimu()<<endl;
  cout << "   dimasscut   = " << get_dimasscut()<<endl;
  
  cout << "   dofilter            = " << get_dofilter()<< endl;
  cout << "   min_dimass_filter   = " << get_min_dimass_filter()<< endl;
  cout << "   max_dimass_filter   = " << get_max_dimass_filter()<< endl;
}

//___________________________________________________________________
PHBoolean MWGInclusiveNanoCutsv2::MuonOK(PHMuoTracksOut* php, const unsigned int itrk) {

// sanity check
// coverity: checking unsigned int < 0 is pointless
  //  if(itrk<0 || itrk>=(unsigned int)php->get_npart()) return False;
  if(itrk>=(unsigned int)php->get_npart()) return False;

// mu cuts 
  if(ghostsel && php->get_ghostflag(itrk)) return False;
  if(php->get_pt(itrk)<ptlowcut ) return False;
  if(php->get_pt(itrk)>pthighcut) return False;
  if(php->get_nhits(itrk)<minhitcut) return False;
  // pass all cuts
  return True;

}

//___________________________________________________________________
PHBoolean MWGInclusiveNanoCutsv2::diMuonOK(PHMuoTracksOut* php, const unsigned int idimu) {

  // sanity check
  // coverity: checking unsigned int < 0 is pointless
  //  if(idimu<0 || idimu>=(unsigned int)php->get_ndimu()) return False;
  if(idimu>=(unsigned int)php->get_ndimu()) return False;

  // dimu cuts
  if (php->get_dimass(idimu)<dimasscut) return False;

  // pass all cuts
  return True;

}

//___________________________________________________________________
PHBoolean MWGInclusiveNanoCutsv2::GlobalOK(PHCompositeNode* topNode) {

  // This example cutter throws away anything with
  // a Z vertex outside of the specified range.
  //                   TKH 3-7-2002

  // Here we search the NDST node for the PHGlobal object...
  PHNodeIterator iter(topNode);
  PHCompositeNode *udstNode, *ndstNode;
  udstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  ndstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "NDST"));

  if(!udstNode) cout << "MWGInclusiveNanoCutsv2::GlobalOK - WARNING: udstnode not found " << endl;
  if(!ndstNode) ndstNode=topNode;

  // Find Global Summary Object...
  PHTypedNodeIterator<PHGlobal> iGBL(ndstNode);
  PHGlobalNode_t *GBL = iGBL.find("PHGlobal");
  if(!GBL) {
    cout << "MWGInclusiveNanoCutsv2::GlobalOK - no Global Summary Found" << endl;
    return False;
  } 
  PHGlobal *d_global = GBL->getData();

  // Now we apply the cut to the PHGlobal Object...
  if (fabs(d_global->getBbcZVertex())>vertexcut) return False;

  // pass all cuts
  return True;

}

