// $Id: RxnpReco.cxx,v 1.10 2009/05/19 07:19:12 hpereira Exp $

/*!
  \file    RxnpReco.cxx
  \ingroup supermodules 
  \brief   Rxnp reconstruction event loop
  \author  Chun Zhang
  \version $Revision: 1.10 $
  \date    $Date: 2009/05/19 07:19:12 $
*/

//#include <boost/shared_ptr.hpp>

#include <PHCompositeNode.h>
#include <recoConsts.h>
#include <getClass.h>
#include <RunHeader.h>
#include <TMutNode.h>
#include <Fun4AllReturnCodes.h>
#include <PHTimeStamp.h>

#include <TRxnpGeom.h>
#include <TRxnpFEM.h>

#include <TRxnpRawScintMap.h>
#include <TRxnpScintMap.h>
#include <TRxnpRawXangMap.h>

#include "RxnpReco.h"


using namespace std;
using namespace findNode;
//______________________________________________________
RxnpReco::RxnpReco( const char* name ) : 
  MuonSubsysReco( name ),
  _timer( PHTimeServer::get()->insert_new(name) )
{ 
  _iter = 0;
  _use_db = 1;
}

//______________________________________________________
int RxnpReco::InitRun(PHCompositeNode *topNode)
{
  MUTOO::PRINT( cout, "RxnpReco::InitRun" );
  CreateNodeTree(topNode);

  // initialize the database info to
  // need run node too
  RunHeader* d_runhdr = getClass<RunHeader>(topNode,"RunHeader");
  if( !d_runhdr )
  {
    std::cerr << PHWHERE << " Can not find RunHeader :: you should have HeaderReco in your macro" << std::endl;  
    return ABORTRUN;
  }
  
  // run number
  int runnumber =  d_runhdr->get_RunNumber();

  // init the reaction plane calculation
  _mRxnpCalXang_mod.init_calcu(runnumber, 0);

  // Get TRxnpFEM and init it
  boost::shared_ptr<TRxnpFEM> fem = TRxnpFEM::instance();
  if(_use_db == 0) fem->set_use_db(false);
  if(!fem->init(runnumber))
  {
    std::cerr << PHWHERE << " Can not init the FEM database info." << std::endl;  
    return ABORTRUN;      
  }
  
  // StartTime 
  PHTimeStamp Tstart(d_runhdr->get_TimeStart());
    
  // Get TRxnpGeom and init it
  boost::shared_ptr<TRxnpGeom> geom = TRxnpGeom::instance();

  if(_use_db == 0) geom->set_use_db(false);

  if(!geom->init(Tstart))
  {
    std::cerr << PHWHERE << " Can not init the geometry database info." << std::endl;  
    return ABORTRUN;      
  }
 
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//______________________________________________________
int RxnpReco::CreateNodeTree(PHCompositeNode *top_node)
{
  
  // Instantiate nodes for Rxnp containers
  {
    PHNodeIterator nodeItr(top_node);
    rxnp_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RXNP"));
    if(!rxnp_node){
      rxnp_node = new PHCompositeNode("RXNP");
      top_node->addNode(rxnp_node);
    }
  }
  
  {
    PHNodeIterator nodeItr(top_node);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!dst_node) {
      dst_node = new PHCompositeNode("DST");
      top_node->addNode(dst_node);
    }
  }
  
  // Interface Object Containers (IOCS)
  //  recoConsts *rc = recoConsts::instance();
  TRxnpRawScintMap* raw_map = TMutNode<TRxnpRawScintMap>::new_node(rxnp_node, "TRxnpRawScintMap");    
  TRxnpScintMap* scint_map = TMutNode<TRxnpScintMap>::new_node(rxnp_node, "TRxnpScintMap");    
  TRxnpRawXangMap* xang_map = TMutNode<TRxnpRawXangMap>::new_node(rxnp_node, "TRxnpRawXangMap");    
  
  // Make IOCs persistent here
  raw_map->make_persistant(dst_node,"TRxnpRawScint");
  scint_map->make_persistant(dst_node,"TRxnpScint");
  xang_map->make_persistant(dst_node,"TRxnpRawXang");

  // Module parameter tables
  TMutNode<mRxnpUnpackPRDFPar>::new_node(rxnp_node,"mRxnpUnpackPRDFPar");
  TMutNode<mRxnpCalXangPar>::new_node(rxnp_node,"mRxnpCalXangPar");
  return 0;
}

//______________________________________________________
int RxnpReco::process_event(PHCompositeNode *top_node)
{
  
  _timer.get()->restart();

  // for vertex loading
  MuonSubsysReco::load_vertex_if_needed( top_node );

  // Call RXNP modules for track momentum reconstruction and vertex finding
  try {
    
    mRxnpCalXangPar* calxang_par = TMutNode<mRxnpCalXangPar>::find_node(top_node, "mRxnpCalXangPar");
    calxang_par->set_iteration(_iter);
    
    // unpack and calibrabte
    _mRxnpUnpackPRDF_mod.event(top_node);
    
    // calculate Xang
    _mRxnpCalXang_mod.event(top_node);

  } catch (exception& e) {
    MUTOO::TRACE(e.what());
  }  

  MuonSubsysReco::write_maps_if_needed();
  
  _timer.get()->stop();

  return 0;
}

//______________________________________________________
int RxnpReco::End(PHCompositeNode* top_node) 
{
  _timer.get()->print_stat();
  _mRxnpCalXang_mod.closeHisto(); 
  return 0;
}

