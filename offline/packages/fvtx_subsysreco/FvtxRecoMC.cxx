// $Id: FvtxRecoMC.cxx,v 1.6 2009/05/19 07:18:48 hpereira Exp $

/*!
\filev FvtxRecoMC.cxx 
\ingroup supermodules
\brief  
FVTX reconstruction module, using Monte Carlo information.
Reads a DST that has been through the FVTX response stage, creates
TFvtxTrk objects from the MC tracks (mFvtxFindTrackMC) and then fits the
tracks using Kalman Filter fitting (mFvtxKalFitMC).
\author Melynda Brooks
\date  2006/07/27
*/

#include <boost/array.hpp>
#include <FVTXOO.h>
#include <FvtxGeom.h>
#include <mFvtxFindTrackMCPar.h>
#include <mFvtxKalFitMCPar.h>
#include <PHTimer.h>
#include <recoConsts.h>
#include <TFvtxClusMap.h>
#include <TFvtxCoordMap.h>

#include "FvtxRecoMC.h"

using namespace std;

//______________________________________________________
FvtxRecoMC::FvtxRecoMC( const char* name ) :
  MuonSubsysReco( name ),
  _fvtx_node(0),
  _timer( new PHTimer( name ) )
{}

//______________________________________________________
int FvtxRecoMC::InitRun(PHCompositeNode *top_node)
{
  try {
    
    set_node_ptrs(top_node);
    set_interface_ptrs(top_node);
    set_module_ptrs(top_node);

  } catch(std::exception& e){ cout << e.what() << endl; } 
    
  return 0;
}

//______________________________________________________
int FvtxRecoMC::set_node_ptrs(PHCompositeNode *top_node)
{
    
  {
    // FVTX working node _fvtx_node 
    PHNodeIterator nodeItr(top_node);
    _fvtx_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
    if(!_fvtx_node){
      _fvtx_node = new PHCompositeNode("FVTXOO");
      top_node->addNode(_fvtx_node);
    }
  }
  
  {
    // dst node 
    PHNodeIterator nodeItr(top_node);
    _dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if(!_dst_node){
      _dst_node = new PHCompositeNode("DST");
      top_node->addNode(_dst_node);
    }
    
//     _gea_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "GEA"));
//     if(_fvtx_node){
//       if (_gea_node) _fvtx_node->addNode(_gea_node);
//     }
    
  }
  
  return 0;
  
}

//______________________________________________________
int FvtxRecoMC::set_interface_ptrs(PHCompositeNode *top_node)
{
      
  try{
    
    // FVTX coords and tracks made on fvtx_node and written to dst_node
    TFvtxCoordMap* fvtx_coord_map = TMutNode<TFvtxCoordMap>::new_node(_fvtx_node, "TFvtxCoordMap");
    fvtx_coord_map->make_persistant(_dst_node,"TFvtxCoordMap");

    TFvtxClusMap* fvtx_clus_map = TMutNode<TFvtxClusMap>::new_node(_fvtx_node, "TFvtxClusMap");
    fvtx_clus_map->make_persistant(_dst_node,"TFvtxClusMap");

    TFvtxTrkMap* fvtx_trk_map = TMutNode<TFvtxTrkMap>::new_node(_fvtx_node, "TFvtxTrkMap");
    fvtx_trk_map->make_persistant(_dst_node,"TFvtxTrkMap");

    top_node->print();

  } catch(std::exception& e){ cout << e.what() << endl; } 

  return 0;
  
}

//______________________________________________________
int FvtxRecoMC::set_module_ptrs(PHCompositeNode *top_node)
{
  try{
    // Module parameters for mFvtxKalFitMC
    mFvtxKalFitMCPar *mFvtxKalFitMC_par = TMutNode<mFvtxKalFitMCPar>::new_node( _fvtx_node, "mFvtxKalFitMCPar" );
    mFvtxKalFitMC_par->set_verbosity( FVTXOO::NONE );
    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) mFvtxKalFitMC_par->print();
    
    // Module parameters for mFvtxFindTrackMC
    mFvtxFindTrackMCPar* mFvtxFindTrackMC_par = TMutNode<mFvtxFindTrackMCPar>::new_node( _fvtx_node, "mFvtxFindTrackMCPar" );
    mFvtxFindTrackMC_par->set_verbosity( FVTXOO::NONE );
    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) mFvtxFindTrackMC_par->print();
    
  } catch(std::exception& e){ cout << e.what() << endl; } 
  
  return 0;

}

//______________________________________________________
int FvtxRecoMC::process_event(PHCompositeNode *top_node)
{
  
  _timer->restart();
  	
  // Call the response
  try {
     
    _mFvtxFindTrackMC_mod.event( _fvtx_node );
    _mFvtxKalFitMC_mod.event( _fvtx_node );
    
  } catch (exception& e) {
    FVTXOO::TRACE(e.what());
  }	

  write_maps_if_needed();
  _timer->stop();
  return 0;

}

//______________________________________________________
int FvtxRecoMC::End(PHCompositeNode* top_node) 
{
  _timer->print_stat();
  return 0;
}
