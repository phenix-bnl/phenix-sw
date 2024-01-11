// $Id: FvtxFastSim.cxx,v 1.6 2009/05/19 07:18:48 hpereira Exp $

/*!
  \file FvtxFastSim.cxx	
  \ingroup supermodules
  \brief reads TMutMCtrkMap and create TFvtxMCHit accordingly
  \author Hugo Pereira
  \version $Revision: 1.6 $
  \date $Date: 2009/05/19 07:18:48 $
*/

#include <FVTXOO.h>
#include <mFvtxFastSim.h>
#include <mFvtxFastSimPar.h>
#include <mFvtxResponse.h>
#include <mFvtxResponsePar.h>
#include <recoConsts.h>
#include <TFvtxHitMap.h>
#include <TFvtxMCHitMap.h>

#include "FvtxFastSim.h"

using namespace std;

//______________________________________________________
FvtxFastSim::FvtxFastSim( const char* name ) : 
  MuonSubsysReco( name ),
  _mFvtxFastSim_mod( 0 ),
  _mFvtxResponse_mod(0),
  _timer( new PHTimer(name) )
{
  return ;
}

//______________________________________________________
FvtxFastSim::~FvtxFastSim()
{ if( _mFvtxFastSim_mod ) delete _mFvtxFastSim_mod; }

//______________________________________________________
int FvtxFastSim::InitRun(PHCompositeNode *top_node)
{

  // Create Node Tree
  CreateNodeTree(top_node);
  
  // Zero Suppression
  if( !_mFvtxFastSim_mod ) _mFvtxFastSim_mod = new mFvtxFastSim();
  if( !_mFvtxResponse_mod ) _mFvtxResponse_mod = new mFvtxResponse();

  return 0;
}

//______________________________________________________
int FvtxFastSim::CreateNodeTree(PHCompositeNode *top_node)
{
  try {
    
    // Instantiate nodes for fvtxoo containers
    {
      PHNodeIterator nodeItr(top_node);
      _fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "fvtxoo"));
      if(!_fvtxoo_node){
        _fvtxoo_node = new PHCompositeNode("FVTXOO");
        top_node->addNode(_fvtxoo_node);
      }
    }
    
    // dest node
    {
      PHNodeIterator nodeItr(top_node);
      _dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    }
              
    // create TFvtxMCHitMap
    TFvtxMCHitMap *hit_map = TMutNode<TFvtxMCHitMap>::new_node( _fvtxoo_node, "TFvtxMCHitMap");
    if( _dst_node ) hit_map->make_persistant( _dst_node, "TFvtxMCHit" );

    // FVTX merged hits (on FVTXOO node)
    TFvtxHitMap* fvtx_hit_map = TMutNode<TFvtxHitMap>::new_node(_fvtxoo_node, "TFvtxHitMap");
    if( _dst_node ) fvtx_hit_map->make_persistant( _dst_node,"TFvtxHit");
    
    // mFvtxFastSimPar
    mFvtxFastSimPar* mFvtxFastSim_par = TMutNode<mFvtxFastSimPar>::new_node( _fvtxoo_node, "mFvtxFastSimPar");
    
    mFvtxResponsePar *mFvtxResponse_par = TMutNode<mFvtxResponsePar>::new_node( _fvtxoo_node, "mFvtxResponsePar" );
    mFvtxResponse_par->set_verbosity( FVTXOO::NONE );
      
    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) 
    {
      mFvtxFastSim_par->print();
      mFvtxResponse_par->print();
    }
            
  } catch (exception& e) { FVTXOO::TRACE(e.what()); }
  return 0;
}

//______________________________________________________
int FvtxFastSim::process_event(PHCompositeNode *top_node)
{

  _timer->restart();

  try {
    
    // read external vertex
    load_vertex_if_needed( top_node );
    
    _mFvtxFastSim_mod->event( top_node );
    _mFvtxResponse_mod->event( top_node );
      
  } catch (std::exception& e) { FVTXOO::TRACE(e.what()); }	
  
  write_maps_if_needed();
  _timer->stop();
  return 0;
}

//______________________________________________________
int FvtxFastSim::End(PHCompositeNode* top_node) 
{
  _timer->print_stat();	  
  if( _mFvtxFastSim_mod ) _mFvtxFastSim_mod->print_summary();
  
  return 0;
}
