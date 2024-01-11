// $Id: FvtxUnpackPisa.cxx,v 1.17 2014/12/31 18:58:44 jinhuang Exp $
#include <FvtxGeom.h>
#include <FVTXOO.h>
#include <mFvtxSlowSim.h>
#include <mFvtxResponse.h>
#include <PHIODataNode.h>
#include <PHMapManager.h>
#include <PHTimer.h>
#include <PHTimeServer.h>
#include <recoConsts.h>
#include <SvxGetGEA.h>
#include <SvxPisaHitv1.h>
#include <TMutMCTrkMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxPisaHitMap.h> 
#include <TFvtxGlobalParCntrl.h>

#include "FvtxUnpackPisa.h"

using namespace std;

//______________________________________________________
FvtxUnpackPisa::FvtxUnpackPisa() : 
  MuonSubsysReco("FVTXUNPACKPISA"),
  _run_response( true ),
  _fvtx_node( 0 ),
  _mutoo_node( 0 ),
  _dst_node( 0 ),
  _timer( PHTimeServer::get()->insert_new("FVTXUNPACKPISA") )
{}

//______________________________________________________
int FvtxUnpackPisa::InitRun(PHCompositeNode *topNode)
{

  TFvtxGlobalParCntrl::init_run();
  // Create Node Tree
  create_node_tree(topNode);
  
  return 0;
}

//__________________________________________________________________________
int FvtxUnpackPisa::create_node_tree(PHCompositeNode *top_node)
{
  
  // FVTXOO node
  {
    PHNodeIterator nodeItr(top_node);
    _fvtx_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
    
    if(!_fvtx_node)
    {
      _fvtx_node = new PHCompositeNode("FVTXOO");
      top_node->addNode(_fvtx_node);
    }
    
  }
  
  // MUTOO node (for MCTrkMaps)
  {
    PHNodeIterator nodeItr(top_node);
    _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    
    if(!_mutoo_node)
    {
      _mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(_mutoo_node);
    }
    
  }
  
  // DST node
  {
    PHNodeIterator nodeItr(top_node);
    _dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    
    if (!_dst_node) 
    {
      _dst_node = new PHCompositeNode("DST");
      top_node->addNode( _dst_node );
    }
    
  }

  // fvtx PISA hit map (to store barrel hits)
  TFvtxPisaHitMap* pisa_hit_map = TMutNode<TFvtxPisaHitMap>::new_node(_fvtx_node,"TFvtxPisaHitMap");
  pisa_hit_map->make_persistant(_dst_node,"TFvtxPisaHit");
  
  // fvtx MC hit map
  TFvtxMCHitMap* mc_hit_map = TMutNode<TFvtxMCHitMap>::new_node(_fvtx_node,"TFvtxMCHitMap");
  mc_hit_map->make_persistant(_dst_node,"TFvtxMCHit");
  
  // fvtx MC hit map
  TFvtxHitMap* hit_map = TMutNode<TFvtxHitMap>::new_node(_fvtx_node,"TFvtxHitMap");
  if ( do_response() ) hit_map->make_persistant(_dst_node,"TFvtxHit");
  

  // MC Trk map
  TMutMCTrkMap* mc_trk_map = 0;
  try {
    
    mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(_mutoo_node,"TMutMCTrkMap");
    cout << "FvtxUnpackPisa::create_node_tree - found existing TMutMCTrkMap" << endl;
    
  } catch(std::exception& e){
    
    cout << "FvtxUnpackPisa::create_node_tree - creating TMutMCTrkMap" << endl;
    mc_trk_map = TMutNode<TMutMCTrkMap>::new_node(_mutoo_node,"TMutMCTrkMap");
    mc_trk_map->make_persistant(_dst_node,"TMutMCTrk");
    
  }
  
  // Set up pisa takes needed for mut/muioo slowsim
  PHNodeIterator nodeIter(top_node);
  PHCompositeNode *gea_node = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "GEA"));
  
  // setup pisa. There is one FVTX node per station
  SvxPisaHit* svx = new SvxPisaHitv1();
  PHIODataNode<SvxPisaHit>* svx_node = new PHIODataNode<SvxPisaHit>(svx, "SvxPisaHit");
  gea_node->addNode(svx_node);

  FVTXOO::Verbosity verblevel = FVTXOO::NONE;
  if ( verbosity == 1 ) verblevel = FVTXOO::SOME;
  else if ( verbosity > 1 ) verblevel = FVTXOO::ALOT;

  // Module parameter tables
  mFvtxSlowSimPar* mFvtxSlowSim_par = TMutNode<mFvtxSlowSimPar>::new_node( _fvtx_node, "mFvtxSlowSimPar" );
  mFvtxSlowSim_par->set_verbosity(verblevel);
  mFvtxSlowSim_par->set_do_evaluation( false ); 
  mFvtxSlowSim_par->set_check_consistency( false );
      
  // Module parameter tables
  mFvtxResponsePar* mFvtxResponse_par = TMutNode<mFvtxResponsePar>::new_node( _fvtx_node, "mFvtxResponsePar" );
  mFvtxResponse_par->set_verbosity(verblevel);
  //if (do_response() ) mFvtxResponse_par->set_do_evaluation( true );
  mFvtxResponse_par->set_do_evaluation( false );
      
  if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) 
  {
    mFvtxSlowSim_par->print();
    mFvtxResponse_par->print();
  }
   
  return 0;
}

//________________________________________________________________________________________
int FvtxUnpackPisa::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();
      
  try {
  
    // read external vertex
    load_vertex_if_needed( top_node );
    
    // Fill fvtxhits table from PISA tree
    SvxGetGEA(top_node);
    _mFvtxSlowSim_mod.event(top_node);

    // response modules
    if(do_response() ) _mFvtxResponse_mod.event(top_node);
 
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  write_maps_if_needed();
  _timer.get()->stop();

  return 0;

}

//____________________________________________________________
int FvtxUnpackPisa::End(PHCompositeNode* top_node) 
{
  if( verbosity >= 1 ) 
  {
    PHMapManager::clear();
    PHMapManager::print_stat();
  }
  
  _timer.get()->print_stat();
  _mFvtxSlowSim_mod.print_summary();
  if( _run_response ) _mFvtxResponse_mod.print_summary();
  return 0;
}
