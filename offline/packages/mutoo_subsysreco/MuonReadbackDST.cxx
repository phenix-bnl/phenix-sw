// $Id: MuonReadbackDST.cxx,v 1.50 2013/01/07 22:58:43 abhisek Exp $
#include<Event.h>
#include<PHMapManager.h>
#include<PHTimeServer.h>
#include<recoConsts.h>

#include "MuonReadbackDST.h"
#include "MuonUtil.h"

using namespace std;

//______________________________________________________
MuonReadbackDST::MuonReadbackDST( const char* name ):
  MuonSubsysReco( name ),
  _timer(PHTimeServer::get()->insert_new(name) ),
  _do_dbinit( false )
{ return; }

//______________________________________________________
int MuonReadbackDST::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT(cout, "MuonReadbackDST::InitRun");

  // Create Node Tree
  CreateNodeTree(top_node);

  // initialize the database, if necessary
  if( _do_dbinit ) MuonUtil::initialize_database( top_node );
  MUTOO::PRINT(cout, "**");

  return 0;
}


//______________________________________________________
int MuonReadbackDST::CreateNodeTree(PHCompositeNode *top_node)
{
  try {

    // Instantiate nodes for mutoo containers
    {
      PHNodeIterator nodeItr(top_node);
      _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
      if(!_mutoo_node){
        _mutoo_node = new PHCompositeNode("MUTOO");
        top_node->addNode(_mutoo_node);
      }
    }

    // Instantiate nodes for muioo containers
    {
      PHNodeIterator nodeItr(top_node);
      _muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
      if(!_muioo_node){
        _muioo_node = new PHCompositeNode("MUIOO");
        top_node->addNode(_muioo_node);
      }
    }

    {
      PHNodeIterator nodeItr(top_node);
      _dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
      if (!_dst_node) {
        // Error condition -- need input DST node
        cerr << "MuonReadbackDST::CreateNodeTree - could not find DST node.\n";
      }
    }


    // Adding nodes for rpcoo containers
    // These should be in RpcReadBack
  /*{
   PHNodeIterator nodeItr(top_node);
   _rpcoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode","RPCOO"));
   if(!_rpcoo_node )
   {
    _rpcoo_node = new PHCompositeNode("PRCOO");
    top_node->addNode(_rpcoo_node);
   }
  }*/
 


    // load all maps
    _load_map<TMCPrimaryMap>( _dst_node, _mutoo_node, "TMCPrimaryMap", "TMCPrimary");

    // muioo maps
    _load_map<TMuiMCHitMapO>( _dst_node, _muioo_node, "TMuiMCHitMapO", "TMuiMCHitO");
    _load_map<TMuiHitMapO>( _dst_node, _muioo_node, "TMuiHitMapO", "TMuiHitO");
    _load_map<TMuiClusterMapO>( _dst_node, _muioo_node, "TMuiClusterMapO", "TMuiClusterO");
    _load_map<TMui1DRoadMapO>( _dst_node, _muioo_node, "TMui1DRoadMapO", "TMui1DRoadO");
    _load_map<TMuiPseudoBLTMapO>(	 _dst_node, _muioo_node, "TMuiPseudoBLTMapO", "TMuiPseudoBLTO");
    _load_map<TMuiRoadMapO>( _dst_node, _muioo_node, "TMuiRoadMapO", "TMuiRoadO");

    // load pseudoLL1 map.
    /*
      it must be handled with special care because it was first added with a
      "wrong" name to the node tree. The following implementation is able to read
      both the old "wrong" name and the correct "new" one. Should be switched back
      to the "standard" implementation in the future, when sure that no files are
      used with the old wrong name
    */
    try {

      // try load from TMuiPseudoLL1 node
      TMutNode<TMuiPseudoLL1Map>::new_dst_input_node( _muioo_node, "TMuiPseudoLL1Map", _dst_node, "TMuiPseudoLL1" );
      cout << "MuonReadbackDST - loading LL1 from TMuiPseudoLL1 node" << endl;

    } catch( std::exception &e ) {
      try {

        // try load from TMuiPseudoLL1Map node
        /* this is non-standard and was due to a bug in the Run5 production macro */
        TMutNode<TMuiPseudoLL1Map>::new_dst_input_node( _muioo_node, "TMuiPseudoLL1Map", _dst_node, "TMuiPseudoLL1Map" );
        cout << "MuonReadbackDST - loading LL1 from TMuiPseudoLL1Map node" << endl;

      } catch( std::exception &e ) {

        // create empty map
        TMutNode<TMuiPseudoLL1Map>::new_node(_dst_node , "TMuiPseudoLL1Map" );

      }

    }

    // mutoo maps
    _load_map<TMutMCHitMap>( _dst_node, _mutoo_node, "TMutMCHitMap", "TMutMCHit");
    _load_map<TMutMCTrkMap>( _dst_node, _mutoo_node, "TMutMCTrkMap", "TMutMCTrk");
    _load_map<TMutHitMap>( _dst_node, _mutoo_node, "TMutHitMap", "TMutHit");
    _load_map<TMutClusMap>( _dst_node, _mutoo_node, "TMutClusMap", "TMutClus");
    _load_map<TMutCoordMap>( _dst_node, _mutoo_node, "TMutCoordMap", "TMutCoord");
    _load_map<TMutGapCoordMap>( _dst_node, _mutoo_node, "TMutGapCoordMap",	"TMutGapCoord");
    _load_map<TMutStubMap>( _dst_node, _mutoo_node, "TMutStubMap", "TMutStub");
    _load_map<TMutTrkMap>( _dst_node, _mutoo_node, "TMutTrkMap", "TMutTrk");
    _load_map<TMutVtxMap>( _dst_node, _mutoo_node, "TMutVtxMap", "TMutVtx");
    _load_map<TMutAlignParMap>( _dst_node, _mutoo_node, "TMutAlignParMap", "TMutAlignPar");

    // rpc maps -- moved to RpcReadBack
    //_load_map<TRpcHitMap>( _dst_node, _rpcoo_node, "TRpcHitMap", "TRpcHit");
    //_load_map<TRpcTrkMap>( _dst_node, _rpcoo_node, "TRpcTrkMap", "TRpcTrk");
    //_load_map<TRpcCoordMap>( _dst_node, _rpcoo_node, "TRpcCoordMap", "TRpcCoord");


    // Dump the node tree to cout
    top_node->print();

  } catch (exception& e) {
    MUTOO::TRACE(e.what());
  }
  return 0;
}

//______________________________________________________
int MuonReadbackDST::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();

  // load vertex
  load_vertex_if_needed( top_node );

  // check vertex matching
  // MuonUtil::check_vertex_matching( top_node );

  // read maps
  PHMapManager::read(top_node);

  write_maps_if_needed();
  _timer.get()->stop();
  return 0;
}

//______________________________________________________
int MuonReadbackDST::End(PHCompositeNode* top_node)
{

  // print this module timer statistics
//   _timer.get()->print_stat();

  return 0;
}
