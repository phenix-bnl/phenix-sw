// $Id: FvtxReadbackDST.C,v 1.9 2016/04/04 13:38:58 slash Exp $

#include<Event.h>
#include<PHMapManager.h>
#include<recoConsts.h>

#include <FvtxReadbackDST.h>
#include <FVTXOO.h>
#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxStraightTrkParMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxCompactCoordMap.h>
#include <TFvtxClusMap.h>
#include <TFvtxTrkMap.h>
#include <TFvtxCompactTrkMap.h>

FvtxReadbackDST::FvtxReadbackDST() : 
  MuonSubsysReco( "FVTXREADBACKDST" ),
  _timer( new PHTimer("FVTXREADBACKDST") )
{
  ThisName = "FVTXREADBACKDST";
  return ;
}

FvtxReadbackDST::~FvtxReadbackDST()
{
  if (_timer)
    {
      delete _timer;
      _timer = NULL;
    }
}

int
FvtxReadbackDST::InitRun(PHCompositeNode *top_node)
{
  FVTXOO::PRINT(std::cout, "FvtxReadbackDST::InitRun");	
  top_node->print();
  
  // Create Node Tree
  CreateNodeTree(top_node);
  
  return 0;
}

int
FvtxReadbackDST::CreateNodeTree(PHCompositeNode *top_node)
{
  try {
  
    // Instantiate nodes for fvtxoo containers
    {
      PHNodeIterator nodeItr(top_node);
      _fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
      if(!_fvtxoo_node){
        _fvtxoo_node = new PHCompositeNode("FVTXOO");
        top_node->addNode(_fvtxoo_node);
      }
    }
    
    {
      PHNodeIterator nodeItr(top_node);
      _dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
      if (!_dst_node) {
        // Error condition -- need input DST node
        std::cerr << "FvtxReadbackDST::CreateNodeTree - could not find DST node." << std::endl;;
      }
    }
    
    // load all maps
    //_load_map<TMCPrimaryMap>( _dst_node, _fvtxoo_node, "TMCPrimaryMap", "TMCPrimary");	
    
    // fvtxoo maps
//     std::vector<std::string> maps; // TODO: add a member addMap(std::string) to allow user to specify which maps
//     maps.push_back("TMutMCHit");
//     maps.push_back("TMutMCTrk");
//     maps.push_back("TFvtxMCHit");
//     maps.push_back("TFvtxClus");
//     maps.push_back("TFvtxCoord");
//     maps.push_back("TFvtxTrk");
//     maps.push_back("TMutTrk");

        { //load muon arm nodes
          PHNodeIterator nodeItr(top_node);
          PHCompositeNode* mut_node =
              static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode",
                  "MUTOO"));
          if (!mut_node)
            {
              // missing MUTOO node, try to build it
              // but suggest use MuonReadbackDST instead

              mut_node = new PHCompositeNode("MUTOO");
              top_node->addNode(mut_node);

              // Error condition -- need input DST node
              std::cout
                  << "FvtxReadbackDST::CreateNodeTree - building nodes for MuTr; best to use MuonReadbackDST."
                  << std::endl;
              _load_map<TMutMCHitMap>(_dst_node, mut_node, "TMutMCHitMap",
                  "TMutMCHit");
              _load_map<TMutTrkMap>(_dst_node, mut_node, "TMutTrkMap",
                  "TMutTrk");
              _load_map<TMutVtxMap>(_dst_node, mut_node, "TMutVtxMap",
                  "TMutVtx");

            }
        }


    _load_map<TFvtxMCHitMap>( _dst_node, _fvtxoo_node, "TFvtxMCHitMap", "TFvtxMCHit");	
    _load_map<TFvtxPisaHitMap>( _dst_node, _fvtxoo_node, "TFvtxPisaHitMap", "TFvtxPisaHit");	
    _load_map<TFvtxStraightTrkParMap>(_dst_node, _fvtxoo_node, "TFvtxStraightTrkParMap", "TFvtxStraightTrkParMap");
    _load_map<TFvtxHitMap>(   _dst_node, _fvtxoo_node, "TFvtxHitMap",   "TFvtxHit");	
    _load_map<TFvtxClusMap>(  _dst_node, _fvtxoo_node, "TFvtxClusMap",  "TFvtxClus");	
    _load_map<TFvtxCoordMap>( _dst_node, _fvtxoo_node, "TFvtxCoordMap", "TFvtxCoord");	
    _load_map<TFvtxCompactCoordMap>( _dst_node, _fvtxoo_node, "TFvtxCompactCoordMap", "TFvtxCompactCoord");	
    _load_map<TFvtxTrkMap>(   _dst_node, _fvtxoo_node, "TFvtxTrkMap",   "TFvtxTrk");	
    _load_map<TFvtxCompactTrkMap>(   _dst_node, _fvtxoo_node, "TFvtxCompactTrkMap",   "TFvtxCompactTrk");	
  
    // Dump the node tree to cout
    top_node->print();
    
  } catch (const std::exception& e) {
    FVTXOO::TRACE(e.what());
  }
  return 0;
}

int
FvtxReadbackDST::process_event(PHCompositeNode *top_node)
{
  _timer->restart();
  
  // load vertex
  load_vertex_if_needed( top_node );

  // read maps
  PHMapManager::read(top_node);		
  
  write_maps_if_needed();
  _timer->stop();
  return 0;
}

int
FvtxReadbackDST::End(PHCompositeNode* top_node) 
{
  
  // print this module timer statistics
  _timer->print_stat();	
  
  return 0;
}
