// $Id: MuonDisplay.cxx,v 1.1.1.1 2008/07/30 17:43:58 hpereira Exp $

/*!
	\file    MuonDisplay.cxx
  \ingroup supermodules 
	\brief   mutoo 2D event display supermodule. 
    Displays Mutr detectors, mc tracks, 
    tracks, stubs, mutr clusters, on user request. 
    Only front and side (octant) views are supported.
	  Also dumps all event maps on request.
  \author  Hugo Pereira
	\version $Revision: 1.1.1.1 $
	\date    $Date: 2008/07/30 17:43:58 $
*/

#include "MuonDisplay.h"

// MUTOO
// 
#include<PhMutooDisplay.h>
#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<TMuiMCHitMapO.h>
#include<TMuiRoadMapO.h>
#include<TMutHitMap.h>
#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutVtxMap.h>
#include<TMutEvalMap.h>
#include<TMCPrimaryMap.h>

// MUIOO
#include<TMuiEvalMap.h>

using namespace std;

MuonDisplay::MuonDisplay( const char* name ) : 
	SubsysReco( name ),
  _top_node(0),
  _display(0),
  _timer( PHTimeServer::get()->insert_new( name ) ),
  _init(false)
{}

int MuonDisplay::Init(PHCompositeNode *topNode)
{
  _init = setup_display();
  return 0;
}

int MuonDisplay::InitRun(PHCompositeNode *top_node)
{
  return 0;
}


int MuonDisplay::CreateNodeTree(PHCompositeNode *top_node)
{
  return 0;
}

int MuonDisplay::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();
  _top_node = top_node;
  _display->event(top_node);
  _timer.get()->stop();
  return 0;
}


int MuonDisplay::End(PHCompositeNode* top_node) 
{
  
  // dump timer for this supermodule
  _timer.get()->print_stat();
  
  return 0;
}

bool MuonDisplay::setup_display() {    
  _display = new PhMutooDisplay();
  _display->set_draw_tracks(true);
  _display->set_draw_stubs(false);
  return true;
}

void 
MuonDisplay::dump_mc_primary() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMCPrimaryMap* primary_map = TMutNode<TMCPrimaryMap>::find_node(_top_node, "TMCPrimaryMap");          
    primary_map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_mui_road() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node(_top_node, "TMuiRoadMapO");          
    road_map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_mui_eval() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMuiEvalMap* eval_map = TMutNode<TMuiEvalMap>::find_node(_top_node, "TMuiEvalMap");          
    eval_map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_mui_mchit() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMuiMCHitMapO* mchit_map = TMutNode<TMuiMCHitMapO>::find_node(_top_node, "TMuiMCHitMapO");          
    mchit_map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_hit() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(_top_node, "TMutHitMap");          
    hit_map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_clus() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutClusMap* map = TMutNode<TMutClusMap>::find_node(_top_node, "TMutClusMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_coord() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutCoordMap* map = TMutNode<TMutCoordMap>::find_node(_top_node, "TMutCoordMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_coord(ULong_t obj_key) {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutCoordMap* map = TMutNode<TMutCoordMap>::find_node(_top_node, "TMutCoordMap");          
    TMutCoordMap::const_iterator coord_iter = map->range();
    while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
      if(coord_ptr->get()->get_key().get_obj_key() == obj_key) coord_iter->get()->print();
    }
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}


void 
MuonDisplay::dump_gap_coord() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutGapCoordMap* map = TMutNode<TMutGapCoordMap>::find_node(_top_node, "TMutGapCoordMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_stub() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutStubMap* map = TMutNode<TMutStubMap>::find_node(_top_node, "TMutStubMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_mc_trk() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutMCTrkMap* map = TMutNode<TMutMCTrkMap>::find_node(_top_node, "TMutMCTrkMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_mc_hit() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutMCHitMap* map = TMutNode<TMutMCHitMap>::find_node(_top_node, "TMutMCHitMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_trk() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutTrkMap* map = TMutNode<TMutTrkMap>::find_node(_top_node, "TMutTrkMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_vtx() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutVtxMap* map = TMutNode<TMutVtxMap>::find_node(_top_node, "TMutVtxMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void 
MuonDisplay::dump_eval() {
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  try {
    TMutEvalMap* map = TMutNode<TMutEvalMap>::find_node(_top_node, "TMutEvalMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}


//____________________________________________________________
void MuonDisplay::draw_plane(UShort_t arm, UShort_t octant)
{
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  _display->paint_plane_view(arm,octant);
}

//____________________________________________________________
void MuonDisplay::draw_half_octant_bidim(UShort_t arm, UShort_t octant, UShort_t halfoctant, UShort_t station)
{
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  _display->paint_half_octant_bidim_view(arm, octant, halfoctant, station);
}

//____________________________________________________________
void MuonDisplay::draw_octant(UShort_t arm, int octant, int station)
{
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  _display->paint_octant_view(arm, octant, station);
}

//____________________________________________________________________
void MuonDisplay::draw_side(UShort_t arm, UShort_t octant, int station)
{
  if(!_init) return;
  if( !MuonDisplay::sanity_check() ) return;
  _display->paint_side_view(arm, octant, station);
}

//____________________________________________________________________
// internal method to see whether top_node has been set
bool MuonDisplay::sanity_check() {
  if( _top_node == NULL ) {
    MUTOO::PRINT( std::cout, "top_node does not exist!" );
    MUTOO::PRINT( std::cout, "MuonTrigFilter flag ABORT_EVENT is being used" );
    return 0;
  } else
    return 1;
}

//___________________________________
void MuonDisplay::help( const char* module_name )
{
  
  cout << "MuonDisplay Visualize Commands" << endl;
  cout << "--------------------------------" << endl;
  cout << module_name << "->draw_octant(arm,octant,station)" << endl;
  cout << module_name << "->draw_octant(arm,octant)" << endl;
  cout << module_name << "->draw_octant(arm)" << endl;
  cout << module_name << "->draw_side(arm,octant,station)" << endl;
  cout << module_name << "->draw_side(arm,octant)" << endl;
  cout << module_name << "->draw_plane(arm,octant)" << endl;
  cout << module_name << "->draw_half_octant_bidim(arm,octant,halfoctant,station)" << endl;
  cout << "--------------------------------------------------------------------------------" << endl;
  cout << "Dump Commands" << endl;
  cout << module_name << "->dump_hit()" << endl;
  cout << module_name << "->dump_trk()" << endl;
  cout << "etc...." << endl;
  cout << "--------------------------------------------------------------------------------" << endl;

  return;
}





