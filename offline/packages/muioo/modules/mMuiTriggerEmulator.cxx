//////////////////////////////////////////////////////////////////
//
// Utility class: mMuiTriggerEmulator:
// Author: S.Kelly 
// Date: 
// Description: 
//              
//////////////////////////////////////////////////////////////////

// MUIOO/MUTOO headers
//
#include <mMuiTriggerEmulator.h>
#include <mMuiTriggerEmulatorPar.h>
//INCLUDECHECKER: Removed this line: #include <TMutNode.h>
//INCLUDECHECKER: Removed this line: #include <PHException.h>
#include <PHTimer.h>
#include <TMuiHitMapO.h>

// PHENIX headers
//

/*! \ingroup modules */

// STL/BOOST
//
//INCLUDECHECKER: Removed this line: #include <iostream>
#include <string>
#include <list>
#include <boost/array.hpp>

mMuiTriggerEmulator::mMuiTriggerEmulator() : _timer("mMuiTriggerEmulator")
{
  MUIOO::TRACE("initializing module mMuiTriggerEmulator");  

  for(int i=0; i<8;++i) _deep_mask.set(i);
  for(int i=0; i<6;++i) _shallow_mask.set(i);
}

// Event method.

PHBoolean mMuiTriggerEmulator::event(PHCompositeNode* top_node)
{

  _timer.restart(); 
  
  try { 

    // Reset IOC pointers
    //
    set_interface_ptrs(top_node);

    // Clear all lists, bitmasks and trigger booleans
    //
    clear_state_variables();

    // Populate quadrant lists
    //
    fill_quadrant_list();

    // Set plane bitset
    //
    set_plane_bitset();

    // Set trigger status
    //
    set_trigger_status();

  } catch(std::exception& e) {
    MUIOO::TRACE(e.what());
    return False;
  }  

  // Timer
  //
  _timer.print(); 
  return True;
}

/*! Reset IOC and external interface pointers */
void 
mMuiTriggerEmulator::set_interface_ptrs(PHCompositeNode* top_node){  
  // module runtime parameters
  //
  _mod_par = TMutNode<mMuiTriggerEmulatorPar>::find_node(top_node,"mMuiTriggerEmulatorPar");
  _hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");
} 

void
mMuiTriggerEmulator::clear_state_variables()
{
  _deep_deep = false;
  _deep_shallow = false;
  for(size_t i=0;i<_quad_hit_list.size();++i){
    _quad_hit_list[i].clear();
    _quad_bitset[i].reset();
  }
}

void
mMuiTriggerEmulator::fill_quadrant_list()
{
  // Loop over all hits in the MUID.  Store hit point in
  // lists by quadrant.  Note small panel hits contribute 
  // a hit to 2 quadrants.
  //
  TMuiHitMapO::const_iterator hit_iter = _hit_map->range();
  while(TMuiHitMapO::const_pointer hit_ptr = hit_iter.next()){
    UShort_t panel = hit_ptr->get()->get_panel();
    // plane 1 (small panel) contributes to quadrant 0 and 1
    // plane 4 (small panel) contributes to quadrant 2 and 3
    if(panel == 0 || panel == 1) {
      _quad_hit_list[0].push_back(*hit_ptr);
    }
    if(panel == 1 || panel == 2) {
      _quad_hit_list[1].push_back(*hit_ptr);
    }
    if(panel == 3 || panel == 4) {
      _quad_hit_list[2].push_back(*hit_ptr);
    }
    if(panel == 4 || panel == 5) {
      _quad_hit_list[3].push_back(*hit_ptr);
    }
  }
}


void
mMuiTriggerEmulator::set_plane_bitset()
{
  for(size_t quad=0;quad<_quad_hit_list.size();++quad) {
    // Loop over all hits in quadrant hit list
    //
    hit_list::iterator hit_iter = _quad_hit_list[quad].begin();
    for(;hit_iter!= _quad_hit_list[quad].begin();++hit_iter){
      // Set the plane bit
      //
      UShort_t plane = hit_iter->get()->get_plane();
      _quad_bitset[quad].set(plane);
    }
  }
}

void
mMuiTriggerEmulator::set_trigger_status()
{
  // The BLT deep deep trigger is 7/8 in the first 8 planes,
  // the BLT deep shallow trigger is 5/6 in the first 6 planes.
  //
  UShort_t ndeep=0, nshallow=0;
  for(size_t quad=0;quad<_quad_bitset.size();++quad) {
    // Deep is 7 of 8
    //
    if ((_quad_bitset[quad] & _deep_mask).count() >= 7) ++ndeep;
    // Shallow is 5 of 6
    // 
    if ((_quad_bitset[quad] & _shallow_mask).count() >= 5) ++nshallow;
  }
  if(ndeep>=2) _deep_deep = true;
  if(ndeep>=1 && nshallow>=1) _deep_shallow = true;
}





