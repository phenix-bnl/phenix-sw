// $Id: mMuiEmbed.cxx,v 1.2 2009/09/24 09:11:37 hpereira Exp $

/*!
  \file mMuiEmbed.cxx
  \brief muid hit embedding module.
  Merge hits from signal map and background map, merge them into
  main map in MUIOO node, using clones.
  \author S. Kelly
  \version $Revision: 1.2 $
  \date $Date: 2009/09/24 09:11:37 $
*/

// MUIOO headers
#include "mMuiEmbed.h"
#include "mMuiEmbedPar.h"

#include <TMutNode.h>
#include <PHException.h>
#include <MUIOO.h>

#include <TMuiHitMapO.h>
#include <TMuiMCHitMapO.h>

#include <iostream>
#include <string>

using namespace std;

//______________________________________________________
mMuiEmbed::mMuiEmbed() : _mod_par(0),
   _signal_hit_map(0),
   _backgr_hit_map(0),
   _merged_hit_map(0),
   _muioo_node(0),
   _ioc_signal_node(0),
   _ioc_backgr_node(0),
   _n_non_overlap_hits(0),
   _n_overlap_hits(0),
   _timer( PHTimeServer::get()->insert_new("mMuiEmbed"))
{
  MUIOO::TRACE("initializing module mMuiEmbed");
}

//______________________________________________________
PHBoolean mMuiEmbed::event(
  PHCompositeNode* signal_top_node,
  PHCompositeNode* background_top_node,
  PHCompositeNode* top_node
  )
{
  _timer.get()->restart();

  try {
    // Set node pointers
    _ioc_signal_node = signal_top_node;
    _ioc_backgr_node = background_top_node;
    set_node_ptrs(top_node);

    // Reset IOC pointers
    set_interface_ptrs(top_node);
    _merged_hit_map->clear();
    do_embedding();

  } catch(exception& e) {
    MUIOO::TRACE(e.what());
    return False;
  }

  // Timer
  //
  _timer.get()->stop();
  if(_mod_par->get_verbosity() == MUIOO::SOME) _timer.get()->print();

  return True;
}

//______________________________________________________
void mMuiEmbed::print_summary( ostream& out )
{
    MUIOO::PRINT(out,"mMuiEmbed::print_summary");
    out << "non-overlap merged hits: " << _n_non_overlap_hits << endl;
    out << "overlap merged hits: " << _n_overlap_hits << endl;
    MUIOO::PRINT(out,"**");
}

//______________________________________________________
void mMuiEmbed::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMuiEmbedPar>::find_node(top_node,"mMuiEmbedPar");

  // Get signal, background and merged containers from their nodes
  _merged_hit_map = TMutNode<TMuiHitMapO>::find_node(_muioo_node,"TMuiHitMapO");
  _signal_hit_map = TMutNode<TMuiHitMapO>::find_node(_ioc_signal_node,"TMuiHitMapO");
  _backgr_hit_map = TMutNode<TMuiHitMapO>::find_node(_ioc_backgr_node,"TMuiHitMapO");

}

//______________________________________________________
void mMuiEmbed::set_node_ptrs(PHCompositeNode* top_node)
{
  PHNodeIterator nodeItr(top_node);

  // MUIOO node
  _muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
  if(!_muioo_node) throw runtime_error(DESCRIPTION("Cannot locate MUIOO node"));

}

//______________________________________________________
void mMuiEmbed::do_embedding()
{
  // All hits are non-overlap until proven otherwise
  _n_non_overlap_hits += _signal_hit_map->size() + _backgr_hit_map->size();

  // Insert the signal in the merged map
  TMuiHitMapO::iterator signal_iter = _signal_hit_map->range();
  while(TMuiHitMapO::pointer signal_ptr = signal_iter.next())
  { _merged_hit_map->insert_clone(signal_ptr); }

  // Insert background if hit already exists do the merge
  TMuiHitMapO::iterator backgr_iter = _backgr_hit_map->range();
  while(TMuiHitMapO::pointer backgr_ptr = backgr_iter.next())
  {

    TMuiHitMapO::iterator merged_iter = _merged_hit_map->get(backgr_ptr->get()->get_arm(),
                   backgr_ptr->get()->get_plane(),
                   backgr_ptr->get()->get_panel(),
                   backgr_ptr->get()->get_orientation(),
                   backgr_ptr->get()->get_twopack());
    if(merged_iter.count() == 0)
    {

      // No hit -- do the insert
      _merged_hit_map->insert_clone(backgr_ptr);

    } else {

      // Adjust the counters
      _n_overlap_hits++;
      _n_non_overlap_hits--;

      TMuiHitMapO::pointer merged_ptr = merged_iter.current();

      // propagate associations to TMuiMCHitO from background hit to merged hit
      TMuiMCHitMapO::key_iterator mc_iter = backgr_ptr->get()->get_associated<TMuiMCHitO>();
      while( TMuiMCHitMapO::pointer mc_ptr = mc_iter.next() )
      { PHKey::associate( mc_ptr, merged_ptr ); }

    }
  }
}
