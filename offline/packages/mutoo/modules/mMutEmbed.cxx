// $Id: mMutEmbed.cxx,v 1.16 2011/07/14 04:26:08 richi Exp $

/*!
  \file mMutEmbed.cxx
  \brief mutr hit embedding module.
  Merge hits from signal map and background map, merge them into
  main map in MUTOO node, using clones.
  \author S. Kelly
  \version $Revision: 1.16 $
  \date $Date: 2011/07/14 04:26:08 $
*/

// MUTOO headers
#include "mMutEmbed.h"
#include "mMutEmbedPar.h"

#include <TMutNode.h>
#include <PHException.h>
#include <MUTOO.h>
#include <MutCalib.h>

#include <TMutHitMap.h>
#include <TMutMCHitMap.h>

#include <iostream>
#include <string>

using namespace std;

//______________________________________________________
mMutEmbed::mMutEmbed() : _mod_par(0),
   _signal_hit_map(0),
   _backgr_hit_map(0),
   _merged_hit_map(0),
   _mutoo_node(0),
   _ioc_signal_node(0),
   _ioc_backgr_node(0),
   _n_non_overlap_hits(0),
   _n_overlap_hits(0),
   _timer( PHTimeServer::get()->insert_new("mMutEmbed"))
{
  MUTOO::TRACE("initializing module mMutEmbed",MUTOO::ALOT);
}

//______________________________________________________
PHBoolean mMutEmbed::event(
  PHCompositeNode* signal_top_node,
  PHCompositeNode* background_top_node,
  PHCompositeNode* top_node
  )
{
  _timer.get()->restart();

  try 
  {

    // Set node pointers
    _ioc_signal_node = signal_top_node;
    _ioc_backgr_node = background_top_node;
    set_node_ptrs(top_node);

    // Reset IOC pointers
    set_interface_ptrs(top_node);
    _merged_hit_map->clear();
    do_embedding();

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  // Timer
  _timer.get()->stop();
  if(_mod_par->get_verbosity() == MUTOO::SOME) _timer.get()->print();
  return True;
}

//______________________________________________________
void mMutEmbed::print_summary( ostream& out ) const
{
    MUTOO::PRINT(out,"mMutEmbed::print_summary");
    out << "non-overlap merged hits: " << _n_non_overlap_hits << endl;
    out << "overlap merged hits: " << _n_overlap_hits << endl;
    MUTOO::PRINT(out,"**");
}

//______________________________________________________
void mMutEmbed::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMutEmbedPar>::find_node(top_node,"mMutEmbedPar");

  // Get signal, background and merged containers from their nodes
  _merged_hit_map = TMutNode<TMutHitMap>::find_node(_mutoo_node,"TMutHitMap");
  _signal_hit_map = TMutNode<TMutHitMap>::find_node(_ioc_signal_node,"TMutHitMap");
  _backgr_hit_map = TMutNode<TMutHitMap>::find_node(_ioc_backgr_node,"TMutHitMap");

}

//______________________________________________________
void mMutEmbed::set_node_ptrs(PHCompositeNode* top_node)
{
  PHNodeIterator nodeItr(top_node);

  // Mutoo node
  _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_mutoo_node) throw runtime_error(DESCRIPTION("Cannot locate MUTOO node"));

}

//______________________________________________________
void mMutEmbed::do_embedding()
{
  float pedestal;

  // All hits are non-overlap until proven otherwise
  _n_non_overlap_hits += _signal_hit_map->size() + _backgr_hit_map->size();

  // Insert the signal in the merged map
  TMutHitMap::iterator signal_iter = _signal_hit_map->range();
  while(TMutHitMap::pointer signal_ptr = signal_iter.next())
  { _merged_hit_map->insert_clone(signal_ptr); }

  // Insert background if hit already exists do the merge
  TMutHitMap::iterator backgr_iter = _backgr_hit_map->range();
  while(TMutHitMap::pointer backgr_ptr = backgr_iter.next())
  {

    TMutHitMap::iterator merged_iter = _merged_hit_map->get(backgr_ptr->get()->get_arm(),
      backgr_ptr->get()->get_station(),
      backgr_ptr->get()->get_octant(),
      backgr_ptr->get()->get_half_octant(),
      backgr_ptr->get()->get_gap(),
      backgr_ptr->get()->get_cathode(),
      backgr_ptr->get()->get_strip());

    MutCalibStrip *calib = MutCalib();
    const PdbMutCalibStrip *dbstrip = calib->getPdbMutCalibStrip(backgr_ptr->get()->get_arm(),
      backgr_ptr->get()->get_station(),
      backgr_ptr->get()->get_octant(),
      backgr_ptr->get()->get_half_octant(),
      backgr_ptr->get()->get_gap(),
      backgr_ptr->get()->get_cathode(),
      backgr_ptr->get()->get_strip());

    if (dbstrip) pedestal = dbstrip->get_pedestal();
    else pedestal = 0;

    if( merged_iter.count() == 0 ) { _merged_hit_map->insert_clone(backgr_ptr); }
    else {

      // Hit exists -- do the merge
      TMutHitMap::pointer merged_ptr = merged_iter.current();
      double q_sum = merged_ptr->get()->get_q() + backgr_ptr->get()->get_q();
      merged_ptr->get()->set_q(q_sum);

      // What saturation value to use ?
      // Note:	need to take pedestal into account when summing adc values, i.e.
      // need to calculate pedestal - ((pedestal-adc1) + (pedestal-adc2))
      for(int i=0; i<4;++i) {
        double adc_sum = merged_ptr->get()->get_adc(i) + backgr_ptr->get()->get_adc(i) - pedestal;
        merged_ptr->get()->set_adc(i, static_cast<unsigned int>( adc_sum ) );
      }

      // propagate associations to MC hits from background hit to merge hit
      TMutMCHitMap::key_iterator mc_hit_iter = backgr_ptr->get()->get_associated<TMutMCHit>();
      while( TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next() )
      { PHKey::associate( mc_hit_ptr, merged_ptr ); }

      // increment counters
      _n_overlap_hits++;
      _n_non_overlap_hits--;
    }
  }

}
