// $Id: mFvtxEmbed.cxx,v 1.7 2011/02/08 23:12:24 youzy Exp $

/*!
  \file mFvtxEmbed.cxx
  \brief fvtx hit embedding module.
  Merge hits from signal map and background map, merge them into
  main map in FVTXOO node, using clones.
  \author X.R. Wang
  \version $Revision: 1.7 $
  \date $Date: 2011/02/08 23:12:24 $
*/

// FVTXOO headers
#include <mFvtxEmbed.h>
#include <mFvtxEmbedPar.h>
#include <TFvtxHitMap.h>
#include <FVTXOO.h>

#include <string>

using namespace std;

//______________________________________________________
mFvtxEmbed::mFvtxEmbed() : _mod_par(0),
   _signal_hit_map(0),
   _backgr_hit_map(0),
   _merged_hit_map(0),
   _fvtxoo_node(0),
   _ioc_signal_node(0),
   _ioc_backgr_node(0),
   _n_non_overlap_hits(0),
   _n_overlap_hits(0),
   _n_tot_signal_hits(0),
   _n_tot_backgr_hits(0),
   _n_tot_non_overlap_hits(0),
   _n_tot_overlap_hits(0),
   _timer( PHTimeServer::get()->insert_new("mFvtxEmbed"))
{ FVTXOO::TRACE("initializing module mFvtxEmbed"); }

//______________________________________________________
PHBoolean mFvtxEmbed::event(
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

    // embedding
    do_embedding();

    //! increment counters
    _n_tot_signal_hits += _signal_hit_map->size();
    _n_tot_backgr_hits += _backgr_hit_map->size();
    _n_tot_non_overlap_hits+=_n_non_overlap_hits;
    _n_tot_overlap_hits+=_n_overlap_hits;

  } catch(exception& e) {
    FVTXOO::TRACE(e.what());
    return False;
  }

  // Timer
  _timer.get()->stop();
  if(_mod_par->get_verbosity() == FVTXOO::SOME) _timer.get()->print();
  if(_mod_par->get_verbosity() == FVTXOO::ALOT) {
    FVTXOO::PRINT(cout,"mFvtxEmbed Summary");
    cout << "signal hits: " << _signal_hit_map->size() << endl;
    cout << "background hits: " <<  _backgr_hit_map->size() << endl;
    cout << "non-overlap merged hits: " << _n_non_overlap_hits << endl;
    cout << "overlap merged hits: " << _n_overlap_hits << endl;
    FVTXOO::PRINT(cout,"**");
  }
  return True;
}

//______________________________________________________
void mFvtxEmbed::print_summary( ostream& out ) const
{
  FVTXOO::PRINT(out,"mFvtxEmbed::print_summary");
  out << "total signal hits: " << _n_tot_signal_hits << endl;
  out << "total background hits: " <<  _n_tot_backgr_hits << endl;
  out << "total non-overlap merged hits: " << _n_non_overlap_hits << endl;
  out << "total overlap merged hits: " << _n_overlap_hits << endl;
  FVTXOO::PRINT(out,"**");
}

//______________________________________________________
void mFvtxEmbed::set_node_ptrs(PHCompositeNode* top_node)
{

  PHNodeIterator nodeItr(top_node);

  // FVTXOO node
  if(!(_fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"))))
  throw runtime_error(DESCRIPTION("Cannot locate FVTXOO node"));

}

//______________________________________________________
void mFvtxEmbed::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mFvtxEmbedPar>::find_node(top_node,"mFvtxEmbedPar");

  // Get signal, background and merged containers from their nodes
  _merged_hit_map = TMutNode<TFvtxHitMap>::find_node(_fvtxoo_node,"TFvtxHitMap");
  _signal_hit_map = TMutNode<TFvtxHitMap>::find_node(_ioc_signal_node,"TFvtxHitMap");
  _backgr_hit_map = TMutNode<TFvtxHitMap>::find_node(_ioc_backgr_node,"TFvtxHitMap");

}

//______________________________________________________
void mFvtxEmbed::do_embedding()
{

  // All hits are non-overlap until proven otherwise
  _n_non_overlap_hits = _signal_hit_map->size() + _backgr_hit_map->size();
  _n_overlap_hits = 0;

  // Insert the signal in the merged map
  TFvtxHitMap::iterator signal_iter = _signal_hit_map->range();
  while(TFvtxHitMap::pointer signal_ptr = signal_iter.next())
  _merged_hit_map->insert_clone(signal_ptr);

  // Insert background if hit already exists do the merge
  TFvtxHitMap::iterator backgr_iter = _backgr_hit_map->range();
  while(TFvtxHitMap::pointer backgr_ptr = backgr_iter.next()){

    TFvtxHitMap::iterator merged_iter = _merged_hit_map->get(backgr_ptr->get()->get_arm(),
                   backgr_ptr->get()->get_cage(),
                   backgr_ptr->get()->get_station(),
                   backgr_ptr->get()->get_sector(),
                   backgr_ptr->get()->get_column(),
                   backgr_ptr->get()->get_strip());


    if(merged_iter.count() == 0) _merged_hit_map->insert_clone(backgr_ptr);
    else {

      // Hit exist -- do the merge
      TFvtxHitMap::pointer merged_ptr = merged_iter.current();
      merged_ptr->get()->set_q( merged_ptr->get()->get_q() + backgr_ptr->get()->get_q() );

      // for the error on the charge, we set the squared sum
      //			merged_ptr->get()->set_q_error( sqrt(
      //			FVTXOO::SQUARE(merged_ptr->get()->get_q()) +
      //			FVTXOO::SQUARE(backgr_ptr->get()->get_q()) ) );

      _n_overlap_hits++;
      _n_non_overlap_hits--;

    }
  }
}
