// $Id: mRpcEmbed.cxx,v 1.4 2010/06/27 02:31:20 richi Exp $

/*!
  \file    mRpcEmbed.cxx
  \brief   rpc hit embedding module.
	Merge hits from signal map and background map, merge them into
	main map in RPCOO node, using clones.
  \author  H. Pereira Da Costa
  \version $Revision: 1.4 $
  \date    $Date: 2010/06/27 02:31:20 $
*/

// RPCOO headers
#include <mRpcEmbed.h>
#include <mRpcEmbedPar.h>
//INCLUDECHECKER: Removed this line: #include <TMutNode.h>
//INCLUDECHECKER: Removed this line: #include <PHException.h>
#include <RPCOO.h>

#include <string>

using namespace std;

//______________________________________________________
mRpcEmbed::mRpcEmbed() : _mod_par(0),
			 _signal_hit_map(0),
			 _backgr_hit_map(0),
			 _merged_hit_map(0),
			 _rpcoo_node(0),
			 _ioc_signal_node(0),
			 _ioc_backgr_node(0),
			 _n_non_overlap_hits(0),
			 _n_overlap_hits(0),
			 _n_tot_signal_hits(0),
			 _n_tot_backgr_hits(0),
			 _n_tot_non_overlap_hits(0),
			 _n_tot_overlap_hits(0),
			 _timer( PHTimeServer::get()->insert_new("mRpcEmbed"))
{
  RPCOO::TRACE("initializing module mRpcEmbed");
}

//______________________________________________________
PHBoolean mRpcEmbed::event(
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

		// embedding
    do_embedding();

		//! increment counters
		_n_tot_signal_hits += _signal_hit_map->size();
		_n_tot_backgr_hits += _backgr_hit_map->size();
		_n_tot_non_overlap_hits+=_n_non_overlap_hits;
		_n_tot_overlap_hits+=_n_overlap_hits;

  } catch(exception& e) {
    RPCOO::TRACE(e.what());
    return False;
  }

  // Timer
  _timer.get()->stop();
  if(_mod_par->get_verbosity() == RPCOO::SOME) _timer.get()->print();
  if(_mod_par->get_verbosity() == RPCOO::ALOT) {
    RPCOO::PRINT(cout,"mRpcEmbed Summary");
    cout << "signal hits: " << _signal_hit_map->size() << endl;
    cout << "background hits: " <<  _backgr_hit_map->size() << endl;
    cout << "non-overlap merged hits: " << _n_non_overlap_hits << endl;
    cout << "overlap merged hits: " << _n_overlap_hits << endl;
    RPCOO::PRINT(cout,"**");
  }
  return True;
}

//______________________________________________________
void mRpcEmbed::print_summary( ostream& out ) const
{
	RPCOO::PRINT(out,"mRpcEmbed::print_summary");
	out << "total signal hits: " << _n_tot_signal_hits << endl;
	out << "total background hits: " <<  _n_tot_backgr_hits << endl;
	out << "total non-overlap merged hits: " << _n_non_overlap_hits << endl;
	out << "total overlap merged hits: " << _n_overlap_hits << endl;
	RPCOO::PRINT(out,"**");
}

//______________________________________________________
void mRpcEmbed::set_node_ptrs(PHCompositeNode* top_node)
{
	PHNodeIterator nodeItr(top_node);

  // rpc working node
  if(!(_rpcoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RPCOO"))))
	throw runtime_error(DESCRIPTION("Cannot locate RPCOO node"));

}

//______________________________________________________
void mRpcEmbed::set_interface_ptrs(PHCompositeNode* top_node)
{
	// module runtime parameters
  _mod_par = TMutNode<mRpcEmbedPar>::find_node(top_node,"mRpcEmbedPar");

  // Get signal, background and merged containers from their nodes
  _merged_hit_map = TMutNode<TRpcHitMap>::find_node(_rpcoo_node,"TRpcHitMap");
  _signal_hit_map = TMutNode<TRpcHitMap>::find_node(_ioc_signal_node,"TRpcHitMap");
  _backgr_hit_map = TMutNode<TRpcHitMap>::find_node(_ioc_backgr_node,"TRpcHitMap");

}

//______________________________________________________
void mRpcEmbed::do_embedding()
{

  // All hits are non-overlap until proven otherwise
  _n_non_overlap_hits = _signal_hit_map->size() + _backgr_hit_map->size();
  _n_overlap_hits = 0;

  // Insert the signal in the merged map
  TRpcHitMap::iterator signal_iter = _signal_hit_map->range();
  while(TRpcHitMap::pointer signal_ptr = signal_iter.next())
	_merged_hit_map->insert_clone(signal_ptr);

  // Insert background if hit already exists do the merge
  TRpcHitMap::iterator backgr_iter = _backgr_hit_map->range();
  while(TRpcHitMap::pointer backgr_ptr = backgr_iter.next()){

    TRpcHitMap::iterator merged_iter = _merged_hit_map->get(backgr_ptr->get()->get_arm(),
							    backgr_ptr->get()->get_station(),
							    backgr_ptr->get()->get_octant(),
							    backgr_ptr->get()->get_half_octant(),
							    backgr_ptr->get()->get_rseg(),
							    backgr_ptr->get()->get_strip());

    if(merged_iter.count() == 0) _merged_hit_map->insert_clone(backgr_ptr);
    else {

      // Hit exist -- do the merge
      TRpcHitMap::pointer merged_ptr = merged_iter.current();
      merged_ptr->get()->set_q( merged_ptr->get()->get_q() + backgr_ptr->get()->get_q() );

			// for the error on the charge, we set the squared sum
			merged_ptr->get()->set_q_error( sqrt(
				RPCOO::SQUARE(merged_ptr->get()->get_q()) +
				RPCOO::SQUARE(backgr_ptr->get()->get_q()) ) );

			// take the average time
			merged_ptr->get()->set_t( 0.5*(merged_ptr->get()->get_t() + backgr_ptr->get()->get_t() ) );
			merged_ptr->get()->set_t_error( 0.5*sqrt(
				RPCOO::SQUARE(merged_ptr->get()->get_t_error()) +
				RPCOO::SQUARE(backgr_ptr->get()->get_t_error()) ) );

			_n_overlap_hits++;
      _n_non_overlap_hits--;

		}
  }
}
