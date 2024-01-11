// $Id: mRpcResponse.cxx,v 1.4 2008/08/28 00:53:56 kempel Exp $

/*!
	\file mRpcResponse.cxx
	\brief rpc response module. Generate TRpcHit from TMutMCTrk and TRpcMCHit
	\author H. Pereira Da Costa
	\version $Revision: 1.4 $
	\date $Date: 2008/08/28 00:53:56 $
*/

#include "mRpcResponse.h"
#include "RPCOO.h"
#include <iostream>

using namespace std;

//_____________________________________________
mRpcResponse::mRpcResponse() : 
	_rng(0),
	_timer( PHTimeServer::get()->insert_new("mRpcResponse") )
{
	RPCOO::TRACE("initializing module mRpcResponse");	
}

//_____________________________________________
PHBoolean mRpcResponse::event(PHCompositeNode* top_node)
{
	_timer.get()->restart();	 
	try { 
				
		// Reset IOC pointers
		set_interface_ptrs(top_node);
		_hit_map->clear();
		
		// Loop over TMutMCTrk and do the digitization
		response_loop();

	} catch(std::exception& e) {
		RPCOO::TRACE(e.what());
		return False;
	}	

	// If verbose dump the contents of trks and primary
	//
	_timer.get()->stop();
	if(_mod_par->get_verbosity() >= RPCOO::ALOT) 
	{
		_mc_hit_map->print();
		_hit_map->print();
	}
	
	return True;
}

//_____________________________________________
void mRpcResponse::response_loop()
{
	// get iterator over all tracks
	TRpcMCHitMap::iterator iter( _mc_hit_map->range() );
	while( TRpcMCHitMap::pointer ptr = iter.next() )
	{
		create_hit( ptr );
	}
}

//_____________________________________________
void mRpcResponse::create_hit( TRpcMCHitMap::pointer mc_hit_ptr )
{

	// put plane efficiency
	double eff = gsl_rng_uniform(_rng.get());
	if( eff >= _mod_par->get_plane_efficiency(
			mc_hit_ptr->get()->get_arm(),
			mc_hit_ptr->get()->get_station() ) ) return;
		
	// retrieve associated strip
	const TRpcMCHit::strip_list strips( mc_hit_ptr->get()->get_strip_list() );
	for( TRpcMCHit::strip_iterator strip_iter = strips.begin(); strip_iter != strips.end(); strip_iter++ )
	{
		
		// insert new hit
		TRpcHitMap::iterator hit_iter( _hit_map->insert_new( 
				mc_hit_ptr->get()->get_arm(),
				mc_hit_ptr->get()->get_station(),
				mc_hit_ptr->get()->get_octant(),
				mc_hit_ptr->get()->get_half_octant(),
				mc_hit_ptr->get()->get_rseg(),
				(*strip_iter)->get_stripid() ) );
		
		// fill charge
		hit_iter->get()->set_q( (*strip_iter)->get_q() );
		hit_iter->get()->set_q_error( 0 );
		
		// fill time
		hit_iter->get()->set_t( mc_hit_ptr->get()->get_tof() );
		hit_iter->get()->set_t_error( 0 );
		
		// associate hit and MC hit
		PHKey::associate( mc_hit_ptr, hit_iter.current() );
		
	}		
}

//______________________________________________________________________
void mRpcResponse::set_interface_ptrs(PHCompositeNode* top_node)
{	
	// module runtime parameters
	_mod_par = TMutNode<mRpcResponsePar>::find_node(top_node,"mRpcResponsePar");

	// IOC pointers
	_mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
	_mc_hit_map = TMutNode<TRpcMCHitMap>::find_node(top_node,"TRpcMCHitMap");
	_hit_map = TMutNode<TRpcHitMap>::find_node(top_node,"TRpcHitMap");
} 
