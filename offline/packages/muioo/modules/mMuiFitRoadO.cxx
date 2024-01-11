// $Id: mMuiFitRoadO.cxx,v 1.1 2006/04/22 01:59:14 hpereira Exp $

/*!
	\file		mMuiFitRoadO.cxx
	\brief	 road straight track fit
	\author	Hugo Pereira
	\version $Revision: 1.1 $
	\date		$Date: 2006/04/22 01:59:14 $
*/

#include <TMuiGeo.h>

#include "mMuiFitRoadO.h"
#include "mMuiFitRoadOPar.h"

#include <string>
using namespace std;

//__________________________________________________________
mMuiFitRoadO::mMuiFitRoadO( void ):
	_timer(PHTimeServer::get()->insert_new("mMuiFitRoadO") )
{

	MUIOO::TRACE("initializing module mMuiFitRoadO");			

}


//__________________________________________________________
PHBoolean mMuiFitRoadO::event( PHCompositeNode *top_node )
{

	_timer.get()->restart(); 
	
	try { 
		
		// Reset IOC pointers
		set_interface_ptrs(top_node);	
							
		// loop over roads
		road_loop();
										 
	} catch(exception& e) {
		MUIOO::TRACE(e.what());
		return False;
	}	
	
	_timer.get()->stop();
	return True;

}

//__________________________________________________________
void mMuiFitRoadO::set_interface_ptrs( PHCompositeNode *top_node )
{

	// module parameters
	_mod_par = TMutNode< mMuiFitRoadOPar >::find_node( top_node, "mMuiFitRoadOPar" );
	
	// load road map
	_road_map = TMutNode< TMuiRoadMapO >::find_node( top_node, "TMuiRoadMapO" );

}

//__________________________________________________________
void mMuiFitRoadO::road_loop( void )
{

	// loop over roads
	TMuiRoadMapO::iterator road_iter = _road_map->range();
	while( TMuiRoadMapO::pointer road_ptr = road_iter.next() ) 
	{
		
		// allocate new RoadFitObject
		RoadFit road_fit;
		road_fit.set_z( 0 );
		
		// retrieve associated clusters
		TMuiClusterMapO::key_iterator clus_iter( road_ptr->get()->get_associated<TMuiClusterO>() );
		while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
		road_fit.add_cluster( clus_ptr );
		
		if( !road_fit.fit() ) {
			if( _mod_par->get_verbosity() >= MUIOO::SOME )
			cout << "mMuiFitRoadO::road_loop - fit failed" << endl;
			continue;
		}
		
		// fill road with fit results
		fill_road( road_ptr, road_fit );
				
	}

}

//_________________________________________________________________________________________________
void mMuiFitRoadO::fill_road( TMuiRoadMapO::pointer road_ptr, const mMuiFitRoadO::RoadFit& road_fit ) const
{
	// local fit parameters object 
	TMutFitPar fit_par( 
		road_fit.get_state()(0,0),
		road_fit.get_state()(1,0),
		road_fit.get_z(),
		road_fit.get_state()(2,0),
		road_fit.get_state()(3,0),	
		road_fit.get_chisquare()/road_fit.get_ndf() );

	// get covariance matrix
	PHGslMatrix cov( road_fit.get_gain() );
	cov.invert();		
	for( int i=0; i<4; i++ )
	for( int j=0; j<4; j++ )
	fit_par.set_covar( i, j, cov(i,j) );

	road_ptr->get()->set_fit_par( fit_par );

}

//________________________________________________________
mMuiFitRoadO::RoadFit::RoadFit( void ):
	_chisquare( 0 ),
	_state( 4, 1 ),
	_gain( 4, 4 ),
	_z( 0 )
{}

//________________________________________________________
mMuiFitRoadO::Node::Node( TMuiClusterMapO::pointer clus_ptr, const double &z ):
	_m( 1, 1 ),
	_g( 1, 1 ),
	_h( 1, 4 )
{
	
	// coordinate
	_m( 0, 0 ) = clus_ptr->get()->get_w_absolute();
	_g( 0, 0 ) = 1/MUIOO::SQUARE( clus_ptr->get()->get_error() );
			
	//! transfer matrix
	double angle( TMuiGeo::get_panel_angle( clus_ptr->get()->get_location() ) );
	_h(0,0) = -sin( angle );
	_h(0,1) = cos( angle );
	_h(0,2) = -sin( angle )*(clus_ptr->get()->get_mean_z() - z);
	_h(0,3) = cos( angle )*(clus_ptr->get()->get_mean_z() - z);
	
	
}

//_________________________________________________________________
bool mMuiFitRoadO::RoadFit::fit( void )
{

	// allocate needed matrices
	PHGslMatrix htgh( 4, 4 );
	PHGslMatrix htgm( 4, 1 );
	
	// loop over nodes, update matrices
	for( list< Node >::iterator node_iter = _nodes.begin(); node_iter != _nodes.end(); node_iter++ )
	{
		htgh += node_iter->_h.transpose()*node_iter->_g*node_iter->_h;
		htgm += node_iter->_h.transpose()*node_iter->_g*node_iter->_m;
	}
	
	// calculate state vector
	_state = htgh.invert()*htgm;
	
	// calculate gain matrix
	_gain = htgh;

	// calculate chisquare
	for( list< Node >::iterator node_iter = _nodes.begin(); node_iter != _nodes.end(); node_iter++ )
	{
		PHGslMatrix residual = node_iter->_m;
		residual -= node_iter->_h*_state;
		_chisquare += ( residual.transpose()*node_iter->_g*residual )(0,0);
	}
	
	return true;
}
