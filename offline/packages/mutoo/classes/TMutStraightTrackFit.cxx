// $Id: TMutStraightTrackFit.cxx,v 1.1 2007/08/20 16:38:33 hpereira Exp $

/*!
	\file TMutStraightTrackFit.h
	\brief general interface to analytic 2 dimensional straight line fit
	\author Hugo Pereira
	\version $Revision: 1.1 $
	\date $Date: 2007/08/20 16:38:33 $
*/

#include "TMutStraightTrackFit.h"

using namespace std;

//____________________________________________________________________________________
bool TMutStraightTrackFit::fit( void )
{
 
	// allocate needed matrices
	PHGslMatrix htgh( 4, 4 );
	PHGslMatrix htgm( 4, 1 );
	
	// loop over nodes, update matrices
	for( node_list::iterator node_iter = _nodes.begin(); node_iter != _nodes.end(); node_iter++ )
	{
		htgh += node_iter->get_h().transpose()*node_iter->get_g()*node_iter->get_h();
		htgm += node_iter->get_h().transpose()*node_iter->get_g()*node_iter->get_m();
	}
	
	// calculate state vector
	_state = htgh.invert()*htgm;
	
	// calculate gain matrix
	_gain = htgh;

	// calculate chisquare
	for( node_list::iterator node_iter = _nodes.begin(); node_iter != _nodes.end(); node_iter++ )
	{
		PHGslMatrix residual = node_iter->get_m();
		residual -= node_iter->get_h()*_state;
		_chisquare += ( residual.transpose()*node_iter->get_g()*residual )(0,0);
	}
	
	return true;
 
}
