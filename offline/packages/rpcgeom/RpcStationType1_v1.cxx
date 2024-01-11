// $Id: RpcStationType1_v1.cxx,v 1.4 2007/06/20 19:05:25 phnxbld Exp $

/*!
	\file RpcStationType1_v1.cxx
	\brief RPC Type1 Station geometry
	Type1 station is circular. Geometry implemented using r, phi segmentation and r window.
	Creates RpcPadType1 according to segmentation.
	\author Hugo Pereira da costa
	\version $Revision: 1.4 $
	\date    $Date: 2007/06/20 19:05:25 $
*/

#include <RpcStationType1_v1.h>

#include <gsl/gsl_math.h>

#include <cmath>
#include <stdexcept>
#include <sstream>

using namespace std;

ClassImp( RpcStationType1_v1 )

//__________________________________________________________________
void RpcStationType1_v1::create_pads( void )
{
	clear_pads();
	
	_pad_r_size = (_r_max - _r_min)/_r_segmentation;
	_pad_phi_size = 2*M_PI/_phi_segmentation ;

	UShort_t n_pads( _phi_segmentation*_r_segmentation );	            
	for( UShort_t pad_index = 0; pad_index < n_pads; pad_index++ )
	{
		
		// create new pad
		RpcPadType1 pad;
  		
  	// get phi index from pad index	
  	UShort_t phi_index( pad_index/_r_segmentation );
  	
  	// calculate pad phi center
  	Float_t phi( _pad_phi_size*(phi_index+0.5) );
  	
  	// get r index from pad index
  	UShort_t r_index( pad_index % _r_segmentation );
  	
  	// calculate pad r center
  	Float_t r( _r_min + _pad_r_size*(r_index+0.5) );		
		
		// calculate pad center
		Float_t x = r*cos( phi );
		Float_t y = r*sin( phi );
		pad.set_center( PHPoint( x, y, _z ) );
		
		// set r phi dimensions
		pad.set_r_center( r );
		pad.set_r_size( _pad_r_size );
		pad.set_phi_center( phi );
		pad.set_phi_size( _pad_phi_size );

		_pads.push_back( pad );
				
	}

	ostringstream what;
	what << "RpcStationType1_v1::create_pads - created " << _pads.size() << " pads";
	RPCGEOM::TRACE( what.str() );
	
}

//__________________________________________________________________
RpcPad* RpcStationType1_v1::get_pad( UShort_t index )
{
	
	if( !_pads.size() )
		throw runtime_error( "RpcStationType1_v1::get_pad - pads not created" );
	
	if( index >= _pads.size() )
		throw runtime_error( "RpcStationType1_v1::get_pad - invalid index" );
	
	return &_pads[index];
	
}

//__________________________________________________________________
UShort_t RpcStationType1_v1::get_pad_index( const Float_t& x, const Float_t& y ) const
{
	
	if( !_pads.size() )
		throw runtime_error( "RpcStationType1_v1::get_pad - pads not created" );
	
	Float_t phi( atan2( y, x ) );
	Float_t r( sqrt( RPCGEOM::SQUARE( x ) + RPCGEOM::SQUARE( y ) ) );
	
	// make sure phi is in [0,2pi[
	while( phi >= 2*M_PI ) phi -= 2*M_PI;
	while( phi <0 ) phi += 2*M_PI;
		
	// check r against station window
	if( r < _r_min || r >= _r_max ) 
	{
		ostringstream what;
		what << "RpcStationType1_v1::get_pad - r out of bounds. Station=" << _index << " r=" << r;
		throw runtime_error( what.str() );
	}
	
	// calculate r index
	UShort_t r_index( (UShort_t)( (r-_r_min)/_pad_r_size ) );
	
	// calculate phi index
	UShort_t phi_index( (UShort_t)( phi/_pad_phi_size ) );
	
	// calculate pad index
	return phi_index * _r_segmentation + r_index;
	
}
