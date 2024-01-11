// $Id: RpcStationType2_v1.cxx,v 1.4 2006/02/17 19:48:12 dave Exp $

/*!
	\file RpcStationType2_v1.cxx
	\brief RPC Type2 Station geometry
	Type2 station is circular. Geometry implemented using x, y segmentation and x window.
	Creates RpcPadType2 according to segmentation.
	\author Hugo Pereira da costa
	\version $Revision: 1.4 $
	\date    $Date: 2006/02/17 19:48:12 $
*/

#include "RpcStationType2_v1.h"
//INCLUDECHECKER: Removed this line: #include "RpcPadType2.h"

#include <stdexcept>
#include <sstream>

using namespace std;

ClassImp( RpcStationType2_v1 )

//__________________________________________________________________
void RpcStationType2_v1::create_pads( void )
{
	clear_pads();
	
	_pad_x_size = (_x_max-_x_min)/_x_segmentation;
	_pad_y_size = (_y_max-_y_min)/_y_segmentation ;
	
	UShort_t n_pads( _y_segmentation*_x_segmentation );	            
	for( UShort_t pad_index = 0; pad_index < n_pads; pad_index++ )
	{
		
		// create new pad
		RpcPadType2 pad;
  		
  	// get y index from pad index	
  	UShort_t y_index( pad_index/_x_segmentation );
  	
  	// calculate pad y center
  	Float_t y( _y_min + _pad_y_size*(y_index+0.5) );
  	
  	// get x index from pad index
  	UShort_t x_index( pad_index % _x_segmentation );
  	
  	// calculate pad x center
  	Float_t x( _x_min + _pad_x_size*(x_index+0.5) );		

			pad.set_center( PHPoint( x, y, _z ) );
		
		// set x y dimensions
		pad.set_x_size( _pad_x_size );
		pad.set_y_size( _pad_y_size );

		_pads.push_back( pad );
				
	}

	ostringstream what;
	what << "RpcStationType2::create_pads - created " << _pads.size() << " pads";
	RPCGEOM::TRACE( what.str() );

}

//__________________________________________________________________
RpcPad* RpcStationType2_v1::get_pad( UShort_t index )
{
	
	if( !_pads.size() )
		throw runtime_error( "RpcStationType2_v1::get_pad - pads not created" );
	
	if( index >= _pads.size() )
		throw runtime_error( "RpcStationType2_v1::get_pad - invalid index" );
	
	return &_pads[index];
	
}

//__________________________________________________________________
UShort_t RpcStationType2_v1::get_pad_index( const Float_t& x, const Float_t& y ) const
{
	
	if( !_pads.size() )
		throw runtime_error( "RpcStationType2_v1::get_pad - pads not created" );
			
	// check x against station window
	if( x < _x_min || x >= _x_max ) 
	throw runtime_error( "RpcStationType2_v1::get_pad - x out of bounds" );
			
	// check x against station window
	if( y < _y_min || y >= _y_max ) 
	throw runtime_error( "RpcStationType2_v1::get_pad - y out of bounds" );
	
	// calculate x index
	UShort_t x_index( (UShort_t)( (x-_x_min)/_pad_x_size ) );
	
	// calculate y index
	UShort_t y_index( (UShort_t)( (y-_y_min)/_pad_y_size ) );
	
	// calculate pad index
	return y_index * _x_segmentation + x_index;
	
}
