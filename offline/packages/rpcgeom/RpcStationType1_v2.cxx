// $Id: RpcStationType1_v2.cxx,v 1.1 2005/10/04 21:25:42 aglenn Exp $

/*!
	\file RpcStationType1_v2.cxx
	\brief RPC Type1 Station geometry
	Type1 station is circular. Geometry implemented using r, phi segmentation and r window.
	This version creates uniform steps in theta.
	Creates RpcPadType1 according to segmentation.
	\author Andrew Glenn
	\version $Revision: 1.1 $
	\date    $Date: 2005/10/04 21:25:42 $
*/

#include "RpcStationType1_v2.h"
#include "RpcPadType1.h"

#include <stdexcept>
#include <sstream>

using namespace std;

ClassImp( RpcStationType1_v2 )

//__________________________________________________________________
void RpcStationType1_v2::create_pads( void )
{
	clear_pads();
	
	_pad_theta_size = fabs(atan2(_r_max,_z) - atan2(_r_min,_z))/_r_segmentation;
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
	    Float_t r_out(fabs(_z)*tan(atan(_r_min/fabs(_z)) + _pad_theta_size*(r_index+1.0)));
	    Float_t r_in(fabs(_z)*tan(atan(_r_min/fabs(_z)) + _pad_theta_size*r_index));
//	    Float_t r( fabs(_z)*tan(atan(_r_min/fabs(_z)) + _pad_theta_size*(r_index+0.5)));
	    Float_t r((r_out + r_in)/2.0);
	    
	    // calculate pad center
	    Float_t x = r*cos( phi );
	    Float_t y = r*sin( phi );
	    pad.set_center( PHPoint( x, y, _z ) );
	    
	    // set r phi dimensions
	    pad.set_r_center( r );
	    pad.set_r_size( r_out - r_in );
	    pad.set_phi_center( phi );
	    pad.set_phi_size( _pad_phi_size );
	    
	    _pads.push_back( pad );
	    if (phi_index == -1) cout<<"---AMG created pad with r="<<r
				    <<" delR="<<r_out-r_in
				    <<" phi="<<phi
				    <<" delPhi="<<_pad_phi_size*180.0/M_PI
				    <<" index="<<pad_index
				    <<" returned index="<<get_pad_index(x,y)
				    <<endl;
	  }

	ostringstream what;
	what << "RpcStationType1_v2::create_pads - created " << _pads.size() << " pads";
	RPCGEOM::TRACE( what.str() );
	
}

//__________________________________________________________________
RpcPad* RpcStationType1_v2::get_pad( UShort_t index )
{
	
	if( !_pads.size() )
		throw runtime_error( "RpcStationType1_v2::get_pad - pads not created" );
	
	if( index >= _pads.size() )
		throw runtime_error( "RpcStationType1_v2::get_pad - invalid index" );
	
	return &_pads[index];
	
}

//__________________________________________________________________
UShort_t RpcStationType1_v2::get_pad_index( const Float_t& x, const Float_t& y ) const
{
	
	if( !_pads.size() )
		throw runtime_error( "RpcStationType1_v2::get_pad - pads not created" );
	
	Float_t phi( atan2( y, x ) );
	Float_t r( sqrt( RPCGEOM::SQUARE( x ) + RPCGEOM::SQUARE( y ) ) );
	Float_t theta(atan(r/fabs(_z)));


	// make sure phi is in [0,2pi[
	while( phi >= 2*M_PI ) phi -= 2*M_PI;
	while( phi <0 ) phi += 2*M_PI;
		
	// check r against station window
	if( r < _r_min || r >= _r_max ) 
	{
		ostringstream what;
		what << "RpcStationType1_v2::get_pad - r out of bounds. Station=" << _index << " r=" << r;
		throw runtime_error( what.str() );
	}
	
	// calculate r index
	UShort_t r_index( (UShort_t)(( theta-atan(_r_min/fabs(_z))) /_pad_theta_size ));
	
	// calculate phi index
	UShort_t phi_index( (UShort_t)( phi/_pad_phi_size ) );
	
	// calculate pad index
	return phi_index * _r_segmentation + r_index;
	
}
