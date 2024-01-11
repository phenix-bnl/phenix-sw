// $Id: RpcPadType2.h,v 1.2 2005/09/15 15:42:33 hpereira Exp $

#ifndef _RpcPadType2_h_
#define _RpcPadType2_h_

/*!
	\file RpcPadType2.h
	\brief type2 RPC pad base class. X,Y segmentation
	\author Hugo Pereira da costa
	\version $Revision: 1.2 $
	\date    $Date: 2005/09/15 15:42:33 $
*/
#include "RpcPad.h"
#include "RPCGEOM.h"
#include <PHPoint.h>

//! type2 RPC pad class. X,Y segmentation.
class RpcPadType2: public RpcPad
{
	
	public:
	
	//! constructor
	RpcPadType2( void ):
			_x_size(0),
			_y_size(0)
	{}
					
	//! destructor
	virtual ~RpcPadType2( void )
	{}
		
	//! size x
	Float_t get_x_size( void ) const
	{ return _x_size; }

	//! size x
	void set_x_size( const Float_t &x_size )
	{ _x_size = x_size; } 
	
	//! x window
	RPCGEOM::Window get_x_window( void ) const
	{ 
		return RPCGEOM::Window( 
			_x - 0.5*_x_size,
			_x + 0.5*_x_size
		);
	}
	
	//! size y
	Float_t get_y_size( void ) const
	{ return _y_size; }

	//! size y
	void set_y_size( const Float_t &y_size )
	{ _y_size = y_size; } 
	
	//! y window
	RPCGEOM::Window get_y_window( void ) const
	{ 
		return RPCGEOM::Window( 
			_y - 0.5*_y_size,
			_y + 0.5*_y_size
		);
	}
		
	protected:
	
	//! _x_size
	Float_t _x_size;
	
	//! _y_size
	Float_t _y_size;
	
	ClassDef( RpcPadType2, 1 )
			
};

#endif
