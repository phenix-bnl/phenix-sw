// $Id: RpcPadType1.h,v 1.2 2005/09/15 15:42:33 hpereira Exp $

#ifndef _RpcPadType1_h_
#define _RpcPadType1_h_

/*!
	\file RpcPadType1.h
	\brief type1 RPC pad class. r,phi segmentation
	\author Hugo Pereira da costa
	\version $Revision: 1.2 $
	\date    $Date: 2005/09/15 15:42:33 $
*/
#include "RpcPad.h"
#include "RPCGEOM.h"
#include <PHPoint.h>

//! type1 RPC pad class. r,phi segmentation.
class RpcPadType1: public RpcPad
{
	
	public:
	
	//! constructor
	RpcPadType1( void ):
			_r_center(0),
			_r_size(0),
			_phi_center(0),
			_phi_size(0)
	{}
					
	//! destructor
	virtual ~RpcPadType1( void )
	{}
	
	//! center r
	Float_t get_r_center( void ) const
	{ return _r_center; }

	//! center r
	void set_r_center( const Float_t &r_center )
	{ _r_center = r_center; } 
	
	//! size r
	Float_t get_r_size( void ) const
	{ return _r_size; }

	//! size r
	void set_r_size( const Float_t &r_size )
	{ _r_size = r_size; } 
	
	//! r window
	RPCGEOM::Window get_r_window( void ) const
	{ 
		return RPCGEOM::Window( 
			_r_center - 0.5*_r_size,
			_r_center + 0.5*_r_size
		);
	}
	
	//! center phi
	Float_t get_phi_center( void ) const
	{ return _phi_center; }

	//! center phi
	void set_phi_center( const Float_t &phi_center )
	{ _phi_center = phi_center; } 
	
	//! size phi
	Float_t get_phi_size( void ) const
	{ return _phi_size; }

	//! size phi
	void set_phi_size( const Float_t &phi_size )
	{ _phi_size = phi_size; } 
	
	//! phi window
	RPCGEOM::Window get_phi_window( void ) const
	{ 
		return RPCGEOM::Window( 
			_phi_center - 0.5*_phi_size,
			_phi_center + 0.5*_phi_size
		);
	}
		
	protected:
	
	//! r center
	Float_t _r_center;
	
	//! _r_size
	Float_t _r_size;
	
	//! phi center
	Float_t _phi_center;
	
	//! _phi_size
	Float_t _phi_size;
	
	ClassDef( RpcPadType1, 1 )
			
};

#endif
