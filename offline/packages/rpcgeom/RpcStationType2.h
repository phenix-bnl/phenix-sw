// $Id: RpcStationType2.h,v 1.2 2005/09/15 15:42:35 hpereira Exp $

#ifndef _RpcStationType2_h_
#define _RpcStationType2_h_

/*!
	\file RpcStationType2.h
	\brief RPC Type2 Station geometry
	Type2 station is rectangular. Geometry implemented using x,y segmentation and x window.
	Creates RpcPadType2 according to segmentation.
	\author Hugo Pereira da costa
	\version $Revision: 1.2 $
	\date    $Date: 2005/09/15 15:42:35 $
*/
#include <vector>

#include "RpcStation.h"
#include "RpcPadType2.h"
#include "RPCGEOM.h"

//!	RPC Type2 Station geometry
/*!	
	RPC Type2 Station geometry
	Type2 station is rectangular. Geometry implemented using x,y segmentation and x window.
*/
class RpcStationType2: public RpcStation
{
	public: 
			
	//! constructor 
	RpcStationType2( void )
	{}
	
	//! destructor
	virtual	~RpcStationType2(void)
	{}

	//! @name locator
	//@{
	
	//! arm
	virtual UShort_t get_arm( void ) const 
	{ return 0; }
	
	//! arm
	virtual void set_arm( UShort_t arm ) 
	{}
	
	//! index
	virtual UShort_t get_index( void ) const
	{ return 0; }
	
	//! index 
	virtual void set_index( UShort_t index )
	{}
	
	//@}
	
	//! @name accessors
	//@{
	
	//! z position
	virtual Float_t get_z( void ) const
	{ return 0; }
	
	//! z position
	virtual void set_z( Float_t z )
	{}
	
	//! get pad from index
	/* 
		get pad from index
		note: pads must be casted to RpcPadType2 or RpcPadType2
		to access geometrical informations
	*/
	virtual RpcPad* get_pad( UShort_t index ) = 0;
	
	//! get pad index from (x,y) position
	virtual UShort_t get_pad_index( const Float_t& x, const Float_t& y ) const
	{ return 0; } 

	//! get number of pads
	virtual UShort_t get_n_pads( void ) const
	{ return 0; }
	
	//! create all pads. RpcPadType2 pads are created
	virtual void create_pads( void )
	{}
	
	//! clear pads
	virtual void clear_pads( void )
	{}
		
	//! x winwow
	virtual RPCGEOM::Window get_x_window( void ) const
	{ return RPCGEOM::Window(0,0); }
		
	//! y winwow
	virtual RPCGEOM::Window get_y_window( void ) const
	{ return RPCGEOM::Window(0,0); }
	
	//@}

	//!@name dumper
	//@{
	
	//! print
	virtual void print( std::ostream& out = std::cout ) const
	{}
	
	//@}
					
	protected:
	
	ClassDef( RpcStationType2, 1 )
			
};

#endif
