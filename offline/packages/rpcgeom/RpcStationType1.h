// $Id: RpcStationType1.h,v 1.2 2005/09/15 15:42:34 hpereira Exp $

#ifndef _RpcStationType1_h_
#define _RpcStationType1_h_

/*!
	\file RpcStationType1.h
	\brief RPC Type1 Station geometry
	Type1 station is circular. Geometry implemented using r, phi segmentation and r window.
	Creates RpcPadType1 according to segmentation.
	\author Hugo Pereira da costa
	\version $Revision: 1.2 $
	\date    $Date: 2005/09/15 15:42:34 $
*/

#include "RpcStation.h"
#include "RPCGEOM.h"
#include <vector>

//!	RPC Type1 Station geometry
/*!	
	RPC Type1 Station geometry
	Type1 station is circular. Geometry implemented using r, phi segmentation and r window.
*/
class RpcStationType1: public RpcStation
{
	public: 
			
	//! constructor 
	RpcStationType1( void )
	{}
	
	//! destructor
	virtual	~RpcStationType1(void)
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
		note: pads must be casted to RpcPadType1 or RpcPadType2
		to access geometrical informations
	*/
	virtual RpcPad* get_pad( UShort_t index ) = 0;
	
	//! get pad index from (x,y) position
	virtual UShort_t get_pad_index( const Float_t& x, const Float_t& y ) const
	{ return 0; } 

	//! get total number of pads
	virtual UShort_t get_n_pads( void )
	{ return 0; }
	
	//! create all pads. RpcPadType2 pads are created
	virtual void create_pads( void )
	{}
	
	//! clear pads
	virtual void clear_pads( void )
	{}
		
	//! r window
	virtual RPCGEOM::Window get_r_window( void ) const
	{ return RPCGEOM::Window( 0,0 ); }
	
	//@}

	//!@name dumper
	//@{
	
	//! print
	virtual void print( std::ostream& out = std::cout ) const
	{}
	//@}
	
	protected:

	ClassDef( RpcStationType1, 1 )
			
};

#endif
