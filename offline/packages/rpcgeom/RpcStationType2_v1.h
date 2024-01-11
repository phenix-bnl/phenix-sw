// $Id: RpcStationType2_v1.h,v 1.3 2005/09/15 18:57:59 hpereira Exp $

#ifndef _RpcStationType2_v1_h_
#define _RpcStationType2_v1_h_

/*!
	\file RpcStationType2_v1.h
	\brief RPC Type2 Station geometry
	Type2 station is rectangular. Geometry implemented using x,y segmentation and x window.
	Creates RpcPadType2 according to segmentation.
	\author Hugo Pereira da costa
	\version $Revision: 1.3 $
	\date    $Date: 2005/09/15 18:57:59 $
*/
#include <vector>

#include "RpcStationType2.h"
#include "RpcPadType2.h"
#include "RPCGEOM.h"

//!	RPC Type2 Station geometry
/*!	
	RPC Type2 Station geometry
	Type2 station is rectangular. Geometry implemented using x,y segmentation and x window.
*/
class RpcStationType2_v1: public RpcStationType2
{
	public: 
			
	//! constructor 
	RpcStationType2_v1( UShort_t arm = 0, UShort_t index = 0 ):
			_arm( arm ),
			_index( index ),
			_y_segmentation( 0 ),
			_x_segmentation( 0 ),
			_x_min( 0 ),
			_x_max( 0 ),
			_y_min( 0 ),
			_y_max( 0 ),
			_pad_x_size( 0 ),
			_pad_y_size( 0 )
	{}
	
	//! destructor
	virtual	~RpcStationType2_v1(void)
	{ clear_pads(); }
	
	//! @name locator
	//@{
	
	//! arm
	virtual UShort_t get_arm( void ) const 
	{ return _arm; }
	
	//! arm
	virtual void set_arm( UShort_t arm ) 
	{ _arm = arm; }
	
	//! index
	virtual UShort_t get_index( void ) const
	{ return _index; }
	
	//! index 
	virtual void set_index( UShort_t index )
	{ _index = index; }
	
	//@}

	//! @name accessors
	//@{
	
	//! z position
	virtual Float_t get_z( void ) const
	{ return _z; }
	
	//! z position
	virtual void set_z( Float_t z )
	{ _z = z; }

	//! get pad from index
	/* 
		get pad from index
		note: pads must be casted to RpcPadType2 or RpcPadType2
		to access geometrical informations
	*/
	virtual RpcPad* get_pad( UShort_t index );
	
	//! get pad index from (x,y) position
	virtual UShort_t get_pad_index( const Float_t& x, const Float_t& y ) const; 

	//! get number of pads
	virtual UShort_t get_n_pads( void ) const
	{ return _pads.size(); }
	
	//! create all pads. RpcPadType2 pads are created
	virtual void create_pads( void );
	
	//! clear pads
	virtual void clear_pads( void )
	{ _pads.clear(); }
		
	//! x winwow
	RPCGEOM::Window get_x_window( void ) const
	{ return RPCGEOM::Window( _x_min, _x_max ); }
	
	//! y size
	RPCGEOM::Window get_y_window( void ) const
	{ return RPCGEOM::Window( _y_min, _y_max ); }
	
	//@}
	
	//!@name specific geometry description
	//!@{

	//! y segmentation
	UShort_t get_y_segmentation( void ) const
	{ return _y_segmentation; }

	//! y segmentation
	void set_y_segmentation( UShort_t y_segmentation ) 
	{ _y_segmentation = y_segmentation; }
	
	//! x segmentation
	UShort_t get_x_segmentation( void ) const
	{ return _x_segmentation; }

	//! x segmentation
	void set_x_segmentation( UShort_t x_segmentation ) 
	{ _x_segmentation = x_segmentation; }
		
	//! x size
	void set_x_window( const RPCGEOM::Window& x_window )
	{ 
		_x_min = x_window.first; 
		_x_max = x_window.second; 
	}
		
	//! y size
	void set_y_window( const RPCGEOM::Window& y_window )
	{ 
		_y_min = y_window.first; 
		_y_max = y_window.second; 
	}
		
	//@}
	
	
	//!@name dumper
	//@{
	
	//! print
	virtual void print( std::ostream& out = std::cout ) const
	{
		out << "RpcStationType2_v1::print - index: " << _index << " z: " << _z << std::endl;
		for( std::vector<RpcPadType2>::const_iterator iter = _pads.begin(); iter!=_pads.end(); iter++ )
		iter->print( out );
	}
	//@}
				
	protected:
	
	//! arm
	UShort_t _arm;
	
	//! index 
	UShort_t _index;
			
	//! station z
	Float_t _z;
			
	//! vector of pads
	std::vector<RpcPadType2> _pads;
	
	//! y segmentation
	UShort_t _y_segmentation;
	
	//! x segmentation
	UShort_t _x_segmentation;
	
	//! x size
	Float_t _x_min;
	
	//! x size
	Float_t _x_max;
	
	//! y size
	Float_t _y_min;
	
	//! y size
	Float_t _y_max;
	
	//! pad x size
	Float_t _pad_x_size;
	
	//! pad y size
	Float_t _pad_y_size;
	
	ClassDef( RpcStationType2_v1, 1 )
			
};

#endif
