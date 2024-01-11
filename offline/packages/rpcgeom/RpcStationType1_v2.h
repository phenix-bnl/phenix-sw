// $Id: RpcStationType1_v2.h,v 1.1 2005/10/04 21:25:42 aglenn Exp $

#ifndef _RpcStationType1_v2_h_
#define _RpcStationType1_v2_h_

/*!
	\file RpcStationType1_v2.h
	\brief RPC Type1 Station geometry
	Type1 station is circular. Geometry implemented using r, phi segmentation and r window.
	This version creates uniform steps in theta.
	Creates RpcPadType1 according to segmentation.
	\author Andrew Glenn
	\version $Revision: 1.1 $
	\date    $Date: 2005/10/04 21:25:42 $
*/

#include "RpcStationType1.h"
#include "RpcPadType1.h"
#include "RPCGEOM.h"

#include <vector>

//!	RPC Type1 Station geometry
/*!	
	RPC Type1 Station geometry
	Type1 station is circular. Geometry implemented using r, phi segmentation and r window.
*/
class RpcStationType1_v2: public RpcStationType1
{
	public: 
			
	//! constructor 
	RpcStationType1_v2( UShort_t arm = 0, UShort_t index = 0 ):
			_arm( arm ),
			_index( index ),
			_z( 0 ),
			_phi_segmentation( 0 ),
			_r_segmentation( 0 ),
			_r_min( 0 ),
			_r_max( 0 ),
			_pad_theta_size( 0 ),
			_pad_phi_size( 0 )
	{}
	
	//! destructor
	virtual	~RpcStationType1_v2(void)
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
		note: pads must be casted to RpcPadType1 or RpcPadType2
		to access geometrical informations
	*/
	virtual RpcPad* get_pad( UShort_t index );
	
	//! get pad index from (x,y) position
	virtual UShort_t get_pad_index( const Float_t& x, const Float_t& y ) const; 

	//! get number of pads
	virtual UShort_t get_n_pads( void ) const
	{ return _pads.size(); }
	
	//! create all pads. RpcPadType1 pads are created
	virtual void create_pads( void );
	
	//! clear pads
	virtual void clear_pads( void )
	{ _pads.clear(); }
		
	//! r window
	virtual RPCGEOM::Window get_r_window( void ) const
	{ return RPCGEOM::Window( _r_min, _r_max ); }
	
	//!@name specific geometry description
	//!@{

	//! phi segmentation
	UShort_t get_phi_segmentation( void ) const
	{ return _phi_segmentation; }

	//! phi segmentation
	void set_phi_segmentation( UShort_t phi_segmentation ) 
	{ _phi_segmentation = phi_segmentation; }
	
	//! r segmentation
	UShort_t get_r_segmentation( void ) const
	{ return _r_segmentation; }

	//! r segmentation
	void set_r_segmentation( UShort_t r_segmentation ) 
	{ _r_segmentation = r_segmentation; }

	//! phi segmentation
	void set_r_window( const RPCGEOM::Window& r_window ) 
	{ 
		_r_min = r_window.first; 
		_r_max = r_window.second; 
	}
	
	//@}
	
	//!@name dumper
	//@{
	
	//! print
	virtual void print( std::ostream& out = std::cout ) const
	{
		out << "RpcStationType1_v2::print - index: " << _index << " z: " << _z << std::endl;
		for( std::vector<RpcPadType1>::const_iterator iter = _pads.begin(); iter!=_pads.end(); iter++ )
		iter->print( out );
	}
	//@}
	
	protected:
			
	//! vector of pads
	std::vector<RpcPadType1> _pads;

	//! arm
	UShort_t _arm;
	
	//! index 
	UShort_t _index;
			
	//! station z
	Float_t _z;
	
	//! phi segmentation
	UShort_t _phi_segmentation;
	
	//! r segmentation
	UShort_t _r_segmentation;
	
	//! r window
	Float_t _r_min;
	
	//! r window
	Float_t _r_max;
	
	//! pad r size
	Float_t _pad_theta_size;
	
	//! pad phi size
	Float_t _pad_phi_size;

	ClassDef( RpcStationType1_v2, 1 )
			
};

#endif
