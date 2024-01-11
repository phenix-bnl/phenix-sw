
#ifndef __RpcStation_H__
#define __RpcStation_H__

#include <TObject.h>
#include "RpcStrip.h"
#include "RPCGEOM.h"

#include <vector>

//!	RPC Station geometry
/*!	
*/

class RpcStrip;

class RpcStation: public TObject
{
	public: 
			
	//! constructor 
      RpcStation( UShort_t arm = 0, UShort_t index = 0 ):
			_arm( arm ),
			_index( index ),
			_z( 0 ),
			_R_segmentation( 0 )
	{}
	
	//! destructor
	virtual	~RpcStation(void)
	{ clear_strips(); }
	
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
	
	//! get strip from index

	virtual RpcStrip* get_strip( Int_t index );
	
	//! get pad index from (x,y) position
	virtual Int_t get_strip_index( const double& x, const double& y ) const; 

	//! get number of pads
	virtual UShort_t get_n_strips( void ) const
	{ return _strips.size(); }
	
	//! create all pads. RpcPadType1 pads are created
	virtual void create_strips( void );
	
	//! clear pads
	virtual void clear_strips( void )
	{ _strips.clear(); }
	
	//! octant boundaries
	double get_octant_upperbound( void )
	  { return RPCGEOM::R_bound[_index][_R_segmentation]; }

	double get_octant_lowerbound( void )
	  { return RPCGEOM::R_bound[_index][0]; }

	double get_octant_phimiin( int octant )
	  { return (octant*45.-22.5)*RPCGEOM::DEG_TO_RAD; }

	double get_octant_phimiax( int octant )
	  { return (octant*45.+22.5)*RPCGEOM::DEG_TO_RAD; }

	//!@name specific geometry description
	//!@{

	//! R segmentation
	UShort_t get_R_segmentation( void ) const
	{ return _R_segmentation; }

	//! R segmentation
	void set_Rsegmentation( UShort_t R_segmentation ) 
	{ _R_segmentation = R_segmentation; }	
	
	//@}
	
	//!@name dumper
	//@{
	
	//! print
	virtual void print( std::ostream& out = std::cout ) const
	{
		out << "RpcStation::print - index: " << _index << " z: " << _z << std::endl;
		for( std::vector<RpcStrip>::const_iterator iter = _strips.begin(); iter!=_strips.end(); iter++ )
		iter->print( out );
	}
	//@}
	
	protected:
			
	//! vector of strips
	std::vector<RpcStrip> _strips;

	RpcStrip initvals;

	//! arm
	UShort_t _arm;
	
	//! index 
	UShort_t _index;
			
	//! station z
	Float_t _z;
	
	//! R segmentation
	UShort_t _R_segmentation;
	

	ClassDef( RpcStation, 1 )
			
};

#endif
