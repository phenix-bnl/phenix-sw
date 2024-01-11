
#ifndef __RpcStation_v1_H__
#define __RpcStation_v1_H__

#include <TObject.h>
#include "RpcStation.h"
#include "RpcStrip_v1.h"
#include "RPCFULLGEOM.h"

#include <vector>

//!	RPC Station geometry
/*!	
*/

class RpcStrip_v1;
class RpcStation;

class RpcStation_v1: public RpcStation
{
	public: 
			
	//! constructor 
	RpcStation_v1( UShort_t arm = 0, UShort_t index = 0 ):
			_arm( arm ),
			_index( index ),
			_z( 0 ),
			_R_segmentation( 3 )
	{}
	
	//! destructor
	virtual	~RpcStation_v1(void)
	{ clear_strips(); }
	
	virtual RpcStrip* get_strip( Int_t index );
	
	//! get pad index from (x,y) position
	virtual Int_t get_strip_index( const double& x, const double& y ) const; 

	//! create all pads. RpcStrip_v1's are created
	virtual void create_strips( void );
		
	//! clear pads
	virtual void clear_strips( void )
	{ _strips.clear(); }

	//!@name dumper
	//@{
	
	//! print
	virtual void print( std::ostream& out = std::cout ) const
	{
	  out << "RpcStation_v1::print - index: " << _index << " z: " << _z << std::endl;
	  for( std::vector<RpcStrip_v1>::const_iterator iter = _strips.begin(); iter!=_strips.end(); iter++ ) {
	    iter->print( out );	} }
	//@}
	
	protected:
	
	//! vector of strips
	std::vector<RpcStrip_v1> _strips;
	
	RpcStrip_v1 initvals;
	
	//! arm
	UShort_t _arm;
	
	//! index 
	UShort_t _index;
			
	//! station z
	Float_t _z;
	
	//! R segmentation
	UShort_t _R_segmentation;
	

	ClassDef( RpcStation_v1, 1 )
			
};

#endif
