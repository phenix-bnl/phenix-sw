
#ifndef __RpcStation_proto_H__
#define __RpcStation_proto_H__

#include <TObject.h>
#include "RpcStation.h"
#include "RpcStrip_proto.h"
#include "RPCPROTOGEOM.h"

#include <vector>

//!	RPC Station geometry
/*!	
*/

class RpcStrip_proto;
class RpcStation;

class RpcStation_proto: public RpcStation
{
	public: 
			
	//! constructor 
	RpcStation_proto( UShort_t arm = 0, UShort_t index = 0 ):
			_arm( arm ),
			_index( index ),
			_z( 0 ),
			_R_segmentation( 3 )
	{}
	
	//! destructor
	virtual	~RpcStation_proto(void)
	{ clear_strips(); }
	
	virtual RpcStrip* get_strip( Int_t index );
	
	//! get pad index from (x,y) position
	virtual Int_t get_strip_index( const double& x, const double& y ) const; 

	//! create all pads. RpcStrip_proto's are created
	virtual void create_strips( void );
		
	//! clear pads
	virtual void clear_strips( void )
	{ _strips.clear(); }

	//!@name dumper
	//@{
	
	//! print
	virtual void print( std::ostream& out = std::cout ) const
	{
	  out << "RpcStation_proto::print - index: " << _index << " z: " << _z << std::endl;
	  for( std::vector<RpcStrip_proto>::const_iterator iter = _strips.begin(); iter!=_strips.end(); iter++ ) {
	    iter->print( out );	} }
	//@}
	
	protected:
	
	//! vector of strips
	std::vector<RpcStrip_proto> _strips;
	
	RpcStrip_proto initvals;
	
	//! arm
	UShort_t _arm;
	
	//! index 
	UShort_t _index;
			
	//! station z
	Float_t _z;
	
	//! R segmentation
	UShort_t _R_segmentation;
	

	ClassDef( RpcStation_proto, 1 )
			
};

#endif
