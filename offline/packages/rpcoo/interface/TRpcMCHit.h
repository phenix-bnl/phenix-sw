// $Id: TRpcMCHit.h,v 1.4 2014/01/26 17:43:23 bbannier Exp $

#ifndef _TRpcMCHit_h_
#define _TRpcMCHit_h_

/*!
	\file TRpcMCHit.h
	\brief Rpc MC hit interface object
	\author H. Pereira Da Costa
	\version $Revision: 1.4 $
	\date $Date: 2014/01/26 17:43:23 $
*/


#include <PHKey.hh>
#include <PHPoint.h>
#include <PHVector.h>
#include "TRpcMCStrip_v1.h"

#include <vector>

/*! @ingroup interface */
//! Rpc MC hit interface object
class TRpcMCHit : public PHKey
{		
	
	public:	
	
	//! typedef for reference to the current centroid version
	typedef TRpcMCStrip_v1 strip_value_type;

	//! typedef for pointer to the current centroid version
	typedef TRpcMCStrip_v1* strip_pointer;

	//! Name for the list of TRpcMCStrip contained in this object 
	typedef std::vector<TRpcMCStrip*> strip_list;

	//! Name for the TRpcMCStrip list iterator	
	typedef strip_list::const_iterator strip_iterator;

	//! Default constructor 
	TRpcMCHit()
	{}

	//! Construct with key and location 
	TRpcMCHit(const Key& key) : PHKey(key) 
	{}

	//! Virtual destructor 
	virtual ~TRpcMCHit()
	{}

	//! @name TRpcMCStrip Interface
	//@{		
	//! construct and add a new strip with specified parameters 
	virtual void add_strip(UShort_t stripid, Float_t q)
	{}
	
	//! clear strip list 
	virtual void clear_strip_list()
	{}			
			 
	//! number of strips associated with this MChit 
	virtual size_t get_n_strip() const 
	{ return 0;}
	
	//! read only access to strip list 
	virtual const strip_list get_strip_list() const 
	{ return strip_list(); }
	//!@}

	//! Arm [0,1] 
	virtual UShort_t get_arm() const 
	{return 0;}
	
	//! Station [0,2] 
	virtual UShort_t get_station() const 
	{return 0;}	
	 
	//! Octant [0,7] 
	virtual UShort_t get_octant() const 
	{return 0;}
  
	//! Half Octant [0,1] 
	virtual UShort_t get_half_octant() const 
	{return 0;}
  
	//! Radial Segment (see RPCGEOM.h)
	virtual UShort_t get_rseg() const 
	{return 0;}

	//! index 
	virtual UShort_t get_index() const 
	{return 0;}	
	
	//! Arm [0,1] 
	virtual void set_arm(UShort_t arm)
	{}

	//! Station [0,2] 
	virtual void set_station(UShort_t station)
	{}							 	
	  
	//! Octant [0,7] 
	virtual void set_octant(UShort_t octant) 
	{}
	
	//! Half Octant [0,1]
	virtual void set_half_octant(UShort_t half_octant) 
	{}

        //! Radial Segment (see RPCGEOM.h)
        virtual void set_rseg(UShort_t rseg) {}

        //! index
        virtual void set_index(UShort_t index) {}

        //! User word to tag which file
        virtual ULong_t get_file_key() const { return 0; }

        //! Get the PISA track number associated with this track
        virtual Long_t get_track_id() const { return 0; }

        //! Get time-of-flight associated with this hit
        virtual Float_t get_tof() const { return 0; }

        //! Get energy loss associated with this hit
        virtual Float_t get_eloss() const { return 0; }

        //! position along x
	virtual Float_t get_x() const 
	{ return 0; }

	//! position along y
	virtual Float_t get_y() const 
	{ return 0; }

	//! position along z
	virtual Float_t get_z() const 
	{ return 0; }

	//! Get PHPoint(x,y,z) of this hit 
	virtual PHPoint get_coord() const 
	{ return PHPoint( get_x(), get_y(), get_z() ); }
	
	//! Get x-momentum of track at this hit position 
	virtual Float_t get_px() const 
	{return 0;}

	//! Get y-momentum of track at this hit position 
	virtual Float_t get_py() const 
	{return 0;}

	//! Get z-momentum of track at this hit position 
	virtual Float_t get_pz() const 
	{return 0;}

	//! get momentum
	virtual PHVector get_momentum() const
	{ return PHVector( get_px(), get_py(), get_pz() ); }
	
	//! User word to tag which file 
	virtual void set_file_key(ULong_t file_key) 
	{}
	
	//! PISA track number associated with this track 
	virtual void set_track_id(Long_t track_id) 
	{}

	//! Set time-of-flight associated with this hit 
	virtual void set_tof(Float_t tof) 
	{}

	//! Get energy loss associated with this hit 
	virtual void set_eloss(Float_t eloss) 
	{}

	//! position along x
	virtual void set_x( Float_t value ) 
	{}
	
	//! position along y
	virtual void set_y( Float_t value ) 
	{}
	
	//! position along z
	virtual void set_z( Float_t value ) 
	{}
	
	//! Get PHPoint(x,y,z) of this hit 
	virtual void set_coord( const PHPoint &point ) 
	{ 
		set_x( point.getX() );
		set_y( point.getY() );
		set_z( point.getZ() );
	}

	//! Set x-momentum of track at this hit position 
	virtual void set_px(Float_t px) 
	{}
	
	//! Set y-momentum of track at this hit position 
	virtual void set_py(Float_t py) 
	{}
	
	//! Set z-momentum of track at this hit position 
	virtual void set_pz(Float_t pz) 
	{}

	//! set momentum
	virtual void set_momentum( const PHVector& p )
	{
		set_px( p.getX() );
		set_py( p.getY() );
		set_pz( p.getZ() );
	}
	
	//! Print data members to ostream os, defaults to std::cout 
	virtual void print(std::ostream& os = std::cout) const 
	{} 

	//! ROOT dictionary
	ClassDef(TRpcMCHit,1)
};

#endif
