// $Id: TRpcMCHit_v1.h,v 1.3 2008/08/28 00:50:21 kempel Exp $

#ifndef _TRpcMCHit_v1_h_
#define _TRpcMCHit_v1_h_

/*!
	\file TRpcMCHit_v1.h
	\brief Rpc MC hit interface object
	\author H. Pereira Da Costa
	\version $Revision: 1.3 $
	\date		$Date: 2008/08/28 00:50:21 $
*/

#include "TRpcMCHit.h"

/*! @ingroup interface */
//! Rpc MC hit interface object
class TRpcMCHit_v1 : public TRpcMCHit
{		
	
	public:	

	/*! Default constructor */
	TRpcMCHit_v1();

	/*! Construct with key and location */
	TRpcMCHit_v1(
		const Key& key, 
		const UShort_t& arm, 
		const UShort_t& station, 
		const UShort_t& octant,
		const UShort_t& halfoctant,
		const UShort_t& rseg,
		const UShort_t& index);

	//! constructor from base class
	TRpcMCHit_v1(const TRpcMCHit* base_ptr);
	
	//! constructor from base class
	TRpcMCHit_v1(const TRpcMCHit& base_ref);

	/*! Virtual destructor */
	virtual ~TRpcMCHit_v1()
	{ clear_strip_list(); }

	//! @name TRpcMCStrip Interface
	//@{		
	/*! construct and add a new strip with specified parameters */
	virtual void add_strip(UShort_t stripid, Float_t q)
	{ _strip_list.push_back( new strip_value_type( stripid, q ) ); }
	
	/*! clear strip list */
	virtual void clear_strip_list()
	{ 
		for( strip_iterator iter = _strip_list.begin(); iter != _strip_list.end(); iter++ )
		if( *iter ) delete *iter;
		_strip_list.clear(); 
	}			
	
	/*! read only access to strip list */
	virtual const strip_list get_strip_list() const 
	{ return _strip_list; }
	//!@}

	/*! Arm [0,1] */
	virtual UShort_t	get_arm() const 
	{return _arm;}
	
	/*! Station [0,2] */
	virtual UShort_t	get_station() const 
	{return _station;}	
	
	/*! Octant [0,7] */
	virtual UShort_t	get_octant() const 
	{return _octant;}
	
	/*! Half Octant [0,1] */
	virtual UShort_t	get_half_octant() const 
	{return _halfoctant;}
	
	/*! Radial Segment (see RPCGEOM.h) */
	virtual UShort_t	get_rseg() const 
	{return _rseg;}

	/*! index */
	virtual UShort_t	get_index() const 
	{return _index;}	
	
	/*! Arm [0,1] */
	virtual void set_arm(UShort_t arm)
	{ _arm = arm; }

	/*! Station [0,2] */
	virtual void set_station(UShort_t station)
	{ _station = station; }

	/*! Octant [0,7] */
	virtual void set_octant(UShort_t octant)
	{ _octant = octant; }	

	/*! Half Octant [0,1] */
	virtual void set_half_octant(UShort_t halfoctant)
	{ _halfoctant = halfoctant; }		

	/*! Radial Segment (see RPCGEOM.h) */
	virtual void set_rseg(UShort_t rseg)
	{ _rseg = rseg; }

	/*! index */
	virtual void set_index( UShort_t index) 
	{_index = index;}	

	/*!	User word to tag which file */
	virtual ULong_t get_file_key() const
	{return _file_key;}

	/*!	Get the PISA track number associated with this track */
	virtual Long_t get_track_id() const 
	{return _track_id;}

	/*! Get time-of-flight associated with this hit */
	virtual Float_t get_tof() const 
	{return _tof;}

	/*! Get energy loss associated with this hit */
	virtual Float_t get_eloss() const 
	{return _eloss;}

	//! position along x
	virtual Float_t get_x() const 
	{ return _x; }

	//! position along y
	virtual Float_t get_y() const 
	{ return _y; }

	//! position along z
	virtual Float_t get_z() const 
	{ return _z; }

	/*! Get x-momentum of track at this hit position */
	virtual Float_t get_px() const 
	{return _px;}

	/*! Get y-momentum of track at this hit position */
	virtual Float_t get_py() const 
	{return _py;}

	/*! Get z-momentum of track at this hit position */
	virtual Float_t get_pz() const 
	{return _pz;}

	/*! User word to tag which file */
	virtual void set_file_key(ULong_t file_key) 
	{ _file_key = file_key; }
	
	/*! PISA track number associated with this track */
	virtual void set_track_id(Long_t track_id) 
	{ _track_id = track_id; }

	/*! Set time-of-flight associated with this hit */
	virtual void set_tof(Float_t tof) 
	{ _tof = tof; }

	/*! Get energy loss associated with this hit */
	virtual void set_eloss(Float_t eloss) 
	{ _eloss = eloss; }

	//! position along x
	virtual void set_x( Float_t x ) 
	{ _x = x; }
	
	//! position along y
	virtual void set_y( Float_t y ) 
	{ _y = y; }
	
	//! position along z
	virtual void set_z( Float_t z ) 
	{ _z = z; }

	/*! Set x-momentum of track at this hit position */
	virtual void set_px(Float_t px) 
	{ _px = px; }
	
	/*! Set y-momentum of track at this hit position */
	virtual void set_py(Float_t py) 
	{ _py = py; }
	
	/*! Set z-momentum of track at this hit position */
	virtual void set_pz(Float_t pz) 
	{ _pz = pz; }

	/*! Print data members to ostream os, defaults to std::cout */
	virtual void print(std::ostream& os = std::cout) const;
	
	private:	
	
	UShort_t _arm;
	UShort_t _station;
	UShort_t _octant;
	UShort_t _halfoctant;
	UShort_t _rseg;
	UShort_t _index;
	
	ULong_t _file_key;
	Long_t _track_id;
	Float_t _tof;
	Float_t _eloss;
	Float_t _x;
	Float_t _y;
	Float_t _z;
	Float_t _px;
	Float_t _py;
	Float_t _pz;
	strip_list _strip_list;

	//! ROOT dictionary
	ClassDef(TRpcMCHit_v1,1)
};

#endif
