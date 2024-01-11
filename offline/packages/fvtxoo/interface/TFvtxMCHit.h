#ifndef __TFvtxMCHit_h__
#define __TFvtxMCHit_h__

// $Id: TFvtxMCHit.h,v 1.8 2011/12/01 04:16:20 slash Exp $

/*!
	 \file TFvtxMCHit.h
	 \brief The forward vertex Monte Carlo hit object 
	 \author Hugo Pereira Da Costa
	 \version $Revision: 1.8 $
	 \date $Date: 2011/12/01 04:16:20 $
*/

#include <vector>

#include<PHKey.hh>
#include<PHException.h>
#include<PHPoint.h>
#include<PHVector.h>
#include<TDataType.h>

#include "TFvtxMCStrip.h"

/*! @ingroup interface */
//! The forward vertex Monte Carlo hit object 

class TFvtxMCHit : public PHKey 
{
	
	public:
		
	//! @name Constructors/Destructor
	//@{		
	//! Default
	TFvtxMCHit()
	{}

	//! Construct with key
	TFvtxMCHit(const Key& key) : PHKey(key)
	{}

	//! Default
	virtual ~TFvtxMCHit() 
	{}
	//!@}

	//! @name TFvtxMCStrip Interface
	//@{		
	//! construct and add a new strip with specified parameters
	virtual void add_strip(unsigned short strip, Float_t q)
	{}
	
	//! clear strip list
	virtual void clear_strip_list()
	{}	
					 
	//! number of strips associated with this MChit
	virtual size_t get_n_strip() const 
	{ return 0;}
	
	//! return strip of given index in list
	virtual const TFvtxMCStrip* get_strip( const unsigned int& index ) const
	{
		throw std::logic_error( DESCRIPTION( "call to base class method forbidden" ) );
		return 0;
	}
		
	//!@}
	
	//! @name Locators
	//@{		
	//! Get arm number for this hit
	virtual unsigned short get_arm() const 
	{return 0;}
	
        //! Get cage number for this hit
        virtual unsigned short get_cage() const
        {return 0;}

	//! Get station number for this hit
	virtual unsigned short get_station() const 
	{return 0;}
	
	//! Get sector number for this hit
	virtual unsigned short get_sector() const 
	{return 0;}
	
	//! Get column number for this hit
	virtual unsigned short get_column() const
	{ return 0; }
	
	//! Get hit index associated with this hit
	virtual unsigned short get_index() const 
	{return 0;}
	
	//! Set arm number for this hit
	virtual void set_arm(const unsigned short& ) 
	{}

        //! Set cage number for this hit
        virtual void set_cage(const unsigned short&)
        {}
	
	//! Set station number for this hit
	virtual void set_station(const unsigned short&) 
	{}
	
	//! Set sector number for this hit
	virtual void set_sector(const unsigned short&) 
	{}
	
	//! set column number for this hit
	virtual void set_column( const unsigned short& )
	{}
	
	//! Set hit index associated this hit
	virtual void set_index( const unsigned short& ) 
	{}
	
	//!@}

	//! @name Functional Interface
	//@{	

	//!  User word to tag which file 
	virtual unsigned long get_file_key() const
	{return 0;}
			
	//!	Get the PISA track number associated with this track
	virtual Long_t get_track_id() const 
	{return 0;}
	
	//! Get time-of-flight associated with this hit
	virtual Float_t get_tof() const 
	{return 0;}
	
	//! Get energy loss associated with this hit
	virtual Float_t get_eloss() const 
	{return 0;}
	
	//! Get x-position of track for this hit
	virtual Float_t get_x() const 
	{return 0;}
	
	//! Get y-position of track for this hit
	virtual Float_t get_y() const 
	{return 0;}
	
	//! Get z-position of track for this hit
	virtual Float_t get_z() const 
	{return 0;}
	
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
	virtual void set_file_key(unsigned long file_key) 
	{}
	
	//! PISA track number associated with this track
	virtual void set_track_id( const Long_t&) 
	{}
	
	//! Get time-of-flight associated with this hit
	virtual void set_tof( const Float_t& ) 
	{}
	
	//! Get energy loss associated with this hit
	virtual void set_eloss( const Float_t& ) 
	{}
	
	//! Set x-position of track for this hit
	virtual void set_x(Float_t x) 
	{}
	
	//! Set y-position of track for this hit
	virtual void set_y(Float_t y) 
	{}
	
	//! Set z-position of track for this hit
	virtual void set_z(Float_t z) 
	{}
	
	//! Set x-momentum of track at this hit position
	virtual void set_px(Float_t px) 
	{}
	
	//! Set y-momentum of track at this hit position
	virtual void set_py(Float_t py) 
	{}
	
	//! Set z-momentum of track at this hit position
	virtual void set_pz(Float_t pz) 
	{}
		
	//!@}
	
	//! @name Dumpers
	//@{		
	//! Set Charge associated with this strip
	virtual void print(std::ostream& os = std::cout) const 
	{}
	//!@}
	
	ClassDef(TFvtxMCHit,1)
};


#endif /* __TFvtxMCHit_H__*/
