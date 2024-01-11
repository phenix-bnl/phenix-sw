#ifndef __TFvtxMCHit_v1_h__
#define __TFvtxMCHit_v1_h__

// $Id: TFvtxMCHit_v1.h,v 1.8 2014/01/24 09:55:32 bbannier Exp $

/*!
	 \file TFvtxMCHit_v1.h
	 \brief The forward vertex Monte Carlo hit object 
	 \author Hugo Pereira Da Costa
	 \version $Revision: 1.8 $
	 \date $Date: 2014/01/24 09:55:32 $
*/

#include <FVTXOO.h>
#include <PHException.h>

#include "TFvtxMCHit.h"
#include "TFvtxMCStrip_v1.h"

/*! @ingroup interface */
//! The forward vertex Monte Carlo hit object 

class TFvtxMCHit_v1 : public TFvtxMCHit 
{
	
	protected:
		
	//! TMutMCStrip implementation
	typedef TFvtxMCStrip_v1 strip_value_type;

	//! strip list
	typedef std::vector< strip_value_type > strip_list;
	
	public:
							
	//! @name Constructors/Destructor
	//@{		
	//! Default
	TFvtxMCHit_v1();
	
	//! located constructor
	TFvtxMCHit_v1(
		const Key& key,
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column,
		const unsigned short& index );
	
	//! Construct from base class
	TFvtxMCHit_v1(const TFvtxMCHit& base_ref );
	
	//! Construct from base class
	TFvtxMCHit_v1(const TFvtxMCHit* base_pointer );

	//! Default destructor
	virtual ~TFvtxMCHit_v1()
	{}
	
	//!@}

	//! @name TFvtxMCStrip Interface
	//@{		
	//! construct and add a new strip with specified parameters
	virtual void add_strip( unsigned short strip, Float_t q)
	{ _strip_list.push_back( strip_value_type( strip, q ) ); }
	
	//! clear strip list
	virtual void clear_strip_list()
	{ _strip_list.clear();}
					 
	//! number of strips associated with this MChit
	virtual size_t get_n_strip() const 
	{ return _strip_list.size();}
	
	//! returns strip at given index
	virtual const TFvtxMCStrip* get_strip( const unsigned int& index ) const
	{
		BOUNDS_CHECK( index, get_n_strip() );
		return &_strip_list[index];
	}
		
	//!@}
	
	//! @name Locators
	//@{		
	//! Get arm number for this hit
	virtual unsigned short get_arm() const 
	{return _arm;}
	
        //! Get cage number for this hit
        virtual unsigned short get_cage() const
        {return _cage;}

	//! Get station number for this hit
	virtual unsigned short get_station() const 
	{return _station;}
	
	//! Get sector number for this hit
	virtual unsigned short get_sector() const 
	{return _sector;}
	
	//! Get column number for this hit
	virtual unsigned short get_column() const
	{ return _column; }
	
	//! Get hit index associated with this hit
	virtual unsigned short get_index() const 
	{return _index;}
	
	//! Set arm number for this hit
	virtual void set_arm(const unsigned short& arm ) 
	{ _arm = arm; }
	
        //! Set cage number for this hit
        virtual void set_cage(const unsigned short& cage )
        { _cage = cage; }

	//! Set station number for this hit
	virtual void set_station(const unsigned short& station ) 
	{ _station = station; }
	
	//! Set sector number for this hit
	virtual void set_sector(const unsigned short& sector ) 
	{ _sector = sector; }
	
	//! set column index for this hit
	virtual void set_column( const unsigned short& column )
	{ _column = column; }
	
	//! Set hit index associated with this hit
	virtual void set_index( const unsigned short& index ) 
	{ _index = index; }
	
	//!@}

	//! @name Functional Interface
	//@{	

	//!  User word to tag which file 
	virtual unsigned long get_file_key() const
	{ return _file_key; }
			
	//!	Get the PISA track number associated with this track
	virtual Long_t get_track_id() const 
	{ return _track_id; }
	
	//! Get time-of-flight associated with this hit
	virtual Float_t get_tof() const 
	{ return _tof; }
	
	//! Get energy loss associated with this hit
	virtual Float_t get_eloss() const 
	{ return _eloss; }
	
	//! Get x-position of track for this hit
	virtual Float_t get_x() const 
	{ return _x; }
	
	//! Get y-position of track for this hit
	virtual Float_t get_y() const 
	{ return _y; }
	
	//! Get z-position of track for this hit
	virtual Float_t get_z() const 
	{ return _z; }
		
	//! Get x-momentum of track at this hit position
	virtual Float_t get_px() const 
	{ return _px; }
	
	//! Get y-momentum of track at this hit position
	virtual Float_t get_py() const 
	{ return _py; }
	
	//! Get z-momentum of track at this hit position
	virtual Float_t get_pz() const 
	{ return _pz; }

	//! User word to tag which file
        virtual void set_file_key(unsigned long file_key) {
          _file_key = file_key;
        }

        //! PISA track number associated with this track
	virtual void set_track_id( const Long_t& track_id ) 
	{ _track_id = track_id; }
	
	//! Get time-of-flight associated with this hit
	virtual void set_tof( const Float_t& tof ) 
	{ _tof = tof; }
	
	//! Get energy loss associated with this hit
	virtual void set_eloss( const Float_t& eloss ) 
	{ _eloss = eloss; }
	
	//! Set x-position of track for this hit
	virtual void set_x(Float_t x) 
	{ _x = x; }
	
	//! Set y-position of track for this hit
	virtual void set_y(Float_t y) 
	{ _y = y; }
	
	//! Set z-position of track for this hit
	virtual void set_z(Float_t z) 
	{ _z = z; }
	
	//! Set x-momentum of track at this hit position
	virtual void set_px(Float_t px) 
	{ _px = px; }
	
	//! Set y-momentum of track at this hit position
	virtual void set_py(Float_t py) 
	{ _py = py; }
	
	//! Set z-momentum of track at this hit position
	virtual void set_pz(Float_t pz) 
	{ _pz = pz; }
		
	//!@}
	
	//! @name Dumpers
	//@{		
	//! Set Charge associated with this strip
	virtual void print(std::ostream& os = std::cout) const;

	//!@}
			
	private:
	
	//! arm index
	unsigned short _arm;
	
        //! cage index
        unsigned short _cage;

	//! station index
	unsigned short _station;
	
	//! sector index
	unsigned short _sector;
	
	//! plane index
	unsigned short _plane;
	
	//! radial segmentation index
	unsigned short _radius;
	
	//! column index
	unsigned short _column;
	
	//! MC hit index in column
	unsigned short _index;
	
	//! MC file index from which the MC hit is read (for multi file simulations
	unsigned long _file_key;
	
	//! parent MC track id
	Long_t _track_id;

	//! time of flight when particle reach the detector
	Float_t _tof;

	//! energy loss in the detector
	Float_t _eloss;

	//! position along x
	Float_t _x;

	//! position along y
	Float_t _y;

	//! position along z
	Float_t _z;

	//! momentum along x
	Float_t _px;

	//! momentum along y
	Float_t _py;

	//! momentum along z
	Float_t _pz;

	//! list of strips fired by this MC hit
	strip_list _strip_list;	
	
	ClassDef(TFvtxMCHit_v1,1)
};

#endif /* __TFvtxMCHit_v1_H__*/
