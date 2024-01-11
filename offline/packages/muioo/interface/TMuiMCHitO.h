// Interface Object Class : TMuiMCHit
// Author: J.L.Nagle
// Date: 6/23/2003

#ifndef __TMUIMCHITO_H__
#define __TMUIMCHITO_H__

#include<PHException.h>
#include<PHKey.hh>
#include<PHPoint.h>
#include<PHVector.h>
#include<TDataType.h>

#include "MUIOO.h"
#include "TMuiMCTwoPack.h"

/*! @ingroup interface */
//!	The Muon identifier Monte Carlo hit object 
/*!	The Muon identifier Monte Carlo hit object */

class TMuiMCHitO : public PHKey 
{
	
public:

	/*! Name for the list of TMuiMCTwoPack contained in this object */
	typedef std::vector<TMuiMCTwoPack> twopack_list;

	/*! Name for the TMutMCTwoPack list iterator	*/
	typedef twopack_list::const_iterator twopack_iterator;

	//! @name Constructors/Destructor
	//@{		
	/*! Default */
	TMuiMCHitO(){;}

	/*! Construct with key */
	TMuiMCHitO(const Key& key) : PHKey(key){;}

	/*! Default */
	virtual ~TMuiMCHitO() {;}
	//!@}

	//! @name TMuiMCTwoPack Interface
	//@{		
	
	/*! construct and add a new strip with specified parameters */
	virtual void add_twopack(UShort_t orient, UShort_t panel, UShort_t twopack_index){ }
	
	/*! clear twopack list */
	virtual void clear_twopack_list(){}					 
	
	/*! number of twopacks associated with this MChit */
	virtual size_t get_n_twopack() const { return 0;}
	
	/*! immutable access to twopack list */
	virtual const twopack_list* get_twopack_list() const {
		// Print a warning about leaked resource, in case it is called.
		//
		MUIOO::TRACE(DESCRIPTION("Resource leak from base"));
		return new twopack_list();
	}
	
	/*! immutable access to twopack list */
	virtual twopack_list* get_mutable_twopack_list() {
		// Print a warning about leaked resource, in case it is called.
		//
		MUIOO::TRACE(DESCRIPTION("Resource leak from base"));
		return new twopack_list();
	}
	//!@}

	//! @name Functional Interface
	//@{		
	
	/*!	Get the PISA track number associated with this track */
	virtual Int_t get_track_id() const {return 0;}
	
	/*! Get x-position of track for this hit */
	virtual Float_t get_x() const {return 0;}
	
	/*! Get y-position of track for this hit */
	virtual Float_t get_y() const {return 0;}
	
	/*! Get z-position of track for this hit */
	virtual Float_t get_z() const {return 0;}
	
	/*! Get PHPoint(x,y,z) of this hit */
	virtual PHPoint get_coord() const 
	{ return PHPoint( get_x(), get_y(), get_z() ); }
	
	/*! Get x-momentum of track at this hit position */
	virtual Float_t get_px() const {return 0;}
	
	/*! Get y-momentum of track at this hit position */
	virtual Float_t get_py() const {return 0;}
	
	/*! Get z-momentum of track at this hit position */
	virtual Float_t get_pz() const {return 0;}
	
	//! Get momentum vector of track at this hit position
	virtual PHVector get_momentum() const 
	{ return PHVector( get_px(), get_py(), get_pz() ); } 
	
	/*! Get particle id of the hit */
	virtual Short_t get_pid() const {return 0;}

	/*! Get file key, which is used for embeding */
	virtual UShort_t get_file_key() const {return 0;} 

	/*! PISA track number associated with this track */
	virtual void set_track_id(Int_t track_id) { }

	/*! Set x-position of track for this hit */
	virtual void set_x(Float_t x) { }

	/*! Set y-position of track for this hit */
	virtual void set_y(Float_t y) { }

	/*! Set z-position of track for this hit */
	virtual void set_z(Float_t z) { }

	/*! Set x-momentum of track at this hit position */
	virtual void set_px(Float_t px) { }

	/*! Set y-momentum of track at this hit position */
	virtual void set_py(Float_t py) { }

	/*! Set z-momentum of track at this hit position */
	virtual void set_pz(Float_t pz) { }

	/*! Set particle id for the hit */
	virtual void set_pid(Short_t pid) { }

	/*! Set file key, which is used for embeding */
	virtual void set_file_key(UShort_t file_key) { }

	//!@}
	
	//! @name Locators
	//@{		

	/*! Get arm number for this hit */
	virtual UShort_t get_arm() const {return 0;}

	/*! Get muid plane number for this hit */
	virtual UShort_t get_plane() const {return 0;}

	/*! Get hit index associated with this hit */
	virtual UShort_t get_index() const {return 0;}

	/*! Set arm number for this hit */
	virtual void set_arm(UShort_t arm) { }

	/*! Set muid plane number for this hit */
	virtual void set_plane(UShort_t plane) { }

	/*! Set hit index associated with this hit */
	virtual void set_index(UShort_t index) { }
	
	//!@}
	
	//! @name Dumpers
	//@{		
	/*! Set Charge associated with this strip */
	virtual void print(std::ostream& os = std::cout) const {}
	//!@}
	
	ClassDef(TMuiMCHitO,1)
};

#endif /* __TMUIMCHITO_H__*/



