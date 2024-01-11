// Static Class : TMuiKeyGen
// Author: J.Newby 
// Date: 2/12/03
// Description: Generates keys for PHENIX MUID objects

#ifndef __TMuiKeyGen_H__
#define __TMuiKeyGen_H__

#include<TDataType.h>
#include<PHKey.hh>

/*! \ingroup classes */
//! Key generation class for PHENIX muon identifier
/*! 
	Static utility class for generating unique keys from specification
	of map_key, arm, plane, panel, orientation, and index.	The allowed 
	range of input parameters is specified in the TMuiKeyGen::get_key 
	description. The get_key method is overloaded to provide appropriate
	keys for each type of interface object.
*/

class TMuiKeyGen
{

	public:

	/*! Import name of Key object from PHKey */
	typedef PHKey::object_key_type key_type; 
	typedef std::pair<key_type,key_type> key_range;

	/*! Generate key for: TMuiHitMap, TMuiClusMap */
	static key_type get_key(
    const UShort_t& i_arm, 
    const UShort_t& i_plane, 
    const UShort_t& i_panel, 
    const UShort_t& i_orientation, 
    const UShort_t& i_twopack,
    const UShort_t& i_index);

	/*! Generate key for: TMuiHitMap, TMuiClusMap */
	static key_type get_key(
    const UShort_t& i_arm, 
    const UShort_t& i_plane, 
    const UShort_t& i_panel, 
    const UShort_t& i_orientation, 
    const UShort_t& i_index);
	
	/*! Generate a pair of keys with specified range, used by TMuiClusterMapO and TMuiHitMapO */
	/* TMui1DRoadMapO also uses this four argument get_key_range, but plane=0 since plane
		 is not relevant for 1D Roads */ 
	static key_range get_key_range(
    const UShort_t& i_arm,
    const UShort_t& i_plane,
    const UShort_t& i_panel,
    const UShort_t& i_orientation,
    const UShort_t& i_twopack);

	/*! Generate a pair of keys with specified range, used by TMuiClusterMapO and TMuiHitMapO */
	/* TMui1DRoadMapO also uses this four argument get_key_range, but plane=0 since plane
		 is not relevant for 1D Roads */ 
	static key_range get_key_range(
    const UShort_t& i_arm,
    const UShort_t& i_plane,
    const UShort_t& i_panel,
    const UShort_t& i_orientation);

	/*! Generate key for: arm, plane, panel and index */
	static key_type get_key(
    const UShort_t& i_arm,
    const UShort_t& i_plane,
    const UShort_t& i_panel,
    const UShort_t& index );

	/*! Generate a pair of keys with specified range arm, plane and panel range */
	static key_range get_key_range(
    const UShort_t& i_arm,
    const UShort_t& i_plane,
    const UShort_t& i_panel );

	/*! Generate key for: TMuiMCHit */
	static key_type get_key(const UShort_t& i_arm, const UShort_t& i_plane, const UShort_t& i_index);
	
	 /*! Generate a pair of keys with specified range, used by TMuiMCHitMapO */
	static key_range get_key_range(const UShort_t& i_arm, const UShort_t& i_plane);
					 
	/*! Generate key for: TMuiRoadO */
	static key_type get_key(const UShort_t& i_arm, const UShort_t& i_index);

	/*! Generate a pair of keys with specified range, relevant for all MUIOO containers	*/
	static key_range get_key_range(const UShort_t& i_arm);

	/*! Return the index from a key */
	static int get_index(key_type);

	/*! Returns the maximum value of index */
	static unsigned long get_max_index() 
  { return INDEX_MASK; }
	
 private:
	
	// Note: we must define sufficients bits 
	// for arm_max+1.. plane_max+1 etc
	//
	static const unsigned long ARM_BITS;
	static const unsigned long PLANE_BITS;
	static const unsigned long PANEL_BITS;
	static const unsigned long ORIENTATION_BITS;
	static const unsigned long TWOPACK_BITS;
	static const unsigned long INDEX_BITS;	
	
	static const unsigned long ARM_MASK;
	static const unsigned long PLANE_MASK;
	static const unsigned long PANEL_MASK;
	static const unsigned long ORIENTATION_MASK;
	static const unsigned long TWOPACK_MASK;
	static const unsigned long INDEX_MASK; 

	static const unsigned long INDEX_SHIFT;
	static const unsigned long TWOPACK_SHIFT;
	static const unsigned long ORIENTATION_SHIFT;
	static const unsigned long PANEL_SHIFT;
	static const unsigned long PLANE_SHIFT;
	static const unsigned long ARM_SHIFT;
	
};

/*! 
	Returns an unique key for input parameters in range shown
	below.
	<ul>
	<li> map key (32 bit)
	<li> i_arm [0,1]
	<li> i_plane [0,4]
	<li> i_panel [0,5]
	<li> i_orienation [0,1]
	<li> i_index [0,64]
	</ul>
	In the event that one of the input parameters is out of bounds
	the key returned will correspond to the upper bound of the 
	allowed range.	The significance of the input parameters with
	respect to key ordering is from left to right. That is 
	get_key(1,0,0,0,0,0,0) > get_key(0,2,7,1,2,1,1,1023).
*/


#endif // __TMuiKeyGen_H__

