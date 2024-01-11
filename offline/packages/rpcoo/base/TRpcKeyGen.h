// $Id: TRpcKeyGen.h,v 1.4 2009/09/24 07:53:16 hpereira Exp $
#ifndef _TRpcKeyGen_h_
#define _TRpcKeyGen_h_

/*!
	\file TRpcKeyGen.h
	\brief static class to generate keys for RPC objects
	\author H. Pereira Da Costa
	\version $Revision: 1.4 $
	\date		$Date: 2009/09/24 07:53:16 $
*/

#include <PHKey.hh>

//! Static utility class for generating RPC objects unique keys
/*!	
	Static utility class for generating unique keys from specification of
	map_key, arm, station.	The allowed range of input parameters is specified in
	the TRpcKeygen::get_key description. The get_key method is overloaded to
	provide appropriate keys for each type of interface object.
*/
class TRpcKeyGen
{

 public:

	/*! Import name of Key object from PHKey */
	typedef PHKey::object_key_type key_type; 
 
 	//! shortcut for first last key pair
	typedef std::pair<key_type,key_type> key_range;

	/*! Generate key for: TRpcHit */
	static key_type get_key(UShort_t i_arm, 
				UShort_t i_station,
				UShort_t i_octant,
				UShort_t i_halfoctant,
				UShort_t i_rseg,
				UShort_t i_index);

	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(UShort_t i_arm, UShort_t i_station, UShort_t i_octant, UShort_t i_halfoctant, UShort_t i_rseg );
 
	/*! Generate key for: TRpcTrk */
	static key_type get_key(UShort_t i_arm, UShort_t i_index);

	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(UShort_t i_arm );

	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(UShort_t i_arm, UShort_t i_station);

	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(UShort_t i_arm, UShort_t i_station, UShort_t i_octant);

	/*! Returns the maximum value of index */
	static ULong_t get_max_index() { return INDEX_MASK; }

 private:
	
	// Note: we must define sufficients bits 
	// for arm_max+1.. station_max+1 etc
	static const ULong_t ARM_BITS;
	static const ULong_t STATION_BITS;
	static const ULong_t OCTANT_BITS;
	static const ULong_t HALF_OCTANT_BITS;
	static const ULong_t RSEG_BITS;
	static const ULong_t INDEX_BITS;	
	
	static const ULong_t ARM_MASK;
	static const ULong_t STATION_MASK;
	static const ULong_t OCTANT_MASK;
	static const ULong_t HALF_OCTANT_MASK;
	static const ULong_t RSEG_MASK;
	static const ULong_t INDEX_MASK; 

	static const ULong_t INDEX_SHIFT;
	static const ULong_t RSEG_SHIFT;
	static const ULong_t HALF_OCTANT_SHIFT;
	static const ULong_t OCTANT_SHIFT;
	static const ULong_t STATION_SHIFT;
	static const ULong_t ARM_SHIFT;
	
};

/*! 
	Returns an unique key for input parameters in range shown
	below.
	<ul>
	<li> map key (32 bit)
	<li> i_arm [0,1]
	<li> i_station [0,2]
	<li> i_octant [0,7]
	<li> i_halfoctant [0,1]
	<li> i_rseg [0,4]
	<li> i_index [0,2^27-1]
	</ul>
	In the event that one of the input parameters is out of bounds
	the key returned will correspond to the upper bound of the 
	allowed range.	The significance of the input parameters with
	respect to key ordering is from left to right. That is 
	get_key(1,0,0) > get_key(0,2,2^27-1).
*/


#endif // __TMutKeyGen_H__

