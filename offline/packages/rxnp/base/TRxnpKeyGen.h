// $Id: TRxnpKeyGen.h,v 1.3 2006/12/08 20:20:52 zhangc Exp $
#ifndef _TRxnpKeyGen_h_
#define _TRxnpKeyGen_h_

/*!
	\file TRxnpKeyGen.h
	\brief static class to generate keys for Rxnp objects
	\author Chun Zhang
	\version $Revision: 1.3 $
	\date		$Date: 2006/12/08 20:20:52 $
*/

#include <PHKey.hh>

//! Static utility class for generating rxnp objects unique keys
/*!	
	Static utility class for generating unique keys from specification of
	map_key, arm, ring.	The allowed range of input parameters is specified in
	the TRxnpKeygen::get_key description. The get_key method is overloaded to
	provide appropriate keys for each type of interface object.

	Note about key :
	We have two rxnp detectors, one in south, the other in north.
	The detect is made up from 24 scintilators. They are organized as
        an inner ring and an outter ring. Each ring has 12 scintilators.
        So it would be national to map this geometry to key with three components
        arm (SOUTH, NORTH), ring (INNER, OUTTER), strip (0-11, for each scintilator).

*/
class TRxnpKeyGen
{

 public:

	/*! Import name of Key object from PHKey */
	typedef PHKey::object_key_type key_type; 
 
 	//! shortcut for first last key pair
	typedef std::pair<key_type,key_type> key_range;

	/*! Generate key for: TRxnpHit */
	static key_type get_key(UShort_t i_arm, 
				UShort_t i_ring, 
				UShort_t i_strip, 
				UShort_t i_index);

	/*! Generate key for: TRxnp angles */
	static key_type get_key(UShort_t i_arm, 
				UShort_t i_ring, 
				UShort_t i_index);

	/*! Generate key for: TRxnp angles */
	static key_type get_key(UShort_t i_arm, 
				UShort_t i_index);

	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(UShort_t i_arm, UShort_t i_ring, UShort_t i_strip );
 
	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(UShort_t i_arm, UShort_t i_ring );
 
	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(UShort_t i_arm );

	/*! Returns the maximum value of index */
	static ULong_t get_max_index() { return INDEX_MASK; }

 private:
	
	// Note: we must define sufficients bits 
	// for arm_max+1.. station_max+1 etc
	static const ULong_t ARM_BITS;
	static const ULong_t RING_BITS;
	static const ULong_t STRIP_BITS;
	static const ULong_t INDEX_BITS;	
	
	static const ULong_t ARM_MASK;
	static const ULong_t RING_MASK;
	static const ULong_t STRIP_MASK;
	static const ULong_t INDEX_MASK; 

	static const ULong_t INDEX_SHIFT;
	static const ULong_t STRIP_SHIFT;
	static const ULong_t RING_SHIFT;
	static const ULong_t ARM_SHIFT;
	
};

/*! 
	Returns an unique key for input parameters in range shown
	below.
	<ul>
	<li> map key (32 bit)
	<li> i_arm [0,1]
	<li> i_ring [0,1]
	<li> i_strip [0,11]
	<li> i_index [0,1]
	</ul>
	In the event that one of the input parameters is out of bounds
	the key returned will correspond to the upper bound of the 
	allowed range.	The significance of the input parameters with
	respect to key ordering is from left to right. That is 
	get_key(1,0,0,0) > get_key(0,1,11,1).
*/


#endif // __TRxnpKeyGen_H__

