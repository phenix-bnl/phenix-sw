#ifndef __TMutKeyGen_H__
#define __TMutKeyGen_H__
// $Id: TMutKeyGen.h,v 1.3 2011/12/24 04:48:19 slash Exp $

/*!
	\file TMutKeyGen.h
	\brief static class to generate keys for forward vertex objects
	\author H. Pereira Da Costa
	\version $Revision: 1.3 $
	\date $Date: 2011/12/24 04:48:19 $
*/

#include<TDataType.h>
#include<PHKey.hh>

/*! \ingroup classes */
//! Key generation class for PHENIX muon tracker
/*! 
	Static utility class for generating unique keys from specification
	of map_key, arm, station, half octant, plane and index.	The allowed 
	range of input parameters is specified in the TMutKeyGen::get_key 
	description. The get_key method is overloaded to provide appropriate
	keys for each type of interface object.
*/

class TMutKeyGen
{

 public:

	/*! Import name of Key object from PHKey */

	typedef PHKey::object_key_type key_type; 
	typedef std::pair<key_type,key_type> key_range;

	/*! Generate key for: TMutHitMap, TMutClusMap, TMutCoord */
	static key_type get_key(unsigned short i_arm, 
		unsigned short i_station, 
		unsigned short i_octant, 
		unsigned short i_half_octant, 
		unsigned short i_gap, 
		unsigned short i_cathode,
		unsigned short i_index);

	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(unsigned short i_arm, 
				 unsigned short i_station, 
				 unsigned short i_octant, 
				 unsigned short i_half_octant, 
				 unsigned short i_gap, 
				 unsigned short i_cathode);

	/*! Generate key for: TMutGapCoord, TMutMCHit */
	static key_type get_key(unsigned short i_arm, 
				unsigned short i_station, 
				unsigned short i_octant, 
				unsigned short i_half_octant, 
				unsigned short i_gap, 
				unsigned short i_index);


	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(unsigned short i_arm, 
				 unsigned short i_station, 
				 unsigned short i_octant, 
				 unsigned short i_half_octant, 
				 unsigned short i_gap);
					 

	/*! Generate key for: TMutStub */
	static key_type get_key(unsigned short i_arm, 
				unsigned short i_station, 
				unsigned short i_octant, 
				unsigned short i_half_octant,
				unsigned short i_index);


	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(unsigned short i_arm, 
				 unsigned short i_station, 
				 unsigned short i_octant, 
				 unsigned short i_half_octant);
				

	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(unsigned short i_arm, 
				 unsigned short i_station, 
				 unsigned short i_octant);
	
	/*! Generate key for: TMutTrk */
	static key_type get_key(unsigned short i_arm, 
				unsigned short i_octant, 
				unsigned short i_index);

	/*! Generate a pair of keys with specified range	*/
	static key_range get_key_range(unsigned short i_arm, 
				 unsigned short i_octant);
	
	/*! Generate key for: TMutMCTrk */
	static key_type get_key(unsigned short i_arm, 
				unsigned short i_index);

	/*! Generate a pair of keys with specified range */
	static key_range get_key_range(unsigned short i_arm);

	/*! Returns the maximum value of index */
	static ULong_t get_max_index() { return INDEX_MASK; }

 private:
	
	// Note: we must define sufficients bits 
	// for arm_max+1.. station_max+1 etc
	//
	static const ULong_t ARM_BITS;
	static const ULong_t STATION_BITS;
	static const ULong_t OCTANT_BITS;
	static const ULong_t HALF_OCTANT_BITS;
	static const ULong_t GAP_BITS;
	static const ULong_t CATHODE_BITS;
	static const ULong_t INDEX_BITS;	
	
	static const ULong_t ARM_MASK;
	static const ULong_t STATION_MASK;
	static const ULong_t OCTANT_MASK;
	static const ULong_t HALF_OCTANT_MASK;
	static const ULong_t GAP_MASK;
	static const ULong_t CATHODE_MASK;
	static const ULong_t INDEX_MASK; 

	static const ULong_t INDEX_SHIFT;
	static const ULong_t CATHODE_SHIFT;
	static const ULong_t GAP_SHIFT;
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
	<li> i_half_octant [0,1]
	<li> i_gap [0,2]
	<li> i_cathode [0,1]
	<li> i_index [0,1023]
	</ul>
	In the event that one of the input parameters is out of bounds
	the key returned will correspond to the upper bound of the 
	allowed range.	The significance of the input parameters with
	respect to key ordering is from left to right. That is 
	get_key(1,0,0,0,0,0,0) > get_key(0,2,7,1,2,1,1,1023).
*/


#endif // __TMutKeyGen_H__

