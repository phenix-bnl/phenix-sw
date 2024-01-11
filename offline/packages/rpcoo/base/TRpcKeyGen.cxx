// $Id: TRpcKeyGen.cxx,v 1.5 2009/09/24 07:53:16 hpereira Exp $

/*!
	\file TRpcKeyGen.cxx
	\brief static class to generate keys for RPC objects
	\author H. Pereira Da Costa
	\version $Revision: 1.5 $
	\date		$Date: 2009/09/24 07:53:16 $
*/
	
#include "TRpcKeyGen.h"

	// Note: we must define sufficients bits 
	// for arm_max+1.. station_max+1 etc
const ULong_t TRpcKeyGen::ARM_BITS=2;
const ULong_t TRpcKeyGen::STATION_BITS=3;
const ULong_t TRpcKeyGen::OCTANT_BITS=4;
const ULong_t TRpcKeyGen::HALF_OCTANT_BITS=2;
const ULong_t TRpcKeyGen::RSEG_BITS=4;
const ULong_t TRpcKeyGen::INDEX_BITS=16;	
	
const ULong_t TRpcKeyGen::ARM_MASK = 0x00000003;
const ULong_t TRpcKeyGen::STATION_MASK = 0x00000007;
const ULong_t TRpcKeyGen::OCTANT_MASK = 0x0000000F;
const ULong_t TRpcKeyGen::HALF_OCTANT_MASK = 0x00000003;
const ULong_t TRpcKeyGen::RSEG_MASK = 0x0000000F;
const ULong_t TRpcKeyGen::INDEX_MASK = 0x0000FFFF; 

const ULong_t TRpcKeyGen::INDEX_SHIFT=0;
const ULong_t TRpcKeyGen::RSEG_SHIFT = TRpcKeyGen::INDEX_SHIFT + TRpcKeyGen::INDEX_BITS;
const ULong_t TRpcKeyGen::HALF_OCTANT_SHIFT= TRpcKeyGen::RSEG_SHIFT + TRpcKeyGen::RSEG_BITS;
const ULong_t TRpcKeyGen::OCTANT_SHIFT= TRpcKeyGen::HALF_OCTANT_SHIFT  + TRpcKeyGen::HALF_OCTANT_BITS;
const ULong_t TRpcKeyGen::STATION_SHIFT= TRpcKeyGen::OCTANT_SHIFT + TRpcKeyGen::OCTANT_BITS;
const ULong_t TRpcKeyGen::ARM_SHIFT = TRpcKeyGen::STATION_SHIFT + TRpcKeyGen::STATION_BITS;
	
//______________________________________________________
PHKey::object_key_type TRpcKeyGen::get_key(
		UShort_t arm, 
		UShort_t station,
		UShort_t octant, 
		UShort_t halfoctant, 
		UShort_t rseg,  
		UShort_t index)
{
	// shift and mask					 
	return	
			((ARM_MASK & arm) << ARM_SHIFT) |
			((STATION_MASK & station) << STATION_SHIFT) |
			((OCTANT_MASK & octant) << OCTANT_SHIFT) |
			((HALF_OCTANT_MASK & halfoctant) << HALF_OCTANT_SHIFT) |
			((RSEG_MASK & rseg) << RSEG_SHIFT) |
			((INDEX_MASK & index) << INDEX_SHIFT);
}

//______________________________________________________
TRpcKeyGen::key_range TRpcKeyGen::get_key_range(
		UShort_t arm, 
		UShort_t station,
		UShort_t octant,
		UShort_t halfoctant,
		UShort_t rseg)
{
						
	return std::make_pair(
		     get_key(arm, station, octant, halfoctant, rseg, 0), 
		     get_key(arm, station, octant, halfoctant, rseg+1, 0)-1); 
}

//__________________________________________________________
TRpcKeyGen::key_range TRpcKeyGen::get_key_range(UShort_t arm, 
						UShort_t station)
{ 
	return std::make_pair(get_key(arm,
				station,
				0,
				0,
				0,
				0),
				get_key(arm,
				station+1,
				0,
				0,
				0,
				0)-1); 
}

//__________________________________________________________
TRpcKeyGen::key_range TRpcKeyGen::get_key_range(UShort_t arm, 
						UShort_t station, 
						UShort_t octant)
{ 
	return std::make_pair(get_key(arm,
				station,
				octant,
				0,
				0,
				0),
				get_key(arm,
				station,
				octant+1,
				0,
				0,
				0)-1); 
}

//______________________________________________________
PHKey::object_key_type TRpcKeyGen::get_key(UShort_t arm, UShort_t index)
{
	// shift and mask					 
	return	
			((ARM_MASK & arm) << ARM_SHIFT) |
			((INDEX_MASK & index) << INDEX_SHIFT);
}

//______________________________________________________
TRpcKeyGen::key_range TRpcKeyGen::get_key_range(UShort_t arm)
{
	return std::make_pair(
			get_key(arm, 0), 
			get_key(arm+1, 0)-1); 
}
