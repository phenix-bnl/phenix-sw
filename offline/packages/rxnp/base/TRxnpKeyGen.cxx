// $Id: TRxnpKeyGen.cxx,v 1.3 2006/12/08 20:20:52 zhangc Exp $

/*!
	\file TRxnpKeyGen.cxx
	\brief static class to generate keys for rxnp objects
	\author C. Zhang
	\version $Revision: 1.3 $
	\date		$Date: 2006/12/08 20:20:52 $
*/
	
#include "TRxnpKeyGen.h"

// Note: we must define sufficients bits 
// for arm_max+1.. ring_max+1 etc
const ULong_t TRxnpKeyGen::ARM_BITS=2;
const ULong_t TRxnpKeyGen::RING_BITS=2;
const ULong_t TRxnpKeyGen::STRIP_BITS= 5;
const ULong_t TRxnpKeyGen::INDEX_BITS=2;	
	
const ULong_t TRxnpKeyGen::ARM_MASK = 0x00000003;
const ULong_t TRxnpKeyGen::RING_MASK = 0x00000003;
const ULong_t TRxnpKeyGen::STRIP_MASK = 0x0000000F;
const ULong_t TRxnpKeyGen::INDEX_MASK = 0x00000003; 

const ULong_t TRxnpKeyGen::INDEX_SHIFT=0;
const ULong_t TRxnpKeyGen::STRIP_SHIFT=	TRxnpKeyGen::INDEX_SHIFT + TRxnpKeyGen::INDEX_BITS;
const ULong_t TRxnpKeyGen::RING_SHIFT = TRxnpKeyGen::STRIP_SHIFT + TRxnpKeyGen::STRIP_BITS;
const ULong_t TRxnpKeyGen::ARM_SHIFT = TRxnpKeyGen::RING_SHIFT + TRxnpKeyGen::RING_BITS;

//______________________________________________________
PHKey::object_key_type TRxnpKeyGen::get_key(UShort_t arm, 
					    UShort_t ring, 
					    UShort_t strip, 
					    UShort_t index)
{
  // shift and mask					 
  return	
    ((ARM_MASK & arm) << ARM_SHIFT) |
    ((RING_MASK & ring) << RING_SHIFT) |
    ((STRIP_MASK & strip) << STRIP_SHIFT) |
    ((INDEX_MASK & index) << INDEX_SHIFT);
}

//______________________________________________________
PHKey::object_key_type TRxnpKeyGen::get_key(UShort_t arm, 
					    UShort_t ring, 
					    UShort_t index)
{
  // shift and mask					 
  return	
    ((ARM_MASK & arm) << ARM_SHIFT) |
    ((RING_MASK & ring) << RING_SHIFT) |
    ((INDEX_MASK & index) << INDEX_SHIFT);
}

//______________________________________________________
PHKey::object_key_type TRxnpKeyGen::get_key(UShort_t arm, 
					    UShort_t index)
{
  // shift and mask					 
  return	
    ((ARM_MASK & arm) << ARM_SHIFT) |
    ((INDEX_MASK & index) << INDEX_SHIFT);
}

//______________________________________________________
TRxnpKeyGen::key_range TRxnpKeyGen::get_key_range(UShort_t arm, 
						  UShort_t ring,
						  UShort_t strip)
{
						
  return std::make_pair(get_key(arm, ring, strip, 0), 
			get_key(arm, ring, strip+1, 0)-1); 
}
	
//______________________________________________________
TRxnpKeyGen::key_range TRxnpKeyGen::get_key_range(UShort_t arm, 
						  UShort_t ring)
{
						
  return std::make_pair(get_key(arm, ring, 0), 
			get_key(arm, ring+1, 0)-1); 
}

//______________________________________________________
TRxnpKeyGen::key_range TRxnpKeyGen::get_key_range(UShort_t arm)
{
  return std::make_pair(get_key(arm, 0), 
			get_key(arm+1, 0)-1); 
}
