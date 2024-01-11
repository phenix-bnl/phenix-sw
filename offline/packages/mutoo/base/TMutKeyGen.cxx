// $Id: TMutKeyGen.cxx,v 1.5 2011/12/24 04:48:19 slash Exp $

/*!
	\file TMutKeyGen.h
	\brief static class to generate keys for forward vertex objects
s	\author H. Pereira Da Costa
	\version $Revision: 1.5 $
	\date $Date: 2011/12/24 04:48:19 $
*/


#include<TMutKeyGen.h>

const ULong_t TMutKeyGen::ARM_BITS=2;
const ULong_t TMutKeyGen::STATION_BITS=3;
const ULong_t TMutKeyGen::OCTANT_BITS=4;
const ULong_t TMutKeyGen::HALF_OCTANT_BITS=2;
const ULong_t TMutKeyGen::GAP_BITS=3;
const ULong_t TMutKeyGen::CATHODE_BITS=2;
const ULong_t TMutKeyGen::INDEX_BITS=16;	
	
const ULong_t TMutKeyGen::ARM_MASK = 0x00000003;
const ULong_t TMutKeyGen::STATION_MASK = 0x00000007;
const ULong_t TMutKeyGen::OCTANT_MASK = 0x0000000F;
const ULong_t TMutKeyGen::HALF_OCTANT_MASK = 0x00000003;
const ULong_t TMutKeyGen::GAP_MASK = 0x00000007;
const ULong_t TMutKeyGen::CATHODE_MASK = 0x00000003;
const ULong_t TMutKeyGen::INDEX_MASK = 0x0000FFFF; 

const ULong_t TMutKeyGen::INDEX_SHIFT=0;
const ULong_t TMutKeyGen::CATHODE_SHIFT = TMutKeyGen::INDEX_BITS + TMutKeyGen::INDEX_SHIFT;
const ULong_t TMutKeyGen::GAP_SHIFT = TMutKeyGen::CATHODE_BITS + TMutKeyGen::CATHODE_SHIFT;
const ULong_t TMutKeyGen::HALF_OCTANT_SHIFT = TMutKeyGen::GAP_SHIFT + TMutKeyGen::GAP_BITS;
const ULong_t TMutKeyGen::OCTANT_SHIFT = TMutKeyGen::HALF_OCTANT_SHIFT + TMutKeyGen::HALF_OCTANT_BITS;
const ULong_t TMutKeyGen::STATION_SHIFT = TMutKeyGen::OCTANT_SHIFT + TMutKeyGen::OCTANT_BITS;
const ULong_t TMutKeyGen::ARM_SHIFT = TMutKeyGen::STATION_SHIFT + TMutKeyGen::STATION_BITS;

//__________________________________________________________
PHKey::object_key_type TMutKeyGen::get_key(unsigned short arm, 
						 unsigned short station, 
						 unsigned short octant, 
						 unsigned short half_octant, 
						 unsigned short gap,
						 unsigned short cathode,
						 unsigned short index)
{
	// shift and mask					 
	//
	return	
			((ARM_MASK & arm) << ARM_SHIFT) |
			((STATION_MASK & station) << STATION_SHIFT) |
			((OCTANT_MASK & octant) << OCTANT_SHIFT) |
			((HALF_OCTANT_MASK & half_octant) << HALF_OCTANT_SHIFT) |
			((GAP_MASK & gap) << GAP_SHIFT) |
			((CATHODE_MASK & cathode) << CATHODE_SHIFT) |
			((INDEX_MASK & index) << INDEX_SHIFT);
}

//__________________________________________________________
TMutKeyGen::key_range TMutKeyGen::get_key_range(unsigned short arm, 
						unsigned short station, 
						unsigned short octant, 
						unsigned short half_octant, 
						unsigned short gap,
						unsigned short cathode){
						
	return std::make_pair(get_key(arm,
				station,
				octant,
				half_octant,
				gap,
				cathode,
				0),
				get_key(arm,
				station,
				octant,
				half_octant,
				gap,
				cathode+1,
				0)-1); 
}

//__________________________________________________________
PHKey::object_key_type TMutKeyGen::get_key(unsigned short arm, 
						 unsigned short station, 
						 unsigned short octant, 
						 unsigned short half_octant, 
						 unsigned short gap,
						 unsigned short index)
{
	// shift and mask					 
	//
	return 
			((ARM_MASK & arm) << ARM_SHIFT) |
			((STATION_MASK & station) << STATION_SHIFT) |
			((OCTANT_MASK & octant) << OCTANT_SHIFT) |
			((HALF_OCTANT_MASK & half_octant) << HALF_OCTANT_SHIFT) |
			((GAP_MASK & gap) << GAP_SHIFT) |
			((INDEX_MASK & index) << INDEX_SHIFT);
}

//__________________________________________________________
TMutKeyGen::key_range TMutKeyGen::get_key_range(unsigned short arm, 
						unsigned short station, 
						unsigned short octant, 
						unsigned short half_octant, 
						unsigned short gap)
{
						
						
						
	return std::make_pair(get_key(arm,
				station,
				octant,
				half_octant,
				gap,
				0),
				get_key(arm,
				station,
				octant,
				half_octant,
				gap+1,
				0)-1); 
}

//__________________________________________________________
PHKey::object_key_type TMutKeyGen::get_key(unsigned short arm, 
						 unsigned short station, 
						 unsigned short octant, 
						 unsigned short half_octant, 
						 unsigned short index)
{
	// shift and mask					 
	//
	return
			((ARM_MASK & arm) << ARM_SHIFT) |
			((STATION_MASK & station) << STATION_SHIFT) |
			((OCTANT_MASK & octant) << OCTANT_SHIFT) |
			((HALF_OCTANT_MASK & half_octant) << HALF_OCTANT_SHIFT) |
			((INDEX_MASK & index) << INDEX_SHIFT);
}

//__________________________________________________________
TMutKeyGen::key_range TMutKeyGen::get_key_range(unsigned short arm, 
						unsigned short station, 
						unsigned short octant)
{ 
	return std::make_pair(get_key(arm,
				station,
				octant,
				0,
				0),
				get_key(arm,
				station,
				octant+1,
				0,
				0)-1); 
}

//__________________________________________________________
TMutKeyGen::key_range TMutKeyGen::get_key_range(unsigned short arm, 
						unsigned short station, 
						unsigned short octant, 
						unsigned short half_octant)
{
	return std::make_pair(get_key(arm,
				station,
				octant,
				half_octant,
				0),
				get_key(arm,
				station,
				octant,
				half_octant+1,
				0)-1); 
}

//__________________________________________________________
PHKey::object_key_type TMutKeyGen::get_key(unsigned short arm, 
					   unsigned short octant, 
					   unsigned short index)
{
	// shift and mask					 
	//
	return 
	  ((ARM_MASK & arm) << ARM_SHIFT) |
	  ((OCTANT_MASK & octant) << OCTANT_SHIFT) |
	  ((INDEX_MASK & index) << INDEX_SHIFT);
}

//__________________________________________________________
TMutKeyGen::key_range TMutKeyGen::get_key_range(unsigned short arm, unsigned short octant)
{ 
  return std::make_pair(get_key(arm,
				      octant,
				      0),
			      get_key(arm,
				      octant+1,
				      0)-1); 
}

//__________________________________________________________
PHKey::object_key_type TMutKeyGen::get_key(unsigned short arm, unsigned short index)
{
	// shift and mask					 
	return 
	  ((ARM_MASK & arm) << ARM_SHIFT) | 
	  ((INDEX_MASK & index) << INDEX_SHIFT);
}

//__________________________________________________________
TMutKeyGen::key_range TMutKeyGen::get_key_range(unsigned short arm)
{ return std::make_pair(get_key(arm,0), get_key(arm+1,0)-1); }








