
/*!
	\file TMutIndex.cxx
	\brief returns unique integer associated to plane location
	\author H. Pereira Da Costa
	\version $Revision: 1.3 $
	\date $Date: 2006/05/25 06:00:06 $
*/

#include "TMutIndex.h"

const unsigned long TMutIndex::ARM_BITS=2;
const unsigned long TMutIndex::STATION_BITS=3;
const unsigned long TMutIndex::OCTANT_BITS=4;
const unsigned long TMutIndex::HALF_BITS=2;
const unsigned long TMutIndex::GAP_BITS=3;

const unsigned long TMutIndex::ARM_MASK = 0x00000003;
const unsigned long TMutIndex::STATION_MASK = 0x00000007;
const unsigned long TMutIndex::OCTANT_MASK = 0x0000000F;
const unsigned long TMutIndex::HALF_MASK = 0x00000003;
const unsigned long TMutIndex::GAP_MASK = 0x00000007;

const unsigned long TMutIndex::GAP_SHIFT = 0;
const unsigned long TMutIndex::HALF_SHIFT = TMutIndex::GAP_SHIFT + TMutIndex::GAP_BITS;
const unsigned long TMutIndex::OCTANT_SHIFT = TMutIndex::HALF_SHIFT + TMutIndex::HALF_BITS;
const unsigned long TMutIndex::STATION_SHIFT = TMutIndex::OCTANT_SHIFT + TMutIndex::OCTANT_BITS;
const unsigned long TMutIndex::ARM_SHIFT = TMutIndex::STATION_SHIFT + TMutIndex::STATION_BITS;
