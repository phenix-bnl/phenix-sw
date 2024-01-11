	
/*!
	\file TFvtxIndex.cxx
	\brief returns unique integer associated to plane location
	\author H. Pereira Da Costa
	\version $Revision: 1.5 $
	\date $Date: 2011/02/08 23:10:54 $
*/

#include "TFvtxIndex.h"

const unsigned long TFvtxIndex::ARM_BITS=2;
const unsigned long TFvtxIndex::CAGE_BITS=2;
const unsigned long TFvtxIndex::STATION_BITS=3;
const unsigned long TFvtxIndex::SECTOR_BITS=5;
const unsigned long TFvtxIndex::COLUMN_BITS = 2;
	
// mask to retrieve index from global bit map
const unsigned long TFvtxIndex::ARM_MASK     = 0x00000003;
const unsigned long TFvtxIndex::CAGE_MASK    = 0x00000003;
const unsigned long TFvtxIndex::STATION_MASK = 0x00000007;
const unsigned long TFvtxIndex::SECTOR_MASK  = 0x0000001F;
const unsigned long TFvtxIndex::COLUMN_MASK  = 0x00000003;

	// position of the different indexes in the global bit map
const unsigned long TFvtxIndex::COLUMN_SHIFT  = 0;
const unsigned long TFvtxIndex::SECTOR_SHIFT  = TFvtxIndex::COLUMN_SHIFT + TFvtxIndex::COLUMN_BITS;
const unsigned long TFvtxIndex::STATION_SHIFT = TFvtxIndex::SECTOR_SHIFT + TFvtxIndex::SECTOR_BITS;
const unsigned long TFvtxIndex::CAGE_SHIFT    = TFvtxIndex::STATION_SHIFT + TFvtxIndex::STATION_BITS;
const unsigned long TFvtxIndex::ARM_SHIFT     = TFvtxIndex::CAGE_SHIFT + TFvtxIndex::CAGE_BITS;
