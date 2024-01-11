/*!
	\file TMuiIndex.cxx
	\brief returns unique integer associated to muid panel location
	\author Hugo Pereira
	\version $Revision: 1.1 $
	\date $Date: 2006/05/25 06:00:25 $
*/

#include "TMuiIndex.h"

const unsigned long TMuiIndex::ARM_BITS=2;
const unsigned long TMuiIndex::PLANE_BITS=4;
const unsigned long TMuiIndex::PANEL_BITS=4;
	
const unsigned long TMuiIndex::ARM_MASK = 0x00000003;
const unsigned long TMuiIndex::PLANE_MASK = 0x0000000F;
const unsigned long TMuiIndex::PANEL_MASK = 0x0000000F;

const unsigned long TMuiIndex::PANEL_SHIFT = 0;
const unsigned long TMuiIndex::PLANE_SHIFT = TMuiIndex::PANEL_SHIFT + TMuiIndex::PANEL_BITS;
const unsigned long TMuiIndex::ARM_SHIFT = TMuiIndex::PLANE_SHIFT + TMuiIndex::PLANE_BITS;
