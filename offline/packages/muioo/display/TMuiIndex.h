// $Id: TMuiIndex.h,v 1.3 2006/05/25 06:55:39 hpereira Exp $

#ifndef __TMUIINDEX_H__
#define __TMUIINDEX_H__

/*!
	\file TMuiIndex.h
	\brief returns unique integer associated to muid panel location
	\author Hugo Pereira
	\version $Revision: 1.3 $
	\date $Date: 2006/05/25 06:55:39 $
*/

#include <MUIOO.h>
#include <iostream>

/*!
	\class TMuiIndex
	\brief returns unique integer associated to muid panel location
*/
class TMuiIndex {
	
	public:
	
	//! constructor
	TMuiIndex( 
		unsigned long arm, 
		unsigned long plane, 
		unsigned long panel )
	{
		update( arm, plane, panel );
		if( !check() ) std::cerr << "TMuiIndex::TMuiIndex - invalid index " << *this << std::endl;
	}
	
	//! check indexes
	bool check( void )
	{
		if( arm() >= MUIOO::MAX_ARM ) return false;
		if( plane() >= MUIOO::MAX_PLANE ) return false;
		if( panel() >= MUIOO::MAX_PANEL ) return false;
		return true;
	}

	//! equal to operator
	bool operator == ( const TMuiIndex& index ) const
	{ return _index == index._index; }

		//! equal to operator
	bool operator < ( const TMuiIndex& index ) const
	{ return _index < index._index; }

	//! print
	friend std::ostream& operator << (std::ostream &out,const TMuiIndex &index)
	{
		out << "(" << index.arm() << "," << index.plane() << "," << index.panel() << ") (" << index._index << ")";
		return out;
	}
	
	//! return arm
	int arm( void ) const
	{ return (_index >> ARM_SHIFT ) & ARM_MASK; }
	
	//! return station
	int plane( void ) const 
	{ return (_index >> PLANE_SHIFT ) & PLANE_MASK; }
	
	//! return octant
	int panel( void ) const 
	{ return (_index >> PANEL_SHIFT ) & PANEL_MASK; }
 
	private:

	//! get index from locator
	void update( unsigned long arm, unsigned long plane, unsigned long panel )
	{ 
		_index = 
			(arm << ARM_SHIFT)|
			(plane << PLANE_SHIFT)|
			(panel << PANEL_SHIFT); 
	} 
	
	//! muioo unique index
	unsigned long _index;
	
	// bits are calculated to match max value + 1
	static const unsigned long ARM_BITS;
	static const unsigned long PLANE_BITS;
	static const unsigned long PANEL_BITS;
	
	static const unsigned long ARM_MASK;
	static const unsigned long PLANE_MASK;
	static const unsigned long PANEL_MASK;

	static const unsigned long PANEL_SHIFT;
	static const unsigned long PLANE_SHIFT;
	static const unsigned long ARM_SHIFT;
	
};

#endif
