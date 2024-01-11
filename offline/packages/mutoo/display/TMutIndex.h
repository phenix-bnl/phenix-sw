// $Id: TMutIndex.h,v 1.7 2006/05/25 06:58:02 hpereira Exp $

#ifndef __TMUTINDEX_H__
#define __TMUTINDEX_H__

/*!
	\file TMutIndex.h
	\brief returns unique integer associated to plane location
	\author H. Pereira Da Costa
	\version $Revision: 1.7 $
	\date $Date: 2006/05/25 06:58:02 $
*/

#include <MUTOO.h>
#include <iostream>

/*!
	\class TMutIndex
	\brief returns unique integer associated to mutr cathode location
*/

class TMutIndex {
	
	public:
	
	enum { MAX_GAP=3 };
		
	//! constructor
	TMutIndex( 
		unsigned long arm, 
		unsigned long station, 
		unsigned long octant, 
		unsigned long half, 
		unsigned long gap )
	{
		update( arm, station, octant, half, gap );
		if( !check() ) std::cout << "TMutIndex::TMutIndex - invalid index " << *this << std::endl;
	}
	
	//! check indexes
	bool check( void )
	{
		if( arm() >= (unsigned long)MUTOO::MAX_ARM ) return false;
		if( station() >= (unsigned long) MUTOO::MAX_STATION ) return false;
		if( octant() >= (unsigned long) MUTOO::MAX_OCTANT ) return false;
		if( half() >= (unsigned long) MUTOO::MAX_HALF_OCTANT ) return false;
		if( gap() >= ( ( station() == MUTOO::Station3 ) ? 2:3 ) ) return false;
		return true;
	}

	//! equal to operator
	bool operator == ( const TMutIndex& index ) const
	{ return _index == index._index; }

		//! equal to operator
	bool operator < ( const TMutIndex& index ) const
	{ return _index < index._index; }

	//! print
	friend std::ostream& operator << (std::ostream &out,const TMutIndex &index)
	{
		out << "(" << index.arm() << "," << index.station() << "," << index.octant() << "," << index.half() << "," << index.gap() << ") (" << index._index << ")";
		return out;
	}
	
	//! return arm
	unsigned long arm( void ) const
	{ return (_index >> ARM_SHIFT) & ARM_MASK; }
	
	//! return station
	unsigned long station( void ) const 
	{ return (_index >> STATION_SHIFT) & STATION_MASK; }
	
	//! return octant
	unsigned long octant( void ) const 
	{ return (_index >> OCTANT_SHIFT) & OCTANT_MASK; }
	
	//! return half octant
	unsigned long half( void ) const 
	{ return (_index >> HALF_SHIFT) & HALF_MASK; }
	
	//! return gap
	unsigned long gap( void ) const 
	{ return (_index >> GAP_SHIFT) & GAP_MASK; }

	private:

	//! create index from location
	void update( unsigned long arm, unsigned long station, unsigned long octant, unsigned long half, unsigned long gap )
	{
		_index = 
			( arm << ARM_SHIFT )|
			( station << STATION_SHIFT )|
			( octant << OCTANT_SHIFT )|
			( half << HALF_SHIFT )|
			( gap << GAP_SHIFT );
	}
	
	//! unique id
	int _index;	

	static const unsigned long ARM_BITS;
	static const unsigned long STATION_BITS;
	static const unsigned long OCTANT_BITS;
	static const unsigned long HALF_BITS;
	static const unsigned long GAP_BITS;
	
	static const unsigned long ARM_MASK;
	static const unsigned long STATION_MASK;
	static const unsigned long OCTANT_MASK;
	static const unsigned long HALF_MASK;
	static const unsigned long GAP_MASK;

	static const unsigned long GAP_SHIFT;
	static const unsigned long HALF_SHIFT;
	static const unsigned long OCTANT_SHIFT;
	static const unsigned long STATION_SHIFT;
	static const unsigned long ARM_SHIFT;

};

#endif
