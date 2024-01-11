// $Id: TMutWire3D.h,v 1.1 2007/04/11 18:31:26 hpereira Exp $
#ifndef TMutWire3D_h
#define TMutWire3D_h

/*!
	\file TMutWire3D.h
	\brief 3D object for mutr anode wires
	\author Hugo Pereira
	\version $Revision: 1.1 $
	\date $Date: 2007/04/11 18:31:26 $
*/

#include <iostream>
#include "TMutIndex.h"
#include "PHObj3D.h"

class MutWire;

/*! \ingroup display */
/*!
	\class TMutWire3D
	\brief 3D object for mutr coordinates
*/

class TMutWire3D:public PHObj3D 
{

	public:
	
  //! constructor
	TMutWire3D(  const TMutIndex& index, const unsigned int& wire_id, TNode* parent );
		
	//! equal operator
	bool operator == (const TMutWire3D& obj ) const
	{ return _index == obj._index && _wire_id == obj._wire_id; }
 
	//! print geometry information
	void print( std::ostream& out = std::cout ) const;
  
	private:
	
	//! create nodes and shapes
	void _make_nodes( void );
	 
	//! location	
	TMutIndex _index; 
	
  //! wire index
  unsigned int _wire_id;
  
	//! object name
	std::string _name;		 
	
	//! pointer to mutr wire geom structure
	MutWire* _wire_ptr;

};

#endif
