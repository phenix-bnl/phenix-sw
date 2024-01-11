// $Id: TMutCoord3D.h,v 1.4 2007/04/11 18:31:25 hpereira Exp $
#ifndef _TMUTCOORD3D_H_
#define _TMUTCOORD3D_H_

/*!
	\file TMutCoord3D.h
	\brief 3D object for mutr coordinates
	\author Hugo Pereira
	\version $Revision: 1.4 $
	\date $Date: 2007/04/11 18:31:25 $
*/

#include <iostream>
#include <TMutCoordMap.h>
#include "PHObj3D.h"

/*! \ingroup display */
/*!
	\class TMutCoord3D
	\brief 3D object for mutr coordinates
*/

class TMutCoord3D:public PHObj3D 
{

	public:
	//! constructor
	TMutCoord3D( TNode* parent, const TMutCoordMap::value_type& coord ):
		PHObj3D( parent ),
		_coord( coord )
	{ 
		_line_color = 4;
		_line_width = 1;
		_is_valid = true; 
	}
		
	private:
	
	//! create nodes and shapes
	void _make_nodes( void );
	
	//! mutoo coordinate object
	TMutCoordMap::value_type _coord;

};

#endif
