// $Id: TMutGap3D.h,v 1.4 2006/05/25 06:00:06 hpereira Exp $
#ifndef _TMutGap3D_H_
#define _TMutGap3D_H_

/*!
	 \file TMutGap3D.h
	 \brief 3D object for mutr gap
	 \author H. Pereira Da Costa
	 \version $Revision: 1.4 $
	 \date $Date: 2006/05/25 06:00:06 $
*/
#include <string>
#include <iostream>

#include "PHObj3D.h"
#include "TMutIndex.h"
#include <MutOctant.h>
#include <MutGap.h>

/*! \brief 3D object for mutr cathode */
class TMutGap3D:public PHObj3D {

	public:
	
	//! constructor from location
	TMutGap3D( const TMutIndex& index, TNode* parent );
		
	//! equal operator
	bool operator == (const TMutGap3D& obj ) const
	{ return _index == obj._index; }
 
	//! print geometry information
	void print( std::ostream& out = std::cout ) const;
	
	private:
	
	//! create nodes and shapes
	void _make_nodes( void );
	 
	//! location	
	TMutIndex _index; 
	
	//! object name
	std::string _name;		 
	
	//! pointer to mutr octant structure
	MutOctant* _octant_ptr;
	
	//! pointer to mutr gap structure
	MutGap* _gap_ptr;		 
			
};


#endif
