// $Id: TFvtxSector3D.h,v 1.1 2006/05/25 13:10:33 hpereira Exp $
#ifndef _TFvtxSector3D_H_
#define _TFvtxSector3D_H_

/*!
	 \file TFvtxSector3D.h
	 \brief 3D object for forward vertex sector
	 \author H. Pereira Da Costa
	 \version $Revision: 1.1 $
	 \date $Date: 2006/05/25 13:10:33 $
*/
#include <string>
#include <iostream>

#include <PHObj3D.h>
#include <FvtxSector.h>
#include <TFvtxIndex.h>


/*! \brief 3D object for mutr cathode */
class TFvtxSector3D:public PHObj3D 
{

	public:
	
	//! constructor from location
	TFvtxSector3D( const TFvtxIndex& index, TNode* parent );
		
	//! equal operator
	bool operator == (const TFvtxSector3D& obj ) const
	{ return _index == obj._index; }
 
	//! print geometry information
	void print( std::ostream& out = std::cout ) const;
	
	private:
	
	//! create nodes and shapes
	void _make_nodes( void );
	 
	//! location	
	TFvtxIndex _index; 
	
	//! object name
	std::string _name;		 
	
	//! pointer to forward vertex sector structure
	FvtxSector* _sector_ptr;
			
};


#endif
