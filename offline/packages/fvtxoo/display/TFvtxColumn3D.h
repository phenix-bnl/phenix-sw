// $Id: TFvtxColumn3D.h,v 1.2 2006/05/26 20:34:02 hpereira Exp $
#ifndef _TFvtxColumn3D_H_
#define _TFvtxColumn3D_H_

/*!
	 \file TFvtxColumn3D.h
	 \brief 3D object for forward vertex column
	 \author H. Pereira Da Costa
	 \version $Revision: 1.2 $
	 \date $Date: 2006/05/26 20:34:02 $
*/
#include <string>
#include <iostream>

#include <PHObj3D.h>
#include <FvtxColumn.h>
#include <TFvtxIndex.h>


/*! \brief 3D object for mutr cathode */
class TFvtxColumn3D:public PHObj3D 
{

	public:
	
	//! constructor from location
	TFvtxColumn3D( const TFvtxIndex& index, TNode* parent );
		
	//! equal operator
	bool operator == (const TFvtxColumn3D& obj ) const
	{ return _index == obj._index; }
 
	//! draw strips
	void set_draw_strips( const bool& value ) 
	{ _draw_strips = value; }
	
	//! print geometry information
	void print( std::ostream& out = std::cout ) const;
	
	private:
	
	//! create nodes and shapes
	void _make_nodes( void );
		
	//! location	
	TFvtxIndex _index; 
	
	//! object name
	std::string _name;		 

	//! true if strips are to be drawn
	bool _draw_strips;
	
	//! pointer to forward vertex column structure
	FvtxColumn* _column_ptr;
					
};


#endif
