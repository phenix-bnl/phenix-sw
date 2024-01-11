// $Id: TFvtxMCHit3D.h,v 1.1 2006/05/29 04:29:52 hpereira Exp $
#ifndef _TFvtxMCHit3D_H_
#define _TFvtxMCHit3D_H_

/*!
	 \file TFvtxMCHit3D.h
	 \brief 3D object for forward vertex mc_hit
	 \author H. Pereira Da Costa
	 \version $Revision: 1.1 $
	 \date $Date: 2006/05/29 04:29:52 $
*/
#include <string>
#include <iostream>

#include <PHObj3D.h>
#include <TFvtxMCHitMap.h>


/*! \brief 3D object for mutr cathode */
class TFvtxMCHit3D:public PHObj3D 
{

	public:
	
	//! constructor from location
	TFvtxMCHit3D( TNode* parent, const TFvtxMCHitMap::value_type& mc_hit ):
		PHObj3D( parent ),
		_mc_hit( mc_hit )
	{ 
		_line_color = 2;
		_line_width = 2;
		_is_valid = true; 
	}
	
	private:
	
	//! create nodes and shapes
	void _make_nodes( void );
	 	
	//! pointer to forward vertex mc_hit structure
	TFvtxMCHitMap::value_type _mc_hit;
			
};


#endif
