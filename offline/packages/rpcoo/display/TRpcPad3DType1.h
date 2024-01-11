// $Id: TRpcPad3DType1.h,v 1.1 2006/04/22 02:01:38 hpereira Exp $
#ifndef _TRpcPad3DType1_h_
#define _TRpcPad3DType1_h_

/*!
   \file    TRpcPad3DType1.h
   \brief   3D object for type1 RPC pad (r,phi)
   \author  H. Pereira Da Costa
   \version $Revision: 1.1 $
   \date    $Date: 2006/04/22 02:01:38 $
*/
#include <string>
#include <iostream>

#include <PHObj3D.h>

#include "TRpcIndex.h"

/*! @ingroup display */
//! 3D object for type1 RPC pad (r,phi)
class TRpcPad3DType1:public PHObj3D 
{

  public:
  
  //! constructor from location
  TRpcPad3DType1( UShort_t arm, UShort_t station, UShort_t index, TNode* parent ):
			PHObj3D( parent ),
  		_arm( arm ), 
  		_station( station ), 
  		_index( index ), 
  		_name( "unnamed" )
	{

  	_line_color = 4;
		_line_width = 1;
		_is_valid = true; 
  
	}

  
  //! print geometry information
  void print( std::ostream& out = std::cout ) const;
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
	
	//! arm
	UShort_t _arm;
	
	//! station
	UShort_t _station;
	
	//! strip index
	UShort_t _index;
		
	//! object name
  std::string _name;

};

#endif
