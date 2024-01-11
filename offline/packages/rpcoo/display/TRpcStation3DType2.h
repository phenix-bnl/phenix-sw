// $Id: TRpcStation3DType2.h,v 1.1 2006/04/22 02:01:39 hpereira Exp $
#ifndef _TRpcStation3DType2_H_
#define _TRpcStation3DType2_H_

/*!
   \file    TRpcStation3DType2.h
   \brief   3D object for Type2 RPC station (x,y)
   \author  H. Pereira Da Costa
   \version $Revision: 1.1 $
   \date    $Date: 2006/04/22 02:01:39 $
*/
#include <string>
#include <iostream>

#include <PHObj3D.h>

#include "TRpcIndex.h"

/*! @ingroup display */
//! 3D object for Type2 RPC station (x,y)
class TRpcStation3DType2:public PHObj3D 
{

  public:
  
  //! constructor from location
  TRpcStation3DType2( const TRpcIndex& index, TNode* parent );
    
  //! equal operator
  bool operator == (const TRpcStation3DType2& obj ) const
  { return _index == obj._index; }
 
  //! print geometry information
  void print( std::ostream& out = std::cout ) const;
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
   
	//! station index
  TRpcIndex _index; 
	
	//! object name
  std::string _name;

};

#endif
