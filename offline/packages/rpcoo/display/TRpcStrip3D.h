#ifndef _TRpcStrip3D_h_
#define _TRpcStrip3D_h_

/*!
   \file    TRpcStrip3D.h
   \brief   3D object for RPC strips
   \author  R. Hollis (UCR - rhollis@ucr.edu)
   \version 
   \date    
*/
#include <string>
#include <iostream>

#include <PHObj3D.h>

#include "TRpcIndex.h"
//#include "RpcStrip.h"

/*! @ingroup display */
//! 3D object for RPC strips
class TRpcStrip3D:public PHObj3D 
{

  public:
  
  //! constructor from location
  TRpcStrip3D( const TRpcIndex& index, TNode* parent );

  
  //! print geometry information
  void print( std::ostream& out = std::cout ) const;
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
  
  //! location
  TRpcIndex _index;

  //! object name
  std::string _name;
  
  //! RpcStrip pointer
  //RpcStrip *_strip_ptr;

};

#endif
