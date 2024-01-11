// $Id: TMutStub3D.h,v 1.3 2007/04/11 18:31:25 hpereira Exp $
#ifndef _TMUTSTUB3D_H_
#define _TMUTSTUB3D_H_

/*!
  \file    TMutStub3D.h
  \brief   3D object for mutr stubs
  \author  Hugo Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2007/04/11 18:31:25 $
*/
#include <string>
#include <sstream>
#include <iostream>
#include <TMutStubMap.h>

#include "PHObj3D.h"

/*! \ingroup display */
/*!
  \class TMutStub3D
  \brief 3D object for muid road
*/

class TMutStub3D:public PHObj3D 
{

  public:
  TMutStub3D( TNode* parent, const TMutStubMap::value_type & stub ):
    PHObj3D( parent ),
    _stub( stub )
  { 
    _line_color = 6;
    _line_width = 2;
    _is_valid = true; 
  }
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
    
  //! mutoo track object
  TMutStubMap::value_type _stub;
  
};

#endif

