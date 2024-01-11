// $Id: TMuiRoad3D.h,v 1.1 2006/04/22 01:59:31 hpereira Exp $
#ifndef _TMUIROAD3D_H_
#define _TMUIROAD3D_H_

/*!
  \file    TMuiRoad3D.h
  \brief   3D object for muid road
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:59:31 $
*/
#include <string>
#include <sstream>
#include <iostream>
#include <PHObj3D.h>
#include <TMuiRoadMapO.h>


/*!
  \class TMuiRoad3D
  \brief 3D object for muid road
*/

class TMuiRoad3D:public PHObj3D 
{

  public:
  TMuiRoad3D( TNode* parent, const TMuiRoadMapO::value_type & road ):
    PHObj3D( parent ),
    _road( road )
  { 
    _line_color = 2;
    _is_valid = true; 
  }
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
    
  //! mutoo track object
  TMuiRoadMapO::value_type _road;
  
};

#endif

