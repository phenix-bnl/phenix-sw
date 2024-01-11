// $Id: TMuiCluster3D.h,v 1.1 2006/04/22 01:59:30 hpereira Exp $
#ifndef _TMUICLUSTER3D_H_
#define _TMUICLUSTER3D_H_

/*!
  \file    TMuiCluster3D.h
  \brief   3D object for muid clusters
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:59:30 $
*/

#include "TMuiClusterMapO.h"

#include <iostream>
#include <PHObj3D.h>

/*!
  \class TMuiCluster3D
  \brief 3D object for muid clusters
*/

class TMuiCluster3D:public PHObj3D 
{

  public:
  //! constructor
  TMuiCluster3D( TNode* parent, const TMuiClusterMapO::value_type& cluster ):
    PHObj3D( parent ),
    _cluster( cluster )
  { 
    _line_color = 4;
    _line_width = 1;
    _is_valid = true; 
  }
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
  
  //! mutoo coordinate object
  TMuiClusterMapO::value_type _cluster;

};

#endif
