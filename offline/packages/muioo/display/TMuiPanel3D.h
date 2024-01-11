// $Id: TMuiPanel3D.h,v 1.2 2007/01/07 20:21:09 hpereira Exp $
#ifndef _TMUIPANEL3D_H_
#define _TMUIPANEL3D_H_

/*!
   \file    TMuiPanel3D.h
   \brief   3D object for muid panel
   \author  Hugo Pereira
   \version $Revision: 1.2 $
   \date    $Date: 2007/01/07 20:21:09 $
*/

#include <string>
#include <iostream>

#include "TMuiIndex.h"

#include <PHObj3D.h>
#include <MuiGeomClasses.hh>
#include <TMuiPanelGeo.hh>

/*!
  \class TMuiPanel3D
  \brief 3D object for muid panel
*/


class TMuiPanel3D:public PHObj3D
{

  public:

  //! constructor from location
  TMuiPanel3D( const TMuiIndex& index, TNode* parent );

  //! equal operator
  bool operator == (const TMuiPanel3D& obj ) const
  { return _index == obj._index; }

  //! print geometrical information
  void print( std::ostream &out = std::cout ) const;

  private:

  //! create nodes and shapes
  void _make_nodes( void );

  //! pointer to panel structure
  const TMuiPanelGeo* _panel_ptr;

  //! location
  TMuiIndex _index;

  //! object name
  std::string _name;

};

#endif
