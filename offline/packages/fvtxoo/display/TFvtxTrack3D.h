// $Id: TFvtxTrack3D.h,v 1.1 2011/02/15 17:32:04 youzy Exp $
#ifndef _TFVTXTRACK3D_H_
#define _TFVTXTRACK3D_H_

/*!
  \file    TFvtxTrack3D.h
  \brief   3D object for fvtx tracks
  \author  Zhengyun
  \version $Revision: 1.1 $
  \date    $Date: 2011/02/15 17:32:04 $
*/
#include <string>
#include <sstream>
#include <iostream>
#include <TFvtxTrkMap.h>

#include "PHObj3D.h"

/*! \ingroup display */
/*!
  \class TFvtxTrack3D
  \brief 3D object for fvtx tracks
*/

class TFvtxTrack3D:public PHObj3D 
{


  public:
  TFvtxTrack3D( TNode* parent, const TFvtxTrkMap::value_type & trk ):
    PHObj3D( parent ),
    _trk( trk )
  { 
    _line_color = 2;
    _is_valid = true; 
  }
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
    
  //! fvtxoo track object
  TFvtxTrkMap::value_type _trk;
  
};

#endif
