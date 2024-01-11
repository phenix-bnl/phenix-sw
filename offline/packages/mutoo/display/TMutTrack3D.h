// $Id: TMutTrack3D.h,v 1.3 2007/04/11 18:31:26 hpereira Exp $
#ifndef _TMUTTRACK3D_H_
#define _TMUTTRACK3D_H_

/*!
  \file    TMutTrack3D.h
  \brief   3D object for mutr tracks
  \author  Hugo Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2007/04/11 18:31:26 $
*/
#include <string>
#include <sstream>
#include <iostream>
#include <TMutTrkMap.h>

#include "PHObj3D.h"

/*! \ingroup display */
/*!
  \class TMutTrack3D
  \brief 3D object for mutr tracks
*/

class TMutTrack3D:public PHObj3D 
{


  public:
  TMutTrack3D( TNode* parent, const TMutTrkMap::value_type & trk ):
    PHObj3D( parent ),
    _trk( trk )
  { 
    _line_color = 2;
    _is_valid = true; 
  }
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
    
  //! mutoo track object
  TMutTrkMap::value_type _trk;
  
};

#endif
