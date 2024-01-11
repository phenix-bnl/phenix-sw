// $Id: TFvtxMCTrack3D.h,v 1.1 2011/02/15 17:32:04 youzy Exp $
#ifndef _TFVTXMCTRACK3D_H_
#define _TFVTXMCTRACK3D_H_

/*!
  \file    TFvtxMCTrack3D.h
  \brief   3D object for fvtx mc tracks
  \author  Zhengyun You
  \version $Revision: 1.1 $
  \date    $Date: 2011/02/15 17:32:04 $
*/
#include <string>
#include <sstream>
#include <iostream>
#include <list>
#include <TMutMCTrkMap.h>

#include "PHObj3D.h"

/*! \ingroup display */
/*!
  \class TFvtxMCTrack3D
  \brief 3D object for fvtx mc tracks
*/

class TFvtxMCTrack3D:public PHObj3D 
{


  public:
  
  //! constructor
  TFvtxMCTrack3D( 
    TNode* parent, 
    const TMutMCTrkMap::value_type & mc_trk ):
    PHObj3D( parent ),
    _mc_trk( mc_trk )
  { 
    _is_valid = true; 
    _line_color = 3;
  
  }
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
    
  //! mutoo mc track object
  TMutMCTrkMap::value_type _mc_trk;
  
};

#endif
