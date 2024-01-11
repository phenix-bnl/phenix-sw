// $Id: TMutMCTrack3D.h,v 1.3 2007/04/11 18:31:25 hpereira Exp $
#ifndef _TMUTMCTRACK3D_H_
#define _TMUTMCTRACK3D_H_

/*!
  \file    TMutMCTrack3D.h
  \brief   3D object for mutr mc tracks
  \author  Hugo Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2007/04/11 18:31:25 $
*/
#include <string>
#include <sstream>
#include <iostream>
#include <list>
#include <TMutMCTrkMap.h>

#include "PHObj3D.h"

/*! \ingroup display */
/*!
  \class TMutMCTrack3D
  \brief 3D object for mutr mc tracks
*/

class TMutMCTrack3D:public PHObj3D 
{


  public:
  
  //! constructor
  TMutMCTrack3D( 
    TNode* parent, 
    const TMutMCTrkMap::value_type & mc_trk,
    bool draw_mut_hits = true, bool draw_mui_hits = true ):
    PHObj3D( parent ),
    _mc_trk( mc_trk ),
    _draw_mut_hits( draw_mut_hits ),
    _draw_mui_hits( draw_mui_hits )
  { 
    _is_valid = true; 
    _line_color = 3;
  
  }
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
    
  //! mutoo mc track object
  TMutMCTrkMap::value_type _mc_trk;
  
  bool _draw_mut_hits;  //!< if true, mut mc hits are drawn
  bool _draw_mui_hits;  //!< if true, mui mc hits are drawn
  
};

#endif
