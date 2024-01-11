// $Id: TMutMCTrack3D.cxx,v 1.2 2004/11/29 22:08:28 hpereira Exp $

/*!
  \file    TMutMCTrack3D.cxx
  \brief   3D object for mutr coordinates
  \author  Hugo Pereira
  \version $Revision: 1.2 $
  \date    $Date: 2004/11/29 22:08:28 $
*/

#include "TMutMCTrack3D.h"
#include <MUTOO.h>
#include <PHKeyIterator.h>
#include <TMutMCHitMap.h>
#include <TMuiMCHitMapO.h>
#include <TTUBE.h>
#include <PHVector.h>

using namespace std;

//_________________________________________________
void TMutMCTrack3D::_make_nodes( void )
{
  if( !_is_valid ) return;

  // put track points into a list
  list< PHPoint > points;
  
  // adds vertex point
  if( _draw_mut_hits || _draw_mui_hits )
  points.push_back( PHPoint(
    _mc_trk.get()->get_x_orig(),
    _mc_trk.get()->get_y_orig(),
    _mc_trk.get()->get_z_orig()
  ) );
  
  // retrieves all mutr MC hits
  if( _draw_mut_hits ) {
    TMutMCHitMap::key_iterator mc_hit_iter = _mc_trk.get()->get_associated<TMutMCHit>();
    while( TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next() )
    points.push_back( mc_hit_ptr->get()->get_coord() );
  }
  
  // retrieves all muid MC hits
  if( _draw_mui_hits ) {
    TMuiMCHitMapO::key_iterator mc_mui_hit_iter = _mc_trk.get()->get_associated<TMuiMCHitO>();
    while( TMuiMCHitMapO::pointer mc_mui_hit_ptr = mc_mui_hit_iter.next() )
    points.push_back( mc_mui_hit_ptr->get()->get_coord() );
  }
    
  //! check size
  if( points.size() < 2 ) return;
  
  if( _parent ) _parent->cd();
  
  // process list, make segments for each pair
  list<PHPoint>::iterator p0 = points.begin();
  list<PHPoint>::iterator p1 = points.begin();
  p1++;
    
  while( p1 != points.end() ) {
    _make_segment( *p0, *p1, 1 );
    p0 = p1;
    p1++;
  }
  
  return;
}
