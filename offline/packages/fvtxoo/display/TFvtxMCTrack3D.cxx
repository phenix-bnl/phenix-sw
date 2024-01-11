// $Id: TFvtxMCTrack3D.cxx,v 1.2 2011/03/14 18:44:53 youzy Exp $

/*!
  \file    TFvtxMCTrack3D.cxx
  \brief   3D object for fvtx mc tracks
  \author  Zhengyun You
  \version $Revision: 1.2 $
  \date    $Date: 2011/03/14 18:44:53 $
*/

#include "TFvtxMCTrack3D.h"
#include <FVTXOO.h>
#include <PHKeyIterator.h>
#include <TFvtxMCHitMap.h>
#include <TTUBE.h>
#include <PHVector.h>

class TFvtxMCHit;

using namespace std;

//_________________________________________________
void TFvtxMCTrack3D::_make_nodes( void )
{
  if( !_is_valid ) return;

  // put track points into a list
  list< PHPoint > points;
  
  // adds vertex point
  points.push_back( PHPoint(
    _mc_trk.get()->get_x_orig(),
    _mc_trk.get()->get_y_orig(),
    _mc_trk.get()->get_z_orig()
  ) );

  // retrieves all fvtx MC hits
  TFvtxMCHitMap::key_iterator mc_hit_iter = _mc_trk.get()->get_associated<TFvtxMCHit>();
  //cout << "fvtx mc hit " <<  mc_hit_iter.count() << endl;
  while( TFvtxMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next() )
  {
    points.push_back( mc_hit_ptr->get()->get_coord() );
  }
  
  //! check size
  if( points.size() < 2 ) return;
  
  if( _parent ) _parent->cd();
  
  // process list, make segments for each pair
  list<PHPoint>::iterator p0 = points.begin();
  list<PHPoint>::iterator p1 = points.begin();
  p1++;
    
  while( p1 != points.end() ) {
    _make_segment( *p0, *p1, 0.1 );
    p0 = p1;
    p1++;
  }
  
  return;
}
