// $Id: TFvtxTrack3D.cxx,v 1.1 2011/02/15 17:32:04 youzy Exp $

/*!
  \file    TFvtxTrack3D.cxx
  \brief   3D object for fvtx tracks
  \author  Zhengyun
  \version $Revision: 1.1 $
  \date    $Date: 2011/02/15 17:32:04 $
*/

#include "TFvtxTrack3D.h"
#include <FVTXOO.h>
#include <list>

using namespace std;

//_________________________________________________
void TFvtxTrack3D::_make_nodes( void )
{
  if( !_is_valid ) return;

  // put track points into a list
  list< PHPoint > points;
  
  // adds vertex
  //if( !_trk.get()->get_status( TFvtxTrk::BOTH_ARMS ) )
  {
    points.push_back( PHPoint(
      _trk.get()->get_trk_par_vtx()->get_x(),
      _trk.get()->get_trk_par_vtx()->get_y(),
      _trk.get()->get_trk_par_vtx()->get_z()
      ) );
  }

  double trk_end_z = _trk.get()->get_trk_par_vtx()->get_pz() > 0 ? 40 : -40;
  double delta_z = trk_end_z - _trk.get()->get_trk_par_vtx()->get_z();
  points.push_back( PHPoint(
    _trk.get()->get_trk_par_vtx()->get_x() + _trk.get()->get_trk_par_vtx()->get_px() / _trk.get()->get_trk_par_vtx()->get_pz() * delta_z,
    _trk.get()->get_trk_par_vtx()->get_y() + _trk.get()->get_trk_par_vtx()->get_py() / _trk.get()->get_trk_par_vtx()->get_pz() * delta_z,
    trk_end_z
    ) );

  //! check size
  if( points.size() < 2 ) return;

  //! sort list
  points.sort( point_less_ftor() );

  if( _parent ) _parent->cd();

  // process list, make segments for each pair
  list<PHPoint>::iterator p0 = points.begin();
  list<PHPoint>::iterator p1 = points.begin();
  p1++;

  while( p1 != points.end() ) {
    _make_segment( *p0, *p1, 0.2 );
    p0 = p1;
    p1++;
  }
    
  return;
}
