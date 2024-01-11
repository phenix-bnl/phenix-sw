// $Id: TMuiRoad3D.cxx,v 1.1 2006/04/22 01:59:31 hpereira Exp $

/*!
  \file    TMuiRoad3D.cxx
  \brief   3D object for mutr tracks
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:59:31 $
*/

#include <TMutTrackUtil.h>
#include "TMuiRoad3D.h"

//_________________________________________________
void TMuiRoad3D::_make_nodes( void )
{
  if( !_is_valid ) return;
  
  // retrieve parameters
  const TMutFitPar* fit_par = _road.get()->get_const_fitpar();

  PHPoint first_point( TMutTrackUtil::linear_track_model( fit_par, fit_par->get_z_begin() ) );
  PHPoint second_point( TMutTrackUtil::linear_track_model( fit_par, fit_par->get_z_end() ) );
  
  // make road segment
  if( _parent ) _parent->cd();
  _make_segment( first_point, second_point, 1 );
  return;
}
