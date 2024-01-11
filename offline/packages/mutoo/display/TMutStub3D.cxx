// $Id: TMutStub3D.cxx,v 1.2 2004/02/24 14:28:13 hpereira Exp $

/*!
  \file    TMutStub3D.cxx
  \brief   3D object for mutr tracks
  \author  Hugo Pereira
  \version $Revision: 1.2 $
  \date    $Date: 2004/02/24 14:28:13 $
*/

#include "TMutStub3D.h"
#include <list>
#include <TMutTrackUtil.h>

//_________________________________________________
void TMutStub3D::_make_nodes( void )
{
  if( !_is_valid ) return;
  
  // retrieve parameters
  const TMutFitPar* fit_par = _stub.get()->get_fit_par();

  double z_begin = fit_par->get_z_begin() ? fit_par->get_z_begin():fit_par->get_z();
  double z_end = fit_par->get_z_end() ? fit_par->get_z_end():fit_par->get_z();

  PHPoint first_point( TMutTrackUtil::linear_track_model( fit_par,  z_begin ) );
  PHPoint second_point( TMutTrackUtil::linear_track_model( fit_par, z_end ) );
  
  // make road segment
  if( _parent ) _parent->cd();
  _make_segment( first_point, second_point, 1 );
  return;
}
