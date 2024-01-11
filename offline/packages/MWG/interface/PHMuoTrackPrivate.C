// $Id: PHMuoTrackPrivate.C,v 1.1 2009/07/04 18:32:21 hpereira Exp $
/*!
  \file    PHMuoTrackPrivate.h
  \brief   stores track information that must be hidden to root
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2009/07/04 18:32:21 $
*/

#include "PHMuoTrackPrivate.h"

//_________________________________________________________
PHMuoTrackPrivate::PHMuoTrackPrivate( void ):
  _n_primitives( 0 ),
  _n_mutr_primitives( 0 ),
  _event_vertex_z( 0 ),
  _event_vertex_z_error( 0 )
{
  
  // level2 primitive angles
  for( unsigned int i = 0; i<_lvl2dim; i++ )
  {
    _level2_phi[i] = 0;
    _level2_theta[i] = 0;
    
    _level2_pmin_x[i] = 0;
    _level2_pmin_y[i] = 0;
    _level2_pmin_z[i] = 0;

    _level2_pmax_x[i] = 0;
    _level2_pmax_y[i] = 0;
    _level2_pmax_z[i] = 0;
  }
  
  // reaction plane angles
  for( unsigned int i=0; i<_rpdim; i++ )
  { _event_rp_angle[i] = 0; } 
  
}
