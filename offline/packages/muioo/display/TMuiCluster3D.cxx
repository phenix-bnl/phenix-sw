// $Id: TMuiCluster3D.cxx,v 1.1 2006/04/22 01:59:30 hpereira Exp $

/*!
  \file    TMuiCluster3D.cxx
  \brief   3D object for muid clusters
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:59:30 $
*/

#include "TMuiCluster3D.h"

//_________________________________________________
void TMuiCluster3D::_make_nodes( void )
{
  if( !_is_valid ) return;
  if( _parent ) _parent->cd();
  _make_segment( _cluster.get()->get_coord_begin(), _cluster.get()->get_coord_end(), 1 );
  return;
}
