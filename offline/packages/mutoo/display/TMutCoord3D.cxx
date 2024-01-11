// $Id: TMutCoord3D.cxx,v 1.1 2003/12/03 09:44:13 hpereira Exp $

/*!
  \file    TMutCoord3D.cxx
  \brief   3D object for mutr coordinates
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2003/12/03 09:44:13 $
*/

#include "TMutCoord3D.h"
#include <MUTOO.h>
#include <TTUBE.h>
#include <PHVector.h>

//_________________________________________________
void TMutCoord3D::_make_nodes( void )
{
  if( !_is_valid ) return;
  if( _parent ) _parent->cd();
  _make_segment( _coord.get()->get_coord_begin(), _coord.get()->get_coord_end(), _coord.get()->get_error() );
  return;
}
