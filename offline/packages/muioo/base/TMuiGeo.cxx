// $Id: TMuiGeo.cxx,v 1.2 2007/02/13 13:45:54 hpereira Exp $
//////////////////////////////////////////////////////////////////
/*
  \file TMuiGeo.cxx
  \brief utility class to interface with the muigeom library
  \author H. Pereira 
  \version $Revision: 1.2 $
  \date    $Date: 2007/02/13 13:45:54 $
*/
//////////////////////////////////////////////////////////////////

#include <MuiGeomClasses.hh>  
#include <PHException.h>  
#include <sstream>

#include "TMuiGeo.h"

using namespace std;

//__________________________________________________
double TMuiGeo::get_panel_angle (
  UShort_t arm,
  UShort_t plane,
  UShort_t panel,
  UShort_t orientation )
{
  
  // retrieve panel geometry
  TMuiChannelId pPanelId(arm,plane,panel);
  TMuiPanelGeo* fpMuiPanelGeo = TMuiGeometry::Geom()->getPanel(pPanelId);
  
  // rotate i vector
  static const PHVector i( 1, 0, 0 );
  PHVector rotated( fpMuiPanelGeo->RotateToGlobal( i ) );
  
  
  // get angle depending on orientation
  double angle = (orientation == kHORIZ) ?
    atan2( rotated.getY(), rotated.getX() ):
    atan2( rotated.getX(), -rotated.getY() ); 
  
  return angle;
  
}

//__________________________________________________
bool TMuiGeo::is_in_panel (
  const PHPoint& point,
  UShort_t arm,
  UShort_t plane,
  UShort_t panel )

{
  
  // retrieve panel geometry
  TMuiChannelId pPanelId(arm,plane,panel);
  TMuiPanelGeo* fpMuiPanelGeo = TMuiGeometry::Geom()->getPanel(pPanelId);
  if( !fpMuiPanelGeo ) {
    ostringstream what; 
    what << "could not retrieve geometry for panel " << arm << "," << plane <<"," << panel;
    throw runtime_error( DESCRIPTION( what.str() ) );
  }
  
  // get panel dimensions
  float dx(0), dy(0), dz(0);
  fpMuiPanelGeo->Size( dx, dy, dz );		
    
  // transform point to local coordinate system
  PHPoint local( fpMuiPanelGeo->TransformToPanel( point ) );

  return (
    fabs(local.getX()) <= dx/2 &&
    fabs(local.getY()) <= dy/2 &&
    fabs(local.getZ()) <= dz/2
 	);
    
}
