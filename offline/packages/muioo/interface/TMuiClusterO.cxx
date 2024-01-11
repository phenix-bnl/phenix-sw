// $Id: TMuiClusterO.cxx,v 1.1 2006/04/22 01:58:27 hpereira Exp $
#include<TMuiClusterO.h>
ClassImp(TMuiClusterO)

//INCLUDECHECKER: Removed this line: #include <PHGeometry.h>
#include <MuiCommon.hh>
#include <TMuiGeo.h>

//________________________________________________
double TMuiClusterO::get_w_absolute() const
{
	double angle( TMuiGeo::get_panel_angle( get_location() ) );
	PHVector u( -sin( angle ), cos( angle ), 0 );
	PHVector v( get_coord_begin() );
	return u.dot( v );
			
}

//________________________________________________
double TMuiClusterO::get_error() const
{
	
	/*
		note: since the centroid error is always expressed in local coordinate,
		there is no need to rotate it back, unlike get_w_absolute.
	*/
	
	return (
		get_orientation() == kHORIZ ? 
			get_centroidsigma().getY():
			get_centroidsigma().getX() );
}
