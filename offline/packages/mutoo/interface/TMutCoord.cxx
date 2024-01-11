
// $Id: TMutCoord.cxx,v 1.8 2010/01/15 23:41:59 hpereira Exp $

/*!
   \file TMutCoord.hh
   \brief Class for Muon Tracker 1D Cathode Coordinates
   \author S. Kelly
   \version $Revision: 1.8 $
   \date $Date: 2010/01/15 23:41:59 $
*/

#include<PHGeometry.h>

#include "TMutCoord.hh"

ClassImp(TMutCoord)

//
PHClassId::id_type TMutCoord::_class_id( 0 );

//________________________________________________________
double TMutCoord::get_w_absolute() const
{

  // Point on z axis
  static PHVector z_axis(0,0,1);
  static PHVector x_axis(1,0,0);

  PHPoint z_axis_point(0,0,get_mean_z());

  // Vector to point on coordinate closest to z axis
  PHPoint close_point(PHGeometry::closestApproachLinePoint(get_coord(),z_axis_point));
  PHVector close_vector(close_point);

  // Sign from cross product
  int sign = -MUTOO::SIGN( close_vector.cross(x_axis).dot(z_axis));

  // return signed distance
  return double(sign)*PHGeometry::distancePointToPoint(z_axis_point,close_point);

}
