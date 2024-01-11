#include <TFvtxCoord.h>
#include <FVTXOO.h>
#include <PHGeometry.h>

ClassImp(TFvtxCoord)

//_____________________________________________
PHPoint TFvtxCoord::get_coord_midpoint() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return (get_coord_begin() + get_coord_end()) * 0.5;
}

//_____________________________________________
double TFvtxCoord::get_w_absolute() const
{
  // Point on z axis
  //
  static PHVector z_axis(0,0,1);
  static PHVector x_axis(1,0,0);

  PHPoint z_axis_point(0,0,get_mean_z());

  // Vector to point on coordinate closest to z axis
  //
  PHPoint close_point(PHGeometry::closestApproachLinePoint(get_coord(),z_axis_point));
  PHVector close_vector(close_point);

  // Sign from cross product
  int sign = -FVTXOO::SIGN( close_vector.cross(x_axis).dot(z_axis) );

  // return signed distance
  return double(sign)*PHGeometry::distancePointToPoint(z_axis_point,close_point);

}



