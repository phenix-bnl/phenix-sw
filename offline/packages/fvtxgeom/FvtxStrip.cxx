// $Id: FvtxStrip.cxx,v 1.10 2015/09/14 15:28:52 snowball Exp $

/*!
 \file FvtxStrip.cxx
 \brief Forward vertex Strip geometry
 Initialize and provide access to FVTX planes
 \author Hugo Pereira da costa
 \version $Revision: 1.10 $
 \date $Date: 2015/09/14 15:28:52 $
 */

#include <cmath>
#include "FvtxStrip.h"
#include <PHGeometry.h>
#include <FVTXOO.h>
#include "FvtxGeom.h"

using namespace std;

//_____________________________________________________________
double
FvtxStrip::get_path_length(const PHPoint& point_in, const PHPoint& point_out)
{

  /*
  cout << "get_center         (" << get_center().getX()         << ", " << get_center().getY()         << ", " << get_center().getZ()         << ")" << endl;
  cout << "get_position_begin (" << get_position_begin().getX() << ", " << get_position_begin().getY() << ", " << get_position_begin().getZ() << ")" << endl;
  cout << "get_position_end   (" << get_position_end().getX()   << ", " << get_position_end().getY()   << ", " << get_position_end().getZ()   << ")" << endl;
  cout << "get_position_begin_local_strip (" << get_position_begin_local_strip().getX() << ", " << get_position_begin_local_strip().getY() << ", " << get_position_begin_local_strip().getZ() << ")" << endl;
  cout << "get_position_end_local_strip   (" << get_position_end_local_strip().getX()   << ", " << get_position_end_local_strip().getY()   << ", " << get_position_end_local_strip().getZ()   << ")" << endl;
  cout << "point_in           (" << point_in.getX()             << ", " << point_in.getY()             << ", " << point_in.getZ()             << ")" << endl;
  cout << "point_out          (" << point_out.getX()            << ", " << point_out.getY()            << ", " << point_out.getZ()            << ")" << endl;
  */
  PHPoint point_in_strip = column_to_strip(point_in);
  PHPoint point_out_strip = column_to_strip(point_out);

  //cout << "point_in_strip           (" << point_in_strip.getX()             << ", " << point_in_strip.getY()             << ", " << point_in_strip.getZ()             << ")" << endl;
  //cout << "point_out_strip          (" << point_out_strip.getX()            << ", " << point_out_strip.getY()            << ", " << point_out_strip.getZ()            << ")" << endl;

  // get normalized vector perpendicular to the strip
  //PHVector v = PHVector(get_position_end_local_strip().getX() - get_position_begin_local_strip().getX(),
  //			get_position_end_local_strip().getY() - get_position_begin_local_strip().getY(), 0).orthogonal();
  //v.normalize();

  PHVector v = PHVector(0,0,1);

  // get algebric distance from point_in to the middle of the strip
  double d_in = PHVector(point_in_strip.getX() - get_position_begin_local_strip().getX(),
			 point_in_strip.getY() - get_position_begin_local_strip().getY(), 0).dot(v);

  // get algebric distance from point_out to the middle of the strip
  double d_out = PHVector(point_out_strip.getX() - get_position_begin_local_strip().getX(),
			  point_out_strip.getY() - get_position_begin_local_strip().getY(), 0).dot(v);

  // get traversal length
  double half_width = get_width() / 2;
  if (d_in == d_out)
    return (fabs(d_in) > half_width) ? 0 : 1;

  double distance(  FVTXGEOM::SIGN(d_out) * min(fabs(d_out), half_width)
		  - FVTXGEOM::SIGN(d_in) * min(fabs(d_in), half_width));

  //std::cout << "get_path_length: " << d_in << "  " << d_out << "  " << half_width<< "  " << distance << endl;

  if (distance == 0)
    {
      cout << "FvtxStrip::get_path_length - something wrong: distance is zero"
          << endl;
      cout << "FvtxStrip::get_path_length - d_in, d_out, half_width = " << d_in
          << "," << d_out << "," << half_width << endl;
      cout << "FvtxStrip::get_path_length - Print of strip follows..." << endl;
      print(cout);
    }

  return distance / (d_out - d_in);

}

//______________________________________________________________
double
FvtxStrip::get_angle() const
{
  return atan2((get_position_end().getY() - get_position_begin().getY()),
	       (get_position_end().getX() - get_position_begin().getX()));
}

//______________________________________________________________
//! get sign of w_absolute relative to the r direction
int
FvtxStrip::get_w_sign() const
{
  // from double TFvtxCoord::get_w_absolute() const

  // Point on z axis
  //
  static PHVector z_axis(0, 0, 1);
  static PHVector x_axis(1, 0, 0);

  PHPoint coord_begin = get_position_begin();
  PHPoint coord_end = get_position_end();

  PHPoint z_axis_point(0, 0, get_z());

  PHLine coord_line(coord_begin, coord_end);

  // Vector to point on coordinate closest to z axis
  //
  PHPoint close_point(
      PHGeometry::closestApproachLinePoint(coord_line, z_axis_point));
  PHVector close_vector(close_point);

  // Sign from cross product
  int sign = -FVTXOO::SIGN(close_vector.cross(x_axis).dot(z_axis));

  if (sign == 0)
    {
      cerr
          << "Oops! FvtxStrip::get_w_sign() = 0 due to the cockamamie sign definition"
          << endl;
    }

  return sign;
}

//! set TGeoPhysicalNode with this GeomObject
void
FvtxStrip::set_phy_node(TGeoPhysicalNode *node)
{
  FvtxGeomObject::set_phy_node(node);

//    //! strip width
  _width =
      2
          * ((TGeoTrd1*) (_phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()))->GetDz();
//
//    //! thickness
  _thickness =
      2
          * ((TGeoTrd1*) (_phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()))->GetDy();
//
//    //! strip begin position
//    PHPoint _position_begin;
    {

      PHPoint local(0.0, 0.0, 0.0);
      local.setX(
          -0.5
              * (((TGeoTrd1*) (_phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()))->GetDx1()
                  + ((TGeoTrd1*) (_phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()))->GetDx2()));
      _position_begin = local_to_global(local);
      _position_begin_local_column = strip_to_column(_position_begin);
      _position_begin_local_strip = local;
    }
//
//    //! strip end position
//    PHPoint _position_end;
    {
      PHPoint local(0.0, 0.0, 0.0);
      local.setX(
          0.5
              * (((TGeoTrd1*) (_phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()))->GetDx1()
                  + ((TGeoTrd1*) (_phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()))->GetDx2()));
      //local.setY(_thickness);
      //local.setZ(_width);
      _position_end = local_to_global(local);
      _position_end_local_column = strip_to_column(_position_end);
      _position_end_local_strip = local;

    }

  return;
}

//! GeoData_t -> FvtxGeomObject
const FvtxStrip::GeoDataRec_t &
FvtxStrip::apply_geometry_data(const FvtxStrip::GeoData_t & data)
{
  GeoData_t::const_iterator iter = data.find(local_strip_index());

  if (iter == data.end())
    {
      cout
          << "FvtxStrip::apply_geometry_data - ERROR - missing geometry data record for index = "
          << index() << " and unique index = " << unique_index() << endl;
    }

  const GeoDataRec_t & rec = (*iter).second;

  GeoDataRec_t::const_iterator i;

  i = rec.find("_width");
  if (i == rec.end())
    {
      throw std::runtime_error(
          "FvtxStrip::apply_geometry_data - Missing _width");
    }
  _width = (*i).second;

  i = rec.find("_thickness");
  if (i == rec.end())
    {
      throw std::runtime_error(
          "FvtxStrip::apply_geometry_data - Missing _thickness");
    }
  _thickness = (*i).second;

  FvtxColumn * col =
      FvtxGeom::get_arm(index().arm())->get_cage(index().cage())->get_station(
          index().station())->get_sector(index().sector())->get_column(
          index().column());
  PHPoint p;

  apply_geometry_data_to_PHPoint("_position_begin_local", p, rec);
  _position_begin = col->local_to_global(p);
  apply_geometry_data_to_PHPoint("_position_end_local", p, rec);
  _position_end = col->local_to_global(p);

  return rec;
}

//! FvtxGeomObject -> GeoData_t
FvtxStrip::GeoDataRec_t &
FvtxStrip::save_geometry_data(FvtxStrip::GeoData_t & data) const
{
  GeoData_t::iterator iter = data.find(local_strip_index());

  if (iter != data.end())
    {
      return iter->second;
    }

  GeoDataRec_t & rec = data[local_strip_index()];

  rec["_width"] = _width;
  rec["_thickness"] = _thickness;

  FvtxColumn * col =
      FvtxGeom::get_arm(index().arm())->get_cage(index().cage())->get_station(
          index().station())->get_sector(index().sector())->get_column(
          index().column());

  save_geometry_data_from_PHPoint("_position_begin_local",
      col->global_to_local(_position_begin), rec);
  save_geometry_data_from_PHPoint("_position_end_local",
      col->global_to_local(_position_end), rec);

  return rec;
}

//! FvtxGeomObject -> GeoData_t
FvtxStrip::GeoDataRec_t &
FvtxStrip::save_geometry_data_and_check(FvtxStrip::GeoData_t & data) const
{
  GeoData_t::iterator iter = data.find(local_strip_index());

  if (iter != data.end())
    {
      GeoDataRec_t & rec = data[local_strip_index()];

      if (rec["_width"] != _width)
        {
          cout << "FvtxStrip::save_geometry_data " << unique_index()
              << " _width inconsistency" << endl;
        }
      if (rec["_thickness"] != _thickness)
        {
          cout << "FvtxStrip::save_geometry_data " << unique_index()
              << " _thickness inconsistency" << endl;
        }

      FvtxColumn * col = FvtxGeom::get_arm(index().arm())->get_cage(
          index().cage())->get_station(index().station())->get_sector(
          index().sector())->get_column(index().column());

      PHPoint p;

      apply_geometry_data_to_PHPoint("_position_begin_local", p, rec);

      if (!((p) == col->global_to_local(_position_begin)))
        {
          cout << "FvtxStrip::save_geometry_data " << unique_index()
              << " _position_begin inconsistency: "<<p<<" - "<< col->global_to_local(_position_begin)<< endl;
        }

      apply_geometry_data_to_PHPoint("_position_end_local", p, rec);

      if (!((p) == col->global_to_local(_position_end)))
        {
          cout << "FvtxStrip::save_geometry_data " << unique_index()
              << " _position_end inconsistency" <<p<<" - "<< col->global_to_local(_position_end)<< endl;
        }

      return rec;
    }

  GeoDataRec_t & rec = data[local_strip_index()];

  rec["_width"] = _width;
  rec["_thickness"] = _thickness;

  FvtxColumn * col =
      FvtxGeom::get_arm(index().arm())->get_cage(index().cage())->get_station(
          index().station())->get_sector(index().sector())->get_column(
          index().column());

  save_geometry_data_from_PHPoint("_position_begin_local",
      col->global_to_local(_position_begin), rec);
  save_geometry_data_from_PHPoint("_position_end_local",
      col->global_to_local(_position_end), rec);

  return rec;
}
