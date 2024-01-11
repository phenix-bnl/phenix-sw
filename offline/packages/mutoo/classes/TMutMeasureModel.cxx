// $Id: TMutMeasureModel.cxx,v 1.7 2011/12/24 04:48:21 slash Exp $

/*!
  \file    TMutMeasureModel.cxx.
  \brief   utility functions to calculate muon arm gap measurement
  \author  S.Kelly
  \version $Revision: 1.7 $
  \date    $Date: 2011/12/24 04:48:21 $
*/

#include<TMutGeo.h>
#include<TMutMeasureModel.h>
#include<TMutFitPar.hh>
#include<TMutCoordMap.h>
#include<PHGeometry.h>
#include<MutWire.h>
#include<gsl/gsl_math.h>
#include<gsl/gsl_diff.h>
#include<MUTOO.h>

//___________________________________________________________________________
double TMutMeasureModel::cathode_measure(
  const PHPoint& point, 
  const TMutCoord* coord_ptr, 
  bool anode_project)
{

  // Get PHLine from coordinate
  PHLine coord_line = coord_ptr->get_coord();
  
  // Get the direction from input projective coordinate, this will become the y axis 
  // in the prime coordinate system.
  PHVector y_prime = coord_line.getDirection();

  // Base point of input coord_line
  PHPoint coord_point = coord_line.getBasepoint();

  // Determine the angle to rotate into to prime coordinates (WZ coords)
  double phi = -1.0*atan2(y_prime.getX(),y_prime.getY());
  double cos_phi = std::cos(phi);
  double sin_phi = std::sin(phi);

  double w_trk=0;
  
  if(anode_project) {
    // Project input point onto anode
    //
    unsigned short arm = coord_ptr->get_arm();
    PHPoint anode_proj = get_anode_projection(arm,point);
    
    // Rotate projected point into prime coordinates (This is the w coord of the track)
    //
    w_trk = anode_proj.getX()*cos_phi + anode_proj.getY()*sin_phi;
  } else {
    // Not using anode projected measurement model, just calc dca between track and projection
    //
    w_trk = point.getX()*cos_phi + point.getY()*sin_phi;
  }

  // Rotate input coord_point into the prime coordinates (This is the w coord of the offset)
  //
  double w_meas = coord_point.getX()*cos_phi + coord_point.getY()*sin_phi;
  
  // The residual is the difference between w_trk and w_measured (signed)
  //
  double residual = w_trk - w_meas;

  return residual;
}

//___________________________________________________________________________
double TMutMeasureModel::anode_measure(const PHPoint& trk_point, const TMutGapCoord* gap_ptr)
{
  // Get location and anode number from gap coordinate
  MUTOO::gap_locator location = gap_ptr->get_location();
  unsigned short anode_number = gap_ptr->get_anode();
  
  // Get wire geometry
  //
  MutWire* wire_ptr = TMutGeo::get_wire_geom(location,anode_number);
  PHLine wire_line(wire_ptr->getGlobalPositionBegin(),
		   wire_ptr->getGlobalPositionEnd());
  
  // Base point of input wire
  //
  PHPoint wire_point = wire_line.getBasepoint();
  
  // Get the direction from input projective coordinate, this will become 
  // the y axis in the prime coordinate system.
  //
  PHVector y_prime = wire_line.getDirection();
  
  // Determine the angle to rotate into to prime coordinates (y_prime ~ wire direction)
  //
  double phi = -1.0*atan2(y_prime.getX(),y_prime.getY());
  double cos_phi = std::cos(phi);
  double sin_phi = std::sin(phi);
  
  // Rotate point into prime coordinates 
  //
  double r_trk = trk_point.getX()*cos_phi + trk_point.getY()*sin_phi;
  double r_meas = gap_ptr->get_coord().getX()*cos_phi + gap_ptr->get_coord().getY()*sin_phi;

  // Note -- we formerly used the  anode wire dca but this introduced a 
  // sensitivity to the anode wire positions/geometry.  Now we use the 
  // prime distance between the and the track and the gap coordinate.
  //
  //  double r_meas = wire_point.getX()*cos_phi + wire_point.getY()*sin_phi;

  // The residual is the difference between r_trk and r_meas (signed)
  //
  double residual = r_trk - r_meas;
  
  return residual;
}


//___________________________________________________________________________
double TMutMeasureModel::d_cathode_measure_dx(const PHPoint& point, 
				       const TMutCoord* coord,
				       bool anode_project)
{  
  gsl_function F;
  cathode_function_data params = {point,coord,anode_project};
  F.function = &cathode_measure_x;
  F.params = &params;

  double derivative = 0;
  double err = 0;  

  // Here we use gsl forward difference to calculate the partial
  // derivative of "measure" with respect to x. Where x is the
  // x coordintate of "point", and the derivate is evaluated at
  // point.
  //
  gsl_diff_central(&F, point.getX(), &derivative, &err);

  return derivative;
}

//___________________________________________________________________________
double TMutMeasureModel::d_anode_measure_dx(const PHPoint& point, const TMutGapCoord* gap)
{  
  gsl_function F;
  anode_function_data params = {point,gap};
  F.function = &anode_measure_x;
  F.params = &params;

  double derivative = 0;
  double err = 0;  

  // Here we use gsl forward difference to calculate the partial
  // derivative of "measure" with respect to x. Where x is the
  // x coordintate of "point", and the derivate is evaluated at
  // point.
  //
  gsl_diff_central(&F, point.getX(), &derivative, &err);

  return derivative;
}

//___________________________________________________________________________
double TMutMeasureModel::d_cathode_measure_dy(const PHPoint& point, 
				       const TMutCoord* coord,
				       bool anode_project)
{  
  gsl_function F;
  cathode_function_data params = {point,coord,anode_project};
  F.function = &cathode_measure_y;
  F.params = &params;

  double derivative = 0;
  double err = 0;  
  
  // Here we use gsl forward difference to calculate the partial
  // derivative of "measure" with respect to y. Where y is the
  // y coordintate of "point", and the derivate is evaluated at
  // point.
  //
  gsl_diff_central(&F, point.getY(), &derivative, &err);
  
  return derivative;
}

//___________________________________________________________________________
double TMutMeasureModel::d_anode_measure_dy(const PHPoint& point, const TMutGapCoord* gap)
{  
  gsl_function F;
  anode_function_data params = {point,gap};
  F.function = &anode_measure_y;
  F.params = &params;

  double derivative = 0;
  double err = 0;  

  // Here we use gsl forward difference to calculate the partial
  // derivative of "measure" with respect to x. Where x is the
  // x coordintate of "point", and the derivate is evaluated at
  // point.
  //
  gsl_diff_central(&F, point.getY(), &derivative, &err);

  return derivative;
}

//___________________________________________________________________________
PHPoint TMutMeasureModel::get_anode_projection(unsigned short arm, const PHPoint& point)
{
  // Project input PHPoint nearest anode.

  // Get the geometry of the nearest anode
  //
  MutWire* wire_ptr = TMutGeo::find_nearest_anode(arm,point);

  // If no anode is found then just return the original point
  //
  if(wire_ptr == 0) return point;
  
  // PHLine corresponding to wire
  //
  PHLine wire_line(wire_ptr->getGlobalPositionBegin(),
		   wire_ptr->getGlobalPositionEnd());
  
  // DCA between input point and anode wire 
  //
  PHPoint dca = PHGeometry::closestApproachLinePoint(wire_line,point);
  
  // Return PHPoint on anode corresponding to DCA
  //
  return dca;
}

//___________________________________________________________________________
double TMutMeasureModel::cathode_measure_x(double x, void* params)
{
  // Here we expose cathode_measure as a 1d function of the coordinate
  // x.  The y,z coordinates and the line are specified in the
  // parameter data structure function_data.  This is used by
  // gsl forward difference routines to calculate the derivate
  // w/r x numerically.
  cathode_function_data* data = reinterpret_cast<cathode_function_data*>(params);
  PHPoint point(x,data->point.getY(),data->point.getZ());
  return cathode_measure(point, data->coord, data->use_anodes);  
}

//___________________________________________________________________________
double TMutMeasureModel::anode_measure_x(double x, void* params)
{
  // Here we expose anode_measure as a 1d function of the coordinate
  // x.  The y,z coordinates and the line are specified in the
  // parameter data structure function_data.  This is used by
  // gsl forward difference routines to calculate the derivate
  // w/r x numerically.
  anode_function_data* data = reinterpret_cast<anode_function_data*>(params);
  PHPoint point(x,data->point.getY(),data->point.getZ());
  return anode_measure(point, data->gap);  
}

//___________________________________________________________________________
double TMutMeasureModel::cathode_measure_y(double y, void* params)
{
  // Here we expose measure as a 1d function of the coordinate
  // y.  The y,z coordinates and the line are specified in the
  // parameter data structure function_data.  This is used by gsl
  // forward difference routines to calcualte the derivate w/r
  // to x numerically
  //
  cathode_function_data* data = reinterpret_cast<cathode_function_data*>(params);
  PHPoint point(data->point.getX(),y,data->point.getZ());
  return cathode_measure(point, data->coord, data->use_anodes);  
}

//___________________________________________________________________________
double TMutMeasureModel::anode_measure_y(double y, void* params)
{
  // Here we expose anode_measure as a 1d function of the coordinate
  // y.  The y,z coordinates and the line are specified in the
  // parameter data structure function_data.  This is used by gsl
  // forward difference routines to calcualte the derivate w/r
  // to x numerically
  //
  anode_function_data* data = reinterpret_cast<anode_function_data*>(params);
  PHPoint point(data->point.getX(),y,data->point.getZ());
  return anode_measure(point, data->gap);  
}

//___________________________________________________________________________
double TMutMeasureModel::get_w_error(const  PHPoint& point, const PHPoint& error, const TMutCoord* coord)
{
  // Freshman error propagation: dw = sqrt((dM/dx dx)^2 + (dM/dy dy)^2)
  return std::sqrt(
    MUTOO::SQUARE(TMutMeasureModel::d_cathode_measure_dx(point,coord)*error.getX()) + 
    MUTOO::SQUARE(TMutMeasureModel::d_cathode_measure_dy(point,coord)*error.getY()));
}








