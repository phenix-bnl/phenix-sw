// $Id: TMutGeo.cxx,v 1.7 2011/12/24 04:48:19 slash Exp $
//////////////////////////////////////////////////////////////////
/*
  \file TMutGeo.cxx
  \brief utility class to interface with the muigeom library
  \author S.Kelly 
  \version $Revision: 1.7 $
  \date    $Date: 2011/12/24 04:48:19 $
*/
//////////////////////////////////////////////////////////////////

#include <MUTGEOM.h>
#include <MutGeom.h>
#include <MutWire.h>
#include <MutStrip.h>
#include <PHPoint.h>
#include <PHVector.h>
#include <PHCylPoint.h>
#include <PHGeometry.h>

#include <gsl/gsl_math.h>

#include "TMutGeo.h"
#include "MUTOO.h"
#include "PHException.h"

using namespace std;

//__________________________________________________
MutStrip* TMutGeo::get_strip_geom(
  const unsigned short& arm,
  const unsigned short& station,
  const unsigned short& octant,
  const unsigned short& half_octant,
  const unsigned short& gap,
  const unsigned short& cathode,
  const unsigned short& strip ) 
{
  
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();
  unsigned short plane = (cathode == 0) ? MUTGEOM::Cathode1 : MUTGEOM::Cathode2;
  
  // return a const pointer to the requested strip object
  return geometry->
    f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[plane]->
    f_pMutStrips[strip];  
}

//__________________________________________________
PHVector TMutGeo::get_strip_direction(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap,
  unsigned short cathode,
  unsigned short strip) 
{
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();
  unsigned short plane = (cathode == 0) ? MUTGEOM::Cathode1 : MUTGEOM::Cathode2;
  
  // Get begining and end of strip
  // 
  PHPoint begin = geometry->
    f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[plane]->
    f_pMutStrips[strip]->getGlobalPositionBegin();
  
  PHPoint end = geometry->
    f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[plane]->
    f_pMutStrips[strip]->getGlobalPositionEnd();
  
  // return the normalized tangent to strip
  PHLine line(end,begin);
  line.normalize();
  return line.getDirection();
}

//__________________________________________________
PHVector TMutGeo::get_strip_direction(
  const MUTOO::cathode_locator& location,
  unsigned short strip) 
{
  unsigned short arm = location.get<0>();
  unsigned short station = location.get<1>(); 
  unsigned short octant = location.get<2>();
  unsigned short half_octant = location.get<3>();
  unsigned short gap = location.get<4>();
  unsigned short cathode = location.get<5>();  
  return get_strip_direction(arm,station,octant,half_octant,gap,cathode,strip);
}

//__________________________________________________
MutWire* TMutGeo::get_wire_geom(
  const unsigned short& arm,
  const unsigned short& station,
  const unsigned short& octant,
  const unsigned short& half_octant,
  const unsigned short& gap,
  const unsigned short& wire)
{
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();
  return geometry->
    f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[MUTOO::Wire]->
    f_pMutWires[wire];  
}

//__________________________________________________
const set<int>& TMutGeo::get_disabled_anode_cards(
  unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap)
            
{
  
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();
  return geometry->
    f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[MUTOO::Wire]->getDisabledAnodeCards();  
}

//__________________________________________________
double TMutGeo::get_cathode_angle(unsigned short arm, 
  unsigned short station,
  unsigned short octant, 
  unsigned short half_octant,
  unsigned short gap, 
  unsigned short cathode,
  unsigned short strip)
{

  MutStrip* strip_ptr = TMutGeo::get_strip_geom(arm,
    station,
    octant,
    half_octant,
    gap,
    cathode,
    strip);
  double angle = strip_ptr->getAngle();
  
  // Ensure that the range is -pi/2, pi/2
  //
  angle = (angle < -M_PI_2) ? angle + M_PI : angle;
  angle = (angle > M_PI_2) ? angle - M_PI : angle;
  
  return angle;
}


//__________________________________________________
unsigned short TMutGeo::get_n_gaps(unsigned short arm, unsigned short station) 
{
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();
  return  geometry->
    f_pMutStations[station]->
    f_pMutOctants[0]->
    f_pMutHalfOctants[0]->getNumberOfGaps();
}
  
//__________________________________________________
bool TMutGeo::in_fiducial(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  const PHPoint& point) 
{

  // figure out which arm
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();

  // get a pointer to the octant object
  MutOctant* oct_ptr = geometry->f_pMutStations[station]->f_pMutOctants[octant];
  
  // transform input point to cylindrical coordindates
  PHCylPoint cyl_point;
  PHGeometry::cartesianToCylindrical(point,cyl_point);

  const double R_TOLERANCE = 5.0; // cm
  const double PHI_TOLERANCE = 1.0; // cm

  // check R range first (simpler)
  if(cyl_point.getR() < oct_ptr->getInnerRadius() - R_TOLERANCE ||
     cyl_point.getR() > oct_ptr->getOuterRadius() + R_TOLERANCE) 
  { return false; }

  double x1 = oct_ptr->getOuterRadius()*cos(oct_ptr->getBeginPhi());
  double y1 = oct_ptr->getOuterRadius()*sin(oct_ptr->getBeginPhi());
  double x2 = oct_ptr->getOuterRadius()*cos(oct_ptr->getEndPhi());
  double y2 = oct_ptr->getOuterRadius()*sin(oct_ptr->getEndPhi());
  double xp = point.getX();
  double yp = point.getY();

  double r1 = sqrt(MUTOO::SQUARE(x1) + MUTOO::SQUARE(y1));
  double r2 = sqrt(MUTOO::SQUARE(x2) + MUTOO::SQUARE(y2));
  double rp = sqrt(MUTOO::SQUARE(xp) + MUTOO::SQUARE(yp));

  double dphi_window = acos((x1*x2 + y1*y2)/(r1*r2));
  double dphi_left = acos((x1*xp + y1*yp)/(r1*rp));
  double dphi_right = acos((x2*xp + y2*yp)/(r2*rp));

  // Intermediate value -- has to be withing dphi_window of
  // both edges.
  double PhiTol = atan(PHI_TOLERANCE/sqrt(xp*xp+yp*yp));
  return (dphi_left < dphi_window + PhiTol && dphi_right < dphi_window + PhiTol);
    
}

//__________________________________________________
bool TMutGeo::in_phi_fiducial(unsigned short arm,
  unsigned short octant,
  const PHPoint& point) 
{

  // figure out which arm
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();
  
  // get a pointer to the octant object
  MutOctant* oct_ptr = geometry->f_pMutStations[0]->f_pMutOctants[octant];

  double x1 = oct_ptr->getOuterRadius()*cos(oct_ptr->getBeginPhi());
  double y1 = oct_ptr->getOuterRadius()*sin(oct_ptr->getBeginPhi());
  double x2 = oct_ptr->getOuterRadius()*cos(oct_ptr->getEndPhi());
  double y2 = oct_ptr->getOuterRadius()*sin(oct_ptr->getEndPhi());
  
  double phi_begin = atan2(y1,x1);
  double phi_end = atan2(y2,x2);
  double phi_in = atan2(point.getY(),point.getX());
  
  double x1p = cos(phi_begin);
  double x2p = cos(phi_end);
  double y1p = sin(phi_begin);
  double y2p = sin(phi_end);
  
  double xmin = min(x1p,x2p);
  double xmax = max(x1p,x2p);
  double ymin = min(y1p,y2p);
  double ymax = max(y1p,y2p);
  
  double x = cos(phi_in);
  double y = sin(phi_in);
  
  return (x > xmin && x < xmax) && (y > ymin && y < ymax);
}

//__________________________________________________
double TMutGeo::get_angle_cathode_anode(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap,
  unsigned short cathode)
{
  MutStrip* strip = TMutGeo::get_strip_geom(arm, 
    station,
    octant,
    half_octant,
    gap,
    cathode,
    0);
  
  PHLine strip_line(strip->getGlobalPositionBegin(), strip->getGlobalPositionEnd());
  
  // first wire
  MutWire* wire = TMutGeo::get_wire_geom(arm,
  station,
  octant,
  half_octant,
  gap,
  0);
  
  PHLine wire_line(wire->getGlobalPositionBegin(), wire->getGlobalPositionEnd());
  
  
  // angle between strip and wire
  PHAngle angle = PHGeometry::angle(wire_line.getDirection(),strip_line.getDirection());
  
  // return angle in radians (PHGeometry call returns the complement of the angle we
  // want, hence we correct by PI)
  return M_PI - angle.getPhi();
}

//__________________________________________________
double TMutGeo::get_angle_cathode_anode(const MUTOO::cathode_locator& location)
{
  return get_angle_cathode_anode(location.get<0>(),
    location.get<1>(),
    location.get<2>(),
    location.get<3>(),
    location.get<4>(),
    location.get<5>());
}


//__________________________________________________
double TMutGeo::get_point_cathode_rdist(const PHPoint& in_point,
  const MUTOO::cathode_locator& location,
  unsigned short strip)
{
  // get strip object 
  MutStrip* strip_ptr = TMutGeo::get_strip_geom(location,strip);
  PHLine strip_line(strip_ptr->getGlobalPositionBegin(),strip_ptr->getGlobalPositionEnd());
  
  // find point on strip closest to in_point
  PHPoint projection = PHGeometry::closestApproachLinePoint(strip_line,in_point);

  // return the distance between the end of the strip and the projection
  return PHGeometry::distancePointToPoint(projection,strip_ptr->getGlobalPositionEnd());
}					

//__________________________________________________
PHVector TMutGeo::get_anode_direction(unsigned short arm, 
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant, 
  unsigned short gap, 
  unsigned short wire)
{
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();
  MutWire* wire_ptr = geometry->
    f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[MUTOO::Wire]->
    f_pMutWires[wire];  
 
  double x1 = wire_ptr->getGlobalPositionBegin().getX();
  double y1 = wire_ptr->getGlobalPositionBegin().getY();
  double z1 = wire_ptr->getGlobalPositionBegin().getZ();
  double x2 = wire_ptr->getGlobalPositionEnd().getX();
  double y2 = wire_ptr->getGlobalPositionEnd().getY();
  double z2 = wire_ptr->getGlobalPositionEnd().getZ();

  PHVector direction(x2-x1, y2-y1, z2-z1);
  direction.normalize();

  // Return unit vector in direction of anode 
  return direction;
}

//__________________________________________________
PHVector TMutGeo::get_w_axis(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant)
{
  // get the strip object
  MutStrip* strip_ptr = TMutGeo::get_strip_geom(arm,station,octant,half_octant,0,0,0);
  PHLine strip_line(strip_ptr->getGlobalPositionBegin(),strip_ptr->getGlobalPositionEnd());

  // return the cross product of the strip direction and the z unit vector
  PHVector waxis = PHGeometry::cross(strip_line.getDirection(),PHVector(0,0,1));
  waxis.normalize();
  return waxis;
}

//__________________________________________________
PHVector TMutGeo::get_w_axis(const MUTOO::cathode_locator&  location, unsigned short strip)
{

  // get the strip object
  MutStrip* strip_ptr = TMutGeo::get_strip_geom(location,strip);
  PHLine strip_line(strip_ptr->getGlobalPositionBegin(),strip_ptr->getGlobalPositionEnd());

  // normalize the strip direction
  strip_line.normalize();

  // return the cross product of the strip direction and the z unit vector
  return PHGeometry::cross(strip_line.getDirection(),PHVector(0,0,1));
}

//__________________________________________________
MutWire* TMutGeo::find_nearest_anode(unsigned short arm, const PHPoint& point)
{
  //Local storage for geo call (c-style to placate geo interface)
  MutStrip* strip_array[2] = {0};
  double strip_ip_array[2] = {0};  
  MutWire* wire_array[1] = {0};
  double wire_ip=0;
  
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();    
  
  geometry->convertPisaHitToChamber(point,
    wire_array,
    wire_ip,
    strip_array,
    strip_ip_array);  
  
  // return the MutWire* or null if no wire is found
  return wire_array[0];
}

//__________________________________________________
pair<unsigned short, double> TMutGeo::find_anode_dca(unsigned short arm, const PHPoint& point)
{
  //Local storage for geo call (c-style to placate geo interface)
  MutStrip* strip_array[2] = {0};
  double strip_ip_array[2] = {0};  
  MutWire* wire_array[1] = {0};
  double wire_ip=0;
  
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();    
  
  geometry->convertPisaHitToChamber(point,
    wire_array,
    wire_ip,
    strip_array,
    strip_ip_array);  
  
  // If no wire is found throw
  if(!wire_array[0]) throw invalid_argument(DESCRIPTION("found no anode wire for input point"));
  
  // Return the wire number and distance of closest approach 
  // in a pair.
  return make_pair(wire_array[0]->getWire(),wire_ip);
}

//__________________________________________________
PHPoint TMutGeo::get_anode_plane_position(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap)
{
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();      
  return geometry->f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[MUTOO::Wire]->getGlobalPosition();
  
}

//__________________________________________________
PHPoint TMutGeo::get_cathode_plane_position(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap,
  unsigned short cathode)
{
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();    
  unsigned short plane = (cathode == 0) ? MUTOO::Cathode1 : MUTOO::Cathode2;

  return geometry->f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[plane]->getGlobalPosition();
}

//__________________________________________________
unsigned short TMutGeo::get_n_strip(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap,
  unsigned short cathod)
{
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();      
  return geometry->f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[cathod]->getNumElements();
}

//__________________________________________________
bool TMutGeo::are_parallel(const MUTOO::cathode_locator& first, const MUTOO::cathode_locator& second)
{
  double angle1 = TMutGeo::get_angle_cathode_anode(first);
  double angle2 = TMutGeo::get_angle_cathode_anode(second);
  return fabs(angle1 - angle2)*MUTOO::RAD_TO_DEG <= 1;
}

//__________________________________________________
void TMutGeo::test_invariant(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap,
  unsigned short cathode,
  unsigned short strip)  
{
  #ifndef NDEBUG
  if(arm>=MUTOO::NumberOfArms) 
  {
    ostringstream what;
    what << "Bad Arm specifier:: " << arm  <<" larger than upbound 1.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  
  if(station>=MUTOO::NumberOfStations) 
  {
    ostringstream what;
    what << "Bad Station specifier:: " << station <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  
  if(octant>=MUTOO::NumberOfOctants) 
  {
    ostringstream what;
    what << "Bad Octant specifier:: " << octant <<" larger than upbound 7.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  
  if(half_octant>=MUTOO::NumberOfHalfOctants) 
  {
    ostringstream what;
    what << "Bad Half Octant specifier:: " << half_octant <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }

  MutArm* Arm_geom = (arm==MUTOO::South) ? SouthArm() : NorthArm();
  MutStation* Station_geom = Arm_geom->f_pMutStations[station];
  if(gap>=(Station_geom->getNumberOfGaps())) 
  {
    ostringstream what;
    what <<"Bad Gap specifier:: " << gap << "large than upbound " << Station_geom->getNumberOfGaps();
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  
  if(cathode>=MUTOO::NumberOfCathodePlanes) 
  {
    ostringstream what;
    what <<"Bad cathode specifier:: " << cathode << "large than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  
  if(strip>=TMutGeo::get_n_strip(arm,station,octant,half_octant,gap,cathode)) 
  {
    ostringstream what;
    what << "Bad Strip specifier:: "<< strip << " larger than upbound " << TMutGeo::get_n_strip(arm,station,octant,half_octant,gap,cathode);
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  #endif
}  

//__________________________________________________
void TMutGeo::test_invariant(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap) 
{
  #ifndef NDEBUG
  if(arm>=MUTOO::NumberOfArms) 
  {
    ostringstream what;
    what << "Bad Arm specifier:: " << arm  <<" larger than upbound 1.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  
  if(station>=MUTOO::NumberOfStations) 
  {
    ostringstream what;
    what << "Bad Station specifier:: " << station <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  
  if(octant>=MUTOO::NumberOfOctants) 
  {
    ostringstream what;
    what << "Bad Octant specifier:: " << octant <<" larger than upbound 7.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }

  if(half_octant>=MUTOO::NumberOfHalfOctants) 
  {
    ostringstream what;
    what << "Bad Half Octant specifier:: " << half_octant <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }

  MutArm* Arm_geom = (arm==MUTOO::South) ? SouthArm() : NorthArm();
  MutStation* Station_geom = Arm_geom->f_pMutStations[station];
  if(gap>=(Station_geom->getNumberOfGaps())) 
  {
    ostringstream what;
    what <<"Bad Gap specifier:: " << gap << "large than upbound " << Station_geom->getNumberOfGaps();
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }

  #endif
}  

//__________________________________________________
void TMutGeo::test_invariant(unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  unsigned short gap,
  unsigned short cathode) 
{
#ifndef NDEBUG

  if(arm>=MUTOO::NumberOfArms) {
    ostringstream what;
    what << "Bad Arm specifier:: " << arm  <<" larger than upbound 1.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  if(station>=MUTOO::NumberOfStations) {
    ostringstream what;
    what << "Bad Station specifier:: " << station <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  if(octant>=MUTOO::NumberOfOctants) {
    ostringstream what;
    what << "Bad Octant specifier:: " << octant <<" larger than upbound 7.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  if(half_octant>=MUTOO::NumberOfHalfOctants) {
    ostringstream what;
    what << "Bad Half Octant specifier:: " << half_octant <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }

  MutArm* Arm_geom = (arm==MUTOO::South) ? SouthArm() : NorthArm();
  MutStation* Station_geom = Arm_geom->f_pMutStations[station];
  if(gap>=(Station_geom->getNumberOfGaps())) {
    ostringstream what;
    what <<"Bad Gap specifier:: " << gap << "large than upbound " << Station_geom->getNumberOfGaps();
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  if(cathode>=MUTOO::NumberOfCathodePlanes) {
    ostringstream what;
    what <<"Bad cathode specifier:: " << cathode << "large than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }

#endif
}  

//__________________________________________________
void TMutGeo::test_invariant(unsigned short arm,
           unsigned short station,
           unsigned short octant,
           unsigned short half_octant) 
{
#ifndef NDEBUG
  if(arm>=MUTOO::NumberOfArms) {
    ostringstream what;
    what << "Bad Arm specifier:: " << arm  <<" larger than upbound 1.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  if(station>=MUTOO::NumberOfStations) {
    ostringstream what;
    what << "Bad Station specifier:: " << station <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  if(octant>=MUTOO::NumberOfOctants) {
    ostringstream what;
    what << "Bad Octant specifier:: " << octant <<" larger than upbound 7.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  if(half_octant>=MUTOO::NumberOfHalfOctants) {
    ostringstream what;
    what << "Bad Half Octant specifier:: " << half_octant <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
              
#endif
}  


//__________________________________________________
void TMutGeo::test_invariant(unsigned short arm,
           unsigned short station)
{
#ifndef NDEBUG
  if(arm>=MUTOO::NumberOfArms) {
    ostringstream what;
    what << "Bad Arm specifier:: " << arm  <<" larger than upbound 1.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
  if(station>=MUTOO::NumberOfStations) {
    ostringstream what;
    what << "Bad Station specifier:: " << station <<" larger than upbound 2.";
    throw invalid_argument(DESCRIPTION(what.str().c_str()));
  }
#endif
}  

 
