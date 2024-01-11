// ====================================================================
// IMPLEMENTATIONS OF TMuiTwoPackGeo METHODS.
// ====================================================================

#include "TMuiChannelId.hh"
#include "TMuiTwoPackGeo.hh"
#include "TMuiPanelGeo.hh"
#include <PdbMuiTubeGeo.hh>
#include <PHVector.h>
#include <PHGeometry.h>

#include <cmath>
#include <iostream>

using namespace std;

//______________________________________________________
TMuiTwoPackGeo::TMuiTwoPackGeo(
  const short& arm, const short& plane,
  const short& panel, const EOrient_t& orient,
  const short& tube,
  TMuiPanelGeo* const pointer): 
  _id(arm,plane,panel,orient,tube),
  _left_neighbor(0L), _right_neighbor(0L),
  _front_tube(true), _back_tube(false),
  _panel(pointer),
  _center_pos(0.0,0.0,0.0),
  _center_sigma(0.0,0.0,0.0),
  _length(0),
  _width(0),
  _thickness(0),
  _hit_status( false )
{}

//___________________________________________
TMuiTwoPackGeo::~TMuiTwoPackGeo() 
{
  _left_neighbor = 0L;
  _right_neighbor = 0L;
  _panel = 0L;
}

//___________________________________________
bool TMuiTwoPackGeo::IsInTwoPack(const float& x, const float& y, const float &z) const
{ return ( _front_tube.IsInTube(x, y, z) || _back_tube.IsInTube(x, y, z) ); }

//___________________________________________
bool TMuiTwoPackGeo::IsInTwoPack(const PHPoint& p, const PHVector& v) const
{ return ( _front_tube.IsInTube(p,v) || _back_tube.IsInTube(p,v) ); }

//___________________________________________
bool TMuiTwoPackGeo::HitStatus() const
{ return _hit_status; }

//___________________________________________
void TMuiTwoPackGeo::SetTubeEdges(
  const short& layer,
  const float& x1, const float& x2,
  const float& y1, const float& y2,
  const float& length,
  const float& width,
  const float& thick)
{
  
  _length = length;
  _width = width;
  _thickness = thick;
  
  float zCenter;
  float xmin,xmax,ymin,ymax;
  static double a = 1.0/sqrt(12.0);

  if (layer == 0) 
  {
    if (_id.Orient() == kHORIZ) 
    {
      xmin = x1;
      xmax = x2;
      ymin = y1 - 0.5 * width;
      ymax = y1 + 0.5 * width;
      zCenter = _panel->DzCenterToNearHTubes();
    } else {
      xmin = x1 - 0.5 * width;
      xmax = x1 + 0.5 * width;
      ymin = y1;
      ymax = y2;
      zCenter = _panel->DzCenterToNearVTubes();
    }

    _front_tube.SetEdges(xmin,xmax,ymin,ymax,zCenter-0.5*thick,zCenter+0.5*thick);

  } else if (layer == 1) 
  {
    if (_id.Orient() == kHORIZ) {
      xmin = x1;
      xmax = x2;
      ymin = y1 - 0.5 * width;
      ymax = y1 + 0.5 * width;
      zCenter = _panel->DzCenterToFarHTubes();
    } else {
      xmin = x1 - 0.5 * width;
      xmax = x1 + 0.5 * width;
      ymin = y1;
      ymax = y2;
      zCenter = _panel->DzCenterToFarVTubes();
    }

    _back_tube.SetEdges(xmin,xmax,ymin,ymax,zCenter-0.5*thick,zCenter+0.5*thick);
  } else {
    cout << "TMuiTwoPackGeo::SetTubeEdges  invalid layer " << layer
	 << endl; 
  }

  float xf,yf,zf,xb,yb,zb;
  _front_tube.CenterPos(xf,yf,zf);
  _back_tube.CenterPos(xb,yb,zb);
  _center_pos.setX(0.5*(xf + xb));
  _center_pos.setY(0.5*(yf + yb));
  _center_pos.setZ(0.5*(zf + zb));
  _center_sigma.setX(a * fabs(xmax-xmin));
  _center_sigma.setY(a * fabs(ymax-ymin));
  _center_sigma.setZ(a * thick);
}

//___________________________________________
void TMuiTwoPackGeo::SetTubeEdges(
  const PdbMuiTubeGeo& geo,
  const float& length,
  const float& width,
  const float& thick)
{
  short arm, gap, panel, orient, layer, twopack;
  float x1, x2, y1, y2;

  geo.Channel(arm, gap, panel, orient, layer, twopack);
  geo.EndPositionLo(x1, y1);
  geo.EndPositionHi(x2, y2);

  SetTubeEdges(layer, x1, x2, y1, y2, length, width, thick);
}

//___________________________________________
void TMuiTwoPackGeo::SetHitStatus(const bool hit)
{ _hit_status = hit; }

//___________________________________________
ostream&  operator << (ostream& s, const TMuiTwoPackGeo& g)
{
  float x, y, z;
  g.CenterPos(x, y, z);
  s << g.Channel();
  s << "  center pos = (" << x << " , " << y << " , " << z << ")\n";
  s << "  front tube: "
    << " x " << g.FrontTube().XMin()
    << " - " << g.FrontTube().XMax()
    << " y " << g.FrontTube().YMin()
    << " - " << g.FrontTube().YMax()
    << " z " << g.FrontTube().ZMin()
    << " - " << g.FrontTube().ZMax() << "\n";
  s << "  back  tube: "
    << " x " << g.BackTube().XMin()
    << " - " << g.BackTube().XMax()
    << " y " << g.BackTube().YMin()
    << " - " << g.BackTube().YMax()
    << " z " << g.BackTube().ZMin()
    << " - " << g.BackTube().ZMax() << "\n";
  return s;
}


//___________________________________________
TMuiTubeGeo::TMuiTubeGeo(
  const bool& front,
  const float& xmin, const float& xmax,
  const float& ymin, const float& ymax,
  const float& zmin, const float& zmax): 
  _front(front),
  _x_min(xmin), _x_max(xmax),
  _y_min(ymin), _y_max(ymax),
  _z_min(zmin), _z_max(zmax)
  {}

//___________________________________________
TMuiTubeGeo::TMuiTubeGeo(const bool& front): 
  _front(front),
  _x_min(0.0), _x_max(0.0),
  _y_min(0.0), _y_max(0.0),
  _z_min(0.0), _z_max(0.0)
  {}

// TMuiTubeGeo::CenterPos
// Return position of center (in the panel coordinate system)
void
TMuiTubeGeo::CenterPos(float& x, float& y, float& z) const
{
  x = 0.5*(_x_min + _x_max);
  y = 0.5*(_y_min + _y_max);
  z = 0.5*(_z_min + _z_max);
  return;
}


// TMuiTubeGeo::IsInTube (All Panel Coordinates!)
// Is the point within the boundaries of this tube?
bool
TMuiTubeGeo::IsInTube(const float& x, const float& y, const float& z) const
{
  return ( (x > _x_min) && (x < _x_max) && (y > _y_min) && (y < _y_max) );
}

// TMuiTubeGeo::IsInTube
// Is this line within the boundaries of this tube?
// We will find intersections with the 6 sides of tube
// and then check that those are with in the edges of
// the tube.
bool
TMuiTubeGeo::IsInTube(const PHPoint& p, const PHVector& v) const
{
  float dz = 0;
  return IsInTube(p, v, dz);
}

bool
TMuiTubeGeo::IsInTube(const PHPoint& p, const PHVector& v, float& dz) const
{
  dz = 0;
  
   PHLine  line(p, v);
   PHPoint q;

  //We need to define the 6 panels then loop over to check for intersection
  // All are defined in the panel coordinate system
  PHPanel pSides[6];

  //XMin
  PHPoint fP0Xmin(_x_min,_y_min,_z_min);
  PHPoint fP1Xmin(_x_min,_y_max,_z_min);
  PHPoint fP2Xmin(_x_min,_y_min,_z_max);
  PHPanel pPanel0(fP0Xmin,fP1Xmin,fP2Xmin);
  pSides[0] = pPanel0;
  //XMax
  PHPoint fP0Xmax(_x_max,_y_min,_z_min);
  PHPoint fP1Xmax(_x_max,_y_max,_z_min);
  PHPoint fP2Xmax(_x_max,_y_min,_z_max);
  PHPanel pPanel1(fP0Xmax,fP1Xmax,fP2Xmax);
  pSides[1] = pPanel1;
  //YMin
  PHPoint fP0Ymin(_x_min,_y_min,_z_min);
  PHPoint fP1Ymin(_x_max,_y_min,_z_min);
  PHPoint fP2Ymin(_x_min,_y_min,_z_max);
  PHPanel pPanel2(fP0Ymin,fP1Ymin,fP2Ymin);
  pSides[2] = pPanel2;
  //Ymax
  PHPoint fP0Ymax(_x_min,_y_max,_z_min);
  PHPoint fP1Ymax(_x_max,_y_max,_z_min);
  PHPoint fP2Ymax(_x_min,_y_max,_z_max);
  PHPanel pPanel3(fP0Ymax,fP1Ymax,fP2Ymax);
  pSides[3] = pPanel3;
  //ZMin
  PHPoint fP0Zmin(_x_min,_y_min,_z_min);
  PHPoint fP1Zmin(_x_max,_y_min,_z_min);
  PHPoint fP2Zmin(_x_min,_y_max,_z_min);
  PHPanel pPanel4(fP0Zmin,fP1Zmin,fP2Zmin);
  pSides[4] = pPanel4;
  //Zmax
  PHPoint fP0Zmax(_x_min,_y_min,_z_max);
  PHPoint fP1Zmax(_x_max,_y_min,_z_max);
  PHPoint fP2Zmax(_x_min,_y_max,_z_max);
  PHPanel pPanel5(fP0Zmax,fP1Zmax,fP2Zmax);
  pSides[5] = pPanel5;
  
  //If particle intersects this tube there should only be two crossings.
  int numIntersections = 0;
  float intersection_z[6];
  for(int sideid = 0; sideid<6; sideid++)
  {
    PHBoolean ok = PHGeometry::intersectionLinePanel(line,
								pSides[sideid],
								q);
    if(ok)
    {
      intersection_z[numIntersections] = q.getZ();
      // to speed things up we should check on most likely then return true.
      numIntersections++;
    }
  }
  if(numIntersections==1)
  {
    cout<<"TMuiTwoPackGeo::IsInTube(): Warning only one intersection.  Should be two!\n";
  }
  
  if(numIntersections>2)
  {
    cout<<"TMuiTwoPackGeo::IsInTube(): Warning too many intersections "<< numIntersections <<"\n";
  }
  
  if(numIntersections>0)
    {
      dz = fabs( intersection_z[1] - intersection_z[0] );
      return true;
    }
  
  return false;
}



// TMuiTubeGeo::SetEdges
// Set the edge positions  (in the panel coordinate system)
void
TMuiTubeGeo::SetEdges(const float& xmin, const float& xmax,
		      const float& ymin, const float& ymax,
		      const float& zmin, const float& zmax)
{
  _x_min = xmin;
  _x_max = xmax;
  _y_min = ymin;
  _y_max = ymax;
  _z_min = zmin;
  _z_max = zmax;
}
