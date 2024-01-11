// ====================================================================
// IMPLEMENTATIONS OF TMuiPanelGeo METHODS.
// ====================================================================

#include "TMuiChannelId.hh"
#include "TMuiTwoPackGeo.hh"
#include "TMuiPanelGeo.hh"
#include <PdbMuiPanelGeo.hh>

#include <PHMatrix.h>
#include <PHPanel.h>
#include <PHGeometry.h>

#include <vector>
#include <iostream>

using namespace std;

//_______________________________________________
// Real constructor (construct two-packs externally)
TMuiPanelGeo::TMuiPanelGeo(
  const short& arm,
  const short& gap,
  const short& panel,
  const short& num_twopacks_horiz,
  const short& num_twopacks_vert,
  const float& xSize,
  const float& ySize,
  const float& zSize,
  const float& xTarget1Global,
  const float& yTarget1Global,
  const float& zTarget1Global,
  const float& xTarget2Global,
  const float& yTarget2Global,
  const float& zTarget2Global,
  const float& dxTarget1ToFiducial,
  const float& dyTarget1ToFiducial,
  const float& dxFiducialToCenter,
  const float& dyFiducialToCenter,
  const float& dzCenterToNearHTubes,
  const float& dzCenterToFarHTubes,
  const float& dzCenterToNearVTubes,
  const float& dzCenterToFarVTubes ): 
  _id(arm,gap,panel),
  _twopacks_h(num_twopacks_horiz, 0),
  _twopacks_v(num_twopacks_vert, 0),
  f_dzCenterToNearHTubes(dzCenterToNearHTubes),
  f_dzCenterToFarHTubes (dzCenterToFarHTubes),
  f_dzCenterToNearVTubes(dzCenterToNearVTubes),
  f_dzCenterToFarVTubes (dzCenterToFarVTubes),
  _x_size(xSize), _y_size(ySize), _z_size(zSize),
  fTwoPacksH(0),
  fTwoPacksV(0)
{
    
  // U and V are the position vectors of the two survey targets in the
  // PHENIX global co-ordinate system.
  PHVector u(xTarget1Global,yTarget1Global,zTarget1Global);
  PHVector v(xTarget2Global,yTarget2Global,zTarget2Global);
  PHVector w  = v  - u;
  
  // Up and Vp are the position vectors of the two survey targets in the
  // panel co-ordinate system ((0,0,0) is the panel center).
  // Assume that only the X positions of the two targets differ.
  PHVector up(0.0,0.0,0.0);
  PHVector vp(0.0,0.0,0.0);

  up.setX(-dxFiducialToCenter-dxTarget1ToFiducial);
  up.setY(-dyFiducialToCenter-dyTarget1ToFiducial);
  if (u.getX() > v.getX()) {
    vp.setX(up.getX()-w.length());
  } else {
    vp.setX(up.getX()+w.length());
  }
  vp.setY(up.getY());

  PHVector   wp = vp - up;

  // Make some PHENIX geometry objects.
  PHVector newX, newY, newZ;

  newZ = wp;
  newZ.normalize();
  PHVector xx1(-newZ.getY(), newZ.getX(), 0.0);
  newX = xx1;
  newX.normalize();
  newY = newZ.cross(newX);
  PHMatrix N(newX, newY, newZ);

  newZ = w;
  newZ.normalize();
  PHVector xx2(-newZ.getY(), newZ.getX(), 0.0);
  newX = xx2;
  newX.normalize();
  newY = newZ.cross(newX);
  PHMatrix MN(newX, newY, newZ);

  PHMatrix M(MN * N.transpose());
  PHVector Q(v - M * vp);

  PHPoint qx(_x_size, 0.0, 0.0); // midpoint position of panel side
  PHPoint qy(0.0, _y_size, 0.0); // midpoint position of panel top

  // transform to PHENIX coordinate system
  PHPoint Qx = PHGeometry::transformPoint(M, Q, qx);
  PHPoint Qy = PHGeometry::transformPoint(M, Q, qy);

  PHPanel gpanel(Q, Qx, Qy);

  _rotation    = M;
  _translation = Q;
  fGeomPanel   = gpanel;
  _center   = _translation;
}

//_______________________________________________
// Real constructor (construct two-packs externally)
TMuiPanelGeo::TMuiPanelGeo(
  const PdbMuiPanelGeo& geo,
  const short& num_twopacks_horiz,
  const short& num_twopacks_vert,
  const float& xSize,
  const float& ySize,
  const float& zSize): 
  _id(-1, -1, -1),
  _twopacks_h(num_twopacks_horiz, 0),
  _twopacks_v(num_twopacks_vert, 0),
  f_dzCenterToNearHTubes(0.0),
  f_dzCenterToFarHTubes (0.0),
  f_dzCenterToNearVTubes(0.0),
  f_dzCenterToFarVTubes (0.0),
  _x_size(xSize), _y_size(ySize), _z_size(zSize),
  fTwoPacksH(0),
  fTwoPacksV(0)
{
    
  // FIXME:  not done here yet?
  short arm, gap, panel;
  geo.Channel(arm, gap, panel);
  _id.Set(arm, gap, panel);

  // Get the tube displacements.
  geo.TubeDisplacement(kHORIZ, f_dzCenterToNearHTubes, f_dzCenterToFarHTubes);
  geo.TubeDisplacement(kVERT,  f_dzCenterToNearVTubes, f_dzCenterToFarVTubes);
  
  // U and V are the position vectors of the two survey targets in the
  // PHENIX global co-ordinate system.
  float xg, yg, zg;
  geo.TargetPosition(1, xg, yg, zg);
  PHVector u(xg, yg, zg);
  geo.TargetPosition(2, xg, yg, zg);
  PHVector v(xg, yg, zg);
  PHVector w  = v  - u;
  
  // Up and Vp are the position vectors of the two survey targets in the
  // panel co-ordinate system ((0,0,0) is the panel center).
  // Assume that only the X positions of the two targets differ.
  float xp, yp, zp;
  geo.LocalTargetPos(1, xp, yp, zp);
  PHVector up(xp, yp, zp);
  geo.LocalTargetPos(2, xp, yp, zp);
  PHVector vp(xp, yp, zp);
  
  PHVector   wp = vp - up;
  
  // Make some PHENIX geometry objects.
  PHVector newX, newY, newZ;
  
  newZ = wp;
  newZ.normalize();
  PHVector xx1(-newZ.getY(), newZ.getX(), 0.0);
  newX = xx1;
  newX.normalize();
  newY = newZ.cross(newX);
  PHMatrix N(newX, newY, newZ);
  
  newZ = w;
  newZ.normalize();
  PHVector xx2(-newZ.getY(), newZ.getX(), 0.0);
  newX = xx2;
  newX.normalize();
  newY = newZ.cross(newX);
  PHMatrix MN(newX, newY, newZ);
  
  PHMatrix M(MN * N.transpose());
  PHVector Q(v - M * vp);
  
  PHPoint qx(_x_size, 0.0, 0.0); // midpoint position of panel side
  PHPoint qy(0.0, _y_size, 0.0); // midpoint position of panel top
  
  // transform to PHENIX coordinate system
  PHPoint Qx = PHGeometry::transformPoint(M, Q, qx);
  PHPoint Qy = PHGeometry::transformPoint(M, Q, qy);
  
  PHPanel gpanel(Q, Qx, Qy);
  
  _rotation    = M;
  _translation = Q;
  fGeomPanel   = gpanel;
  _center   = _translation;
}

//_______________________________________________
// Destructor.
TMuiPanelGeo::~TMuiPanelGeo()
{

  fTwoPacksH = 0;
  fTwoPacksV = 0;

  while (_twopacks_v.size()) {
    delete _twopacks_v.back();
    _twopacks_v.pop_back();
  }

  while (_twopacks_h.size()) {
    delete _twopacks_h.back();
    _twopacks_h.pop_back();
  }
}


//_______________________________________________
// TMuiPanelGeo::DzNearHorizTubes
// Z displacement of "near" horizontal tubes from the panel center.
float TMuiPanelGeo::DzCenterToNearHTubes() const
{ return f_dzCenterToNearHTubes; }

//_______________________________________________
// TMuiPanelGeo::DzNearVertTubes
// Z displacement of "near" vertical tubes from the panel center.
float TMuiPanelGeo::DzCenterToNearVTubes() const
{ return f_dzCenterToNearVTubes; }

//_______________________________________________
// TMuiPanelGeo::DzFarHorizTubes
// Z displacement of "far" horizontal tubes from the panel center.
float TMuiPanelGeo::DzCenterToFarHTubes() const
{ return f_dzCenterToFarHTubes; }

//_______________________________________________
// TMuiPanelGeo::DzFarVertTubes
// Z displacement of "far" vertical tubes from the panel center.
float TMuiPanelGeo::DzCenterToFarVTubes() const
{ return f_dzCenterToFarVTubes; }

//_______________________________________________
void TMuiPanelGeo::Rotate( const PHMatrix& rotation )
{
  /* 
     both the old rotation and translations are updated
     because we assume the rotation is performed around the beam axis
  */
  _rotation = rotation*_rotation;
  _translation = rotation*_translation;
	
  PHPoint qx(_x_size, 0, 0); // midpoint position of panel side
  PHPoint qy(0, _y_size, 0); // midpoint position of panel top
	
  // we assume rotations are done wrt the beam axis
  PHPoint Qx = PHGeometry::transformPoint(_rotation, _translation, qx);
  PHPoint Qy = PHGeometry::transformPoint(_rotation, _translation, qy);
  PHPoint centerXYZ = PHGeometry::transformPoint(_rotation, _translation, PHPoint(0,0,0) );

  // update panel
  fGeomPanel = PHPanel( centerXYZ, Qx, Qy );
  _center   = centerXYZ;
  return;	
}

//_______________________________________________
void TMuiPanelGeo::Translate( const PHVector& translation )
{
  _translation = _translation + translation;

  PHPoint qx(_x_size, 0.0, 0.0); // midpoint position of panel side
  PHPoint qy(0.0, _y_size, 0.0); // midpoint position of panel top
  PHPoint Qx = PHGeometry::transformPoint(_rotation, _translation, qx);
  PHPoint Qy = PHGeometry::transformPoint(_rotation, _translation, qy);
  fGeomPanel = PHPanel( _translation, Qx, Qy );
  _center   = _translation;
  return;	
}

//_______________________________________________
// TMuiPanelGeo::RotationMatrix
// Get the angles (in degrees) defining the rotation matrix of the
// panel in the global coordinate system.
void TMuiPanelGeo::RotationMatrix(float& thetaX, float& phiX,
				  float& thetaY, float& phiY,
				  float& thetaZ, float& phiZ) const
{
  PHPoint x(_rotation.getColumn0());
  PHPoint y(_rotation.getColumn1());
  PHPoint z(_rotation.getColumn2());

  PHSphPoint X(x);
  PHSphPoint Y(y);
  PHSphPoint Z(z);

  thetaX = double(X.getTheta()) * 180/ M_PI;
  phiX   = double(X.getPhi())   * 180/ M_PI;
  thetaY = double(Y.getTheta()) * 180/ M_PI;
  phiY   = double(Y.getPhi())   * 180/ M_PI;
  thetaZ = double(Z.getTheta()) * 180/ M_PI;
  phiZ   = double(Z.getPhi())   * 180/ M_PI;

  return;
}


//_______________________________________________
// TMuiPanelGeo::GuessTwoPack
// Given a position q, estimate the corresponding two-pack number
// (in the given orientation).
short TMuiPanelGeo::GuessTwoPack(const int& Orient,
				 const float& qx,
				 const float& qy,
				 const float& qz) const
{
  float X,X0,X1;
  float dummy0, dummy1, dummy2;
  short nTwoPackMax;
  if (Orient == kHORIZ)
    {
      X = qy;
      nTwoPackMax = fTwoPacksH - 1;
      TwoPackPointer(kHORIZ,0)->CenterPos(dummy0, X0, dummy2);
      TwoPackPointer(kHORIZ,nTwoPackMax)->CenterPos(dummy0, X1, dummy2);
    }
  else
    {
      X = qx;
      nTwoPackMax = fTwoPacksV - 1;
      TwoPackPointer(kVERT, 0)->CenterPos(X0, dummy1, dummy2);
      TwoPackPointer(kVERT, nTwoPackMax)->CenterPos(X1, dummy1, dummy2);
    }

  float dX = (X1-X0)/nTwoPackMax;
  return short((X-X0)/dX + 0.5);
}

//_______________________________________________
// Find the intersection of the given line with the panel.
PHPoint TMuiPanelGeo::ProjectToPanel(const PHPoint& p, const PHVector& v) const
{
  PHLine  line(p, v);
  PHPoint q;

  // This call should only return "false" if the line is parallel to
  // the plane containing the panel.
  PHGeometry::intersectionLinePlane(line, fGeomPanel, q);

  return q;
}

//_______________________________________________
PHVector TMuiPanelGeo::RotateToPanel(const PHVector& GVect) const
{
  return PHGeometry::transformVector(_rotation.transpose(), GVect);
}

//_______________________________________________
PHVector TMuiPanelGeo::RotateToGlobal(const PHVector& PVect) const
{
  return PHGeometry::transformVector(_rotation, PVect);
}

//_______________________________________________
PHPoint TMuiPanelGeo::TransformToPanel(const PHPoint& GVect) const
{
  PHPoint null(0.0, 0.0, 0.0);
  PHPoint Q(GVect - _translation);

  return PHGeometry::transformPoint(_rotation.transpose(), null, Q);
}

//_______________________________________________
PHPoint TMuiPanelGeo::TransformToGlobal(const PHPoint& PVect) const
{
  return PHGeometry::transformPoint(_rotation, _translation, PVect);
}

//_______________________________________________
// TMuiPanelGeo::SetID
// Set the channel ID of this panel.
void TMuiPanelGeo::SetID(const short& arm, const short& plane,
			 const short& panel)
{
  _id.Set(arm,plane,panel);
}


//_______________________________________________
// TMuiPanelGeo::AddTwoPack
// Add a two-pack to the panel.
TMuiTwoPackGeo*
TMuiPanelGeo::AddTwoPack(const short& orient, const short& twopack)
{
  TMuiTwoPackGeo* t = 0;
  TMuiTwoPackGeo* neighbor = 0;

  if (orient == kHORIZ) {

    if ( twopack >= (int)_twopacks_h.size()) {
      cout << "TMuiPanelGeo::AddTwoPack-E1  H twopack number "
	   << twopack << " outside of expected range 0-"
	   << _twopacks_h.size() << endl;
      return 0;
    }

    if ( (twopack + 1) > fTwoPacksH) fTwoPacksH = twopack + 1;

    if (_twopacks_h[twopack]) {
      // The two-pack object has already been created; don't create a
      // new one.
      return _twopacks_h[twopack];
    }
    
    t = new TMuiTwoPackGeo(_id.Arm(), _id.Plane(),
			   _id.Panel(),
			   kHORIZ, twopack, this);
    _twopacks_h[twopack] = t;
    t->SetLeftNeighbor(0L);
    t->SetRightNeighbor(0L);

    // Make this two-pack and the previous one neighbors.
    if (twopack > 0) {
      neighbor = _twopacks_h[twopack-1];
      if (neighbor) {
	neighbor->SetRightNeighbor(t);
	t->SetLeftNeighbor(neighbor);
      }
    }

  } else {

    if ( twopack >= (int)_twopacks_v.size()) {
      cout << "TMuiPanelGeo::AddTwoPack-E2  V twopack number "
	   << twopack << " outside of expected range 0-"
	   << _twopacks_v.size() << endl;
      return 0;
    }

    if ( (twopack + 1) > fTwoPacksV) fTwoPacksV = twopack + 1;

    if (_twopacks_v[twopack]) {
      // The two-pack object has already been created; don't create a
      // new one.
      return _twopacks_v[twopack];
    }

    t = new TMuiTwoPackGeo(_id.Arm(), _id.Plane(),
			   _id.Panel(),
			   kVERT,  twopack, this);
    _twopacks_v[twopack] = t;
    t->SetLeftNeighbor(0L);
    t->SetRightNeighbor(0L); // next two-pack added will go here

    // Make this two-pack and the previous one neighbors.
    if (twopack > 0) {
      neighbor = _twopacks_v[twopack-1];
      if (neighbor) {
	neighbor->SetRightNeighbor(t);
	t->SetLeftNeighbor(neighbor);
      }
    }
  }

  return t;
}

//_______________________________________________
// Print TMuiPanelGeo information to a stream.
ostream& 
operator << (ostream& s, const TMuiPanelGeo& g)
{
  float x0, y0, z0, dx, dy, dz;
  g.CenterPos(x0, y0, z0);
  g.Size(dx, dy, dz);

  s << g.Channel();
  s << "  " << g.getTwoPackCount(kHORIZ) << " H two-packs"
    << "  " << g.getTwoPackCount(kVERT)  << " V two-packs\n";
  s << "  center pos =  " << x0 << "  " << y0 << "  " << z0 << "\n";
  s << "  size =        " << dx << "  " << dy << "  " << dz << "\n";
  
  return s;
}

