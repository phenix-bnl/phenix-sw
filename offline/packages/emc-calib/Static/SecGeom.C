#include "SecGeom.h"
#include "PHGeometry.h"

#include <iostream>
using namespace std;

double SecGeom::fgABSURD = -999999.;

//_____________________________________________________________________________
SecGeom::SecGeom() :
  fNx(0), fNy(0),
  fTower_xSize(0), fTower_ySize(0)
{
}

//_____________________________________________________________________________
SecGeom::SecGeom(const SecGeom& obj)
{
  (const_cast<SecGeom&>(obj)).Copy(*this);
}

//_____________________________________________________________________________
SecGeom& SecGeom::operator=(const SecGeom& obj)
{
  if ( this != &obj ) {
    (const_cast<SecGeom&>(obj)).Copy(*this);
  }
  return *this;
}

//_____________________________________________________________________________
void SecGeom::Copy(SecGeom& obj)
{
  obj.fNx = fNx;
  obj.fNy = fNy;
  obj.fTower_xSize = fTower_xSize;
  obj.fTower_ySize = fTower_ySize;
  obj.fRotationMat = fRotationMat;
  obj.fTranslationVec = fTranslationVec;
  obj.fInvRotationMat = fInvRotationMat;
  obj.fInvTranslationVec = fInvTranslationVec;
}

//_____________________________________________________________________________
int SecGeom::GetTowerPosLocal(size_t ind, float &x, float &y, float &z) const
{
  //
  // Calculates tower position in Sector frame,
  // ind - tower index (ix+iy*nx, ix=0,...; iy=0,... )
  //
  // Returns 1 in case of success, 0 otherwise.

  size_t ix = ind % fNx;
  size_t iy = ind / fNx; 

  if ( iy >= fNy ) {
    x = y = z = fgABSURD;
    cerr << "<E> SecGeom::GetTowerPosLocal : Wrong tower index : "
	 << ind << endl;
    return 0; // Error
  }

  x = fTower_xSize * ix;
  y = fTower_ySize * iy;
  z = 0;

  return 1; // OK
}

//_____________________________________________________________________________
int SecGeom::GetTowerPosGlobal(size_t ind, float &x, float &y, float &z) const
{
  //
  // Calculates tower position in Sector frame,
  // is - sector index (0-7)
  // ind - tower index (ix+iy*nx, ix=0,...; iy=0,... )
  //
  // Returns 1 in case of success, 0 otherwise.

  float xl, yl, zl;

  int st = GetTowerPosLocal( ind, xl, yl, zl );

  if( !st ) { 
    x = fgABSURD;
    y = fgABSURD;
    z = fgABSURD;
    return 0;
  }

  PHPoint emcHit(xl, yl, zl);
  PHPoint phnxHit  = PHGeometry::transformPoint(fRotationMat, 
					      fTranslationVec, 
					      emcHit);
  x =  phnxHit.getX();
  y =  phnxHit.getY();
  z =  phnxHit.getZ();

  return 1;
}

//_____________________________________________________________________________
void SecGeom::LocalToGlobal( float xl, float yl, float zl, 
			     float &xg, float &yg, float &zg) const
{
  //
  // Converts local coordinates (xl,yl,zl) to global ones (xg,yg,zg)
  //

  PHPoint emcHit(xl, yl, zl);
  PHPoint phnxHit  = PHGeometry::transformPoint(fRotationMat, 
					      fTranslationVec, emcHit);
  xg =  phnxHit.getX();
  yg =  phnxHit.getY();
  zg =  phnxHit.getZ();
}

//_____________________________________________________________________________
void SecGeom::GlobalToLocal( float xg, float yg, float zg, 
			     float &xl, float &yl, float &zl) const
{
  //
  // Converts global coordinates (xg,yg,zg) to local ones (xl,yl,zl)
  //

  PHPoint phnxHit(xg, yg, zg);
  PHPoint emcHit  = PHGeometry::transformPoint(fInvRotationMat, 
					     fInvTranslationVec,
					     phnxHit);
  xl =  emcHit.getX();
  yl =  emcHit.getY();
  zl =  emcHit.getZ();
}

//_____________________________________________________________________________
void
SecGeom::Print() const
{ 
  cout << *this << endl; 
}

//_____________________________________________________________________________
void SecGeom::SetDirectTransformation(PHMatrix mat, PHVector vec)
{
  fRotationMat = mat;
  fTranslationVec = vec;
  PHFrame local;
  PHFrame global = PHGeometry::MatrixAndVector2frames(local,
						     fRotationMat,
						     fTranslationVec);
  PHGeometry::frames2MatrixAndVector(global,local,
				    fInvRotationMat,
				    fInvTranslationVec);
}

//_____________________________________________________________________________
ostream& operator<< (ostream& out, const SecGeom& obj)
{
  streamsize old_precision = out.precision(20);
  out << obj.fNx << " towers in x (xSize=" << obj.fTower_xSize << "cm), " 
      << obj.fNy << " towers in y (ySize=" << obj.fTower_ySize << "cm) "
      << endl 
      << " rot. mat. = " 
      << endl 
      << obj.fRotationMat
      << " trans. vec = " << obj.fTranslationVec
      << endl;
  out.precision(old_precision);
  return out;
}
