#ifndef __SECGEOM_H__
#define __SECGEOM_H__

#include "PHMatrix.h"
#include "PHVector.h"

#include <iosfwd>

/** Geometrical information for one EMCAL Sector.

Used as a "storage"-unit by emcGeometry.

*/

class SecGeom
{

public:

  friend std::ostream& operator<<(std::ostream&, const SecGeom&);

  SecGeom();
  virtual ~SecGeom(){}
  SecGeom(const SecGeom& obj);
  SecGeom& operator = (const SecGeom& obj);
  void Copy(SecGeom& obj);

  int GetTowerPosGlobal(size_t ind, float &x, float &y, float &z) const;

  int GetTowerPosLocal(size_t ind, float &x, float &y, float &z) const;

  void GlobalToLocal(float xg, float yg, float zg,
		     float &xl, float &yl, float &zl) const;

  const PHMatrix& InvRotationMatrix(void) const
  {
    return fInvRotationMat;
  }
  const PHVector& InvTranslationVector(void) const
  {
    return fInvTranslationVec;
  }

  void LocalToGlobal(float xl, float yl, float zl,
		     float &xg, float &yg, float &zg) const;

  size_t nx(void) const
  {
    return fNx;
  }
  size_t ny(void) const
  {
    return fNy;
  }

  void Print(void) const;

  const PHMatrix& RotationMatrix(void) const
  {
    return fRotationMat;
  }

  void SetNxNy(size_t nx, size_t ny)
  {
    fNx = nx;
    fNy = ny;
  }

  void SetTowerXYSize(double txsize, double tysize)
  {
    fTower_xSize = txsize;
    fTower_ySize = tysize;
  }

  void SetDirectTransformation(PHMatrix mat, PHVector vec);

  double Tower_xSize(void) const
  {
    return fTower_xSize;
  }
  double Tower_ySize(void) const
  {
    return fTower_ySize;
  }

  const PHVector& TranslationVector(void) const
  {
    return fTranslationVec;
  }

private:
  size_t fNx;          // Number of cells in X dir
  size_t fNy;          // Number of cells in Y dir
  double fTower_xSize; // Tower size in X dir
  double fTower_ySize; // Tower size in Y dir
  PHMatrix fRotationMat;
  PHVector fTranslationVec;
  PHMatrix fInvRotationMat;
  PHVector fInvTranslationVec;
  static double fgABSURD;
};

#endif
