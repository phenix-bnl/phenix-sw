#ifndef __PdbEmcSecGeom_h__
#define __PdbEmcSecGeom_h__

#include "PdbCalChan.hh"
#include "PHMatrix.h"
#include "PHVector.h"

class PdbEmcSecGeom : public PdbCalChan
{

 public:
  PdbEmcSecGeom();
  virtual ~PdbEmcSecGeom();

  virtual void print() const;

  void TowerXYSize(double& xs, double& ys) const;

  void NxNy(size_t& nx, size_t& ny) const;

  const PHMatrix RotationMatrix(void) const;

  void SetNxNy(size_t nx, size_t ny);

  void SetTowerXYSize(double xs, double ys);

  void SetTransformation(const PHMatrix& mat, const PHVector& vec);
  
  const PHVector TranslationVector(void) const;

 private:
  size_t fNx;
  size_t fNy;
  double fTower_xSize;
  double fTower_ySize;
  double dTranslationVec[3];
  double dRotMatRow0[3];
  double dRotMatRow1[3];
  double dRotMatRow2[3];

  ClassDef(PdbEmcSecGeom, 1);
};

#endif
