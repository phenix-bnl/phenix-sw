#include "PdbEmcSecGeom.hh"
#include <iostream>
#include <iomanip>

//_____________________________________________________________________________
PdbEmcSecGeom::PdbEmcSecGeom() :
  fNx(0),
  fNy(0),
  fTower_xSize(0),
  fTower_ySize(0)
{
  dTranslationVec[0] = dTranslationVec[1] = dTranslationVec[2] = 0.0;
  dRotMatRow0[0] = dRotMatRow0[1] = dRotMatRow0[2] = 0.0;
  dRotMatRow1[0] = dRotMatRow1[1] = dRotMatRow1[2] = 0.0;
  dRotMatRow2[0] = dRotMatRow2[1] = dRotMatRow2[2] = 0.0;
}

//_____________________________________________________________________________
PdbEmcSecGeom::~PdbEmcSecGeom()
{}

//_____________________________________________________________________________
void PdbEmcSecGeom::print() const
{
  std::ostream::fmtflags oldflags = std::cout.flags();

  std::cout.setf(std::ios::scientific);
  std::cout.precision(4);
  std::cout << fNx << " towers in x direction (" << fTower_xSize << " cm)\n"
	    << fNy << " towers in y direction (" << fTower_ySize << " cm)\n"
	    << "Translation vector = ("
	    << dTranslationVec[0] << ","
	    << dTranslationVec[1] << ","
	    << dTranslationVec[2] << ")"
	    << "\n"
	    << "RotMatrix = | "
	    << std::setw(12) << dRotMatRow0[0] << ","
	    << std::setw(12) << dRotMatRow0[1] << ","
	    << std::setw(12) << dRotMatRow0[2] << " |" << "\n"
	    << "            | "
	    << std::setw(12) << dRotMatRow1[0] << ","
	    << std::setw(12) << dRotMatRow1[1] << ","
	    << std::setw(12) << dRotMatRow1[2] << " |" << "\n"
	    << "            | "
	    << std::setw(12) << dRotMatRow2[0] << ","
	    << std::setw(12) << dRotMatRow2[1] << ","
	    << std::setw(12) << dRotMatRow2[2] << " |"
	    << std::endl;
  std::cout.flags(oldflags);
}

//_____________________________________________________________________________
void
PdbEmcSecGeom::TowerXYSize(double& xs, double& ys) const
{
  xs = fTower_xSize;
  ys = fTower_ySize;
}

//_____________________________________________________________________________
void
PdbEmcSecGeom::NxNy(size_t& nx, size_t& ny) const
{
  nx = fNx;
  ny = fNy;
}

//_____________________________________________________________________________
const PHMatrix
PdbEmcSecGeom::RotationMatrix(void) const
{
  PHMatrix mat;

  mat.setA00(dRotMatRow0[0]);
  mat.setA01(dRotMatRow0[1]);
  mat.setA02(dRotMatRow0[2]);

  mat.setA10(dRotMatRow1[0]);
  mat.setA11(dRotMatRow1[1]);
  mat.setA12(dRotMatRow1[2]);

  mat.setA20(dRotMatRow2[0]);
  mat.setA21(dRotMatRow2[1]);
  mat.setA22(dRotMatRow2[2]);

  return mat;
}

//_____________________________________________________________________________
void 
PdbEmcSecGeom::SetNxNy(size_t nx, size_t ny)
{
  fNx = nx;
  fNy = ny;
}

//_____________________________________________________________________________
void
PdbEmcSecGeom::SetTowerXYSize(double xs, double ys)
{
  fTower_xSize = xs;
  fTower_ySize = ys;
}

//_____________________________________________________________________________
void
PdbEmcSecGeom::SetTransformation(const PHMatrix& mat, const PHVector& vec)
{
  dTranslationVec[0] = vec.getX();
  dTranslationVec[1] = vec.getY();
  dTranslationVec[2] = vec.getZ();
  
  dRotMatRow0[0] = mat.getA00();
  dRotMatRow0[1] = mat.getA01();
  dRotMatRow0[2] = mat.getA02();
  
  dRotMatRow1[0] = mat.getA10();
  dRotMatRow1[1] = mat.getA11();
  dRotMatRow1[2] = mat.getA12();
  
  dRotMatRow2[0] = mat.getA20();
  dRotMatRow2[1] = mat.getA21();
  dRotMatRow2[2] = mat.getA22();
} 

//_____________________________________________________________________________
const PHVector 
PdbEmcSecGeom::TranslationVector(void) const
{
  return PHVector(dTranslationVec[0],dTranslationVec[1],dTranslationVec[2]);
}
