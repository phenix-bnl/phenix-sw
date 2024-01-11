#ifndef EMCSECTORREC_H
#define EMCSECTORREC_H

// Name: EmcSectorRec.h
// Author: A. Bazilevsky (RIKEN-BNL)
// Major modifications by M. Volkov (RRC KI) Jan 27 2000

// EmcSectorRec -- abstract base class for both PbSc and PbGl

#include "PHMatrix.h"
#include "PHVector.h"
#include <vector>


class EmcCluster;
class EmcModule;
class emcTowerContent;

typedef struct SecGeom{

  short nx;          // Number of cells in X dir
  short ny;          // Number of cells in Y dir
  float Tower_xSize; // Tower size in X dir
  float Tower_ySize; // Tower size in Y dir

} SecGeom;

// ///////////////////////////////////////////////////////////////////////////

/** ABC of a clusterizer for one EMCAL sector. 

@ingroup clustering

 */

class EmcSectorRec
{
  
 public:

  EmcSectorRec();
  virtual ~EmcSectorRec();

  void SetVertex(float *vv){  fVx = vv[0]; fVy = vv[1]; fVz = vv[2]; }
  void SetGeometry(SecGeom const &geom, PHMatrix * rm, PHVector * tr );

  int GetNx() const { return fNx; }
  int GetNy() const { return fNy; }
  float GetModSizex() const { return fModSizex; }
  float GetModSizey() const { return fModSizey; }
  float GetVx() const { return fVx; }
  float GetVy() const { return fVy; }
  float GetVz() const { return fVz; }
  void SetPeakThreshold(float Thresh) { fgMinPeakEnergy = Thresh; }
  float GetPeakThreshold() { return fgMinPeakEnergy; }
  float GetTowerThreshold() { return fgTowerThresh; }
  
#ifndef __CINT__
  void SetModules(std::vector<EmcModule> const *modules);
  std::vector<EmcModule> *GetModules(){ return fModules; }
  std::vector<EmcCluster> *GetClusters(){ return fClusters; }
#endif

  int FindClusters();
  void GetImpactAngle(float x, float y, float *sinT );
  void GlobalToSector(float, float, float, float*, float*, float*);
  void SectorToGlobal(float xsec, float ysec, float zsec, float* px,
		      float* py, float* pz );
  void SectorToGlobalErr( float dxsec, float dysec, float dzsec, float* pdx,
			  float* pdy, float* pdz );
  /// Converts coordinates in units of towers into cm's (Local coord. system)
  virtual void TowersToSector(float, float, float &, float &) = 0;
  /// Returns  coordinates of the tower centers in cm's (Local coord. system)
  virtual void TowersToSector(int,   int,   float &, float &) = 0;
  /// Converts Local Sector coordinates in cm into integer tower numbers
  virtual void SectorToTowers(float, float, int &,   int &)   = 0;

  virtual void Gamma(int, EmcModule*, float*, float*, float*, float*, float*,
		     float*, float*, float*,
		     int &ndf); // ndf added MV 28.01.00

  virtual void Gamma(int, emcTowerContent**, float*, float*, float*, float*, float*,
		     float*, float*, float*,
		     int &ndf); // ndf added by mlp 

  void Mom1(int, EmcModule*, float*, float*, float*);

  void Mom1(int, emcTowerContent**, float*, float*, float*);

  void Momenta(int, EmcModule*, float*, float*, float*, float*, float*,
	       float* );

  void Momenta(int, emcTowerContent**, float*, float*, float*, float*, float*,
	       float* );

  // Pure virtual functions
  virtual void SetTowerThreshold(float Thresh)=0;
  virtual void SetProfileParameters(int, float, float, float)=0;
  void SetChi2Limit(int lim);

  virtual float ClusterChisq(int, EmcModule*, float, float, float,
			     int &ndf)=0; // ndf added MV 28.01.00

  virtual float ClusterChisq(int, emcTowerContent**, float, float, float,
			     int &ndf)=0; // ndf added MV 28.01.00

  virtual float Chi2Correct(float chi2,int ndf)=0;
  virtual  void CorrectPosition(float energy, float x, float y, float *xcorr,
				float *ycorr, bool callSetPar=true)=0;
  virtual void CorrectEnergy(float energy, float x, float y, float *ecorr)=0;
  virtual void CorrectECore(float ecore, float x, float y, float *ecorecorr)=0;
  virtual void TwoGamma(int, EmcModule*, float*, float*, float*, float*,
			float*, float*, float*)=0;
  virtual void TwoGamma(int, emcTowerContent**, float*, float*, float*, float*,
			float*, float*, float*)=0;
  virtual float Chi2Limit(int ndf)=0;
  virtual float PredictEnergy(float, float, float)=0;
  virtual void CalculateErrors(float e, float x, float y, float* pde,
			       float* pdx, float* pdy, float* pdz)=0;
  virtual void getTowerPos(int ix, int iy, float &x, float & y) = 0;

  // Auxiliary static functions
  static int HitNCompare(const void*, const void*);
  static int HitACompare(const void*, const void*);

  static void CopyVector(int*, int*, int);
  static void CopyVector(EmcModule*, EmcModule*, int);
  static void ZeroVector(int*, int);
  static void ZeroVector(float*, int);
  static void ZeroVector(EmcModule*, int);
  static void c3to5(float, float, float, float, float, float, float*, float*,
		    float*,  float*, float*, float*);

 protected:

  // geometry
  int fNx; // length in X direction
  int fNy; // length in Y direction
  float fModSizex; // module size in X direction (cm)
  float fModSizey; // module size in Y direction
  float fVx; // vertex position (cm)
  float fVy;
  float fVz;
  //  Sector - to - Global PHENIX transformation matrix
  PHMatrix emcrm;
  //  Sector - to - Global PHENIX translation vector
  PHVector emctr;
  //  Inverse transforamtion matrix and vector
  PHMatrix invemcrm;
  PHVector invemctr;

  // Tabulated values of Chi2 corresponding to 1% and 2% CL
  // for 50 values ndf (1-50)
  static float fgChi2Level1[];
  static float fgChi2Level2[];
  static float fgChi2Level[]; // actual level, chosen with SetChi2Limit().

#ifndef __CINT__
  std::vector<EmcModule> *fModules;
  std::vector<EmcCluster> *fClusters;
#endif

  float fgTowerThresh;
  float fgMinPeakEnergy;
  static float const fgMinShowerEnergy;
  static int const fgMaxLen;
  static float const fgMinIterStep;
  static int const fgMaxIter;

};

#endif // #ifndef EMCSECTORREC_H

// ///////////////////////////////////////////////////////////////////////////
// EOF
