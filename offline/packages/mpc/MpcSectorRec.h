#ifndef __MPCSECTOR_H__
#define __MPCSECTOR_H__

// Name: MpcSectorRec.h
// Derived from emc/EmcSectorRec.h

// MpcSectorRec -- abstract base class for both PbSc and PbGl and now MPC

#include <phool.h>
#include "PHMatrix.h"
#include "PHVector.h"
#include <vector>

struct MpcModule;
class MpcCluster;
class MpcPeakarea;
class mpcTowerContainer;
class mpcClusterContainer;
class mpcClusterContent;
class MpcMap;
class PHPoint;
class TH2F;
class recoConsts;

/*
typedef struct SecGeom{

  short nx;          // Number of cells in X dir
  short ny;          // Number of cells in Y dir
  float Tower_xSize; // Tower size in X dir
  float Tower_ySize; // Tower size in Y dir

} SecGeom;
*/

// ///////////////////////////////////////////////////////////////////////////

/** ABC of a clusterizer for one EMCAL sector. 

@ingroup clustering

 */

class MpcSectorRec
{
  
 public:

  MpcSectorRec(const int iarm);
  virtual ~MpcSectorRec();

  void SetVertex(float iz) {  fVx = 0.; fVy = 0.; fVz = iz; }
  void SetVertex(float ix, float iy, float iz) {  fVx = ix; fVy = iy; fVz = iz; }
  void SetVertex(const PHPoint& p);

  float GetModSizex() const { return fModSizex; }
  float GetModSizey() const { return fModSizey; }
  float GetVx() const { return fVx; }
  float GetVy() const { return fVy; }
  float GetVz() const { return fVz; }
  int   GetArm() const { return fArm; }

  void SetPeakThreshold(float Thresh) { fgMinPeakEnergy = Thresh; }
  float GetPeakThreshold() { return fgMinPeakEnergy; }
  float GetTowerThreshold() { return fgTowerThresh; }
  
  void FillHitList(const mpcTowerContainer *mtow);
  virtual mpcClusterContent *FillPeakArea(MpcPeakarea&, MpcCluster&);
  void SetClusterOut(mpcClusterContainer *mclus);

  mpcTowerContainer *GetModules(){ return fTowers; }
  std::vector<MpcCluster> *GetClusters(){ return fClusters; }

  int FindClusters();
  void GetImpactAngle(float x, float y, float *sinT );

  virtual void Gamma(int, MpcModule*, float*, float*, float*, float*, float*,
		     float*, float*, float*,
		     int &ndf); // ndf added MV 28.01.00


  virtual void Gammacore(int nh, MpcModule* phit, float* pchi, float* pchi0,
			       float* pe1, float* px1, float* py1, float* pe2,
			       float* px2, float* py2, int &ndf,
			       bool &split, float &chi2_split, float &se1, float &sx1, float &sy1,
			       float &se2, float &sx2, float &sy2)
  { PHOOL_VIRTUAL_WARNING; return; }

  /* virtual void Gammacore(int, MpcModule*, float*, float*, float*, float*, float*, */
/* 			 float*, float*, float*, */
/* 			 int &ndf,) */
/*   { PHOOL_VIRTUAL_WARNING; return; } */


  void Mom1(int, MpcModule*, float*, float*, float*);
  void Momenta(int, MpcModule*, float*, float*, float*, float*, float*,
	       float* );

  virtual void Mom1V2(int, MpcModule*, float*, float*, float*)
  { PHOOL_VIRTUAL_WARNING; return; }
  
  virtual void Mom1Linear(int, MpcModule*, float*, float*, float*);
  virtual void Mom1Log(int, MpcModule*, float*, float*, float*, float);
  virtual void MomentsLog(int, MpcModule*, float*, float*, float*, float*
			  ,float*,float*,float);
  virtual void MomentsLinear(int, MpcModule*, float*, float*, float*, 
			     float*, float*,float*);

  
  
  
  // Pure virtual functions
  virtual void SetTowerThreshold(float Thresh);
  virtual void SetProfileParameters(int, float, float, float);
  void SetChi2Limit(int lim);
  virtual float ClusterChisq(int, MpcModule*, float, float, float,
			     int &ndf); // ndf added MV 28.01.00
  virtual float ClusterChisqV2(int, MpcModule*, float, float, float,
			       int &ndf) // ndf added MV 28.01.00
  { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual float ClusterChisqcoreV2(int, MpcModule*, float, float, float,
			       int &ndf) // ndf added MV 28.01.00
  { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual float ClusterChisqcore_TwoGammaV2(int nh, MpcModule* phit, float e1, float x1,
					    float y1, float e2, float x2, float y2, int &ndf)
  { PHOOL_VIRTUAL_WARNING; return 9999; }

  virtual float Chi2Correct(float chi2,int ndf);
  virtual  void CorrectPosition(float energy, float x, float y, float *xcorr,
				float *ycorr, bool callSetPar=true);
  virtual void CorrectEnergy(float energy, float x, float y, float *ecorr);
  virtual void CorrectECore(float ecore, float x, float y, float *ecorecorr);
  virtual void TwoGamma(int, MpcModule*, float*, float*, float*, float*,
			float*, float*, float*);
  virtual void TwoGammaV2(int, MpcModule*, float*, float*, float*, float*,
			float*, float*, float*)
  { PHOOL_VIRTUAL_WARNING; return; }

  virtual float Chi2Limit(int ndf);
  virtual float PredictEnergy(float, float, float);
  virtual void CalculateErrors(float e, float x, float y, float* pde,
			       float* pdx, float* pdy, float* pdz);
  virtual void getTowerPos(int ix, int iy, float &x, float & y);
  void computeCorrectedDispersion(float ycg, float xcg,
                                   float dispy, float dispx,
                                   float yModSize, float xModSize,
                                   float& corrdispy, float& corrdispx);
  virtual void computeCorrectedDispersionV2(float ycg, float xcg,
					    float dispy, float dispx,
					    float yModSize, float xModSize,
					    float& corrdispy, float& corrdispx)
  { PHOOL_VIRTUAL_WARNING; return; }
  virtual float EvalRMSPar(int ipar, float e)
  { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual float EvalShowPar(int ipar, float e)
  { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual float EvalShowPar2(int ipar, float e)
  { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual void SetProfileParametersV2(int sec, float Energy, float x, float y )  
  { PHOOL_VIRTUAL_WARNING; return; }
  virtual void SetProfileParametersV2_2(int sec, float Energy, float x, float y )  
  { PHOOL_VIRTUAL_WARNING; return; }
  virtual float PredictEnergyV2(float x, float y, float Energy)
  { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual float PredictEnergyV2_2(float x, float y, float Energy)
  { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual float PredictRMS(float x, float y, float Energy, float efrac)
  { PHOOL_VIRTUAL_WARNING; return 9999; }


  // Auxiliary static functions
  static int HitNCompare(const void*, const void*);
  static int HitACompare(const void*, const void*);
  static void CopyVector(int*, int*, int);
  static void CopyVector(MpcModule*, MpcModule*, int);
  static void ZeroVector(int*, int);
  static void ZeroVector(float*, int);
  static void ZeroVector(MpcModule*, int);
  static void ResizeVector(int* , int, int);
  static void c3to5(float, float, float, float, float, float, float*, float*,
		    float*,  float*, float*, float*);


protected:


  static int moment_type;

  // geometry
  static const int fNx = 18; // length in X direction
  static const int fNy = 18; // length in Y direction
  float fModSizex; // module size in X direction (cm)
  float fModSizey; // module size in Y direction
  float fVx; // vertex position (cm)
  float fVy;
  float fVz;
  int   fArm;

  MpcMap  *mpcmap;
  TH2F *sdisplay;
  TH2F *ndisplay;

  recoConsts *reco_consts;      // stores var's to control reco behavior
  int mpc_event_display;        // whether to generate display
  int mpc_verbosity;        	// how much info to print out

  // Tabulated values of Chi2 corresponding to 1% and 2% CL
  // for 50 values ndf (1-50)
  static float fgChi2Level1[];
  static float fgChi2Level2[];
  static float fgChi2Level[]; // actual level, chosen with SetChi2Limit().


  //for the partesum function
  MpcModule* fHVect;
  static const int HVECTSIZE=220;

  mpcTowerContainer *fTowers;
  std::vector<MpcModule> *fModules;
  std::vector<MpcCluster> *fClusters;
  mpcClusterContainer *fClusterOut;

  float fgTowerThresh;
  float fgMinPeakEnergy;
  static float const fgMinShowerEnergy;
  static int const fgMaxLen;

  // Parameters for sigma calculation in Chi2 analysis
  static float fgEpar00;
  static float fgEpar0;
  static float fgEpar1;
  static float fgEpar2;
  static float fgEpar3;
  static float fgEpar4;

  // Parameters for shower shape and Chi2 calculation
  // Set by SetProfileParameters()
  float fSin4T;
  float fSinTx;
  float fSinTy;
  float fPpar1;
  float fPpar2;
  float fPpar3;
  float fPpar4;
  float fPshiftx;
  float fPshifty;

public:
  static void SetMomentType(int type)
    { 
      moment_type = (type<1 || type>2)?0:type;
    }


};

#endif // __MPCSECTOR_H__

// ///////////////////////////////////////////////////////////////////////////
// EOF
