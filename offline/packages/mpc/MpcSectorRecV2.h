#ifndef __MPCSECTORV2_H__
#define __MPCSECTORV2_H__

// Name: MpcSectorRec.h
// Derived from emc/EmcSectorRec.h

// MpcSectorRec -- abstract base class for both PbSc and PbGl and now MPC

#include <PHMatrix.h>
#include <PHVector.h>
#include <MpcSectorRec.h>
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

class MpcSectorRecV2 : public MpcSectorRec
{
  
 public:
  
  MpcSectorRecV2(const int iarm);
  virtual ~MpcSectorRecV2();
  
  virtual mpcClusterContent* FillPeakArea(MpcPeakarea& pp, MpcCluster& cluster);
  virtual void Gamma(int, MpcModule*, float*, float*, float*, float*, float*,
		     float*, float*, float*,
		     int &ndf); // ndf added MV 28.01.00
  virtual void Gammacore(int nh, MpcModule* phit, float* pchi, float* pchi0,
			 float* pe1, float* px1, float* py1, float* pe2,
			 float* px2, float* py2, int &ndf,
			 bool &split, float &chi2_split, float &se1, float &sx1, float &sy1,
			 float &se2, float &sx2, float &sy2);
  
  

  /* virtual void Gammacore(int, MpcModule*, float*, float*, float*, float*, float*, */
/* 			 float*, float*, float*, */
/* 			 int &ndf); */
  
  virtual void Mom1V2(int, MpcModule*, float*, float*, float*);

  //these are now in the base class
  //  virtual void Mom1Linear(int, MpcModule*, float*, float*, float*); 
  //virtual void Mom1Log(int, MpcModule*, float*, float*, float*, float);
  //virtual void MomentsLog(int, MpcModule*, float*, float*, float*, 
  //			  float*, float*,float*,float);
  //virtual void MomentsLinear(int, MpcModule*, float*, float*, float*, 
  //			     float*, float*,float*);

  // Pure virtual functions
  virtual void SetTowerThreshold(float Thresh);
  //  virtual void SetProfileParameters(int, float, float, float);
  void SetChi2Limit(int lim);
  virtual float ClusterChisq(int, MpcModule*, float, float, float,
			     int &ndf); // ndf added MV 28.01.00
  virtual float ClusterChisqV2(int, MpcModule*, float, float, float,
			     int &ndf); // ndf added MV 28.01.00
  
  virtual float ClusterChisqcoreV2(int, MpcModule*, float, float, float,
			     int &ndf); // ndf added MV 28.01.00
  virtual float ClusterChisqcore_TwoGammaV2(int nh, MpcModule* phit, float e1, float x1,
					    float y1, float e2, float x2, float y2, int &ndf);
  
  virtual float Chi2Correct(float chi2,int ndf);
  virtual  void CorrectPosition(float energy, float x, float y, float *xcorr,
				float *ycorr, bool callSetPar=true);
  virtual void CorrectEnergy(float energy, float x, float y, float *ecorr);
  virtual void CorrectECore(float ecore, float x, float y, float *ecorecorr);
  virtual void TwoGamma(int, MpcModule*, float*, float*, float*, float*,
			float*, float*, float*);
  virtual void TwoGammaV2(int, MpcModule*, float*, float*, float*, float*,
			float*, float*, float*);
  virtual float Chi2Limit(int ndf);
  //  virtual float PredictEnergy(float, float, float);
  virtual void CalculateErrors(float e, float x, float y, float* pde,
			       float* pdx, float* pdy, float* pdz);
  virtual void getTowerPos(int ix, int iy, float &x, float & y);
  virtual void computeCorrectedDispersionV2(float ycg, float xcg,
                                    float dispy, float dispx,
                                    float yModSize, float xModSize,
                                    float& corrdispy, float& corrdispx);




  float EvalRMSPar(int ipar, float e);
  float EvalShowPar(int ipar, float e);
  float EvalShowPar2(int ipar, float e);
  void SetProfileParametersV2(int sec, float Energy, float x, float y );
  void SetProfileParametersV2_2(int sec, float Energy, float x, float y );
  float PredictEnergyV2(float x, float y, float Energy);
  float PredictEnergyV2_2(float x, float y, float Energy);

  float PredictRMS(float x, float y, float Energy, float efrac);
  

protected:


  
  float fShowPar[6][12]; //exp(-r)+exp^(-r^3)+exp^(-r^5)
  float fShowPar2[6][12]; //exp(-r)+exp^(-r^3)+exp^(-r^5) used in sharing
  float fRMSPar[7][12];  //piecewise: pol4(Efrac), r < 1.2, else exp(-r)
  

  float fShow[6]; //parameters 
  float fShow2[6]; //parameters used in sharing
  float fRMS[7];

  float r1[200];
  float r2[200];
  float en1[200];
  float en2[200];
  float s1[200];
  float s2[200];

  int split_clus[2];  //do we perform cluster splitting in N, S?
 /*  Float fEtot;  */
/*   float fXcg;  */
/*   float fYcg; */
/*   float fXXcg; //in accordance w/ emcal obfuscation procedure  */
/*   float fYYcg; //XX is dispersion x w.r.t linear cg */
/*   float fYXcg;  */

/*   float fLogXcg;  */
/*   float fLogYcg; */
/*   float fLogXXcg; //in accordance w/ emcal obfuscation procedure  */
/*   float fLogYYcg; //xx is dispersion x w.r.t linear cg */
/*   float fLogYXcg;  */

  
};

#endif // __MPCSECTORV2_H__

