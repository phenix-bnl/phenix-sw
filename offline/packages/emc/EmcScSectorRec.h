#ifndef EMCSCSECTORREC_H
#define EMCSCSECTORREC_H

// Name: EmcScSectorRec.h
// Author: A. Bazilevsky (RIKEN-BNL)
// Major modifications by M. Volkov (RRC KI) Jan 27 2000

#include "EmcSectorRec.h"

// ///////////////////////////////////////////////////////////////////////////

/** PbSc implementation of EmcSectorRec.
@ingroup clustering
*/

class EmcScSectorRec: public EmcSectorRec
{
  
 protected:

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
  float sin2ax, sin2ay, sin2a, lgE;

 public:

  EmcScSectorRec():EmcSectorRec(){ SetTowerThreshold(0.05); }
  virtual ~EmcScSectorRec(){}

  // These functions must be defined
  virtual  void CorrectPosition(float energy, float x, float y, float *xcorr,
				float *ycorr, bool callSetPar=true);
  virtual  void CorrectEnergy(float energy, float x, float y, float *ecorr);
  virtual  void CorrectECore(float ecore, float x, float y, float *ecorecorr);
  virtual void CalculateErrors(float e, float x, float y, float* pde,
			       float* pdx, float* pdy, float* pdz);
  virtual void SetProfileParameters(int, float, float, float);
  virtual float PredictEnergy(float, float, float);
  virtual void TwoGamma(int, EmcModule*, float*, float*, float*, float*,
			float*, float*, float*);
  virtual void TwoGamma(int nmod, emcTowerContent **modules, float *pchi,
			float *pe1, float *pz1, float *py1,
			float *pe2, float *pz2, float *py2);

  virtual float ClusterChisq(int, EmcModule*, float, float, float,
			     int &ndf); // ndf added MV 28.01.00

  virtual float ClusterChisq(int, emcTowerContent**, float, float, float,
			     int &ndf); // added by mlp

  virtual float Chi2Limit(int ndf);
  virtual float Chi2Correct(float chi2,int ndf);
  virtual void SetTowerThreshold(float Thresh);
  virtual void getTowerPos(int ix, int iy, float &x, float & y);
  /// Converts coordinates in units of towers into cm's (Local coord. system)
  void TowersToSector(float, float, float &, float &);
  /// Returns  coordinates of the tower centers in cm's (Local coord. system)
  void TowersToSector(int,   int,   float &, float &);
  /// Converts Local Sector coordinates in cm into integer tower numbers
  void SectorToTowers(float, float, int &,   int &);

};

#endif // #ifndef EMCSCSECTORREC_H

// ///////////////////////////////////////////////////////////////////////////
// EOF
