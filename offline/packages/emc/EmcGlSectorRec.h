#ifndef EMCGLSECTORREC_H
#define EMCGLSECTORREC_H

// Name: EmcGlSectorRec.h
// Author: M. Volkov (RRC KI) Jan 27 2000
// PbGl sector reconstruction

#include "EmcSectorRec.h"


// ///////////////////////////////////////////////////////////////////////////

/** PbGl implementation of EmcSectorRec. 
@ingroup clustering
*/

class EmcGlSectorRec: public EmcSectorRec
{
  
 public:

  EmcGlSectorRec():EmcSectorRec(){ SetTowerThreshold(0.05); }
  virtual ~EmcGlSectorRec(){}

  virtual void Gamma(int nmod, EmcModule *modules, float *pchi, float *pchi0,
		     float *pe1, float *px1, float *py1,
		     float *pe2, float *px2, float *py2,
		     int &ndf); // ndf added MV 28.01.00

  // These functions must be defined
  virtual void SetTowerThreshold(float Thresh){fgTowerThresh = Thresh;}
  virtual float Chi2Limit(int ndf);
  virtual float Chi2Correct(float chi2, int ndf);
  virtual void SetProfileParameters(int sectorID, float energy,
				    float z, float y);
  virtual float PredictEnergy(float deltaz, float deltay, float energy);
  virtual float ClusterChisq(int nmod, EmcModule* modules,
			     float energy, float zcg, float ycg, int &ndf);
  virtual float ClusterChisq(int, emcTowerContent**, float, float, float,
			      int &ndf); // add by mlp

   virtual  void CorrectPosition(float energy, float zcg, float ycg,
				float *zcgcorr, float *ycgcorr, bool callSetPar=true);
  virtual  void CorrectEnergy(float energy, float x, float y, float *ecorr);
  virtual  void CorrectECore(float ecore, float x, float y, float *ecorecorr);

  // These functions don't return meaningful results yet
  virtual void CalculateErrors(float e, float x, float y, float* pde,
			       float* pdx, float* pdy, float* pdz);
  virtual void TwoGamma(int nmod, EmcModule *modules, float *pchi,
			float *pe1, float *pz1, float *py1,
			float *pe2, float *pz2, float *py2);
  virtual void TwoGamma(int nmod, emcTowerContent **modules, float *pchi,
			float *pe1, float *pz1, float *py1,
			float *pe2, float *pz2, float *py2);
  virtual void getTowerPos(int ix, int iy, float &x, float & y);
  /// Converts coordinates in units of towers into cm's (Local coord. system)
  void TowersToSector(float, float, float &, float &);
  /// Returns  coordinates of the tower centers in cm's (Local coord. system)
  void TowersToSector(int,   int,   float &, float &);
  /// Converts Local Sector coordinates in cm into integer tower numbers
  void SectorToTowers(float, float, int &,   int &);

 protected:

  // Parameters for sigma calculation in Chi2 analysis
  static float fgEpar00;
  static float fgEpar0;
  static float fgEpar1;
  static float fgEpar2;
  static float fgEpar3;
  static float fgEpar4;

  static float fSin4T;

  static float fgConfLevel; // confidence level, chosen with SetChi2Limit().

  // Parameters for sigma calculation in Chi2 analysis
  static float const fgSigEcorr0; // 2 energy correction parameters
  static float const fgSigEcorr1;
  static float const fgSigAcorr0; // 2 angle (theta) correction parameters
  static float const fgSigAcorr1;

  // Parameters for threshold calculation in Chi2 analysis
  static float const fgCutEcorr0; // 2 energy correction parameters
  static float const fgCutEcorr1;

  // Parameters for coordinate correction
  static float const fgCoorPar00;
  static float const fgCoorPar01;
  static float const fgCoorPar02;
  static float const fgCoorPar03;
  static float const fgCoorPar10;
  static float const fgCoorPar11;
  static float const fgCoorPar12;
  static float const fgCoorPar20;
  static float const fgCoorPar21;

  // Parameters for shower shape prediction, set by SetProfileParameters()
  float fSinTx; // sin of angle in XZ plane
  float fSinTy; // sin of angle in YZ plane
  float fTheta; // particle incidence angle (polar), degrees
  float fPhi; // angle wrt horisontal axis in calorimeter plane, degrees
  float fShift; // coord. transf. parameters
  float fExc1;
  float fExc2;
  float fShapePars[4]; // shower shape parameters

  float fSpecThr; // special cut for chi2 calculation
  float fThrCorr; // cut shower shape correction

 private:

  // Auxiliary static funcions
  static float ShiftFunc(float energy, float angle); // Coord. transf.
  static float Sigma1Func(float energy, float angle);
  static float Sigma2Func(float energy, float angle);

  static float ShapeFunc(float *x, float *par); // Shower shape
  static float AFunc(float energy, float angle, float phi);
  static float CFunc(float energy, float angle, float phi);
  static float DFunc(float energy, float angle, float phi);
  static float SFunc(float energy, float angle, float phi);
  static float PeriodicFunc(float *x, float *par);

  static float InvScurveFunc(float cog, float *par);
  static void Rotate(float phi, float &deltaRow, float &deltaCol);

  static float CalcSigma(float predicted, float totSignal, float theta);

};

#endif // #ifndef EMCGLSECTORREC_H
