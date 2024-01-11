#ifndef __DPHDCHTRACK_H__
#define __DPHDCHTRACK_H__


typedef struct {
  short id;
  short arm;
  short side;
  short charge;
  short numberOfX1X2hitsFitted;
  short numberOfSuccessfulIterations;
  double chi2;
  short ErrorCode;
  double alpha;
  double zed;
  double momentum;
  double fittedAlpha;
  double fittedPhi;
  double fittedPhi0;
  double fittedBeta;
  double fittedTheta0;
  float fittedPoint[3];
  float fittedDirection[3];

  float vertex[3];
  float projectToVertex[3];
  float predictMomentum[3];
  float projectToPc1[3];
  float projectToPc2[3];
  float projectToPc3[3];
  float projectToTec[3];
  float projectToPbSc[3];
  float projectToPbGl[3];
  float projectToCrk[3];
  float projectToTof[3];
} DPHDCHTRACK_ST;
#endif /*__DPHDCHTRACK_H__*/
