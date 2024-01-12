#ifndef __PHACCEPTPARTICLECENTRALARM_HH__
#define __PHACCEPTPARTICLECENTRALARM_HH__

#include <TLorentzVector.h>

class PHAcceptParticleCentralArm
{

private:
  double line1(double phi);
  double line2(double phi);
  double line3(double phi);
  double line4(double phi);
  double line5(double phi);
  double line6(double phi);
  double line7(double phi);
  double line8(double phi);
  double line9(double phi);
  double line10(double phi);
  double line11(double phi);
  double line12(double phi);
  double line13(double phi);
  double line14(double phi);
  double line15(double phi);
  double line16(double phi);
  double line17(double phi);
  double line18(double phi);
  double line19(double phi);
  double line20(double phi);

  int doNeutral(double phi);
  int doPositive(double mom, double phi);
  int doNegative(double mom, double phi);

public:
  PHAcceptParticleCentralArm(double rdc, double minTheta, double maxTheta, double minPhi, double maxPhi);
  ~PHAcceptParticleCentralArm(){;}

  int acceptParticle(TLorentzVector *part, int charge, double vertex);

protected:
  double curpar[20][3];
  double RadiusDC;
  double ThetaMin, ThetaMax;
  double PhiMin, PhiMax;

  static const int NUMBER = 1000000;
  static const double pi = 3.14159;
};

#endif  /* __PHACCEPTPARTICLEiCENTRALARM_HH__ */


