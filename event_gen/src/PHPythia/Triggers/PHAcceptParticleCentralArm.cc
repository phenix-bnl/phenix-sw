#include "PHAcceptParticleCentralArm.hh"

PHAcceptParticleCentralArm::PHAcceptParticleCentralArm(double rdc, double minTheta, double maxTheta, double minPhi, double maxPhi)
{
  RadiusDC = rdc;
  ThetaMin = minTheta;
  ThetaMax = maxTheta;
  PhiMin = minPhi;
  PhiMax = maxPhi;

  curpar[0][0]  =  2.864789;   curpar[0][1]  =  -.58904862;  curpar[0][2]  =   .14;
  curpar[1][0]  =  2.4064227;  curpar[1][1]  =  -.19634954;  curpar[1][2]  =   .15;
  curpar[2][0]  =  2.4064227;  curpar[2][1]  =   .19634954;  curpar[2][2]  =   .16;
  curpar[3][0]  =  2.4064227;  curpar[3][1]  =   .58904862;  curpar[3][2]  =   .17;
  curpar[4][0]  =  2.6356059;  curpar[4][1]  =   .9817477;   curpar[4][2]  =   .18;
  curpar[5][0]  =  2.864789;   curpar[5][1]  =  2.1598449;   curpar[5][2]  =   .14;
  curpar[6][0]  =  2.4064227;  curpar[6][1]  =  2.552544;    curpar[6][2]  =   .15;
  curpar[7][0]  =  2.4064227;  curpar[7][1]  =  2.9452431;   curpar[7][2]  =   .16;
  curpar[8][0]  =  2.4064227;  curpar[8][1]  =  3.3379422;   curpar[8][2]  =   .17;
  curpar[9][0]  =  2.6356059;  curpar[9][1]  =  3.7306413;   curpar[9][2]  =   .18;
  curpar[10][0] = -2.6356059;  curpar[10][1] =  -.58904862;  curpar[10][2] =   .14;
  curpar[11][0] = -2.4064227;  curpar[11][1] =  -.19634954;  curpar[11][2] =   .15;
  curpar[12][0] = -2.4064227;  curpar[12][1] =   .19634954;  curpar[12][2] =   .16;
  curpar[13][0] = -2.4064227;  curpar[13][1] =   .58904862;  curpar[13][2] =   .17;
  curpar[14][0] = -2.864789;   curpar[14][1] =   .9817477;   curpar[14][2] =   .18;
  curpar[15][0] = -2.6356059;  curpar[15][1] =  2.1598449;   curpar[15][2] =   .14;
  curpar[16][0] = -2.4064227;  curpar[16][1] =  2.552544;    curpar[16][2] =   .15;
  curpar[17][0] = -2.4064227;  curpar[17][1] =  2.9452431;   curpar[17][2] =   .16;
  curpar[18][0] = -2.4064227;  curpar[18][1] =  3.3379422;   curpar[18][2] =   .17;
  curpar[19][0] = -2.864789;   curpar[19][1] =  3.7306413;   curpar[19][2] =   .18;

}


double PHAcceptParticleCentralArm::line1(double phi)
{
  if(phi>=curpar[0][1]) return NUMBER; 
  return .5*(1/(1-exp((curpar[0][0])*(phi-curpar[0][1])))-1)+curpar[0][2];
}
double PHAcceptParticleCentralArm::line2(double phi)
{
  if(phi>=curpar[1][1]) return NUMBER;
  return .5*(1/(1-exp((curpar[1][0])*(phi-curpar[1][1])))-1)+curpar[1][2];
}
double PHAcceptParticleCentralArm::line3(double phi)
{
  if(phi>=curpar[2][1]) return NUMBER;
  return .5*(1/(1-exp((curpar[2][0])*(phi-curpar[2][1])))-1)+curpar[2][2];
}
double PHAcceptParticleCentralArm::line4(double phi)
{
  if(phi>=curpar[3][1]) return NUMBER;
  return .5*(1/(1-exp((curpar[3][0])*(phi-curpar[3][1])))-1)+curpar[3][2];
}
double PHAcceptParticleCentralArm::line5(double phi)
{
  if(phi>=curpar[4][1]) return NUMBER; 
  return .5*(1/(1-exp((curpar[4][0])*(phi-curpar[4][1])))-1)+curpar[4][2];
}
double PHAcceptParticleCentralArm::line6(double phi)
{
  if(phi>=curpar[5][1]) return NUMBER; 
  return .5*(1/(1-exp((curpar[5][0])*(phi-curpar[5][1])))-1)+curpar[5][2];
}
double PHAcceptParticleCentralArm::line7(double phi)
{
  if(phi>=curpar[6][1]) return NUMBER; 
  return .5*(1/(1-exp((curpar[6][0])*(phi-curpar[6][1])))-1)+curpar[6][2];
}
double PHAcceptParticleCentralArm::line8(double phi)
{
  if(phi>=curpar[7][1]) return NUMBER; 
  return .5*(1/(1-exp((curpar[7][0])*(phi-curpar[7][1])))-1)+curpar[7][2];
}
double PHAcceptParticleCentralArm::line9(double phi)
{
  if(phi>=curpar[8][1]) return NUMBER; 
  return .5*(1/(1-exp((curpar[8][0])*(phi-curpar[8][1])))-1)+curpar[8][2];
}
double PHAcceptParticleCentralArm::line10(double phi)
{
  if(phi>=curpar[9][1]) return NUMBER; 
  return .5*(1/(1-exp((curpar[9][0])*(phi-curpar[9][1])))-1)+curpar[9][2];
}

double PHAcceptParticleCentralArm::line11(double phi)
{
  if(phi<=curpar[10][1]) return -NUMBER;
  return  -.5*(1/(1-exp((curpar[10][0])*(phi-curpar[10][1])))-1)-curpar[10][2];
}
double PHAcceptParticleCentralArm::line12(double phi)
{
  if(phi<=curpar[11][1]) return -NUMBER;
  return -.5*(1/(1-exp((curpar[11][0])*(phi-curpar[11][1])))-1)-curpar[11][2];
}
double PHAcceptParticleCentralArm::line13(double phi)
{
  if(phi<=curpar[12][1]) return -NUMBER;
  return -.5*(1/(1-exp((curpar[12][0])*(phi-curpar[12][1])))-1)-curpar[12][2];
}
double PHAcceptParticleCentralArm::line14(double phi)
{
  if(phi<=curpar[13][1]) return -NUMBER;
  return -.5*(1/(1-exp((curpar[13][0])*(phi-curpar[13][1])))-1)-curpar[13][2];
}
double PHAcceptParticleCentralArm::line15(double phi)
{
  if(phi<=curpar[14][1]) return -NUMBER;
  return -.5*(1/(1-exp((curpar[14][0])*(phi-curpar[14][1])))-1)-curpar[14][2];
}
double PHAcceptParticleCentralArm::line16(double phi)
{
  if(phi<=curpar[15][1]) return -NUMBER;
  return -.5*(1/(1-exp((curpar[15][0])*(phi-curpar[15][1])))-1)-curpar[15][2];
}
double PHAcceptParticleCentralArm::line17(double phi)
{
  if(phi<=curpar[16][1]) return -NUMBER;
  return -.5*(1/(1-exp((curpar[16][0])*(phi-curpar[16][1])))-1)-curpar[16][2];
}
double PHAcceptParticleCentralArm::line18(double phi)
{
  if(phi<=curpar[17][1]) return -NUMBER;
  return  -.5*(1/(1-exp((curpar[17][0])*(phi-curpar[17][1])))-1)-curpar[17][2];
}
double PHAcceptParticleCentralArm::line19(double phi)
{
  if(phi<=curpar[18][1]) return -NUMBER;
  return -.5*(1/(1-exp((curpar[18][0])*(phi-curpar[18][1])))-1)-curpar[18][2];
}
double PHAcceptParticleCentralArm::line20(double phi)
{
  if(phi<=curpar[19][1]) return -NUMBER;
  return  -.5*(1/(1-exp((curpar[19][0])*(phi-curpar[19][1])))-1)-curpar[19][2];
}

int PHAcceptParticleCentralArm::doNeutral(double phi)
{ 
  int val = 0;
  if(phi<-PhiMin)phi+=2.*pi;
  if(phi>curpar[0][1]&&phi<curpar[4][1])
    {
      //in east arm
      val = 1;
    }
  else if(phi>curpar[5][1]&&phi<curpar[9][1])
    {
      //in west arm
      val = 2;
    }
  return val;
}

int PHAcceptParticleCentralArm::doPositive(double mom, double phi)
{
  int val=0;

  if(mom<0)mom=-mom;
  if(phi<-PhiMax) phi+=2.*pi;
  if(mom>line5(phi)&&mom<line1(phi))
    {
      val = 1;
    }
  else if(mom>line10(phi)&&mom<line6(phi))
    {
      val = 2;
    }
  return val;
}

int PHAcceptParticleCentralArm::doNegative(double mom, double phi)
{
  int val=0;

  if(mom>0)mom=-mom;
  if(phi<-PhiMin) phi+=2.*pi;
  if(mom>line15(phi)&&mom<line11(phi))
    {
      val = 1;
    }
  if(mom>line20(phi)&&mom<line16(phi))
    {
      val = 2;
    }
  return val;
}


int PHAcceptParticleCentralArm::acceptParticle(TLorentzVector *part, int charge, double vertex)
{
  int val=0;
  double mom = part->P();
  double theta = part->Theta();
  double phi = part->Phi();

  double ThetaMinZ = atan(tan(ThetaMin) / (1 - vertex/RadiusDC * tan(ThetaMin)));
  double ThetaMaxZ = pi - atan(tan(ThetaMin) / (1 + vertex/RadiusDC * tan(ThetaMin)));

  //theta cut
  if(theta>ThetaMaxZ || theta<ThetaMinZ) return val;
  
  //deal with neutral particle.
  //   if(charge==0) return doNeutral(phi);

  //now when |charge|>1 we need to adjust mom, so we can still use the same filter
  mom/=charge;
  if(charge>0)
    {
      return doPositive(mom,phi);
    } else {
      return doNegative(mom,phi);
    }

  return val;
}


