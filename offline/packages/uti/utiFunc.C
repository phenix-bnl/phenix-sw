#include "utiFunc.h"

utiFunc::utiFunc() {
}

utiFunc::~utiFunc() {
}

float utiFunc::pc3MatchSDphi(float mom) {
  float pc3dphicut = 0.044*exp(-7.14*fabs(mom))+0.0023;
  return pc3dphicut;
}

float utiFunc::pc3MatchSDzps(float mom) {
  float pc3dzcut   = 12.5*exp(-5.65*fabs(mom))+1.02;
  return pc3dzcut;
}

float utiFunc::tofMatchSDphi(float mom) {
  float tofdphicut = 0.019*exp(-4.84*fabs(mom))+0.004;
  return tofdphicut;
}

float utiFunc::tofMatchSDzps(float mom) {
  float tofdzcut   = 15.18*exp(-6.24*fabs(mom))+1.25;
  return tofdzcut;
}

float utiFunc::emcMatchSDphi(float mom) {
  float emcdphicut = 0.033*exp(-3.36*fabs(mom))+0.006;  
  return emcdphicut;
}

float utiFunc::emcMatchSDzps(float mom) {
  float emcdzcut   = 24.95*exp(-8.33*fabs(mom))+3.47;
  return emcdzcut;
}

int utiFunc::tofMass2Pid(float mom, float ms2, float sel, float vet) {
  float par[5],fun[12];
  for (int jval=0; jval<2; jval++) {
    for (int ival=0; ival<6; ival++) {
      if (ival<2) {
        par[0]=0.00290591; par[1]=-0.00030206; par[2]=0.0131466;
        par[3]=0.14*0.14;
      }
      if (ival>1 && ival<4) {
        par[0]=0.00422273; par[1]=0.006422;    par[2]=0.0138111;
        par[3]=0.49*0.49;
      }
      if (ival>3) {
        par[0]=0.0638367;  par[1]=-0.0390393;  par[2]=0.0263881;
        par[3]=0.94*0.94;
        if (ival==5) par[3]-=0.03;
      }
      if (jval==0) {
        if (ival%2==0) par[4]=sel;
        else           par[4]=-sel;
      } else {
        if (ival%2==0) par[4]=vet;
        else           par[4]=-vet;
      }
      float x = fabs(mom);
      float y = par[0] + par[1]*x + par[2]*x*x;
      fun[ival+jval*6] = par[3] + par[4]*y;
    }
  }
  int pid=0;
  int sum=0;
  if (ms2>fun[1]&&ms2<fun[0]&&ms2<fun[3+6])               {pid=1; sum++;}
  if (ms2>fun[3]&&ms2<fun[2]&&ms2>fun[0+6]&&ms2<fun[5+6]) {pid=2; sum++;}
  if (ms2>fun[5]&&ms2<fun[4]&&ms2>fun[2+6])               {pid=3; sum++;}
  if (sum!=1) pid=0;
  return pid;
}
