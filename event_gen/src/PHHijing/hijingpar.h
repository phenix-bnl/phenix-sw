#ifndef __HIJINGPAR_H__
#define __HIJINGPAR_H__

// Maps the namelist used in PHENIX's hijing program
struct HIJINGPAR {
  float efrm;
  char frame[4];
  char hproj[4];
  char htarg[4];
  int iap;
  int izp;
  int iat;
  int izt;
  int nhievt;
  float bmin0;
  float bmax0;
  int trigger_jet;
  float thetamin;
  float thetamax;
  int naccepted;
};

#endif
