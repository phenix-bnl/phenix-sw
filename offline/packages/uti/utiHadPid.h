#ifndef UTIHADPID_H
#define UTIHADPID_H
///////////////////////////////////////////////////////////////////////
//
// utiHadPid class
//
// Author:  J. Burward-Hoy
// 
// Description:  
//  class that determines the PID of hadrons (pi+/-, K+/-, (anti)p)
//
//
// 
//////////////////////////////////////////////////////////////////////

class PHCompositeNode;

class utiHadPid {

 public:

  utiHadPid();
  utiHadPid(float TOF, float PL, float MOM);
  utiHadPid(PHCompositeNode *topNode);
  /*  
      functions that return 0: no or 1: yes if pion, kaon, or proton 
      THIS IS NO LONGER TRUE!!!  

      IsPion, IsKaon, and IsProton return the number of SIGMA that the 
      calculated mass**2 value is from the measured 'true' mass**2 value.
  */

  // charge: input charge of hadron
  // n:  number of sigma away from mass-squared centroid based on 
  //     measured mass-squared and mass-squared width
  // TOF: measured time-of-flight of hadron
  // PL:  measured pathlength of hadron
  // MOM:  measured momentum of hadron

  float IsPion(int charge,float TOF, float PL, float MOM);
  float IsKaon(int charge,float TOF, float PL, float MOM);
  float IsProton(int charge,float TOF, float PL, float MOM);

  //  NOTE:  In order to use these (i.e. charge only argument fxns),
  //  you MUST set the momentum, pathlength, and TOF AS WELL AS call
  //  the calc M2 and sigmaM2 functions!!!  

  //  These are lower overhead functions for using the same TOF/PL/MOM 
  //  to do checks over all PIDS.
  float IsPion(int charge);
  float IsKaon(int charge);
  float IsProton(int charge);

  // Miscellaneous/additional functions to override default values
  void setM2MeanPi(int charge, float pi);
  void setM2MeanK(int charge, float k);
  void setM2MeanP(int charge, float p);
  float getM2MeanPi(int charge);
  float getM2MeanK(int charge);
  float getM2MeanP(int charge);

  void setHadronTimeOfFlight(float time) {t = time;}
  void setHadronPathlength(float pathlength) { L = pathlength;}
  void setHadronMomentum(float mom) { p = mom;}

  float getHadronTimeOfFlight() {return t;}
  float getHadronPathLength() {return L;}
  float getHadronMomentum() {return p;}

  void setSigmaEMC(float sigma);
  float getSigmaEMC() {return sigmaEMC;}

  void setSigmaTOF(float sigma);
  float getSigmaTOF() {return sigmaTOF;}
  void setSigmaNtcTof(float sigma){sigmaNtcTof=sigma;}
  float getSigmaNtcTof() {return sigmaNtcTof;}
  void setSigmaTzr(float sigma){sigmaTzr = sigma;}
  float getSigmaTzr() {return sigmaTzr;}
  void setSigmaNtcDC(float sigma){sigmaNtcDC=sigma;}
  float getSigmaNtcDC() {return sigmaNtcDC;}

  void setSigmaDC(float sigma_ms, float sigma_alpha);
  float getSigmaDC_MS() {return sigmaDC[0];}
  float getSigmaDC_Alpha() {return sigmaDC[1];}
  void setK1(float k1) {K1 = k1;}
  float getK1() {return K1;}

  void setMeasM2(float measM2) { m2 = measM2;}
  float getMeasM2() { return m2;}
  void calcMeasM2();

  void setSigmaM2(float sigM2) { sigmaM2 = sigM2;} 
  float getSigmaM2() {return sigmaM2;}
  void calcSigmaM2();

  void set_TofPlMom(float TOF, float PL, float MOM);

  void set_TofPID() { TofPID = true; }
  void set_TofNtcPID() { TofNtcPID = true;TofPID =false;TzrTofPID=false; }
  void set_TzrTofPID() { TzrTofPID = true;TofPID =false;TofNtcPID=false; }
  void set_EmcPID() { TofPID = false; }
  
  bool get_TofPID() { return TofPID; }
  bool get_TofNtcPID() { return TofNtcPID; }
  bool get_TzrTofPID() { return TzrTofPID; }
 
 private:

  void Load_Default();

  int PIDYEARSPECIESFLAG; //read comments in source file
  
  float meanPi[2], meanK[2], meanP[2];
  float sigmaDC[2];
  float sigmaTOF;
  float sigmaNtcTof;
  float sigmaTzr;
  float sigmaNtcDC;   
  float sigmaEMC;
  float K1;

  float t,L,p; //meas. time-of-flight, pathlength, and momentum
  float m2; // measured mass-squared based on t, L, and p given
  float sigmaM2; // measured M2 width

  float c; // speed of light
  bool TofPID;
  bool TofNtcPID;
  bool TzrTofPID;
  bool EmcPID;  
}; 

#endif







