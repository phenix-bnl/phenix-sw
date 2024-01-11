#ifndef __REACTIONPLANE_H__
#define __REACTIONPLANE_H__

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include <fstream>
#include <cmath>

class reactionPlane {

public:

  reactionPlane();
  virtual ~reactionPlane();
  void  calBbcPlane(int imul, int izps, short adc[128], short tdc[128]);
  void  clrPlane();
  void  filHitPlane(int idet, float phi, float wgt, float pm);
  void  calPlane(int imul, int izps);
  float getBbcPlane(int idet, int ihar);
  float getHitPlane(int idet, int ihar);
  void  checkResolution(int imul, int izps);
  void  closePlane();
  int   getNdet() {return ndet;}
  int   getNhar() {return nhar;}
  int   getNmul() {return nmul;}
  int   getNzps() {return nzps;}
  int   getNord() {return nord;}

private:

  static const int nDet=8;
  static const int nHar=4;
  static const int nMul=50;
  static const int nZps=5;
  static const int nOrd=32;
  void calHitPlane(int imul, int izps, int idet);
  void filResolution(int ibin, int hid,
                     int idet, int ihar, int jdet, int jhar);
  void readSetup();
  void readGeoCal();
  void readTable();
  void writeTable();
  void readCorre();
  void writeCorre();
  void readOffst();
  void writeOffst();
  void makeHisto();
  void saveHisto();
  int  nrun;
  int  ndet;
  int  nhar;
  int  nmul;
  int  nzps;
  int  nord;
  int  flag;
  float     pede[128],oflw[128],slw1[128],slw2[128];
  float     gain[128],tgai[128],qgai[128];
  float     xpos[128],ypos[128],zpos[128];
  float     sumx[nDet][nHar];
  float     sumy[nDet][nHar];
  float     sumw[nDet][nHar];
  float      psi[nDet][nHar];
  float      aaa[nDet][nHar][nMul][nZps][nOrd];
  float      bbb[nDet][nHar][nMul][nZps][nOrd];
  float     ofsx[nDet][nHar][nMul][nZps];
  float     ofsy[nDet][nHar][nMul][nZps];
  int       irad[128];
  float     fac[nMul][nZps][128];
  TH1F     *Hphi[nDet][nHar][nMul][nZps];
  TH1F     *Hphj[nDet][nHar][nMul][nZps];
  TProfile *Hprc[nDet][nHar][nMul][nZps];
  TProfile *Hprs[nDet][nHar][nMul][nZps];
  TProfile *Hsum[nDet][nHar][nMul][nZps];
  TProfile *Hpmt[nMul][nZps];
  TProfile *Hrad[nMul][nZps];
  TH2F     *Hcor[23][nZps];
  TH2F     *Hrsp[23];
  TProfile *Hrsc[23];
  TProfile *Hrss[23];
  TFile    *hfile;

};
#endif  /* __REACTIONPLANE_H__ */

