#ifndef __FCLCALIB__
#define __FCLCALIB__

#include "FclConsts.h"

class PHTimeStamp;

class FclCalib
{

 public:

  FclCalib();

  void setSide(int side) {whichSide=side;}
  int getDatabaseInfo(PHTimeStamp& time);
  int getDatabaseInfo(int runNumber);
  int getDatabaseInfoCosmicOnly();
  float getCalib(int channel);
  float getCalib(int row, int col) {return cal[row][col];}
  void getZdcXtalk(int row, int col, float &intercept, float &slope,  float &intercept_err, float &slope_err);
  void print();

 private:
  float cal[ROWUSE][COLUSE];
  float zdcxtalk_intercept[ROWUSE][COLUSE];
  float zdcxtalk_intercept_err[ROWUSE][COLUSE];
  float zdcxtalk_slope[ROWUSE][COLUSE];
  float zdcxtalk_slope_err[ROWUSE][COLUSE];
  int whichSide;
};

#endif
