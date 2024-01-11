#ifndef __MBBCRAWRECAL_H__
#define __MBBCRAWRECAL_H__

#include "phool.h"

class BbcCalib;
class PHCompositeNode;

class mBbcRawReCal
{

 public:
  mBbcRawReCal() {}
  virtual ~mBbcRawReCal() {}
  PHBoolean event(PHCompositeNode*);
  void Clear();
  int restore();
  float UnCalibAdc(int ipmt, int Adc);
  float UnCalibTdc0(int ipmt, int Tdc0);
  float UnCalibTdc1(int ipmt, int Tdc1);
private:
        BbcCalib *bbccalib;
};

#endif /*__MBBCRAWRECAL_H__*/
