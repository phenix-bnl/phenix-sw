#ifndef __ZDC_CALIBRATOR_H__
#define __ZDC_CALIBRATOR_H__

#include <TObject.h>

class ZdcCalibrator : public TObject
{
public:
  ZdcCalibrator();
  virtual ~ZdcCalibrator() { }

  int ReadZdcTree(const char *fname);
  int CalculateSlew();

private:
  TProfile *slewprof[40][2];
  Double_t tdclut[8][2][4096];

  ClassDef(ZdcCalibrator,0)
};

#endif

