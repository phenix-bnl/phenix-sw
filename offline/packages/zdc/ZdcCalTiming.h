#ifndef __ZDCCALTIMING_H__
#define __ZDCCALTIMING_H__

#include <TH1.h>

class Packet;
class ZdcCalib;

class ZdcCalTiming
{
public:
  ZdcCalTiming();
  ZdcCalTiming(ZdcCalib *z);
  virtual ~ZdcCalTiming();

  void InitHistograms();
  int CalculateConstants();
  int SaveToFile(const char *fname = ".temp");

  int Fill(const float d_zdc_bbc_zvtx, const float d_zdc_bbc_t0);

  void SetCalibration(ZdcCalib *z);

protected:
  ZdcCalib *zdccalib;

  // histograms for correcting vtx and t0
  TH1 *zvtxbvtxcheck;
  TH1 *zt0bt0check;

  float zvtx_offset;
  float t0_offset;

};

#endif	// __ZDCCALTIMING_H__
