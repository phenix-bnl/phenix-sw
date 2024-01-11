#ifndef __ZDCCALSLEW_H__
#define __ZDCCALSLEW_H__

#include <vector>

class Packet;
class TFile;
class TH1F;
class TH2F;
class TProfile;
class ZdcCalib;

class ZdcCalSlew
{
public:
  ZdcCalSlew();
  ZdcCalSlew(ZdcCalib *z);
  virtual ~ZdcCalSlew(){}

  void InitHistograms();
  int FillFromPacket(Packet *p, float bbcntime, float bbcstime);
  int CalculateConstants();
  int SaveToFile(const char *fname = "ZdcCalib.adc.temp");

  int FillFromFile(TFile *);
  int FillFromFile(const char *);

  void SetCalibration(ZdcCalib *z);

protected:
  ZdcCalib *zdccalib;

  // histograms for calculating slew correction
  std::vector<TH2F*> slewhist;
  std::vector<TProfile*> slewprof;

  // histograms for correcting vtx and t0
  std::vector<TH1F*> ztdiff;

};

#endif	// __ZDCCALSLEW_H__
