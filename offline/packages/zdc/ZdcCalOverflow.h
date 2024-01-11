#ifndef __ZDCCALOVERFLOW_H__
#define __ZDCCALOVERFLOW_H__

#include <vector>

#include <Rtypes.h>

class Packet;
class TH1F;

class ZdcCalOverflow
{
public:
  ZdcCalOverflow();
  virtual ~ZdcCalOverflow(){}

  int FillFromPacket(Packet *p);
  int CalculateConstants();
  //int SaveToFile(const char *fname = ".temp");

protected:
  std::vector<Double_t> oflowmean;
  std::vector<Double_t> oflowrms;
  std::vector<TH1F*> oflowhist;	// ADC histograms for calculating overflow

};

#endif	// __ZDCCALOVERFLOW_H__
