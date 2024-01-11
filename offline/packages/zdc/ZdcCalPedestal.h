#ifndef ZDCCALPEDESTAL_H__
#define ZDCCALPEDESTAL_H__

#include <vector>
#include <Rtypes.h>

class Packet;
class TFile;
class TH1F;
class TH2F;

class ZdcCalPedestal
{
public:
  ZdcCalPedestal();
  virtual ~ZdcCalPedestal(){}

  int FillFromPacket(Packet *p);
  int CalculateConstants();
  int SaveToFile(const char *fname = "ZdcCalib.pedestal.temp");
  int FillFromFile(TFile *infile);
  int FillFromFile(const char *fname);

  friend class ZdcCalEnergy;

protected:
  std::vector<Double_t> pedmean;
  std::vector<Double_t> pedrms;
  std::vector<TH1F*> pedhist;		// ADC histograms for calculating pedestal
  std::vector<TH2F*> analogvsdigital;	// ADC histograms for calculating sum pedestals

};

#endif	// ZDCCALPEDESTAL_H__
