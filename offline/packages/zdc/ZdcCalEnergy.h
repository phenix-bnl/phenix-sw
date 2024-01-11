#ifndef ZDCCALENERGY_H__
#define ZDCCALENERGY_H__

class Packet;
class TFile;
class TH1;
class ZdcCalib;


class ZdcCalEnergy
{
public:
  ZdcCalEnergy();
  ZdcCalEnergy(ZdcCalib *z);
  virtual ~ZdcCalEnergy() {}

  int FillFromPacket(Packet *p);
  int CalculateConstants();
  int SaveToFile(const char *fname = "ZdcCalib.adc.temp");
  int FillFromFile(const char *);
  int FillFromFile(TFile *);

  void SetCalibration(ZdcCalib *z);
  double FitAu(TH1 *, double& scale, double& error);
  double FitDeuteron(TH1 *, double& scale, double& error);

protected:
  void InitVars();
  void CreateHistos();
  ZdcCalib *zdccalib;

  // ADC histograms for calculating energy gain
  TH1 *zssum;
  TH1 *znsum;
  TH1 *zs123;
  TH1 *zn123;
  TH1 *zs1;
  TH1 *zs2;
  TH1 *zs3;
  TH1 *zn1;
  TH1 *zn2;
  TH1 *zn3;

  // energy scale and error
  double zssumescale;
  double zssumerror;
  double zs123escale;
  double zs123error;
  double znsumescale;
  double znsumerror;
  double zn123escale;
  double zn123error;

};

#endif	// __ZDCCALENERGY_H__
