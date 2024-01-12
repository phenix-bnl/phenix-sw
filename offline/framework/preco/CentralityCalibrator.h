#ifndef __CENTRALITYCALIBRATOR_H__
#define __CENTRALITYCALIBRATOR_H__

#include <SubsysReco.h>

class TH1F;
class TH2F;
class TProfile;
class TNtuple;
class TFile;
class PHCentralTrack;
class PHGlobal;
class PHCompositeNode;

class CentralityCalibrator : public SubsysReco
{
public:
  CentralityCalibrator(const char *Type="PHGlobal", const char *name="CentralityCalibrator");
  virtual ~CentralityCalibrator() {}

  //  Standard methods
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void setInput(int a) {Input=a;}
  void CalibrateFromNtuple(char*);
  void Book();

protected:
  int runNumber;
  PHGlobal *d_global;
  int GetNodes(PHCompositeNode *topNode);
  TFile* OutputFile;
  TFile* InputFile;
  void CalibrateByClock();
  void CalibrateByPerp();

  int Input;
  int nbins;
  float start;
  float stop;
  TNtuple* ntpcent;
  TH1F* hBbcZdcAngle;
  TH1F* hBbcZdcAngleCSum;
  TH1F* hcalib1;
  TH1F* perpCSum;
  TProfile* prof1;
  float bbcFraction;
  float bbcScale;
  float bbcShift;
  float bbcPower;
  float zdcMax;

};

#endif 

