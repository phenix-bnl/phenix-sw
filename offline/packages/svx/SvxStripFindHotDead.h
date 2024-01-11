#ifndef SVXSTRIPFINDHOTDEAD_H
#define SVXSTRIPFINDHOTDEAD_H

#include <SubsysReco.h>
#include "svxAddress.hh"
#include <TString.h>

class PHCompositeNode;
class TH2F;
class TH3F;
class TFile;
class TNtuple;

/**
 * @brief  A SubsysReco module to find hot or dead channel and write them
 *         to the database.
 * @date  Created by Sasha Lebedev in September 2010
 *        Update by Lei Ding in Aug 2011
 *        Update by Rachid Nouicer December 2012
 */
class SvxStripFindHotDead : public SubsysReco
{

 public:

  SvxStripFindHotDead(const std::string &name = "SVXSTRIPFINDHOTDEAD");
  virtual ~SvxStripFindHotDead() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void set_OutputFileName(std::string name) {OutputFileName=name;}
  void set_MarkType(const int mktp=1) {Mark_Type=mktp;}
  void set_ThresholdShift(const int thrd=24) {Threshold_shift=thrd;}
  void Load_ThresholdFile(char *thresholdfile) {thresholdfilename=thresholdfile;}
  int set_Threshold();

  void setOutputName(TString name) { fOutputName = name; }
  
  enum RunFlag {Run11=11,Run12}; 
  void setRunFlag(RunFlag runflag) { fRunFlag = runflag; }

 protected:

  int  DeadChip(int lay, int lad, int sens, int chip, int type);
  int  DeadSensor(int lay, int lad, int sens,int type);
  int  DeadLadder(int lay, int lad,int type);
  double  Get_AdcMean(TH2F *h2, int ichnl);
  double  Get_AdcRMS(TH2F *h2, int ichnl);

  svxAddress *address;
  TH2F* hsvxrawhit[SVXLAYERNUMBER-2][SVXLADDERNUMBER][SVXSENSORNUMBER][2];
  int thresh[SVXLAYERNUMBER-2][SVXLADDERNUMBER][SVXSENSORNUMBER][12][128];
  int  EventNumber;
  int Threshold_shift;
  std::string OutputFileName;
  char *thresholdfilename;
  int Mark_Type;

  TNtuple *nt;
  TNtuple *ntev;
  TString fOutputName;

  TFile *fOutFile;

  RunFlag fRunFlag;
};
#endif 


