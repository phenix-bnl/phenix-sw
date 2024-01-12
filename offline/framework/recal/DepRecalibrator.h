#ifndef __DEPRECALIBRATOR_H__
#define __DEPRECALIBRATOR_H__

#include "Recalibrator.h"
#include <string>

#include "TF1.h"

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;
class TH2F;

class DepRecalibrator : public Recalibrator
{
 public:
  DepRecalibrator(const std::string &name="DepRecalibrator");
  virtual ~DepRecalibrator() {}

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  void Print(const std::string& ="") const;
  void help() const;

 protected:

  int runNumber;

  PHCentralTrack *d_cnt;
  PHGlobal *d_global;
  float calculate_dep(const int armsect, const int charge, const float pt, const float ep);
  TH2F *hdep_pt_e; 
  TH2F *hsdep_pt_e;  
  TH2F *hdep_pt_p; 
  TH2F *hsdep_pt_p;  

  TF1  depfitmean0;
  TF1 depfitwidth0;
  TF1  depfitmean1;
  TF1 depfitwidth1;
  TF1  depfitmean2;
  TF1 depfitwidth2;
  TF1  depfitmean3;
  TF1 depfitwidth3;
  TF1  depfitmean4;
  TF1 depfitwidth4;
  TF1  depfitmean5;
  TF1 depfitwidth5;
  TF1  depfitmean6;
  TF1 depfitwidth6;
  TF1  depfitmean7;
  TF1 depfitwidth7;

};

#endif 

