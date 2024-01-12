#ifndef __momchangeRECALRECO_H__
#define __momchangeRECALRECO_H__

#include "Recalibrator.h"

#include <Fun4AllReturnCodes.h>

#include <string>

class PHCentralTrack;
class PHCompositeNode;
class TH1;
class TProfile;

class MomChangeRecalReco : public Recalibrator
{
 public:

  /// .
  MomChangeRecalReco(const std::string &name = "MomChangeRecalReco");
  virtual ~MomChangeRecalReco() {}
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  void fetch(const int runnumber);
  void update(const int runnumber, const int beginrun, const int endrun=-1);

  void fetchScaleFactor(const int runnumber);
  void updateScaleFactor(const int runnumber, const int beginrun, const int endrun=-1);

  void SetScaleFactor(const float val) { ScaleFactor=val; return;}
  void SetScaleFactorW(const float val) { ScaleFactorW=val; return;}
  void SetScaleFactorE(const float val) { ScaleFactorE=val; return;}
  void SetScaleFactor0(const float val) { ScaleFactor0=val; return;}
  void SetScaleFactor1(const float val) { ScaleFactor1=val; return;}

  void SetYOffSetW(const float val) { YOffsetW=val; return;}
  void SetYOffSetE(const float val) { YOffsetE=val; return;}
  void SetYOffSet(const float val) { YOffset=val; return;}

  void SetXOffSetW(const float val) { XOffsetW=val; return;}
  void SetXOffSetE(const float val) { XOffsetE=val; return;}
  void SetXOffSet(const float val)  { XOffset=val; return;}  

 private:
  float alpha;
  float phi;
  float  new_alpha(const float alpha, const float phi, const int arm, const int runnumber);
  float  delta_phi0(const float del_alpha, const int runnumber);
  std::string databaseName; // TBC (see init())
  std::string databaseName2; // TBC (see init())

  void InitObj();
  float XOffsetW;
  float XOffsetE;
  float YOffsetW;
  float YOffsetE;
  float XOffset;
  float YOffset;
  
  float AlphaOffsetW;
  float AlphaOffsetE;
  float AlphaOffset;

  
  float ScaleFactor;
  float ScaleFactorE;
  float ScaleFactorW;
  float ScaleFactor0;
  float ScaleFactor1;
  unsigned int runnumber;

  int db;
  int arm;
 
  TH1 *phi0old;
  TH1 *phi0new;
  TH1 *phi0diff;
  TProfile *phi0prof;
  TH1 *momold;
  TH1 *momnew;
  TH1 *momdiff;
  TProfile *momprof;

};

#endif /* __GENERICRECALRECO_H__ */
