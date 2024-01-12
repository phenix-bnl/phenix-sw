#ifndef RUN5PPEMCTOFRECALRECO_H__
#define RUN5PPEMCTOFRECALRECO_H__

#include "Recalibrator.h"
#include "EmcPar.h"

#include <string>
#include <vector>
 
class PHCompositeNode;
class PHSnglCentralTrack;

// based on emcal timing calibrations from Kazuya and Sarah's
// EmctofrecalReco code
// A. Sickles 3/16/2006

class Run5PPEmctofRecalReco : public Recalibrator
{
 public:
  Run5PPEmctofRecalReco(const std::string &name="Run5PPEmctofRecalReco");

  //  Standard methods to set up analysis
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  int isValidRun(const int runno) const;

  //store and retrieve from the db
  void  fetchGain     (const int runnumber);      
  void  updateGain    (const int beginrun, const int endrun=-1); 

  void fetchDeltaT (const int runnumber);           
  void updateDeltaT(const int beginrun, const int endrun=-1); 

  void fetchSlew (const int runnumber);               
  void updateSlew(const int beginrun, const int endrun=-1);  

  void fetchRunbyRun  (const int runnumber);               
  void updateRunbyRun (const int beginrun, const int endrun=-1); 

  void fetchT0Shift  (const int runnumber);               
  void updateT0Shift (const int beginrun, const int endrun=-1);  

  void fetchWarnmap (const int runnumber);
  void updateWarnmap (const int beginrun, const int endrun=-1);

  // procedures to apply calibrations
  void applyTimeFromRaw(PHSnglCentralTrack *sngltrk, const float bbct0);
  void applyDeltaT     (PHSnglCentralTrack *sngltrk);
  void applySlewing    (PHSnglCentralTrack *sngltrk);
  int applyRunbyRun   (PHSnglCentralTrack *sngltrk);
  void applyT0Shift    (PHSnglCentralTrack *sngltrk);
  int applyWarn       (PHSnglCentralTrack *sngltrk);

  void readDeltaT(const char *filename);
  void readRunbyRun(const char *filename);
  void readgain(const char *filename);
  void readslewing(const char *filename);
  void readwarnmap(const char *filename);

protected:
  // values for hadron timing calibrations
  int   NumRuns;
  float Gain[EmcPar::NEMC_ARM][EmcPar::NEMC_SECTOR][EmcPar::NEMC_Y][EmcPar::NEMC_Z];
  float DeltaT[EmcPar::NEMC_ARM][EmcPar::NEMC_SECTOR][EmcPar::NEMC_Y][EmcPar::NEMC_Z];
  float SlewParameters[EmcPar::NEMC_ARM][EmcPar::NEMC_SECTOR][EmcPar::NEMC_Y][EmcPar::NEMC_Z][5];
  float warnmap[EmcPar::NEMC_ARM][EmcPar::NEMC_SECTOR][EmcPar::NEMC_Y][EmcPar::NEMC_Z];
  double t0_shift;

  //  Pointers to the data...
  int recalcnt;
  int abortevent;
  std::vector<std::string> phglobalnodes;
};

#endif /* RUN5PPEMCTOFRECALRECO_H__ */
