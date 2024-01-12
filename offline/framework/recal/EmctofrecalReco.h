#ifndef EMCTOFRECALRECO_H__
#define EMCTOFRECALRECO_H__

#include "EmcPar.h"
#include "Recalibrator.h"

#include <string>
#include <vector>
#include <map>


class PHCompositeNode;
class PHSnglCentralTrack;

//
//  Hello PHENIX recalibration fans:
//
//      Live and learn.  After some years of experience we
//  conclude that some calibrations are too sensitive to be
//  learned with ultimate precision prior to data production.
//  In some cases this is cause for despair.  In other cases,
//  there is hope.  The cases that have hope are those cases
//  for which it is both simple and correct to apply corrections
//  at a later stage and tune up the data at that point.
//
//      This is certainly the case with the Tzero of the EMC
//  subsystem.  A final adjustment to Tzero can be made long
//  after production and result in the best physics performance
//  we can hope for.  Historically in PHENIX such corrections have
//  indeed been made, however, there has never been a standard 
//  mechanism for this technique.
//
//      Since the advent of Fun4All, modules become context-
//  independent, and the standard mechanism for ANY analysis
//  task is the same:  Write a SubsysReco() module!!
//
//      This module accomplishes the task of applying corrections
//  to the time reported by towers in the EMC detector system.  You
//  can simply run this module prior to your own analysis code and 
//  BINGO, you get corrected times into your analysis.
//
//                                    TKH 2-4-2004
//

class EmctofrecalReco : public Recalibrator
{
 public:
  EmctofrecalReco(const int SlewScheme=3, const std::string &name = "EmctofrecalReco");
  virtual ~EmctofrecalReco() {}

  //  Standard methods to set up analysis
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  void Print(const std::string &what) const;
  int isValidRun(const int runno) const;

  void  fetchLaserLC  (const int runnumber);               // get constants from emc database -- don't use this
  void  fetchGain     (const int runnumber);               // get constants from emctof hadron database
  void  updateGain    (const int beginrun, const int endrun=-1); // store constants in database

  void fetchDeltaT (const int runnumber);               // get constants from emctof hadron database
  void updateDeltaT(const int beginrun, const int endrun=-1); // store constants in database

  void fetchSlew (const int runnumber);                 // get constants from emctof hadron database
  void updateSlew(const int beginrun, const int endrun=-1);   // store constants in database

  void fetchRunbyRun  (const int runnumber);                 // get constants from emctof hadron database
  void updateRunbyRun (const int beginrun, const int endrun=-1);   // store constants in database

  void fetchT0Shift  (const int runnumber);                 // get constants from emctof hadron database
  void updateT0Shift (const int beginrun, const int endrun=-1);   // store constants in database

  //  Accessors for flags...
  void DoTimeFromRaw(const bool b) {FlagTimeFromRaw = b;}
  void DoDeltaT     (const bool b) {FlagDeltaT      = b;}
  void DoRunbyRun   (const bool b) {FlagRunbyRun    = b;}
  void DoT0Shift    (const bool b) {FlagT0Shift     = b;}
  
  // to change the database you use (don't use this)
  void setDB        (const int i)  {FlagDB =i;}
  int  getDB        () const       {return FlagDB;}
  // to change the slewing scheme
  void setSlewScheme(const int sc) {SlewScheme=sc;}
  int  getSlewScheme() const       {return SlewScheme;}

  // procedures to apply calibrations
  void applyTimeFromRaw(PHSnglCentralTrack *sngltrk, const float bbct0);
  void applyDeltaT     (PHSnglCentralTrack *sngltrk);
  void applySlewing    (PHSnglCentralTrack *sngltrk);
  int applyRunbyRun   (PHSnglCentralTrack *sngltrk);
  void applyT0Shift    (PHSnglCentralTrack *sngltrk);

protected:
  // values for hadron timing calibrations
  std::map<int, float> RunShifts;
  float thisrunshift;
  int   NumRuns;
  float Gain[EmcPar::NEMC_ARM][EmcPar::NEMC_SECTOR][EmcPar::NEMC_Y][EmcPar::NEMC_Z];
  float DeltaT[EmcPar::NEMC_ARM][EmcPar::NEMC_SECTOR][EmcPar::NEMC_Y][EmcPar::NEMC_Z];
  float SlewParameters[EmcPar::NEMC_ARM][EmcPar::NEMC_SECTOR][EmcPar::NEMC_Y][EmcPar::NEMC_Z][4];
  double t0_shift;

// for photon timing calibrations
   float a0[8]; 
   float a1[8];
   float a2[8];
   float a3[8];

  //  Flags for what corrections to apply for hadron calibrations
  bool FlagTimeFromRaw;   // use temcrawtdc with a gain - bbc t0
  bool FlagDeltaT;        // a time shift for each tower
  bool FlagRunbyRun;   // for runbyrun time shift correction
  bool FlagT0Shift;        //for t0shift correction to prevent any momentum dependance
  int  FlagDB;        //flag for the different db's 0=stable, 1=tower by tower ppc, 2 =lc -- don't change
  int  SlewScheme;        // (< or =0)--> no slewing, 3--> is defined (slewing for each tower), rest not defined

  int  runnumber;

  int recalcnt;
  int abortevent;
  std::vector<std::string> phglobalnodes;
};

#endif /* __EMCTOFRECALRECO_H__ */
