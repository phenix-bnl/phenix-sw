#ifndef RUN56PPEMCPHOTONTOFRECALRECO_H__
#define RUN56PPEMCPHOTONTOFRECALRECO_H__


#include <Recalibrator.h>

#include <string>
#include <vector>

#define NEMC_ARM 2
#define NEMC_SECTOR 4
#define NEMC_Y 48
#define NEMC_Z 96


class PHCompositeNode;
class emcClusterContent;

// #based on EmctofRecalReco 
// for pro72/73 production (Run5pp 200GeV)
// A new photon ToF is reconstucted from rawTDC and ecent, and 
// is put into "tofcorr" valiable. 
// Due to a historical reason, the procedure is complicated. 
// From Run7, it would be simpler. KO

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

class Run56PPEmcphotontofRecalReco : public Recalibrator
{
 public:
  Run56PPEmcphotontofRecalReco(const int SlewScheme=3, const std::string &name = "Run56PPEmcphotontofRecalReco");
  virtual ~Run56PPEmcphotontofRecalReco() {}

  //  Standard methods to set up analysis
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  float Gettflash(const emcClusterContent *gt, const float vtx[]) const;
  int GetAdc(const float ecent,const int arm,const int sector,const int ypos,const int zpos) const;
  float Tdc2Tof(const float rawtdc,const int adc,const int arm,const int sector,const int ypos,const int zpos,const int adcbase) const;
  float Log(const int adc) const;

  int isValidRun(const int runno) const;

 
 // KO
  void fetchgtoftparprod(const int run); 			// get constants from emcgtoftparprod
  void updategtoftparprod(const int beginrun, const int endrun);// store constants from emcgtoftparprod

  void fetchgtofeparprod(const int run); 			// get constants from emcgtofeparprod
  void updategtofeparprod(const int beginrun, const int endrun);// store constants from emcgtofeparprod

  void fetchgtofsecoffset(const int run); 			// get constants from emcgtofsecoffset
  void updategtofsecoffset(const int beginrun, const int endrun);// store constants from emcgtofsecoffset

  void fetchgtoftwroffset(const int run); 			// get constants from emcgtoftwroffset
  void updategtoftwroffset(const int beginrun, const int endrun);// store constants from emcgtofstwroffset

  void fetchgtoftwrwalk(const int run); 			// get constants from emcgtoftwrwalk
  void updategtoftwrwalk(const int beginrun, const int endrun);// store constants from emcgtoftwrwalk


  // to change the database you use (don't use this)
  void setDB        (const int i)  {FlagDB =i;}
  int  getDB        () const       {return FlagDB;}
  // to change the slewing scheme
  void setSlewScheme(const int sc) {SlewScheme=sc;}
  int  getSlewScheme() const       {return SlewScheme;}

// KO 
  void readsecoffset(const char *filename);
  void readtwroffset(const char *filename);
  void readtwrwalk(const char *filename);
  void readprodpar(const int runnumber);

  void printprodpar(void);

  void SetSecOffset(const int arm, const int sect, const float vv){
    SecOffset[arm][sect]=vv;
  }


protected:
// for photon timing calibrations
  float SecOffset[NEMC_ARM][NEMC_SECTOR];
  float TwrOffset[NEMC_ARM][NEMC_SECTOR][NEMC_Y][NEMC_Z];
  float WalkPar[NEMC_ARM][NEMC_SECTOR][NEMC_Y][NEMC_Z];

// Calibration parameter used in Run5pp pro.72/73 production 
  // constant through the entire run		gtoftparprod
  float Pwalk[NEMC_ARM][NEMC_SECTOR][NEMC_Y][NEMC_Z];
  float Ptzero[NEMC_ARM][NEMC_SECTOR][NEMC_Y][NEMC_Z];
  float Plc[NEMC_ARM][NEMC_SECTOR][NEMC_Y][NEMC_Z];

  // different run by run
  float PGain[NEMC_ARM][NEMC_SECTOR][NEMC_Y][NEMC_Z];
  float PNormt[NEMC_ARM][NEMC_SECTOR][NEMC_Y][NEMC_Z];
  float PScale[NEMC_ARM][NEMC_SECTOR][NEMC_Y][NEMC_Z];

  // residual energy dependent correction parameters
  int Switch_edep;
  float A_PbSc;
  float B_PbSc;
  float C_PbSc;
  float A_PbGl;
  float B_PbGl;
  float C_PbGl;


  //  Flags for what corrections to apply for hadron calibrations
  bool FlagTimeFromRaw;   // use temcrawtdc with a gain - bbc t0
  bool FlagDeltaT;        // a time shift for each tower
  bool FlagRunbyRun;   // for runbyrun time shift correction
  int  FlagDB;        //flag for the different db's 0=stable, 1=tower by tower ppc, 2 =lc -- don't change 
                      // using FlagDB=1 only
  int  SlewScheme;        // (< or =0)--> no slewing, 3--> is defined (slewing for each tower), rest not defined

  bool firstInit;
  int  runnumber;

  //  Pointers to the data...
  int recalemc; 
  std::vector<std::string> phglobalnodes;
};

#endif /* RUN56PPEMCPHOTONTOFRECALRECO_H__*/
