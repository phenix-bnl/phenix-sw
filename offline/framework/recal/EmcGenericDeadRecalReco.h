#ifndef __EMCGENERICDEADRECALRECO_H__
#define __EMCGENERICDEADRECALRECO_H__

#include "Recalibrator.h"
#include "PHTimeStamp.h"

#include <string>
#include <vector>

class TH3;

/** Module to (re-)apply dead and warnmap to emc clusters.
 */


static std::string INFO_NO_REJECT_FILE = "<INFO> No Q&A information from physics.";
static std::string ERROR_CANNOT_READ_REJECT_FILE = "<ERROR> Cannot open reject file ";

class EmcGenericDeadRecalReco : public Recalibrator
{
 public:

  EmcGenericDeadRecalReco(const std::string &name = "EmcGenericDeadRecalReco");

  virtual ~EmcGenericDeadRecalReco(){};

  int Init(PHCompositeNode*);

  int InitRun(PHCompositeNode*);
  int process_event(PHCompositeNode*);
  int isValidRun(const int runno) const;

  unsigned int ErrorFast(int towerID) const;
  unsigned int DeadmapFast(int towerID) const;
  unsigned int WarningFast(int towerID) const;
  unsigned int WarnmapFast(int towerID) const;
  std::ostream& print(int towerid, std::ostream& out) const;
  std::ostream& PrintOne(int towerid, std::ostream& out) const;

 private:
  
  std::string histoName(const int is, const char* suffix);
  void Allocate();
  std::string CollectPhysicsQAFromDB(PHTimeStamp time_stamp);
  std::string CollectPhysicsQAFromFile(PHTimeStamp time_stamp);

  void ComputeMaps(const char* sectors);
  void ComputeMaps(size_t ifem);

  int recalemc;
  int recalcnt;
  std::vector<unsigned int> fErrorMap;
  std::vector<unsigned int> fWarnMap;

  std::vector<bool> fCollectedFEMs;
  std::vector<bool> fComputedFEMs;

  std::vector<unsigned int> fErrorRaw;
  std::vector<unsigned int> fWarnRaw;

  int DataFromFile;

  static const unsigned int fMASK_Ampl_Hot_Physics    = 0x00008;
  static const unsigned int fMASK_Ampl_Warm_Physics   = 0x00004;
//  static const unsigned int fMASK_Ampl_Physics    = 0x0000F;
  static const unsigned int fMASK_Ampl_Online     = 0x40000;
  static const unsigned int fMASK_Ampl_OnlineWarn = 0x00000;

  static const unsigned int fMASK_TOF_Physics     = 0x000F0;
  static const unsigned int fMASK_TOF_Online      = 0x00000;
  static const unsigned int fMASK_TOF_OnlineWarn  = 0x00000;
  TH3 *histo[8][7];

  int nevents;

};

  // Compute the neighbour error and warning flags for each tower
  // in the fem ifem.
  //
  // For amplitude bits are:
  // ---------------------
  // |   | 18| 19| 20|   |
  // ---------------------
  // | 13| 14| 15| 16| 17|
  // ---------------------  ^ y
  // | 8 | 9 | 10| 11| 12|  |
  // ---------------------  |
  // | 3 | 4 | 5 | 6 | 7 |  |
  // ---------------------  ------> z(x)
  // |   | 0 | 1 | 2 |   |
  // ---------------------
  // as viewed from the back of the central tower (which has bit 10 set
  // to 1 if it's itself a bad module); corner towers are excluded
  //
  // For ToF bits are :
  // -------------
  // | 27| 28| 29|  ^ y
  // -------------  |
  // | 24| 25| 26|  |
  // -------------  |
  // | 21| 22| 23|  ------> z(x)
  // -------------
  // as viewed from the back of the central tower (which has bit 25 set
  // to 1 if it's itself a bad module)
  //
  // So, a channel has a problem with amplitude measurements if its neighbor
  // error bit map  satisfies this mask:
  //            0x400
  // Actually, this mask is returned by the IamDeadMask() method
  // so that the amplitude for bad modules can be set to 0 at the calibration
  // stage.
  //
  // Some other useful masks.
  // The mask to look for amplitude errors or warnings in the 3x3 region
  // around a tower is:
  //          0x1ce70
  // In the 5x5 region:
  //         0x1fffff
  // To see if there are ToF problems for this tower:
  //        0x2000000
  //
  //
  //   From Here, added by T. Sakaguchi on 04/20/2005
  //     The 30th bit shows if the middle tower is is warm or hot tower
  //              in terms of charge
  //
  //    warm(high frequency in 1-2GeV) - 0, hot(high frequency in >2GeV) - 1
  //
  //    In WarnRaw Map, hot is implemented as 0x08.
  //       (This is just for the case of recalibration database)
  //
  // ----------------------------------
  // Now, ToF bits are utilized as warm map definition:
  // -------------
  // | 27| 28| 29|  ^ y
  // -------------  |
  // | 24| 30| 26|  |
  // -------------  |
  // | 21| 22| 23|  ------> z(x)
  // -------------
  // as viewed from the back of the central tower (which has bit 25 set
  // to 1 if it's itself a bad module)
  //
  // !!!!For Coldmap, 21-30 bits of deadmap are used.
  //       The definition is the same as in the case of warn map.!!!
  //

#endif
