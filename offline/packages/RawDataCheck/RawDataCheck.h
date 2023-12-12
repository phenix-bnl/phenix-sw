#ifndef __RAWDATACHECK_H__
#define __RAWDATACHECK_H__

#include "BadEvent.h"

#include <SubsysReco.h>

#include <iostream>
#include <list>
#include <map>
#include <set>
#include <vector>

class Event;
class GranuleCheck;
class Packet;
class RawDataCheckDBodbc;

class RawDataCheck: public SubsysReco
{
 public:
  static RawDataCheck *instance();
  virtual ~RawDataCheck();

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  int EndRun(const int runnumber);
  void Verbosity(const int ival);
  void Print(const std::string &what = "ALL") const;

  int registerGranule(GranuleCheck *granule);
  void identify(std::ostream& out = std::cout) const;
  int EventNumber() const {return eventnumber;}
  int RunNumber() const {return runnumber;}
  int GL1exists() const {return gl1exists;}
  int LastGL1Clock() const {return last_gl1_clock;}
  int GL1ClockDiff() const {return gl1clockdiff;}
  int GL1ClockCounter() const {return gl1_clock_counter;}
  int GL1GranuleCounter(const short i) const {return gl1_granule_counter[i];}
  unsigned int RawDataCheckEvts() const {return rawdatacheckevts;}
  void AddKnownBadPacket(const int ibad);
  void AddBadPacket(const unsigned int ibad, const unsigned int status);
  int isBadPacket(const unsigned int ibad) const;
  void AddFatalGl1Error(const int ival) {fatalgl1error += ival;}
  int RemoveKnownPacket(const int ibad);
  void PrintBadPackets() const;
  const std::set<unsigned int> *GetBadPacketList(const char *name = "KNOWN") const;
  void SetReturnCode(const int i) {returncode = i;}
  void AddDcmChkSumFail(const int packetid);
  int MultiBuffered() const {return multieventbuffered;}
  void UpdateDB(const int ival = 1) {updatedb = ival;}
  void MissingPacket() {hasmissingpackets++;}
  void DumpFileName(const char *fname) {savefile = fname;}
  int AddToList(Event *e, const std::string &str);
  int FrameStatusCheck(Event *evt);
  int AddToAllPacketList(const int pktid);
  void SearchUncheckedPackets(const int i = 1) {searchuncheckedpackets = i;}
  int isGranuleActive(const short i) {return granule_active[i];}

 protected:
  RawDataCheck(const std::string &name = "RawDataCheck");
  static RawDataCheck *__instance;
  int DcmCheckSumCheck(Event *e);
  int SearchUnchecked(Event *evt);
  int AddToDB();
  void WriteFile();
  std::vector<GranuleCheck *> Granule;
  std::set<unsigned int> defaultbadpackets;
  std::set<unsigned int> badpackets;
  std::map<int, unsigned int> dcmchksumfail;
  int updatedb;
  int eventnumber;
  int runnumber;
  int gl1exists;
  int runhasgl1data;
  int hasmissingpackets;
  int last_gl1_clock;
  int gl1clockdiff;
  int gl1_clock_counter;
  unsigned int rawdatacheckevts;
  unsigned int abortedevents;
  unsigned int gl1_granule_counter[32];
  int granule_active[32];
  unsigned int fatalgl1error; 
  unsigned int clkdiff_to_previous_event;
  int multieventbuffered;
  int returncode;
  Packet *plist[10000];
  std::list<BadEvent> badlist;
  RawDataCheckDBodbc *baddb;
  std::string savefile;
  std::string cvstag;
  std::set<int> allpackets;
  int searchuncheckedpackets;
};

#endif /* __RAWDATACHECK_H__ */
